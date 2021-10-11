module fluentd.syntax.parser;

import std.array: Appender, appender;
import std.range.primitives: empty;

import fluentd.syntax.parser.span;
import fluentd.syntax.parser.stream;
import fluentd.utils.lexing: isCallee;
import ast = fluentd.syntax.ast;
import err = fluentd.syntax.parser.errors: ErrorKind, ParserError;

import sumtype;

private pure @safe:

public struct ParserResult {
    ast.Resource resource;
    ParserError[ ] errors;
}

class _ParserException: Exception {
    ParserError err;

    this(ParserError e) nothrow pure @nogc {
        super(null);
        err = e;
    }
}

struct _NamedPattern {
    ast.Identifier id;
    ast.OptionalPattern value;
}

struct _MessageLike {
    _NamedPattern pattern;
    ast.Attribute[ ] attributes;
}

enum _PatternState: ubyte {
    haveNoText,
    haveText,
    mergingTexts,
}

T _unreachable(T = void)(const(char)[ ] msg) nothrow @nogc {
    assert(false, msg);
}

Span _byteAt(ByteOffset pos) nothrow @nogc {
    return Span(pos, ByteOffset(pos + 1));
}

S _stripTrailingSpaces(S: const(char)[ ])(S text) nothrow @nogc {
    import std.algorithm.mutation;
    import std.utf;

    return text.byCodeUnit().stripRight(' ').source;
}

string _processTrailingText(_PatternState state, string laggedText, const(char)[ ] mergedText)
nothrow {
    final switch (state) with (_PatternState) {
    case haveNoText:
        return null;
    case haveText:
        return _stripTrailingSpaces(laggedText);
    case mergingTexts:
        return _stripTrailingSpaces(mergedText).idup;
    }
}

string _sanitize(string s) nothrow @trusted {
    import std.encoding;

    return sanitize(s);
}

struct _Parser {
pure:
    ParserStream ps;
    uint maxDepth;
    Appender!(char[ ]) bufText;
    Appender!(ast.PatternElement[ ]) bufPatternElements;
    Appender!(size_t[ ]) bufLinePtrs; // Indices into `bufPatternElements`.
    Appender!(ast.Attribute[ ]) bufAttrs;
    Appender!(ast.InlineExpression[ ]) bufArgs;
    Appender!(ast.NamedArgument[ ]) bufKwargs;
    Appender!(ast.Variant[ ]) bufVariants;
    bool[string] seenKwargs;

    @property Span curSpan() const nothrow @nogc {
        return _byteAt(ps.pos);
    }

    T throw_(T = void, K: ErrorKind)(K kind, Span span) const {
        throw new _ParserException(ParserError(span, kind));
    }

    T throw_(T = void, K)(K kind, Span span) const {
        return throw_!T(ErrorKind(kind), span);
    }

    T throw_(T = void, K: ErrorKind)(K kind) const {
        return throw_!T(kind, curSpan);
    }

    T throw_(T = void, K)(K kind) const {
        return throw_!T(ErrorKind(kind));
    }

    void clearBuffers() nothrow @trusted {
        bufText.clear();
        bufPatternElements.clear();
        bufLinePtrs.clear();
        // bufAttrs.clear(); // Not used in recursive methods.
        bufArgs.clear();
        bufKwargs.clear();
        bufVariants.clear();
        // We don't hold pointers into the hash table, so it's `@safe` to clear and reuse it.
        seenKwargs.clear();
    }

    void expect(char c) {
        if (!ps.skip(c))
            throw_(err.ExpectedToken(c));
    }

    ast.StringLiteral parseStringLiteral() {
        import std.encoding: validLength;

        const s = ps.skipStringLiteral();
        if (s is null) // TODO: Differentiate errors.
            throw_(err.UnterminatedStringExpression());
        const prefix = validLength(s);
        if (prefix != s.length) {
            // `-1` for closing `"`.
            throw_(err.InvalidUTF(), _byteAt(ByteOffset(ps.pos - 1 - s.length + prefix)));
        }
        return ast.StringLiteral(s);
    }

    ast.NumberLiteral parseNumberLiteral() {
        const start = ps.pos;
        if (!ps.skipNumberLiteral())
            throw_(err.ExpectedCharRange("0-9"));
        return ast.NumberLiteral(ps.slice(start));
    }

    ast.Literal parseLiteral() {
        switch (ps.testInlineExpression()) with (InlineExpressionStart) {
        case stringLiteral:
            return ast.Literal(parseStringLiteral());
        case numberLiteral:
            return ast.Literal(parseNumberLiteral());
        default:
            throw_(err.ExpectedLiteral());
            assert(false);
        }
    }

    T parseIdentifier(T = ast.Identifier)() {
        const s = ps.skipIdentifier();
        if (s.empty)
            throw_(err.ExpectedCharRange("A-Za-z"));
        return T(s);
    }

    ast.VariableReference parseVariableReference()
    in {
        assert(ps.test('$'));
    }
    do {
        ps.skip();
        return ast.VariableReference(parseIdentifier());
    }

    ast.OptionalIdentifier parseAttributeAccessor() {
        if (!ps.skip('.'))
            return ast.OptionalIdentifier.init;
        return parseIdentifier!(ast.OptionalIdentifier);
    }

    ast.CallArguments parseArgumentList() {
        const argsStart = bufArgs.data.length;
        const kwargsStart = bufKwargs.data.length;
        // Works because named arguments may only be literals.
        assert(seenKwargs.empty, "Starting to parse an argument list with non-empty `seenKwargs`");
        scope(success) {
            bufArgs.shrinkTo(argsStart);
            bufKwargs.shrinkTo(kwargsStart);
            // We don't hold pointers into the hash table, so it's `@safe` to clear and reuse it.
            () @trusted { seenKwargs.clear(); }();
        }

        if (!maxDepth--)
            throw_(err.TooDeepNesting());
        scope(success) maxDepth++;

        do {
            ps.skipBlank();
            if (ps.skip(')'))
                goto finish;
            auto arg = parseInlineExpression();
            ps.skipBlank();
            if (ps.skip(':')) {
                // Named argument.
                ast.Identifier argName;
                // Argument's name is parsed as a message reference (without an attribute).
                if (arg.match!(
                    (ref ast.MessageReference mr) {
                        if (!mr.attribute.name.empty)
                            return true;
                        argName = mr.id;
                        return false;
                    },
                    (ref _) => true,
                ))
                    throw_(err.InvalidArgumentName());

                ps.skipBlank();
                const argValue = parseLiteral();
                // Check for duplicates.
                if (argName.name in seenKwargs)
                    throw_(err.DuplicatedNamedArgument(argName.name));
                seenKwargs[argName.name] = true;

                bufKwargs ~= ast.NamedArgument(argName, argValue);
                ps.skipBlank();
            } else {
                // Positional argument.
                if (bufKwargs.data.length != kwargsStart)
                    throw_(err.PositionalArgumentFollowsNamed());
                bufArgs ~= arg;
            }
        } while (ps.skip(','));

        if (ps.skip(')')) {
        finish:
            return ast.CallArguments(
                bufArgs.data[argsStart .. $].dup,
                bufKwargs.data[kwargsStart .. $].dup,
            );
        }
        throw_(err.ExpectedCharRange(",)"));
        assert(false);
    }

    ast.OptionalCallArguments parseCallArguments() {
        ps.skipBlank();
        if (!ps.skip('('))
            return ast.OptionalCallArguments(ast.NoCallArguments());
        return ast.OptionalCallArguments(parseArgumentList());
    }

    ast.TermReference parseTermReference()
    in {
        assert(ps.test('-'));
    }
    do {
        ps.skip();
        return ast.TermReference(parseIdentifier(), parseAttributeAccessor(), parseCallArguments());
    }

    ast.InlineExpression parseMessageOrFunctionReference() {
        const id = ast.Identifier(ps.skipIdentifier());
        const attr = parseAttributeAccessor();
        if (!attr.name.empty)
            return ast.InlineExpression(ast.MessageReference(id, attr));
        return parseCallArguments().match!(
            (ast.NoCallArguments _) =>
                ast.InlineExpression(ast.MessageReference(id)),
            (ref ast.CallArguments ca) {
                if (!isCallee(id.name))
                    throw_(err.ForbiddenCallee());
                return ast.InlineExpression(ast.FunctionReference(id, ca));
            },
        );
    }

    ast.InlineExpression parseInlineExpression() {
        final switch (ps.testInlineExpression()) with (InlineExpressionStart) {
        case stringLiteral:
            return ast.InlineExpression(parseStringLiteral());

        case numberLiteral:
            return ast.InlineExpression(parseNumberLiteral());

        case variableReference:
            return ast.InlineExpression(parseVariableReference());

        case termReference:
            return ast.InlineExpression(parseTermReference());

        case identifier:
            return parseMessageOrFunctionReference();

        case placeable: {
            assert(ps.test('{'));
            ps.skip();
            if (!maxDepth--)
                throw_(err.TooDeepNesting());
            scope(success) maxDepth++;
            return ast.InlineExpression(new ast.Expression(parsePlaceable()));
        }

        case invalid:
            throw_(err.ExpectedInlineExpression());
            assert(false);
        }
    }

    ast.VariantKey parseVariantKeyContent() {
        switch (ps.testInlineExpression()) with (InlineExpressionStart) {
        case numberLiteral:
            return ast.VariantKey(parseNumberLiteral());
        case identifier:
            return ast.VariantKey(ast.Identifier(ps.skipIdentifier()));
        default:
            throw_(err.ForbiddenKey());
            assert(false);
        }
    }

    ast.Variant[ ] parseVariantList() {
        if (!maxDepth--)
            throw_(err.TooDeepNesting());
        const vStart = bufVariants.data.length;
        scope(success) {
            bufVariants.shrinkTo(vStart);
            maxDepth++;
        }

        bool foundDefault;
        while (true) {
            bool default_;
            {
                const wsStart = ps.pos;
                ps.skipBlank();
                if (!ps.skip('[')) { // Test for '[' first.
                    if (!ps.skip('*')) {
                        ps.backtrack(wsStart);
                        break;
                    }
                    if (foundDefault)
                        throw_(err.MultipleDefaultVariants());
                    default_ = foundDefault = true;
                    expect('[');
                }
            }

            ps.assertLast!q{a == '['};
            ps.skipBlank();
            const key = parseVariantKeyContent();
            ps.skipBlank();
            expect(']');

            ps.skipBlankInline();
            parsePattern().match!(
                (ast.NoPattern _) => throw_(err.MissingValue()),
                (ref ast.Pattern pattern) => bufVariants ~= ast.Variant(key, pattern, default_),
            );
        }

        auto result = bufVariants.data[vStart .. $];
        if (result.empty)
            throw_(err.MissingVariants());
        if (!foundDefault)
            throw_(err.MissingDefaultVariant());
        return result.dup;
    }

    ast.Expression parsePlaceable()
    in {
        ps.assertLast!q{a == '{'};
    }
    do {
        ps.skipBlank();
        auto ie = parseInlineExpression();
        ps.skipBlank();
        scope(success) expect('}');
        if (!ps.skipArrow()) {
            ie.match!(
                (ref ast.TermReference tr) {
                    if (!tr.attribute.name.empty)
                        throw_(err.TermAttributeAsPlaceable());
                },
                (ref _) { },
            );
            return ast.Expression(ie);
        }

        ie.match!(
            (ast.StringLiteral _) { },
            (ast.NumberLiteral _) { },
            (ast.VariableReference _) { },
            (ref ast.FunctionReference _) { },
            (ref ast.MessageReference mr) => throw_(
                mr.attribute.name.empty ? (
                    ErrorKind(err.MessageReferenceAsSelector())
                ) : ErrorKind(err.MessageAttributeAsSelector())
            ),
            (ref ast.TermReference tr) {
                if (tr.attribute.name.empty)
                    throw_(err.TermReferenceAsSelector());
            },
            (ast.Expression* _) => throw_(err.ExpectedSimpleExpressionAsSelector()),
        );

        ps.skipBlankInline();
        if (!ps.skipLineEnd())
            throw_(err.ExpectedToken('\n'));
        auto variants = parseVariantList();
        ps.skipBlank();
        return ast.Expression(ast.SelectExpression(ie, variants));
    }

    void appendInlineText(ByteOffset start, ByteOffset end) {
        import std.encoding: validLength;

        const s = ps.slice(start, end);
        const prefix = validLength(s);
        if (prefix != s.length)
            throw_(err.InvalidUTF(), _byteAt(ByteOffset(start + prefix)));
        bufPatternElements ~= ast.PatternElement(ast.TextElement(s));
    }

    void parsePatternLine(ByteOffset inlineTextStart) {
        version (Posix)
            ubyte newlineLength = 1; // \n
        else
            ubyte newlineLength = 2; // \r\n
    loop:
        while (true)
            final switch (ps.skipInlineText()) with (TextElementTermination) {
            case placeableStart:
                appendInlineText(inlineTextStart, ByteOffset(ps.pos - 1));
                bufPatternElements ~= ast.PatternElement(parsePlaceable());
                inlineTextStart = ps.pos;
                continue;

            case lf:
                version (Windows)
                    newlineLength = 1;
                break loop;

            case crlf:
                version (Posix)
                    newlineLength = 2;
                break loop;

            case eof:
                newlineLength = 0;
                break loop;

            case unbalancedBrace:
                throw_(err.UnbalancedClosingBrace(), _byteAt(ByteOffset(ps.pos - 1)));
                assert(false);
            }

        appendInlineText(inlineTextStart, ByteOffset(ps.pos - newlineLength));
    }

    ast.OptionalPattern parsePattern() {
        import std.algorithm.comparison: among, min;

        if (ps.skip('}'))
            throw_(err.UnbalancedClosingBrace(), _byteAt(ByteOffset(ps.pos - 1)));

        const peStart = bufPatternElements.data.length;
        const lpStart = bufLinePtrs.data.length;
        scope(success) {
            bufPatternElements.shrinkTo(peStart);
            bufLinePtrs.shrinkTo(lpStart);
        }

        // Parse the first line.
        parsePatternLine(ps.pos);
        bufLinePtrs ~= bufPatternElements.data.length - peStart;

        // Parse the rest of the pattern.
        size_t nonBlankLines =
            bufPatternElements.data.length - peStart > 1 || bufPatternElements.data[peStart].match!(
                (ref ast.TextElement te) => !te.value.empty,
                (ref _) => _unreachable!bool("The first line of a pattern doesn't start with text"),
            );
        size_t firstNonBlankLine = nonBlankLines - 1;
        size_t commonIndentation = size_t.max;
        while (true) {
            const lineStart = ps.pos;
            const indented = ps.skipBlankInline();
            if (!ps.skipLineEnd()) {
                // A non-blank line.
                if (ps.test!(among!('.', '[', '*', '}'))) {
                    ps.backtrack(lineStart);
                    break;
                } else if (!indented && !ps.test('{'))
                    break;
                commonIndentation = min(commonIndentation, ps.pos - lineStart);
                parsePatternLine(lineStart);

                // `+1` because we append after setting this variable.
                nonBlankLines = bufLinePtrs.data.length - lpStart + 1;
                if (firstNonBlankLine == size_t.max)
                    firstNonBlankLine = nonBlankLines - 1;
            } else if (ps.eof) // Blank lines are ignored, even if they are indented deeper.
                break;
            bufLinePtrs ~= bufPatternElements.data.length - peStart;
        }

        if (!nonBlankLines)
            return ast.OptionalPattern(ast.NoPattern());

        // Dedent, merge adjacent text elements, and remove empty ones.
        _PatternState state;
        string laggedText;
        const textStart = bufText.data.length;
        scope(success) bufText.shrinkTo(textStart);

        auto result = bufPatternElements.data[peStart .. $];
        const linePtrs = bufLinePtrs.data[lpStart .. $];
        size_t r = firstNonBlankLine ? linePtrs[firstNonBlankLine - 1] : 0;
        size_t w;
        // Iterate through parsed lines.
        foreach (lineNumber, nextR; linePtrs[firstNonBlankLine .. nonBlankLines]) {
            // Append a newline unless it's the first line.
            if (lineNumber)
                final switch (state) with (_PatternState) {
                case haveNoText:
                    state = haveText;
                    laggedText = "\n";
                    break;

                case haveText:
                    state = mergingTexts;
                    bufText ~= laggedText;
                    goto case;
                case mergingTexts:
                    bufText ~= '\n';
                    break;
                }

            // Process the line.
            bool atBOL = !!(firstNonBlankLine | lineNumber);
            foreach (ref pe; result[r .. nextR]) {
                pe.match!(
                    (ref ast.TextElement te) {
                        // Dedent unless it's the very first line (directly after `=` or `]`).
                        const content = atBOL ? te.value[commonIndentation .. $] : te.value;
                        atBOL = false;
                        if (content.empty)
                            return;
                        // Merge with previous text.
                        final switch (state) with (_PatternState) {
                        case haveNoText:
                            state = haveText;
                            laggedText = content;
                            break;

                        case haveText:
                            state = mergingTexts;
                            bufText ~= laggedText;
                            goto case;
                        case mergingTexts:
                            bufText ~= content;
                            break;
                        }
                    },
                    (ref ast.Expression e) @trusted {
                        // We don't hold pointers to members of `SumType`s in `result`,
                        // so reassigning them is `@safe`.
                        assert(w <= r, "Not enough space for rewriting the pattern");
                        // Append merged text.
                        final switch (state) with (_PatternState) {
                        case haveNoText: break;
                        case haveText:
                            result[w++] = ast.TextElement(laggedText);
                            break;
                        case mergingTexts:
                            result[w++] = ast.TextElement(bufText.data[textStart .. $].idup);
                            bufText.shrinkTo(textStart);
                            break;
                        }
                        state = _PatternState.haveNoText;

                        assert(w <= r, "Not enough space for rewriting the pattern");
                        result[w++] = pe;
                    },
                );
                version (assert)
                    r++;
            }
            version (assert)
                assert(r == nextR, "Wrong number of iterations of the line parsing loop");
            else
                r = nextR;
        }

        // Append trailing text, stripping spaces from it.
        const content = _processTrailingText(state, laggedText, bufText.data[textStart .. $]);
        if (!content.empty) () @trusted {
            result[w++] = ast.TextElement(content);
        }();

        return ast.OptionalPattern(ast.Pattern(result[0 .. w].dup));
    }

    _NamedPattern parseNamedPattern() {
        const id = parseIdentifier();
        ps.skipBlankInline();
        expect('=');
        ps.skipBlankInline();
        return _NamedPattern(id, parsePattern());
    }

    ast.Attribute[ ] parseAttributes() {
        bufAttrs.clear();
        ByteOffset lineStart;
        while (true) {
            lineStart = ps.pos;
            ps.skipBlankInline();
            if (!ps.skip('.'))
                break;
            auto np = parseNamedPattern();
            np.value.match!(
                (ast.NoPattern _) => throw_(err.MissingValue()),
                (ref ast.Pattern pattern) => bufAttrs ~= ast.Attribute(np.id, pattern),
            );
        }
        ps.backtrack(lineStart);
        return bufAttrs.data.dup;
    }

    _MessageLike parseMessageLike() {
        return _MessageLike(parseNamedPattern(), parseAttributes());
    }

    ast.Message parseMessage() {
        const entryStart = ps.pos;
        auto m = parseMessageLike();
        m.pattern.value.match!(
            (ast.NoPattern _) {
                if (m.attributes.empty)
                    throw_(err.ExpectedMessageField(), Span(entryStart, ps.pos));
            },
            (ref _) { },
        );
        return ast.Message(m.pattern.id, m.pattern.value, m.attributes);
    }

    ast.Term parseTerm()
    in {
        ps.assertLast!q{a == '-'};
    }
    do {
        const entryStart = ps.pos;
        auto m = parseMessageLike();
        return m.pattern.value.match!(
            (ast.NoPattern _) =>
                throw_!(ast.Term)(err.ExpectedTermField(), Span(entryStart, ps.pos)),
            (ref ast.Pattern pattern) => ast.Term(m.pattern.id, pattern, m.attributes),
        );
    }

    ast.AnyComment parseComment()
    in {
        ps.assertLast!q{a == '#'};
    }
    do {
        import std.encoding: isValid, validLength;

        const level = cast(ubyte)(ps.skipCommentSigil(2) + 1);
        string lastLine;
        if (ps.skip(' ')) {
            const commentStart = ps.pos;
            lastLine = ps.skipLine();
            const prefix = validLength(lastLine);
            if (prefix != lastLine.length)
                throw_(err.InvalidUTF(), _byteAt(ByteOffset(commentStart + prefix)));
        } else if (!ps.skipLineEnd())
            throw_(err.ExpectedToken(' '));

        assert(bufText.data.empty, "Starting to parse a comment with non-empty text buffer");
        {
            ByteOffset lineStart;
            while (true) {
                lineStart = ps.pos;
                if (ps.skipCommentSigil(level) != level)
                    break; // A shorter comment (or not a comment at all).
                bufText ~= lastLine;
                if (ps.skip(' ')) {
                    lastLine = ps.skipLine();
                    if (!isValid(lastLine)) {
                        lastLine = null;
                        break;
                    }
                } else {
                    lastLine = null;
                    if (!ps.skipLineEnd())
                        break; // Either a longer comment or a syntax error.
                }
                bufText ~= '\n';
            }
            ps.backtrack(lineStart);
        }
        if (!bufText.data.empty) {
            bufText ~= lastLine;
            lastLine = bufText.data.idup;
            bufText.clear();
        }
        final switch (level) {
            case 1: return ast.AnyComment(ast.Comment(lastLine));
            case 2: return ast.AnyComment(ast.GroupComment(lastLine));
            case 3: return ast.AnyComment(ast.ResourceComment(lastLine));
        }
    }

    ast.Entry parseEntry() {
        if (ps.skip('#'))
            return ast.Entry(parseComment());
        else if (ps.skip('-'))
            return ast.Entry(parseTerm());
        else
            return ast.Entry(parseMessage());
    }

    ast.ResourceEntry parseResourceEntry(ref Appender!(ParserError[ ]) errors, uint initialDepth)
    nothrow {
        const entryStart = ps.pos;
        try
            return ast.ResourceEntry(parseEntry());
        catch (_ParserException e) {
            errors ~= e.err;
            ps.skipJunk();
            clearBuffers();
            maxDepth = initialDepth;
            // Somewhat surprising, junk must also be valid UTF.
            return ast.ResourceEntry(ast.Junk(_sanitize(ps.slice(entryStart))));
        } catch (Exception e)
            assert(false, e.msg);
    }
}

_Parser _createParser(string source, uint maxDepth) nothrow {
    import std.array;

    _Parser p = {
        ParserStream(source),
        maxDepth,
        appender(uninitializedArray!(char[ ])(255)),
        appender(minimallyInitializedArray!(ast.PatternElement[ ])(15)),
        appender(uninitializedArray!(size_t[ ])(7)),
        appender(minimallyInitializedArray!(ast.Attribute[ ])(7)),
        appender(minimallyInitializedArray!(ast.InlineExpression[ ])(7)),
        appender(minimallyInitializedArray!(ast.NamedArgument[ ])(15)),
        appender(minimallyInitializedArray!(ast.Variant[ ])(7)),
    };
    p.clearBuffers();
    return p;
}

public ParserResult parse(string source, uint maxDepth = 100) nothrow {
    auto entries = appender!(ast.ResourceEntry[ ]);
    auto errors = appender!(ParserError[ ]);

    auto p = _createParser(source, maxDepth);
    ast.OptionalComment lastComment;
    while (true) {
        const haveVSpace = p.ps.skipBlankBlock();
        if (p.ps.eof)
            break;

        auto rcEntry = p.parseResourceEntry(errors, maxDepth);
        // Attach preceding comment to a message or term.
        lastComment.match!(
            (ref ast.Comment c) {
                if (haveVSpace || rcEntry.match!(
                    (ref ast.Entry entry) => entry.match!(
                        (ref msgOrTerm) @trusted {
                            assert(msgOrTerm.comment.match!(
                                (ast.NoComment _) => true,
                                (ref _) => false,
                            ), "Attempting to detach a comment");
                            msgOrTerm.comment = lastComment;
                            return false;
                        },
                        _ => true,
                    ),
                    _ => true,
                ))
                    entries ~= ast.ResourceEntry(ast.Entry(ast.AnyComment(c)));
                () @trusted { lastComment = ast.NoComment(); }();
            },
            (ast.NoComment _) { },
        );

        // Append `rcEntry` to `entries` unless it is a `Comment`.
        if (rcEntry.match!(
            (ref ast.Entry entry) => entry.match!(
                (ref ast.AnyComment ac) => ac.match!(
                    (ref ast.Comment c) @trusted {
                        lastComment = c;
                        return false;
                    },
                    _ => true,
                ),
                _ => true,
            ),
            _ => true,
        ))
            entries ~= rcEntry;
    }

    lastComment.match!(
        (ref ast.Comment c) => entries ~= ast.ResourceEntry(ast.Entry(ast.AnyComment(c))),
        (ast.NoComment _) { },
    );

    return ParserResult(ast.Resource(entries.data), errors.data);
}
