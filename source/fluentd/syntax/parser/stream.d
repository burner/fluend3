module fluentd.syntax.parser.stream;

import std.algorithm.comparison: among;
import std.functional: unaryFun;
import std.traits: ifTestable;

import fluentd.syntax.parser.span;
import fluentd.utils.lexing;

nothrow pure @safe @nogc:

enum TextElementTermination: ubyte {
    placeableStart,
    lf,
    crlf,
    eof,
    unbalancedBrace,
}

enum InlineExpressionStart: ubyte {
    stringLiteral,
    numberLiteral,
    variableReference,
    termReference,
    identifier,
    placeable,
    invalid,
}

private bool _isEntryStart(char c) {
    return isAlpha(c) || c.among!('-', '#');
}

struct ParserStream {
nothrow pure @nogc:
    private {
        string _source;
        ByteOffset _pos;
    }

    invariant {
        assert(_pos <= _source.length);
    }

    this(string source) {
        _source = source;
    }

    @property string source() const {
        return _source;
    }

    @property ByteOffset pos() const {
        return _pos;
    }

    @property bool eof() const {
        return _pos == _source.length;
    }

    void backtrack(ByteOffset point)
    in {
        assert(point <= _pos, "Cannot backtrack forwards");
    }
    do {
        _pos = point;
    }

    string slice(ByteOffset from) const {
        return _source[from .. _pos];
    }

    string slice(ByteOffset from, ByteOffset to) const {
        return _source[from .. to];
    }

    // DMD's optimizer is not as advanced as LDC's, so we need to give it a hint.
    private mixin template _fastAccess() {
        const s = _source;
        auto i = _pos;
    }

    void assertLast(alias pred)() const if (ifTestable!(typeof(unaryFun!pred('x')))) {
        assert(_pos && unaryFun!pred(_source[_pos - 1]), "Lookbehind failed");
    }

    bool test(char c) const {
        mixin _fastAccess;
        return i < s.length && s[i] == c;
    }

    bool test(alias pred)() const if (ifTestable!(typeof(unaryFun!pred('x')))) {
        mixin _fastAccess;
        return i < s.length && unaryFun!pred(s[i]);
    }

    void skip() {
        _pos++;
    }

    bool skip(char c) {
        mixin _fastAccess;
        if (i == s.length || s[i] != c)
            return false;
        _pos = i + 1;
        return true;
    }

    bool skip(alias pred)() if (ifTestable!(typeof(unaryFun!pred('x')))) {
        mixin _fastAccess;
        if (i == s.length || !unaryFun!pred(s[i]))
            return false;
        _pos = i + 1;
        return true;
    }

    bool skipArrow() {
        mixin _fastAccess;
        if (i + 1 >= s.length || s[i] != '-' || s[i + 1] != '>')
            return false;
        _pos = i + 2;
        return true;
    }

    bool skipLineEnd() {
        mixin _fastAccess;
        if (i == s.length)
            return true;
        const c = s[i];
        if (c == '\n') {
            _pos = i + 1;
            return true;
        } else if (c == '\r' && i + 1 < s.length && s[i + 1] == '\n') {
            _pos = i + 2;
            return true;
        }
        return false;
    }

    bool skipBlankInline() {
        mixin _fastAccess;
        if (i == s.length || s[i] != ' ')
            return false;
        scope(success) _pos = i;
        while (++i < s.length && s[i] == ' ') { }
        return true;
    }

    bool skipBlankBlock() {
        mixin _fastAccess;
        scope(success) _pos = i;
        bool found;
        while (true) {
            const lineStart = i;
            while (true) {
                if (i == s.length)
                    return true;
                const c = s[i];
                if (c != ' ') {
                    if ((c != '\r' || ++i < s.length) && s[i] == '\n')
                        break;
                    i = lineStart;
                    return found;
                }
                i++;
            }
            found = true;
            i++; // \n
        }
    }

    void skipBlank() {
        mixin _fastAccess;
        scope(success) _pos = i;
        while (i < s.length) {
            const c = s[i];
            if (!c.among!(' ', '\n')) {
                if (c == '\r' && i + 1 < s.length && s[i + 1] == '\n') {
                    i += 2;
                    continue;
                }
                break;
            }
            i++;
        }
    }

    void skipJunk() {
        mixin _fastAccess;
        if (i == s.length)
            return;
        char c = s[i];
        if ((!i || s[i - 1] == '\n') && _isEntryStart(c))
            return;
        scope(success) _pos = i;
        do {
            while (true) {
                if (++i == s.length)
                    return;
                if (c == '\n')
                    break;
                c = s[i];
            }
            c = s[i];
        } while (!_isEntryStart(c));
    }

    ubyte skipCommentSigil(ubyte limit) {
        import std.algorithm.comparison: min;

        mixin _fastAccess;
        scope(success) _pos = i;
        const start = i;
        const end = min(s.length, i + limit);
        while (i < end && s[i] == '#')
            i++;
        assert(i - start <= limit, "Too many sigil characters consumed");
        return cast(ubyte)(i - start);
    }

    string skipLine() {
        mixin _fastAccess;
        scope(success) _pos = i;
        const start = i;
        while (i < s.length)
            if (s[i++] == '\n')
                return s[start .. i - (i >= 2 && s[i - 2] == '\r' ? 2 : 1)];
        return s[start .. $];
    }

    string skipIdentifier() {
        mixin _fastAccess;
        if (i == s.length || !isAlpha(s[i]))
            return null;
        scope(success) _pos = i;
        const start = i;
        while (++i < s.length && isIdent(s[i])) { }
        return s[start .. i];
    }

    TextElementTermination skipInlineText() {
        mixin _fastAccess;
        scope(success) _pos = i;
        while (i < s.length) {
            const c = s[i++];
            with (TextElementTermination)
                if (c == '\n')
                    return i >= 2 && s[i - 2] == '\r' ? crlf : lf;
                else if (c == '{')
                    return placeableStart;
                else if (c == '}')
                    return unbalancedBrace;
        }
        return TextElementTermination.eof;
    }

    InlineExpressionStart testInlineExpression() const {
        mixin _fastAccess;
        with (InlineExpressionStart) {
            if (i == s.length)
                return invalid;
            char c = s[i];
            if (c == '$')
                return variableReference;
            if (c == '"')
                return stringLiteral;
            if (isAlpha(c))
                return identifier;
            if (isDigit(c))
                return numberLiteral;
            if (c == '-') {
                if (i + 1 < s.length) {
                    c = s[i + 1];
                    if (isAlpha(c))
                        return termReference;
                    if (isDigit(c))
                        return numberLiteral;
                }
            } else if (c == '{')
                return placeable;
            return invalid;
        }
    }

    // Returns `null` on failure.
    // TODO: Differentiate errors.
    string skipStringLiteral()
    in {
        assert(testInlineExpression() == InlineExpressionStart.stringLiteral);
    }
    do {
        mixin _fastAccess;
        assert(s[i] == '"');
        scope(success) _pos = i;
        const start = i + 1;
        while (++i < s.length) {
            char c = s[i];
            if (c == '"')
                return s[start .. i++];
            else if (c == '\\') {
                if (++i == s.length)
                    return null; // Unknown escape sequence (EOF).
                c = s[i];
                if (c.among!('"', '\\'))
                    continue;

                ubyte n = void;
                if (c == 'u')
                    n = 4;
                else if (c == 'U')
                    n = 6;
                else
                    return null; // Unknown escape sequence.
                if (i + n >= s.length)
                    return null; // Invalid unicode escape sequence (EOF).
                do
                    if (!isHexDigit(s[++i]))
                        return null; // Invalid unicode escape sequence.
                while (--n);
            } else if (c == '\n') // Don't need to handle `\r\n` specially.
                return null; // Unterminated string expression.
        }
        return null; // Unterminated string expression (EOF).
    }

    bool skipNumberLiteral()
    in {
        assert(testInlineExpression() == InlineExpressionStart.numberLiteral);
    }
    do {
        mixin _fastAccess;
        scope(success) _pos = i;
        // Minus sign.
        if (s[i] == '-')
            i++;
        // Integer part.
        assert(isDigit(s[i]));
        while (true) {
            if (++i == s.length)
                return true;
            const c = s[i];
            if (!isDigit(c)) {
                if (c != '.')
                    return true;
                break;
            }
        }
        // Fractional part.
        if (++i == s.length || !isDigit(s[i]))
            return false; // A number cannot end with a dot in Fluent.
        while (++i < s.length && isDigit(s[i])) { }
        return true;
    }
}
