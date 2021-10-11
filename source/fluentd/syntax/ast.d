module fluentd.syntax.ast;

import std.meta: staticIndexOf;
import std.range.primitives: empty;

import sumtype;

@safe:

/+
    Expressions:
+/
struct TextElement {
    string value;
}

struct Identifier {
    string name;

    invariant {
        assert(!name.empty, "Empty identifier");
    }
}

struct OptionalIdentifier {
    string name;
}

struct StringLiteral {
    string value;
}

struct NumberLiteral {
    string value;

    invariant {
        assert(!value.empty, "Empty number literal");
    }
}

alias Literal = SumType!(StringLiteral, NumberLiteral);

struct NoCallArguments { }

struct NamedArgument {
    Identifier name;
    Literal value;
}

struct CallArguments {
    InlineExpression[ ] positional;
    NamedArgument[ ] named;
}

alias OptionalCallArguments = SumType!(NoCallArguments, CallArguments);

struct FunctionReference {
    Identifier id;
    CallArguments arguments;
}

struct MessageReference {
    Identifier id;
    OptionalIdentifier attribute;
}

struct TermReference {
    Identifier id;
    OptionalIdentifier attribute;
    OptionalCallArguments arguments;
}

struct VariableReference {
    Identifier id;
}


struct InlineExpression {
nothrow pure @nogc:
    SumType!(
        StringLiteral,
        NumberLiteral,
        FunctionReference,
        MessageReference,
        TermReference,
        VariableReference,
        Expression*,
    ) value;

    alias value this;

    invariant {
        value.match!(
            (const(Expression)* e) {
                assert(e !is null, "Null `Expression` as part of `InlineExpression`");
            },
            (ref _) { },
        );
    }

    this(typeof(value) x) { value = x; }

    this(T)(T x) if (staticIndexOf!(T, typeof(value).Types) >= 0) {
        value = x;
    }
}

alias VariantKey = SumType!(Identifier, NumberLiteral);

alias PatternElement = SumType!(TextElement, Expression);

struct Pattern {
    PatternElement[] elements;

    invariant {
        assert(!elements.empty, "Empty pattern");
    }
}

struct Variant {
    VariantKey key;
    Pattern value;
    bool default_;
}

struct SelectExpression {
    InlineExpression selector;
    Variant[ ] variants;

    invariant {
        selector.match!(
            (ref const MessageReference _) { assert(false, "Message reference as a selector"); },
            (ref const TermReference tr) =>
                assert(!tr.attribute.name.empty, "Term reference as a selector"),
            (const(Expression)* _) { assert(false, "Placeable as a selector"); },
            (ref _) { },
        );
        assert(!variants.empty, "Empty selection");
    }
}

/+
    DMD's support for recursive templates is quite restricted.
    For example, these declarations are sane, but, unfortunately, do not compile:

    struct A { }
    struct B { }
    alias C = SumType!(A, D*);
    alias D = SumType!(B, C);

    We have to manually break the template chain by defining a struct.
+/
struct Expression {
nothrow pure @nogc:
    SumType!(InlineExpression, SelectExpression) value;

    alias value this;

    this(typeof(value) x) { value = x; }

    this(T)(T x) if (staticIndexOf!(T, typeof(value).Types) >= 0) {
        value = x;
    }
}
/+
    End of expressions.
+/

struct NoComment { }

struct Comment {
    string content;
}

struct GroupComment {
    string content;
}

struct ResourceComment {
    string content;
}

alias OptionalComment = SumType!(NoComment, Comment);
alias AnyComment = SumType!(Comment, GroupComment, ResourceComment);

struct Attribute {
    Identifier id;
    Pattern value;
}

struct NoPattern { }

alias OptionalPattern = SumType!(NoPattern, Pattern);

struct Message {
    Identifier id;
    OptionalPattern value;
    Attribute[ ] attributes;
    OptionalComment comment;

    invariant {
        value.match!(
            (NoPattern _) => assert(!attributes.empty, "Empty message"),
            (ref _) { },
        );
    }
}

struct Term {
    Identifier id;
    Pattern value;
    Attribute[ ] attributes;
    OptionalComment comment;
}

alias Entry = SumType!(Message, Term, AnyComment);

struct Junk {
    string content;

    invariant {
        assert(!content.empty, "Empty junk");
    }
}

alias ResourceEntry = SumType!(Junk, Entry);

struct Resource {
    ResourceEntry[ ] body;
}
