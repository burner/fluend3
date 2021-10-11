module fluentd.syntax.parser.span;

struct ByteOffset {
    size_t value;

    alias value this;
}

struct Span {
    ByteOffset start, end;

    invariant {
        assert(start <= end, "Reverse spans are not supported");
    }
}
