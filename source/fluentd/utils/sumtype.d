module fluentd.utils.sumtype;

// https://github.com/pbackus/sumtype/issues/31
template case_(T, alias f) {
    static if (__traits(compiles, f()))
        auto case_()(ref const T _) { return f(); } // With no argument.
    else static if (__traits(compiles, f(T.init)))
        auto case_()(ref const T x) { return f(x); } // By value.
    else
        auto case_()(ref T x) { return f(x); } // By ref.
}

// A shortcut for a no-op handler.
template case_(T) {
    void case_()(ref const T _) { }
}
