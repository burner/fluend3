module fluentd.utils.lexing;

import std.algorithm.comparison: among;
import std.algorithm.searching: all;
import std.range.primitives: empty;
import std.utf: byCodeUnit;

nothrow pure @safe @nogc:

pragma(inline, true) {
    bool isUpper(char c) {
        return uint(c - 'A') < 26u;
    }

    bool isAlpha(char c) {
        return uint((c | 0x20) - 'a') < 26u;
    }

    bool isDigit(char c) {
        return uint(c - '0') < 10u;
    }
}

bool isHexDigit(char c) {
    return isDigit(c) || uint((c | 0x20) - 'a') < 6u;
}

bool isIdent(char c) {
    return isAlpha(c) || isDigit(c) || c.among!('-', '_');
}

bool isCallee(char c) {
    return isUpper(c) || isDigit(c) || c.among!('-', '_');
}

bool isIdent(const(char)[ ] s) {
    return !s.empty && isAlpha(s[0]) && s[1 .. $].byCodeUnit().all!isIdent();
}

bool isCallee(const(char)[ ] s) {
    return !s.empty && isUpper(s[0]) && s[1 .. $].byCodeUnit().all!isCallee();
}
