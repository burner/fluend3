module fluentd.gen;

import std.array;
import std.algorithm;
import std.conv : to;
import std.ascii : toUpper;
import std.format;
import std.typecons : Nullable, nullable;
import std.stdio;
import std.traits : FieldNameTuple, isArray, isSomeString;


import fluentd.syntax.parser;
import fluentd.syntax.ast;
import sumtype;

string toCamelCase(string name) @safe {
	auto app = appender!string();
	app.put("tr");

	bool nextUpper = true;
	for(size_t i = 0; i < name.length; ++i) {
		if(nextUpper && name[i] != '-') {
			app.put(toUpper(name[i]));
			nextUpper = false;
		} else if(name[i] == '-') {
			nextUpper = true;
		} else {
			app.put(name[i]);
		}
	}
	return app.data();
}

string genString(string s, string fun) {
	return format(" (%s a) => a.%s()", s, fun);
}

template genImpl(string s, Args...) {
	static if(Args.length == 1) {
		enum genImpl = genString(Args[0].stringof, s);
	} else {
		enum genImpl = genString(Args[0].stringof, s)
			~ "\n\t," ~ genImpl!(s, Args[1 .. $]);
	}
}

string gen(string s, T)() {
	string gen = "// " ~ T.stringof ~ "\nauto ret = input.match!\n\t(";
	static if(is(T : SumType!F, F...)) {
		gen ~= genImpl!(s, F);
	}
	gen ~= "\n\t)();";
	return gen;
}

VariantKey[] getVariantKeys(T)(T input) {
	static if(is(T == Junk)) {
		return [];
	} else static if(is(T == SumType!(Identifier, NumberLiteral))) {
		return [ input ];
	} else static if(is(T == SumType!F, F...)) {
		mixin(gen!("getVariantKeys", T));
		return ret;
	} else static if(is(T == struct)) {
		VariantKey[] ret;
		static if(FieldNameTuple!(T).length > 0) {
			foreach(mem; FieldNameTuple!T) {
				static if(mem != "") {
					alias MemT = typeof(__traits(getMember, T, mem));
					static if(isArray!MemT && !isSomeString!MemT) {
						foreach(it; __traits(getMember, input, mem)) {
							ret ~= it.getVariantKeys();
						}
					} else {
						ret ~= __traits(getMember, input, mem).getVariantKeys();
					}
				}
			}
		}
		return ret;
	} else static if(is(T == Expression*)) {
		if(input !is null) {
			return (*input).getVariantKeys();
		} else {
			return [];
		}
	} else {
		pragma(msg, T.stringof);
		return [];
	}
}

SelectExpression[] getSelectExpression(T)(T input) {
	static if(is(T == SumType!F, F...)) {
		mixin(gen!("getSelectExpression", T));
		return ret;
	} else static if(is(T == SelectExpression)) {
		return [input] ~ input.variants
			.map!(i => i.getSelectExpression())
			.joiner
			.array;
	} else static if(is(T == struct)) {
		SelectExpression[] ret;
		static if(FieldNameTuple!(T).length > 0) {
			foreach(mem; FieldNameTuple!T) {
				static if(mem != "") {
					alias MemT = typeof(__traits(getMember, T, mem));
					static if(isArray!MemT && !isSomeString!MemT) {
						foreach(it; __traits(getMember, input, mem)) {
							ret ~= it.getSelectExpression();
						}
					} else {
						ret ~= __traits(getMember, input, mem).getSelectExpression();
					}
				}
			}
		}
		return ret;
	} else static if(is(T == Expression*)) {
		return input !is null
			? (*input).getSelectExpression()
			: [];
	} else {
		pragma(msg, T.stringof);
		return [];
	}
}

immutable numericStrings = [ "zero", "one", "two", "few", "many", "other"];

bool allSelectorsAreNumeric(ref const(SelectExpression) se) {
	return se.variants.map!(it => it.key)
		.map!(vk => 
			vk.match! 
				( (Identifier i) => canFind(numericStrings, i.name)
				, (NumberLiteral nl) => true
				)
		)
		.all;
}

alias AllReferences = SumType!
	( StringLiteral
	, NumberLiteral
	, FunctionReference
	, MessageReference
	, TermReference
	, VariableReference);

AllReferences getAllReferences(T)(T input) {
}

unittest {
	string r = toCamelCase("tabs-close-warning-multiple");
	assert(r == "trTabsCloseWarningMultiple", r);
}

void gen(Out)(ref Out o, Junk j) {
}

void gen(Out)(ref Out o, Message m) {
	gen(o, m.comment);
	string funcName = m.id.name.toCamelCase();
	formattedWrite(o, "%s:", funcName);	
	formattedWrite(o, " -> String\n");
	formattedWrite(o, "%s", funcName);	
	formattedWrite(o, " =\n");
	writefln("attributes %s", m.attributes);
	writefln("value %s", m.value);
}

void gen(Out)(ref Out o, Term t) {
}

void gen(Out)(ref Out o, AnyComment ac) {
}

void gen(Out)(ref Out o, OptionalComment oc) {
	oc.match!
		( (Comment c) => formattedWrite(o, "-- %s\n", c.content)
		, (NoComment _) => 0U
		);
}

void gen(Out)(ref Out o, Entry e) {
	e.match!
		( (Message m) => gen(o, m)
		, (Term t) => gen(o, t)
		, (AnyComment ac) => gen(o, ac)
		);
}

void gen(Out)(ref Out o, ResourceEntry r) {
	r.match!
		( (Junk j) => gen(o, j)
		, (Entry e) => gen(o, e)
		);
}

void gen(Out)(ref Out o, string modName, Resource r) {
	foreach(ref rb; r.body) {
		gen(o, rb);
	}
}

unittest {
	string input = 
`
shared-photos =
    {$userName} {$photoCount ->
        [one] added a new photo
       *[other] added {$photoCount} new photos
    } to {$userGender ->
        [male] his stream
        [female] her stream
       *[other] their stream {$args ->
            [foo] hello
           *[bar] world
        }
    }.
`;

	auto p = parse(input);
	//writeln(p, "\n");
	auto ltw = stdout.lockingTextWriter();
	gen(ltw, "test", p.resource);
	foreach(it; getSelectExpression(p.resource)) {
		writefln("it %s", it);
		writeln(allSelectorsAreNumeric(it));
		writeln("\n\n\n");
	}
}
