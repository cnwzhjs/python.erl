-module(py_tokenizer).
-export([tokenize/2, all_errors/1]).

tokenize([], _Position) ->
    [];
tokenize(L, Position) ->
    {Token, Rest, NewPosition} = tokenize_one(L, Position),
    [Token] ++ tokenize(Rest, NewPosition).

% deal with empty list
tokenize_one([], Position) ->
    {{eof, Position, Position}, [], Position};
% deal with long strings
tokenize_one([M, M, M | Rest], {Filename, Line, Column}) when (M == $') or (M == $") ->
    match_large_string(M, [], Rest, {Filename, Line, Column}, {Filename, Line, Column+3});
% deal with strings
tokenize_one([Q | Rest], {Filename, Line, Column}) when (Q == $') or (Q == $") ->
    match_string(Q, [], Rest, {Filename, Line, Column}, {Filename, Line, Column+1});
% deal with caret return
tokenize_one([$\r | Rest], {Filename, Line, Column}) ->
    tokenize_one(Rest, {Filename, Line, Column+1});
% deal with new line
tokenize_one([$\n | Rest], {Filename, Line, Column}) ->
    {{new_line, {Filename, Line, Column}}, Rest, {Filename, Line+1, 1}};
% deal with spaces
tokenize_one([S | Rest], {Filename, Line, 1}) when (S == $\s) or (S == $\t) ->
    match_leading_space([S], Rest, {Filename, Line, 1}, {Filename, Line, 2});
tokenize_one([S | Rest], {Filename, Line, Column}) when (S == $\s) or (S == $\t) ->
    tokenize_one(Rest, {Filename, Line, Column+1});
% deal with line comment
tokenize_one([$# | Rest], Position) ->
    match_line_comment(Rest, Position);
% deal with symbols
tokenize_one([C | Rest], {Filename, Line, Column}) when (C == $_) or ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)) ->
    match_symbol([C], Rest, {Filename, Line, Column}, {Filename, Line, Column+1});
% deal with numbers
tokenize_one([$., N | Rest], {Filename, Line, Column}) when (N >= $0) and (N =< $9) ->
    match_number([$., N], Rest, {Filename, Line, Column}, {Filename, Line, Column+2});
tokenize_one([N | Rest], {Filename, Line, Column}) when (N >= $0) and (N =< $9) ->
    match_number([N], Rest, {Filename, Line, Column}, {Filename, Line, Column+1});
% deal with punctuations
tokenize_one([$*, $* | Rest], {Filename, Line, Column})->
    {{punctuation, {Filename, Line, Column}, "**"}, Rest, {Filename, Line, Column+2}};
tokenize_one([$>, $= | Rest], {Filename, Line, Column})->
    {{punctuation, {Filename, Line, Column}, ">="}, Rest, {Filename, Line, Column+2}};
tokenize_one([$=, $> | Rest], {Filename, Line, Column})->
    {{punctuation, {Filename, Line, Column}, ">="}, Rest, {Filename, Line, Column+2}};
tokenize_one([$<, $= | Rest], {Filename, Line, Column})->
    {{punctuation, {Filename, Line, Column}, "<="}, Rest, {Filename, Line, Column+2}};
tokenize_one([$=, $< | Rest], {Filename, Line, Column})->
    {{punctuation, {Filename, Line, Column}, "<="}, Rest, {Filename, Line, Column+2}};
tokenize_one([$!, $= | Rest], {Filename, Line, Column})->
    {{punctuation, {Filename, Line, Column}, "!="}, Rest, {Filename, Line, Column+2}};
tokenize_one([$=, $= | Rest], {Filename, Line, Column})->
    {{punctuation, {Filename, Line, Column}, "=="}, Rest, {Filename, Line, Column+2}};
tokenize_one([C | Rest], {Filename, Line, Column})
    when (C == $:) or (C == $=) or (C == $+) or (C == $-) or (C == $*) or (C == $/) or (C == $\\)
         or (C == $() or (C == $)) or (C == $.) or (C == $,) or (C == $[) or (C == $]) or (C == ${)
         or (C == $}) or (C == $>) or (C == $<) or (C == $@) or (C == $%) or (C == $;)
    -> {{punctuation, {Filename, Line, Column}, C}, Rest, {Filename, Line, Column+1}};
% the rest
tokenize_one([C | Rest], {Filename, Line, Column}) ->
    {{error, {Filename, Line, Column}, unexpected_char, C}, Rest, {Filename, Line, Column+1}}.

match_large_string(Mark, Parsed, [Mark, Mark, Mark | Rest], StartPosition, {Filename, Line, Column}) ->
    {{string, StartPosition, Parsed}, Rest, {Filename, Line, Column+3}};
match_large_string(Mark, Parsed, [$\n | Rest], StartPosition, {Filename, Line, _Column}) ->
    match_large_string(Mark, Parsed ++ [$\n], Rest, StartPosition, {Filename, Line+1, 1});
match_large_string(Mark, Parsed, [C | Rest], StartPosition, {Filename, Line, Column}) ->
    match_large_string(Mark, Parsed ++ [C], Rest, StartPosition, {Filename, Line, Column+1});
match_large_string(_Mark, _Parsed, [], _StartPosition, Position) ->
    {{error, Position, unexpected_end_of_file}, [], Position}.

translate_char(C) ->
    case C of
        $n->$\n;
        $t->$\t;
        $r->$\r;
        $\\->$\\;
        $/->$/;
        $'->$';
        $"->$";
        _Any->illegal
    end.

match_string(Mark, Parsed, [Mark | Rest], StartPosition, {Filename, Line, Column}) ->
    {{string, StartPosition, Parsed}, Rest, {Filename, Line, Column+1}};
match_string(Mark, Parsed, [$\\, C | Rest], StartPosition, {Filename, Line, Column}) when (C == $x) or (C == $u)->
    match_hex_string(Mark, Parsed, [], Rest, StartPosition, {Filename, Line, Column+2});
match_string(Mark, Parsed, [$\\, C | Rest], StartPosition, {Filename, Line, Column}) ->
    case translate_char(C) of
        illegal->{{error, {Filename, Line, Column+1}, unexpected_char, C}, Rest, {Filename, Line, Column+2}};
        TranslatedC->match_string(Mark, Parsed ++ [TranslatedC], Rest, StartPosition, {Filename, Line, Column+2})
    end;
match_string(_Mark, _Parsed, [$\n | Rest], _StartPosition, {Filename, Line, Column}) ->
    {{error, {Filename, Line, Column}, unexpected_end_of_line}, Rest, {Filename, Line+1, 1}};
match_string(Mark, Parsed, [C | Rest], StartPosition, {Filename, Line, Column}) ->
    match_string(Mark, Parsed ++ [C], Rest, StartPosition, {Filename, Line, Column+1}).

match_hex_string(Mark, Parsed, ParsedHex, [N | Rest], StartPosition, {Filename, Line, Column}) when ((N >= $0) and (N =< $9)) or ((N >= $a) and (N =< $f)) or ((N >= $A) and (N =< $F))->
    match_hex_string(Mark, Parsed, ParsedHex ++ [N], Rest, StartPosition, {Filename, Line, Column+1});
match_hex_string(_Mark, _Parsed, [], [_Mark | Rest], _StartPosition, {Filename, Line, Column}) ->
    {{error, {Filename, Line, Column}, unexpected_end_of_string}, Rest, {Filename, Line, Column}};
match_hex_string(_Mark, _Parsed, [], [C | Rest], _StartPosition, {Filename, Line, Column}) ->
    {{error, {Filename, Line, Column}, unexpected_char, C}, Rest, {Filename, Line, Column+1}};
match_hex_string(Mark, Parsed, ParsedHex, Rest, StartPosition, Position) ->
    match_string(Mark, Parsed ++ [list_to_integer(ParsedHex, 16)], Rest, StartPosition, Position).


match_leading_space(Parsed, [$\s | Rest], StartPosition, {Filename, Line, Column}) ->
    match_leading_space(Parsed ++ [$\s], Rest, StartPosition, {Filename, Line, Column+1});
match_leading_space(Parsed, [$\t | Rest], StartPosition, {Filename, Line, Column}) ->
    match_leading_space(Parsed ++ [$\t], Rest, StartPosition, {Filename, Line, Column+1});
match_leading_space(Parsed, Rest, StartPosition, Position) ->
    {{leading_space, StartPosition, Parsed}, Rest, Position}.


match_line_comment([$\n | Rest], {Filename, Line, _Column}) ->
    tokenize_one(Rest, {Filename, Line + 1, 1});
match_line_comment([_ | Rest], {Filename, Line, Column}) ->
    match_line_comment(Rest, {Filename, Line, Column+1}).


symbol_type(Symbol) ->
    case Symbol of
        "class"->keyword;
        "def"->keyword;
        "end"->keyword;
        "import"->keyword;
        "pass"->keyword;
		"global"->keyword;
        "from"->keyword;
        "for"->keyword;
        "in"->keyword;
        "if"->keyword;
        "else"->keyword;
        "elif"->keyword;
        "and"->keyword;
        "or"->keyword;
        "not"->keyword;
        "as"->keyword;
        _->symbol
    end.

match_symbol(Parsed, [C | Rest], StartPosition, {Filename, Line, Column}) when (C == $_) or ((C >= $0) and (C =< $9)) or ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)) ->
    match_symbol(Parsed ++ [C], Rest, StartPosition, {Filename, Line, Column+1});
match_symbol(Parsed, Rest, StartPosition, Position) ->
    {{symbol_type(Parsed), StartPosition, Parsed}, Rest, Position}.


match_number(Parsed, [N | Rest], StartPosition, {Filename, Line, Column}) when (N >= $0) and (N =< $9) ->
    match_number(Parsed ++ [N], Rest, StartPosition, {Filename, Line, Column+1});
match_number(Parsed, [$. | Rest], StartPosition, {Filename, Line, Column})->
    match_float(Parsed ++ [$.], Rest, StartPosition, {Filename, Line, Column+1});
match_number([$0], [$x | Rest], StartPosition, {Filename, Line, Column})->
    match_hex("0x", Rest, StartPosition, {Filename, Line, Column+1});
match_number(Parsed, Rest, StartPosition, Position)->
    {{integer, StartPosition, Parsed}, Rest, Position}.

match_float(Parsed, [N | Rest], StartPosition, {Filename, Line, Column}) when (N >= $0) and (N =< $9) ->
    match_float(Parsed ++ [N], Rest, StartPosition, {Filename, Line, Column+1});
match_float(Parsed, Rest, StartPosition, Position) ->
    {{float, StartPosition, Parsed}, Rest, Position}.

match_hex(Parsed, [N | Rest], StartPosition, {Filename, Line, Column}) when ((N >= $0) and (N =< $9)) or ((N >= $a) and (N =< $z)) or ((N >= $A) and (N =< $Z)) ->
    match_hex(Parsed ++ [N], Rest, StartPosition, {Filename, Line, Column+1});
match_hex(Parsed, Rest, StartPosition, Position) ->
    {{hex, StartPosition, Parsed}, Rest, Position}.

all_errors([]) ->
    [];
all_errors([X | Rest]) ->
    case X of
        {error, Position, Reason}->[{error, Position, Reason} | all_errors(Rest)];
        {error, Position, Reason, Relative}->[{error, Position, Reason, Relative} | all_errors(Rest)];
        _Other->all_errors(Rest)
    end.
