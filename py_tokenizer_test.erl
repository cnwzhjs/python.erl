-module(py_tokenizer_test).
-include_lib("eunit/include/eunit.hrl").

read_all_content(Filename) ->
    case file:open(Filename, [read, binary]) of
        {ok, Device} -> scan_file(Device);
        {error, Reason} -> {error, Reason}
    end.

scan_file(Device) -> scan_file(Device, []).
scan_file(Device, Read) ->
    case file:read(Device, 1024) of
        {ok, Chars} -> scan_file(Device, Read ++ binary_to_list(Chars));
        eof -> file:close(Device), {ok, Read}
    end.

test_file(Filename, ShortName) ->
    Result = case read_all_content(Filename) of
        {ok, Source} ->
            Tokens = py_tokenizer:tokenize(Source, {ShortName, 1, 1}),
            py_tokenizer:all_errors(Tokens);
        {error, Reason} ->
            {error, Reason};
        Other->Other
    end,
    ?assertMatch([], Result).

tokenize_string_test() ->
    Tokens = py_tokenizer:tokenize("\"Hello, world\"", {"Unnamed", 1, 1}),
    ?assertMatch([{string, {"Unnamed", 1, 1}, "Hello, world"}], Tokens).

tokenize_string_single_test() ->
    Tokens = py_tokenizer:tokenize("\'Hello, \\nworld\'", {"Unnamed", 1, 1}),
    ?assertMatch([{string, {"Unnamed", 1, 1}, "Hello, \nworld"}], Tokens).

%content_scripts_const_test() ->
%    Filename="test/const.py",
%    test_file(Filename, "const.py").
