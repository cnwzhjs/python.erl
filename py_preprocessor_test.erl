-module(py_preprocessor_test).
-include_lib("eunit/include/eunit.hrl").

% used to debug
print_tokens([]) ->
    ok;
print_tokens([Token | Rest]) ->
    erlang:display(Token),
    print_tokens(Rest).

preprocessor_test() ->
    Tokens = py_tokenizer:tokenize("def hello():\n    name=\"Tony\";\n    print name; createUser(name)", {"Unnamed", 1, 1}),
    PreprocessedTokens = py_preprocessor:preprocess(Tokens),
    %print_tokens(PreprocessedTokens),
    ?assertMatch([
        {keyword, {"Unnamed", 1, 1}, "def"},
        {symbol, {"Unnamed", 1, 5}, "hello"},
        {punctuation, {"Unnamed", 1, 10}, $(},
        {punctuation, {"Unnamed", 1, 11}, $)},
        {punctuation, {"Unnamed", 1, 12}, $:},
        {new_line, {"Unnamed", 1, 13}},
        {leading_space, {"Unnamed", 2, 1}, "    "},
        {symbol, {"Unnamed", 2, 5}, "name"},
        {punctuation, {"Unnamed", 2, 9}, $=},
        {string, {"Unnamed", 2, 10}, "Tony"},
        {new_line, {"Unnamed", 2, 17}},
        {leading_space, {"Unnamed", 3, 1}, "    "},
        {symbol, {"Unnamed", 3, 5}, "print"},
        {symbol, {"Unnamed", 3, 11}, "name"},
        {new_line, {"Unnamed", 3, 15}},
        {leading_space, {"Unnamed", 3, 15}, "    "},
        {symbol, {"Unnamed", 3, 17}, "createUser"},
        {punctuation, {"Unnamed", 3, 27}, $(},
        {symbol, {"Unnamed", 3, 28}, "name"},
        {punctuation, {"Unnamed", 3, 32}, $)},
        {new_line, {"Unnamed", 3, 33}}
    ], PreprocessedTokens).