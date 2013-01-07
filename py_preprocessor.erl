% A preprocessor used to remove the ';', and make lines connected with \ into a single line
% The preprocessor also break lines with ';' into separate code lines to simplify the parser
% The preprocessor will add a new_line to the end of the token stream if there is not one to simplify the parser

-module(py_preprocessor).
-export([preprocess/1]).

preprocess([]) ->
    [];
preprocess(Tokens) ->
    Size = length(Tokens),
    LastToken = lists:nth(Size, Tokens),
    case LastToken of
        {new_line, _} -> preprocess(none, Tokens);
        _ ->
            {{Filename, Line, Column}, ContentSize} = case LastToken of
                {_, Position} -> {Position, 0};
                {_, Position, Content} when is_list(Content) -> {Position, length(Content)};
                {_, Position, _} -> {Position, 1}
            end,
            preprocess(none, Tokens ++ [{new_line, {Filename, Line, Column + ContentSize}}])
    end.

preprocess(_, []) ->
    [];
preprocess(_, [{leading_space, Position, Space} | Rest]) ->
    [{leading_space, Position, Space} | preprocess({leading_space, Position, Space}, Rest)];
% Semicolons in python is optional, but it is illegal if it is just after a colon, so just left it in the token stream, and the parser will detect the error
preprocess(LeadingSpace, [{punctuation, ColonPosition, $:}, {punctuation, SemicolonPosition, $;} | Rest]) ->
    [{punctuation, ColonPosition, $:}, {punctuation, SemicolonPosition, $;} | preprocess(LeadingSpace, Rest)];
preprocess(LeadingSpace, [{punctuation, _, $;}, {new_line, Position} | Rest]) ->
    [{new_line, Position} | preprocess(LeadingSpace, Rest)];
preprocess(none, [{punctuation, Position, $;} | Rest])->
    [{new_line, Position} | preprocess(none, Rest)];
preprocess({leading_space, SpacePosition, Space}, [{punctuation, Position, $;} | Rest]) ->
    [{new_line, Position}, {leading_space, Position, Space} | preprocess({leading_space, SpacePosition, Space}, Rest)];
preprocess(LeadingSpace, [{punctuation, _, $\\}, {new_line, _}, {leading_space, _, _} | Rest]) ->
    preprocess(LeadingSpace, Rest);
preprocess(LeadingSpace, [{punctuation, _, $\\}, {new_line, _} | Rest]) ->
    preprocess(LeadingSpace, Rest);
preprocess(LeadingSpace, [Token | Rest]) ->
    [Token | preprocess(LeadingSpace, Rest)].
