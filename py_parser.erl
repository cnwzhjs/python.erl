-module(py_parser).
-export([parse/1]).

% utilities
begin_with(_Any, []) -> true;
begin_with([A | ARest], [A | BRest]) -> begin_with(ARest, BRest).

get_position(TokenOrStatement) ->
    lists:nth(2, tuple_to_list(TokenOrStatement)).

indicate_position(na, []) ->
    na;
indicate_position(na, [Statement | _]) ->
    get_position(Statement);
indicate_position(StartPosition, _) ->
    StartPosition.

parse(Tokens) ->
    parse_statement_block("", false, na, [], [], Tokens).

parse_statement_block(_LeadingSpace, _RequireIndent, StartPosition, Statements, [], []) ->
    {ok, {statement_block, indicate_position(StartPosition, Statements), Statements}, []};
parse_statement_block(_LeadingSpace, _RequireIndent, StartPosition, Statements, Errors, []) ->
    {error, {statement_block, indicate_position(StartPosition, Statements), Statements}, Errors, []};
parse_statement_block(LeadingSpace, RequireIndent, StartPosition, Statements, Errors, Tokens) ->
    case parse_statement(LeadingSpace, RequireIndent, Tokens) of
        {empty_line, Rest} ->
            parse_statement_block(LeadingSpace, RequireIndent, StartPosition, Statements, Errors, Rest);
        {ok, Statement, Rest} ->
            parse_statement_block(LeadingSpace, false, StartPosition, Statements ++ [Statement], Errors, Rest);
        {end_of_block, Rest} ->
            NewErrors = case Statements of
                [] ->
                    case Tokens of
                        [] -> Errors ++ [{error, StartPosition, unexpected_end_of_file}];
                        [Token | _] -> Errors ++ [{error, get_position(Token), unexpected_token, Token}]
                    end;
                _ -> Errors
            end,
            case NewErrors of
                [] -> {ok, {statement_block, indicate_position(StartPosition, Statements), Statements}, Rest};
                _ -> {error, {statement_block, indicate_position(StartPosition, Statements), Statements}, NewErrors, Rest}
            end;
        {error, NewErrors, Rest} ->
            parse_statement_block(LeadingSpace, RequireIndent, StartPosition, Statements, Errors ++ NewErrors, Rest);
        {error, Statement, NewErrors, Rest} ->
            parse_statement_block(LeadingSpace, RequireIndent, StartPosition, Statements ++ [Statement], Errors ++ NewErrors, Rest)
    end.

% deal with empty lines
parse_statement(_LeadingSpace, _RequireIndent, [{new_line, _} | Rest]) ->
    {empty_line, Rest};
parse_statement(_LeadingSpace, _RequireIndent, [{leading_space, _SpacePosition, _Space}, {new_line, _} | Rest]) ->
    {empty_line, Rest};
parse_statement(LeadingSpace, RequireIndent, [{leading_space, SpacePosition, Space} | Rest]) ->
    case {begin_with(LeadingSpace, Space), begin_with(Space, LeadingSpace), RequireIndent} of
        {true, true, false} ->
            do_parse_statement(Rest);
        {true, true, true} ->
            {end_of_block, [{leading_space, SpacePosition, Space} | Rest]};
        {true, false, _} ->
            {end_of_block, [{leading_space, SpacePosition, Space} | Rest]};
        {false, true, _} ->
            parse_statement_block(Space, false, SpacePosition, [], [], Rest);
        {false, false, _} ->
            {error, [{error, SpacePosition, illegal_indent}], Rest}
    end;
parse_statement(_, true, [Token | Rest]) ->
    {error, [{error, get_position(Token), unexpected_token, Token}], Rest};
parse_statement("", false, Rest) ->
    do_parse_statement(Rest);
parse_statement(_, false, Rest) ->
    {end_of_block, Rest}.

% parse import statements
do_parse_statement([{keyword, ImportPosition, "import"} | Rest]) ->
    case parse_qualified_name_list(Rest) of
        {ok, Names, _, [{new_line, _} | NewRest]} ->
            {ok, {import, ImportPosition, Names}, NewRest};
        {ok, Names, _, [{keyword, AsPosition, "as"}, {symbol, AsSymbolPosition, AsName} | NewRest]} ->
            case Names of
                [Name] ->
                    case NewRest of
                        [{new_line, _} | RestAfterNewLine] ->
                            {ok, {import_as, ImportPosition, Name, AsName}, RestAfterNewLine};
                        [Token | RestAfterIllegalToken] ->
                            {error, [{error, get_position(Token), unexpected_token, Token}], RestAfterIllegalToken}
                    end;
                _ ->
                    {error, [{error, AsPosition, unexpected_token, {keyword, AsPosition, "as"}}], [{symbol, AsSymbolPosition, AsName} | NewRest]}
            end;
        {ok, _, _, [Token | NewRest]} ->
            {error, [{error, get_position(Token), unexpected_token, Token}], NewRest};
        Error ->
            Error
    end;
do_parse_statement([{keyword, FromPosition, "from"} | Rest]) ->
    case parse_qualified_name(Rest) of
        {ok, Name, _, [{keyword, _, "import"}, {punctuation, _, $*}, {new_line, _} | NewRest]} ->
            {ok, {import_from_all, FromPosition, Name}, NewRest};
        {ok, Name, _, [{keyword, _, "import"} | NewRest]} ->
            case parse_name_list(NewRest) of
                {ok, [ImportName], _, [{keyword, _, "as"}, {symbol, _, AsName}, {new_line, _} | NewRestAfterImport]} ->
                    {ok, {import_from_as, FromPosition, Name, ImportName, AsName}, NewRestAfterImport};
                {ok, Names, _, [{new_line, _} | NewRestAfterImport]} ->
                    {ok, {import_from, FromPosition, Name, Names}, NewRestAfterImport};
                {ok, _, _, [Token | NewRestAfterImport]} ->
                    {error, [{error, get_position(Token), unexpected_token, Token}], NewRestAfterImport};
                Error -> Error
            end;
        {ok, _, _, [Token | NewRest]} ->
            {error, [{error, get_position(Token), unexpected_token, Token}], NewRest};
        Error ->
            Error
    end;
do_parse_statement([Token | Rest]) ->
    {error, [{error, get_position(Token), unexpected_token, Token}], Rest}.

% parse assign expression
%do_parse_statement([{symbol, Position, Symbol}, {punctuation, PunctuationPosition, AssignPunctuation} | Rest])
%    when (AssignPunctuation == "=") or (AssignPunctuation == "+=") or (AssignPunctuation == "-=") or (AssignPunctuation == "*=") or (AssignPunctuation == "/=")
%    -> case parse_expression(Rest) of
%        {ok, Expression, []} ->
%            {ok, {assign, Position, AssignPunctuation, Symbol, Expression}, []};
%        {ok, Expression, [{new_line, _} | Rest]} ->
%            {ok, {assign, Position, AssignPunctuation, Symbol, Expression}, Rest};
%        {ok, Expression, [Token | Rest]} ->
%            {error, [{error, get_position(Token), unexpected_token, Token}], Rest};
%        Other->Other
%    end;
%
%do_parse_statement([Token | Rest]) ->
%    {error, [{error, get_position(Token), unexpected_token, Token}]}.
%
%parse_expression()

parse_qualified_name([{symbol, Position, Name}, {punctuation, _, $.} | Rest]) ->
    case parse_qualified_name(Rest) of
        {ok, AfterName, _, NewRest} ->
            {ok, [Name | AfterName], Position, NewRest};
        Error ->
            Error
    end;
parse_qualified_name([{symbol, Position, Name} | Rest]) ->
    {ok, [Name], Position, Rest};
parse_qualified_name([Token | Rest]) ->
    {error, [{error, get_position(Token), unexpected_token, Token}], Rest}.

parse_name_list([{symbol, Position, Name}, {punctuation, _, $,} | Rest]) ->
    case parse_name_list(Rest) of
        {ok, Names, _, NewRest} ->
            {ok, [Name | Names], Position, NewRest};
        Error ->
            Error
    end;
parse_name_list([{symbol, Position, Name} | Rest]) ->
    {ok, [Name], Position, Rest};
parse_name_list([Token | Rest]) ->
    {error, [{error, get_position(Token), unexpected_token, Token}], Rest}.

parse_qualified_name_list(Tokens) ->
    case parse_qualified_name(Tokens) of
        {ok, Name, Position, [{punctuation, _, $,} | Rest]} ->
            case parse_qualified_name_list(Rest) of
                {ok, Names, _, NewRest} ->
                    {ok, [Name | Names], Position, NewRest};
                Error ->
                    Error
            end;
        {ok, Name, Position, Rest} ->
            {ok, [Name], Position, Rest};
        Error ->
            Error
    end.
