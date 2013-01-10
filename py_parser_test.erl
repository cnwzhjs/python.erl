-module(py_parser_test).
-include_lib("eunit/include/eunit.hrl").

parse_single_import_test() ->
    Code = "import graphics",
    Tokens = py_tokenizer:tokenize(Code, {"Unnamed", 1, 1}),
    PreprocessedTokens = py_preprocessor:preprocess(Tokens),
    Ast = py_parser:parse(PreprocessedTokens),
    ?assertMatch({ok, {
        statement_block,
        {"Unnamed", 1, 1},
        [{import, {"Unnamed", 1, 1}, [["graphics"]]}]
    }, []}, Ast).

parse_single_qualified_name_import_test() ->
    Code = "import ui.ogml;",
    Tokens = py_tokenizer:tokenize(Code, {"Unnamed", 1, 1}),
    PreprocessedTokens = py_preprocessor:preprocess(Tokens),
    Ast = py_parser:parse(PreprocessedTokens),
    ?assertMatch({ok, {
        statement_block,
        {"Unnamed", 1, 1},
        [{import, {"Unnamed", 1, 1}, [["ui", "ogml"]]}]
    }, []}, Ast).

parse_qualified_name_list_import_test() ->
    Code = "import ui, ui.ogml, graphics, ui.ogml.parsers",
    Ast = py_parser:parse(py_preprocessor:preprocess(py_tokenizer:tokenize(Code, {"Unnamed", 1, 1}))),
    ?assertMatch({ok, {
        statement_block,
        {"Unnamed", 1, 1},
        [{import, {"Unnamed", 1, 1}, [["ui"], ["ui", "ogml"], ["graphics"], ["ui", "ogml", "parsers"]]}]
    }, []}, Ast).

parse_from_all_test() ->
    Code = "from ui import *",
    Ast = py_parser:parse(py_preprocessor:preprocess(py_tokenizer:tokenize(Code, {"Unnamed", 1, 1}))),
    ?assertMatch({ok, {
        statement_block,
        {"Unnamed", 1, 1},
        [{import_from_all, {"Unnamed", 1, 1}, ["ui"]}]
    }, []}, Ast).

parse_from_qualified_name_import_name_list_test() ->
    Code = "from ui.ogml import OgmlLoader, PropertyRef",
    Ast = py_parser:parse(py_preprocessor:preprocess(py_tokenizer:tokenize(Code, {"Unnamed", 1, 1}))),
    ?assertMatch({ok, {
        statement_block,
        {"Unnamed", 1, 1},
        [{import_from, {"Unnamed", 1, 1}, ["ui", "ogml"], ["OgmlLoader", "PropertyRef"]}]
    }, []}, Ast).

parse_import_as_test() ->
    Code = "import game.consts as C",
    Ast = py_parser:parse(py_preprocessor:preprocess(py_tokenizer:tokenize(Code, {"Unnamed", 1, 1}))),
    ?assertMatch({ok, {
        statement_block,
        {"Unnamed", 1, 1},
        [{import_as, {"Unnamed", 1, 1}, ["game", "consts"], "C"}]
    }, []}, Ast).

parse_from_import_as_test() ->
    Code = "from game import consts as C",
    Ast = py_parser:parse(py_preprocessor:preprocess(py_tokenizer:tokenize(Code, {"Unnamed", 1, 1}))),
    ?assertMatch({ok, {
        statement_block,
        {"Unnamed", 1, 1},
        [{import_from_as, {"Unnamed", 1, 1}, ["game"], "consts", "C"}]
    }, []}, Ast).

parse_multi_line_imports_test() ->
    Code = "import ui.ogml\r\nimport graphics\r\nimport platform\r\nfrom ui.ogml import OgmlLoader",
    Ast = py_parser:parse(py_preprocessor:preprocess(py_tokenizer:tokenize(Code, {"Unnamed", 1, 1}))),
    ?assertMatch({ok, {
        statement_block,
        {"Unnamed", 1, 1},
        [
            {import, {"Unnamed", 1, 1}, [["ui", "ogml"]]},
            {import, {"Unnamed", 2, 1}, [["graphics"]]},
            {import, {"Unnamed", 3, 1}, [["platform"]]},
            {import_from, {"Unnamed", 4, 1}, ["ui", "ogml"], ["OgmlLoader"]}
        ]
    }, []}, Ast).