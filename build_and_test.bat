set PATH=%PATH%;C:\Runtime\erl5.9.3.1\bin

erl -noshell -eval "make:all(),eunit:test(py_tokenizer_test)."

pause