printf "\n\n-------------- String  --------------"
cabal run scheme "\"this is a string\""
cabal run scheme "\"\\\"string with backward slash\""
cabal run scheme "\"\\nstring with escaped n\""
cabal run scheme "\"\\\\string with escaped \\\\ \""

printf "\n\n-------------- Symbol  --------------"
cabal run scheme "symbol"
cabal run scheme symbol
cabal run scheme "(symbol)" 

printf "\n\n-------------- Numbers  --------------"
cabal run scheme 123adsfds
cabal run scheme 12323423423423423423423
cabal run scheme 0xAbc
cabal run scheme 0o1230
cabal run scheme 0b01010
cabal run scheme 0b1235
cabal run scheme 0xAby

printf "\n\n-------------- Characters  --------------"
cabal run scheme "#\\A"
cabal run scheme "#\\ "
cabal run scheme "#\\b"
cabal run scheme "#\\*"
cabal run scheme "#\\8"

printf "\n\n-------------- Float  --------------"
cabal run scheme 123.3244
cabal run scheme 123.324asb
cabal run scheme 3..3244


printf "\n\n-------------- List  --------------"
cabal run scheme "(a \"test\")"
cabal run scheme "(a (nested) test)"
cabal run scheme "(a (dotted . list) test)"
cabal run scheme "(a (double dotted . list) test)"
cabal run scheme "(a '(quote (dotted . list)) test)"
cabal run scheme "(a '(imbalanced parens)"
cabal run scheme "'(1 3 (\"this\" \"one\"))"
