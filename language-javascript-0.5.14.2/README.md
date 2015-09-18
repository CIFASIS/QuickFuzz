Parser for JavaScript
---------------------

[![Build Status](https://secure.travis-ci.org/alanz/language-javascript.png?branch=master)](http://travis-ci.org/alanz/language-javascript)

Based (loosely) on language-python

How to build
------------

Library:

    cabal clean && cabal configure && cabal build

Tests:

    cabal clean && cabal configure -fbuildtests && cabal build

Running the tests

    ./dist/build/runtests/runtests


To debug the grammar

    happy -iparse.txt -g -a  -d src/Language/JavaScript/Parser/Grammar5.y

This generates src/Language/JavaScript/Parser/Grammar.hs, delete this
when done with the debug version


UTF8/Unicode version
--------------------

Alex 3.0 now supports unicode natively, and has been included as a
dependency in the cabal file.

Note: The generation of the lexical analyser has been separated out,
      to remove the install-time dependency on Alex. If any changes
      need to be made to the lexer, the Lexer.x source lies in
      src-dev, and the runalex.sh script will invoke Alex with the
      appropriate directories.

Changes
-------

```
0.5.14.2 Update Alex dependency

0.5.14.1 Fix haddocks

0.5.14.0 Fix a break/continue/return parsing problem

0.5.13.3 Fix utf8-string lower bound typo no 2

0.5.13.2 Fix utf8-string lower bound typo

0.5.13.1 Bump utf8-string dependency upper bound, and loosen others

0.5.13 Put GHC 7.8.x -specific alex and happy versions, thanks
       @simonmichael

0.5.12 Put dependency on alex >= 3.0.5 in the cabal file, thanks @peti

0.5.11 Remove pre-generated Lexer.hs as it does not work will all
       versions of GHC. This means a current alex will have to be
       installed before this package can be installed.

0.5.10 Bring in alex 3.1.3, allowing compatibility with GHC 7.8

0.5.9 Relax array dependency for GHC 7.8.1, thanks @maoe

0.5.8 Fixed compilation issue using GHC 7.6.3 on Mac OSX, thanks @albertov

0.5.7 Remove the hs-source-dirs from test suite to prevent compilation
      issues (@nomeata)

      Introduce parseFileUtf8 to explicitly use utf8 for parsing a
      file, and update tests to use it where needed. Closes #21

0.5.6 Remove constraint on Alex 3.0.1, it is only required to make
      changes to the lexer. Closes #19

0.5.5 Updated version ranges for GHC 7.6.1, courtesy of @mietek
      Note: requires alex 3.0.1 for development (not install)

0.5.4 Fixed bug where lexer switches to regexp mode after a ']'
      char, reported by @aszlig

0.5.3 Merge pull requests from @aszlig to add octal support, and to
      allow leading zeros in exponents

0.5.2 Merged pull request from @markwright added some missing test
      files to the cabal file.
      Added runalex.sh and Lexer.x to the cabal file.

0.5.1 Export CommentAnnotation(..). Simplify AST by getting rid of
      JSFunctionBody, JSSourceElements, JSStatementBlock, JSStatementList.
      They are replaced by JSBlock or a simple list.
      Also fix lexer mode in presence of whitespace.
      Changed way tests are invoked, to allow Travis integration.

0.5.0 Rework AST to allow full round-trip parsing and output of
      JavaScript. Breaks AST compatibility with prior versions

0.4.10 Moved Lexer.x into a separate directory, and made a script to
       call alex to generate Lexer.hs. This means alex is not required
       at install time

0.4.9 Make alex and happy versions more explicit. Expose the AlexSpan
      data type, so parse error positions can be reported.

0.4.8 Close issue https://github.com/alanz/language-javascript/issues/5 by
      correcting the lexical analyser for DecimalLiteral

0.4.7 Continue ECMASCRIPT update, remove incorrect future reserved words for
      "code", "mode", "of", "one", "or" and "strict", put in by accident/stupidity.

0.4.6 Update to ECMASCRIPT 5.1 by allowing continuations in string literals
      Include build dependencies on alex >= 3.0 and happy.

0.4.5 Update cabal file and docs for Alex 3.0, giving out of the box unicode support

0.4.4 Allow unicode characters in comments

0.4.3 correct lexer for decimalToken, only leading digit needs to be non-zero.

0.4.2 Expose SrcSpan

0.4.1 Expose AST.Node as well.

0.4.0 Updated JSNode to include location information, and made the parse more true to the original by leaving blocks as such, not replacing with ';'.

0.3.0 ECMA-262 edition 5 compliance.  Required adding JSFunctionExpression and JSPropertyAccessor to the AST.

0.2.2 Heading toward compliance with edition 5. Trailing commas allowed in array literals too.

0.2.1 Allow trailing comma in object literal

0.2.0 ECMAScript 3 allows function expressions to have names, AST.JSFunctionExpression now reflects this

0.1.0 Simplified AST by removing JSElement and JSElementList components

0.0.3 Support for unicode in source. At the moment it only supports
UTF8 encoding, does not recognise byte order marks or UTF-16/UTF-32

0.0.2 Multiline comments were processed in greedy form, now end of
comment recognised properly. Thanks to Tony Morris for reporting this.

0.0.1 Initial release
```

EOF

