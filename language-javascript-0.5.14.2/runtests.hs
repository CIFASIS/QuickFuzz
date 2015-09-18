
import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar5
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.Parser

import Data.List (intercalate)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


main :: IO ()
main = defaultMain
    [ lexerSuite
    , parserSuite
    -- ++AZ++temporary++ , commentSuite
    , commentPrintSuite
    , pendingSuite
    ]

pendingSuite :: Test
pendingSuite = testGroup "Pending"
    [
    ]

lexerSuite:: Test
lexerSuite = testGroup "Lexer"
    [ testCase "assign1"    (testLexer "x=1"            "[IdentifierToken,SimpleAssignToken,DecimalToken]")
    , testCase "assign2"    (testLexer "x=1\ny=2"       "[IdentifierToken,SimpleAssignToken,DecimalToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]")
    , testCase "break"      (testLexer "break\nx=1"     "[BreakToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]")
    , testCase "return"     (testLexer "return\nx=1"    "[ReturnToken,WsToken,IdentifierToken,SimpleAssignToken,DecimalToken]")
    ]

parserSuite :: Test
parserSuite = testGroup "Parser"
    [ testCase "helloWorld"        caseHelloWorld
    , testCase "LiteralNull"       (testLiteral "null"     "Right (JSLiteral \"null\")")
    , testCase "LiteralFalse"      (testLiteral "false"    "Right (JSLiteral \"false\")")
    , testCase "LiteralTrue"       (testLiteral "true"     "Right (JSLiteral \"true\")")
    , testCase "LiteralHexInteger1" (testLiteral "0x1234fF" "Right (JSHexInteger \"0x1234fF\")")
    , testCase "LiteralHexInteger2" (testLiteral "0X1234fF" "Right (JSHexInteger \"0X1234fF\")")
    , testCase "LiteralDecimal1"   (testLiteral "1.0e4"    "Right (JSDecimal \"1.0e4\")")
    , testCase "LiteralDecimal2"   (testLiteral "2.3E6"    "Right (JSDecimal \"2.3E6\")")
    , testCase "LiteralDecimal3"   (testLiteral "4.5"      "Right (JSDecimal \"4.5\")")
    , testCase "LiteralDecimal3"   (testLiteral "0.7e8"    "Right (JSDecimal \"0.7e8\")")
    , testCase "LiteralDecimal4"   (testLiteral "0.7E8"    "Right (JSDecimal \"0.7E8\")")
    , testCase "LiteralDecimal5"   (testLiteral "10"       "Right (JSDecimal \"10\")")
    , testCase "LiteralDecimal6"   (testLiteral "0"        "Right (JSDecimal \"0\")")
    , testCase "LiteralDecimal7"   (testLiteral "0.03"     "Right (JSDecimal \"0.03\")")
    , testCase "LiteralDecimal9"   (testLiteral "0.7e+8"   "Right (JSDecimal \"0.7e+8\")")
    , testCase "LiteralDecimal10"  (testLiteral "0.7e-18"  "Right (JSDecimal \"0.7e-18\")")
    , testCase "LiteralDecimal11"  (testLiteral "1.0e+4"   "Right (JSDecimal \"1.0e+4\")")
    , testCase "LiteralDecimal12"  (testLiteral "1.0e-4"   "Right (JSDecimal \"1.0e-4\")")
    , testCase "LiteralDecimal13"  (testLiteral "1e18"     "Right (JSDecimal \"1e18\")")
    , testCase "LiteralDecimal14"  (testLiteral "1e+18"    "Right (JSDecimal \"1e+18\")")
    , testCase "LiteralDecimal15"  (testLiteral "1e-18"    "Right (JSDecimal \"1e-18\")")
    , testCase "LiteralDecimal16"  (testLiteral "1E-01"    "Right (JSDecimal \"1E-01\")")

    , testCase "LiteralOctal"      (testLiteral "010"      "Right (JSOctal \"010\")")

    , testCase "LiteralString1"    (testLiteral "\"hello\\nworld\"" "Right (JSStringLiteral '\"' \"hello\\\\nworld\")")
    , testCase "LiteralString2"    (testLiteral "'hello\\nworld'"  "Right (JSStringLiteral '\\'' \"hello\\\\nworld\")")

    , testCase "LiteralThis"       (testPE "this"  "Right (JSLiteral \"this\")")

    , testCase "LiteralRegex1"     (testPE "/blah/"  "Right (JSRegEx \"/blah/\")")
    , testCase "LiteralRegex2"     (testPE "/$/g"    "Right (JSRegEx \"/$/g\")")
    , testCase "LiteralRegex3"     (testPE "/\\n/g"  "Right (JSRegEx \"/\\\\n/g\")")
    , testCase "LiteralRegex4"     (testPE "/^\"(?:\\.|[^\"])*\"|^'(?:[^']|\\.)*'/" "Right (JSRegEx \"/^\\\"(?:\\\\.|[^\\\"])*\\\"|^'(?:[^']|\\\\.)*'/\")")

    , testCase "Identifier1"       (testPE "_$"      "Right (JSIdentifier \"_$\")")
    , testCase "Identifier2"       (testPE "this_"   "Right (JSIdentifier \"this_\")")

    , testCase "ArrayLiteral1"     (testPE "[]"      "Right (JSArrayLiteral [])")
    , testCase "ArrayLiteral2"     (testPE "[,]"     "Right (JSArrayLiteral [JSElision JSLiteral \",\"])")
    , testCase "ArrayLiteral3"     (testPE "[,,]"    "Right (JSArrayLiteral [JSElision JSLiteral \",\",JSElision JSLiteral \",\"])")
    , testCase "ArrayLiteral4"     (testPE "[,,x]"   "Right (JSArrayLiteral [JSElision JSLiteral \",\",JSElision JSLiteral \",\",JSIdentifier \"x\"])")
    , testCase "ArrayLiteral5"     (testPE "[,,x]"   "Right (JSArrayLiteral [JSElision JSLiteral \",\",JSElision JSLiteral \",\",JSIdentifier \"x\"])")
    , testCase "ArrayLiteral6"     (testPE "[,x,,x]" "Right (JSArrayLiteral [JSElision JSLiteral \",\",JSIdentifier \"x\",JSElision JSLiteral \",\",JSElision JSLiteral \",\",JSIdentifier \"x\"])")
    , testCase "ArrayLiteral7"     (testPE "[x]"     "Right (JSArrayLiteral [JSIdentifier \"x\"])")
    , testCase "ArrayLiteral8"     (testPE "[x,]"    "Right (JSArrayLiteral [JSIdentifier \"x\",JSLiteral \",\"])")

    , testCase "ObjectLiteral1"    (testPE "{}"        "Right (JSObjectLiteral [])")
    , testCase "ObjectLiteral2"    (testPE "{x:1}"     "Right (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"x\") [JSDecimal \"1\"]])")
    , testCase "ObjectLiteral3"    (testPE "{x:1,y:2}" "Right (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"x\") [JSDecimal \"1\"],JSLiteral \",\",JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"2\"]])")

    , testCase "ObjectLiteral4"    (testPE "{evaluate:evaluate,load:function load(s){if(x)return s;1}}"
                                    "Right (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"evaluate\") [JSIdentifier \"evaluate\"],JSLiteral \",\",JSPropertyNameandValue (JSIdentifier \"load\") [JSFunctionExpression [JSIdentifier \"load\"] [JSIdentifier \"s\"] (JSBlock ([JSIf (JSExpression [JSIdentifier \"x\"]) ([JSReturn [JSExpression [JSIdentifier \"s\"]] JSLiteral \";\"]) ([]),JSExpression [JSDecimal \"1\"]]))]])")

    , testCase "ObjectLiteral5"    (testPE "{x:1,}"    "Right (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"x\") [JSDecimal \"1\"],JSLiteral \",\"])")

    , testCase "ObjectLiteral6"    (testProg "a={\n  values: 7,\n}\n" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"a\",JSOperator JSLiteral \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"values\") [JSDecimal \"7\"],JSLiteral \",\"]],JSLiteral \"\"])")

    -- Edition 5 extensions
    , testCase "ObjectLiteral7"    (testProg "x={get foo() {return 1},set foo(a) {x=a}}" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSObjectLiteral [JSPropertyAccessor NT (JSLiteral \"get\") (TokenPn 3 1 4) [NoComment] (JSIdentifier \"foo\") [] (JSBlock ([JSReturn [JSExpression [JSDecimal \"1\"]] JSLiteral \"\"])),JSLiteral \",\",JSPropertyAccessor NT (JSLiteral \"set\") (TokenPn 24 1 25) [NoComment] (JSIdentifier \"foo\") [JSIdentifier \"a\"] (JSBlock ([JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSIdentifier \"a\"]]))]],JSLiteral \"\"])")

    , testCase "ObjectLiteral8"    (testProg "a={if:1,interface:2}" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"a\",JSOperator JSLiteral \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"if\") [JSDecimal \"1\"],JSLiteral \",\",JSPropertyNameandValue (JSIdentifier \"interface\") [JSDecimal \"2\"]]],JSLiteral \"\"])")

    , testCase "ExpressionParen"   (testPE "(56)"     "Right (JSExpressionParen (JSExpression [JSDecimal \"56\"]))")

    , testCase "Statement1"        (testStmt "x"        "Right (JSExpression [JSIdentifier \"x\"])")
    , testCase "Statement2"        (testStmt "null"     "Right (JSExpression [JSLiteral \"null\"])")
    , testCase "Statement3"        (testStmt "true?1:2" "Right (JSExpression [JSExpressionTernary [JSLiteral \"true\"] [JSDecimal \"1\"] [JSDecimal \"2\"]])")

    , testCase "Statement4"        (testStmt "x||y"     "Right (JSExpression [JSExpressionBinary \"||\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement5"        (testStmt "x&&y"     "Right (JSExpression [JSExpressionBinary \"&&\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement6"        (testStmt "x|y"     "Right (JSExpression [JSExpressionBinary \"|\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement6"        (testStmt "x^y"     "Right (JSExpression [JSExpressionBinary \"^\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement7"        (testStmt "x&y"     "Right (JSExpression [JSExpressionBinary \"&\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement8"        (testStmt "x==y"     "Right (JSExpression [JSExpressionBinary \"==\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement9"        (testStmt "x!=y"     "Right (JSExpression [JSExpressionBinary \"!=\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement10"       (testStmt "x===y"     "Right (JSExpression [JSExpressionBinary \"===\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement11"       (testStmt "x!==y"     "Right (JSExpression [JSExpressionBinary \"!==\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement12"       (testStmt "x<y"     "Right (JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement12"       (testStmt "x>y"     "Right (JSExpression [JSExpressionBinary \">\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement12"       (testStmt "x<=y"     "Right (JSExpression [JSExpressionBinary \"<=\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement12"       (testStmt "x>=y"     "Right (JSExpression [JSExpressionBinary \">=\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    -- , testCase "Statement12"       (testStmt "x instanceof y"     "") -- TODO: restore test case

    , testCase "Statement13"       (testStmt "x<<y"     "Right (JSExpression [JSExpressionBinary \"<<\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement13"       (testStmt "x>>y"     "Right (JSExpression [JSExpressionBinary \">>\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement13"       (testStmt "x>>>y"     "Right (JSExpression [JSExpressionBinary \">>>\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement14"       (testStmt "x+y"     "Right (JSExpression [JSExpressionBinary \"+\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement14"       (testStmt "x-y"     "Right (JSExpression [JSExpressionBinary \"-\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement15"       (testStmt "x*y"     "Right (JSExpression [JSExpressionBinary \"*\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement16"       (testStmt "x/y"     "Right (JSExpression [JSExpressionBinary \"/\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")
    , testCase "Statement17"       (testStmt "x%y"     "Right (JSExpression [JSExpressionBinary \"%\" [JSIdentifier \"x\"] [JSIdentifier \"y\"]])")

    , testCase "Statement18"       (testStmt "delete y"  "Right (JSExpression [JSUnary \"delete \",JSIdentifier \"y\"])")
    , testCase "Statement19"       (testStmt "void y"    "Right (JSExpression [JSUnary \"void \",JSIdentifier \"y\"])")
    , testCase "Statement20"       (testStmt "typeof y"  "Right (JSExpression [JSUnary \"typeof \",JSIdentifier \"y\"])")
    , testCase "Statement21"       (testStmt "++y"    "Right (JSExpression [JSUnary \"++\",JSIdentifier \"y\"])")
    , testCase "Statement22"       (testStmt "--y"    "Right (JSExpression [JSUnary \"--\",JSIdentifier \"y\"])")
    , testCase "Statement23"       (testStmt "+y"     "Right (JSExpression [JSUnary \"+\",JSIdentifier \"y\"])")
    , testCase "Statement24"       (testStmt "-y"     "Right (JSExpression [JSUnary \"-\",JSIdentifier \"y\"])")
    , testCase "Statement25"       (testStmt "~y"     "Right (JSExpression [JSUnary \"~\",JSIdentifier \"y\"])")
    , testCase "Statement26"       (testStmt "!y"     "Right (JSExpression [JSUnary \"!\",JSIdentifier \"y\"])")

    , testCase "Statement27"       (testStmt "y++"     "Right (JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"y\"]])")
    , testCase "Statement28"       (testStmt "y--"     "Right (JSExpression [JSExpressionPostfix \"--\" [JSIdentifier \"y\"]])")

      -- Member Expressions
    , testCase "MemberExpression1a" (testStmt "function(){}"    "Right (JSExpression [JSFunctionExpression [] [] (JSBlock ([]))])")
    , testCase "MemberExpression1b" (testStmt "function(a){}"   "Right (JSExpression [JSFunctionExpression [] [JSIdentifier \"a\"] (JSBlock ([]))])")
    , testCase "MemberExpression1c" (testStmt "function(a,b){}" "Right (JSExpression [JSFunctionExpression [] [JSIdentifier \"a\",JSLiteral \",\",JSIdentifier \"b\"] (JSBlock ([]))])")

    , testCase "MemberExpression1d" (testStmt "x[y]"     "Right (JSExpression [JSMemberSquare [JSIdentifier \"x\"] (JSExpression [JSIdentifier \"y\"])])")
    , testCase "MemberExpression1e" (testStmt "x[y][z]"  "Right (JSExpression [JSMemberSquare [JSMemberSquare [JSIdentifier \"x\"] (JSExpression [JSIdentifier \"y\"])] (JSExpression [JSIdentifier \"z\"])])")
    , testCase "MemberExpression1f" (testStmt "x.y"      "Right (JSExpression [JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")])")
    , testCase "MemberExpression1g" (testStmt "x.y.z"    "Right (JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")] (JSIdentifier \"z\")])")

    , testCase "MemberExpression1h" (testStmt "new x()"  "Right (JSExpression [JSLiteral \"new\",JSIdentifier \"x\",JSArguments []])")

    , testCase "NewExpression1" (testStmt "new x.y"  "Right (JSExpression [JSLiteral \"new\",JSMemberDot [JSIdentifier \"x\"] (JSIdentifier \"y\")])")

    , testCase "CallExpression1" (testStmt "x()"     "Right (JSExpression [JSIdentifier \"x\",JSArguments []])")
    , testCase "CallExpression2" (testStmt "x()()"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \"()\" [JSArguments []]])")
    , testCase "CallExpression3" (testStmt "x()[4]"  "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \"[]\" [JSExpression [JSDecimal \"4\"]]])")
    , testCase "CallExpression4" (testStmt "x().x"   "Right (JSExpression [JSIdentifier \"x\",JSArguments [],JSCallExpression \".\" [JSIdentifier \"x\"]])")
    , testCase "CallExpression5" (testStmt "x(a,b=2).x"  "Right (JSExpression [JSIdentifier \"x\",JSArguments [JSIdentifier \"a\",JSLiteral \",\",JSIdentifier \"b\",JSOperator JSLiteral \"=\",JSDecimal \"2\"],JSCallExpression \".\" [JSIdentifier \"x\"]])")

    , testCase "AssignExpression1"  (testStmt "x=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"])")
    , testCase "AssignExpression2"  (testStmt "x*=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"*=\",JSDecimal \"1\"])")
    , testCase "AssignExpression3"  (testStmt "x/=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"/=\",JSDecimal \"1\"])")
    , testCase "AssignExpression4"  (testStmt "x%=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"%=\",JSDecimal \"1\"])")
    , testCase "AssignExpression5"  (testStmt "x+=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"+=\",JSDecimal \"1\"])")
    , testCase "AssignExpression6"  (testStmt "x-=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"-=\",JSDecimal \"1\"])")
    , testCase "AssignExpression7"  (testStmt "x<<=1"  "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"<<=\",JSDecimal \"1\"])")
    , testCase "AssignExpression8"  (testStmt "x>>=1"  "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \">>=\",JSDecimal \"1\"])")
    , testCase "AssignExpression9"  (testStmt "x>>>=1" "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \">>>=\",JSDecimal \"1\"])")
    , testCase "AssignExpression10" (testStmt "x&=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"&=\",JSDecimal \"1\"])")
    , testCase "AssignExpression11" (testStmt "x^=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"^=\",JSDecimal \"1\"])")
    , testCase "AssignExpression12" (testStmt "x|=1"   "Right (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"|=\",JSDecimal \"1\"])")


    , testCase "Block1" (testStmt "{}"     "Right (JSBlock ([]))")
    , testCase "Block2" (testStmt "{x=1}"  "Right (JSBlock ([JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"]]))")
    , testCase "Block3" (testStmt "{x=1;y=2}" "Right (JSBlock ([JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"y\",JSOperator JSLiteral \"=\",JSDecimal \"2\"]]))")
    , testCase "Block4" (testStmt "{{}}"     "Right (JSBlock ([JSBlock ([])]))")
    , testCase "Block5" (testStmt "{{{}}}"   "Right (JSBlock ([JSBlock ([JSBlock ([])])]))")

    , testCase "If1" (testStmt "if (1) {}"  "Right (JSIf (JSExpression [JSDecimal \"1\"]) ([JSBlock ([])]) ([]))")

    , testCase "IfElse1" (testStmt "if (1) {} else {}"  "Right (JSIf (JSExpression [JSDecimal \"1\"]) ([JSBlock ([])]) ([JSLiteral \"else\",JSBlock ([])]))")
    , testCase "IfElse2" (testStmt "if (1) x=1; else {}" "Right (JSIf (JSExpression [JSDecimal \"1\"]) ([JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\"]) ([JSLiteral \"else\",JSBlock ([])]))")

    , testCase "DoWhile1" (testStmt "do {x=1} while (true);"  "Right (JSDoWhile (JSBlock ([JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"]])) (JSExpression [JSLiteral \"true\"]) (JSLiteral \";\"))")
    , testCase "DoWhile2" (testStmt "do x=x+1;while(x<4);" "Right (JSDoWhile (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSExpressionBinary \"+\" [JSIdentifier \"x\"] [JSDecimal \"1\"]]) (JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"x\"] [JSDecimal \"4\"]]) (JSLiteral \";\"))")

    , testCase "While1"   (testStmt "while(true);"             "Right (JSWhile (JSExpression [JSLiteral \"true\"]) (JSLiteral \";\"))")

    , testCase "For1"   (testStmt "for(;;);"             "Right (JSFor [] [] [] (JSLiteral \";\"))")
    , testCase "For2"   (testStmt "for(x=1;x<10;x++);"   "Right (JSFor [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"]] [JSExpression [JSExpressionBinary \"<\" [JSIdentifier \"x\"] [JSDecimal \"10\"]]] [JSExpression [JSExpressionPostfix \"++\" [JSIdentifier \"x\"]]] (JSLiteral \";\"))")

    , testCase "ForVar1"   (testStmt "for(var x;;);"            "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") []] [] [] (JSLiteral \";\"))")
    , testCase "ForVar2a"   (testStmt "for(var x=1;;);"          "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") [JSLiteral \"=\",JSDecimal \"1\"]] [] [] (JSLiteral \";\"))")
    , testCase "ForVar2b"   (testStmt "for(var x;y;z){}"         "Right (JSForVar [JSVarDecl (JSIdentifier \"x\") []] [JSExpression [JSIdentifier \"y\"]] [JSExpression [JSIdentifier \"z\"]] (JSBlock ([])))")

    , testCase "ForIn1"   (testStmt "for(x in 5){}"         "Right (JSForIn [JSIdentifier \"x\"] (JSExpression [JSDecimal \"5\"]) (JSBlock ([])))")

    , testCase "ForVarIn1" (testStmt "for(var x in 5){}"    "Right (JSForVarIn (JSVarDecl (JSIdentifier \"x\") []) (JSExpression [JSDecimal \"5\"]) (JSBlock ([])))")

    , testCase "Var1" (testStmt "var x=1;"        "Right (JSVariables JSLiteral \"var\" [JSVarDecl (JSIdentifier \"x\") [JSLiteral \"=\",JSDecimal \"1\"]])")
    , testCase "Var2" (testStmt "const x=1,y=2;"  "Right (JSVariables JSLiteral \"const\" [JSVarDecl (JSIdentifier \"x\") [JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \",\",JSVarDecl (JSIdentifier \"y\") [JSLiteral \"=\",JSDecimal \"2\"]])")

    , testCase "Continue1" (testStmt "continue;"       "Right (JSContinue [] JSLiteral \";\")")
    , testCase "Continue2" (testStmt "continue x;"     "Right (JSContinue [JSIdentifier \"x\"] JSLiteral \";\")")

    , testCase "Break1" (testStmt "break;"       "Right (JSBreak [] JSLiteral \";\")")
    , testCase "Break2" (testStmt "break x;"     "Right (JSBreak [JSIdentifier \"x\"] JSLiteral \";\")")

    , testCase "Return1" (testStmt "return;"     "Right (JSReturn [] JSLiteral \";\")")
    , testCase "Return2" (testStmt "return x;"   "Right (JSReturn [JSExpression [JSIdentifier \"x\"]] JSLiteral \";\")")

    , testCase "With1" (testStmt "with (x) {};"   "Right (JSWith (JSExpression [JSIdentifier \"x\"]) [JSBlock ([]),JSLiteral \";\"])")

    , testCase "Labelled1" (testStmt "abc:x=1"    "Right (JSLabelled (JSIdentifier \"abc\") (JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"]))")

    , testCase "Switch1" (testStmt "switch (x) {}" "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) JSBlock ([JSLiteral \"\"]))")
    , testCase "Switch2" (testStmt "switch (x) {case 1:break;}"  "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) JSBlock ([JSCase (JSExpression [JSDecimal \"1\"]) ([JSBreak [] JSLiteral \";\"])]))")
    , testCase "Switch3" (testStmt "switch (x) {case 0:\ncase 1:break;}"  "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) JSBlock ([JSCase (JSExpression [JSDecimal \"0\"]) ([]),JSCase (JSExpression [JSDecimal \"1\"]) ([JSBreak [] JSLiteral \";\"])]))")
    , testCase "Switch4" (testStmt "switch (x) {default:break;}"     "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) JSBlock ([JSLiteral \"\",JSDefault ([JSBreak [] JSLiteral \";\"]),JSLiteral \"\"]))")
    , testCase "Switch5" (testStmt "switch (x) {default:\ncase 1:break;}" "Right (JSSwitch (JSExpression [JSIdentifier \"x\"]) JSBlock ([JSLiteral \"\",JSDefault ([]),JSCase (JSExpression [JSDecimal \"1\"]) ([JSBreak [] JSLiteral \";\"])]))")

    , testCase "Throw1" (testStmt "throw 1"   "Right (JSThrow (JSExpression [JSDecimal \"1\"]))")

    , testCase "Try1" (testStmt "try{}catch(a){}"             "Right (JSTry (JSBlock ([])) [JSCatch (JSIdentifier \"a\") [] (JSBlock ([]))])")
    , testCase "Try2" (testStmt "try{}finally{}"              "Right (JSTry (JSBlock ([])) [JSFinally (JSBlock ([]))])")
    , testCase "Try3" (testStmt "try{}catch(a){}finally{}"    "Right (JSTry (JSBlock ([])) [JSCatch (JSIdentifier \"a\") [] (JSBlock ([])),JSFinally (JSBlock ([]))])")

    , testCase "Try4" (testStmt "try{}catch(a){}catch(b){}finally{}"   "Right (JSTry (JSBlock ([])) [JSCatch (JSIdentifier \"a\") [] (JSBlock ([])),JSCatch (JSIdentifier \"b\") [] (JSBlock ([])),JSFinally (JSBlock ([]))])")
    , testCase "Try5" (testStmt "try{}catch(a){}catch(b){}"            "Right (JSTry (JSBlock ([])) [JSCatch (JSIdentifier \"a\") [] (JSBlock ([])),JSCatch (JSIdentifier \"b\") [] (JSBlock ([]))])")
    , testCase "Try6" (testStmt "try{}catch(a if true){}catch(b){}"    "Right (JSTry (JSBlock ([])) [JSCatch (JSIdentifier \"a\") [JSLiteral \"if\",JSLiteral \"true\"] (JSBlock ([])),JSCatch (JSIdentifier \"b\") [] (JSBlock ([]))])")

    , testCase "Function1" (testProg "function a(){}"      "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"a\") [] (JSBlock ([])),JSLiteral \"\"])")
    , testCase "Function2" (testProg "function a(b,c){}"  "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"a\") [JSIdentifier \"b\",JSLiteral \",\",JSIdentifier \"c\"] (JSBlock ([])),JSLiteral \"\"])")

    , testCase "Comment1" (testProg "//blah\nx=1;//foo\na"   "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"a\"],JSLiteral \"\"])")

    , testCase "Comment2" (testProg "/*x=1\ny=2\n*/z=2;//foo\na"  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"z\",JSOperator JSLiteral \"=\",JSDecimal \"2\"],JSLiteral \";\",JSExpression [JSIdentifier \"a\"],JSLiteral \"\"])")

    , testCase "min_100_animals1" (testProg "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\""
                                   "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"Animal\") [JSIdentifier \"name\"] (JSBlock ([JSIf (JSExpression [JSUnary \"!\",JSIdentifier \"name\"]) ([JSThrow (JSExpression [JSLiteral \"new\",JSIdentifier \"Error\",JSArguments [JSStringLiteral '\\'' \"Must specify an animal name\"]]),JSLiteral \";\"]) ([]),JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\"),JSOperator JSLiteral \"=\",JSIdentifier \"name\"]])),JSLiteral \";\",JSExpression [JSMemberDot [JSMemberDot [JSIdentifier \"Animal\"] (JSIdentifier \"prototype\")] (JSIdentifier \"toString\"),JSOperator JSLiteral \"=\",JSFunctionExpression [] [] (JSBlock ([JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")]] JSLiteral \"\"]))],JSLiteral \";\",JSExpression [JSIdentifier \"o\",JSOperator JSLiteral \"=\",JSLiteral \"new\",JSIdentifier \"Animal\",JSArguments [JSStringLiteral '\"' \"bob\"]],JSLiteral \";\",JSExpression [JSExpressionBinary \"==\" [JSMemberDot [JSIdentifier \"o\"] (JSIdentifier \"toString\"),JSArguments []] [JSStringLiteral '\"' \"bob\"]],JSLiteral \"\"])")

    , testCase "min_100_animals2" (testProg "Animal=function(){return this.name};" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"Animal\",JSOperator JSLiteral \"=\",JSFunctionExpression [] [] (JSBlock ([JSReturn [JSExpression [JSMemberDot [JSLiteral \"this\"] (JSIdentifier \"name\")]] JSLiteral \"\"]))],JSLiteral \";\",JSLiteral \"\"])")

    , testCase "min_100_animals3" (testProg "if(a)x=1;y=2" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"a\"]) ([JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\"]) ([]),JSExpression [JSIdentifier \"y\",JSOperator JSLiteral \"=\",JSDecimal \"2\"],JSLiteral \"\"])")

    , testCase "min_100_animals4" (testProg "if(a)x=a()y=2" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"a\"]) ([JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSIdentifier \"a\",JSArguments []]]) ([]),JSExpression [JSIdentifier \"y\",JSOperator JSLiteral \"=\",JSDecimal \"2\"],JSLiteral \"\"])")

    , testCase "05_regex"  (testProg "newlines=spaces.match(/\\n/g)" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"newlines\",JSOperator JSLiteral \"=\",JSMemberDot [JSIdentifier \"spaces\"] (JSIdentifier \"match\"),JSArguments [JSRegEx \"/\\\\n/g\"]],JSLiteral \"\"])")

    , testCase "05_regex2" (testProg "x=/\\n/g" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSRegEx \"/\\\\n/g\"],JSLiteral \"\"])")

    , testCase "05_regex3" (testProg "x=i(/[?|^&(){}\\[\\]+\\-*\\/\\.]/g,\"\\\\$&\")" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSIdentifier \"i\",JSArguments [JSRegEx \"/[?|^&(){}\\\\[\\\\]+\\\\-*\\\\/\\\\.]/g\",JSLiteral \",\",JSStringLiteral '\"' \"\\\\\\\\$&\"]],JSLiteral \"\"])")

    , testCase "05_regex4" (testProg "x=i(/^$/g,\"\\\\$&\")" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSIdentifier \"i\",JSArguments [JSRegEx \"/^$/g\",JSLiteral \",\",JSStringLiteral '\"' \"\\\\\\\\$&\"]],JSLiteral \"\"])")

    , testCase "05_regex5" (testProg "if(/^[a-z]/.test(t)){consts+=t.toUpperCase();keywords[t]=i}else consts+=(/^\\W/.test(t)?opTypeNames[t]:t);"
                           "Right (JSSourceElementsTop [JSIf (JSExpression [JSMemberDot [JSRegEx \"/^[a-z]/\"] (JSIdentifier \"test\"),JSArguments [JSIdentifier \"t\"]]) ([JSBlock ([JSExpression [JSIdentifier \"consts\",JSOperator JSLiteral \"+=\",JSMemberDot [JSIdentifier \"t\"] (JSIdentifier \"toUpperCase\"),JSArguments []],JSLiteral \";\",JSExpression [JSMemberSquare [JSIdentifier \"keywords\"] (JSExpression [JSIdentifier \"t\"]),JSOperator JSLiteral \"=\",JSIdentifier \"i\"]])]) ([JSLiteral \"else\",JSExpression [JSIdentifier \"consts\",JSOperator JSLiteral \"+=\",JSExpressionParen (JSExpression [JSExpressionTernary [JSMemberDot [JSRegEx \"/^\\\\W/\"] (JSIdentifier \"test\"),JSArguments [JSIdentifier \"t\"]] [JSMemberSquare [JSIdentifier \"opTypeNames\"] (JSExpression [JSIdentifier \"t\"])] [JSIdentifier \"t\"]])]]),JSLiteral \";\",JSLiteral \"\"])")

    , testCase "if_semi" (testProg "if(x);x=1" "Right (JSSourceElementsTop [JSIf (JSExpression [JSIdentifier \"x\"]) ([JSLiteral \";\"]) ([]),JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \"\"])")

    , testCase "67_bob" (testProg "(match = /^\"(?:\\\\.|[^\"])*\"|^'(?:[^']|\\\\.)*'/(input))" "Right (JSSourceElementsTop [JSExpression [JSExpressionParen (JSExpression [JSIdentifier \"match\",JSOperator JSLiteral \"=\",JSRegEx \"/^\\\"(?:\\\\\\\\.|[^\\\"])*\\\"|^'(?:[^']|\\\\\\\\.)*'/\",JSArguments [JSIdentifier \"input\"]])],JSLiteral \"\"])")

    , testCase "122_jsexec" (testProg "v = getValue(execute(n[0], x)) in getValue(execute(n[1], x));" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"v\",JSOperator JSLiteral \"=\",JSExpressionBinary \" in \" [JSIdentifier \"getValue\",JSArguments [JSIdentifier \"execute\",JSArguments [JSMemberSquare [JSIdentifier \"n\"] (JSExpression [JSDecimal \"0\"]),JSLiteral \",\",JSIdentifier \"x\"]]] [JSIdentifier \"getValue\",JSArguments [JSIdentifier \"execute\",JSArguments [JSMemberSquare [JSIdentifier \"n\"] (JSExpression [JSDecimal \"1\"]),JSLiteral \",\",JSIdentifier \"x\"]]]],JSLiteral \";\",JSLiteral \"\"])")

    , testCase "bug1a" (testProg "/* */\nfunction f() {\n/*  */\n}\n" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"f\") [] (JSBlock ([])),JSLiteral \"\"])")
    , testCase "bug1b" (testProg "/* **/\nfunction f() {\n/*  */\n}\n" "Right (JSSourceElementsTop [JSFunction (JSIdentifier \"f\") [] (JSBlock ([])),JSLiteral \"\"])")

    , testCase "unicode1-ws" (testProg "a \f\v\t\r\n=\x00a0\x1680\x180e\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000\&1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"a\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSLiteral \"\"])")

    , testCase "unicode2-lt" (testProg "//comment\x000Ax=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSLiteral \"\"])")
    , testCase "unicode3-lt" (testProg "//comment\x000Dx=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSLiteral \"\"])")
    , testCase "unicode4-lt" (testProg "//comment\x2028x=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSLiteral \"\"])")
    , testCase "unicode5-lt" (testProg "//comment\x2029x=1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSLiteral \"\"])")

    , testCase "unicode2" (testProg "àáâãäå = 1;" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"\\224\\225\\226\\227\\228\\229\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSLiteral \"\"])")

    , testCase "unicode3" (testProg "$aà = 1;_b=2;\0065a=2"  "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"$a\\224\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSExpression [JSIdentifier \"_b\",JSOperator JSLiteral \"=\",JSDecimal \"2\"],JSLiteral \";\",JSExpression [JSIdentifier \"Aa\",JSOperator JSLiteral \"=\",JSDecimal \"2\"],JSLiteral \"\"])")

    , testCase "unicode4" (testProg "x=\"àáâãäå\";y='\3012a\0068'" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSStringLiteral '\"' \"\\224\\225\\226\\227\\228\\229\"],JSLiteral \";\",JSExpression [JSIdentifier \"y\",JSOperator JSLiteral \"=\",JSStringLiteral '\\'' \"\\3012aD\"],JSLiteral \"\"])")

    , testCase "unicode5f" (testFileUtf8 "./test/Unicode.js" "JSSourceElementsTop [JSExpression [JSIdentifier \"\\224\\225\\226\\227\\228\\229\",JSOperator JSLiteral \"=\",JSDecimal \"1\"],JSLiteral \";\",JSLiteral \"\"]")

    , testCase "bug2.a" (testProg "function() {\nz = function /*z*/(o) {\nreturn r;\n};}" "Right (JSSourceElementsTop [JSExpression [JSFunctionExpression [] [] (JSBlock ([JSExpression [JSIdentifier \"z\",JSOperator JSLiteral \"=\",JSFunctionExpression [] [JSIdentifier \"o\"] (JSBlock ([JSReturn [JSExpression [JSIdentifier \"r\"]] JSLiteral \";\"]))],JSLiteral \";\"]))],JSLiteral \"\"])")

    , testCase "bug2.b" (testProg "function() {\nz = function z(o) {\nreturn r;\n};}" "Right (JSSourceElementsTop [JSExpression [JSFunctionExpression [] [] (JSBlock ([JSExpression [JSIdentifier \"z\",JSOperator JSLiteral \"=\",JSFunctionExpression [JSIdentifier \"z\"] [JSIdentifier \"o\"] (JSBlock ([JSReturn [JSExpression [JSIdentifier \"r\"]] JSLiteral \";\"]))],JSLiteral \";\"]))],JSLiteral \"\"])")

   -- https://github.com/alanz/hjsmin/issues/#issue/3
    , testCase "bug3" (testProg "var myLatlng = new google.maps.LatLng(56.8379100, 60.5806664);" "Right (JSSourceElementsTop [JSVariables JSLiteral \"var\" [JSVarDecl (JSIdentifier \"myLatlng\") [JSLiteral \"=\",JSLiteral \"new\",JSMemberDot [JSMemberDot [JSIdentifier \"google\"] (JSIdentifier \"maps\")] (JSIdentifier \"LatLng\"),JSArguments [JSDecimal \"56.8379100\",JSLiteral \",\",JSDecimal \"60.5806664\"]]],JSLiteral \"\"])")

   -- https://github.com/alanz/hjsmin/issues/#issue/4
    , testCase "bug4" (testProg "/* * geolocation. пытаемся определить свое местоположение * если не получается то используем defaultLocation * @Param {object} map экземпляр карты * @Param {object LatLng} defaultLocation Координаты центра по умолчанию * @Param {function} callbackAfterLocation Фу-ия которая вызывается после * геолокации. Т.к запрос геолокации асинхронен */x" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\"],JSLiteral \"\"])")

    , testCase "02_sm.js"   (testProg "{zero}\none1;two\n{three\nfour;five;\n{\nsix;{seven;}\n}\n}" "Right (JSSourceElementsTop [JSBlock ([JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"one1\"],JSLiteral \";\",JSExpression [JSIdentifier \"two\"],JSBlock ([JSExpression [JSIdentifier \"three\"],JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"five\"],JSLiteral \";\",JSBlock ([JSExpression [JSIdentifier \"six\"],JSLiteral \";\",JSBlock ([JSExpression [JSIdentifier \"seven\"],JSLiteral \";\"])])]),JSLiteral \"\"])")

    , testCase "02_sm.js.2" (testProg "{zero}\nget;two\n{three\nfour;set;\n{\nsix;{seven;}\n}\n}" "Right (JSSourceElementsTop [JSBlock ([JSExpression [JSIdentifier \"zero\"]]),JSExpression [JSIdentifier \"get\"],JSLiteral \";\",JSExpression [JSIdentifier \"two\"],JSBlock ([JSExpression [JSIdentifier \"three\"],JSExpression [JSIdentifier \"four\"],JSLiteral \";\",JSExpression [JSIdentifier \"set\"],JSLiteral \";\",JSBlock ([JSExpression [JSIdentifier \"six\"],JSLiteral \";\",JSBlock ([JSExpression [JSIdentifier \"seven\"],JSLiteral \";\"])])]),JSLiteral \"\"])")

    , testCase "loc1" (testProgUn "x = 1\n  y=2;" "Right (NN (JSSourceElementsTop [NN (JSExpression [NT (JSIdentifier \"x\") (TokenPn 0 1 1) [NoComment],NN (JSOperator (NT (JSLiteral \"=\") (TokenPn 2 1 3) [WhiteSpace (TokenPn 1 1 2) \" \"])),NT (JSDecimal \"1\") (TokenPn 4 1 5) [WhiteSpace (TokenPn 3 1 4) \" \"]]),NN (JSExpression [NT (JSIdentifier \"y\") (TokenPn 8 2 3) [WhiteSpace (TokenPn 5 1 6) \"\\n  \"],NN (JSOperator (NT (JSLiteral \"=\") (TokenPn 9 2 4) [NoComment])),NT (JSDecimal \"2\") (TokenPn 10 2 5) [NoComment]]),NT (JSLiteral \";\") (TokenPn 11 2 6) [NoComment],NT (JSLiteral \"\") (TokenPn 0 0 0) [NoComment]]))")

   -- https://github.com/alanz/language-javascript/issues/2
    , testCase "issue2" (testProg "var img = document.createElement('img');\nimg.src = \"mylogo.jpg\";\n$(img).click(function() {\n   alert('clicked!');\n});" "Right (JSSourceElementsTop [JSVariables JSLiteral \"var\" [JSVarDecl (JSIdentifier \"img\") [JSLiteral \"=\",JSMemberDot [JSIdentifier \"document\"] (JSIdentifier \"createElement\"),JSArguments [JSStringLiteral '\\'' \"img\"]]],JSExpression [JSMemberDot [JSIdentifier \"img\"] (JSIdentifier \"src\"),JSOperator JSLiteral \"=\",JSStringLiteral '\"' \"mylogo.jpg\"],JSLiteral \";\",JSExpression [JSIdentifier \"$\",JSArguments [JSIdentifier \"img\"],JSCallExpression \".\" [JSIdentifier \"click\"],JSCallExpression \"()\" [JSArguments [JSFunctionExpression [] [] (JSBlock ([JSExpression [JSIdentifier \"alert\",JSArguments [JSStringLiteral '\\'' \"clicked!\"]],JSLiteral \";\"]))]]],JSLiteral \";\",JSLiteral \"\"])")

   -- Working in ECMASCRIPT 5.1 changes
    , testCase "lineTerminatorInString1" (testProg "x='abc\\\ndef';"   "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSStringLiteral '\\'' \"abc\\\\\\ndef\"],JSLiteral \";\",JSLiteral \"\"])")
    , testCase "lineTerminatorInString2" (testProg "x=\"abc\\\ndef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSStringLiteral '\"' \"abc\\\\\\ndef\"],JSLiteral \";\",JSLiteral \"\"])")
    , testCase "lineTerminatorInString3" (testProg "x=\"abc\\\rdef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSStringLiteral '\"' \"abc\\\\\\rdef\"],JSLiteral \";\",JSLiteral \"\"])")
    , testCase "lineTerminatorInString4" (testProg "x=\"abc\\\x2028 def\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSStringLiteral '\"' \"abc\\\\\\8232 def\"],JSLiteral \";\",JSLiteral \"\"])")
    , testCase "lineTerminatorInString5" (testProg "x=\"abc\\\x2029 def\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSStringLiteral '\"' \"abc\\\\\\8233 def\"],JSLiteral \";\",JSLiteral \"\"])")
    , testCase "lineTerminatorInString6" (testProg "x=\"abc\\\r\ndef\";" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSStringLiteral '\"' \"abc\\\\\\r\\ndef\"],JSLiteral \";\",JSLiteral \"\"])")


     -- https://github.com/alanz/language-javascript/issues/4
    , testCase "issue4ok"   (testProg "var k = {\ny: somename\n}" "Right (JSSourceElementsTop [JSVariables JSLiteral \"var\" [JSVarDecl (JSIdentifier \"k\") [JSLiteral \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"somename\"]]]],JSLiteral \"\"])")
    , testCase "issue4bug1" (testProg "var k = {\ny: code\n}" "Right (JSSourceElementsTop [JSVariables JSLiteral \"var\" [JSVarDecl (JSIdentifier \"k\") [JSLiteral \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"code\"]]]],JSLiteral \"\"])")
    , testCase "issue4bug2" (testProg "var k = {\ny: mode\n}" "Right (JSSourceElementsTop [JSVariables JSLiteral \"var\" [JSVarDecl (JSIdentifier \"k\") [JSLiteral \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSIdentifier \"mode\"]]]],JSLiteral \"\"])")

     -- https://github.com/alanz/language-javascript/issues/5
    , testCase "issue5bug1" (testProg "x = { y: 1e8 }" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"1e8\"]]],JSLiteral \"\"])")
    , testCase "issue5ok2" (testProg "{ y: 1e8 }" "Right (JSSourceElementsTop [JSBlock ([JSLabelled (JSIdentifier \"y\") (JSExpression [JSDecimal \"1e8\"])]),JSLiteral \"\"])")
    , testCase "issue5ok3" (testProg "{ y: 18 }" "Right (JSSourceElementsTop [JSBlock ([JSLabelled (JSIdentifier \"y\") (JSExpression [JSDecimal \"18\"])]),JSLiteral \"\"])")
    , testCase "issue5ok4" (testProg "x = { y: 18 }" "Right (JSSourceElementsTop [JSExpression [JSIdentifier \"x\",JSOperator JSLiteral \"=\",JSObjectLiteral [JSPropertyNameandValue (JSIdentifier \"y\") [JSDecimal \"18\"]]],JSLiteral \"\"])")

     -- https://github.com/alanz/language-javascript/issues/14
    , testCase "issue14" (testProg "var z = x[i] / y;" "Right (JSSourceElementsTop [JSVariables JSLiteral \"var\" [JSVarDecl (JSIdentifier \"z\") [JSLiteral \"=\",JSExpressionBinary \"/\" [JSMemberSquare [JSIdentifier \"x\"] (JSExpression [JSIdentifier \"i\"])] [JSIdentifier \"y\"]]],JSLiteral \"\"])")

    , testCase "AutoSemiBreak"     (testProg "if(true)break \nfoo();"      "Right (JSSourceElementsTop [JSIf (JSExpression [JSLiteral \"true\"]) ([JSBreak [] JSLiteral \"\"]) ([]),JSExpression [JSIdentifier \"foo\",JSArguments []],JSLiteral \";\",JSLiteral \"\"])")
    , testCase "AutoSemiContinue"  (testProg "if(true)continue \nfoo();"   "Right (JSSourceElementsTop [JSIf (JSExpression [JSLiteral \"true\"]) ([JSContinue [] JSLiteral \"\"]) ([]),JSExpression [JSIdentifier \"foo\",JSArguments []],JSLiteral \";\",JSLiteral \"\"])")
    , testCase "AutoSemiReturn"    (testProg "if(true)break \nfoo();"      "Right (JSSourceElementsTop [JSIf (JSExpression [JSLiteral \"true\"]) ([JSBreak [] JSLiteral \"\"]) ([]),JSExpression [JSIdentifier \"foo\",JSArguments []],JSLiteral \";\",JSLiteral \"\"])")

    , testCase "BreakBlock"       (testProg "{break}"      "Right (JSSourceElementsTop [JSBlock ([JSBreak [] JSLiteral \"\"]),JSLiteral \"\"])")
    , testCase "ContinueBlock"    (testProg "{continue}"   "Right (JSSourceElementsTop [JSBlock ([JSContinue [] JSLiteral \"\"]),JSLiteral \"\"])")
    , testCase "ReturnBlock"      (testProg "{return}"     "Right (JSSourceElementsTop [JSBlock ([JSReturn [] JSLiteral \"\"]),JSLiteral \"\"])")
    ]

caseHelloWorld :: Assertion
caseHelloWorld =
    "JSSourceElementsTop [JSExpression [JSIdentifier \"Hello\"],JSLiteral \"\"]" @=? showStripped (readJs "Hello")

-- ---------------------------------------------------------------------

commentSuite :: Test
commentSuite = testGroup "Comments"
    [ testCase "helloWorld"        caseHelloWorld
    , testCase "LiteralNull"       (testLiteralC "/*a*/null"     "Right (NS (JSLiteral \"null\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"])")
    , testCase "LiteralFalse"      (testLiteralC "/*b*/false"    "Right (NS (JSLiteral \"false\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*b*/\"])")
    , testCase "LiteralTrue"       (testLiteralC "true"          "Right (NS (JSLiteral \"true\") (TokenPn 0 1 1) [NoComment])")
    , testCase "LiteralTrue"       (testLiteralC "/*c*/true"     "Right (NS (JSLiteral \"true\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*c*/\"])")

    , testCase "LiteralHexInteger" (testLiteralC "/*d*/0x1234fF" "Right (NS (JSHexInteger \"0x1234fF\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*d*/\"])")
    , testCase "LiteralDecimal"    (testLiteralC "/*e*/1.0e4"    "Right (NS (JSDecimal \"1.0e4\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*e*/\"])")
    , testCase "LiteralOctal"      (testLiteralC "/*x*/011"      "Right (NS (JSOctal \"011\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*x*/\"])")
    , testCase "LiteralString1"    (testLiteralC "/*f*/\"hello\\nworld\"" "Right (NS (JSStringLiteral '\"' \"hello\\\\nworld\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*f*/\"])")
    , testCase "LiteralString2"    (testLiteralC "/*g*/'hello\\nworld'"   "Right (NS (JSStringLiteral '\\'' \"hello\\\\nworld\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*g*/\"])")

    , testCase "LiteralThis"       (testPEC "/*h*/this"  "Right (NS (JSLiteral \"this\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*h*/\"])")

    , testCase "LiteralRegex1"     (testPEC "/*i*//blah/"  "Right (NS (JSRegEx \"/blah/\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*i*/\"])")

    , testCase "Identifier2"       (testPEC "//j\nthis_"   "Right (NS (JSIdentifier \"this_\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"//j\"])")

    , testCase "ArrayLiteral1"     (testPEC "/*a*/[/*b*/]"      "Right (NS (JSArrayLiteral []) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 6 1 7) \"/*b*/\"])")
    , testCase "ArrayLiteral2"     (testPEC "/*a*/[/*b*/,/*c*/]"     "Right (NS (JSArrayLiteral [NS (JSElision []) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 12 1 13) \"/*c*/\"])")
    , testCase "ArrayLiteral3"     (testPEC "/*a*/[/*b*/,/*c*/,/*d*/]"    "Right (NS (JSArrayLiteral [NS (JSElision []) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"],NS (JSElision []) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 18 1 19) \"/*d*/\"])")
    , testCase "ArrayLiteral4"     (testPEC "/*a*/[/*b/*,/*c*/,/*d*/x/*e*/]"   "Right (NS (JSArrayLiteral [NS (JSElision []) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b/*,/*c*/\"],NS (JSIdentifier \"x\") (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 24 1 25) \"/*e*/\"])")
    , testCase "ArrayLiteral5"     (testPEC "/*a*/[/*b*/,/*c*/,/*d*/x/*e*/]"   "Right (NS (JSArrayLiteral [NS (JSElision []) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"],NS (JSElision []) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"],NS (JSIdentifier \"x\") (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 24 1 25) \"/*e*/\"])")
    , testCase "ArrayLiteral6"     (testPEC "/*a*/[/*b*/,/*c*/x/*d*/,/*e*/,/*f*/x/*g*/]" "Right (NS (JSArrayLiteral [NS (JSElision []) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"],NS (JSIdentifier \"x\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"],NS (JSElision []) (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"],NS (JSElision []) (TokenPn 24 1 25) [CommentA (TokenPn 24 1 25) \"/*e*/\"],NS (JSIdentifier \"x\") (TokenPn 30 1 31) [CommentA (TokenPn 30 1 31) \"/*f*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 36 1 37) \"/*g*/\"])")
    , testCase "ArrayLiteral7"     (testPEC "/*a*/[/*b*/x/*c*/]"     "Right (NS (JSArrayLiteral [NS (JSIdentifier \"x\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 12 1 13) \"/*c*/\"])")
    , testCase "ArrayLiteral8"     (testPEC "/*a*/[/*b*/x/*c*/,/*d*/]"    "Right (NS (JSArrayLiteral [NS (JSIdentifier \"x\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"],NS (JSLiteral \",\") (TokenPn 17 1 18) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 18 1 19) \"/*d*/\"])")

    , testCase "ObjectLiteral1"    (testPEC "/*a*/{/*b*/}"       "Right (NS (JSObjectLiteral []) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 6 1 7) \"/*b*/\"])")
    , testCase "ObjectLiteral2"    (testPEC "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/}"    "Right (NS (JSObjectLiteral [NS (JSPropertyNameandValue (NS (JSIdentifier \"x\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]) [NS (JSDecimal \"1\") (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"]]) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 24 1 25) \"/*e*/\"])")
    , testCase "ObjectLiteral3"    (testPEC "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/}"     "Right (NS (JSObjectLiteral [NS (JSPropertyNameandValue (NS (JSIdentifier \"x\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]) [NS (JSDecimal \"1\") (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"]]) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"],NS (JSPropertyNameandValue (NS (JSIdentifier \"y\") (TokenPn 30 1 31) [CommentA (TokenPn 30 1 31) \"/*f*/\"]) [NS (JSDecimal \"2\") (TokenPn 42 1 43) [CommentA (TokenPn 42 1 43) \"/*h*/\"]]) (TokenPn 36 1 37) [CommentA (TokenPn 24 1 25) \"/*e*/\",CommentA (TokenPn 36 1 37) \"/*g*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 48 1 49) \"/*i*/\"])")

    , testCase "ObjectLiteral5"    (testPEC "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/}"    "Right (NS (JSObjectLiteral [NS (JSPropertyNameandValue (NS (JSIdentifier \"x\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]) [NS (JSDecimal \"1\") (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\"]]) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"],NS (JSLiteral \",\") (TokenPn 29 1 30) [CommentA (TokenPn 24 1 25) \"/*e*/\"]]) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 30 1 31) \"/*f*/\"])")

    -- Edition 5 extensions
    , testCase "ObjectLiteral7"    (testProgC "/*a*/x/*b*/=/*c*/{/*d*/get/*e*/ foo/*f*/(/*g*/)/*h*/ {/*i*/return/*j*/ 1/*k*/}/*l*/,/*m*/set/*n*/ foo/*o*/(/*p*/a/*q*/) /*r*/{/*s*/x/*t*/=/*u*/a/*v*/}/*w*/}"  "Right (NS (JSSourceElementsTop [NS (JSExpression [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"],NS (JSOperator \"=\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"],NS (JSObjectLiteral [NS (JSPropertyAccessor \"get\" (NS (JSIdentifier \"foo\") (TokenPn 26 1 27) [CommentA (TokenPn 26 1 27) \"/*e*/\"]) [] (NS (JSFunctionBody [NS (JSSourceElements [NS (JSReturn [NS (JSExpression [NS (JSDecimal \"1\") (TokenPn 65 1 66) [CommentA (TokenPn 65 1 66) \"/*j*/\"]]) (TokenPn 65 1 66) [],NS (JSLiteral \"\") (TokenPn 0 0 0) []]) (TokenPn 54 1 55) [CommentA (TokenPn 54 1 55) \"/*i*/\"]]) (TokenPn 54 1 55) []]) (TokenPn 54 1 55) [])) (TokenPn 18 1 19) [CommentA (TokenPn 18 1 19) \"/*d*/\",CommentA (TokenPn 35 1 36) \"/*f*/\",CommentA (TokenPn 41 1 42) \"/*g*/\",CommentA (TokenPn 47 1 48) \"/*h*/\",CommentA (TokenPn 72 1 73) \"/*k*/\"],NS (JSPropertyAccessor \"set\" (NS (JSIdentifier \"foo\") (TokenPn 92 1 93) [CommentA (TokenPn 92 1 93) \"/*n*/\"]) [NS (JSIdentifier \"a\") (TokenPn 107 1 108) [CommentA (TokenPn 107 1 108) \"/*p*/\"]] (NS (JSFunctionBody [NS (JSSourceElements [NS (JSExpression [NS (JSIdentifier \"x\") (TokenPn 126 1 127) [CommentA (TokenPn 126 1 127) \"/*s*/\"],NS (JSOperator \"=\") (TokenPn 132 1 133) [CommentA (TokenPn 132 1 133) \"/*t*/\"],NS (JSIdentifier \"a\") (TokenPn 138 1 139) [CommentA (TokenPn 138 1 139) \"/*u*/\"]]) (TokenPn 126 1 127) []]) (TokenPn 126 1 127) []]) (TokenPn 126 1 127) [])) (TokenPn 84 1 85) [CommentA (TokenPn 78 1 79) \"/*l*/\",CommentA (TokenPn 84 1 85) \"/*m*/\",CommentA (TokenPn 101 1 102) \"/*o*/\",CommentA (TokenPn 113 1 114) \"/*q*/\",CommentA (TokenPn 120 1 121) \"/*r*/\",CommentA (TokenPn 144 1 145) \"/*v*/\"]]) (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\",CommentA (TokenPn 150 1 151) \"/*w*/\"]]) (TokenPn 0 1 1) []]) (TokenPn 0 1 1) [])")

    , testCase "ExpressionParen"   (testPEC "/*a*/(/*b*/56/*c*/)"     "Right (NS (JSExpressionParen (NS (JSExpression [NS (JSDecimal \"56\") (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])) (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\",CommentA (TokenPn 13 1 14) \"/*c*/\"])")

    , testCase "Statement1"        (testStmtC "/*a*/x"        "Right (NS (JSExpression [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]]) (TokenPn 0 1 1) [])")

    , testCase "Statement2"        (testStmtC "/*a*/null"     "Right (NS (JSExpression [NS (JSLiteral \"null\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]]) (TokenPn 0 1 1) [])")

    , testCase "Statement3"        (testStmtC "/*a*/true/*b*/?/*c*/1/*d*/:/*e*/2" "Right (NS (JSExpression [NS (JSExpressionTernary [NS (JSLiteral \"true\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSDecimal \"1\") (TokenPn 15 1 16) [CommentA (TokenPn 15 1 16) \"/*c*/\"]] [NS (JSDecimal \"2\") (TokenPn 27 1 28) [CommentA (TokenPn 27 1 28) \"/*e*/\"]]) (TokenPn 9 1 10) [CommentA (TokenPn 9 1 10) \"/*b*/\",CommentA (TokenPn 21 1 22) \"/*d*/\"]]) (TokenPn 9 1 10) [])")

    , testCase "Statement4"        (testStmtC "/*a*/x/*b*/||/*c*/y"    "Right (NS (JSExpression [NS (JSExpressionBinary \"||\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement5"        (testStmtC "/*a*/x/*b*/&&/*c*/y"    "Right (NS (JSExpression [NS (JSExpressionBinary \"&&\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement6a"       (testStmtC "/*a*/x/*b*/|/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"|\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement6b"       (testStmtC "/*a*/x/*b*/^/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"^\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement7"        (testStmtC "/*a*/x/*b*/&/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"&\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement8"        (testStmtC "/*a*/x/*b*/==/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"==\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement9"        (testStmtC "/*a*/x/*b*/!=/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"!=\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement10"       (testStmtC "/*a*/x/*b*/===/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"===\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 14 1 15) [CommentA (TokenPn 14 1 15) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement11"       (testStmtC "/*a*/x/*b*/!==/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"!==\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 14 1 15) [CommentA (TokenPn 14 1 15) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement12a"       (testStmtC "/*a*/x/*b*/</*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"<\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement12b"       (testStmtC "/*a*/x/*b*/>/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \">\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 12 1 13) [CommentA (TokenPn 12 1 13) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement12c"       (testStmtC "/*a*/x/*b*/<=/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \"<=\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement12d"       (testStmtC "/*a*/x/*b*/>=/*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \">=\" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 13 1 14) [CommentA (TokenPn 13 1 14) \"/*c*/\"]]) (TokenPn 6 1 7) [CommentA (TokenPn 6 1 7) \"/*b*/\"]]) (TokenPn 6 1 7) [])")

    , testCase "Statement12e"       (testStmtC "/*a*/x /*b*/instanceof /*c*/y"     "Right (NS (JSExpression [NS (JSExpressionBinary \" instanceof \" [NS (JSIdentifier \"x\") (TokenPn 0 1 1) [CommentA (TokenPn 0 1 1) \"/*a*/\"]] [NS (JSIdentifier \"y\") (TokenPn 23 1 24) [CommentA (TokenPn 23 1 24) \"/*c*/\"]]) (TokenPn 7 1 8) [CommentA (TokenPn 7 1 8) \"/*b*/\"]]) (TokenPn 7 1 8) [])")
    ]

-- ---------------------------------------------------------------------

commentPrintSuite :: Test
commentPrintSuite = testGroup "Comments"
    [ testCase "Multi-comment"     (testRoundTrip "/*a*/\n//foo\nnull")

    , testCase "LiteralNull"       (testRoundTrip "/*a*/null")
    , testCase "LiteralFalse"      (testRoundTrip "/*b*/false")
    , testCase "LiteralTrue"       (testRoundTrip "true")
    , testCase "LiteralTrue"       (testRoundTrip "/*c*/true")

    , testCase "LiteralHexInteger" (testRoundTrip "/*d*/0x1234fF")
    , testCase "LiteralDecimal"    (testRoundTrip "/*e*/1.0e4")
    , testCase "LiteralOctal"      (testRoundTrip "/*x*/011")
    , testCase "LiteralString1"    (testRoundTrip "/*f*/\"hello\\nworld\"")
    , testCase "LiteralString2"    (testRoundTrip "/*g*/'hello\\nworld'")

    , testCase "LiteralThis"       (testRoundTrip "/*h*/this")

    , testCase "LiteralRegex1"     (testRoundTrip "/*i*//blah/")

    , testCase "Identifier2"       (testRoundTrip "//j\nthis_")

    , testCase "ArrayLiteral1"     (testRoundTrip "/*a*/[/*b*/]")
    , testCase "ArrayLiteral2"     (testRoundTrip "/*a*/[/*b*/,/*c*/]")
    , testCase "ArrayLiteral3"     (testRoundTrip "/*a*/[/*b*/,/*c*/,/*d*/]")
    , testCase "ArrayLiteral4"     (testRoundTrip "/*a*/[/*b/*,/*c*/,/*d*/x/*e*/]")
    , testCase "ArrayLiteral5"     (testRoundTrip "/*a*/[/*b*/,/*c*/,/*d*/x/*e*/]")
    , testCase "ArrayLiteral6"     (testRoundTrip "/*a*/[/*b*/,/*c*/x/*d*/,/*e*/,/*f*/x/*g*/]")
    , testCase "ArrayLiteral7"     (testRoundTrip "/*a*/[/*b*/x/*c*/]")
    , testCase "ArrayLiteral8"     (testRoundTrip "/*a*/[/*b*/x/*c*/,/*d*/]")

    , testCase "ObjectLiteral1"    (testRoundTrip "/*a*/{/*b*/}")
    , testCase "ObjectLiteral2"    (testRoundTrip "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/}")
    , testCase "ObjectLiteral3"    (testRoundTrip "x=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/}")
    , testCase "ObjectLiteral3a"   (testRoundTrip "x=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/,/*j*/z/*k*/:/*l*/3/*m*/}")

    , testCase "ObjectLiteral5"    (testRoundTrip "a=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/}")

    -- Edition 5 extensions
    , testCase "ObjectLiteral7"    (testRoundTrip "/*a*/x/*b*/=/*c*/{/*d*/get/*e*/ foo/*f*/(/*g*/)/*h*/ {/*i*/return/*j*/ 1/*k*/}/*l*/,/*m*/set/*n*/ foo/*o*/(/*p*/a/*q*/) /*r*/{/*s*/x/*t*/=/*u*/a/*v*/}/*w*/}")

    , testCase "ExpressionParen"   (testRoundTrip "/*a*/(/*b*/56/*c*/)")

    , testCase "Statement1"        (testRoundTrip "/*a*/x")

    , testCase "Statement2"        (testRoundTrip "/*a*/null")

    , testCase "Statement3"        (testRoundTrip "/*a*/true/*b*/?/*c*/1/*d*/:/*e*/2")

    , testCase "Statement4"        (testRoundTrip "/*a*/x/*b*/||/*c*/y")

    , testCase "Statement5"        (testRoundTrip "/*a*/x/*b*/&&/*c*/y")

    , testCase "Statement6a"       (testRoundTrip "/*a*/x/*b*/|/*c*/y")

    , testCase "Statement6b"       (testRoundTrip "/*a*/x/*b*/^/*c*/y")

    , testCase "Statement7"        (testRoundTrip "/*a*/x/*b*/&/*c*/y")

    , testCase "Statement8"        (testRoundTrip "/*a*/x/*b*/==/*c*/y")

    , testCase "Statement9"        (testRoundTrip "/*a*/x/*b*/!=/*c*/y")

    , testCase "Statement10"       (testRoundTrip "/*a*/x/*b*/===/*c*/y")

    , testCase "Statement11"       (testRoundTrip "/*a*/x/*b*/!==/*c*/y")

    , testCase "Statement12a"       (testRoundTrip "/*a*/x/*b*/</*c*/y")

    , testCase "Statement12b"       (testRoundTrip "/*a*/x/*b*/>/*c*/y")

    , testCase "Statement12c"       (testRoundTrip "/*a*/x/*b*/<=/*c*/y")

    , testCase "Statement12d"       (testRoundTrip "/*a*/x/*b*/>=/*c*/y")

    , testCase "Statement12e"       (testRoundTrip "/*a*/x /*b*/instanceof /*c*/y")

    , testCase "Statement13"       (testRoundTrip "x<<y")
    , testCase "Statement13"       (testRoundTrip "x>>y")
    , testCase "Statement13"       (testRoundTrip "x>>>y")

    , testCase "Statement14"       (testRoundTrip "x+y")
    , testCase "Statement14"       (testRoundTrip "x-y")

    , testCase "Statement15"       (testRoundTrip "x*y")
    , testCase "Statement16"       (testRoundTrip "x/y")
    , testCase "Statement17"       (testRoundTrip "x%y")

    , testCase "Statement18"       (testRoundTrip "delete y")
    , testCase "Statement19"       (testRoundTrip "void y")
    , testCase "Statement20"       (testRoundTrip "typeof y")
    , testCase "Statement21"       (testRoundTrip "++y")
    , testCase "Statement22"       (testRoundTrip "--y")
    , testCase "Statement23"       (testRoundTrip "+y")
    , testCase "Statement24"       (testRoundTrip "-y")
    , testCase "Statement25"       (testRoundTrip "~y")
    , testCase "Statement26"       (testRoundTrip "!y")

    , testCase "Statement27"       (testRoundTrip "y++")
    , testCase "Statement28"       (testRoundTrip "y--")

      -- Member Expressions
    , testCase "MemberExpression1a" (testRoundTrip "function(){}")
    , testCase "MemberExpression1b" (testRoundTrip "function(a){}")
    , testCase "MemberExpression1c" (testRoundTrip "function(a,b){}")

    , testCase "MemberExpression1d" (testRoundTrip "x[y]")
    , testCase "MemberExpression1e" (testRoundTrip "x[y][z]")
    , testCase "MemberExpression1f" (testRoundTrip "x.y")
    , testCase "MemberExpression1g" (testRoundTrip "x.y.z")

    , testCase "MemberExpression1h" (testRoundTrip "new x()")

    , testCase "NewExpression1" (testRoundTrip "new x.y")

    , testCase "CallExpression1" (testRoundTrip "x()")
    , testCase "CallExpression2" (testRoundTrip "x()()")
    , testCase "CallExpression3" (testRoundTrip "x()[4]")
    , testCase "CallExpression4" (testRoundTrip "x().x")
    , testCase "CallExpression5" (testRoundTrip "x(a,b=2).x")

    , testCase "AssignExpression1" (testRoundTrip "x=1")
    , testCase "AssignExpression1" (testRoundTrip "x*=1")
    , testCase "AssignExpression1" (testRoundTrip "x/=1")
    , testCase "AssignExpression1" (testRoundTrip "x%=1")
    , testCase "AssignExpression1" (testRoundTrip "x+=1")
    , testCase "AssignExpression1" (testRoundTrip "x-=1")
    , testCase "AssignExpression1" (testRoundTrip "x<<=1")
    , testCase "AssignExpression1" (testRoundTrip "x>>=1")
    , testCase "AssignExpression1" (testRoundTrip "x>>>=1")
    , testCase "AssignExpression1" (testRoundTrip "x&=1")
    , testCase "AssignExpression1" (testRoundTrip "x^=1")
    , testCase "AssignExpression1" (testRoundTrip "x|=1")

    , testCase "Block1" (testRoundTrip "{}")
    , testCase "Block2" (testRoundTrip "{x=1}")
    , testCase "Block3" (testRoundTrip "{x=1;y=2}")
    , testCase "Block4" (testRoundTrip "{{}}")
    , testCase "Block5" (testRoundTrip "{{{}}}")

    , testCase "If1" (testRoundTrip "if (1) {}")

    , testCase "IfElse1" (testRoundTrip "if (1) {} else {}")
    , testCase "IfElse2" (testRoundTrip "if (1) x=1; else {}")

    , testCase "DoWhile1" (testRoundTrip "do {x=1} while (true);")
    , testCase "While1"   (testRoundTrip "while(true);")

    , testCase "For1"   (testRoundTrip "for(;;);")
    , testCase "For2"   (testRoundTrip "for(x=1;x<10;x++);")

    , testCase "ForVar1"   (testRoundTrip "for(var x;;);")
    , testCase "ForVar2"   (testRoundTrip "for(var x=1;;);")
    , testCase "ForVar2"   (testRoundTrip "for(var x;y;z){}")

    , testCase "ForIn1"   (testRoundTrip "for(x in 5){}")

    , testCase "ForVarIn1" (testRoundTrip "for(var x in 5){}")

    , testCase "Var1" (testRoundTrip "var x=1;")
    , testCase "Var2" (testRoundTrip "const x=1,y=2;")

    , testCase "Continue1" (testRoundTrip "continue;")
    , testCase "Continue2" (testRoundTrip "continue x;")

    , testCase "Break1" (testRoundTrip "break;")
    , testCase "Break2" (testRoundTrip "break x;")

    , testCase "Return1" (testRoundTrip "return;")
    , testCase "Return2" (testRoundTrip "return x;")

    , testCase "With1" (testRoundTrip "with (x) {};")

    , testCase "Labelled1" (testRoundTrip "abc:x=1")

    , testCase "Switch1" (testRoundTrip "switch (x) {}")
    , testCase "Switch2" (testRoundTrip "switch (x) {case 1:break;}")
    , testCase "Switch3" (testRoundTrip "switch (x) {case 0:\ncase 1:break;}")
    , testCase "Switch4" (testRoundTrip "switch (x) {default:break;}")
    , testCase "Switch5" (testRoundTrip "switch (x) {default:\ncase 1:break;}")

    , testCase "Throw1" (testRoundTrip "throw 1")

    , testCase "Try1" (testRoundTrip "try{}catch(a){}")
    , testCase "Try2" (testRoundTrip "try{}finally{}")
    , testCase "Try3" (testRoundTrip "try{}catch(a){}finally{}")

    , testCase "Try4" (testRoundTrip "try{}catch(a){}catch(b){}finally{}")
    , testCase "Try5" (testRoundTrip "try{}catch(a){}catch(b){}")
    , testCase "Try6" (testRoundTrip "try{}catch(a if true){}catch(b){}")

    , testCase "Function1" (testRoundTrip "function a(){}")
    , testCase "Function2" (testRoundTrip "function a(b,c){}")

    , testCase "Comment1" (testRoundTrip "//blah\nx=1;//foo\na")

    , testCase "Comment2" (testRoundTrip "/*x=1\ny=2\n*/z=2;//foo\na")

    , testCase "min_100_animals1" (testRoundTrip "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\"")

    , testCase "min_100_animals2" (testRoundTrip "Animal=function(){return this.name};")

    , testCase "min_100_animals3" (testRoundTrip "if(a)x=1;y=2")

    , testCase "min_100_animals4" (testRoundTrip "if(a)x=a()y=2")

    , testCase "05_regex"  (testRoundTrip "newlines=spaces.match(/\\n/g)")

    , testCase "05_regex2" (testRoundTrip "x=/\\n/g")

    , testCase "05_regex3" (testRoundTrip "x=i(/[?|^&(){}\\[\\]+\\-*\\/\\.]/g,\"\\\\$&\")")

    , testCase "05_regex4" (testRoundTrip "x=i(/^$/g,\"\\\\$&\")")

    , testCase "05_regex5" (testRoundTrip "if(/^[a-z]/.test(t)){consts+=t.toUpperCase();keywords[t]=i}else consts+=(/^\\W/.test(t)?opTypeNames[t]:t);")

    , testCase "if_semi" (testRoundTrip "if(x);x=1")

    , testCase "67_bob" (testRoundTrip "(match = /^\"(?:\\\\.|[^\"])*\"|^'(?:[^']|\\\\.)*'/(input))")

    , testCase "122_jsexec" (testRoundTrip "v = getValue(execute(n[0], x)) in getValue(execute(n[1], x));")

    , testCase "bug1a" (testRoundTrip "/* */\nfunction f() {\n/*  */\n}")
    , testCase "bug1b" (testRoundTrip "/* **/\nfunction f() {\n/*  */\n}")

    , testCase "unicode1-ws" (testRoundTrip "a \f\v\t\r\n=\x00a0\x1680\x180e\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000\&1;")

    , testCase "unicode2-lt" (testRoundTrip "//comment\x000Ax=1;")
    , testCase "unicode3-lt" (testRoundTrip "//comment\x000Dx=1;")
    , testCase "unicode4-lt" (testRoundTrip "//comment\x2028x=1;")
    , testCase "unicode5-lt" (testRoundTrip "//comment\x2029x=1;")

    , testCase "unicode2" (testRoundTrip "àáâãäå = 1;")

    , testCase "unicode3" (testRoundTrip "$aà = 1;_b=2;\0065a=2")

    , testCase "unicode4" (testRoundTrip "x=\"àáâãäå\";y='\3012a\0068'")

    -- , testCase "unicode5" (testFile "./test/Unicode.js")

    , testCase "bug2.a" (testRoundTrip "function() {\nz = function /*z*/(o) {\nreturn r;\n};}")

    , testCase "bug2.b" (testRoundTrip "function() {\nz = function z(o) {\nreturn r;\n};}")

    -- https://github.com/alanz/hjsmin/issues/#issue/3
    , testCase "bug3" (testRoundTrip "var myLatlng = new google.maps.LatLng(56.8379100, 60.5806664);")

    -- https://github.com/alanz/hjsmin/issues/#issue/4
    , testCase "bug4" (testRoundTrip "/* * geolocation. пытаемся определить свое местоположение * если не получается то используем defaultLocation * @Param {object} map экземпляр карты * @Param {object LatLng} defaultLocation Координаты центра по умолчанию * @Param {function} callbackAfterLocation Фу-ия которая вызывается после * геолокации. Т.к запрос геолокации асинхронен */x")

    , testCase "02_sm.js"   (testRoundTrip "{zero}\none1;two\n{three\nfour;five;\n{\nsix;{seven;}\n}\n}")

    , testCase "02_sm.js.2" (testRoundTrip "{zero}\nget;two\n{three\nfour;set;\n{\nsix;{seven;}\n}\n}")

    , testCase "loc1" (testRoundTrip "x = 1\n  y=2;")

    -- https://github.com/alanz/language-javascript/issues/2
    , testCase "issue2" (testRoundTrip "var img = document.createElement('img');\nimg.src = \"mylogo.jpg\";\n$(img).click(function() {\n   alert('clicked!');\n});")

    -- Working in ECMASCRIPT 5.1 changes
    , testCase "lineTerminatorInString1" (testRoundTrip "x='abc\\\ndef';")
    , testCase "lineTerminatorInString2" (testRoundTrip "x=\"abc\\\ndef\";")
    , testCase "lineTerminatorInString3" (testRoundTrip "x=\"abc\\\rdef\";")
    , testCase "lineTerminatorInString4" (testRoundTrip "x=\"abc\\\x2028 def\";")
    , testCase "lineTerminatorInString5" (testRoundTrip "x=\"abc\\\x2029 def\";")
    , testCase "lineTerminatorInString6" (testRoundTrip "x=\"abc\\\r\ndef\";")

    -- https://github.com/alanz/language-javascript/issues/4
    , testCase "issue4ok"   (testRoundTrip "var k = {\ny: somename\n}")
    , testCase "issue4bug1" (testRoundTrip "var k = {\ny: code\n}")
    , testCase "issue4bug2" (testRoundTrip "var k = {\ny: mode\n}")

    -- https://github.com/alanz/language-javascript/issues/5
    , testCase "issue5bug1" (testRoundTrip "x = { y: 1e8 }")
    , testCase "issue5ok2" (testRoundTrip "{ y: 1e8 }")
    , testCase "issue5ok3" (testRoundTrip "{ y: 18 }")
    , testCase "issue5ok4" (testRoundTrip "x = { y: 18 }")

    -- function body
    , testCase "functionbody"        (testRoundTrip "function foo(a,b,c)\n{x=1;}")
    , testCase "functionexpression" (testRoundTrip "function foo(a,b,c)\n{x=1;}")

    , testCase "fn1" (testRoundTrip "function foo() { return 5; }")
    , testCase "fn2" (testRoundTrip "var foo = function() { return 5; }")
    , testCase "fn3" (testRoundTrip "var foo = function foo() { return 5; }")

    -- Parse failure in hjsmin
    , testCase "parsefail" (testRoundTrip "switch(t){case DIV:         v = u / v; break;}")

    -- https://github.com/alanz/language-javascript/issues/14
    , testCase "issue14" (testRoundTrip "var z = x[i] / y;")

    -- https://github.com/alanz/language-javascript/issues/15
    , testCase "issue15" (testRoundTrip "x\t=1;")

    , testCase "comment-only" (testRoundTrip "// comment\n\n")
    , testCase "empty-src" (testRoundTrip "")
    ]

-- ---------------------------------------------------------------------
-- Test utilities

testRoundTrip:: String -> Assertion
testRoundTrip str = str @=? renderToString (readJs str)

testLiteral :: String -> String -> Assertion
testLiteral  literal expected = expected @=? showStrippedMaybe (parseUsing parseLiteral literal "src")
testLiteralC :: String -> String -> Assertion
testLiteralC literal expected = expected @=? show (parseUsing parseLiteral literal "src")


testPE :: String -> String -> Assertion
testPE  str expected = expected @=? showStrippedMaybe (parseUsing parsePrimaryExpression str "src")
testPEC :: String -> String -> Assertion
testPEC str expected = expected @=? show (parseUsing parsePrimaryExpression str "src")

testStmt :: String -> String -> Assertion
testStmt  str expected = expected @=? showStrippedMaybe (parseUsing parseStatement str "src")
testStmtC :: String -> String -> Assertion
testStmtC str expected = expected @=? show (parseUsing parseStatement str "src")

testProg :: String -> String -> Assertion
testProg  str expected = expected @=? showStrippedMaybe (parseUsing parseProgram str "src")
testProgC :: String -> String -> Assertion
testProgC str expected = expected @=? show (parseUsing parseProgram str "src")

testProgUn :: String -> String -> Assertion
testProgUn str expected = expected @=? show (parseUsing parseProgram str "src")


testFile :: FilePath -> String -> IO ()
testFile fileName expected = do
    res <- parseFile fileName
    expected @=? showStripped res

testFileUtf8 :: FilePath -> String -> IO ()
testFileUtf8 fileName expected = do
    res <- parseFileUtf8 fileName
    expected @=? showStripped res

testLexer :: String -> String -> Assertion
testLexer str expected =
    expected @=? either id stringify (alexTestTokeniser str)

stringify :: [Token] -> String
stringify xs = "[" ++ intercalate "," (map (takeWhile (/= ' ') . show) xs) ++ "]"
