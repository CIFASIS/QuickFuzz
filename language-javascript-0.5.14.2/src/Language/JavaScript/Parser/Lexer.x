{

module Language.JavaScript.Parser.Lexer
    ( Token(..)
    , AlexPosn(..)
    , Alex
    , lexCont
    , alexError
    , runAlex
    , alexTestTokeniser
    ) where

import Language.JavaScript.Parser.LexerUtils
import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import qualified Data.Map as Map

}

-- %wrapper "basic"
-- %wrapper "monad"
%wrapper "monadUserState"
-- %wrapper "monad-bytestring"

-- character sets
$lf = \n  -- line feed
$cr = \r  -- carriage return
$ht = \t  -- horizontal tab
$sq = '   -- single quote
$dq = \"  -- double quote
$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$digit    = 0-9
$non_zero_digit = 1-9
$ident_letter = [a-zA-Z_]
@eol_pattern = $lf | $cr $lf | $cr $lf

$ls = \x2028
$ps = \x2029
@LineTerminatorSequence = $lf | $cr | $ls | $ps | $cr $lf


$any_char = [\x00-\xff]
$any_unicode_char = [\x00-\xffff]


$eol_char = [\x000A\x000D\x2028\x2029] -- any end of line character
-- $eol_char = [$lf $cr] -- any end of line character
$not_eol_char = ~$eol_char -- anything but an end of line character


-- From GOLD Parser
-- {ID Head}      = {Letter} + [_] + [$]
@IDHead = $alpha | [_] | [\$]

-- {ID Tail}      = {Alphanumeric} + [_] + [$]
@IDTail = $alpha | $digit | [_] | [\$]

-- {String Chars1} = {Printable} + {HT} - ["\]
-- {String Chars2} = {Printable} + {HT} - [\'']
$StringChars1 = [$printable $ht] # [$dq \\]
$StringChars2 = [$printable $ht] # [$sq \\]

-- LineContinuation :: \ LineTerminatorSequence
@LineContinuation = [\\] @LineTerminatorSequence


$short_str_char = [^ \n \r ' \" \\]

-- {Hex Digit}    = {Digit} + [ABCDEF] + [abcdef]
@HexDigit = $digit | [a-fA-F]
-- {Oct Digit}    = {Digit} + [01234567]
@OctDigit = $digit | [0-7]
-- {RegExp Chars} = {Letter}+{Digit}+['^']+['$']+['*']+['+']+['?']+['{']+['}']+['|']+['-']+['.']+[',']+['#']+['[']+[']']+['_']+['<']+['>']
-- $RegExpChars = [$alpha $digit \^\$\*\+\?\{\}\|\-\.\,\#\[\]\_\<\>]
-- $RegExpChars = [$printable] # [\\]
-- {Non Terminator} = {String Chars1} - {CR} - {LF}
-- $NonTerminator = $StringChars1 # [$cr $lf]
$NonTerminator = [$printable] # [$cr $lf]
-- {Non Zero Digits}={Digit}-[0]

-- ~ (LineTerminator | MUL | BSLASH | DIV)
$RegExpFirstChar = [$printable] # [ $cr $lf \* \\ \/]
-- ~ ( LineTerminator | BSLASH | DIV )
$RegExpChars = [$printable] # [ $cr $lf \\ \/]

$MultiLineNotAsteriskChar               = [$any_unicode_char] # [\*]
$MultiLineNotForwardSlashOrAsteriskChar = [$any_unicode_char] # [\* \/]

-- See http://blog.stevenlevithan.com/archives/javascript-regex-and-unicode
    -- *  \u0009 — Tab — \t
    -- * \u000a — Line feed — \n — (newline character)
    -- * \u000b — Vertical tab — \v
    -- * \u000c — Form feed — \f
    -- * \u000d — Carriage return — \r — (newline character)
    -- * \u0020 — Space
    -- * \u00a0 — No-break space
    -- * \u1680 — Ogham space mark
    -- * \u180e — Mongolian vowel separator
    -- * \u2000 — En quad
    -- * \u2001 — Em quad
    -- * \u2002 — En space
    -- * \u2003 — Em space
    -- * \u2004 — Three-per-em space
    -- * \u2005 — Four-per-em space
    -- * \u2006 — Six-per-em space
    -- * \u2007 — Figure space
    -- * \u2008 — Punctuation space
    -- * \u2009 — Thin space
    -- * \u200a — Hair space
    -- * \u2028 — Line separator — (newline character)
    -- * \u2029 — Paragraph separator — (newline character)
    -- * \u202f — Narrow no-break space
    -- * \u205f — Medium mathematical space
    -- * \u3000 — Ideographic space

-- $white_char   = [\ \f\v\t\r\n]
-- Note: from edition 5 the BOM (\xfeff) is also considered whitespace
$white_char = [\x0009\x000a\x000b\x000c\x000d\x0020\x00a0\x1680\x180e\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000\xfeff]

-- Identifier characters
-- UnicodeLetter
--       any character in the Unicode categories “Uppercase letter (Lu)”, “Lowercase letter (Ll)”,
--       “Titlecase letter (Lt)”, “Modifier letter (Lm)”, “Other letter (Lo)”, or “Letter number (Nl)”.

-- http://www.fileformat.info/info/unicode/category/Lu/list.htm etc, see unicode/doit.sh
$UnicodeLetter = [\x41-\x5a\x61-\x7a\xaa-\xaa\xb5-\xb5\xba-\xba\xc0-\xd6\xd8-\xf6\xf8-\x2c1\x2c6-\x2d1\x2e0-\x2e4\x2ec-\x2ec\x2ee-\x2ee\x370-\x374\x376-\x377\x37a-\x37d\x386-\x386\x388-\x38a\x38c-\x38c\x38e-\x3a1\x3a3-\x3f5\x3f7-\x481\x48a-\x527\x531-\x556\x559-\x559\x561-\x587\x5d0-\x5ea\x5f0-\x5f2\x620-\x64a\x66e-\x66f\x671-\x6d3\x6d5-\x6d5\x6e5-\x6e6\x6ee-\x6ef\x6fa-\x6fc\x6ff-\x6ff\x710-\x710\x712-\x72f\x74d-\x7a5\x7b1-\x7b1\x7ca-\x7ea\x7f4-\x7f5\x7fa-\x7fa\x800-\x815\x81a-\x81a\x824-\x824\x828-\x828\x840-\x858\x904-\x939\x93d-\x93d\x950-\x950\x958-\x961\x971-\x977\x979-\x97f\x985-\x98c\x98f-\x990\x993-\x9a8\x9aa-\x9b0\x9b2-\x9b2\x9b6-\x9b9\x9bd-\x9bd\x9ce-\x9ce\x9dc-\x9dd\x9df-\x9e1\x9f0-\x9f1\xa05-\xa0a\xa0f-\xa10\xa13-\xa28\xa2a-\xa30\xa32-\xa33\xa35-\xa36\xa38-\xa39\xa59-\xa5c\xa5e-\xa5e\xa72-\xa74\xa85-\xa8d\xa8f-\xa91\xa93-\xaa8\xaaa-\xab0\xab2-\xab3\xab5-\xab9\xabd-\xabd\xad0-\xad0\xae0-\xae1\xb05-\xb0c\xb0f-\xb10\xb13-\xb28\xb2a-\xb30\xb32-\xb33\xb35-\xb39\xb3d-\xb3d\xb5c-\xb5d\xb5f-\xb61\xb71-\xb71\xb83-\xb83\xb85-\xb8a\xb8e-\xb90\xb92-\xb95\xb99-\xb9a\xb9c-\xb9c\xb9e-\xb9f\xba3-\xba4\xba8-\xbaa\xbae-\xbb9\xbd0-\xbd0\xc05-\xc0c\xc0e-\xc10\xc12-\xc28\xc2a-\xc33\xc35-\xc39\xc3d-\xc3d\xc58-\xc59\xc60-\xc61\xc85-\xc8c\xc8e-\xc90\xc92-\xca8\xcaa-\xcb3\xcb5-\xcb9\xcbd-\xcbd\xcde-\xcde\xce0-\xce1\xcf1-\xcf2\xd05-\xd0c\xd0e-\xd10\xd12-\xd3a\xd3d-\xd3d\xd4e-\xd4e\xd60-\xd61\xd7a-\xd7f\xd85-\xd96\xd9a-\xdb1\xdb3-\xdbb\xdbd-\xdbd\xdc0-\xdc6\xe01-\xe30\xe32-\xe33\xe40-\xe46\xe81-\xe82\xe84-\xe84\xe87-\xe88\xe8a-\xe8a\xe8d-\xe8d\xe94-\xe97\xe99-\xe9f\xea1-\xea3\xea5-\xea5\xea7-\xea7\xeaa-\xeab\xead-\xeb0\xeb2-\xeb3\xebd-\xebd\xec0-\xec4\xec6-\xec6\xedc-\xedd\xf00-\xf00\xf40-\xf47\xf49-\xf6c\xf88-\xf8c\x1000-\x1000\x10000-\x1000b\x1000d-\x1000f\x1001-\x1001\x10010-\x1001f\x1002-\x1002\x10020-\x10026\x10028-\x1002f\x1003-\x1003\x10030-\x1003a\x1003c-\x1003d\x1003f-\x1003f\x1004-\x1004\x10040-\x1004d\x1005-\x1005\x10050-\x1005d\x1006-\x1008\x10080-\x1008f\x1009-\x1009\x10090-\x1009f\x100a-\x100a\x100a0-\x100af\x100b-\x100b\x100b0-\x100bf\x100c-\x100c\x100c0-\x100cf\x100d-\x100d\x100d0-\x100df\x100e-\x100e\x100e0-\x100ef\x100f-\x100f\x100f0-\x100fa\x1010-\x1014\x10140-\x1014f\x1015-\x1015\x10150-\x1015f\x1016-\x1016\x10160-\x1016f\x1017-\x1017\x10170-\x10174\x1018-\x1028\x10280-\x1028f\x1029-\x1029\x10290-\x1029c\x102a-\x102a\x102a0-\x102d0\x10300-\x1031e\x10330-\x1034a\x10380-\x1039d\x103a0-\x103c3\x103c8-\x103cf\x103d1-\x103d5\x103f-\x103f\x10400-\x1049d\x1050-\x1055\x105a-\x105d\x1061-\x1061\x1065-\x1066\x106e-\x1070\x1075-\x1080\x10800-\x10805\x10808-\x10808\x1080a-\x1080f\x1081-\x1081\x10810-\x10835\x10837-\x10838\x1083c-\x1083c\x1083f-\x10855\x108e-\x108e\x10900-\x10915\x10920-\x10939\x10a0-\x10a0\x10a00-\x10a00\x10a1-\x10a1\x10a10-\x10a13\x10a15-\x10a17\x10a19-\x10a1f\x10a2-\x10a2\x10a20-\x10a2f\x10a3-\x10a3\x10a30-\x10a33\x10a4-\x10a6\x10a60-\x10a6f\x10a7-\x10a7\x10a70-\x10a7c\x10a8-\x10b0\x10b00-\x10b0f\x10b1-\x10b1\x10b10-\x10b1f\x10b2-\x10b2\x10b20-\x10b2f\x10b3-\x10b3\x10b30-\x10b35\x10b4-\x10b4\x10b40-\x10b4f\x10b5-\x10b5\x10b50-\x10b55\x10b6-\x10b6\x10b60-\x10b6f\x10b7-\x10b7\x10b70-\x10b72\x10b8-\x10c0\x10c00-\x10c0f\x10c1-\x10c1\x10c10-\x10c1f\x10c2-\x10c2\x10c20-\x10c2f\x10c3-\x10c3\x10c30-\x10c3f\x10c4-\x10c4\x10c40-\x10c48\x10c5-\x10c5\x10d0-\x10fa\x10fc-\x10fc\x1100-\x1100\x11003-\x1100f\x1101-\x1101\x11010-\x1101f\x1102-\x1102\x11020-\x1102f\x1103-\x1103\x11030-\x11037\x1104-\x1108\x11083-\x1108f\x1109-\x1109\x11090-\x1109f\x110a-\x110a\x110a0-\x110af\x110b-\x1200\x12000-\x1200f\x1201-\x1201\x12010-\x1201f\x1202-\x1202\x12020-\x1202f\x1203-\x1203\x12030-\x1203f\x1204-\x1204\x12040-\x1204f\x1205-\x1205\x12050-\x1205f\x1206-\x1206\x12060-\x1206f\x1207-\x1207\x12070-\x1207f\x1208-\x1208\x12080-\x1208f\x1209-\x1209\x12090-\x1209f\x120a-\x120a\x120a0-\x120af\x120b-\x120b\x120b0-\x120bf\x120c-\x120c\x120c0-\x120cf\x120d-\x120d\x120d0-\x120df\x120e-\x120e\x120e0-\x120ef\x120f-\x120f\x120f0-\x120ff\x1210-\x1210\x12100-\x1210f\x1211-\x1211\x12110-\x1211f\x1212-\x1212\x12120-\x1212f\x1213-\x1213\x12130-\x1213f\x1214-\x1214\x12140-\x1214f\x1215-\x1215\x12150-\x1215f\x1216-\x1216\x12160-\x1216f\x1217-\x1217\x12170-\x1217f\x1218-\x1218\x12180-\x1218f\x1219-\x1219\x12190-\x1219f\x121a-\x121a\x121a0-\x121af\x121b-\x121b\x121b0-\x121bf\x121c-\x121c\x121c0-\x121cf\x121d-\x121d\x121d0-\x121df\x121e-\x121e\x121e0-\x121ef\x121f-\x121f\x121f0-\x121ff\x1220-\x1220\x12200-\x1220f\x1221-\x1221\x12210-\x1221f\x1222-\x1222\x12220-\x1222f\x1223-\x1223\x12230-\x1223f\x1224-\x1224\x12240-\x1224f\x1225-\x1225\x12250-\x1225f\x1226-\x1226\x12260-\x1226f\x1227-\x1227\x12270-\x1227f\x1228-\x1228\x12280-\x1228f\x1229-\x1229\x12290-\x1229f\x122a-\x122a\x122a0-\x122af\x122b-\x122b\x122b0-\x122bf\x122c-\x122c\x122c0-\x122cf\x122d-\x122d\x122d0-\x122df\x122e-\x122e\x122e0-\x122ef\x122f-\x122f\x122f0-\x122ff\x1230-\x1230\x12300-\x1230f\x1231-\x1231\x12310-\x1231f\x1232-\x1232\x12320-\x1232f\x1233-\x1233\x12330-\x1233f\x1234-\x1234\x12340-\x1234f\x1235-\x1235\x12350-\x1235f\x1236-\x1236\x12360-\x1236e\x1237-\x1240\x12400-\x1240f\x1241-\x1241\x12410-\x1241f\x1242-\x1242\x12420-\x1242f\x1243-\x1243\x12430-\x1243f\x1244-\x1244\x12440-\x1244f\x1245-\x1245\x12450-\x1245f\x1246-\x1246\x12460-\x12462\x1247-\x1248\x124a-\x124d\x1250-\x1256\x1258-\x1258\x125a-\x125d\x1260-\x1288\x128a-\x128d\x1290-\x12b0\x12b2-\x12b5\x12b8-\x12be\x12c0-\x12c0\x12c2-\x12c5\x12c8-\x12d6\x12d8-\x1300\x13000-\x1300f\x1301-\x1301\x13010-\x1301f\x1302-\x1302\x13020-\x1302f\x1303-\x1303\x13030-\x1303f\x1304-\x1304\x13040-\x1304f\x1305-\x1305\x13050-\x1305f\x1306-\x1306\x13060-\x1306f\x1307-\x1307\x13070-\x1307f\x1308-\x1308\x13080-\x1308f\x1309-\x1309\x13090-\x1309f\x130a-\x130a\x130a0-\x130af\x130b-\x130b\x130b0-\x130bf\x130c-\x130c\x130c0-\x130cf\x130d-\x130d\x130d0-\x130df\x130e-\x130e\x130e0-\x130ef\x130f-\x130f\x130f0-\x130ff\x1310-\x1310\x13100-\x1311f\x1312-\x1312\x13120-\x1312f\x1313-\x1313\x13130-\x1313f\x1314-\x1314\x13140-\x1314f\x1315-\x1315\x13150-\x1317f\x1318-\x1318\x13180-\x1318f\x1319-\x1319\x13190-\x1319f\x131a-\x131a\x131a0-\x131af\x131b-\x131b\x131b0-\x131bf\x131c-\x131c\x131c0-\x131cf\x131d-\x131d\x131d0-\x131df\x131e-\x131e\x131e0-\x131ef\x131f-\x131f\x131f0-\x131ff\x1320-\x1320\x13200-\x1320f\x1321-\x1321\x13210-\x1321f\x1322-\x1322\x13220-\x1322f\x1323-\x1323\x13230-\x1323f\x1324-\x1324\x13240-\x1324f\x1325-\x1325\x13250-\x1325f\x1326-\x1326\x13260-\x1326f\x1327-\x1327\x13270-\x1327f\x1328-\x1328\x13280-\x1328f\x1329-\x1329\x13290-\x1329f\x132a-\x132a\x132a0-\x132af\x132b-\x132b\x132b0-\x132bf\x132c-\x132c\x132c0-\x132cf\x132d-\x132d\x132d0-\x132df\x132e-\x132e\x132e0-\x132ef\x132f-\x132f\x132f0-\x132ff\x1330-\x1330\x13300-\x1330f\x1331-\x1331\x13310-\x1331f\x1332-\x1332\x13320-\x1332f\x1333-\x1333\x13330-\x1333f\x1334-\x1334\x13340-\x1334f\x1335-\x1335\x13350-\x1335f\x1336-\x1336\x13360-\x1336f\x1337-\x1337\x13370-\x1337f\x1338-\x1338\x13380-\x1338f\x1339-\x1339\x13390-\x1339f\x133a-\x133a\x133a0-\x133af\x133b-\x133b\x133b0-\x133bf\x133c-\x133c\x133c0-\x133cf\x133d-\x133d\x133d0-\x133df\x133e-\x133e\x133e0-\x133ef\x133f-\x133f\x133f0-\x133ff\x1340-\x1340\x13400-\x1340f\x1341-\x1341\x13410-\x1341f\x1342-\x1342\x13420-\x1342e\x1343-\x135a\x1380-\x138f\x13a0-\x13f4\x1401-\x166c\x166f-\x167f\x16800-\x1680f\x1681-\x1681\x16810-\x1681f\x1682-\x1682\x16820-\x1682f\x1683-\x1683\x16830-\x1683f\x1684-\x1684\x16840-\x1684f\x1685-\x1685\x16850-\x1685f\x1686-\x1686\x16860-\x1686f\x1687-\x1687\x16870-\x1687f\x1688-\x1688\x16880-\x1688f\x1689-\x1689\x16890-\x1689f\x168a-\x168a\x168a0-\x168af\x168b-\x168b\x168b0-\x168bf\x168c-\x168c\x168c0-\x168cf\x168d-\x168d\x168d0-\x168df\x168e-\x168e\x168e0-\x168ef\x168f-\x168f\x168f0-\x168ff\x1690-\x1690\x16900-\x1690f\x1691-\x1691\x16910-\x1691f\x1692-\x1692\x16920-\x1692f\x1693-\x1693\x16930-\x1693f\x1694-\x1694\x16940-\x1694f\x1695-\x1695\x16950-\x1695f\x1696-\x1696\x16960-\x1696f\x1697-\x1697\x16970-\x1697f\x1698-\x1698\x16980-\x1698f\x1699-\x1699\x16990-\x1699f\x169a-\x169a\x169a0-\x169ff\x16a0-\x16a0\x16a00-\x16a0f\x16a1-\x16a1\x16a10-\x16a1f\x16a2-\x16a2\x16a20-\x16a2f\x16a3-\x16a3\x16a30-\x16a38\x16a4-\x16ea\x16ee-\x16f0\x1700-\x170c\x170e-\x1711\x1720-\x1731\x1740-\x1751\x1760-\x176c\x176e-\x1770\x1780-\x17b3\x17d7-\x17d7\x17dc-\x17dc\x1820-\x1877\x1880-\x18a8\x18aa-\x18aa\x18b0-\x18f5\x1900-\x191c\x1950-\x196d\x1970-\x1974\x1980-\x19ab\x19c1-\x19c7\x1a00-\x1a16\x1a20-\x1a54\x1aa7-\x1aa7\x1b000-\x1b001\x1b05-\x1b33\x1b45-\x1b4b\x1b83-\x1ba0\x1bae-\x1baf\x1bc0-\x1be5\x1c00-\x1c23\x1c4d-\x1c4f\x1c5a-\x1c7d\x1ce9-\x1cec\x1cee-\x1cf1\x1d00-\x1d40\x1d400-\x1d40f\x1d41-\x1d41\x1d410-\x1d41f\x1d42-\x1d42\x1d420-\x1d42f\x1d43-\x1d43\x1d430-\x1d43f\x1d44-\x1d44\x1d440-\x1d44f\x1d45-\x1d45\x1d450-\x1d454\x1d456-\x1d45f\x1d46-\x1d46\x1d460-\x1d46f\x1d47-\x1d47\x1d470-\x1d47f\x1d48-\x1d48\x1d480-\x1d48f\x1d49-\x1d49\x1d490-\x1d49c\x1d49e-\x1d49f\x1d4a-\x1d4a\x1d4a2-\x1d4a2\x1d4a5-\x1d4a6\x1d4a9-\x1d4ac\x1d4ae-\x1d4af\x1d4b-\x1d4b\x1d4b0-\x1d4b9\x1d4bb-\x1d4bb\x1d4bd-\x1d4bf\x1d4c-\x1d4c\x1d4c0-\x1d4c3\x1d4c5-\x1d4cf\x1d4d-\x1d4d\x1d4d0-\x1d4df\x1d4e-\x1d4e\x1d4e0-\x1d4ef\x1d4f-\x1d4f\x1d4f0-\x1d4ff\x1d50-\x1d50\x1d500-\x1d505\x1d507-\x1d50a\x1d50d-\x1d50f\x1d51-\x1d51\x1d510-\x1d514\x1d516-\x1d51c\x1d51e-\x1d51f\x1d52-\x1d52\x1d520-\x1d52f\x1d53-\x1d53\x1d530-\x1d539\x1d53b-\x1d53e\x1d54-\x1d54\x1d540-\x1d544\x1d546-\x1d546\x1d54a-\x1d54f\x1d55-\x1d55\x1d550-\x1d550\x1d552-\x1d55f\x1d56-\x1d56\x1d560-\x1d56f\x1d57-\x1d57\x1d570-\x1d57f\x1d58-\x1d58\x1d580-\x1d58f\x1d59-\x1d59\x1d590-\x1d59f\x1d5a-\x1d5a\x1d5a0-\x1d5af\x1d5b-\x1d5b\x1d5b0-\x1d5bf\x1d5c-\x1d5c\x1d5c0-\x1d5cf\x1d5d-\x1d5d\x1d5d0-\x1d5df\x1d5e-\x1d5e\x1d5e0-\x1d5ef\x1d5f-\x1d5f\x1d5f0-\x1d5ff\x1d60-\x1d60\x1d600-\x1d60f\x1d61-\x1d61\x1d610-\x1d61f\x1d62-\x1d62\x1d620-\x1d62f\x1d63-\x1d63\x1d630-\x1d63f\x1d64-\x1d64\x1d640-\x1d64f\x1d65-\x1d65\x1d650-\x1d65f\x1d66-\x1d66\x1d660-\x1d66f\x1d67-\x1d67\x1d670-\x1d67f\x1d68-\x1d68\x1d680-\x1d68f\x1d69-\x1d69\x1d690-\x1d69f\x1d6a-\x1d6a\x1d6a0-\x1d6a5\x1d6a8-\x1d6af\x1d6b-\x1d6b\x1d6b0-\x1d6bf\x1d6c-\x1d6c\x1d6c0-\x1d6c0\x1d6c2-\x1d6cf\x1d6d-\x1d6d\x1d6d0-\x1d6da\x1d6dc-\x1d6df\x1d6e-\x1d6e\x1d6e0-\x1d6ef\x1d6f-\x1d6f\x1d6f0-\x1d6fa\x1d6fc-\x1d6ff\x1d70-\x1d70\x1d700-\x1d70f\x1d71-\x1d71\x1d710-\x1d714\x1d716-\x1d71f\x1d72-\x1d72\x1d720-\x1d72f\x1d73-\x1d73\x1d730-\x1d734\x1d736-\x1d73f\x1d74-\x1d74\x1d740-\x1d74e\x1d75-\x1d75\x1d750-\x1d75f\x1d76-\x1d76\x1d760-\x1d76e\x1d77-\x1d77\x1d770-\x1d77f\x1d78-\x1d78\x1d780-\x1d788\x1d78a-\x1d78f\x1d79-\x1d79\x1d790-\x1d79f\x1d7a-\x1d7a\x1d7a0-\x1d7a8\x1d7aa-\x1d7af\x1d7b-\x1d7b\x1d7b0-\x1d7bf\x1d7c-\x1d7c\x1d7c0-\x1d7c2\x1d7c4-\x1d7cb\x1d7d-\x1dbf\x1e00-\x1f15\x1f18-\x1f1d\x1f20-\x1f45\x1f48-\x1f4d\x1f50-\x1f57\x1f59-\x1f59\x1f5b-\x1f5b\x1f5d-\x1f5d\x1f5f-\x1f7d\x1f80-\x1fb4\x1fb6-\x1fbc\x1fbe-\x1fbe\x1fc2-\x1fc4\x1fc6-\x1fcc\x1fd0-\x1fd3\x1fd6-\x1fdb\x1fe0-\x1fec\x1ff2-\x1ff4\x1ff6-\x1ffc\x20000-\x20000\x2071-\x2071\x207f-\x207f\x2090-\x209c\x2102-\x2102\x2107-\x2107\x210a-\x2113\x2115-\x2115\x2119-\x211d\x2124-\x2124\x2126-\x2126\x2128-\x2128\x212a-\x212d\x212f-\x2139\x213c-\x213f\x2145-\x2149\x214e-\x214e\x2160-\x2188\x2a6d6-\x2a6d6\x2a700-\x2a700\x2b734-\x2b734\x2b740-\x2b740\x2b81d-\x2b81d\x2c00-\x2c2e\x2c30-\x2c5e\x2c60-\x2ce4\x2ceb-\x2cee\x2d00-\x2d25\x2d30-\x2d65\x2d6f-\x2d6f\x2d80-\x2d96\x2da0-\x2da6\x2da8-\x2dae\x2db0-\x2db6\x2db8-\x2dbe\x2dc0-\x2dc6\x2dc8-\x2dce\x2dd0-\x2dd6\x2dd8-\x2dde\x2e2f-\x2e2f\x2f800-\x2fa1d\x3005-\x3007\x3021-\x3029\x3031-\x3035\x3038-\x303c\x3041-\x3096\x309d-\x309f\x30a1-\x30fa\x30fc-\x30ff\x3105-\x312d\x3131-\x318e\x31a0-\x31ba\x31f0-\x31ff\x3400-\x3400\x4db5-\x4db5\x4e00-\x4e00\x9fcb-\x9fcb\xa000-\xa48c\xa4d0-\xa4fd\xa500-\xa60c\xa610-\xa61f\xa62a-\xa62b\xa640-\xa66e\xa67f-\xa697\xa6a0-\xa6ef\xa717-\xa71f\xa722-\xa788\xa78b-\xa78e\xa790-\xa791\xa7a0-\xa7a9\xa7fa-\xa801\xa803-\xa805\xa807-\xa80a\xa80c-\xa822\xa840-\xa873\xa882-\xa8b3\xa8f2-\xa8f7\xa8fb-\xa8fb\xa90a-\xa925\xa930-\xa946\xa960-\xa97c\xa984-\xa9b2\xa9cf-\xa9cf\xaa00-\xaa28\xaa40-\xaa42\xaa44-\xaa4b\xaa60-\xaa76\xaa7a-\xaa7a\xaa80-\xaaaf\xaab1-\xaab1\xaab5-\xaab6\xaab9-\xaabd\xaac0-\xaac0\xaac2-\xaac2\xaadb-\xaadd\xab01-\xab06\xab09-\xab0e\xab11-\xab16\xab20-\xab26\xab28-\xab2e\xabc0-\xabe2\xac00-\xac00\xd7a3-\xd7a3\xd7b0-\xd7c6\xd7cb-\xd7fb\xf900-\xfa2d\xfa30-\xfa6d\xfa70-\xfad9\xfb00-\xfb06\xfb13-\xfb17\xfb1d-\xfb1d\xfb1f-\xfb28\xfb2a-\xfb36\xfb38-\xfb3c\xfb3e-\xfb3e\xfb40-\xfb41\xfb43-\xfb44\xfb46-\xfbb1\xfbd3-\xfd3d\xfd50-\xfd8f\xfd92-\xfdc7\xfdf0-\xfdfb\xfe70-\xfe74\xfe76-\xfefc\xff21-\xff3a\xff41-\xff5a\xff66-\xffbe\xffc2-\xffc7\xffca-\xffcf\xffd2-\xffd7]

-- UnicodeCombiningMark
--       any character in the Unicode categories “Non-spacing mark (Mn)” or “Combining spacing mark (Mc)”
$UnicodeCombiningMark = [\x300-\x36f\x483-\x487\x591-\x5bd\x5bf-\x5bf\x5c1-\x5c2\x5c4-\x5c5\x5c7-\x5c7\x610-\x61a\x64b-\x65f\x670-\x670\x6d6-\x6dc\x6df-\x6e4\x6e7-\x6e8\x6ea-\x6ed\x711-\x711\x730-\x74a\x7a6-\x7b0\x7eb-\x7f3\x816-\x819\x81b-\x823\x825-\x827\x829-\x82d\x859-\x85b\x900-\x903\x93a-\x93c\x93e-\x94f\x951-\x957\x962-\x963\x981-\x983\x9bc-\x9bc\x9be-\x9c4\x9c7-\x9c8\x9cb-\x9cd\x9d7-\x9d7\x9e2-\x9e3\xa01-\xa03\xa3c-\xa3c\xa3e-\xa42\xa47-\xa48\xa4b-\xa4d\xa51-\xa51\xa70-\xa71\xa75-\xa75\xa81-\xa83\xabc-\xabc\xabe-\xac5\xac7-\xac9\xacb-\xacd\xae2-\xae3\xb01-\xb03\xb3c-\xb3c\xb3e-\xb44\xb47-\xb48\xb4b-\xb4d\xb56-\xb57\xb62-\xb63\xb82-\xb82\xbbe-\xbc2\xbc6-\xbc8\xbca-\xbcd\xbd7-\xbd7\xc01-\xc03\xc3e-\xc44\xc46-\xc48\xc4a-\xc4d\xc55-\xc56\xc62-\xc63\xc82-\xc83\xcbc-\xcbc\xcbe-\xcc4\xcc6-\xcc8\xcca-\xccd\xcd5-\xcd6\xce2-\xce3\xd02-\xd03\xd3e-\xd44\xd46-\xd48\xd4a-\xd4d\xd57-\xd57\xd62-\xd63\xd82-\xd83\xdca-\xdca\xdcf-\xdd4\xdd6-\xdd6\xdd8-\xddf\xdf2-\xdf3\xe31-\xe31\xe34-\xe3a\xe47-\xe4e\xeb1-\xeb1\xeb4-\xeb9\xebb-\xebc\xec8-\xecd\xf18-\xf19\xf35-\xf35\xf37-\xf37\xf39-\xf39\xf3e-\xf3f\xf71-\xf84\xf86-\xf87\xf8d-\xf97\xf99-\xfbc\xfc6-\xfc6\x101fd-\x101fd\x102b-\x103e\x1056-\x1059\x105e-\x1060\x1062-\x1064\x1067-\x106d\x1071-\x1074\x1082-\x108d\x108f-\x108f\x109a-\x109d\x10a01-\x10a03\x10a05-\x10a06\x10a0c-\x10a0f\x10a38-\x10a3a\x10a3f-\x10a3f\x11000-\x11002\x11038-\x11046\x11080-\x11082\x110b0-\x110ba\x135d-\x135f\x1712-\x1714\x1732-\x1734\x1752-\x1753\x1772-\x1773\x17b6-\x17d3\x17dd-\x17dd\x180b-\x180d\x18a9-\x18a9\x1920-\x192b\x1930-\x193b\x19b0-\x19c0\x19c8-\x19c9\x1a17-\x1a1b\x1a55-\x1a5e\x1a60-\x1a7c\x1a7f-\x1a7f\x1b00-\x1b04\x1b34-\x1b44\x1b6b-\x1b73\x1b80-\x1b82\x1ba1-\x1baa\x1be6-\x1bf3\x1c24-\x1c37\x1cd0-\x1cd2\x1cd4-\x1ce8\x1ced-\x1ced\x1cf2-\x1cf2\x1d165-\x1d169\x1d16d-\x1d172\x1d17b-\x1d182\x1d185-\x1d18b\x1d1aa-\x1d1ad\x1d242-\x1d244\x1dc0-\x1de6\x1dfc-\x1dff\x20d0-\x20dc\x20e1-\x20e1\x20e5-\x20f0\x2cef-\x2cf1\x2d7f-\x2d7f\x2de0-\x2dff\x302a-\x302f\x3099-\x309a\xa66f-\xa66f\xa67c-\xa67d\xa6f0-\xa6f1\xa802-\xa802\xa806-\xa806\xa80b-\xa80b\xa823-\xa827\xa880-\xa881\xa8b4-\xa8c4\xa8e0-\xa8f1\xa926-\xa92d\xa947-\xa953\xa980-\xa983\xa9b3-\xa9c0\xaa29-\xaa36\xaa43-\xaa43\xaa4c-\xaa4d\xaa7b-\xaa7b\xaab0-\xaab0\xaab2-\xaab4\xaab7-\xaab8\xaabe-\xaabf\xaac1-\xaac1\xabe3-\xabea\xabec-\xabed\xe0100-\xe01ef\xfb1e-\xfb1e\xfe00-\xfe0f]

-- UnicodeDigit
--       any character in the Unicode category “Decimal number (Nd)”
$UnicodeDigit = [\x30-\x39\x660-\x669\x6f0-\x6f9\x7c0-\x7c9\x966-\x96f\x9e6-\x9ef\xa66-\xa6f\xae6-\xaef\xb66-\xb6f\xbe6-\xbef\xc66-\xc6f\xce6-\xcef\xd66-\xd6f\xe50-\xe59\xed0-\xed9\xf20-\xf29\x1040-\x1049\x104a0-\x104a9\x1090-\x1099\x11066-\x1106f\x17e0-\x17e9\x1810-\x1819\x1946-\x194f\x19d0-\x19d9\x1a80-\x1a89\x1a90-\x1a99\x1b50-\x1b59\x1bb0-\x1bb9\x1c40-\x1c49\x1c50-\x1c59\x1d7ce-\x1d7ff\xa620-\xa629\xa8d0-\xa8d9\xa900-\xa909\xa9d0-\xa9d9\xaa50-\xaa59\xabf0-\xabf9]

-- UnicodeConnectorPunctuation
--       any character in the Unicode category “Connector punctuation (Pc)”
$UnicodeConnectorPunctuation = [\x5f-\x5f\x203f-\x2040\x2054-\x2054\xfe33-\xfe34\xfe4d-\xfe4f]

-- UnicodeEscapeSequence ::
--       u HexDigit HexDigit HexDigit HexDigit
$HexDigit = [0-9a-fA-F]
@UnicodeEscapeSequence = u $HexDigit $HexDigit $HexDigit $HexDigit

-- IdentifierStart ::
--         UnicodeLetter
--         $
--         _
--         \ UnicodeEscapeSequence
@IdentifierStart = $UnicodeLetter | [\$] | [_] | [\\] @UnicodeEscapeSequence

-- IdentifierPart ::
--         IdentifierStart
--         UnicodeCombiningMark
--         UnicodeDigit
--         UnicodeConnectorPunctuation
--         \ UnicodeEscapeSequence

$ZWNJ = [\x200c]
$ZWJ  = [\x200d]
@IdentifierPart = @IdentifierStart | $UnicodeCombiningMark | $UnicodeDigit | UnicodeConnectorPunctuation
        [\\] @UnicodeEscapeSequence | $ZWNJ | $ZWJ

-- ! ------------------------------------------------- Terminals
tokens :-

-- State: 0 is regex allowed, 1 is / or /= allowed

<0> () ; -- { registerStates lexToken reg divide }

-- Skip Whitespace
<reg,divide> $white_char+   { adapt (mkString wsToken) }

-- Skip one line comment
<reg,divide> "//"($not_eol_char)*   { adapt (mkString commentToken) }

-- ---------------------------------------------------------------------
-- Comment definition from the ECMAScript spec, ver 3

-- MultiLineComment ::
--        /* MultiLineCommentChars(opt) */
-- MultiLineCommentChars ::
--        MultiLineNotAsteriskChar MultiLineCommentChars(opt)
--        * PostAsteriskCommentChars(opt)
-- PostAsteriskCommentChars ::
--        MultiLineNotForwardSlashOrAsteriskChar MultiLineCommentChars(opt)
--        * PostAsteriskCommentChars(opt)
-- MultiLineNotAsteriskChar ::
--        SourceCharacter but not asterisk *
-- MultiLineNotForwardSlashOrAsteriskChar ::
--        SourceCharacter but not forward-slash / or asterisk *

-- Skip multi-line comments. Note: may not nest
-- <reg,divide> "/*"($any_char)*"*/"  ;
-- <reg,divide> "/*" (($MultiLineNotAsteriskChar)*| ("*")+ ($MultiLineNotForwardSlashOrAsteriskChar) )* ("*")+ "/"  ;
<reg,divide> "/*" (($MultiLineNotAsteriskChar)*| ("*")+ ($MultiLineNotForwardSlashOrAsteriskChar) )* ("*")+ "/"  { adapt (mkString commentToken) }


-- Identifier    = {ID Head}{ID Tail}*
-- <reg,divide> @IDHead(@IDTail)*  { \loc len str -> keywordOrIdent (take len str) loc }
<reg,divide> @IdentifierStart(@IdentifierPart)*  { \ap@(loc,_,_,str) len -> keywordOrIdent (take len str) (toTokenPosn loc) }

-- StringLiteral = '"' ( {String Chars1} | '\' {Printable} )* '"'
--                | '' ( {String Chars2} | '\' {Printable} )* ''
<reg,divide>  $dq ( $StringChars1 | \\ $printable | @LineContinuation )* $dq
            | $sq ( $StringChars2 | \\ $printable | @LineContinuation )* $sq { adapt (mkString stringToken) }

-- HexIntegerLiteral = '0x' {Hex Digit}+
<reg,divide> ("0x"|"0X") @HexDigit+ { adapt (mkString hexIntegerToken) }

-- OctalLiteral = '0' {Octal Digit}+
<reg,divide> ("0") @OctDigit+ { adapt (mkString octalToken) }

-- RegExp         = '/' ({RegExp Chars} | '\' {Non Terminator})+ '/' ( 'g' | 'i' | 'm' )*
-- <reg> "/" ($RegExpChars | "\" $NonTerminator)+ "/" ("g"|"i"|"m")* { mkString regExToken }

-- Based on the Jint version
<reg> "/" ($RegExpFirstChar | "\" $NonTerminator)  ($RegExpChars | "\" $NonTerminator)* "/" ("g"|"i"|"m")* { adapt (mkString regExToken) }



-- TODO: Work in SignedInteger

-- DecimalLiteral= {Non Zero Digits}+ '.' {Digit}* ('e' | 'E' ) {Non Zero Digits}+ {Digit}*
--              |  {Non Zero Digits}+ '.' {Digit}*
--              | '0' '.' {Digit}+ ('e' | 'E' ) {Non Zero Digits}+ {Digit}*
--              | {Non Zero Digits}+ {Digit}*
--              | '0'
--              | '0' '.' {Digit}+

-- <reg,divide> $non_zero_digit $digit* "." $digit* ("e"|"E") ("+"|"-")? $non_zero_digit+ $digit*
--     | $non_zero_digit $digit* "." $digit*
--     | "0." $digit+  ("e"|"E") ("+"|"-")? $non_zero_digit+ $digit*
--     | $non_zero_digit+ $digit*
--     | "0"
--     | "0." $digit+                    { mkString decimalToken }

<reg,divide> "0"              "." $digit* ("e"|"E") ("+"|"-")? $digit+
    | $non_zero_digit $digit* "." $digit* ("e"|"E") ("+"|"-")? $digit+
    |                "." $digit+          ("e"|"E") ("+"|"-")? $digit+
    |        "0"                          ("e"|"E") ("+"|"-")? $digit+
    | $non_zero_digit $digit*             ("e"|"E") ("+"|"-")? $digit+
-- ++FOO++
    |        "0"              "." $digit*
    | $non_zero_digit $digit* "." $digit*
    |                "." $digit+
    |        "0"
    | $non_zero_digit $digit*         { adapt (mkString decimalToken) }


-- beginning of file
<bof> {
   @eol_pattern                         ;
   -- @eol_pattern                         { endOfLine lexToken }
   -- @eol_pattern                         { endOfLine alexMonadScan }
}

-- / or /= only allowed in state 1
<divide> {
    "/="       { adapt (mkString assignToken)}
    "/"        { adapt (symbolToken DivToken)}
    }

<reg,divide> {
     ";"    { adapt (symbolToken  SemiColonToken)}
     ","    { adapt (symbolToken  CommaToken)}
     "?"    { adapt (symbolToken  HookToken)}
     ":"    { adapt (symbolToken  ColonToken)}
     "||"   { adapt (symbolToken  OrToken)}
     "&&"   { adapt (symbolToken  AndToken)}
     "|"    { adapt (symbolToken  BitwiseOrToken)}
     "^"    { adapt (symbolToken  BitwiseXorToken)}
     "&"    { adapt (symbolToken  BitwiseAndToken)}
     "==="  { adapt (symbolToken  StrictEqToken)}
     "=="   { adapt (symbolToken  EqToken)}
     "*=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|="
            { adapt (mkString assignToken)}
     "="    { adapt (symbolToken  SimpleAssignToken)}
     "!=="  { adapt (symbolToken  StrictNeToken)}
     "!="   { adapt (symbolToken  NeToken)}
     "<<"   { adapt (symbolToken  LshToken)}
     "<="   { adapt (symbolToken  LeToken)}
     "<"    { adapt (symbolToken  LtToken)}
     ">>>"  { adapt (symbolToken  UrshToken)}
     ">>"   { adapt (symbolToken  RshToken)}
     ">="   { adapt (symbolToken  GeToken)}
     ">"    { adapt (symbolToken  GtToken)}
     "++"   { adapt (symbolToken  IncrementToken)}
     "--"   { adapt (symbolToken  DecrementToken)}
     "+"    { adapt (symbolToken  PlusToken)}
     "-"    { adapt (symbolToken  MinusToken)}
     "*"    { adapt (symbolToken  MulToken)}
     "%"    { adapt (symbolToken  ModToken)}
     "!"    { adapt (symbolToken  NotToken)}
     "~"    { adapt (symbolToken  BitwiseNotToken)}
     "."    { adapt (symbolToken  DotToken)}
     "["    { adapt (symbolToken  LeftBracketToken)}
     "]"    { adapt (symbolToken  RightBracketToken)}
     "{"    { adapt (symbolToken  LeftCurlyToken)}
     "}"    { adapt (symbolToken  RightCurlyToken)}
     "("    { adapt (symbolToken  LeftParenToken)}
     ")"    { adapt (symbolToken  RightParenToken)}
     "@*/"  { adapt (symbolToken  CondcommentEndToken)}
}


{

{-
-- The next function select between the two lex input states, as called for in
-- secion 7 of ECMAScript Language Specification, Edition 3, 24 March 2000.

The method is inspired by the lexer in http://jint.codeplex.com/

-}
classifyToken :: Token -> Int
classifyToken aToken =
    case aToken of
        IdentifierToken {}   -> divide
        NullToken {}         -> divide
        TrueToken {}         -> divide
        FalseToken {}        -> divide
        ThisToken {}         -> divide
        OctalToken {}        -> divide
        DecimalToken {}      -> divide
        HexIntegerToken {}   -> divide
        StringToken {}       -> divide
        RightCurlyToken {}   -> divide
        RightParenToken {}   -> divide
        RightBracketToken {} -> divide
        _other               -> reg


lexToken :: Alex Token
lexToken = do
    inp <- alexGetInput
    lt  <- getLastToken
    case lt of
        TailToken {} -> alexEOF
        _other ->
            case alexScan inp (classifyToken lt) of
                AlexEOF        -> do
                    tok <- tailToken
                    setLastToken tok
                    return tok
                AlexError (pos,_,_,_) ->
                    alexError ("lexical error @ line " ++ show (getLineNum(pos)) ++
                                                 " and column " ++ show (getColumnNum(pos)))
                AlexSkip inp' _len -> do
                    alexSetInput inp'
                    lexToken
                AlexToken inp' len action -> do
                    alexSetInput inp'
                    tok <- action inp len
                    setLastToken tok
                    return tok

-- For tesing.
alexTestTokeniser :: String -> Either String [Token]
alexTestTokeniser input =
    runAlex input $ loop []
  where
    loop acc = do
        tok <- lexToken
        case tok of
            EOFToken {} ->
                return $ case acc of
                            [] -> []
                            (TailToken{}:xs) -> reverse xs
                            xs -> reverse xs
            _ -> loop (tok:acc)

-- This is called by the Happy parser.
lexCont :: (Token -> Alex a) -> Alex a
lexCont cont = do
    lexLoop
  where
    lexLoop = do
        tok <- lexToken
        case tok of
            CommentToken {} -> do
                addComment tok
                lexLoop
            WsToken {} -> do
                addComment tok
                ltok <- getLastToken
                case ltok of
                    BreakToken {} -> maybeAutoSemi tok
                    ContinueToken {} -> maybeAutoSemi tok
                    ReturnToken {} -> maybeAutoSemi tok
                    _otherwise -> lexLoop
            _other -> do
                cs <- getComment
                let tok' = tok{ tokenComment=(toCommentAnnotation cs) }
                setComment []
                cont tok'

    -- If the token is a WsToken and it contains a newline, convert it to an
    -- AutoSemiToken and call the continuation, otherwise, just lexLoop.
    maybeAutoSemi ws@(WsToken sp tl cmt) =
        if any (== '\n') tl
            then cont $ AutoSemiToken sp tl cmt
            else lexLoop
    maybeAutoSemi _ = lexLoop


toCommentAnnotation :: [Token] -> [CommentAnnotation]
toCommentAnnotation [] = [NoComment]

toCommentAnnotation xs =
    reverse $ map go xs
  where
    go tok@(CommentToken {}) = (CommentA (tokenSpan tok) (tokenLiteral tok))
    go tok@(WsToken      {}) = (WhiteSpace (tokenSpan tok) (tokenLiteral tok))
    go _                     = error "toCommentAnnotation"

-- ---------------------------------------------------------------------

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _offset lineNum _colNum) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _offset _lineNum colNum) = colNum

-- ---------------------------------------------------------------------

getLastToken :: Alex Token
getLastToken = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, previousToken ust)

setLastToken :: Token -> Alex ()
setLastToken (WsToken {}) = Alex $ \s -> Right (s, ())
setLastToken tok          = Alex $ \s -> Right (s{alex_ust=(alex_ust s){previousToken=tok}}, ())

getComment :: Alex [Token]
getComment = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, comment ust)


addComment :: Token -> Alex ()
addComment c = Alex $ \s ->  Right (s{alex_ust=(alex_ust s){comment=c:(  comment (alex_ust s)  )}}, ())


setComment :: [Token] -> Alex ()
setComment cs = Alex $ \s -> Right (s{alex_ust=(alex_ust s){comment=cs }}, ())

alexEOF :: Alex Token
alexEOF = do return (EOFToken tokenPosnEmpty [])

tailToken :: Alex Token
tailToken = do return (TailToken tokenPosnEmpty [])

adapt :: (TokenPosn -> Int -> String -> Alex Token) -> AlexInput -> Int -> Alex Token
adapt f ((AlexPn offset line col),_,_,inp) len = f (TokenPn offset line col) len inp


toTokenPosn :: AlexPosn -> TokenPosn
toTokenPosn (AlexPn offset line col) = (TokenPn offset line col)

-- ---------------------------------------------------------------------

-- a keyword or an identifier (the syntax overlaps)
keywordOrIdent :: String -> TokenPosn -> Alex Token
keywordOrIdent str location
   = return $ case Map.lookup str keywords of
         Just symbol -> symbol location str []
         Nothing -> IdentifierToken location str []

-- mapping from strings to keywords
-- keywords :: Map.Map String (TokenPosn -> String -> Token)
keywords :: Map.Map String (TokenPosn -> String -> [CommentAnnotation] -> Token)
keywords = Map.fromList keywordNames

-- keywordNames :: [(String, TokenPosn -> String -> Token)]
keywordNames :: [(String, TokenPosn -> String -> [CommentAnnotation] -> Token)]
keywordNames =
    [ ( "break", BreakToken )
    , ( "case", CaseToken )
    , ( "catch", CatchToken )

    , ( "const", ConstToken ) -- not a keyword, nominally a future reserved word, but actually in use

    , ( "continue", ContinueToken )
    , ( "debugger", DebuggerToken )
    , ( "default", DefaultToken )
    , ( "delete", DeleteToken )
    , ( "do", DoToken )
    , ( "else", ElseToken )

    , ( "enum", EnumToken )  -- not a keyword,  nominally a future reserved word, but actually in use

    , ( "false", FalseToken ) -- boolean literal

    , ( "finally", FinallyToken )
    , ( "for", ForToken )
    , ( "function", FunctionToken )
    , ( "if", IfToken )
    , ( "in", InToken )
    , ( "instanceof", InstanceofToken )
    , ( "new", NewToken )

    , ( "null", NullToken ) -- null literal

    , ( "return", ReturnToken )
    , ( "switch", SwitchToken )
    , ( "this", ThisToken )
    , ( "throw", ThrowToken )
    , ( "true", TrueToken )
    , ( "try", TryToken )
    , ( "typeof", TypeofToken )
    , ( "var", VarToken )
    , ( "void", VoidToken )
    , ( "while", WhileToken )
    , ( "with", WithToken )
    -- TODO: no idea if these are reserved or not, but they are needed
    --       handled in parser, in the Identifier rule
    , ( "get", GetToken )
    , ("set", SetToken )
    {- Come from Table 6 of ECMASCRIPT 5.1, Attributes of a Named Accessor Property
       Also include

         Enumerable
         Configurable

      Table 7 includes

         Value
     -}

    -- Future Reserved Words
    , ( "class",  FutureToken )
    -- ("code",   FutureToken ) **** not any more
    -- ("const",  FutureToken ) **** an actual token, used in productions
    -- enum                    **** an actual token, used in productions
    , ( "export",      FutureToken )
    , ( "extends",     FutureToken )

    , ( "import",      FutureToken )
    , ( "super",       FutureToken )


    -- Strict mode FutureReservedWords
    , ( "implements",  FutureToken )
    , ( "interface",   FutureToken )
    , ( "let",         FutureToken )
    -- ("mode",        FutureToken )  **** not any more
    -- ("of",          FutureToken )  **** not any more
    -- ("one",         FutureToken )  **** not any more
    -- ("or",          FutureToken )  **** not any more

    , ( "package",     FutureToken )
    , ( "private",     FutureToken )
    , ( "protected",   FutureToken )
    , ( "public",      FutureToken )
    , ( "static",      FutureToken )
    -- ("strict",      FutureToken )  *** not any more
    , ( "yield",       FutureToken)
   ]
}


-- -- Edition 5.1 of ECMASCRIPT

-- 7.6.1.1 Keywords

-- The following tokens are ECMAScript keywords and may not be used as Identifiers in ECMAScript programs.

-- Syntax
-- Keyword :: one of
--   break
--   case
--   catch
--   continue
--   debugger
--   default
--   delete
--   do
--   else
--   finally
--   for
--   function
--   if
--   in
--   instanceof
--   new
--   return
--   switch
--   this
--   throw
--   try
--   typeof
--   var
--   void
--   while
--   with

-- 7.6.1.2 Future Reserved Words

-- The following words are used as keywords in proposed extensions and
-- are therefore reserved to allow for the possibility of future adoption
-- of those extensions.

-- Syntax
-- FutureReservedWord :: one of
--   class
--   const
--   enum
--   export
--   extends
--   import
--   super

-- The following tokens are also considered to be FutureReservedWords
-- when they occur within strict mode code (see 10.1.1). The occurrence
-- of any of these tokens within strict mode code in any context where
-- the occurrence of a FutureReservedWord would produce an error must
-- also produce an equivalent error:

--  implements
--  interface
--  let
--  package
--  private
--  protected
--  public
--  static
--  yield




-- Set emacs mode
-- Local Variables:
-- mode:haskell
-- End:
