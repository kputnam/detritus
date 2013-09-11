import Data.Monoid ((<>))

obfuscate :: String -> String
obfuscate x = let (Just t) = lookup x table in t
  where
    table = [("[]",                                    "[]")
            ,("{}",                                    "{}")
            ,("true",                                  "!+[]+[]")
            ,("false",                                 "![]+[]")
            ,("undefined",                             "[][[]]+[]")
            ,("NaN",                                   "+{}")
            ,("Infinity",                              obfuscate "1e1000")
            ,("1e+100",                                "+" <> obfuscate "1e100")
            ,("[object Object]",                       "({})+[]")
            ,("0",                                     "+[]")
            ,("1",                                     "+!![]")
            ,("2",                                     "+!+!![]")
            ,("3",                                     "+!+!![]+!![]")
            ,("4",                                     "+!+!![]+!![]+!![]")
            ,("5",                                     "+!+!![]+!![]+!![]+!![]")
            ,("6",                                     "+!+!![]+!![]+!![]+!![]+!![]")
            ,("7",                                     "+!+!![]+!![]+!![]+!![]+!![]+!![]")
            ,("8",                                     "+!+!![]+!![]+!![]+!![]+!![]+!![]+!![]")
            ,("9",                                     "+!+!![]+!![]+!![]+!![]+!![]+!![]+!![]+!![]")
            ,("function Array() { [native code] }",    "[]"   <> "[" <> obfuscate "constructor" <> "]")
            ,("function Number() { [native code] }",   "0"    <> "[" <> obfuscate "constructor" <> "]")
            ,("function Object() { [native code] }",   "({})" <> "[" <> obfuscate "constructor" <> "]")
            ,("function Function() { [native code] }", "[]"   <> "[" <> obfuscate "constructor" <> "][" <> obfuscate "constructor" <> "]")
            ,("function String() { [native code] }",   "(" <> obfuscate "N" <> ")" <> "[" <> obfuscate "constructor" <> "]")
            ,("function Boolean() { [native code] }",  "(![])" <> "[" <> obfuscate "constructor" <> "]")
            ]
-- call
-- concat
-- constructor
-- join
-- slice
-- sort 
