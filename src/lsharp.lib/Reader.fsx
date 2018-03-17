#I "../../build"
#r "System.Collections.Immutable.dll"
#r "lsharp.lib.dll"

#load "Core.fs"
#load "Reader.fs"
#load "Symbols.fs"
#load "SpecialForms.fs"
#load "Eval.fs"

open LSharp.Reader
open LSharp.Eval

let eval str = readAll str |> Result.bind evalAll 
    
eval "( add 1 (add 3 5 )) "
read "( add 1 2324) "

eval "true"
eval "System.Math"

eval "(. LSharp.Lang.Math add 2 3)"
eval "(. System.Math Max 3 4)"

read "/a"


readAll "++ -"
read "++ -"

read "\" asdf b sadf f \" -"

read "_+a+sdf"

read "false"
read "'false"

read "++"
read "-1234"
read "1234"
read " 1234 asdf"

read "01234"
read "0x1234"
read "true false "
read "+1e2"
read "1"

eval ":apa"

eval "( :a :b)"
read "'( :a :b )"
read "(:a :b)"
read "(:a ) "
read "( :a (:b :c))"
read "(:a )"
read "( :a)"
read "()"
read "( )"

eval "[:a :a]"
read "( :a [:b :c])"
read "[:a ]"
read "[ :a]"
read "[]"
read "[ ]"

read " #{ :a :b }"

read "{ :a :b }"
read "{ :a,:b }"
read "{ :a ,:b }"
read "{ :a, :b :c }"

read " :a  "
