#I "../../build"
// #I "bin/Debug/netstandard2.0"
#r "System.Collections.Immutable.dll"
#r "lsharp.lib"

System.Reflection.Assembly.Load("lsharp.lib")
System.Type.GetType("LSharp.Lang.Math, lsharp.lib")

#load "Core.fs"
#load "Reader.fs"
#load "Symbols.fs"
#load "SpecialForms.fs"
#load "Macro.fs"
#load "Eval.fs"

open LSharp.Reader
open LSharp.Eval

let eval str = readAll str |> Result.bind evalAll 
    
eval "( add 1 (add 3 5 )) "
read "( add 1 2324) "

eval "true"
eval "System.Math"
eval "LSharp.Lang.Math"
eval "."
eval "def"
eval "(deftype \"LSharp.Lang.Math, lsharp.lib\" )"
eval "(deftype \"System.Type\" )"

eval "(. System.Type GetType \"LSharp.Lang.Math\")"
eval "(. LSharp.Lang.Math add 2 3)"
eval "(. System.Math Max 3 4)"
eval "(def x (. LSharp.Lang.Math add 2 3))"
eval "(def x 23)"
eval "[:a :a '(:a) x]"
eval "(defmacro m [a b c] a)"

eval "x"
eval "m"
eval "(m 23)"

read "/a"
eval "'."


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
read "(x)"

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
read "{ :a, { :c :d } }"

read " :a  "
