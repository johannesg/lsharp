#load "Core.fs"
#load "Reader.fs"
#load "Eval.fs"
open LSharp.Core.Reader
open LSharp.Core.Eval

let eval str = Result.bind evalAll (readAll str)
    
eval "( add 1 2324) "


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
