let F = ./common/functions.dhall
let Toplevel  = ./common/topLevel.dhall
let T = ./common/types.dhall

let hask = T.mkExplained
let simpleHask = F.simpleHask

-- TODO: use unit type so we don't need to replicate menu in a few places
let menu = [
    T.mkMenuItem "Basics" "index.html" False,
    T.mkMenuItem "ADTs" "adts.html" False,
    T.mkMenuItem "Lists" "lists.html" True,
    T.mkMenuItem "Option" "option.html" False
]

let createListScala = ''
List(1, 2, 3)
''

let createListHaskell = ''
[1, 2, 3]
''

let rangeScala = ''
(1 to 10).toList
''

let rangeHaskell = ''
[1 .. 10]
''

let mapListScala = ''
(1 to 10).toList.map(x => x * x)
''

let mapListHaskell0 = ''
map (\x -> x * x) [1 .. 10]
''

let mapListHaskellQuoted0 = F.quoteExplanation "map :: (a -> b) -> [a] -> [b]"

let mapListHaskell1 = ''
fmap (\x -> x * x) [1 .. 10]
''
let mapListHaskellQuoted1 = F.quoteExplanation "fmap :: Functor f => (a -> b) -> f a -> f b"

let mapListHaskell2 = ''
(\x -> x * x) <$> [1 .. 10]
''

let mapListHaskell = [
                        hask "Haskell" mapListHaskell0 (Some "${F.code "map"} is defined only for list: ${mapListHaskellQuoted0}"),
                        hask "Haskell" mapListHaskell1 (Some "${F.code "fmap"} is defined in terms of Functor: ${mapListHaskellQuoted1}"),
                        hask "Haskell" mapListHaskell2 (Some "${F.code "<$>"} is an infix operator for ${F.code "fmap"}")
					 ]

let flatMapListScala = ''
List(1,2,3).flatMap(x => List(x, x))
// res: List(1, 1, 2, 2, 3, 3)
''

let flatMapListHaskell = ''
[1,2,3] >>= \x -> [x,x]
-- res: [1,1,2,2,3,3]
''

in Toplevel.topLevel "Lists" menu [
	F.mkSimpleComparison "Creating a list" createListScala createListHaskell,
    F.mkSimpleComparison "Creating a range" rangeScala rangeHaskell,
    F.mkComparison "Map through a list" mapListScala mapListHaskell,
    F.mkSimpleComparison "Flatmap through a list" flatMapListScala flatMapListHaskell
]
