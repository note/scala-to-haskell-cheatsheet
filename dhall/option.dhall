let F = ./common/functions.dhall
let Toplevel  = ./common/topLevel.dhall
let T = ./common/types.dhall

let hask = T.mkExplained
let simpleHask = F.simpleHask

-- TODO: use unit type so we don't need to replicate menu in a few places
let menu = [
    T.mkMenuItem "Basics" "index.html" False,
    T.mkMenuItem "ADTs" "adts.html" False,
	T.mkMenuItem "Lists" "lists.html" False,
    T.mkMenuItem "Option" "option.html" True,
    T.mkMenuItem "for comprehension" "for-comprehension.html" False
]

let scalaNonEmpty = ''
val a: Option[Int] = Some(5)
''

let haskellNonEmpty = ''
a :: Maybe Int
a = Just 5
''

let scalaEmpty = ''
val a: Option[Int] = None
''

let haskellEmpty = ''
a :: Maybe Int
a = Nothing
''

let scalaFold = ''
Some(5).fold(0)(x => x * x)
''

let haskellFold = ''
maybe 0 (\x -> x * x) (Just 5)
''

let scalaMap = ''
Some(5).map(x => x * x)
''

let haskellMap = ''
(\x -> x * x) <$> (Just 5)
''

let haskMap = [
    hask "Haskell" haskellMap (Some ''
${F.code "<$>"} is not ${F.code "Maybe"}-specific: ${F.code "<$>"} is an infix operator for ${F.code "fmap"}. ${F.code "fmap"} is defined in terms of Functor and Maybe has an instance of Functor
'')
]

in Toplevel.topLevel "Option" menu [
    F.mkSimpleComparison "Create a non-empty Option" scalaNonEmpty haskellNonEmpty,
    F.mkSimpleComparison "Create an empty Option" scalaEmpty haskellEmpty,
    F.mkSimpleComparison "Fold over an Option" scalaFold haskellFold,
    F.mkComparison "Map over an Option" scalaMap haskMap
]