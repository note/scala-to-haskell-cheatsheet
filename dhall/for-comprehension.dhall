let F = ./common/functions.dhall
let Toplevel  = ./common/topLevel.dhall
let T = ./common/types.dhall

let hask = T.mkExplained
let simpleHask = F.simpleHask

let scalaFor = ''
// given:
// maybeInt: Option[Int] = ???

res: Option[(Int, Int, Int)] = for {
    a <- maybeInt
    b <- maybeInt
    c <- maybeInt
} yield (a, b, c)
''

let haskellForCode = ''
-- given:
-- maybeInt :: Maybe Int

res :: Maybe (Int, Int, Int)
res = do a <- maybeInt
         b <- maybeInt
         c <- maybeInt
         return (a, b, c)
''

let haskellFor = [hask "Haskell" haskellForCode (Some ''
${F.code "return"} is not a keyword - it's just a function of type ${F.code "Monad m => a -> m a"} so you can think of it as cats ${F.code "pure"}.
<br /><br />
<b>This form of do notation is whitespace-sensitive.</b> Subsequent terms (${F.code "a"}, ${F.code "b"}, ${F.code "c"} in above example) must be aligned horizontally.
'')]

-- TODO: use unit type so we don't need to replicate menu in a few places
let menu = [
    T.mkMenuItem "Basics" "index.html" False,
    T.mkMenuItem "ADTs" "adts.html" False,
	T.mkMenuItem "Lists" "lists.html" False,
	T.mkMenuItem "Option" "option.html" False,
    T.mkMenuItem "for comprehension" "for-comprehension.html" True
]

in Toplevel.topLevel "for comprehension" menu [
	F.mkComparison "" scalaFor haskellFor
]