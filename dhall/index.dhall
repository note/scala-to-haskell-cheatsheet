let F = ./common/functions.dhall
let Toplevel  = ./common/topLevel.dhall
let T = ./common/types.dhall

let hask = T.mkExplained
let simpleHask = F.simpleHask

let funDef0 = ''
triple :: Int -> Int
triple x = 3 * x''

let funDef1 = "triple :: Int -> Int; triple x = 3 * x"

let funDef2 = ''
:{
triple :: Int -> Int
triple x = 3 * x
:}
''

let funDef2Explanation = ''
Note the ${F.code ":{"} and ${F.code ":}"} in the first and last line. Besides of that it's the same as one would define the function in a source file.
''

let haskellFunctionsDefs = [simpleHask funDef0,
							hask "Haskell (GHCi)" funDef1 (Some ''
							Many tutorials use ${F.code "let triple :: ..."}. There's <a href="https://stackoverflow.com/a/42988998/429311">no difference</a> between those two notations in GHCi so it's up to you to decide .
							''),
							hask "Haskell (GHCi) - alternative" funDef2 (Some funDef2Explanation)
						   ]

let scalaFunctionDef = "def triple(x: Int): Int = 3 * x"

let questionMarksScala = "def compute: Int = ???"
let questionMarksHaskell = ''
compute :: Int
compute = undefined
''

let andThenScala = ''
// Given:
val toInt: String => Int = Integer.parseInt
val triple: Int => Int = x => 3 * x

// You can compose them:
(toInt andThen triple)("5")
// res: 15
''

let andThenHaskell = ''
-- Given:
toInt  :: String -> Int; toInt  x = read x
triple :: Int    -> Int; triple x = 3 * x

-- You can compose them:
import Control.Arrow
(toInt >>> triple) "5"
-- res: 15
''

let composeScala = ''
> (triple compose toInt)("5")
// res: 15
''

let composeHaskell = ''
> (triple . toInt) "5"
-- res: 15
''

in Toplevel.topLevel "Basics" T.SubPage.Basics [
	F.mkComparison "Defining functions" scalaFunctionDef haskellFunctionsDefs,
	F.mkSimpleComparison "???" questionMarksScala questionMarksHaskell,
	F.mkSimpleComparison "Function composition (andThen style)" andThenScala andThenHaskell,
	F.mkSimpleComparison "Function composition (compose style)" composeScala composeHaskell
]