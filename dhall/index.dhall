let F = ./common/functions.dhall
let Toplevel  = ./common/topLevel.dhall
let T = ./common/types.dhall

let hask = T.mkExplained
let simpleHask = F.simpleHask

let funDef0 = ''
double :: Int -> Int
double x = 2 * x''

let funDef1 = "double :: Int -> Int; double x = 2 *x"

let funDef2 = ''
:{
double :: Int -> Int
double x = 2 * x
:}
''

let funDef2Explanation = ''
Note the :{ and :} in the first and last line. Besides of that it's the same as one would define the function in a source file.
''

let haskellFunctionsDefs = [simpleHask funDef0,
							hask "Haskell (GHCi)" funDef1 (Some "Many tutorials use let double .... There's no difference between those two notations in GHCi so it's up to you to decide ."),
							hask "Haskell (GHCi) - alternative" funDef2 (Some funDef2Explanation)
						   ]

let scalaFunctionDef = "def double(x: Int): Int = 2 * x"

let questionMarksScala = "def compute: Int = ???"
let questionMarksHaskell = ''
compute :: Int
compute = undefined
''

-- TODO: use unit type so we don't need to replicate menu in a few places
let menu = [
    T.mkMenuItem "Basics" "index.html" True,
    T.mkMenuItem "ADTs" "adts.html" False
]

in Toplevel.topLevel "Basics" menu [
	F.mkComparison "Defining functions" scalaFunctionDef haskellFunctionsDefs,
	F.mkSimpleComparison "???" questionMarksScala questionMarksHaskell
]