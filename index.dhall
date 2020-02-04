let Functions = ./functions.dhall
let Toplevel  = ./toplevel.dhall

-- in Functions.mkComparison "abc" "def"

let hask = Functions.mkExplained
let simpleHask = \(code: Text) -> Functions.mkExplained "Haskell" code (None Text)

let funDef0 = ''
double :: Int -> Int
double x = 2 * x''

let funDef1 = "double :: Int -> Int; double x = 2 *x"

let funDef2 = ''
double :: Int -> Int
double x = 2 * x
''

let funDef2Explanation = ''
Note the :{ and :} in the first and last line. Besides of that it's the same as one would define the function in a source file.
''

let abc = 5
--''
--Note the :{ and :} in the first and last line. Besides of that it's the same as one would define the function in a source file.
--''

let haskellFunctionsDefs = [simpleHask funDef0,
							hask "Haskell (GHCi)" funDef1 (Some "Many tutorials use let double .... There's no difference between those two notations in GHCi so it's up to you to decide ."),
							--hask "Haskell (GHCi)" funDef1 (None Text),
							hask "Haskell (GHCi) - alternative" funDef2 (Some funDef2Explanation)
							--hask "Haskell (GHCi) - alternative" funDef2 (None Text)
						   ]

let scalaFunctionDef = "def double(x: Int): Int = 2 * x"

let explainedComparison = (Functions.mkExplainedComparison "Defining functions" scalaFunctionDef haskellFunctionsDefs)

in Toplevel.topLevel "Basics" [Functions.renderExplainedComparison explainedComparison]