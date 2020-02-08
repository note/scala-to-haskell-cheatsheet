let F = ./common/functions.dhall
let Toplevel  = ./common/topLevel.dhall
let T = ./common/types.dhall

let hask = T.mkExplained
let simpleHask = F.simpleHask

let caseClassScala = "case class Point(x: Int, y: Int)"
let caseClassHaskell = ''
data Point = Point { x :: Int
                   , y :: Int
                   }
''

let exportModule = F.quoteCode "module YourModule ( Point(Point) )"
let caseClassHask = hask "Haskell" caseClassHaskell (Some ''
If you need your datatype to be visible outside of the file, in which it's defined, then remember to export it:
${exportModule}
'')

let ccDerivedHaskell = ''
 -- this needs to be at the top of a file
{-# LANGUAGE DeriveGeneric #-}

data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Eq, Generic Show)
''

let ccDerivedHask = hask "Haskell" ccDerivedHaskell (Some ''
<ul>
    <li>${F.code "Eq"} - with instance of that typeclass ${F.code "(Point 1 1) == (Point 123 321)"} will compile.</li>
    <li>${F.code "Generic"} - you can think of it as kind of ${F.code "shapeless.LabelledGeneric"} - something that is a base for other typeclass derivation mechanisms. For example aeson (JSON library) can generate codecs for your data type providing it has an instance of ${F.code "Generic"}. I know it sounds vague but my current understanding is vague too.</li>
    <li>${F.code "Show"} - with instance of that typeclass you can show ${F.code "(Point 1 1)"} which returns a string. We can compare it to ${F.code "toString"} mechanism known from JVM world.</li>
</ul>
You don't need ${F.code "{-# LANGUAGE DeriveGeneric #-}"} if you don't want to derive ${F.code "Generic"} - you would still be able to derive e.g. ${F.code "Show"}.
'')

let instantiateCCScala = "Point(3, 15)"
let instantiateCCHaskell = "Point 3 15"

let accessingFieldsScala = "Point(3, 15).x"
let accessingFieldsHaskellCode = "x (Point 3 5)"


let accessingFieldsHaskell = 
    let quotedCode = F.quoteCode "module YourModule ( Point(Point, x, y) ) where ..."
    in hask "Haskell" accessingFieldsHaskellCode (Some ''
It will work only if you exported `x` and imported it to scope where you use it. You can export field accessors with:
${quotedCode}
'')



let copyScala = "Point(3, 15).copy(y = 100)"
let copyHaskell = "(Point 3 15) { y = 100 }"

let sumTypeScala = ''
sealed trait Colour

case object Black extends Colour
case object Grey  extends Colour
case object Grey  extends Colour
''

let exportSumType = F.quoteCode "module YourModule ( Colour(Black, Grey, White) ) where ..."
let sumTypeHaskell = ''
data Colour = Black
            | Grey
            | White
''
let sumTypeHask = hask "Haskell" sumTypeHaskell (Some ''
In order to be able to use data constructors you need to export them with:
${exportSumType}
'')

let adtScala = ''
sealed trait ClientError

case class ParsingError(input: String, msg: String)
case class HostUnavailable(host: String)
case object OtherError
''

let adtHaskell = ''
data ClientError = ParsingError { input :: String, msg :: String }
                 | HostUnavailable { host :: String }
                 | OtherError
''

-- TODO: use unit type so we don't need to replicate menu in a few places
let menu = [
    T.mkMenuItem "Basics" "index.html" False,
    T.mkMenuItem "ADTs" "adts.html" True,
    T.mkMenuItem "Lists" "lists.html" False,
    T.mkMenuItem "Option" "option.html" False,
    T.mkMenuItem "for comprehension" "for-comprehension.html" False
]

in Toplevel.topLevel "Algebraic Data Types" menu [
	F.mkComparison "Defining case classes (product types)" caseClassScala [caseClassHask],
	F.mkComparison "Defining product types with some typeclasses derived" caseClassScala [ccDerivedHask],
	F.mkSimpleComparison "Instantiating case classes" instantiateCCScala instantiateCCHaskell,
    F.mkComparison "Accessing fields" accessingFieldsScala [accessingFieldsHaskell],
    F.mkSimpleComparison ".copy on case classes" copyScala copyHaskell,
    F.mkComparison "Defining sealed trais hierarchy (sum types)" sumTypeScala [sumTypeHask],
    F.mkSimpleComparison "ADTs (product types and sum types)" adtScala adtHaskell
]