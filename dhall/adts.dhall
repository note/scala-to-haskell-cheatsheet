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
{-# LANGUAGE DeriveGeneric #-} -- this needs to be at the top of a file

data Point = Point { x :: Int
                        , y :: Int
                   } deriving (Eq, Generic Show)
''

let ccDerivedHask = hask "Haskell" ccDerivedHaskell (Some ''
<ul>
    <li>Eq - with instance of that typeclass (Point 1 1) == (Point 123 321) will compile.</li>
    <li>Generic - you can think of it as kind of shapeless.LabelledGeneric - something that is a base for other typeclass derivation mechanisms. For example aeson (JSON library) can generate codecs for your data type providing it has an instance of Generic. I know it sounds vague but my current understanding is vague too.</li>
    <li>Show - with instance of that typeclass you can show (Point 1 1) which returns a string. We can compare it to toString mechanism known from JVM world.</li>
</ul>
You don't need {-# LANGUAGE DeriveGeneric #-} if you don't want to derive Generic - you would still be able to derive e.g. Show.
'')

let instantiateCCScala = "Point(3, 15)"
let instantiateCCHaskell = "Point 3 15"

let accessingFieldsScala = "Point(3, 15).x"
let accessingFieldsHaskell = "x (Point 3 5)"

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
    T.mkMenuItem "ADTs" "adts.html" True
]

in Toplevel.topLevel "Algebraic Data Types" menu [
	F.mkComparison "Defining case classes (product types)" caseClassScala [caseClassHask],
	F.mkComparison "Defining product types with some typeclasses derived" caseClassScala [ccDerivedHask],
	F.mkSimpleComparison "Instantiating case classes" instantiateCCScala instantiateCCHaskell,
    F.mkSimpleComparison "Accessing fields" accessingFieldsScala accessingFieldsHaskell,
    F.mkSimpleComparison ".copy on case classes" copyScala copyHaskell,
    F.mkComparison "Defining sealed trais hierarchy (sum types)" sumTypeScala [sumTypeHask],
    F.mkSimpleComparison "ADTs (product types and sum types)" 
]