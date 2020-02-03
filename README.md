## From Scala to Haskell

### Defining functions

> Scala

```scala
def double(x: Int): Int = 2 * x
```

> Haskell

```haskell
double :: Int -> Int
double x = 2 * x
```

> Haskell REPL (GHCi) - option 1

```haskell
double :: Int -> Int; double x = 2 *x
```

Many tutorials use `let double ...`. There's [no difference](https://www.reddit.com/r/haskell/comments/8ahozm/reason_for_definitions_using_let_in_ghci/) between those two notations in GHCi so it's up to you to decide
.
> Haskell REPL (GHCi) - option 2

```haskell
double :: Int -> Int
double x = 2 * x
```

Note the `:{` and `:}` in the first and last line. Besides of that it's the same as one would define the function in a source file.

### `???`

> Scala

```scala
def compute: Int = ???
```

> Haskell

```haskell
compute :: Int
compute = undefined
```

### Case classes (product types)

#### Defining case classes (product types)

> Scala

```scala
case class Point(x: Int, y: Int)
```

> Haskell

```haskell
data Point = Point { x :: Int
                   , y :: Int
                   }
```

If you need your datatype to be visible outside of the file, in which it's defined, then remember to export it:

```haskell
module YourModule ( Point(Point) )
```

#### Defining product types with some typeclasses derived

Code from above produces a perfectly fine product type but if you need to have some functionalities implemented for it you can derive them as in:

> Haskell

```haskell
{-# LANGUAGE DeriveGeneric #-} -- this needs to be at the top of a file

data Point = Point { x :: Int
                        , y :: Int
                   } deriving (Eq, Generic Show)
```

* `Eq` - with instance of that typeclass `(Point 1 1) == (Point 123 321)` will compile.
* `Generic` - you can think of it as kind of `shapeless.LabelledGeneric` - something that is a base for other typeclass derivation mechanisms. For example `aeson` (JSON library) can generate codecs for your data type providing it has an instance of `Generic`. _I know it sounds vague but my current understanding is vague too._
* `Show` - with instance of that typeclass you can `show (Point 1 1)` which returns a string. We can compare it to `toString` mechanism known from JVM world.


You don't need `{-# LANGUAGE DeriveGeneric #-}` if you don't want to derive `Generic` - you would still be able to derive e.g. `Show`.

#### Instantiating case classes

> Scala

```scala
Point(3, 15)
```

> Haskell

```haskell
Point 3 15
```

#### Accessing fields

> Scala

```scala
> Point(3, 15).x
3
```

> Haskell

```haskell
> x (Point 3 5)
3
```

It would work providing you exported `x` and imported it to scope where you use it. You can export field accessors with:

```haskell
module YourModule ( Point(Point, x, y) ) where ...
```


#### `.copy` on case classes

> Scala

```scala
> Point(3, 15).copy(y = 100)
Point(3, 100)
```

> Haskell

```haskell
> (Point 3 15) { y = 100 }
Point {x = 3, y = 100}
```

### Sealed traits (sum types)

#### Defining sealed trais hierarchy (sum types)

> Scala

```scala
sealed trait Colour

case object Black extends Colour
case object Grey  extends Colour
case object Grey  extends Colour
```

> Haskell

```haskell
data Colour = Black
            | Grey
            | White
```

In order to be able to use data constructors you need to export them with:

```
module YourModule ( Colour(Black, Grey, White) ) where ...
```

### ADTs (product types and sum types)

> Scala

```scala
sealed trait ClientError

case class ParsingError(input: String, msg: String)
case class HostUnavailable(host: String)
case object OtherError
```

> Haskell

```haskell
data ClientError = ParsingError { input :: String, msg :: String }
				 | HostUnavailable { host :: String }
				 | OtherError
```

#### Pattern matching

TBD


### Function composition

### `andThen`

> Scala

Given:

```scala
val toInt: String => Int = Integer.parseInt
val double: Int => Int = x => 2 * x
```

You can compose them as:

```scala
> (toInt andThen double)("5")
10
```

> Haskell

Given:

```haskell
toInt  :: String -> Int; toInt  x = read x
double :: Int    -> Int; double x = 2 * x
```

```haskell
> import Control.Arrow
> (toInt >>> double) "5"
10
```

### `compose`

> Scala

```scala
> (double compose toInt)("5")
10
```

> Haskell

```haskell
> (double . toInt) "5"
10
```

### Lists

#### Map through list

> Scala

```scala
(1 to 10).map(x => x * x)
```

> Haskell

```haskell
map (\x -> x * x) [1 .. 10]
```

#### Flatmap through list

> Scala

```scala
> List(1,2,3).flatMap(x => List(x, x))
List(1, 1, 2, 2, 3, 3)
```

> Haskell

```haskell
> [1,2,3] >>= \x -> [x,x]
[1,1,2,2,3,3]
```

#### TODO:

* higher order functions
* debugging (tracing, println)
* for comprehension
* tuples
* Pattern matching
* either pattern matching
* map over Right
* and others
