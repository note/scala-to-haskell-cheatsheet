## From Scala to Haskell

### Defining functions

> Scala

```
def double(x: Int): Int = 2 * x
```

> Haskell

```
double :: Int -> Int
double x = 2 * x
```

> Haskell REPL (GHCi) - the first way

```
double :: Int -> Int; double x = 2 *x
```

Many tutorials use `let double ...` and it can be used. There's [no difference](https://www.reddit.com/r/haskell/comments/8ahozm/reason_for_definitions_using_let_in_ghci/) between those two notations in GHCi.

> Haskell REPL (GHCi) - alternative

```
double :: Int -> Int
double x = 2 * x
```

Note the `:{` and `:}` in the first and last line. Besides of that it's the same as one would define the function in a source file.

### `???`

> Scala

```
def compute: Int = ???
```

> Haskell

```
compute :: Int
compute = undefined
```

### Case classes

#### Defining product types (case classes)

> Scala

```
case class Point(x: Int, y: Int)
```

> Haskell

```
data Point = Point { x :: Int
                   , y :: Int
                   }
```

If you need your datatype to be visible outside of the file, in which it's defined, then remember to export it:

```
module YourModule ( Point(Point) )
```

#### Defining product types with some typeclasses derived

Code from above produces a perfectly fine product type but if you need to have some functionalities implemented for it you can derive them as in:

> Haskell

```
{-# LANGUAGE DeriveGeneric #-} -- this needs to be at the top of a file

data Point = Point { x :: Int
                        , y :: Int
                   } deriving (Eq, Generic Show)
```

* `Eq` - with instance of that typeclass `(Point 1 1) == (Point 123 321)` will compile.
* `Generic` - you can think of it as kind of `shapeless.LabelledGeneric` - something that is a base for other typeclass derivation mechanisms. For example `aeson` (JSON library) can generate codecs for your data type providing it has an instance of `Generic`. _I know it sounds vague but my current understanding is vague too._
* `Show` - with instance of that typeclass you can `show (Point 1 1)` which returns a string. You can compare it to `toString` mechanism known from JVM world.


You don't need `{-# LANGUAGE DeriveGeneric #-}` if you don't want to derive `Generic`, you would still be able to derive e.g. `Show`.

#### Instantiating case classes

> Scala

```
Point(3, 15)
```

> Haskell

```
Point 3 15
```

#### Accessing fields

> Scala

```
> Point(3, 15).x
3
```

> Haskell

```
> x (Point 3 5)
3
```

It would work providing you exported `x` and imported it to scope where you use it. You can export field accessors with:

```
module YourModule ( Point(Point, x, y) )
```


#### `.copy` on case classes

> Scala

```
> Point(3, 15).copy(y = 100)
Point(3, 100)
```

> Haskell

```
> (Point 3 15) { y = 100 }
Point {x = 3, y = 100}
```

#### Pattern matching

TBD


### Function composition

### `andThen`

> Scala

Given:

```
val toInt: String => Int = Integer.parseInt
val double: Int => Int = x => 2 * x
```

You can compose them as:

```
> (toInt andThen double)("5")
10
```

> Haskell

Given:

```
toInt  :: String -> Int; toInt  x = read x
double :: Int    -> Int; double x = 2 * x
```

```
> import Control.Arrow
> (toInt >>> double) "5"
10
```

### `compose`

> Scala

```
> (double compose toInt)("5")
10
```

> Haskell

```
> (double . toInt) "5"
10
```

### Lists

#### Map through list

> Scala

```
(1 to 10).map(x => x * x)
```

> Haskell

```
map (\x -> x * x) [1 .. 10]
```

#### Flatmap through list

> Scala

```
> List(1,2,3).flatMap(x => List(x, x))
List(1, 1, 2, 2, 3, 3)
```

> Haskell

```
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
