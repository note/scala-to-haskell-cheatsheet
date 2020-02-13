let F = ./common/functions.dhall

let Toplevel = ./common/topLevel.dhall

let T = ./common/types.dhall

let hask = T.mkExplained

let simpleHask = F.simpleHask

let scalaFor =
      ''
      // given:
      // maybeInt: Option[Int] = ???

      res: Option[(Int, Int, Int)] = for {
          a <- maybeInt
          b <- maybeInt
          c <- maybeInt
      } yield (a, b, c)
      ''

let haskellForCode =
      ''
      -- given:
      -- maybeInt :: Maybe Int

      res :: Maybe (Int, Int, Int)
      res = do a <- maybeInt
               b <- maybeInt
               c <- maybeInt
               return (a, b, c)
      ''

let haskellFor =
      [ hask
          "Haskell"
          haskellForCode
          ( Some
              ''
              ${F.code
                  "return"} is not a keyword - it's just a function of type ${F.code
                                                                                "Monad m => a -> m a"} so you can think of it as ${F.code
                                                                                                                                     "pure"} in cats or monix.
              <br /><br />
              <b>This form of do notation is whitespace-sensitive.</b> Subsequent terms (${F.code
                                                                                             "a"}, ${F.code
                                                                                                       "b"}, ${F.code
                                                                                                                 "c"} in the example above) must be aligned horizontally.
              ''
          )
      ]

in  Toplevel.topLevel
      "for comprehension"
      T.SubPage.ForComprehension
      [ F.mkComparison "" scalaFor haskellFor ]
