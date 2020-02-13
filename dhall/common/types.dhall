let Prelude =
      https://prelude.dhall-lang.org/package.dhall sha256:4aa8581954f7734d09b7b21fddbf5d8df901a44b54b4ef26ea71db92de0b1a12

let Comparison = { name : Text, scalaCode : Text, haskellCode : Text }

let Explained = { title : Text, code : Text, explanation : Optional Text }

let ExplainedComparison =
      { comparisonName : Text, scalaCode : Text, haskell : List Explained }

let SubPage = < Basics | Adts | Lists | Option | ForComprehension >

let handlers =
      { Basics = False
      , Adts = False
      , Lists = False
      , Option = False
      , ForComprehension = False
      }

let isBasics = \(t : SubPage) -> merge (handlers // { Basics = True }) t

let isAdts = \(t : SubPage) -> merge (handlers // { Adts = True }) t

let isLists = \(t : SubPage) -> merge (handlers // { Lists = True }) t

let isOption = \(t : SubPage) -> merge (handlers // { Option = True }) t

let isForComprehension =
      \(t : SubPage) -> merge (handlers // { ForComprehension = True }) t

let makeEquals
    : forall (a : Type) -> List (a -> Bool) -> a -> a -> Bool
    =     \(a : Type)
      ->  \(predicates : List (a -> Bool))
      ->  \(l : a)
      ->  \(r : a)
      ->  let apply = \(predicate : a -> Bool) -> predicate l && predicate r

          in  Prelude.Bool.or
                (Prelude.List.map (a -> Bool) Bool apply predicates)

let SubPage/equals
    : SubPage -> SubPage -> Bool
    = makeEquals
        SubPage
        [ isBasics, isAdts, isLists, isOption, isForComprehension ]

let MenuItem = { subPage : SubPage, name : Text, filename : Text }

let mkComparison =
          \(scalaCode : Text)
      ->  \(haskellCode : Text)
      ->  { scalaCode = scalaCode, haskellCode = haskellCode }

let mkExplained =
          \(title : Text)
      ->  \(code : Text)
      ->  \(explanation : Optional Text)
      ->  { title = title, code = code, explanation = explanation }

let mkExplainedComparison =
          \(comparisonName : Text)
      ->  \(scalaCode : Text)
      ->  \(explained : List Explained)
      ->  { comparisonName = comparisonName
          , scalaCode = scalaCode
          , haskell = explained
          }

let mkMenuItem =
          \(subPage : SubPage)
      ->  \(name : Text)
      ->  \(filename : Text)
      ->  { subPage = subPage, name = name, filename = filename }

in  { Comparison = Comparison
    , Explained = Explained
    , ExplainedComparison = ExplainedComparison
    , MenuItem = MenuItem
    , SubPage = SubPage
    , mkComparison = mkComparison
    , mkExplained = mkExplained
    , mkExplainedComparison = mkExplainedComparison
    , mkMenuItem = mkMenuItem
    , SubPage/equals = SubPage/equals
    }
