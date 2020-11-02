let Prelude =
      https://prelude.dhall-lang.org/package.dhall sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

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
    = \(a : Type) ->
      \(predicates : List (a -> Bool)) ->
      \(l : a) ->
      \(r : a) ->
        let apply = \(predicate : a -> Bool) -> predicate l && predicate r

        in  Prelude.Bool.or (Prelude.List.map (a -> Bool) Bool apply predicates)

let SubPage/equals
    : SubPage -> SubPage -> Bool
    = makeEquals
        SubPage
        [ isBasics, isAdts, isLists, isOption, isForComprehension ]

let MenuItem = { subPage : SubPage, name : Text, filename : Text }

let mkComparison =
      \(scalaCode : Text) -> \(haskellCode : Text) -> { scalaCode, haskellCode }

let mkExplained =
      \(title : Text) ->
      \(code : Text) ->
      \(explanation : Optional Text) ->
        { title, code, explanation }

let mkExplainedComparison =
      \(comparisonName : Text) ->
      \(scalaCode : Text) ->
      \(explained : List Explained) ->
        { comparisonName, scalaCode, haskell = explained }

let mkMenuItem =
      \(subPage : SubPage) ->
      \(name : Text) ->
      \(filename : Text) ->
        { subPage, name, filename }

in  { Comparison
    , Explained
    , ExplainedComparison
    , MenuItem
    , SubPage
    , mkComparison
    , mkExplained
    , mkExplainedComparison
    , mkMenuItem
    , SubPage/equals
    }
