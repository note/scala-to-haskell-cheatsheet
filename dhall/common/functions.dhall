let Prelude = https://prelude.dhall-lang.org/package.dhall

let T = ./types.dhall

let renderHaskellCard = \(explained: T.Explained) -> 
	let explainedText = Optional/fold Text explained.explanation Text (\(x: Text) -> x) ""
	in ''
        <div class="card"><div class="lang">${explained.title}</div><pre class="code"><code class="lang-haskell">${explained.code}</code></pre>
  			<div class="explanation"><p>${explainedText}</p></div>
        </div> 
''

let renderPair = \(scalaCode: Text) -> \(explained: T.Explained) -> ''
	  <div class="pair">
        <div class="card"><div class="lang">Scala</div><pre class="code"><code class="lang-scala">${scalaCode}</code></pre>
        </div>
        ${renderHaskellCard explained}
      </div>
''

let renderIncompletePair = \(explained: T.Explained) -> ''
	  <div class="pair">
        <div class="card">
        </div>
        ${renderHaskellCard explained}
      </div>
''

let renderExplainedComparison = \(comparison: T.ExplainedComparison) -> 
	let head 		= (List/head T.Explained comparison.haskell)
	let headAsText  = Optional/fold T.Explained head Text (\(x: T.Explained) -> renderPair comparison.scalaCode x) ""
	let tail = Prelude.List.drop 1 T.Explained comparison.haskell
	let renderedElements = Prelude.List.map T.Explained Text renderIncompletePair tail
	let elementsAsText     = List/fold Text renderedElements Text (\(acc: Text) -> \(curr: Text) -> acc ++ curr) ""
in ''
<div class="case">
	<div class="name">${comparison.comparisonName}</div>
'' ++ headAsText ++ elementsAsText ++ ''
</div>
''

let simpleHask = \(code: Text) -> T.mkExplained "Haskell" code (None Text)

let mkSimpleComparison = \(name: Text) -> \(scalaCode: Text) -> \(haskellCode: Text) -> 
	T.mkExplainedComparison name scalaCode [(simpleHask haskellCode)]

let mkComparison = \(name: Text) -> \(scalaCode: Text) -> \(haskellExplanations: List T.Explained) -> 
	T.mkExplainedComparison name scalaCode haskellExplanations

let quoteExplanation = \(code: Text) -> ''
<pre class="supplementaryCode"><code>${code}</code></pre>
''

let quoteCode = \(code: Text) -> ''
<pre class="code"><code>${code}</code></pre>
''

let code = \(x: Text) -> "<code>${x}</code>"

in {
	code						= code,
	mkComparison 		  		= mkComparison,
	mkSimpleComparison 		  	= mkSimpleComparison,
	quoteCode					= quoteCode,
	quoteExplanation			= quoteExplanation,
	renderExplainedComparison 	= renderExplainedComparison,
	simpleHask					= simpleHask
}
