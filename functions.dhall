let List_head = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/head
let List_drop = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/drop
let List_map = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/map
let List_fold = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/fold
let Optional_fold = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Optional/fold

let Comparison = 
	{ name: Text
	, scalaCode: Text
	, haskellCode: Text
	}

let Explained = 
	{ title: Text
	, code: Text
	, explanation: Optional Text
	}

let ExplainedComparison = 
	{ comparisonName: Text
	, scalaCode: Text
	, haskell: List Explained
	}

let mkComparison = \(scalaCode: Text)   -> 
				   \(haskellCode: Text) -> {
						scalaCode   = scalaCode,
						haskellCode = haskellCode
					}

let mkExplained = \(title: Text) ->
				  \(code: Text) ->
				  \(explanation: Optional Text) -> {
				        title = title,
				  		code = code,
				  		explanation = explanation
				  }

let mkExplainedComparison = \(comparisonName: Text) ->
							\(scalaCode: Text) ->
							\(explained: List Explained) -> {
								comparisonName = comparisonName,
								scalaCode = scalaCode,
								haskell = explained
							}

let renderHaskellCard = \(explained: Explained) -> 
	let explainedText = Optional_fold Text explained.explanation Text (\(x: Text) -> x) ""
	in ''
        <div class="card"><div class="lang">${explained.title}</div><pre class="code"><code>
${explained.code}
  </code></pre>
  			<div class="explanation">${explainedText}</div>
        </div> 
''

let renderPair = \(scalaCode: Text) -> \(explained: Explained) -> ''
	  <div class="pair">
        <div class="card"><div class="lang">Scala</div><pre class="code"><code>
${scalaCode}
  </code></pre>
        </div>
        ${renderHaskellCard explained}
      </div>
''

let renderIncompletePair = \(explained: Explained) -> ''
	  <div class="pair">
        <div class="card">
        </div>
        ${renderHaskellCard explained}
      </div>
''

let renderExplainedComparison = \(comparison: ExplainedComparison) -> 
	let head 		= (List_head Explained comparison.haskell)
	let headAsText  = Optional_fold Explained head Text (\(x: Explained) -> renderPair comparison.scalaCode x) ""
	let tail = List_drop 1 Explained comparison.haskell
	let renderedElements = List_map Explained Text renderIncompletePair tail
	let elementsAsText     = List_fold Text renderedElements Text (\(acc: Text) -> \(curr: Text) -> acc ++ curr) ""
in ''
<div class="case">
	<div class="name">${comparison.comparisonName}</div>
'' ++ headAsText ++ elementsAsText ++ ''
</div>
''


in {
	mkComparison 		  		= mkComparison,
	mkExplained           		= mkExplained,
	mkExplainedComparison 		= mkExplainedComparison,
	renderExplainedComparison 	= renderExplainedComparison
}
