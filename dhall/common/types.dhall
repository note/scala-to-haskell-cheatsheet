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

let MenuItem = 
	{ name: Text
	, filename: Text
	, isActive: Bool
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

let mkMenuItem = \(name: Text) ->
				 \(filename: Text) ->
				 \(isActive: Bool) ->
				 {
					 name = name,
					 filename = filename,
					 isActive = isActive
				 }

in {
	Comparison = Comparison,
	Explained  = Explained,
	ExplainedComparison = ExplainedComparison,
	MenuItem = MenuItem,
	mkComparison = mkComparison,
	mkExplained = mkExplained,
	mkExplainedComparison = mkExplainedComparison,
	mkMenuItem = mkMenuItem
}
