import Data.Maybe

import CFG2Regex

main = do
	putStrLn "== input"
	print additionExpr

	showBoth $ fromJust $
		optimizeRegex $ convertToRegex 8 additionExpr

showBoth reg = do
	putStrLn "== show"
	print reg
	putStrLn "== pretty (maybe wrong)"
	putStrLn $ showRegex reg


-- | Accepts expression like "0+1+(2+3+4)" or "02342202"
additionExpr :: CFG String String
additionExpr = CFG "expr"
	[("expr", CFGClause
		[[NonTerminal "number"]
		,[Terminal "(", NonTerminal "expr", Terminal ")"]
		,[NonTerminal "addition"]])
	,("addition", CFGClause
		[[NonTerminal "expr", Terminal "+", NonTerminal "expr"]])
	,("number", CFGClause
		[[NonTerminal "digit"]
		,[NonTerminal "digit", NonTerminal "number"]])
	,("digit", CFGClause $
		map (\n->[Terminal $ show n]) [0..2])
	]
