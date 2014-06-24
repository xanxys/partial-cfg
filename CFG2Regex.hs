

data CFG key term = CFG key [(key, CFGClause key term)] deriving(Show, Eq)

data CFGClause key term = CFGClause [[CFGTerm key term]] deriving(Show, Eq)

data CFGTerm key term
	= NonTerminal key
	| Terminal term
	deriving(Show, Eq)


simpleExpr :: CFG String String
simpleExpr = CFG "expr"
	[("expr", CFGClause
		[[NonTerminal "number"]
		,[Terminal "(", NonTerminal "expr", Terminal ")"]
		,[NonTerminal "addition"]])
	,("addition", CFGClause
		[[NonTerminal "expr", Terminal "+", NonTerminal "expr"]])
	,("number", CFGClause
		[[NonTerminal "digit"]
		,[NonTerminal "digit", Terminal "number"]])
	,("digit", CFGClause
		[[Terminal "0"]
		,[Terminal "1"]])]


main = do
	putStrLn "hello"
	print simpleExpr
