import qualified Data.Map as Map
import Data.List

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
		,[NonTerminal "digit", NonTerminal "number"]])
	,("digit", CFGClause $
		map (\n->[Terminal $ show n]) [0..9])
	]


data Regex term
	= RTerminal term
	| Selection [Regex term]
	| Sequence [Regex term]
	| Repetition (Regex term)
	deriving(Show, Eq)


-- | Convert CFG to regex (with given expansion depth).
-- Don't pass large depth, since the result size is often exp(depth).
--
-- memo: this is not really useful practically.
-- Each clause should be able to have counter which can be infinite.
--
-- TODO: is there some way to drop dependence on Map (or assoc list)?
convertToRegex :: (Eq k, Ord k) => Int -> CFG k term -> Regex term
convertToRegex depth (CFG root_key clauses_assoc) =
	expandClause clauses depth $ clauses Map.! root_key
	where
		clauses = Map.fromList clauses_assoc

expandClause :: Ord k => Map.Map k (CFGClause k term) -> Int -> CFGClause k term -> Regex term
expandClause dict 0 _ = Selection []  -- doesn't match anything
expandClause dict depth (CFGClause cs) =
	Selection $ map (Sequence . map toTerm) cs
	where
		toTerm (Terminal t) = RTerminal t
		toTerm (NonTerminal k) = expandClause dict (depth - 1) (dict Map.! k)

-- | TODO: use proper operator precedence to decide when to put parens.
showRegex :: Regex String -> String
showRegex (RTerminal t)
	|elem t ["(", ")", "|", "+", "*", "[", "]", "^", "\\"] = "\\" ++ t
	|otherwise = t
showRegex (Selection rs) = "(" ++ intercalate "|" (map showRegex rs) ++ ")"
showRegex (Sequence rs) = concatMap showRegex rs
showRegex (Repetition r) = showRegex r ++ "*"

main = do
	putStrLn "== input"
	print simpleExpr

	putStrLn "== output(depth=1)"
	putStrLn $ showRegex $ convertToRegex 1 simpleExpr
	putStrLn "== output(depth=1)"
	putStrLn $ showRegex $ convertToRegex 10 simpleExpr