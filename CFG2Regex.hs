import Control.Monad
import qualified Data.Map as Map
import Data.List
import Data.Maybe

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


-- | Symbol-based regex.
-- (i.e. doesn't contain character specific syntax such as [abc])
data Regex sym
	= RTerminal sym
	| Selection [Regex sym]
	| Sequence [Regex sym]
	| Repetition (Regex sym)
	-- Extensions
	| Optional (Regex sym)
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

-- | Remove empty Sequence and Selection. Result can be Nothing if
-- the langueage turns out to be empty.
--
-- Selection [] = {}
-- Sequence [] = {""}
optimizeRegex :: Eq term => Regex term -> Maybe (Regex term)
optimizeRegex (Selection []) = Nothing
optimizeRegex (Sequence []) = Nothing
optimizeRegex (Selection [r]) = optimizeRegex r
optimizeRegex (Sequence [r]) = optimizeRegex r
optimizeRegex (Selection rs) =
	case mapMaybe optimizeRegex rs of
		[] -> Nothing
		rs' -> wrapOptional $ nub rs'
	where
		wrapOptional [] = Nothing
		wrapOptional rs
			|lang0 `elem` rs = Just $ Optional $ Selection $ filter (/= lang0) rs
			|otherwise = Just $ Selection rs
		lang0 = Sequence []
optimizeRegex (Sequence rs) = do
	rs' <- mapM optimizeRegex rs
	liftM (flattenSequence . Sequence) $  return rs'
optimizeRegex (Repetition r) = liftM Repetition (optimizeRegex r)
optimizeRegex r = return r

flattenSequence :: Regex term -> Regex term
flattenSequence = flattenRegex extractIfSequence Sequence
	where
		extractIfSequence (Sequence rs) = Just rs
		extractIfSequence _ = Nothing

flattenRegex
	:: (Regex term -> Maybe [Regex term])
	-> ([Regex term] -> Regex term)
	-> Regex term
	-> Regex term
flattenRegex extract unextract r = case extract r of
	Just ls -> unextract $ concatMap (wrapAsList . flattenRegex extract unextract) ls
	Nothing -> r
	where
		wrapAsList r = case extract r of
			Just ls -> ls
			Nothing -> [r]


-- | TODO: use proper operator precedence to decide when to put parens.
showRegex :: Regex String -> String
showRegex (RTerminal t)
	|elem t ["(", ")", "|", "+", "*", "[", "]", "^", "\\"] = "\\" ++ t
	|otherwise = t
showRegex (Selection rs) = "(" ++ intercalate "|" (map showRegex rs) ++ ")"
showRegex (Sequence rs) = concatMap showRegex rs
showRegex (Repetition r) = showRegex r ++ "*"
showRegex (Optional r) = showRegex r ++ "?"

main = do
	putStrLn "== input"
	print simpleExpr

	putStrLn $ showRegex $ fromJust $
		optimizeRegex $ convertToRegex 6 simpleExpr
