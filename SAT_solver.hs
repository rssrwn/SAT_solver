import qualified Data.Set as Set

type Variable = Int
type Clause = Set.Set Variable
type Formula = Set.Set Clause
type VarAssignment = [(Var, Bool)]

getVariables :: Formula -> [Variable]
getVariables cs = Set.toList $ foldr Set.union Set.empty cs

-- Get all clauses containing v
getClausesWithVar :: Formula -> Variable -> Set.Set Clause
getClausesWithVar cs v = Set.filter (Set.member v) cs

-- Get all pairs of clauses containing v with one pair containing -v
getPairsClauses :: Formula -> Variable -> Set.Set (Clause, Clause)
getPairsClauses cs v
  = Set.foldr Set.union Set.empty pairs
  where
    csWithV = getClausesWithVar cs v
    csWithNegV = getClausesWithVar cs (-v)
    pairs = Set.map (\c -> Set.map (\x -> (c, x)) csWithNegV) csWithV

-- Removes all clauses containing v or -v
removeClauses :: Formula -> Variable -> Formula
removeClauses cs v
  = Set.difference cs (Set.union csWithV csWithNegV)
  where
    csWithV = getClausesWithVar cs v
    csWithNegV = getClausesWithVar cs (-v)

-- Returns remaining clause after resolving c1 and c2
-- c1 and c2 must both contain variable and with opposite polarity
resolve :: (Clause, Clause) -> Variable -> Clause
resolve (c1, c2) v
  | Set.member v c1 = Set.union (Set.delete v c1) (Set.delete (-v) c2)
  | otherwise       = Set.union (Set.delete (-v) c1) (Set.delete v c2)

davisPutman :: Formula -> Bool
davisPutman cs = davisPutman' cs (getVariables cs)

davisPutman' :: Formula -> [Variable] -> VarAssignment
davisPutman' cs [] = True
davisPutman' cs (v:vs)
  | Set.member Set.empty cs = False
  | otherwise
    = davisPutman' newCs vs
    where
      resolved = Set.map ((flip resolve) v) (getPairsClauses cs v)
      addedCs = Set.union cs resolved
      newCs = removeClauses addedCs v
