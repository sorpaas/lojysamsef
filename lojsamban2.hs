{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import LojbanTools
import Prolog2
import Language.Lojban.Parser hiding (LA, Brivla, KOhA, GOhA, NA, LerfuString)
import qualified Language.Lojban.Parser as P
import System.Environment
import Data.Maybe
import Data.Either
import Data.List
import Control.Monad
import Control.Arrow
import Control.Applicative

main :: IO ()
main = do
	[fn] <- getArgs
	src <- readFile fn
	let	Right p = parse src
		rules = map readSentence $ getSentences p
	Left q <- (readSentenceFact . either (error "bad") id . parse)
		`fmap` getLine
	let	answer = ask [] [] q rules
--	print (rules :: [Rule String Atom])
--	print (q :: Fact Scope Atom)
	putStrLn $ case answer of
		[] -> "nago'i"
		_ -> case intersperse ".a" $ catMaybes $ (flip map) (map maValue answer) $ (showAtom <$>) of
			[] -> "go'i"
			m -> unwords m
	forM_ (map onlyTopVars answer) $ print
--	putStrLn $ showAnswerAll answer

showAtom :: Atom -> String
showAtom (LA n) = "la " ++ n

maValue :: Result Scope Atom -> Maybe Atom
maValue r = case filter (not . null . fst) $ map (first $ filter isMA) r of
	[] -> Nothing
	((_, tv) : _) -> (\(Con v) -> v) <$> tv

isMA :: Term Scope Atom -> Bool
isMA (Var [_] (KOhA "ma")) = True
isMA _ = False

showAnswerAll a = if null a then "nago'i" else
	intercalate " .a " $ map showAnswer $ map (lookupMA . onlyTop) a

onlyTopVars :: Result Scope s -> Result Scope s
onlyTopVars = filter (not . null . fst) . map (first $ filter isTopVar)

isTopVar :: Term Scope s -> Bool
isTopVar (Var [_] _) = True
isTopVar _ = False

lookupMA = map snd . filter ((Var "top" (KOhA "ma") `elem`) . fst)

showAnswer as = if null as then "go'i" else showLA $ head as

showLA (Just (Con (LA n))) = "la " ++ n
showLA (Just (Con (LO n))) = "lo " ++ n

onlyTop = filter (not . null . fst) .
	map (\(vars, val) -> (filter isTop vars, val))

isTop :: Term String s -> Bool
isTop (Var "top" _) = True
isTop _ = False

data Atom
	= LA String
	| LO String
	| KOhA String
	| Brivla String
	| GOhA String
	| LerfuString String
	deriving (Show, Eq)

type Scope = [Int]

instance TwoD [Int] where
	next (n : ns) = n + 1 : ns
	down ns = 0 : ns

readSumti :: Scope -> Sumti -> Term Scope Atom
readSumti sc (P.LA (_, "la", _) _ _ ns _) = Con $ LA $ concat $ map snd3 ns
readSumti sc (P.LALE (_, "lo", _) _ st _ _) = Con $ LO $ readSumtiTail st
readSumti sc (P.KOhA (_, k, _) _) = Var sc $ KOhA k
readSumti sc (P.LerfuString s _ _) = Var sc $ LerfuString $ concatMap snd3 s

readSumtiTail :: SumtiTail -> String
readSumtiTail (SelbriRelativeClauses (P.Brivla (_, n, _) _) _) = n
readSumtiTail st = show st

readSelbriAtom (P.GOhA (_, n, _) _ _) = GOhA n

readSelbri :: Selbri -> Either (Term Scope Atom) (Term Scope Atom)
readSelbri (P.Brivla (_, n, _) _) = Left $ Con $ Brivla n
readSelbri (P.GOhA (_, n, _) _ _) = Left $ Con $ GOhA n
readSelbri (P.NA (_, "na", _) _ s) = Right $ Con $ readSelbriAtom s

readSentenceFact :: Sentence -> Either (Fact Scope Atom) (Fact Scope Atom)
readSentenceFact s@(TermsBridiTail _ _ _ _) =
	either (\lf -> Left $ \sc -> lf : (h sc ++ t sc))
		(\rf -> Right $ \sc -> rf : (h sc ++ t sc)) f
	where
	h sc = map (readSumti sc) $ headTerms s
	f = readSelbri $ selbri $ bridiTail s
	t sc = map (readSumti sc) $ tailTerms $ bridiTail s
readSentenceFact (TopText _ _ _ _ (Just s) _) = readSentenceFact s
readSentenceFact o = error $ show o

readSentence :: Sentence -> Rule Scope Atom
readSentence s@(TermsBridiTail _ _ _ _) = Rule (\sc -> f : h sc ++ t sc) [] [] []
	where
	h sc = map (readSumti sc) $ headTerms s
	Left f = readSelbri $ selbri $ bridiTail s
	t sc = map (readSumti sc) $ tailTerms $ bridiTail s
readSentence (IJoikJek s [r]) = Rule f [] (getRule r) (getNotRule r)
	where
	Left f = readSentenceFact s

getRule (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) =
	lefts $ readTUhE t
getNotRule (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) =
	rights $ readTUhE t

readTUhE (TUhE _ _ _ t _ _) = map readSentenceFact $ getSentences t
