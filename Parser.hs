
module Parser where

import Control.Monad
import Data.Char


data Term = Atom String
          | Variable String --represents Strings beginning with uppercase
          | String String --represents strings starting with " and ending with "
          | Compound Term [Term]
    
instance Show Term where
    show(Atom a) = a
    show(Variable a) = a
    show(String a) = a
    show (Compound e1 e2) = show e1 ++ show e2
instance Show Predicate where
    show(Predicate e1 e2) = show e1 ++ show e2 
instance Show Query where
    show(Query (c:cs)) = show (c:cs)
instance Show Rule where
    show(Rule (c:cs)) = show (c:cs)
data Predicate = Predicate Term [Term] 
data Rule = Rule [Predicate]
data Rulelist = Rulelist [Rule]
data Query = Query [Predicate]

termEq :: Term -> Term -> Bool
termEq (Atom a) (Atom b) = a == b
termEq (Variable a) (Variable b) = a == b
termEq (String a) (String b) = a == b
--termEq (Compound a [b]) (Compound c [d]) = termEq a c && map (termEq b d)

vars::Term -> [Term]
vars (Atom e1)= []
vars (Variable e1) = [Variable e1]
vars (String e1) = []
vars (Compound e1 e2) = vars1 e2

varsPredicate :: Predicate -> [Term]
varsPredicate (Predicate a (x:xs)) = vars x ++ vars1 xs 

varsQuery :: Query -> [Term]
varsQuery (Query x) = varsQuery2 x

varsQuery2 :: [Predicate] -> [Term]
varsQuery2 [] = [Atom []]
varsQuery2 (c:cs) = varsPredicate c ++ varsQuery2 cs --transforms query into a list of predicates

vars1::[Term] -> [Term]
vars1 [] = []
vars1 (x:xs) = vars x ++ vars1 xs

space :: Parser String
space = many (satisfy isSpace)

trim :: Parser a -> Parser a
trim p = do
     space
     r <- p
     return r

dot = trim (char '.')
comma = trim (char ',')
smiley = trim (string ":-")

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

isString :: Char -> Bool
isString c = c == '"'


termSatisfy:: (Char -> Bool) -> (Char -> Bool) -> Parser String
termSatisfy firstChar restChar = (do
    c <- satisfy firstChar
    cs <- many (satisfy restChar)
    return (c:cs)) </>
    do
        c <- satisfy isDigit
        cs <- many (satisfy restChar)
        return (c:cs)

variable :: Parser Term
variable = fmap Variable $ termSatisfy isUpper isIdentChar

atom :: Parser Term
atom = fmap Atom $ termSatisfy isLower isIdentChar

stringTerm :: Parser Term
stringTerm = fmap String $ termSatisfy isString isIdentChar

compound :: Parser Term
compound =
    do
        c <- atom
        string "("
        d <- term
        e <- compoundTerms
        return (Compound c (d : e))

term :: Parser Term
term = do
    variable </> compound </> stringTerm </> atom


predicate :: Parser Predicate
predicate =
    do
        a <- atom
        string "("
        c <- term
        cs <- predicateTerms
        return (Predicate a (c:cs))


rule :: Parser Rule
rule =
    (do
        a <- predicate
        string " :- "
        d <- predicate
        e <- many ruleTerms
        symbol "."
        return (Rule (a:d:e)) ) </>
    (do
        c <- predicate
        symbol "."
        return (Rule [c]))

query :: Parser Query
query =
    do
        c <- predicate
        cs <- queryTerms
        return (Query (c:cs))

        
---- helper functions ----
queryTerms :: Parser [Predicate]
queryTerms =
    (do
        string ","
        c <- predicate
        cs <- queryTerms
        return (c:cs) 
        ) </>
    (do
        symbol "?"
        return [])

predicateTerms :: Parser [Term]
predicateTerms =
    (do
        symbol ","
        c <- term
        cs <- predicateTerms
        return (c:cs)) </>
    (do string ")"
        return [])

ruleTerms :: Parser Predicate
ruleTerms =
    do
        string ","
        predicate

compoundTerms :: Parser [Term]
compoundTerms =
    (do
        string ","
        c <- term
        cs <- compoundTerms
        return (c:cs)) </>
    (do
        many (string ")")
        return [])
-----------------------------------------------------
--function to check if query is equal to given rule for first val
checkFirstRule::Rule -> Query -> Bool 
checkFirstRule (Rule [a])( Query [b]) =   predEq a b
test1 = Rule [Predicate (Atom "edge") [String "(a,b)"] ]
test2 = Query[Predicate (Atom "edge") [String "(a,b)"] ]
checkFirstRule test1 test2 -- works

{-substitution :: Rulelist -> Query -> Rule
substitution (Rulelist (x:xs)) (Query (c:cs)) = 
    do
        v <- varsQuery (Query (c:cs))
        if 
            not null (x:xs)
        then 
            checkRuleList (Rulelist x:xs) q
        else
            substitution2 v  -}
substitution2 :: [Term] -> Rule
substitution2 v = Rule [Predicate (Atom "test") [Atom "test"]]


{-checkRuleList :: Rulelist -> Query -> Rule
checkRuleList (Rulelist (x:xs)) (Query c) = -- Rulelist is a list of all parsed rules AKA Rulelist [Rule]
    do                                     -- rules are formatted as Rule [Predicate], so we must search through each rule in
        if                                  -- the rulelist for their predicates, then compare to the query predicate
             formatRule x predEq c    --this line compares the rules we already have's predicates to the new query predicate
        then
            x                         --return the rule if the query is inside the rulelist
        else if
            not (null xs)
        then
            checkRuleList (Rulelist xs) (Query c)
        else
            Rule [Predicate (String "False") []]  --return empty substitution if no rule found -}


newtype Parser a = Parser (String -> [(a, String)])

runParser :: Parser a -> String -> [(a, String)]
runParser (Parser f) = f

parse :: Parser a -> String -> Maybe a
parse p s
    | null ps   = Nothing
    | otherwise = Just (fst (head ps))
    where ps = runParser p s

instance Functor Parser where
    fmap f p = Parser (\s -> [(f a, s') | (a, s') <- runParser p s])

instance Monad Parser where
    return a = Parser (\s -> [(a, s)])
    p >>= k  = Parser (\s -> concat [runParser (k a) s' | (a, s') <- runParser p s])

instance Applicative Parser where
    pure = return
    (<*>) = liftM2 ($)

--------------------------------------------------------------------------------

next :: Parser Char
next = Parser f
    where f [] = []
          f (c:cs) = [(c, cs)]

err :: Parser a
err = Parser f
    where f _ = []

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser f
    where f s = runParser p s ++ runParser q s

(</>) :: Parser a -> Parser a -> Parser a
p </> q = Parser f
    where f s | null ps = qs
              | otherwise = ps
              where ps = runParser p s
                    qs = runParser q s

--------------------------------------------------------------------------------

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- next
               if p c
               then return c
               else err

char :: Char -> Parser ()
char c = do satisfy (c ==)
            return ()

string :: String -> Parser ()
string cs = mapM_ char cs

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

digit :: Parser Int
digit = do d <- satisfy isDigit
           return (ord d - ord '0')

--------------------------------------------------------------------------------

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p sep = p >>= rest
    where rest x = (do f <- sep
                       y <- p
                       rest (f x y)) </>
                   return x

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x : xs)

option :: a -> Parser a -> Parser a
option x p = p </> return x

many :: Parser a -> Parser [a]
many = option [] . many1

--------------------------------------------------------------------------------

spaces :: Parser String
spaces = many (satisfy isSpace)

token :: Parser a -> Parser a
token p = spaces >> p

symbol :: String -> Parser ()
symbol cs = token (string cs)

nat, natural :: Parser Int
nat = do ds <- many1 digit
         return (number (reverse ds))
    where number [] = 0
          number (d:ds) = d + 10 * number ds
natural = token nat

int :: Parser Int
int = token (do f <- option id (string "-" >> return negate)
                n <- nat
                return (f n))


--------------------------------------------------------------------------------

sepBy, sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 p q = do x <- q
                xs <- option [] (p >> sepBy p q)
                return (x : xs)
sepBy p q = option [] (sepBy1 p q)

surround :: Parser a -> Parser b -> Parser c -> Parser b
surround p q r = do p
                    x <- q
                    r
                    return x

parens, brackets :: Parser a -> Parser a
parens p = surround (symbol "(") p (symbol ")")
brackets p = surround (symbol "[") p (symbol "]")

       
natLiteral :: Parser Int        -- to include these two in the definition for "compound"
natLiteral = numbers 0
    where numbers :: Int -> Parser Int
          numbers 0 =
              (
               do string "(succ"
                  nums <- numbers 1
                  return (1+nums)) </>
              (
               do string "(zero)"
                  return 0
               )
          numbers n =
              (
               do string "(succ"
                  nums <- numbers (n + 1)
                  return (1 + nums)) </>
              (
               do char ')'
                  nums <- numbers n
                  return (0 + nums)) </>
              (
               do string "(zero"
                  nums <- numbers n
                  return (0 + nums)) </>
                return 0

listLiteral :: Parser [Term]
listLiteral = 
    (do string "nil"
        many (string ")")
        return []) </>  
    (
    do string "cons("
       c <- term
       string ","
       cs <- listLiteral
       return (c:cs)
     ) </>
    (
    do
        c <- term
        string ")"
        many (string ")")
        return [c]   
     
    )

{-main :: String
main =
    do
        d <- many rule -}
