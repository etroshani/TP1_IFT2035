-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Librairie d'analyse syntaxique.
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Llit Int             -- Litéral entier.
          | Lid Var              -- Référence à une variable.
          | Labs Var Lexp        -- Fonction anonyme prenant un argument.
          | Lfuncall Lexp [Lexp] -- Appel de fonction, avec arguments "curried".
          | Lmkref Lexp          -- Construire une "ref-cell".
          | Lderef Lexp          -- Chercher la valeur d'une "ref-cell".
          | Lassign Lexp Lexp    -- Changer la valeur d'une "ref-cell".
          | Lite Lexp Lexp Lexp  -- If/then/else.
          | Ldec Var Lexp Lexp   -- Déclaration locale non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lrec [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Conversion de Sexp à Lambda --------------------------------------------

s2l :: Sexp -> Lexp
s2l (Snum n) = Llit n
s2l (Ssym s) = Lid s
-- ¡¡ COMPLETER !!
-- opérations arithmétiques (genre + 2 3)
s2l (Snode (Ssym "+") [Snum n, Snum m]) = Lfuncall (Lid "+")[Llit n, Llit m] -- il suffit de remplacer "+" par les autres pr avoir toutes les opés
-- détecter une déclaration de fonction anonyme
s2l (Snode (Ssym "λ")[Snode x, Snode e]) = Labs x (s2l e)
-- détecter un appel de fonction (on a juste besoin de 2 cas. Voir ci-dessous comment je transforme même les cas où on dirait qu'il faut plus de 2 cas, en des cas de juste 2 cas)
s2l (Snode e1)[Snode e2] = Lfuncall e1 [s2l e2]

-- explication de pq il faut juste 2 cas pr les appels de fct
--((λ x x) 2)                             ; ↝ 2

--(((λ x (λ y (* x y)))
--  3)
-- 5)                 

-- déclaration : λ x -> e
-- appel : (λ x -> e) 5
-- déclaration : λ y -> * x y (le y vient d'un endroit qqconque du code)
-- appel : (λ y -> * x y) 3

-- construction de "ref-cell"
s2l (Snode (Ssym "ref!")[Snode e]) = Lmkref(s2l e)
-- établir ou changer la valeur d'une "ref-cell"
s2l (Snode (Ssym "set!")[Snode e1, Snode e2]) = Lassign (s2l e1)(s2l e2)
-- get la valeur d'une "ref-cell"
s2l (Snode (Ssym "get!")[Snode e]) = Lderef (s2l e)
-- détecter les expressions du genre "if-then-else"
s2l (Snode (Ssym "if") [Snode e1, Snode e2, Snode e3]) = Lite (s2l e1)(s2l e2)(s2l e3)
-- détecter une déclaration locale non récursive (aussi appelée "simple")
s2l (Snode (Ssym "let")[Snode (Ssym x)[Snode e1, Snode e2]]) = Ldec x (s2l e1)(s2l e2)
-- détecter une déclaration locale récursive
-- À FAIRE
-- détecter une opération booléenne sur les entiers (càd = e1 e2, > e1 1, etc.)
s2l (Ssym "=")[Snode e1, Snode e2] = Lfuncall (Lid "=")[Lid e1, Lid e2] -- pr les autres types d'opés (>, <, >= etc.), il faut juste remplacer "=" par celles-ci

s2l se = error ("Expression Slip inconnue: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Représentation du "tas" ------------------------------------------------

-- Notre tas est représenté par une arbre binaire de type "trie".
-- La position du nœud qui contient l'info pour l'addresse `p` est
-- déterminée par la séquence de bits dans la représentation binaire de `p`.

data Heap = Hempty | Hnode (Maybe Value) Heap Heap

hlookup :: Heap -> Int -> Maybe Value
hlookup Hempty _ = Nothing
hlookup (Hnode mv _ _) 0 = mv
hlookup _ p | p < 0 = error "hlookup sur une adresse négative"
hlookup (Hnode _ e o) p = hlookup (if p `mod` 2 == 0 then e else o) (p `div` 2)

hinsert :: Heap -> Int -> Value -> Heap
hinsert _ p _ | p < 0 = error "hinsert sur une adresse négative"
-- ¡¡ COMPLETER !!

-- Représentation de l'environnement --------------------------------------

-- Type des tables indexées par des `α` et qui contiennent des `β`.
-- Il y a de bien meilleurs choix qu'une liste de paires, mais
-- ça suffit pour notre prototype.
type Map α β = [(α, β)]

-- Transforme une `Map` en une fonctions (qui est aussi une sorte de "Map").
mlookup :: Map Var β -> (Var -> β)
mlookup [] x = error ("Variable inconnue: " ++ show x)
mlookup ((x,v) : xs) x' = if x == x' then v else mlookup xs x'

madd :: Map Var β -> Var -> β -> Map Var β
madd m x v = (x,v) : m

-- On représente l'état de notre mémoire avec non seulement le "tas" mais aussi
-- avec un compteur d'objets de manière a pouvoir créer une "nouvelle" addresse
-- (pour `ref!`) simplement en incrémentant ce compteur.
type LState = (Heap, Int)

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vref Int
           | Vfun ((LState, Value) -> (LState, Value))

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _p (Vref p) = (\s -> "ptr<" ++ show p ++ ">" ++ s)
    showsPrec _ _ = showString "<function>"

type Env = Map Var Value

-- L'environnement initial qui contient les fonctions prédéfinies.
            
env0 :: Env
env0 = let binop :: (Value -> Value -> Value) -> Value
           binop op = Vfun (\ (s1, v1)
                            -> (s1, Vfun (\ (s2, v2)
                                         -> (s2, v1 `op` v2))))

           biniiv :: (Int -> Int -> Value) -> Value
           biniiv op = binop (\ v1 v2
                              -> case (v1, v2) of
                                  (Vnum x, Vnum y) -> x `op` y
                                  _ -> error ("Pas des entiers: "
                                             ++ show v1 ++ "," ++ show v2))

           binii wrap f = biniiv (\ x y -> wrap (f x y))

       in [("+", binii Vnum (+)),
           ("*", binii Vnum (*)),
           ("/", binii Vnum div),
           ("-", binii Vnum (-)),
           ("true",  Vbool True),
           ("false", Vbool False),
           ("<",  binii Vbool (<)),
           (">",  binii Vbool (>)),
           ("=",  binii Vbool (==)),
           (">=", binii Vbool (>=)),
           ("<=", binii Vbool (<=))]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

state0 :: LState
state0 = (Hempty, 0)

eval :: LState -> Env -> Lexp -> (LState, Value)
eval s _env (Llit n) = (s, Vnum n)
-- ¡¡ COMPLETER !!
                  
---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = snd . eval state0 env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode 
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       -- s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
