{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintGram where

-- pretty-printer generated by the BNF converter

import AbsGram
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))



instance Print Entry where
  prt i e = case e of
    EQuestion question -> prPrec i 0 (concatD [prt 0 question, doc (showString "?")])
    EState statement -> prPrec i 0 (concatD [prt 0 statement])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "\n"), prt 0 xs])
instance Print Statement where
  prt i e = case e of
    Location person locverb place -> prPrec i 0 (concatD [prt 0 person, prt 0 locverb, doc (showString "the"), prt 0 place])
    EitherLoc person place1 place2 -> prPrec i 0 (concatD [prt 0 person, doc (showString "is either in the"), prt 0 place1, doc (showString "or the"), prt 0 place2])
    NotLoc person place -> prPrec i 0 (concatD [prt 0 person, doc (showString "is no longer in the"), prt 0 place])
    Take person takeverb object -> prPrec i 0 (concatD [prt 0 person, prt 0 takeverb, doc (showString "the"), prt 0 object])
    Drop person dropverb object -> prPrec i 0 (concatD [prt 0 person, prt 0 dropverb, doc (showString "the"), prt 0 object])
    Hand person1 handverb object person2 -> prPrec i 0 (concatD [prt 0 person1, prt 0 handverb, doc (showString "the"), prt 0 object, doc (showString "to"), prt 0 person2])
    Dir place1 edirection place2 -> prPrec i 0 (concatD [doc (showString "The"), prt 0 place1, doc (showString "is"), prt 0 edirection, doc (showString "of the"), prt 0 place2])

instance Print LocVerb where
  prt i e = case e of
    Visin -> prPrec i 0 (concatD [doc (showString "is in")])
    Vmoved -> prPrec i 0 (concatD [doc (showString "moved to")])
    Vjourneyed -> prPrec i 0 (concatD [doc (showString "journeyed to")])
    Vwent -> prPrec i 0 (concatD [doc (showString "went to")])
    Vtravelled -> prPrec i 0 (concatD [doc (showString "travelled to")])

instance Print TakeVerb where
  prt i e = case e of
    Vtake -> prPrec i 0 (concatD [doc (showString "took")])
    Vpick -> prPrec i 0 (concatD [doc (showString "picked up")])
    Vgot -> prPrec i 0 (concatD [doc (showString "got")])

instance Print DropVerb where
  prt i e = case e of
    Vdrop -> prPrec i 0 (concatD [doc (showString "dropped")])
    Vdisc -> prPrec i 0 (concatD [doc (showString "discarded")])

instance Print HandVerb where
  prt i e = case e of
    Vhand -> prPrec i 0 (concatD [doc (showString "handed")])

instance Print Question where
  prt i e = case e of
    PerLoc person place -> prPrec i 0 (concatD [doc (showString "Is"), prt 0 person, doc (showString "in the"), prt 0 place])
    ObjLoc object -> prPrec i 0 (concatD [doc (showString "Where is the"), prt 0 object])
    HowMany person -> prPrec i 0 (concatD [doc (showString "How many objects is"), prt 0 person, doc (showString "carrying")])
    Before person place -> prPrec i 0 (concatD [doc (showString "Where was"), prt 0 person, doc (showString "before the"), prt 0 place])
    After person place -> prPrec i 0 (concatD [doc (showString "Where was"), prt 0 person, doc (showString "after the"), prt 0 place])
    Path place1 place2 -> prPrec i 0 (concatD [doc (showString "How do you go from the"), prt 0 place1, doc (showString "to the"), prt 0 place2])

instance Print Person where
  prt i e = case e of
    EPerson id -> prPrec i 0 (concatD [prt 0 id])

instance Print Place where
  prt i e = case e of
    EPlace id -> prPrec i 0 (concatD [prt 0 id])

instance Print Object where
  prt i e = case e of
    EObject id -> prPrec i 0 (concatD [prt 0 id])

instance Print EDirection where
  prt i e = case e of
    Dnorth -> prPrec i 0 (concatD [doc (showString "north")])
    Deast -> prPrec i 0 (concatD [doc (showString "east")])
    Dsouth -> prPrec i 0 (concatD [doc (showString "south")])
    Dwest -> prPrec i 0 (concatD [doc (showString "west")])


