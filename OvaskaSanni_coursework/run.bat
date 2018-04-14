bnfc -m -haskell Gram.cf
happy -gca ParGram.y
alex -g LexGram.x
ghc --make OvaskaSanni_coursework.hs
