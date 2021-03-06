{-# LANGUAGE UnicodeSyntax, CPP #-}
module Compta (
  Compta,
  Solde,
  LigneBilan(..),
  TypeSolde(..),
  solde,
  somme,
  soldeToFrac,
  soldeToString,
  fracToSolde,
  comptaFile)
    where

import FunctionalTools.Unicode
import Data.Time

type Solde = Int
type Compta = [LigneBilan]

data LigneBilan = LigneBilan {
  nom ∷ String,
  activé ∷ Bool,
  typeSolde ∷ TypeSolde}
           deriving (Show, Read)

data TypeSolde = Liquidités Solde
               | Créance Solde
               | Budget Solde [(String,Solde)]
               | Dette Solde
               | BudgetJournalier Solde
                 deriving (Show,Read)

solde' ∷ TypeSolde → IO Solde
solde' (Liquidités s) = return s
solde' (Créance s) = return s
solde' (Budget b d) = return $ min 0 $ sum (map snd d) - b
solde' (Dette d) = return (-d)
solde' (BudgetJournalier b) = do
  d ← dayOfMonth
  return $ min 0 $ fromIntegral (d - 31) * b

#ifdef DEBUG
solde ts = do
  s ← solde' ts
  putStrLn "Heavy operation performed"
  return s
#else
solde = solde'
#endif

dayOfMonth :: IO Int
dayOfMonth = getZonedTime ≫=
             (\(_,_,d) → return d) ∘ toGregorian ∘ localDay ∘ zonedTimeToLocalTime

somme ∷ Compta → IO Solde
somme = fmap sum ∘
        mapM (solde ∘ typeSolde) ∘
        filter activé

soldeToFrac ∷ (Fractional γ) ⇒ Solde → γ
soldeToFrac = (÷ 100) ∘ fromIntegral

soldeToString ∷ Solde → String
soldeToString = show ∘ (soldeToFrac ∷ Solde → Double)

fracToSolde ∷ (RealFrac γ) ⇒ γ → Solde
fracToSolde = round ∘ (⋅100)

comptaFile ∷ FilePath
comptaFile = "/home/navaati/.local/share/compta/perso.dat"
