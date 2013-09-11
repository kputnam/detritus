data Nat
  = Z
  | S Nat
  deriving (Eq, Show, Read)

-- induction :: (P:: Nat -> Type)               -- given some property P
--           -> P Z                             --   a proof of P 0
--           -> ((x:: Nat) -> P x -> P (S x))   --   a proof of P n -> P (n + 1)
--           -> (n: Nat)                        -- then for all n, P n
--           -> P n
induction :: a -> (Nat -> a -> a) -> a

induction z _ Z     = z 
induction z s (S x) = s x (induction z s x)
