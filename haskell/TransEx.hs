
type Timeout a
  = MaybeT (ReaderT Integer (StateT Identity)) a

type Timeout a
  = MaybeT (ReaderT Integer (WriterT Sum Identity)) a

-- Count down from starting value
type Timeout a
  = MaybeT (WriterT Sum Identity) a

-- Using Peano natural numbers means we can terinate even
-- if a computation "diverges" consuming a infinite ticks.

--------------------------------------------------------------------------------

-- Remove all duplicate values in a list, producing a log. If there is
-- an element greater than 100, abort the entire computation and produce
-- no result, but append "aborting, ... > 100" substituting for the element.
-- Log any even numbers, like "even ...". Other numbers produce no log.
