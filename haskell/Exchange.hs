-- 

data Ask
data Bid

type Count  = Int
type Symbol = String
type Client = Int

data Order a
  = Order a
  { symbol  :: Symbol   -- ^
  , price   :: Price    -- ^ Price at which to buy/sell
  , count   :: Count    -- ^ How many shares to buy/sell
  , client  :: Client   -- ^ Which client made the order
  } deriving (Eq, Show, Read)

data Queue a =
  Queue
  { price   :: Price
  , orders  :: [a]
  } deriving (Eq, Show, Read)

data Book = Book
  { symbol  :: Symbol
  , bids    :: M.Map Price (Queue (Order Bid))
  , asks    :: M.Map Price (Queue (Order Ask))
  } deriving (Eq, Show, Read)

-- | Bids at the highest price
bestBid :: Book -> Queue Bid
bestBid = head . M.elems . bids

-- | Asks at the lowest price
bestOffer :: Book -> Queue Ask
bestOffer = last . M.elems . ask

-- | Ratio of best bid queue length to best ask queue length
pUP :: Book -> Rational
pUP = imbalance . (qsize . bids &&& qsize . asks)
  where qsize = length . snd
        imbalance (x, y) = x / (x + y)

pDN :: Book -> Rational
pDN = imbalance . (qsize . asks &&& qsize . bids)
  where qsize = length . snd
        imbalance (x, y) = x / (x + y)
