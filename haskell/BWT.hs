{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe

-- https://twitter.com/copumpkin/status/329800696125857794
compress = map (length &&& head)
         . group
         . (last <$>)
         . sort
         . init
         . (zipWith (++) <$> tails <*> inits)
         . (Nothing:)
         . (Just <$>)

-- https://twitter.com/copumpkin/status/329800751809437696
decompress = catMaybes
           . head
           . foldr ((sort .) . zipWith (:)) (repeat [])
           . (replicate =<< length)
           . (uncurry replicate =<<)
