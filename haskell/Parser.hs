{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( offset
  , column
  , line
  , advance
  --
  , parse
  , parseOnly
  , feed
  , (<?>)
  , done
  , position
  , eof
  , char
  , notChar
  , anyChar
  , satisfy
  , skip
  , skipWhile
  , satisfyWith
  , take
  , takeWith
  , takeRest
  , text
  , (.*>)
  , (<*.)
  , takeWhile
  , takeTill
  , manyTill
  , takeWhile1
  , skipSpace
  , skipSpace1
  ) where

import Prelude hiding (take, takeWhile)

import Control.Applicative
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Char as C

hole :: a
hole = undefined

data Status
  = More
  | Done
  deriving (Eq, Show)

data Position
  = P { offset :: Int
      , column :: Int
      , line   :: Int }

type Input t = t

data Result t a
  = Success t Position a
  | Failure t Position String
  | Partial (Parser t a)

newtype Parser t a = Parser { runParser :: Input t
                                        -> Position
                                        -> Status
                                        -> Result t a }

instance Show Position where
  show p = foldr (++) "" ["@["
                         , show (offset p), ", C"
                         , show (column p), ", L"
                         , show (line p), "]"]

instance (Show t, Show a) => Show (Result t a) where
  show (Failure t _ x) = unwords ["Failure", show t, show x]
  show (Success t _ a) = unwords ["Success", show t, show a]
  show (Partial _)     = unwords ["Partial", "_"]

instance Functor (Result t) where
  fmap _ (Failure t p e) = Failure t p e
  fmap f (Success t p a) = Success t p (f a)
  fmap f (Partial k)     = Partial (fmap f k)

instance Functor (Parser t) where
  fmap f p = Parser $ \t q s -> fmap f (runParser p t q s)

instance Applicative (Parser t) where
  pure a  = Parser $ \t p _ -> Success t p a
  f <*> a = Parser $ \t p s -> case runParser f t p s of
                                 Failure t' p' x  -> Failure t' p' x
                                 Success t' p' f' -> f' <$> runParser a t' p' s
                                 Partial k        -> Partial (k <*> a)

instance Alternative (Parser t) where
  empty   = Parser $ \t p _ -> Failure t p "empty"
  a <|> b = Parser $ \t p s -> case runParser a t p s of
                                 Failure {}       -> runParser b t p s
                                 Success t' p' f' -> Success t' p' f'
                                 Partial k        -> Partial (k <|> b)

instance Monad (Parser t) where
  return a = Parser $ \t p _ -> Success t p a
  m >>= f  = Parser $ \t p s -> case runParser m t p s of
                                  Failure t' p' x -> Failure t' p' x
                                  Success t' p' a -> runParser (f a) t' p' s
                                  Partial k       -> Partial (k >>= f)

--------------------------------------------------------------------------------

-- | Run a parser.
parse :: Parser t a -> t -> Result t a
parse p t = runParser p t (P 0 0 1) More

-- | Run a parser that cannot be resupplied via a 'Partial' result.
parseOnly :: Parser t a -> t -> Result t a
parseOnly p t = runParser p t (P 0 0 1) Done

feed :: Monoid t => Result t a -> t -> Result t a
feed (Partial k)     t' = runParser k t' undefined More
feed (Failure t p x) t' = Failure (t <> t') p x
feed (Success t p a) t' = Success (t <> t') p a

done :: Monoid t => Result t a -> Result t a
done (Partial k) = runParser k mempty undefined Done
done r           = r

prompt :: Monoid t => t -> Position -> Status -> Parser t a -> Result t a
prompt t p Done _ = Failure t p "unexpected end of input"
prompt t p More f = Partial (Parser $ \t' _ s -> runParser f (t <> t') p s)

-- | Return the current position.
position :: Parser t Position
position = Parser $ \t p _ -> Success t p p

infix 0 <?>

-- | Name the parser, in case failure occurs.
(<?>) :: Parser t a -> String -> Parser t a
f <?> x = Parser $ \t p s ->
  case runParser f t p s of
    Failure t' p' _ -> Failure t' p' x
    Success t' p' a -> Success t' p' a
    Partial k       -> Partial (k <?> x)

-- Generic combinators
--------------------------------------------------------------------------------

manyTill :: Alternative f => f a -> f b -> f [a]
manyTill = undefined

-- Text-specific implementation
--------------------------------------------------------------------------------

advance :: Position -> T.Text -> Position
advance p = loop (offset p) (line p) (column p)
  where
    loop o l c t = case T.uncons t of
      Nothing         -> P { offset = o
                           , column = c
                           , line   = l }
      Just ('\n', t') -> loop (o + 1) (l + 1) 0 t'
      Just (_, t')    -> loop (o + 1) l (c + 1) t'

infixl 4 .*>
(.*>) :: T.Text -> Parser T.Text a -> Parser T.Text a
t .*> p = text t *> p

infixl 4 <*.
(<*.) :: Parser T.Text a -> T.Text -> Parser T.Text a
p <*. t = p <* text t

-- | Match only if all input has been consumed.
eof :: Parser T.Text ()
eof = Parser $ \t p s ->
  case (T.null t, s) of
    (True, More) -> prompt t p s eof
    (True, Done) -> Success t p ()
    (False, _)   -> Failure t p "expected end of input"

-- | Match a specific character.
char :: Char -> Parser T.Text Char
char a = satisfy (== a) <?> show a

-- | Match any character except the given one.
notChar :: Char -> Parser T.Text Char
notChar a = satisfy (/= a) <?> "not " <> show a

-- | Match any character.
anyChar :: Parser T.Text Char
anyChar = satisfy (const True)

-- | The parser @satisfy p@ succeeds for any character for which the
-- predicate @p@ returns 'True'. Returns the character that was parsed.
satisfy :: (Char -> Bool) -> Parser T.Text Char
satisfy p = satisfyWith id p <?> "satisfy"

-- | The parser @skip p@ succeeds for any character for which the
-- predicate @p@ returns 'True'.
skip :: (Char -> Bool) -> Parser T.Text ()
skip p = () <$ satisfy p <?> "skip"

-- | The parser @satisfyWith f p@ transforms a character, and succeeds
-- if the predicate returns 'True' on the transformed value. Returns the
-- transformed character that was parsed.
satisfyWith :: (Char -> a) -> (a -> Bool) -> Parser T.Text a
satisfyWith f g = Parser $ \t p s ->
  case T.uncons t of
    Nothing      -> prompt t p s (satisfyWith f g)
    Just (c, t') -> let a = f c in
      if g a
      then Success t' (advance p $ T.singleton c) a
      else Failure t p "satisfyWith"

-- | @text t@ parses a sequence of characters that identically match @t@.
-- Returns the parsed string (i.e. @t@). This parser consumes no input if
-- it fails (even if a partial match).
text :: T.Text -> Parser T.Text T.Text
text a = Parser partial
  where
    partial t p = loop a t p
      where
        loop a' t' p' s' =
          case T.compareLength t' an of
            LT -> if b == t'
                  then prompt t' p' s' (Parser $ loop bs)
                  else Failure t p ("expected " <> T.unpack a)
            _  -> if a' == s
                  then Success ss (advance p' s) a
                  else Failure t p ("expected " <> T.unpack a)
          where an     = T.length a'
                tn     = T.length t'
                (b,bs) = T.splitAt tn a'
                (s,ss) = T.splitAt an t'

-- | Consume exactly @n@ characters of input.
take :: Int -> Parser T.Text T.Text
take n = takeWith n (const True)

-- Require a certain number of characters
takeWith :: Int -> (T.Text -> Bool) -> Parser T.Text T.Text
takeWith n f = Parser $ \t p s ->
  case T.compareLength t n of
    LT -> prompt t p s (takeWith n f)
    _  -> let (x, x') = T.splitAt n t in
            if f x
            then Success x' (advance p x) x
            else Failure t p "takeWith"

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input. This parser does not fail. It will return an
-- empty string if the predicate returns 'False' on the first character
-- of input.
takeWhile :: (Char -> Bool) -> Parser T.Text T.Text
takeWhile = hole

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input. The parser requires the predicate to succeed on
-- at least one character of input. It will fail if the predicate never
-- returns 'True' or if there is no input left.
takeWhile1 :: (Char -> Bool) -> Parser T.Text T.Text
takeWhile1 = hole

skipSpace :: Parser T.Text ()
skipSpace = () <$ takeWhile C.isSpace

skipSpace1 :: Parser T.Text ()
skipSpace1 = () <$ takeWhile1 C.isSpace

-- | Consume input as long as the predicate returns 'False' (i.e. until
-- it returns 'True' on the first character of input). This parser does
-- not fail. It will return an empty string if the predicate returns
-- 'True' on the first character of input.
takeTill :: (Char -> Bool) -> Parser T.Text T.Text
takeTill = hole

-- | Skip past input for as long as the predicate returns 'True'.
skipWhile :: (Char -> Bool) -> Parser T.Text ()
skipWhile = hole

-- | Consume all the remaining input and return it as a single string.
takeRest :: Parser T.Text T.Text
takeRest = hole
