{-# LANGUAGE RecordWildCards #-}

module Data.Jevko
  ( parse
  , parseMay
  , render
  ) where

import qualified Data.Text            as T
import qualified Data.Attoparsec.Text as Atto

import Control.Monad        ( guard
                            , void
                            )
import Control.Applicative  ( (<|>)
                            , many
                            )
import Data.Attoparsec.Text ( Parser
                            , anyChar
                            , char
                            )

parse :: T.Text -> Either String Jevko
parse = Atto.parseOnly jevkoP

parseMay :: T.Text -> Maybe Jevko
parseMay = either (const Nothing) Just  . parse

render :: Jevko -> T.Text
render Jevko {..} = T.concat (map renderSubjevko subjevkos) <> renderSuffix suffix

renderSubjevko :: Subjevko -> T.Text
renderSubjevko Subjevko {..} = T.concat
  [ renderPrefix prefix
  , T.singleton openerChar
  , render jevko
  , T.singleton closerChar
  ]

renderPrefix :: Prefix -> T.Text
renderPrefix = renderText . unPrefix

renderSuffix :: Suffix -> T.Text
renderSuffix = renderText . unSuffix

renderText :: Text -> T.Text
renderText = T.pack . concatMap f
  where
    f :: Symbol -> [Char]
    f (Character c)
      | c `elem` delimiterChars = [escaperChar, c]
      | otherwise               = [c]


-- ================================================================
-- ================================================================
--   Country Grammar
-- ================================================================
-- ================================================================

-- ================================================================
-- ; start symbol, main rule #1
-- Jevko = *Subjevko Suffix
--

data Jevko = Jevko
  { subjevkos :: [Subjevko]
  , suffix    :: Suffix
  } deriving (Eq, Ord, Read, Show)

jevkoP :: Parser Jevko
jevkoP = Jevko
  <$> many subjevkoP
  <*> suffixP

-- ================================================================
-- ; main rule #2, mutually recursive with #1
-- Subjevko = Prefix Opener Jevko Closer
--

data Subjevko = Subjevko
  { prefix :: Prefix
  , jevko  :: Jevko
  } deriving (Eq, Ord, Read, Show)

subjevkoP :: Parser Subjevko
-- subjevkoP = Subjevko
--   <$> prefixP
--   <*> jevkoP
subjevkoP = do
  p <- prefixP
  openerP
  j <- jevkoP
  closerP
  pure $ Subjevko
    { prefix = p
    , jevko  = j
    }

-- ================================================================
-- ; aliases
-- Suffix = Text
-- Prefix = Text
--

newtype Prefix = Prefix { unPrefix :: Text }
  deriving (Eq, Ord, Read, Show)

newtype Suffix = Suffix { unSuffix :: Text }
  deriving (Eq, Ord, Read, Show)

prefixP :: Parser Prefix
prefixP = Prefix <$> textP

suffixP :: Parser Suffix
suffixP = Suffix <$> textP

-- ; text
-- Text = *Symbol

type Text = [Symbol]

textP :: Parser Text
textP = many symbolP

-- ================================================================
-- Symbol = Digraph / Character
--

type Symbol = Character

symbolP :: Parser Symbol
symbolP = digraphP <|> characterP

delimiterChars :: String
delimiterChars =
  [ openerChar
  , closerChar
  , escaperChar
  ]

digraphP :: Parser Character
digraphP = Character <$> do
  escaperP
  delimiterP

delimiterP :: Parser Char
delimiterP = do
  c <- anyChar
  guard $ c `elem` delimiterChars
  pure c

-- ; Character is any code point which is not a Delimiter
-- Character = %x0-5a / %x5c / %x5e-5f / %x61-10ffff
newtype Character = Character Char
  deriving (Eq, Ord, Read, Show)

characterP :: Parser Character
characterP = Character <$> do
  c <- anyChar
  guard $ c `notElem` delimiterChars
  pure c

-- ================================================================
-- Opener  = %x5b ; left square bracket
--
openerP :: Parser ()
openerP = void . Atto.char $ openerChar

openerChar :: Char
openerChar = '['

-- ================================================================
-- Closer  = %x5d ; right square bracket
--
closerP :: Parser ()
closerP = void . char $ closerChar

closerChar :: Char
closerChar = ']'

-- ================================================================
-- Escaper = %x60 ; grave accent
--

escaperP :: Parser ()
escaperP = void . char $ escaperChar

escaperChar :: Char
escaperChar = '`'
