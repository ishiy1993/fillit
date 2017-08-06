{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Fillit.Internal
    ( Template
    , Dict
    , Config(..)
    , def
    , parseTemplate
    , combine
    , parseOnly
    , between'
    , Kind(..)
    ) where

import Control.Monad (foldM)
import Data.Bifunctor
import Data.Default
import qualified Data.HashMap.Lazy as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

type Template = Text
type Dict = HM.HashMap Text Text

data Config = Config
    { reqFrom :: String
    , reqTo   :: String
    , optFrom :: String
    , optTo   :: String
    }

instance Default Config where
    def = Config { reqFrom = "$"
                 , reqTo   = "$"
                 , optFrom = "%"
                 , optTo   = "%"
                 }


parseTemplate :: Config -> Template -> Either String [Kind]
parseTemplate cfg = parseOnly tmpParser
    where
        tmpParser :: Parser [Kind]
        tmpParser = many1 $ reqParser cfg
                        <|> optParser cfg
                        <|> try (rawParser cfg)
                        <|> rawParser'

reqParser :: Config -> Parser Kind
reqParser cfg = Req <$> between' (reqFrom cfg) (reqTo cfg)

optParser :: Config -> Parser Kind
optParser cfg = Opt <$> between' (optFrom cfg) (optTo cfg)

rawParser :: Config -> Parser Kind
rawParser cfg = Raw . T.pack <$> manyTill anyChar (lookAhead $ string (reqFrom cfg) <|> string (optFrom cfg))

rawParser' :: Parser Kind
rawParser' = Raw . T.pack <$> many1 anyChar

combine :: Config -> Dict -> [Kind] -> Either String Text
combine cfg dic = foldM replace ""
    where
        wrap k = T.pack (optFrom cfg) <> k <> T.pack (optTo cfg)
        replace :: Text -> Kind -> Either String Text
        replace acc (Req k) = maybe (Left $ "There is no key in dict, such as " ++ T.unpack k) (Right . (acc <>)) $ HM.lookup k dic
        replace acc (Opt k) = Right . maybe (acc <> wrap k) (acc <>) $ HM.lookup k dic
        replace acc (Raw t) = Right $ acc <> t

data Kind = Raw Text
          | Req Text
          | Opt Text
          deriving (Show, Eq)

parseOnly :: Parser a -> Template -> Either String a
parseOnly p = bimap show id . parse p ""

between' :: String -> String -> Parser Text
between' f t = T.pack <$> between (string f) (string t) (many1 letter)
