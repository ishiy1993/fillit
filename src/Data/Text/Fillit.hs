module Data.Text.Fillit
    ( Template
    , Dict
    , Config(..)
    , def
    , fill
    , fill'
    ) where

import Data.Text (Text)

import Data.Text.Fillit.Internal

-- $setup
-- >>> import qualified Data.HashMap.Lazy as HM
-- >>> :set -XOverloadedStrings
-- >>> let dic = HM.fromList [("name","Tom"),("date","2017-08-01 12:00")]

-- |
-- >>> fill "Hi, $name$" dic
-- Right "Hi, Tom"
--
-- >>> fill "$date$: %note%" dic
-- Right "2017-08-01 12:00: %note%"
--
-- >>> fill "%date% $name$" dic
-- Right "2017-08-01 12:00 Tom"
--
-- >>> fill "$name$ ($age$)" dic
-- Left ...
--

fill :: Template -> Dict -> Either String Text
fill = fill' def

-- |
-- >>> :{
--   let config = Config { reqFrom = "<<"
--                       , reqTo   = ">>"
--                       , optFrom = "{{"
--                       , optTo   = "}}"
--                       }
--   in fill' config "Hi, <<name>>" dic
-- :}
-- Right "Hi, Tom"
--

fill' :: Config -> Template -> Dict -> Either String Text
fill' cfg tmp dic = combine cfg dic =<< parseTemplate cfg tmp
