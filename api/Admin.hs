{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Admin where

import Servant
import Servant.API
import Servant.HTML.Blaze
import Servant.Utils.StaticFiles
import Servant.API.WebSocket
import Network.WebSockets
import Data.Text hiding (head)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html5.Attributes (id,src,rel,href,type_,value,for,placeholder)
import Text.Blaze.Html5 hiding (main)
import Prelude hiding (head, (!), id, div)
import Control.Monad.IO.Class
import Control.Monad (forM_)
import Control.Concurrent (threadDelay, forkIO)
import Data.Aeson

type Admin = "admin" :> Header "token" Text :> Get '[JSON] (Maybe AStatus)


data AStatus =
  AStatus { players :: [Text]
          , places  :: [Text]
          , issues  :: [Text]
          , msgs    :: [Text]
          }
  deriving (Show, Generic)

instance FromJSON AStatus
instance ToJSON AStatus

adminStatus :: Maybe Text -> Maybe AStatus
adminStatus token
  | token == Just "placeholder" =
    Just $
      AStatus { players = ["matt"]
              , places  = ["faewyld"]
              , issues  = ["out of milk"]
              , msgs    = ["free hugs"]}
  | otherwise = Nothing


adminServer :: Server Admin
adminServer token = return $ adminStatus token

admin :: Proxy Admin
admin = Proxy
