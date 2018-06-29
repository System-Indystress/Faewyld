{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

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

-- other endpoints
import Home
import Admin

type Game = Admin :<|> Home

game :: Proxy Game
game = Proxy

app :: Application
app = serve game $ adminServer :<|> homeServer 


main :: IO ()
main = run 8081 app
