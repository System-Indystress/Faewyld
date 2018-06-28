{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Home where

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

type Home =     "ws" :> WebSocketPending
           :<|> Get '[HTML] Markup
           :<|> Raw
           :<|> "font" :> Raw



e :: Html
e = toHtml ("" :: Text)

page :: Markup
page = html $ do
  head $ do
    title "Hello"
  body $ "Hello"


homeServer :: Server Home
homeServer =    streamData
       :<|> (return page)
       :<|> (serveDirectoryFileServer "static")
       :<|> (serveDirectoryFileServer "static/font")
  where
    streamData :: MonadIO m => PendingConnection -> m ()
    streamData pc = do
      liftIO $ putStrLn "conncting"
      c <- liftIO $ acceptRequest pc
      liftIO $ putStrLn "connected"
      liftIO $ forkPingThread c 10
      liftIO $ forkIO $ do
        msg <- receive c
        putStrLn $ show msg
      liftIO $ forM_ [1..] $ \i ->
        sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000




home :: Proxy Home
home = Proxy
