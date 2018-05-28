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
import Text.Blaze.Html5.Attributes (id,src,rel,href,type_,value,for)
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
  body $ do
    link ! rel "stylesheet" ! href "gsc.css"
    div ! id "wrapper" $ do
      div ! id "header" $ do
        img ! id "profileBtn" ! src "profile.png"
        button ! id "searchBtn" $ "Search"
        input ! id "search"
        select ! id "filter" $ do
          option ! value "FAll"     $ "All Events + Info"
          option ! value "FEvents"  $ "All Events"
          option ! value "FCurrent" $ "Upcoming Events Only"
          option ! value "FInfo"    $ "Info Only"
        h2 ! id "headerText" $ do "Tufts GSC"

      div ! id "panel" $ do
        div ! id "nav" $ do
          ul $ do
            li ! id "homeNav" $ "What's Happening"
            li ! id "eBoardNav" $ "eBoard"
            li ! id "committeeNav" $ "Committees"
            li ! id "gsoNav" $ "Graduate Student Organizations"
        div ! id "content" $ do
          img ! id "gscLogo" ! src "GSC.png"
          div ! id "modal" $ do
            div ! id "modalBackground" $ e
            img ! id "closeModal" ! src "close.png"
      div ! id "footer" $ "Footer"
    script ! src "https://code.jquery.com/jquery-3.3.1.min.js" $ e
    script ! src "gsc.js" $ e


server :: Server Home
server = streamData :<|> (return page) :<|> (serveDirectoryFileServer "static") :<|> (serveDirectoryFileServer "static/font")
  where
    streamData :: MonadIO m => PendingConnection -> m ()
    streamData pc = do
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

app :: Application
app = serve home server


main :: IO ()
main = run 8081 app
