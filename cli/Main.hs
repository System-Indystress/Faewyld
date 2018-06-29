{-#LANGUAGE OverloadedStrings#-}
module Main where

import Network.HTTP.Client
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL

adminStatus :: String -> IO String
adminStatus url = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest $ url ++ "/admin"
  let request' = request {requestHeaders = ("token","placeholder") : requestHeaders request}
  response <- httpLbs request' manager

  return $ BS.unpack $ BSL.toStrict $ responseBody response

main :: IO ()
main = adminStatus "http://localhost:8081" >>= putStrLn
