{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module RestServer
    ( restServer
    ) where

import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Web.Scotty
import Data.Monoid ((<>))
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as CHE
import           Data.Text.Encoding
import           Control.Concurrent.Async
import qualified Control.Monad.Logger       as LT
import Network.HTTP.Client



restServer paymentGatewayUrl = do
        manager <- newManager defaultManagerSettings
        scotty 3000 $ do
          get "/orderhttp/:productId" $ LT.runStdoutLoggingT $ do
              productId <- lift $ param "productId"
              $(LT.logInfoSH) $ "Order " <> (productId::T.Text) <> " processing started"
              -- payResponse <- liftIO $ WQ.get paymentGatewayUrl
              -- payResponse <- return $ CHE.unpack $ responseBody payResponse
              payResponse <- liftIO $ do
                      request <- parseRequest paymentGatewayUrl
                      let req = request { method = "GET", redirectCount = 0}
                      a <- withResponse req manager $ responseBody
                      return $! a
              payResponse <- return $ T.unpack . decodeUtf8 $ payResponse
              $(LT.logInfoSH) $ "Payment result " <>  payResponse
              $(LT.logInfoSH) $ "Order " <> productId <> " processing finished"
              lift $ text $ TL.pack $ payResponse <> " with http"
          get "/order/:productId" $ LT.runStdoutLoggingT $ do
              productId <- lift $ param "productId"
              $(LT.logInfoSH) $ "Order " <> (productId::T.Text) <> " processing started"
              $(LT.logInfoSH) $ "Order " <> productId <> " processing finished"
              lift $ text $ TL.pack "Done"
