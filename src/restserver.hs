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
import           Data.Text.Encoding
import           Control.Concurrent.Async
import qualified Control.Monad.Logger       as LT


restServer paymentGatewayUrl =  scotty 3000 $ do
        get "/order/:productId" $ LT.runStdoutLoggingT $ do
            productId <- lift $ param "productId"
            $(LT.logInfoSH) $ "Order " <> (productId::T.Text) <> " processing started"
            payResponseAsync <- liftIO $ async $ simpleHttp paymentGatewayUrl
            payResponseStream <- liftIO $ wait payResponseAsync
            payResponse <- return $ (T.unpack . decodeUtf8 . BL.toStrict) payResponseStream
            $(LT.logInfoSH) $ "Payment result " <>  payResponse
            $(LT.logInfoSH) $ "Order " <> productId <> " processing finished"
            lift $ text $ TL.pack payResponse
