module Main where

import RestServer

main :: IO ()
main = restServer "http://localhost:3000/order/wow23"
