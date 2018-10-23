module Main where

import UrlParams
import JsonOutput
import BasicAuth
import ServiceStatic as SS

main :: IO ()
main = do 
        SS.mainFn
        
