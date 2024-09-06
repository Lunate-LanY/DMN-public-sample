module Main where

import DMN.Control.MainLoop
import DMN.Control.MainLoopMemoCycle
import DMN.Control.MainLoopForTest
import DMN.Control.MainLoopForTestHTTPClient

main :: IO ()
main = mainloop_ls
