{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module DPTH where

import Control.Monad.Fix
import Language.Haskell.TH
import DPGen

-- このモジュールでは貰うDPのメモ化再帰自動化を目指す

fib :: ExpQ
fib = [e| fix $ \f n -> case n of
         0 -> 1
         1 -> 1
         n -> f (n - 1) + f (n - 2)
      |]
