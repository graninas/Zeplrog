{-# LANGUAGE DuplicateRecordFields #-}

module ZP.AI.Debug where

import ZP.Prelude

needDebug :: Bool
needDebug = False

outputDbg :: Monad m => Text -> m ()
outputDbg msg = if needDebug then traceM msg else pure ()

type Padding = Text
