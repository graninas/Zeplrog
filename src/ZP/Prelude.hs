{-# OPTIONS -fno-warn-orphans #-}

module ZP.Prelude
  ( module X
  , liftFC
  ) where

import           Control.Concurrent           as X (ThreadId, forkIO,
                                                    killThread, threadDelay)
import           Control.Concurrent.STM       as X (retry)
import           Control.Concurrent.STM.TMVar as X (TMVar, newEmptyTMVar,
                                                    newEmptyTMVarIO, newTMVar,
                                                    newTMVarIO, putTMVar,
                                                    readTMVar, takeTMVar,
                                                    tryReadTMVar)
import           Control.Concurrent.STM.TVar  as X (modifyTVar)
import           Control.Exception            as X (SomeException (..))
import           Control.Lens                 as X (at, (.=))
import           Control.Lens.TH              as X (makeFieldsNoPrefix,
                                                    makeLenses)
import           Control.Monad                as X (liftM, unless, void, when)
import           Control.Monad.Free           as X (Free (..), foldFree, liftF)
import           Control.Monad.Free.Church    as X (F (..), foldF, fromF, iter,
                                                    iterM, retract)
import           Control.Newtype.Generics     as X (Newtype, O, pack, unpack)
import           Data.Aeson                   as X (FromJSON, ToJSON,
                                                    genericParseJSON,
                                                    genericToJSON, parseJSON,
                                                    toJSON)
import           Data.Maybe                   as X (fromJust, fromMaybe)
import           Fmt                          as X ((+|), (+||), (|+), (||+))
import           GHC.Base                     as X (until)
import           GHC.Generics                 as X (Generic)
import           Text.Read                    as X (read, readsPrec)

-- includes Data.IORef
import           Universum                    as X hiding (All, Option, Set,
                                                    Type, head, init, last, set,
                                                    tail, trace, group,
                                                    natVal, symbolVal)
import           Universum.Functor.Fmap       as X ((<<$>>))
import           Universum.Unsafe             as X (head, init, last, tail,
                                                    (!!))

import Debug.Trace as X (trace)

import qualified Control.Monad.Free.Church    as CF
import qualified Control.Monad.Free.Class     as MF

import ZP.System.TypeSelector.Granular as X

-- Lift for Church encoded Free
liftFC :: (Functor f, MF.MonadFree f m) => f a -> m a
liftFC = CF.liftF

