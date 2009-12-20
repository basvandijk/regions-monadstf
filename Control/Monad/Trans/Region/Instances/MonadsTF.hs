{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region.Instances.MonadsTF
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides instances for the monads-tf classes for 'RegionT's.
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.Instances.MonadsTF where

-- from monads-fd:
-- TODO: import Control.Monad.Cont.Class   ( MonadCont, callCC )
import Control.Monad.Error.Class  ( MonadError, ErrorType, throwError, catchError )
import Control.Monad.RWS.Class    ( MonadRWS )
import Control.Monad.Reader.Class ( MonadReader, EnvType, ask, local )
import Control.Monad.State.Class  ( MonadState, StateType, get, put )
import Control.Monad.Writer.Class ( MonadWriter, WriterType, tell, listen, pass )

-- from transformers:
import Control.Monad.Trans ( lift )

-- from unicode-symbols:
import Prelude.Unicode ( (∘) )

-- from regions:
import Control.Monad.Trans.Region.Internal ( RegionT
                                           , liftCatch
                                           , mapRegionT
                                           -- TODO: , liftCallCC
                                           )

-- TODO:
-- instance Monad pr ⇒ MonadCont (RegionT resource s pr) where
--     callCC = liftCallCC callCC

instance MonadError pr ⇒ MonadError (RegionT resource s pr) where

    type ErrorType (RegionT resource s pr) = ErrorType pr

    throwError = lift ∘ throwError
    catchError = liftCatch catchError

instance MonadRWS pr ⇒ MonadRWS (RegionT resource s pr)

instance MonadReader pr ⇒ MonadReader (RegionT resource s pr) where
    type EnvType (RegionT resource s pr) = EnvType pr

    ask   = lift ask
    local = mapRegionT ∘ local

instance MonadWriter pr ⇒ MonadWriter (RegionT resource s pr) where
    type WriterType (RegionT resource s pr) = WriterType pr

    tell   = lift ∘ tell
    listen = mapRegionT listen
    pass   = mapRegionT pass

instance MonadState pr ⇒ MonadState (RegionT resource s pr) where
    type StateType (RegionT resource s pr) = StateType pr

    get = lift get
    put = lift ∘ put
