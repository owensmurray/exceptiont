{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.ExceptionT (
  ExceptionT,
  runExceptionT,
) where


import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError(catchError, throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Coerce (Coercible)
import Data.Kind (Constraint)
import UnliftIO (UnliftIO(UnliftIO), Exception, MonadUnliftIO, catch,
   throwIO, try)


{- |
  A monad transformer whose `MonadError` isntances is implemented using
  raw Haskell exceptions. This is particularly useful if you want
  your transformer stack to be an instance of both `MonadError` and
  `MonadUnliftIO`, which you can't do with `ExceptT`.
-}
newtype ExceptionT e m a = ExceptionT (m a)
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader r
    , MonadMask
    , MonadCatch
    , MonadThrow
    )
type Repr m = forall a b. (Coercible a b) => Coercible (m a) (m b) :: Constraint
deriving newtype instance (MonadIO m, MonadUnliftIO m, Repr m) => MonadUnliftIO (ExceptionT e m)
instance MonadTrans (ExceptionT e) where
  lift = ExceptionT
instance (MonadIO m, MonadUnliftIO m, Exception e, Repr m) => MonadError e (ExceptionT e m) where
  throwError = throwIO
  catchError = catch
deriving newtype instance (MonadBase b m) => MonadBase b (ExceptionT e m)
deriving newtype instance (MonadBaseControl b m) => MonadBaseControl b (ExceptionT e m)


runExceptionT
  :: ( MonadUnliftIO m
     , Exception e
     )
  => ExceptionT e m a
  -> m (Either e a)
runExceptionT (ExceptionT action) = try action


