module Env where

import qualified Data.IORef as IORef
import Data.IORef (IORef)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Control.Monad (liftM)

import Types
import Errors
import qualified Builtins

nullEnv :: IO Env
nullEnv = IORef.newIORef []


primitiveBindings :: IO Env
primitiveBindings =
   nullEnv >>= (flip bindVars $ map makePrimitiveFunc Builtins.builtins)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

isBound :: Env -> String -> IO Bool
isBound envRef var =
  IORef.readIORef envRef >>=
    return . maybe False (const True) . lookup var


getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ IORef.readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . IORef.readIORef)
                             (lookup var env)


setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ IORef.readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip IORef.writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- IORef.newIORef value
             env <- IORef.readIORef envRef
             IORef.writeIORef envRef ((var, valueRef) : env)
             return value


bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
       IORef.readIORef envRef
   >>= extendEnv bindings
   >>= IORef.newIORef
   where extendEnv bindings env  = liftM (++ env) (mapM addBinding bindings)
         addBinding (var, value) = do ref <- IORef.newIORef value
                                      return (var, ref)
