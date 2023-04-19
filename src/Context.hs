module Context where
import Control.Monad.State.Lazy

type Context dom cod = [(dom,cod)]

ctxEmpty :: State (Context dom cod) Bool
ctxEmpty = get >>= \ctx -> return $ null ctx

ctxLookup :: Eq dom => dom -> State (Context dom cod) (Maybe cod)
ctxLookup key = get >>= \ctx -> return $ lookup key ctx

ctxCheck :: (Eq dom, Eq cod) => dom -> cod -> State (Context dom cod) Bool
ctxCheck key val = do
    res <- ctxLookup key
    case res of
        Nothing -> return False
        Just val' -> return $ if val' == val then True else False

ctxSingleton :: State (Context dom cod) Bool
ctxSingleton = get >>= \ctx -> return $ length ctx == 1

removeKey :: Eq dom => dom -> [(dom, cod)] -> [(dom, cod)]
removeKey _ [] = []
removeKey key ((key',_) : ls) | key == key' = removeKey key ls
removeKey key (pair : ls) = pair : removeKey key ls

ctxRemove :: Eq dom => dom -> State (Context dom cod) ()
ctxRemove key = do
    ctx <- get
    put $ removeKey key ctx