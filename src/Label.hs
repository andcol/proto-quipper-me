module Label where
import Context
import Control.Monad.State

type Label = String
data WireType = Qubit | Bit deriving Eq

type LabelContext = Context Label WireType

data MValue = UnitVal | Label Label | Tuple MValue MValue
data MType = UnitType | Wire WireType | Tensor MType MType

checkMType :: MValue -> MType -> State LabelContext Bool
checkMType UnitVal UnitType = ctxEmpty
checkMType (Label l) (Wire w) = do
    matchType <- ctxCheck l w
    singleton <- ctxSingleton
    if matchType && singleton then do
        ctxRemove l
        return True
    else
        return False
checkMType (Tuple l k) (Tensor t u) = do
    lres <- checkMType l t
    kres <- checkMType k u
    empty <- ctxEmpty
    return $ if lres && kres && empty then True else False