module Circuit where
import Label

data Gate
    = Id
    | Hadamard
    | Cnot

data Circuit = Gate Gate MValue MValue | Seq Circuit Gate MValue MValue

checkSignature :: Circuit -> MType -> MType -> Bool
checkSignature (Gate g l k) t u = undefined
checkSignature (Seq c g l k) t u = undefined