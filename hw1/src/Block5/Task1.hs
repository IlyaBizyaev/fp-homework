module Block5.Task1
  ( Expr(..)
  , ArithmeticError(..)
  , eval
  )
where

-- | A recursive type to represent arithmetic exrpressions.
data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show, Eq)

-- | A helper type to store and display arithmetic errors.
data ArithmeticError = ArithmeticError Int Int String deriving (Eq)

instance Show ArithmeticError where
  show (ArithmeticError x y opStr) =
    "Invalid operation: " ++ show x ++ opStr ++ show y

-- | Generalised application of binary operand to Int with error.
applyBinOp :: (Int -> Int -> b) -> Expr -> Expr -> Either ArithmeticError b
applyBinOp op x y = do
  x' <- eval x
  y' <- eval y
  return (x' `op` y')

-- | Evaluates an arithmetic expression.
eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Add x y) = applyBinOp (+) x y
eval (Sub x y) = applyBinOp (-) x y
eval (Mul x y) = applyBinOp (*) x y
eval (Div x y) = do
  x' <- eval x
  y' <- eval y
  if y' == 0 then Left $ ArithmeticError x' y' "/" else return (x' `div` y')
eval (Pow x y) = do
  x' <- eval x
  y' <- eval y
  if y' < 0 then Left $ ArithmeticError x' y' "^" else return (x' ^ y')
