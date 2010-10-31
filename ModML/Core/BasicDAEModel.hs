{-#LANGUAGE NoMonomorphismRestriction,DeriveDataTypeable,MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies #-}
module ModML.Core.BasicDAEModel
where

import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Trans as M
import qualified Data.Data as D
import qualified Data.TypeHash as D
import qualified Data.Map as M

newtype RealVariable = RealVariable Int deriving (Eq, Ord, D.Typeable, D.Data)

variableId (RealVariable a) = a
instance Show RealVariable
    where
      showsPrec _ v = showString "Variable_" . shows (variableId v)

data RealEquation = RealEquation RealExpression RealExpression deriving (Eq, Ord, D.Typeable, D.Data, Show)

data BoolExpression =
    -- A constant true or false.
    BoolConstant Bool |
    -- A common subexpression tagged expression.
    BoolCommonSubexpressionE BoolCommonSubexpression |
    -- Logical and of two expressions.
    And BoolExpression BoolExpression |
    -- Logical not of an expression.
    Not BoolExpression |
    -- Logical or of two expressions.
    Or BoolExpression BoolExpression |
    LessThan RealExpression RealExpression |
    Equal RealExpression RealExpression
                   deriving (Eq, Ord, D.Typeable, D.Data, Show)

data RealExpression =
    -- Any real value, as a constant.
    RealConstant Double |
    -- A free variable...
    RealVariableE RealVariable |
    -- The bound variable of the integration...
    BoundVariableE |
    -- The derivative of an expression with respect to the bound variable...
    Derivative RealExpression |
    -- A common subexpression tagged expression.
    RealCommonSubexpressionE RealCommonSubexpression |
    -- If x {- then -} b {- else -} b
    If BoolExpression RealExpression RealExpression |
    -- A sum of two expressions.
    Plus RealExpression RealExpression |
    -- First expression minus second expression.
    Minus RealExpression RealExpression |
    -- The product of two expressions.
    Times RealExpression RealExpression |
    -- a `Divided` {- by -} b
    Divided RealExpression RealExpression |
    -- a `Power` b - a to the power of b.
    Power RealExpression RealExpression |
    -- The floor function...
    Floor RealExpression |
    -- The ceiling function...
    Ceiling RealExpression |
    -- LogBase a b = log_a b
    LogBase RealExpression RealExpression |
    -- Trigonometric functions...
    Sin RealExpression |
    Tan RealExpression |
    Cos RealExpression |
    ASin RealExpression |
    ATan RealExpression |
    ACos RealExpression |
    Sinh RealExpression |
    Tanh RealExpression |
    Cosh RealExpression |
    ASinh RealExpression |
    ATanh RealExpression |
    ACosh RealExpression |
    RealExpressionTag String RealExpression
          deriving (Eq, Ord, D.Typeable, D.Data, Show)

class (Ord a) => CommonSubexpression a
  where
    commonSubexpressionId :: a -> Int

data RealCommonSubexpression = RealCommonSubexpression Int RealExpression deriving (Eq, Ord, D.Typeable, D.Data, Show)
instance CommonSubexpression RealCommonSubexpression
  where
    commonSubexpressionId (RealCommonSubexpression a _) = a

data BoolCommonSubexpression = BoolCommonSubexpression Int BoolExpression deriving (Eq, Ord, D.Typeable, D.Data, Show)
instance CommonSubexpression BoolCommonSubexpression
  where
    commonSubexpressionId (BoolCommonSubexpression a _) = a

data AnyCommonSubexpression = FromRealCommonSubexpression RealCommonSubexpression | FromBoolCommonSubexpression BoolCommonSubexpression deriving (Eq, Ord, D.Typeable, D.Data, Show)
instance CommonSubexpression AnyCommonSubexpression
  where
    commonSubexpressionId (FromRealCommonSubexpression r) = commonSubexpressionId r
    commonSubexpressionId (FromBoolCommonSubexpression b) = commonSubexpressionId b

data BasicDAEModel = BasicDAEModel {
        -- The equations which apply for this model at all times...
        equations :: [RealEquation],
        -- The boundary conditions (equations which apply at the starting
        -- point, and/or after interventions). Note that other equations also
        -- apply at these times as well.
        boundaryEquations :: [(BoolExpression, RealEquation)],
        -- Expressions which controls when the solver needs to be restarted.
        -- The values should cross zero at the point when the solver needs to
        -- be restarted.
        interventionRoots :: [RealExpression],
        -- An expression which the solver should try to keep positive.
        forcedInequalities :: [RealExpression],
        checkedConditions :: [(String, BoolExpression)],
        variables :: [RealVariable],
        commonSubexpressions :: [AnyCommonSubexpression],
        annotations :: M.Map (String, String) String,
        contextTaggedIDs :: M.Map (D.TypeCode, D.TypeCode) Int,
        nextID :: Int
    } deriving (Eq, Ord, D.Typeable, D.Data, Show)
nullModel = BasicDAEModel { equations = [], boundaryEquations = [], interventionRoots = [],
                            forcedInequalities = [], checkedConditions = [], variables = [],
                            commonSubexpressions = [], annotations = M.empty,
                            contextTaggedIDs = M.empty, nextID = 0
                          }

newtype ModelBuilderT m a = ModelBuilderT (S.StateT BasicDAEModel m a)
type ModelBuilder a = ModelBuilderT I.Identity a

modelBuilderTToState (ModelBuilderT a) = a

instance Monad m => Monad (ModelBuilderT m)
    where
      (ModelBuilderT a) >>= b = ModelBuilderT $ a >>= (modelBuilderTToState . b)
      return a = ModelBuilderT (return a)
      fail a = ModelBuilderT (fail a)
instance M.MonadTrans ModelBuilderT
    where
      lift a = ModelBuilderT $ M.lift a
instance Monad m => S.MonadState BasicDAEModel (ModelBuilderT m)
    where
      get = ModelBuilderT $ S.get
      put = ModelBuilderT . S.put

class BasicModelBuilderAccess m m1 | m -> m1
    where
      liftBasicModelBuilder :: ModelBuilderT m1 a -> m a
instance BasicModelBuilderAccess (ModelBuilderT m1) m1
    where
      liftBasicModelBuilder = id

buildModelT :: Monad m => ModelBuilderT m a -> m BasicDAEModel
buildModelT x = S.execStateT (modelBuilderTToState x) nullModel
buildModel :: ModelBuilder a -> BasicDAEModel
buildModel = I.runIdentity . buildModelT

evalModelT :: Monad m => ModelBuilderT m a -> m a
evalModelT x = S.evalStateT (modelBuilderTToState x) nullModel
evalModel = I.runIdentity . evalModelT

x `newEqM` y = S.modify (\m -> m { equations = (RealEquation x y):(equations m) })
x `newEqX` y = do
  x' <- x
  y' <- y
  x' `newEqM` y'
newEq = newEqX

newBoundaryEqM c x y = S.modify (\m -> m { boundaryEquations = (c, (RealEquation x y)):(boundaryEquations m) })
newBoundaryEqX c x y = do
  c' <- c
  x' <- x
  y' <- y
  newBoundaryEqM c' x' y'
newBoundaryEq = newBoundaryEqX

newInterventionRootM x = S.modify (\m -> m { interventionRoots = x:(interventionRoots m)})
newInterventionRootX x =
    do
      x' <- x
      newInterventionRootM x'
newInterventionRoot = newInterventionRootX

newForcedInequalityM x = S.modify (\m -> m { forcedInequalities = x:(forcedInequalities m)})
newForcedInequalityX x =
    do
      x' <- x
      newForcedInequalityM x'
newForcedInequality = newForcedInequalityX

msg `newCheckedConditionM` x = S.modify (\m -> m { checkedConditions = (msg, x):(checkedConditions m) })
m `newCheckedConditionX` x = do
  x' <- x
  m `newCheckedConditionM` x'
newCheckedCondition = newCheckedConditionX

annotateModel :: (Show a, Show b, Show c, Monad m) => a -> b -> c -> ModelBuilderT m ()
annotateModel s p o = S.modify (\m -> m { annotations = M.insert ((show s), (show p)) (show o) (annotations m) })

getAnnotation :: (Show a, Show b, Monad m) => a -> b -> ModelBuilderT m (Maybe String)
getAnnotation s p = do
  am <- S.gets annotations
  return $ M.lookup (show s, show p) am

registerCommonSubexpression s =
    S.modify (\m -> m { commonSubexpressions = s:(commonSubexpressions m)})

allocateID :: Monad m => ModelBuilderT m Int
allocateID = S.modify (\m -> m { nextID = (+1) $ nextID m}) >> (S.gets $ flip (-) 1 . nextID)

boolConstantM :: Monad m => Bool -> ModelBuilderT m BoolExpression
boolConstantM = return . BoolConstant

trueM :: Monad m => ModelBuilderT m BoolExpression
trueM = boolConstantM True
falseM :: Monad m => ModelBuilderT m BoolExpression
falseM = boolConstantM False

boolCommonSubexpressionM :: Monad m => BoolExpression -> ModelBuilderT m BoolExpression
boolCommonSubexpressionM e =
  do
    id <- allocateID
    let bcs = BoolCommonSubexpression id e
    registerCommonSubexpression (FromBoolCommonSubexpression bcs)
    return $ BoolCommonSubexpressionE bcs

boolCommonSubexpressionX e =
    e >>= boolCommonSubexpressionM

boolCommonSubexpression me = do
  ex <- boolCommonSubexpressionX me
  return (return ex)

andM :: Monad m => BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
a `andM` b = return $ a `And` b
andX :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
andX = S.liftM2 And
(.&&.) = andX

orM :: Monad m => BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
a `orM` b = return $ a `Or` b
orX :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
orX = S.liftM2 Or
(.||.) = orX

notM :: Monad m => BoolExpression -> ModelBuilderT m BoolExpression
notM = return . Not
notX :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
notX = S.liftM Not

lessThanM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `lessThanM` b = return $ a `LessThan` b
lessThanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `lessThanX` b = do
  a' <- a
  b' <- b
  a' `lessThanM` b'
(.<.) = lessThanX

lessEqualM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `lessEqualM` b = do
  aex <- realCommonSubexpressionM a
  bex <- realCommonSubexpressionM b
  (aex `lessThanM` bex) .||. (aex `equalM` bex)
lessEqualX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `lessEqualX` b = do
  a' <- a
  b' <- b
  a' `lessEqualM` b'
(.<=.) = lessEqualX

greaterThanM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `greaterThanM` b = notX $ a `lessEqualM` b
greaterThanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `greaterThanX` b = notX $ a `lessEqualX` b
(.>.) = greaterThanX

greaterEqualM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `greaterEqualM` b = notX $ a `lessThanM` b
greaterEqualX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `greaterEqualX` b = notX $ a `lessThanX` b
(.>=.) = greaterEqualX

equalM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `equalM` b = return $ a `Equal` b
equalX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `equalX` b = do
  a' <- a
  b' <- b
  a' `equalM` b'
(.==.) = equalX

realConstantM :: Monad m => Double -> ModelBuilderT m RealExpression
realConstantM = return . RealConstant
realConstant = realConstantM

realCommonSubexpressionM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
realCommonSubexpressionM e =
  do
    id <- allocateID
    let rcs = RealCommonSubexpression id e
    registerCommonSubexpression (FromRealCommonSubexpression rcs)
    return $ RealCommonSubexpressionE rcs
realCommonSubexpressionX me = me >>= realCommonSubexpressionM

realCommonSubexpression me = do
  ex <- realCommonSubexpressionX me
  return (return ex)


mkNewRealVariable :: Monad m => ModelBuilderT m RealVariable
mkNewRealVariable = do
  id <- allocateID
  let v = RealVariable id
  S.modify (\m -> m { variables = v:(variables m) } )
  return v

mkNewNamedRealVariable name = do
  v <- mkNewRealVariable
  annotateModel v "nameIs" name
  return v

mkNewRealVariableM :: Monad m => ModelBuilderT m (ModelBuilderT m RealVariable)
mkNewRealVariableM = S.liftM return mkNewRealVariable

realVariableM :: Monad m => RealVariable -> ModelBuilderT m RealExpression
realVariableM = return . RealVariableE 
realVariableX :: Monad m => ModelBuilderT m RealVariable -> ModelBuilderT m RealExpression
realVariableX = S.liftM RealVariableE
realVariable = realVariableX

newRealVariableE = realVariable (mkNewRealVariable)
newNamedRealVariableE name = realVariable (mkNewNamedRealVariable name)
newRealVariable :: Monad m => ModelBuilderT m (ModelBuilderT m RealExpression)
newRealVariable = do
  v <- newRealVariableE
  return (return v)

newNamedRealVariable name = do
  v <- newNamedRealVariableE name
  return (return v)

boundVariableM :: Monad m => ModelBuilderT m RealExpression
boundVariableM = return $ BoundVariableE
boundVariableX = boundVariableM
boundVariable = boundVariableM

derivativeM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
derivativeM = return . Derivative
derivativeX = S.liftM Derivative
derivative = derivativeX

ifM :: Monad m => BoolExpression -> RealExpression ->
                      RealExpression -> ModelBuilderT m RealExpression
ifM c {- then -} e1 {- else -} e2 = return $ If c e1 e2
ifX :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m RealExpression ->
                     ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
ifX = S.liftM3 If

plusM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `plusM` b = return $ a `Plus` b
plusX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
plusX = S.liftM2 Plus
(.+.) = plusX

minusM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `minusM` b = return $ a `Minus` b
minusX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
minusX = S.liftM2 Minus
(.-.) = minusX

timesM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `timesM` b = return $ a `Times` b
timesX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
timesX = S.liftM2 Times
(.*.) = timesX

dividedM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `dividedM` b = return $ a `Divided` b
dividedX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
dividedX = S.liftM2 Divided
(./.) = dividedX

powerM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `powerM` b = return $ a `Power` b
powerX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
powerX = S.liftM2 Power
(.**.) = powerX

infixr 2  .||.
infixr 3  .&&.
infix  4  .==., .<., .>., .<=., .>=.
infixl 6 .+., .-.
infixl 7 .*.
infixl 7 ./.
infixl 8 .**.

logBaseM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `logBaseM` b = return $ a `LogBase` b
logBaseX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
logBaseX = S.liftM2 LogBase

floorM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
floorM = return . Floor
floorX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
floorX = S.liftM Floor
ceilingM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
ceilingM = return . Ceiling
ceilingX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
ceilingX = S.liftM Floor
sinM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
sinM = return . Sin
sinX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
sinX = S.liftM Sin
tanM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
tanM = return . Tan
tanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
tanX = S.liftM Tan
cosM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
cosM = return . Cos
cosX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
cosX = S.liftM Cos
asinM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
asinM = return . ASin
asinX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
asinX = S.liftM ASin
atanM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
atanM = return . ATan
atanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
atanX = S.liftM ATan
acosM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
acosM = return . ACos
acosX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
acosX = S.liftM ACos
sinhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
sinhM = return . Sinh
sinhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
sinhX = S.liftM Sinh
tanhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
tanhM = return . Tanh
tanhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
tanhX = S.liftM Tanh
coshM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
coshM = return . Cosh
coshX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
coshX = S.liftM Cosh
asinhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
asinhM = return . ASinh
asinhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
asinhX = S.liftM ASinh
atanhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
atanhM = return . ATanh
atanhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
atanhX = S.liftM ATanh
acoshM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
acoshM = return . ACosh
acoshX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
acoshX = S.liftM ACosh
realExpressionTagM :: Monad m => String -> RealExpression -> ModelBuilderT m RealExpression
realExpressionTagM s = return . RealExpressionTag s
realExpressionTagX :: Monad m => String -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
realExpressionTagX s = S.liftM $ RealExpressionTag s
realExpressionTag = realExpressionTagX

-- Now define some constants...

-- The constant pi.
piM :: Monad m => ModelBuilderT m RealExpression
piM = realConstantM pi
piX = piM

-- The constant e.
econstantE = RealConstant (exp 1.0)
econstant :: Monad m => ModelBuilderT m RealExpression
econstant = realConstantM (exp 1.0)
econstantM = econstant
econstantX = econstant

-- Now some functions which are simple applications of existing functions

-- The exp function, made using .**.
expE = (econstantE `Power`)
expM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
expM = return . expE
expX m = m >>= expM

-- The negative of the expression.
negateE = ((RealConstant (-1.0)) `Times`)
negateM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
negateM = return . negateE
negateX m = m >>= negateM

-- The square root of an expression.
sqrtE x = x `Power` (RealConstant 0.5)
sqrtM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
sqrtM = return . sqrtE
sqrtX m = m >>= sqrtM

-- The log base e of an expression.
logE = LogBase econstantE
logM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
logM x = return $ logE x
logX m = m >>= logM

-- Now some more complex functions that only have M and X forms...
boolIfM :: Monad m => BoolExpression -> BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
boolIfM cond x1 x2 =
    do
      condc <- boolCommonSubexpressionM cond
      (condc `andM` x1) .||. ((Not condc) `andM` x2)
boolIfX mcond mx1 mx2 =
    do
      cond <- mcond
      x1 <- mx1
      x2 <- mx2
      boolIfM

xorM :: Monad m => BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
xorM x1 x2 =
    do
      x1c <- boolCommonSubexpressionM x1
      (x1c `andM` (Not x2)) .||. ((Not x1c) `andM` x2)
xorX mx1 mx2 =
    do
      x1 <- mx1
      x2 <- mx2
      xorM x1 x2

initialValueM bv v iv =
  newBoundaryEq (realConstant bv .==. boundVariable) (return v) (realConstant iv)

initialValueX bv v iv =
  do
    v' <- v
    initialValueM bv v' iv
initialValue = initialValueX

insertContextTag typetag tag v = S.modify (\m -> m {contextTaggedIDs = M.insert (typetag, tag) v (contextTaggedIDs m)})

getContextTag :: Monad m => D.TypeCode -> D.TypeCode -> ModelBuilderT m (Maybe Int)
getContextTag typetag tag = do
  idmap <- S.gets contextTaggedIDs
  return $ M.lookup (typetag, tag) idmap
contextTaggedID typetag tag wrap allocm =
    do
      t <- getContextTag typetag tag
      case t
        of
          Just id -> return $ wrap id
          Nothing ->
            do
              id <- allocateID
              allocm id
              insertContextTag typetag tag id
              return $ wrap id

data RealCSEContextTag = RealCSEContextTag deriving (D.Typeable, D.Data)
realCSEContextTag = D.typeCode RealCSEContextTag
data BoolCSEContextTag = BoolCSEContextTag deriving (D.Typeable, D.Data)
boolCSEContextTag = D.typeCode BoolCSEContextTag
data RealVariableContextTag = RealVariableContextTag deriving (D.Typeable, D.Data)
realVariableContextTag = D.typeCode RealVariableContextTag

contextBoolCommonSubexpressionM :: Monad m => D.TypeCode -> BoolExpression -> ModelBuilderT m BoolExpression
contextBoolCommonSubexpressionM tag e =
  contextTaggedID boolCSEContextTag tag (BoolCommonSubexpressionE . flip BoolCommonSubexpression e) $
    \id ->
        do
          let bcs = BoolCommonSubexpression id e
          registerCommonSubexpression (FromBoolCommonSubexpression bcs)
contextBoolCommonSubexpressionX tag e =
    e >>= contextBoolCommonSubexpressionM tag
contextBoolCommonSubexpression tag me = do
  ex <- contextBoolCommonSubexpressionX tag me
  return (return ex)

contextRealCommonSubexpressionM :: Monad m => D.TypeCode -> RealExpression -> ModelBuilderT m RealExpression
contextRealCommonSubexpressionM tag e =
  contextTaggedID realCSEContextTag tag (RealCommonSubexpressionE . flip RealCommonSubexpression e) $
    \id ->
        do
          let bcs = RealCommonSubexpression id e
          registerCommonSubexpression (FromRealCommonSubexpression bcs)

contextRealCommonSubexpressionX tag me = me >>= contextRealCommonSubexpressionM tag
contextRealCommonSubexpression tag me = do
  ex <- contextRealCommonSubexpressionX tag me
  return (return ex)

contextMkNewRealVariable :: Monad m => D.TypeCode -> ModelBuilderT m RealVariable
contextMkNewRealVariable tag =
    contextTaggedID realVariableContextTag tag RealVariable $
      \id -> S.modify (\m -> m { variables = (RealVariable id):(variables m) } )
contextMkNewRealVariableM :: Monad m => D.TypeCode -> ModelBuilderT m (ModelBuilderT m RealVariable)
contextMkNewRealVariableM tag = S.liftM return (contextMkNewRealVariable tag)

tryEvaluateRealAsConstant (RealConstant v) = Just v
tryEvaluateRealAsConstant (RealVariableE _) = Nothing
tryEvaluateRealAsConstant (BoundVariableE) = Nothing
tryEvaluateRealAsConstant (Derivative ex) =
    case (tryEvaluateRealAsConstant ex)
    of
      Nothing -> Nothing -- We could do better by numeric differentiation.
      Just _ -> Just 0
tryEvaluateRealAsConstant (RealCommonSubexpressionE (RealCommonSubexpression _ ex)) =
    tryEvaluateRealAsConstant ex
tryEvaluateRealAsConstant (If be re1 re2) =
    case tryEvaluateBoolAsConstant be
    of
      Nothing -> Nothing
      Just True ->
          tryEvaluateRealAsConstant re1
      _ -> tryEvaluateRealAsConstant re2
tryEvaluateRealAsConstant (re1 `Plus` re2) =
    case (tryEvaluateRealAsConstant re1, tryEvaluateRealAsConstant re2)
    of
      (Just r1, Just r2) -> Just $ r1 + r2
      _ -> Nothing
tryEvaluateRealAsConstant (re1 `Minus` re2) =
    case (tryEvaluateRealAsConstant re1, tryEvaluateRealAsConstant re2)
    of
      (Just r1, Just r2) -> Just $ r1 - r2
      _ -> Nothing
tryEvaluateRealAsConstant (re1 `Times` re2) =
    case (tryEvaluateRealAsConstant re1, tryEvaluateRealAsConstant re2)
    of
      (Just r1, Just r2) -> Just $ r1 * r2
      _ -> Nothing
tryEvaluateRealAsConstant (re1 `Divided` re2) =
    case (tryEvaluateRealAsConstant re1, tryEvaluateRealAsConstant re2)
    of
      (Just r1, Just r2) -> Just $ r1 / r2
      _ -> Nothing
tryEvaluateRealAsConstant (re1 `Power` re2) =
    case (tryEvaluateRealAsConstant re1, tryEvaluateRealAsConstant re2)
    of
      (Just r1, Just r2) -> Just $ r1 ** r2
      _ -> Nothing
tryEvaluateRealAsConstant (Floor re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ fromIntegral $ floor r
      _ -> Nothing
tryEvaluateRealAsConstant (Ceiling re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ fromIntegral $ ceiling r
      _ -> Nothing
tryEvaluateRealAsConstant (LogBase re1 re2) =
    case (tryEvaluateRealAsConstant re1, tryEvaluateRealAsConstant re2)
    of
      (Just r1, Just r2) -> Just $ r1 ** r2
      _ -> Nothing
tryEvaluateRealAsConstant(Sin re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ sin r
      _ -> Nothing
tryEvaluateRealAsConstant(Tan re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ tan r
      _ -> Nothing
tryEvaluateRealAsConstant(Cos re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ cos r
      _ -> Nothing
tryEvaluateRealAsConstant(ASin re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ asin r
      _ -> Nothing
tryEvaluateRealAsConstant(ATan re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ atan r
      _ -> Nothing
tryEvaluateRealAsConstant(ACos re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ acos r
      _ -> Nothing
tryEvaluateRealAsConstant(Sinh re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ sinh r
      _ -> Nothing
tryEvaluateRealAsConstant(Tanh re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ tanh r
      _ -> Nothing
tryEvaluateRealAsConstant(Cosh re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ cosh r
      _ -> Nothing
tryEvaluateRealAsConstant(ASinh re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ asinh r
      _ -> Nothing
tryEvaluateRealAsConstant(ATanh re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ atanh r
      _ -> Nothing
tryEvaluateRealAsConstant(ACosh re) =
    case (tryEvaluateRealAsConstant re)
    of
      Just r -> Just $ acosh r
      _ -> Nothing
tryEvaluateRealAsConstant (RealExpressionTag _ re) =
    tryEvaluateRealAsConstant re

tryEvaluateBoolAsConstant (BoolConstant b) = Just b
tryEvaluateBoolAsConstant (BoolCommonSubexpressionE (BoolCommonSubexpression _ ex)) =
    tryEvaluateBoolAsConstant ex
tryEvaluateBoolAsConstant (ex1 `And` ex2) =
    case (tryEvaluateBoolAsConstant ex1, tryEvaluateBoolAsConstant ex2)
    of
      (Just b1, Just b2) -> Just $ b1 && b2
      _ -> Nothing
tryEvaluateBoolAsConstant (Not ex) =
    case (tryEvaluateBoolAsConstant ex)
    of
      Just b -> Just $ not b
      _ -> Nothing
tryEvaluateBoolAsConstant (ex1 `Or` ex2) =
    case (tryEvaluateBoolAsConstant ex1, tryEvaluateBoolAsConstant ex2)
    of
      (Just b1, Just b2) -> Just $ b1 || b2
      _ -> Nothing
tryEvaluateBoolAsConstant (ex1 `LessThan` ex2) =
    case (tryEvaluateRealAsConstant ex1, tryEvaluateRealAsConstant ex2)
    of
      (Just r1, Just r2) -> Just $ r1 < r2
      _ -> Nothing
tryEvaluateBoolAsConstant (ex1 `Equal` ex2) =
    case (tryEvaluateRealAsConstant ex1, tryEvaluateRealAsConstant ex2)
    of
      (Just r1, Just r2) -> Just $ r1 == r2
      _ -> Nothing
