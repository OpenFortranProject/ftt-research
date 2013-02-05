import Language.MiniFortran
import Language.MiniFortran.PrettyPrinter

g = nv "g"
l = nv "l"
gamma = nv "gamma"
t = nv "t"
tprime = nv "tprime"
func = nv "f"
theta = nv "theta"
omega = nv "omega"
k1a = nv "k1a"
k1b = nv "k1b"
k2a = nv "k2a"
k2b = nv "k2b"
k3a = nv "k3a"
k3b = nv "k3b"
k4a = nv "k4a"
k4b = nv "k4b"
dt = nv "dt"

nf :: Float -> NumericExpr
nf f = NumericVal $ Lit (show f)

nv :: Variable -> NumericExpr
nv v = NumericVal $ RefExpr (VarRef v)

nvref :: NumericExpr -> Ref
nvref (NumericVal (RefExpr r)) = r
nvref _ = error "Bad nvref"

fhelp f args = NumericVal $ Func f (map NumE args)

solve =
  let 
    l0 = (nvref tprime) :=: (NumE $ t :+: dt)
    l1 = (nvref k1a) :=: (NumE $ omega :*: dt)
    l2 = (nvref k1b) :=: (NumE $ (fhelp "func" [theta, omega, tprime, nf 1]) :*: dt)
    l3 = (nvref k2a) :=: (NumE $ (omega :+: (k1b :/: (nf 2.0))) :*: dt)
    l4 = (nvref k2b) :=: (NumE $ (fhelp "func" [theta :+: (k1a :/: (nf 2)), omega :+: (k1b :/: (nf 2)), tprime :+: (dt :/: (nf 2)), nf 1]) :*: dt)
    l5 = (nvref k3a) :=: (NumE $ (omega :+: (k2b :/: (nf 2.0))) :*: dt)
    l6 = (nvref k3b) :=: (NumE $ (fhelp "func" [theta :+: (k2a :/: (nf 2)), omega :+: (k2b :/: (nf 2)), tprime :+: (dt :/: (nf 2)), nf 1]) :*: dt)
    l7 = (nvref k4a) :=: (NumE $ (omega :+: k3b) :*: dt)
    l8 = (nvref k4b) :=: (NumE $ (fhelp "func" [theta :+: k3a, omega :+: k3b, tprime :+: dt, nf 1]) :*: dt)
    l9 = (nvref theta) :=: (NumE $ theta :+: ((nf 1) :/: (nf 6)) :*: (k1a :+: ((nf 2) :*: k2a) :+: ((nf 2) :*: k3a) :+: k4a))
    l10 = (nvref omega) :=: (NumE $ omega :+: ((nf 1) :/: (nf 6)) :*: (k1b :+: ((nf 2) :*: k2b) :+: ((nf 2) :*: k3b) :+: k4b))
  in
    [l0,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10]

fbody =
  ((NumericMinus gamma) :*: omega) :-: ((g :/: l) :*: (fhelp "sin" [theta]))
