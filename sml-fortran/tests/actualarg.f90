! forces test of actual-arg-list by using argument keywords

real, dimension(10,100) :: Var, lVar

lVar = eoshift(Var, SHIFT=-1, BOUNDARY=zero, DIM=id)
end
