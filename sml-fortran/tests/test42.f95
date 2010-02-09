program foo
  
  type mytype
     integer  i
  end type mytype
  
  type (mytype) X(3)
  
  X(1)% i = 3
  
end
