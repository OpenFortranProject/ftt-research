! tests function prefix and suffix

pure real function add(a,b) result(s) bind(C,name="add_rm")
   real, intent(in) :: a, b

   s = a + b

end function add
