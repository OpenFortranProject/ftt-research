! This file tests two forms of if-then-else constructs that should
! create the same AST nodes.  It also tests for nested
! if-then-else constructs.

if (a == 1) then
  b = 1
else if (a == 2) then
   b = 2
else
   b = 3
end if

if (a == 1) then
  b = 1
else
 if (a == 2) then
   b = 2
 else
   b = 3
 end if
end if

end

