! tests for subroutine prefix and suffix

pure subroutine add(A, B, C) bind(C,name="add_sub_rm")
    real, intent(in)  :: B(:), C(:)
    real, intent(out) :: A(:)
    A = B + C
end subroutine add
