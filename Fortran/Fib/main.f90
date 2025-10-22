program fibonacci
  implicit none
  integer :: i, result

  ! Set the desired Fibonacci number to calculate
  i = 0
  do i = 0, 30, 1
      result = fib(i)
      write(*,*) "fib(", i, "): ", result
    
  end do

contains

  ! Recursive function definition
  recursive function fib(k) result(fib_val)
    integer, intent(in) :: k
    integer :: fib_val

    ! Base cases
    if (k == 0) then
      fib_val = 0
    else if (k == 1) then
      fib_val = 1
    ! Recursive step
    else
      fib_val = fib(k - 1) + fib(k - 2)
    end if

  end function fib

end program fibonacci
