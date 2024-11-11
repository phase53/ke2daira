module subprogs
    implicit none
contains
    function ke2daira(c1, c2, i, j, k1, k2) result(c)
        character(*),intent(in) :: c1, c2
	character(99) :: c(1:2)
	integer :: i, j
        integer :: k1, k2
	
	c(1) = c1(1:i-1) // c2(j:k2) // c1(k1+1:len(c1))
	c(2) = c2(1:j-1) // c1(i:k1) // c2(k2+1:len(c2))
    end function ke2daira
end module subprogs

program main
    use subprogs
    implicit none
    character(20) :: c1, c2
    character(20) :: c(1:2)
    integer :: k, i, j, k1, k2
     

    write(*,*) 'input phrase 1 (<= 20 letters):'
    read(*,*) c1
    write(*,*) 'input phrase 2 (<= 20 letters):'
    read(*,*) c2
    
    write(*,*) 'phrase 1 FROM?:'
    read(*,*) i
    write(*,*) 'phrase 2 FROM:'
    read(*,*) j

    write(*,*) 'phrase 1 TO?:'
    read(*,*) k1
    write(*,*) 'phrase 2 TO:'
    read(*,*) k2


    c = ke2daira(c1, c2, i, j, k1, k2)

    do k = 1, 2
        write(*,*) c(k)
    enddo
end program main
