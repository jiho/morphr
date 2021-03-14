subroutine moments_central(P, nr, nc, xbar, ybar, M, no)
  ! Input (and output) variables :
  ! nr, nc, no: bounds of indices
  ! NB: no = order + 1 = number of row/col of M
  integer, intent(in) :: nr, nc, no
  double precision, intent(in) :: xbar, ybar
  ! P: the pixel values matrix
  ! M: the moments matrix
  double precision, intent(inout) :: P(nr,nc), M(no,no)

  ! Local variables :
  integer :: i, j, o
  double precision :: X(nr,nc,no), Y(nr,nc,no)

  ! initialise X^* : powers of the 0-based index of the column
  do j=1, nc, 1
    do o=1, no, 1
      X(:,j,o) = (j-1-xbar)**(o-1)
    end do
  end do

  ! initialise Y^* : powers of the 0-based index of the row
  do i=1, nr, 1
    do o=1, no, 1
      Y(i,:,o) = (i-1-ybar)**(o-1)
    end do
  end do

  ! compute the moments
  do i=1, no, 1
    do j=1, no, 1
      M(i,j) = sum(X(:,:,i) * Y(:,:,j) * P)
      ! write (*,*) M(i,j)
    end do
  end do

end subroutine moments_central
