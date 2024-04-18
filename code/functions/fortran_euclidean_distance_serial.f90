subroutine fortran_euclidean_distance_serial(mat, nrow, ncol, out)
      use, intrinsic :: iso_c_binding
      implicit none

! Arguments
      integer, intent(in) :: nrow, ncol
      double precision, intent(in) :: mat(nrow, ncol)
      double precision, intent(out) :: out(nrow, nrow)

! Local variables
      integer :: i, j, k
      double precision :: sum_sq_diff, diff

      do i = 1, nrow
        do j = i, nrow
          if (i == j) then
            out(i, j) = 0.0d0
          else
            sum_sq_diff = 0.0d0
            do k = 1, ncol
              diff = mat(i, k) - mat(j, k)
              sum_sq_diff = sum_sq_diff + diff**2
            end do
            out(i, j) = sqrt(sum_sq_diff)
            out(j, i) = out(i, j)
          end if
        end do
      end do
end subroutine fortran_euclidean_distance_serial
