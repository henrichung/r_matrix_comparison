subroutine fortran_euclidean_distance_parallel(mat, nrow, ncol, num_threads, out)
! Use OpenMP for parallel processing
      use, intrinsic :: iso_c_binding
      use, intrinsic :: omp_lib, only : omp_set_num_threads, omp_get_max_threads
      implicit none

! Arguments
      integer, intent(in) :: nrow, ncol, num_threads
      double precision, intent(in) :: mat(nrow, ncol)
      double precision, intent(out) :: out(nrow, nrow)

! Local variables
      integer :: i, j, k
      double precision :: sum_sq_diff, diff

! Set the number of threads for OpenMP
      call omp_set_num_threads(num_threads)
! Optionally print the number of threads
    ! print *, 'Number of threads: ', omp_get_max_threads()
! Parallel loop
!$OMP PARALLEL DO PRIVATE(i, j, k, sum_sq_diff, diff) SCHEDULE(dynamic)
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
!$OMP END PARALLEL DO

end subroutine fortran_euclidean_distance_parallel
