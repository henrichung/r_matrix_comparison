! euclidean_distance.f90
subroutine calculate_distances(matrix, rows, cols, distances)
    implicit none
    ! Input variables
    integer, intent(in) :: rows, cols
    double precision, intent(in) :: matrix(rows, cols)
    ! Output variable
    double precision, intent(out) :: distances(rows, rows)
    ! Local variables
    integer :: i, j, k
    double precision :: sum

    do i = 1, rows
        do j = 1, rows
            sum = 0.0
            do k = 1, cols
                sum = sum + (matrix(i, k) - matrix(j, k))**2
            end do
            distances(i, j) = sqrt(sum)
        end do
    end do
end subroutine calculate_distances
