program main
  use, intrinsic :: iso_fortran_env
  use merge_sort_mod
  implicit none
  integer :: i, m, n
  integer, allocatable :: arr(:), twod_arr(:, :), hist(:)
  real(real64), allocatable :: corresp(:), twod_corresp(:, :)

  m = 2; n = 10
  allocate(arr(n), corresp(n))
  allocate(hist(n), source=[(i, i=1,n)])

  call random_seed()
  block
    real(real64) :: arr_real(n)
    call random_number(arr_real)
    arr = int(arr_real * 100)
    call random_number(arr_real)
    corresp = dble(arr_real * 10)
  end block


  print'("Unsorted array:")'
  print'(10i5)', arr
  print'(10f5.1)', corresp

  call sort(arr, hist)
  ! call rearrange(corresp, hist)
  corresp(:) = corresp(hist(:))

  print'("Sorted array:")'
  print'(10i5)', arr
  print'(10f5.1)', corresp

  deallocate(arr, corresp, hist)


  allocate(twod_arr(m, n), twod_corresp(m, n))
  allocate(hist(n), source=[(i, i=1,n)])

  block
    real(real64) :: arr_real(m, n)
    call random_number(arr_real)
    twod_arr = int(arr_real * 100)
    call random_number(arr_real)
    twod_corresp = dble(arr_real * 10)
  end block

  print'("2D array before sorting:")'
  do i = 1, m
    print'(10i5)', twod_arr(i, :)
  end do
  do i = 1, m
    print'(10f5.1)', twod_corresp(i, :)
  end do

  call sort(twod_arr, hist)
  call rearrange(twod_corresp, hist)

  print'("2D array after sorting:")'
  do i = 1, m
    print'(10i5)', twod_arr(i, :)
  end do
  do i = 1, m
    print'(10f5.1)', twod_corresp(i, :)
  end do


  deallocate(twod_arr, twod_corresp, hist)

end program main
