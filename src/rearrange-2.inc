    m = size(arr, 1)
    n = size(arr, 2)
    allocate(temp(m, n))

    do i = 1, n
      temp(:, i) = arr(:, reference_history(i))
    end do

    arr = temp
    deallocate(temp)
