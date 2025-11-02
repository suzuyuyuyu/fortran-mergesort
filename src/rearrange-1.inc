    n = size(arr)
    allocate(temp(n))

    do i = 1, n
      temp(i) = arr(reference_history(i))
    end do

    arr = temp
    deallocate(temp)
