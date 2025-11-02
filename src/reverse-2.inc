    iarr = size(arr, 1)
    allocate(temp(iarr))

    do i = 1, n / 2
      temp(:) = arr(:, i)
      arr(:, i) = arr(:, n - i + 1)
      arr(:, n - i + 1) = temp(:)
    end do
