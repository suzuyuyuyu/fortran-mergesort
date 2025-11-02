    allocate(temp(size(arr, 1), left:right), source=arr(:, left:right))

    allocate(temp_hist(left:right))
    temp_hist(left:right) = history(left:right)

    i = left
    j = mid + 1
    k = left

    do while (i <= mid .and. j <= right)
      condition = temp(ref, i) <= temp(ref, j)

      if (condition) then
        arr(:, k) = temp(:, i)
        history(k) = temp_hist(i)
        i = i + 1
      else
        arr(:, k) = temp(:, j)
        history(k) = temp_hist(j)
        j = j + 1
      end if
      k = k + 1
    end do

    do while (i <= mid)
      arr(:, k) = temp(:, i)
      history(k) = temp_hist(i)
      i = i + 1
      k = k + 1
    end do

    do while (j <= right)
      arr(:, k) = temp(:, j)
      history(k) = temp_hist(j)
      j = j + 1
      k = k + 1
    end do

    deallocate(temp)
    if (allocated(temp_hist)) deallocate(temp_hist)
