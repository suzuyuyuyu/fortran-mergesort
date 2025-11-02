    allocate(temp(left:right))
    temp(left:right) = arr(left:right)

    i = left
    j = mid + 1
    k = left

    do while (i <= mid .and. j <= right)
      condition = temp(i) <= temp(j)

      if (condition) then
        arr(k) = temp(i)
        i = i + 1
      else
        arr(k) = temp(j)
        j = j + 1
      end if
      k = k + 1
    end do

    do while (i <= mid)
      arr(k) = temp(i)
      i = i + 1
      k = k + 1
    end do

    do while (j <= right)
      arr(k) = temp(j)
      j = j + 1
      k = k + 1
    end do

    deallocate(temp)
