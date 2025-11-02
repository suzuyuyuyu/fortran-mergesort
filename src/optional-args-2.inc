    do_rev = .false.
    if (present(reverse)) do_rev = reverse
    do_hist = .false.
    if (present(history)) do_hist = .true.
    left = 1
    right = size(arr, 2)
    ref = 1
    if (present(aref)) ref = aref
