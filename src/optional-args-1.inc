    do_rev = .false.
    if (present(reverse)) do_rev = reverse
    do_hist = .false.
    if (present(history)) do_hist = .true.
    left = 1
    right = size(arr)
