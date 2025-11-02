<h1 align='center'> Fortran Merge Sort </h1>

A Fortran library that implements the merge sort.

## Usage
### 1. Clone this repository.

```bash
git clone https://github.com/suzuyuyuyu/fortran-mergesort.git
```

### 2. Compile the library.

```bash
cd fortran-mergesort
make
```
You can change the compiler, optimization flags, and so on in the `Makefile`.

| Flag | Default Value |
|--- | --- |
| `FC` | `ifx` |
| `FFLAGS` | `-O3` |
| `AR` | `llvm-ar rcs` |
| `LIB_OUT_DIR` | `lib` |
| `INC_OUT_DIR` | `include` |

e.g.,
```bash
make FC=gfortran FFLAGS='-O0 -g -Wall -Wextra'
```

### 3. Link the library and module in your Fortran code.

```fortran
program main
    use merge_sort_mod
    implicit none
    ⋮
    call sort(array)
    ! or
    call sort(array, history, reverse=.true.)
    ! or
    call sort(array_2d) ! 2d array, e.g., [ [2, 3, 1], [1, 2, 3] ] -> [ [1, 2, 3], [3, 2, 1] ]
    ! or
    call sort(array_2d, history, aref, reverse) ! aref: array reference line
    
    call reverse(array)

    call rearrange(array, history) ! rearrange array based on history after sorting other arrays
```

To find more details, see the sample code in the `examples` directory.
