module merge_sort_mod
  use, intrinsic :: iso_fortran_env
  implicit none
  integer, parameter :: short = int16, long = int32, llong = int64, sp = real32, dp = real64, qp = real128

  character(len=*), parameter :: SORTING_METHOD = 'Merge Sort'
  private
  public :: SORTING_METHOD

  interface sort
    module procedure msort_1i_init, msort_1s_init, msort_1d_init, msort_1q_init
    module procedure msort_2i_init, msort_2s_init, msort_2d_init, msort_2q_init
  end interface sort
  public :: sort

  interface reverse
    module procedure reverse_1i, reverse_1s, reverse_1d, reverse_1q
    module procedure reverse_2i, reverse_2s, reverse_2d, reverse_2q
  end interface reverse
  public :: reverse

  interface rearrange
    module procedure rearrange_1i, rearrange_1s, rearrange_1d, rearrange_1q
    module procedure rearrange_2i, rearrange_2s, rearrange_2d, rearrange_2q
  end interface rearrange
  public :: rearrange

contains
  ! 1ih <-- 1 dimensional integer array with history
  ! _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  ! initial subroutines
  subroutine msort_1i_init(arr, history, reverse)
    implicit none
    integer, intent(inout) :: arr(:)
    logical, intent(in), optional :: reverse
    integer, intent(inout), optional :: history(:)
    integer :: left, mid, right
    logical :: do_rev, do_hist

    include "optional-args-1.inc"

    if (left < right) then
      mid = (left + right) / 2
      if (do_hist) then
        call msort_1ih(arr, left, mid, history)
        call msort_1ih(arr, mid + 1, right, history)
        call merge_1ih(arr, left, mid, right, history)
      else
        call msort_1i(arr, left, mid)
        call msort_1i(arr, mid + 1, right)
        call merge_1i(arr, left, mid, right)
      end if
    end if

    if (do_rev) then
      call reverse_1i(arr(left:right), right - left + 1)
    end if
  end subroutine msort_1i_init
  ! --------------------------------------------------------------------------------
  subroutine msort_1s_init(arr, history, reverse)
    implicit none
    real(sp), intent(inout) :: arr(:)
    logical, intent(in), optional :: reverse
    integer, intent(inout), optional :: history(:)
    integer :: left, mid, right
    logical :: do_rev, do_hist

    include "optional-args-1.inc"

    if (left < right) then
      mid = (left + right) / 2
      if (do_hist) then
        call msort_1sh(arr, left, mid, history)
        call msort_1sh(arr, mid + 1, right, history)
        call merge_1sh(arr, left, mid, right, history)
      else
        call msort_1s(arr, left, mid)
        call msort_1s(arr, mid + 1, right)
        call merge_1s(arr, left, mid, right)
      end if
    end if
    if (do_rev) then
      call reverse_1s(arr(left:right), right - left + 1)
    end if
  end subroutine msort_1s_init
  ! --------------------------------------------------------------------------------
  subroutine msort_1d_init(arr, history, reverse)
    implicit none
    real(dp), intent(inout) :: arr(:)
    logical, intent(in), optional :: reverse
    integer, intent(inout), optional :: history(:)
    integer :: left, mid, right
    logical :: do_rev, do_hist

    include "optional-args-1.inc"

    if (left < right) then
      mid = (left + right) / 2
      if (do_hist) then
        call msort_1dh(arr, left, mid, history)
        call msort_1dh(arr, mid + 1, right, history)
        call merge_1dh(arr, left, mid, right, history)
      else
        call msort_1d(arr, left, mid)
        call msort_1d(arr, mid + 1, right)
        call merge_1d(arr, left, mid, right)
      end if
    end if
    if (do_rev) then
      call reverse_1d(arr(left:right), right - left + 1)
    end if
  end subroutine msort_1d_init
  ! --------------------------------------------------------------------------------
  subroutine msort_1q_init(arr, history, reverse)
    implicit none
    real(qp), intent(inout) :: arr(:)
    logical, intent(in), optional :: reverse
    integer, intent(inout), optional :: history(:)
    integer :: left, mid, right
    logical :: do_rev, do_hist

    include "optional-args-1.inc"

    if (left < right) then
      mid = (left + right) / 2
      if (do_hist) then
        call msort_1qh(arr, left, mid, history)
        call msort_1qh(arr, mid + 1, right, history)
        call merge_1qh(arr, left, mid, right, history)
      else
        call msort_1q(arr, left, mid)
        call msort_1q(arr, mid + 1, right)
        call merge_1q(arr, left, mid, right)
      end if
    end if
    if (do_rev) then
      call reverse_1q(arr(left:right), right - left + 1)
    end if
  end subroutine msort_1q_init
  ! ================================================================================
  ! 2d init
  subroutine msort_2i_init(arr, history, aref, reverse)
    implicit none
    integer, intent(inout) :: arr(:, :)
    integer, intent(in), optional :: aref
    logical, intent(in), optional :: reverse
    integer, intent(inout), optional :: history(:)
    integer :: left, mid, right, ref
    logical :: do_rev, do_hist

    include "optional-args-2.inc"

    if (left < right) then
      mid = (left + right) / 2
      if (do_hist) then
        call msort_2ih(arr, ref, left, mid, history)
        call msort_2ih(arr, ref, mid + 1, right, history)
        call merge_2ih(arr, ref, left, mid, right, history)
      else
        call msort_2i(arr, ref, left, mid)
        call msort_2i(arr, ref, mid + 1, right)
        call merge_2i(arr, ref, left, mid, right)
      end if
    end if

    if (do_rev) then
      call reverse_2i(arr(:, left:right), right - left + 1)
    end if
  end subroutine msort_2i_init
  ! --------------------------------------------------------------------------------
  subroutine msort_2s_init(arr, history, aref, reverse)
    implicit none
    real(sp), intent(inout) :: arr(:, :)
    integer, intent(in), optional :: aref
    logical, intent(in), optional :: reverse
    integer, intent(inout), optional :: history(:)
    integer :: left, mid, right, ref
    logical :: do_rev, do_hist

    include "optional-args-2.inc"

    if (left < right) then
      mid = (left + right) / 2
      if (do_hist) then
        call msort_2sh(arr, ref, left, mid, history)
        call msort_2sh(arr, ref, mid + 1, right, history)
        call merge_2sh(arr, ref, left, mid, right, history)
      else
        call msort_2s(arr, ref, left, mid)
        call msort_2s(arr, ref, mid + 1, right)
        call merge_2s(arr, ref, left, mid, right)
      end if
    end if
    if (do_rev) then
      call reverse_2s(arr(:, left:right), right - left + 1)
    end if
  end subroutine msort_2s_init
  ! --------------------------------------------------------------------------------
  subroutine msort_2d_init(arr, history, aref, reverse)
    implicit none
    real(dp), intent(inout) :: arr(:, :)
    integer, intent(in), optional :: aref
    logical, intent(in), optional :: reverse
    integer, intent(inout), optional :: history(:)
    integer :: left, mid, right, ref
    logical :: do_rev, do_hist

    include "optional-args-2.inc"

    if (left < right) then
      mid = (left + right) / 2
      if (do_hist) then
        call msort_2dh(arr, ref, left, mid, history)
        call msort_2dh(arr, ref, mid + 1, right, history)
        call merge_2dh(arr, ref, left, mid, right, history)
      else
        call msort_2d(arr, ref, left, mid)
        call msort_2d(arr, ref, mid + 1, right)
        call merge_2d(arr, ref, left, mid, right)
      end if
    end if
    if (do_rev) then
      call reverse_2d(arr(:, left:right), right - left + 1)
    end if
  end subroutine msort_2d_init
  ! --------------------------------------------------------------------------------
  subroutine msort_2q_init(arr, history, aref, reverse)
    implicit none
    real(qp), intent(inout) :: arr(:, :)
    integer, intent(in), optional :: aref
    logical, intent(in), optional :: reverse
    integer, intent(inout), optional :: history(:)
    integer :: left, mid, right, ref
    logical :: do_rev, do_hist

    include "optional-args-2.inc"

    if (left < right) then
      mid = (left + right) / 2
      if (do_hist) then
        call msort_2qh(arr, ref, left, mid, history)
        call msort_2qh(arr, ref, mid + 1, right, history)
        call merge_2qh(arr, ref, left, mid, right, history)
      else
        call msort_2q(arr, ref, left, mid)
        call msort_2q(arr, ref, mid + 1, right)
        call merge_2q(arr, ref, left, mid, right)
      end if
    end if
    if (do_rev) then
      call reverse_2q(arr(:, left:right), right - left + 1)
    end if
  end subroutine msort_2q_init

  ! _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  ! merge sort
  recursive subroutine msort_1i(arr, left, right)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_1i(arr, left, mid)
      call msort_1i(arr, mid + 1, right)
      call merge_1i(arr, left, mid, right)
    end if
  end subroutine msort_1i
  recursive subroutine msort_1ih(arr, left, right, history)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer, intent(inout) :: history(:)
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_1ih(arr, left, mid, history)
      call msort_1ih(arr, mid + 1, right, history)
      call merge_1ih(arr, left, mid, right, history)
    end if
  end subroutine msort_1ih
  ! --------------------------------------------------------------------------------
  recursive subroutine msort_1s(arr, left, right)
    implicit none
    real(sp), intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_1s(arr, left, mid)
      call msort_1s(arr, mid + 1, right)
      call merge_1s(arr, left, mid, right)
    end if
  end subroutine msort_1s
  recursive subroutine msort_1sh(arr, left, right, history)
    implicit none
    real(sp), intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer, intent(inout) :: history(:)
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_1sh(arr, left, mid, history)
      call msort_1sh(arr, mid + 1, right, history)
      call merge_1sh(arr, left, mid, right, history)
    end if
  end subroutine msort_1sh
  ! --------------------------------------------------------------------------------
  recursive subroutine msort_1d(arr, left, right)
    implicit none
    real(dp), intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_1d(arr, left, mid)
      call msort_1d(arr, mid + 1, right)
      call merge_1d(arr, left, mid, right)
    end if
  end subroutine msort_1d
  recursive subroutine msort_1dh(arr, left, right, history)
    implicit none
    real(dp), intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer, intent(inout) :: history(:)
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_1dh(arr, left, mid, history)
      call msort_1dh(arr, mid + 1, right, history)
      call merge_1dh(arr, left, mid, right, history)
    end if
  end subroutine msort_1dh
  ! --------------------------------------------------------------------------------
  recursive subroutine msort_1q(arr, left, right)
    implicit none
    real(qp), intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_1q(arr, left, mid)
      call msort_1q(arr, mid + 1, right)
      call merge_1q(arr, left, mid, right)
    end if
  end subroutine msort_1q
  recursive subroutine msort_1qh(arr, left, right, history)
    implicit none
    real(qp), intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer, intent(inout) :: history(:)
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_1qh(arr, left, mid, history)
      call msort_1qh(arr, mid + 1, right, history)
      call merge_1qh(arr, left, mid, right, history)
    end if
  end subroutine msort_1qh
  ! =================================================================================
  ! 2d merge sort
  recursive subroutine msort_2i(arr, ref, left, right)
    implicit none
    integer, intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, right
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_2i(arr, ref, left, mid)
      call msort_2i(arr, ref, mid + 1, right)
      call merge_2i(arr, ref, left, mid, right)
    end if
  end subroutine msort_2i
  recursive subroutine msort_2ih(arr, ref, left, right, history)
    implicit none
    integer, intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, right
    integer, intent(inout) :: history(:)
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_2ih(arr, ref, left, mid, history)
      call msort_2ih(arr, ref, mid + 1, right, history)
      call merge_2ih(arr, ref, left, mid, right, history)
    end if
  end subroutine msort_2ih
  ! --------------------------------------------------------------------------------
  recursive subroutine msort_2s(arr, ref, left, right)
    implicit none
    real(sp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, right
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_2s(arr, ref, left, mid)
      call msort_2s(arr, ref, mid + 1, right)
      call merge_2s(arr, ref, left, mid, right)
    end if
  end subroutine msort_2s
  recursive subroutine msort_2sh(arr, ref, left, right, history)
    implicit none
    real(sp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, right
    integer, intent(inout) :: history(:)
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_2sh(arr, ref, left, mid, history)
      call msort_2sh(arr, ref, mid + 1, right, history)
      call merge_2sh(arr, ref, left, mid, right, history)
    end if
  end subroutine msort_2sh
  ! --------------------------------------------------------------------------------
  recursive subroutine msort_2d(arr, ref, left, right)
    implicit none
    real(dp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, right
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_2d(arr, ref, left, mid)
      call msort_2d(arr, ref, mid + 1, right)
      call merge_2d(arr, ref, left, mid, right)
    end if
  end subroutine msort_2d
  recursive subroutine msort_2dh(arr, ref, left, right, history)
    implicit none
    real(dp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, right
    integer, intent(inout) :: history(:)
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_2dh(arr, ref, left, mid, history)
      call msort_2dh(arr, ref, mid + 1, right, history)
      call merge_2dh(arr, ref, left, mid, right, history)
    end if
  end subroutine msort_2dh
  ! --------------------------------------------------------------------------------
  recursive subroutine msort_2q(arr, ref, left, right)
    implicit none
    real(qp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, right
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_2q(arr, ref, left, mid)
      call msort_2q(arr, ref, mid + 1, right)
      call merge_2q(arr, ref, left, mid, right)
    end if
  end subroutine msort_2q
  recursive subroutine msort_2qh(arr, ref, left, right, history)
    implicit none
    real(qp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, right
    integer, intent(inout) :: history(:)
    integer :: mid

    if (left < right) then
      mid = (left + right) / 2
      call msort_2qh(arr, ref, left, mid, history)
      call msort_2qh(arr, ref, mid + 1, right, history)
      call merge_2qh(arr, ref, left, mid, right, history)
    end if
  end subroutine msort_2qh

  ! _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  ! merge subroutines
  subroutine merge_1i(arr, left, mid, right)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: left, mid, right
    integer, allocatable :: temp(:)
    integer :: i, j, k
    logical :: condition

    include "merge-1.inc"

  end subroutine merge_1i
  subroutine merge_1ih(arr, left, mid, right, history)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: left, mid, right
    integer, intent(inout) :: history(:)
    integer, allocatable :: temp(:)
    integer, allocatable :: temp_hist(:)
    integer :: i, j, k
    logical :: condition

    include "merge-1h.inc"

  end subroutine merge_1ih
  ! --------------------------------------------------------------------------------
  subroutine merge_1s(arr, left, mid, right)
    implicit none
    real(sp), intent(inout) :: arr(:)
    integer, intent(in) :: left, mid, right
    real(sp), allocatable :: temp(:)
    integer :: i, j, k
    logical :: condition

    include "merge-1.inc"

  end subroutine merge_1s
  subroutine merge_1sh(arr, left, mid, right, history)
    implicit none
    real(sp), intent(inout) :: arr(:)
    integer, intent(in) :: left, mid, right
    integer, intent(inout) :: history(:)
    real(sp), allocatable :: temp(:)
    integer, allocatable :: temp_hist(:)
    integer :: i, j, k
    logical :: condition

    include "merge-1h.inc"

  end subroutine merge_1sh
  ! --------------------------------------------------------------------------------
  subroutine merge_1d(arr, left, mid, right)
    implicit none
    real(dp), intent(inout) :: arr(:)
    integer, intent(in) :: left, mid, right
    real(dp), allocatable :: temp(:)
    integer :: i, j, k
    logical :: condition

    include "merge-1.inc"

  end subroutine merge_1d
  subroutine merge_1dh(arr, left, mid, right, history)
    implicit none
    real(dp), intent(inout) :: arr(:)
    integer, intent(in) :: left, mid, right
    integer, intent(inout) :: history(:)
    real(dp), allocatable :: temp(:)
    integer, allocatable :: temp_hist(:)
    integer :: i, j, k
    logical :: condition

    include "merge-1h.inc"

  end subroutine merge_1dh
  ! --------------------------------------------------------------------------------
  subroutine merge_1q(arr, left, mid, right)
    implicit none
    real(qp), intent(inout) :: arr(:)
    integer, intent(in) :: left, mid, right
    real(qp), allocatable :: temp(:)
    integer :: i, j, k
    logical :: condition

    include "merge-1.inc"

  end subroutine merge_1q
  subroutine merge_1qh(arr, left, mid, right, history)
    implicit none
    real(qp), intent(inout) :: arr(:)
    integer, intent(in) :: left, mid, right
    integer, intent(inout) :: history(:)
    real(qp), allocatable :: temp(:)
    integer, allocatable :: temp_hist(:)
    integer :: i, j, k
    logical :: condition

    include "merge-1h.inc"

  end subroutine merge_1qh
  ! =================================================================================
  ! 2d merge
  subroutine merge_2i(arr, ref, left, mid, right)
    implicit none
    integer, intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, mid, right
    integer, allocatable :: temp(:, :)
    integer :: i, j, k
    logical :: condition

    include "merge-2.inc"

  end subroutine merge_2i
  subroutine merge_2ih(arr, ref, left, mid, right, history)
    implicit none
    integer, intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, mid, right
    integer, intent(inout) :: history(:)
    integer, allocatable :: temp(:, :)
    integer, allocatable :: temp_hist(:)
    integer :: i, j, k
    logical :: condition

    include "merge-2h.inc"

  end subroutine merge_2ih
  ! --------------------------------------------------------------------------------
  subroutine merge_2s(arr, ref, left, mid, right)
    implicit none
    real(sp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, mid, right
    real(sp), allocatable :: temp(:, :)
    integer :: i, j, k
    logical :: condition

    include "merge-2.inc"

  end subroutine merge_2s
  subroutine merge_2sh(arr, ref, left, mid, right, history)
    implicit none
    real(sp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, mid, right
    integer, intent(inout) :: history(:)
    real(sp), allocatable :: temp(:, :)
    integer, allocatable :: temp_hist(:)
    integer :: i, j, k
    logical :: condition

    include "merge-2h.inc"

  end subroutine merge_2sh
  ! --------------------------------------------------------------------------------
  subroutine merge_2d(arr, ref, left, mid, right)
    implicit none
    real(dp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, mid, right
    real(dp), allocatable :: temp(:, :)
    integer :: i, j, k
    logical :: condition

    include "merge-2.inc"

  end subroutine merge_2d
  subroutine merge_2dh(arr, ref, left, mid, right, history)
    implicit none
    real(dp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, mid, right
    integer, intent(inout) :: history(:)
    real(dp), allocatable :: temp(:, :)
    integer, allocatable :: temp_hist(:)
    integer :: i, j, k
    logical :: condition

    include "merge-2h.inc"

  end subroutine merge_2dh
  ! --------------------------------------------------------------------------------
  subroutine merge_2q(arr, ref, left, mid, right)

    real(qp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, mid, right
    real(qp), allocatable :: temp(:, :)
    integer :: i, j, k
    logical :: condition

    include "merge-2.inc"

  end subroutine merge_2q
  subroutine merge_2qh(arr, ref, left, mid, right, history)
    implicit none
    real(qp), intent(inout) :: arr(:, :)
    integer, intent(in) :: ref, left, mid, right
    integer, intent(inout) :: history(:)
    real(qp), allocatable :: temp(:, :)
    integer, allocatable :: temp_hist(:)
    integer :: i, j, k
    logical :: condition

    include "merge-2h.inc"

  end subroutine merge_2qh

  ! _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  ! reverse
  subroutine reverse_1i(arr, n)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: n
    integer :: i
    integer :: temp

    include "reverse-1.inc"

  end subroutine reverse_1i
  subroutine reverse_1s(arr, n)
    implicit none
    real(sp), intent(inout) :: arr(:)
    integer, intent(in) :: n
    integer :: i
    real(sp) :: temp

    include "reverse-1.inc"

  end subroutine reverse_1s
  subroutine reverse_1d(arr, n)
    implicit none
    real(dp), intent(inout) :: arr(:)
    integer, intent(in) :: n
    integer :: i
    real(dp) :: temp

    include "reverse-1.inc"

  end subroutine reverse_1d
  subroutine reverse_1q(arr, n)
    implicit none
    real(qp), intent(inout) :: arr(:)
    integer, intent(in) :: n
    integer :: i
    real(qp) :: temp

    include "reverse-1.inc"

  end subroutine reverse_1q
  subroutine reverse_2i(arr, n)
    implicit none
    integer, intent(inout) :: arr(:, :)
    integer, intent(in) :: n
    integer :: i
    integer, allocatable :: temp(:)
    integer :: iarr

    include "reverse-2.inc"

  end subroutine reverse_2i
  subroutine reverse_2s(arr, n)
    implicit none
    real(sp), intent(inout) :: arr(:, :)
    integer, intent(in) :: n
    integer :: i
    real(sp), allocatable :: temp(:)
    integer :: iarr

    include "reverse-2.inc"

  end subroutine reverse_2s
  subroutine reverse_2d(arr, n)
    implicit none
    real(dp), intent(inout) :: arr(:, :)
    integer, intent(in) :: n
    integer :: i
    real(dp), allocatable :: temp(:)
    integer :: iarr

    include "reverse-2.inc"

  end subroutine reverse_2d
  subroutine reverse_2q(arr, n)
    implicit none
    real(qp), intent(inout) :: arr(:, :)
    integer, intent(in) :: n
    integer :: i
    real(qp), allocatable :: temp(:)
    integer :: iarr

    include "reverse-2.inc"

  end subroutine reverse_2q
  ! _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  ! rearrange
  subroutine rearrange_1i(arr, reference_history)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: reference_history(:)
    integer :: n, i
    integer, allocatable :: temp(:)

    include "rearrange-1.inc"

  end subroutine rearrange_1i
  subroutine rearrange_1s(arr, reference_history)
    implicit none
    real(sp), intent(inout) :: arr(:)
    integer, intent(in) :: reference_history(:)
    integer :: n, i
    real(sp), allocatable :: temp(:)

    include "rearrange-1.inc"

  end subroutine rearrange_1s
  subroutine rearrange_1d(arr, reference_history)
    implicit none
    real(dp), intent(inout) :: arr(:)
    integer, intent(in) :: reference_history(:)
    integer :: n, i
    real(dp), allocatable :: temp(:)

    include "rearrange-1.inc"

  end subroutine rearrange_1d
  subroutine rearrange_1q(arr, reference_history)
    implicit none
    real(qp), intent(inout) :: arr(:)
    integer, intent(in) :: reference_history(:)
    integer :: n, i
    real(qp), allocatable :: temp(:)

    include "rearrange-1.inc"

  end subroutine rearrange_1q
  subroutine rearrange_2i(arr, reference_history)
    implicit none
    integer, intent(inout) :: arr(:, :)
    integer, intent(in) :: reference_history(:)
    integer :: m, n, i
    integer, allocatable :: temp(:, :)

    include "rearrange-2.inc"

  end subroutine rearrange_2i
  subroutine rearrange_2s(arr, reference_history)
    implicit none
    real(sp), intent(inout) :: arr(:, :)
    integer, intent(in) :: reference_history(:)
    integer :: m, n, i
    real(sp), allocatable :: temp(:, :)

    include "rearrange-2.inc"

  end subroutine rearrange_2s
  subroutine rearrange_2d(arr, reference_history)
    implicit none
    real(dp), intent(inout) :: arr(:, :)
    integer, intent(in) :: reference_history(:)
    integer :: m, n, i
    real(dp), allocatable :: temp(:, :)

    include "rearrange-2.inc"

  end subroutine rearrange_2d
  subroutine rearrange_2q(arr, reference_history)
    implicit none
    real(qp), intent(inout) :: arr(:, :)
    integer, intent(in) :: reference_history(:)
    integer :: m, n, i
    real(qp), allocatable :: temp(:, :)

    include "rearrange-2.inc"

  end subroutine rearrange_2q
end module merge_sort_mod
