#define _TARGET

program test_reduce
  implicit none

  type xyz
    real, allocatable, dimension(:) :: abc
  end type xyz

  type(xyz) &
#ifdef _TARGET
    & , target &
#endif
    & :: jkl
  integer :: n, i, nloop, ij

  n = 8
  nloop = 20005
  allocate(jkl%abc(n))

  jkl%abc(:) = 0.0
  block
    real, allocatable, dimension(:) :: tmp_arr
    tmp_arr = jkl%abc
    !$omp parallel do private(ij) shared(tmp_arr) reduction(+: tmp_arr(:))
    do i=1, nloop
      ij = mod(i,n)+1
      tmp_arr(ij) = tmp_arr(ij) + 1.
    enddo
    !$omp end parallel do
    jkl%abc(:) = tmp_arr(:)
  end block


  call test_sub_tmp_omp(jkl%abc(:), nloop, n)

contains

  subroutine test_sub_tmp_omp(ref_arr, nloop, n)
    real, dimension(:), intent(inout) :: ref_arr
    real, dimension(8) :: tmp_arr
    integer, intent(in) :: nloop, n
    integer :: i, ii

    tmp_arr(:) = ref_arr(:)
    print *, 'lbound, ubound', lbound(tmp_arr), ubound(tmp_arr)
    !$omp parallel do private(ii) shared(tmp_arr) reduction(+: tmp_arr(1:8))
    do i=1, nloop
    enddo
    !$omp end parallel do
    ref_arr(:) = tmp_arr(:)
  end subroutine test_sub_tmp_omp

end program test_reduce
