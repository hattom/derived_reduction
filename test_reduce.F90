program test_reduce
  implicit none

  type xyz
    real, allocatable, dimension(:) :: abc
  end type xyz

  type(xyz) :: jkl
  integer :: n, i, nloop, ii

  n = 8
  nloop = 20005
  allocate(jkl%abc(n))

  jkl%abc(:) = 0.0
  block
    do i=1, nloop
      ii = mod(i,n)+1
      jkl%abc(ii) = jkl%abc(ii) + 1
    enddo
  end block
  print *, "serial:  ", jkl%abc

  jkl%abc(:) = 0.0
  block
    real, allocatable, dimension(:) :: tmp_arr
    tmp_arr = jkl%abc
    !$omp parallel do private(ii) reduction(+: tmp_arr)
    do i=1, nloop
      ii = mod(i,n)+1
      tmp_arr(ii) = tmp_arr(ii) + 1
    enddo
    !$omp end parallel do
    jkl%abc(:) = tmp_arr(:)
  end block
  print *, "omp tmp: ", jkl%abc

call test_sub(jkl%abc(:), nloop, n)
print *, "subr:    ", jkl%abc

#if 0
  jkl%abc(:) = 0.0
  !$omp parallel do private(ii) reduction(+: jkl%abc)
  do i=1, nloop
    ii = mod(i,n)+1
    jkl%abc(ii) = jkl%abc(ii) + 1
  enddo
  !$omp end parallel do
  print *, jkl%abc
#endif

#if 0
  jkl%abc(:) = 0.0
  !$omp parallel do private(ii) reduction(+: jkl)
  do i=1, nloop
    ii = mod(i,n)+1
    jkl%abc(ii) = jkl%abc(ii) + 1
  enddo
  !$omp end parallel do
  print *, jkl%abc
#endif

#if 0
  jkl%abc(:) = 0.0
  associate(tmp_arr => jkl%abc)
    !$omp parallel do private(ii) reduction(+: tmp_arr)
    do i=1, nloop
      ii = mod(i,n)+1
      tmp_arr(ii) = tmp_arr(ii) + 1
    enddo
    !$omp end parallel do
  end associate
  print *, jkl%abc
#endif

contains

  subroutine test_sub(tmp_arr, nloop, n)
    real, dimension(:), intent(inout) :: tmp_arr
    integer, intent(in) :: nloop, n
    integer :: i, ii

    tmp_arr(:) = 0.0
    !$omp parallel do private(ii) reduction(+: tmp_arr)
    do i=1, nloop
      ii = mod(i,n)+1
      tmp_arr(ii) = tmp_arr(ii) + 1
    enddo
    !$omp end parallel do
  end subroutine test_sub
end program test_reduce
