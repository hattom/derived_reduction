
#ifdef __NVCOMPILER_LLVM__
#define DOCONC
#define DOCONC_LOCAL
#define DOCONC_TMP_REDUCE
!#define DOCONC_REDUCE
#define POINTER
#define POINTER_FUNC
#define ASSOCIATE
#define ASSOCIATE_FUNC
#define ASSOCIATE_SERIAL
#endif

#ifdef __GFORTRAN__
#define DOCONC
!#define DOCONC_LOCAL
!#define DOCONC_TMP_REDUCE
!#define DOCONC_REDUCE
!#define POINTER
!#define POINTER_FUNC
!#define ASSOCIATE
#define ASSOCIATE_FUNC
#define ASSOCIATE_SERIAL
#define ABC
#endif

#if defined(POINTER) || defined(POINTER_FUNC)
#define TARGET
#endif

program test_reduce
  implicit none

  type xyz
    real, allocatable, dimension(:) :: abc
  end type xyz

  type(xyz) &
#ifdef TARGET
    & , target &
#endif
    & :: jkl
  integer :: n, i, nloop

  n = 8
  nloop = 20005
  allocate(jkl%abc(n))

  jkl%abc(:) = 0.0
  block
    integer :: ii
    do i=1, nloop
      ii = mod(i,n)+1
      jkl%abc(ii) = jkl%abc(ii) + 1
    enddo
  end block
  print *, "serial:  ", jkl%abc

  jkl%abc(:) = 0.0
  block
    real, allocatable, dimension(:) :: tmp_arr
    integer :: ii
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

#
call test_sub_omp(jkl%abc(:), nloop, n)
print *, "subr:    ", jkl%abc

call test_sub_serial(jkl%abc(:), nloop, n)
print *, "subr_ser:", jkl%abc

#ifdef SUBR_ATOMIC
call test_sub_atomic(jkl%abc(:), nloop, n)
print *, "subr_atm:", jkl%abc
#endif

#ifdef POINTER
  jkl%abc(:) = 0.0
  block
    integer :: ii
    real, dimension(:), pointer :: tmp_ptr
    tmp_ptr => jkl%abc
    !$omp parallel do private(ii) reduction(+: tmp_ptr)
    do i=1, nloop
      ii = mod(i,n)+1
      tmp_ptr(ii) = tmp_ptr(ii) + 1
    enddo
    !$omp end parallel do
  end block
  print *, "pointer: ", jkl%abc
#endif

#ifdef POINTER
  jkl%abc(:) = 0.0
  block
    integer :: ii
    real, dimension(:), pointer :: tmp_ptr
    tmp_ptr => jkl%abc
    do i=1, nloop
      ii = mod(i,n)+1
      tmp_ptr(ii) = tmp_ptr(ii) + 1
    enddo
  end block
  print *, "ptr ser: ", jkl%abc
#endif

#ifdef POINTER_FUNC
  jkl%abc(:) = 0.0
  block
    real, dimension(:), pointer :: tmp_ptr
    tmp_ptr => jkl%abc
    call test_sub_omp(tmp_ptr(:), nloop, n)
  end block
  print *, "ptr fnc: ", jkl%abc
#endif

#ifdef DERIVED_MEMBER
  jkl%abc(:) = 0.0
  !$omp parallel do private(ii) reduction(+: jkl%abc)
  do i=1, nloop
    ii = mod(i,n)+1
    jkl%abc(ii) = jkl%abc(ii) + 1
  enddo
  !$omp end parallel do
  print *, "der mem: ", jkl%abc
#endif

#ifdef DERIVED
  jkl%abc(:) = 0.0
  !$omp parallel do private(ii) reduction(+: jkl)
  do i=1, nloop
    ii = mod(i,n)+1
    jkl%abc(ii) = jkl%abc(ii) + 1
  enddo
  !$omp end parallel do
  print *, "derived: ", jkl%abc
#endif

#ifdef ASSOCIATE
  jkl%abc(:) = 0.0
  block
    integer :: ii
    associate(tmp_arr => jkl%abc)
      !$omp parallel do private(ii) reduction(+: tmp_arr)
      do i=1, nloop
        ii = mod(i,n)+1
        tmp_arr(ii) = tmp_arr(ii) + 1
      enddo
      !$omp end parallel do
    end associate
  end block
  print *, "assoc:   ", jkl%abc
#endif

#ifdef ASSOCIATE_FUNC
  jkl%abc(:) = 0.0
  associate(tmp_arr => jkl%abc)
    call test_sub_omp(tmp_arr(:), nloop, n)
  end associate
  print *, "assocfn: ", jkl%abc
#endif

#ifdef ASSOCIATE_SERIAL
  jkl%abc(:) = 0.0
  associate(tmp_arr => jkl%abc)
    call test_sub_serial(tmp_arr(:), nloop, n)
  end associate
  print *, "assocsf: ", jkl%abc
#endif

#ifdef DOCONC
  jkl%abc(:) = 0.0
  do concurrent(i=1:nloop)
    block
      integer :: ii
      ii = mod(i,n)+1
      jkl%abc(ii) = jkl%abc(ii) + 1
    end block
  enddo
  print *, "doconcB: ", jkl%abc
#endif

#ifdef DOCONC_LOCAL
  jkl%abc(:) = 0.0
  block
    integer :: ii
    do concurrent(i=1:nloop) local(ii)
      ii = mod(i,n)+1
      jkl%abc(ii) = jkl%abc(ii) + 1
    enddo
  end block
  print *, "doconcL: ", jkl%abc
#endif

#ifdef DOCONC_TMP_REDUCE
  jkl%abc(:) = 0.0
  block
    real, allocatable, dimension(:) :: tmp_arr
    tmp_arr = jkl%abc
    do concurrent(i=1:nloop) reduce(+: tmp_arr)
      block
        integer :: ii
        ii = mod(i,n)+1
        tmp_arr(ii) = tmp_arr(ii) + 1
      end block
    enddo
    jkl%abc(:) = tmp_arr(:)
  end block
  print *, "doconcTR:", jkl%abc
#endif

#ifdef DOCONC_REDUCE
  jkl%abc(:) = 0.0
  block
    integer :: ii
    do concurrent(i=1:nloop) local(ii) reduce(+: jkl%abc)
      ii = mod(i,n)+1
      jkl%abc(ii) = jkl%abc(ii) + 1
    enddo
  end block
  print *, "doconcTR:", jkl%abc
#endif


contains

  subroutine test_sub_omp(tmp_arr, nloop, n)
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
  end subroutine test_sub_omp

  subroutine test_sub_serial(tmp_arr, nloop, n)
    real, dimension(:), intent(inout) :: tmp_arr
    integer, intent(in) :: nloop, n
    integer :: i, ii

    tmp_arr(:) = 0.0
    do i=1, nloop
      ii = mod(i,n)+1
      tmp_arr(ii) = tmp_arr(ii) + 1
    enddo
  end subroutine test_sub_serial

  subroutine test_sub_atomic(tmp_arr, nloop, n)
    real, dimension(:), intent(inout) :: tmp_arr
    integer, intent(in) :: nloop, n
    integer :: i, ii

    tmp_arr(:) = 0.0
    !$omp parallel do private(ii)
    do i=1, nloop
      ii = mod(i,n)+1
      !$omp atomic update
      tmp_arr(ii) = tmp_arr(ii) + 1
    enddo
    !$omp end parallel do
  end subroutine test_sub_atomic
end program test_reduce
