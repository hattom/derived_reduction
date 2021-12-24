
#ifdef __NVCOMPILER_LLVM__
#define DOCONC
#define DOCONC_LOCAL
#define DOCONC_TMP_REDUCE
!#define DOCONC_REDUCE
#define _POINTER
#define _POINTER_FUNC
#define _ASSOCIATE
#define _ASSOCIATE_FUNC
#define _ASSOCIATE_SERIAL
#endif

#ifdef __GFORTRAN__
#define DOCONC
!#define DOCONC_LOCAL
!#define DOCONC_TMP_REDUCE
!#define DOCONC_REDUCE
!#define _POINTER
!#define _POINTER_FUNC
!#define _ASSOCIATE
#define _ASSOCIATE_FUNC
#define _ASSOCIATE_SERIAL
#endif

#ifdef __INTEL_COMPILER
#define DOCONC
! Intel 18.X doesn't support do concurrent local
#if __INTEL_COMPILER >= 1900
#define DOCONC_LOCAL
#endif
!#define DOCONC_TMP_REDUCE
!#define DOCONC_REDUCE
!#define _POINTER
#define _POINTER_FUNC
!#define _ASSOCIATE
#define _ASSOCIATE_FUNC
#define _ASSOCIATE_SERIAL
#endif

#if defined(_POINTER) || defined(_POINTER_FUNC)
#define _TARGET
#endif

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
  integer :: n, i, nloop

  n = 8
  nloop = 20005
  allocate(jkl%abc(n))

  jkl%abc(:) = 0.0
  block
    integer :: ii
    do i=1, nloop
      ii = mod(i,n)+1
      jkl%abc(ii) = jkl%abc(ii) + 1.
    enddo
  end block
  call print_array(jkl%abc, 'serial')

  jkl%abc(:) = 0.0
  block
    real, allocatable, dimension(:) :: tmp_arr
    integer :: ii
    tmp_arr = jkl%abc
    !$omp parallel do private(ii) reduction(+: tmp_arr)
    do i=1, nloop
      ii = mod(i,n)+1
      tmp_arr(ii) = tmp_arr(ii) + 1.
    enddo
    !$omp end parallel do
    jkl%abc(:) = tmp_arr(:)
  end block
  call print_array(jkl%abc, 'omp tmp')

  call test_sub_omp(jkl%abc(:), nloop, n)
  call print_array(jkl%abc, 'subr')

  call test_sub_serial(jkl%abc(:), nloop, n)
  call print_array(jkl%abc, 'subr ser')

#ifdef SUBR_ATOMIC
  call test_sub_atomic(jkl%abc(:), nloop, n)
  call print_array(jkl%abc, 'subr atm')
#endif

#ifdef _POINTER
  jkl%abc(:) = 0.0
  block
    integer :: ii
    real, dimension(:), pointer :: tmp_ptr
    tmp_ptr => jkl%abc
    !$omp parallel do private(ii) reduction(+: tmp_ptr)
    do i=1, nloop
      ii = mod(i,n)+1
      tmp_ptr(ii) = tmp_ptr(ii) + 1.
    enddo
    !$omp end parallel do
  end block
  call print_array(jkl%abc, 'pointer')
#endif

#ifdef _POINTER
  jkl%abc(:) = 0.0
  block
    integer :: ii
    real, dimension(:), pointer :: tmp_ptr
    tmp_ptr => jkl%abc
    do i=1, nloop
      ii = mod(i,n)+1
      tmp_ptr(ii) = tmp_ptr(ii) + 1.
    enddo
  end block
  call print_array(jkl%abc, 'ptr ser')
#endif

#ifdef _POINTER_FUNC
  jkl%abc(:) = 0.0
  block
    real, dimension(:), pointer :: tmp_ptr
    tmp_ptr => jkl%abc
    call test_sub_omp(tmp_ptr(:), nloop, n)
  end block
  call print_array(jkl%abc, 'ptr fn')
#endif

#ifdef DERIVED_MEMBER
  jkl%abc(:) = 0.0
  !$omp parallel do private(ii) reduction(+: jkl%abc)
  do i=1, nloop
    ii = mod(i,n)+1
    jkl%abc(ii) = jkl%abc(ii) + 1.
  enddo
  !$omp end parallel do
  call print_array(jkl%abc, 'der mem')
#endif

#ifdef DERIVED
  jkl%abc(:) = 0.0
  !$omp parallel do private(ii) reduction(+: jkl)
  do i=1, nloop
    ii = mod(i,n)+1
    jkl%abc(ii) = jkl%abc(ii) + 1.
  enddo
  !$omp end parallel do
  call print_array(jkl%abc, 'der')
#endif

#ifdef _ASSOCIATE
  jkl%abc(:) = 0.0
  block
    integer :: ii
    associate(tmp_arr => jkl%abc)
      !$omp parallel do private(ii) reduction(+: tmp_arr)
      do i=1, nloop
        ii = mod(i,n)+1
        tmp_arr(ii) = tmp_arr(ii) + 1.
      enddo
      !$omp end parallel do
    end associate
  end block
  call print_array(jkl%abc, 'ass')
#endif

#ifdef _ASSOCIATE_FUNC
  jkl%abc(:) = 0.0
  associate(tmp_arr => jkl%abc)
    call test_sub_omp(tmp_arr(:), nloop, n)
  end associate
  call print_array(jkl%abc, 'ass fn')
#endif

#ifdef _ASSOCIATE_SERIAL
  jkl%abc(:) = 0.0
  associate(tmp_arr => jkl%abc)
    call test_sub_serial(tmp_arr(:), nloop, n)
  end associate
  call print_array(jkl%abc, 'ass s fn')
#endif

#ifdef DOCONC
  jkl%abc(:) = 0.0
  do concurrent(i=1:nloop)
    block
      integer :: ii
      ii = mod(i,n)+1
      jkl%abc(ii) = jkl%abc(ii) + 1.
    end block
  enddo
  call print_array(jkl%abc, 'doc bl')
#endif

#ifdef DOCONC_LOCAL
  jkl%abc(:) = 0.0
  block
    integer :: ii
    do concurrent(i=1:nloop) local(ii)
      ii = mod(i,n)+1
      jkl%abc(ii) = jkl%abc(ii) + 1.
    enddo
  end block
  call print_array(jkl%abc, 'doc loc')
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
        tmp_arr(ii) = tmp_arr(ii) + 1.
      end block
    enddo
    jkl%abc(:) = tmp_arr(:)
  end block
  call print_array(jkl%abc, 'doc t r')
#endif

#ifdef DOCONC_REDUCE
  jkl%abc(:) = 0.0
  block
    integer :: ii
    do concurrent(i=1:nloop) local(ii) reduce(+: jkl%abc)
      ii = mod(i,n)+1
      jkl%abc(ii) = jkl%abc(ii) + 1.
    enddo
  end block
  call print_array(jkl%abc, 'doc red')
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
      tmp_arr(ii) = tmp_arr(ii) + 1.
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
      tmp_arr(ii) = tmp_arr(ii) + 1.
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
      tmp_arr(ii) = tmp_arr(ii) + 1.
    enddo
    !$omp end parallel do
  end subroutine test_sub_atomic

  subroutine print_array(tmp_arr, string_)
    real, dimension(:), intent(in) :: tmp_arr
    character(len=*), intent(in), optional :: string_
    character(len=9) :: string

    if(present(string_)) then
      string = string_ // ":"
    else
      string = ""
    endif
    print '(2A,*(F5.0,:,"   "))', string, "  ", tmp_arr
  end subroutine print_array
end program test_reduce
