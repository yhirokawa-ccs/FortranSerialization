module global_variables
  implicit none

  integer, allocatable :: a(:)
  character(128)       :: b
  real(8), allocatable :: c(:,:)
end module

program test2
  use global_variables
  implicit none
  integer, parameter :: iounit = 42
  integer, parameter :: NSIZE = 30

  b = 'Good BEAF'
  allocate(a(NSIZE))
  allocate(c(NSIZE,NSIZE*NSIZE))

  open(iounit, file='./backup.dat', status='replace', form='unformatted')
  call test_serialize(iounit)
  close(iounit)

  deallocate(a, c)

  open(iounit, file='./backup.dat', status='old', form='unformatted')
  call test_deserialize(iounit)
  close(iounit)

  if (b /= 'Good BEAF') then
    print *, 'Error: data load error, string (b)'
    stop 1
  end if

  if (.not. allocated(a)) then
    print *, 'Error: allocation error (a)'
    stop 1
  end if

  if (.not. allocated(c)) then
    print *, 'Error: allocation error (c)'
    stop 1
  end if

  if (size(a) /= NSIZE) then
    print *, 'Error: data load error, array size mismatch (a)'
    stop 1
  end if

  if (size(c) /= NSIZE*NSIZE*NSIZE .or. size(c,1) /= NSIZE .or. size(c,2) /= NSIZE*NSIZE) then
    print *, 'Error: data load error, array size mismatch (c)'
    stop 1
  end if

contains
  subroutine test_serialize(iounit)
    use serialization, only: SERIALIZE_SAVE_MODE
    implicit none
    integer, intent(in) :: iounit
    call test_serialization(SERIALIZE_SAVE_MODE, iounit)
  end subroutine

  subroutine test_deserialize(iounit)
    use serialization, only: SERIALIZE_LOAD_MODE
    implicit none
    integer, intent(in) :: iounit
    call test_serialization(SERIALIZE_LOAD_MODE, iounit)
  end subroutine

  subroutine test_serialization(mode, iounit)
    use serialization
    use global_variables
    implicit none
    logical, intent(in) :: mode
    integer, intent(in) :: iounit

    call serialize_value(mode, iounit, a)
    call serialize_value(mode, iounit, b)
    call serialize_value(mode, iounit, c)
  end subroutine
end program
