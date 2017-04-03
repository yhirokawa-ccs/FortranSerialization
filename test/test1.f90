program test1
  implicit none

  call backup
  call restore

contains
  subroutine backup
    use fortran_serialization
    implicit none
    integer, parameter      :: iounit = 42
    integer, allocatable    :: a(:)
    real(8)                 :: b
    complex(8), allocatable :: c(:,:)

    allocate(a(30))
    a = 3

    open(iounit, file='./backup.dat', status='replace', form='unformatted')

    call save_value(iounit, a)
    call save_value(iounit, b)
    call save_value(iounit, c)

    close(iounit)
  end subroutine

  subroutine restore
    use fortran_serialization
    implicit none
    integer, parameter      :: iounit = 42
    integer, allocatable    :: a(:)
    real(8)                 :: b
    complex(8), allocatable :: c(:,:)
    integer :: i

    open(iounit, file='./backup.dat', status='old', form='unformatted')

    call load_value(iounit, a)
    call load_value(iounit, b)
    call load_value(iounit, c)

    close(iounit)

    if (.NOT. allocated(a)) then
      print *, 'Error: array is not allocated'
      stop 1
    end if

    if (allocated(c)) then
      print *, 'Error: array is allocated'
      stop 1
    end if

    if (sum(a) /= (3 * size(a))) then
      print *, 'Error: load array data'
      stop 1
    end if
  end subroutine
end program
