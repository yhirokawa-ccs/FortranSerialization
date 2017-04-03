# FortranSerialization

A binary-type simple serialization module for Fortran (2003 or later versions).


## What is "Serialization"?

"Serialization" means that the process of translating data structures or object state into a format that can be stored to data storage. (e.g. save to file or memory buffer. transmitted to other machine across network)
This module can be saved that binary-type serialized simple variable to a file or IO unit.

In sometimes, extra large scale simulation requires that data backup for recovering from system failure. This is generally called "Checkpoint/Restart" in parallel and distributed computing.
This module helps Checkpoint/Restart strategy.


## Example/Usage

This module has three generic routines.
You can install source file to your applications.

```Fortran
module fortran_serialization
  logical, parameter :: SERIALIZE_SAVE_MODE = .TRUE.
  logical, parameter :: SERIALIZE_LOAD_MODE = .FALSE.

  subrouine save_value(iounit, target_value)            ! save to IO unit
  subrouine load_value(iounit, target_value)            ! load from IO unit
  subrouine serialize_value(mode, iounit, target_value) ! save or load
end module

! logical, intent(in)    :: mode          ! .TRUE. = save, .FALSE. = load
! integer, intent(in)    :: iounit
!       *, intent(inout) :: target_value
```

This supports "allocatable array" and "scalar value".
Allocatable array can be stored without concern for variables is allocated.


### Default supported data types

- scalar only
    - `character(:)`, `logical`
- scalar, 1D, 2D, 3D, and 4D array
    - `real`, `real(8) (double precision)`, `complex`, `complex(8) (double precision complex)`, `integer`


### Basic usage : `save_value` and `load_value`

```Fortran
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

  open(iounit, file='./backup.dat', status='old', form='unformatted')

  call load_value(iounit, a)
  call load_value(iounit, b)
  call load_value(iounit, c)

  close(iounit)

  print *, allocated(a) ! TRUE
  print *, allocated(c) ! FALSE
end subroutine
```

### Recomended usage : `serialize_value`

```Fortran
program main
  implicit none
  integer, parameter :: iounit = 42

  ...

  open(iounit, file='./backup.dat', status='replace', form='unformatted')
  call checkpoint(iounit)
  close(iounit)

  ...

contains
  subroutine checkpoint(iounit)
    use fortran_serialization, only: SERIALIZE_SAVE_MODE
    implicit none
    integer, intent(in) :: iounit
    call check_restart(SERIALIZE_SAVE_MODE, iounit)
  end subroutine

  subroutine restart(iounit)
    use fortran_serialization, only: SERIALIZE_LOAD_MODE
    implicit none
    integer, intent(in) :: iounit
    call check_restart(SERIALIZE_LOAD_MODE, iounit)
  end subroutine

  subroutine check_restart(mode, iounit)
    use fortran_serialization
    use global_variables ! target variables for backup/restore
    implicit none
    logical, intent(in) :: mode   ! SERIALIZE_SAVE_MODE or SERIALIZE_LOAD_MODE
    integer, intent(in) :: iounit

    call serialize_value(mode, iounit, a)
    call serialize_value(mode, iounit, b)
    call serialize_value(mode, iounit, c)
    ...
  end subroutine
end program
```


## Generating and Testing

A library is generated with fortran preprocessor, original file (before preprocessing) exists to src directory.
You can easily add unsupported data types.

    $ mkdir build-temp
    $ cd build-temp
    $ FFLAGS="-std=f2003 -Wall -pedantic-errors" cmake ${FortranSerializationSourceDirectory}
    $ make
    $ ctest
    $ ls *.f90
    fortran_serialization.f90


## Supported (Tested) compilers

- GNU Fortran 4.8.5
- Intel Fortran 17.0.2


## License

FortranSerialization is available under Apache License version 2.0

    Copyright 2017 Yuta Hirokawa (University of Tsukuba, Japan)

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
