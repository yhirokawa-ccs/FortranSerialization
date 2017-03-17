!
!  Copyright 2017 Yuta Hirokawa (University of Tsukuba)
!
!  Licensed under the Apache License, Version 2.0 (the "License");
!  you may not use this file except in compliance with the License.
!  You may obtain a copy of the License at
!
!      http://www.apache.org/licenses/LICENSE-2.0
!
!  Unless required by applicable law or agreed to in writing, software
!  distributed under the License is distributed on an "AS IS" BASIS,
!  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!  See the License for the specific language governing permissions and
!  limitations under the License.
!
module serialization
  implicit none

  public :: SERIALIZE_SAVE_MODE
  public :: SERIALIZE_LOAD_MODE

  public :: save_value
  public :: load_value
  public :: serialize_value

! NOTE: Don't insert a space between preprocessor arguments.

#ifdef __GFORTRAN__
#define PASTE(a) a
#define CONCAT(a,b) PASTE(a)b
#else
#define PASTE(a) a##b
#define CONCAT(a,b) PASTE(a,b)
#endif

#define DECLARE_SCALAR_INTERFACE(SAVE_LOAD,ROUTINE_NAME) \
    module procedure CONCAT(CONCAT(SAVE_LOAD,_scalar_),ROUTINE_NAME)

#define DECLARE_ARRAY_INTERFACE(SAVE_LOAD,ROUTINE_NAME) \
    DECLARE_SCALAR_INTERFACE(SAVE_LOAD,ROUTINE_NAME);;\
    module procedure CONCAT(CONCAT(SAVE_LOAD,_array1d_),ROUTINE_NAME);;\
    module procedure CONCAT(CONCAT(SAVE_LOAD,_array2d_),ROUTINE_NAME);;\
    module procedure CONCAT(CONCAT(SAVE_LOAD,_array3d_),ROUTINE_NAME);;\
    module procedure CONCAT(CONCAT(SAVE_LOAD,_array4d_),ROUTINE_NAME)

#define IMPLEMENT_SCALAR_INTERFACE(ROUTINE_NAME,DATA_TYPE) \
    subroutine CONCAT(save_scalar_,ROUTINE_NAME)(iounit, val);;\
      implicit none;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , intent(in) :: val;;\
      write(iounit) val;;\
    end subroutine;;\
    subroutine CONCAT(load_scalar_,ROUTINE_NAME)(iounit, val);;\
      implicit none;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , intent(out) :: val;;\
      read(iounit) val;;\
    end subroutine;;\
    subroutine CONCAT(serialize_scalar_,ROUTINE_NAME)(mode, iounit, val);;\
      implicit none;;\
      logical, intent(in) :: mode;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , intent(inout) :: val;;\
      if (mode) then;;\
        call CONCAT(save_scalar_,ROUTINE_NAME)(iounit, val);;\
      else;;\
        call CONCAT(load_scalar_,ROUTINE_NAME)(iounit, val);;\
      end if;;\
    end subroutine

#define IMPLEMENT_ARRAY_INTERFACE(ROUTINE_NAME,DATA_TYPE) \
    IMPLEMENT_SCALAR_INTERFACE(ROUTINE_NAME,DATA_TYPE);;\
    subroutine CONCAT(save_array1d_,ROUTINE_NAME)(iounit, val);;\
      implicit none;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(in) :: val(:);;\
      logical :: f;;\
      f = allocated(val);;\
      write(iounit) f;;\
      if (f) then;;\
        write(iounit) lbound(val), ubound(val);;\
        write(iounit) val(:);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(load_array1d_,ROUTINE_NAME)(iounit, val);;\
      implicit none;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(out) :: val(:);;\
      logical :: f;;\
      integer :: lb1, ub1;;\
      read(iounit) f;;\
      if (f) then;;\
        read(iounit) lb1, ub1;;\
        allocate(val(lb1:ub1));;\
        read(iounit) val(:);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(serialize_array1d_,ROUTINE_NAME)(mode, iounit, val);;\
      implicit none;;\
      logical, intent(in) :: mode;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(inout) :: val(:);;\
      if (mode) then;;\
        call CONCAT(save_array1d_,ROUTINE_NAME)(iounit, val);;\
      else;;\
        call CONCAT(load_array1d_,ROUTINE_NAME)(iounit, val);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(save_array2d_,ROUTINE_NAME)(iounit, val);;\
      implicit none;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(in) :: val(:,:);;\
      logical :: f;;\
      f = allocated(val);;\
      write(iounit) f;;\
      if (f) then;;\
        write(iounit) lbound(val,1), ubound(val,1);;\
        write(iounit) lbound(val,2), ubound(val,2);;\
        write(iounit) val(:,:);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(load_array2d_,ROUTINE_NAME)(iounit, val);;\
      implicit none;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(out) :: val(:,:);;\
      logical :: f;;\
      integer :: lb1, ub1;;\
      integer :: lb2, ub2;;\
      read(iounit) f;;\
      if (f) then;;\
        read(iounit) lb1, ub1;;\
        read(iounit) lb2, ub2;;\
        allocate(val(lb1:ub1,lb2:ub2));;\
        read(iounit) val(:,:);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(serialize_array2d_,ROUTINE_NAME)(mode, iounit, val);;\
      implicit none;;\
      logical, intent(in) :: mode;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(inout) :: val(:,:);;\
      if (mode) then;;\
        call CONCAT(save_array2d_,ROUTINE_NAME)(iounit, val);;\
      else;;\
        call CONCAT(load_array2d_,ROUTINE_NAME)(iounit, val);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(save_array3d_,ROUTINE_NAME)(iounit, val);;\
      implicit none;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(in) :: val(:,:,:);;\
      logical :: f;;\
      f = allocated(val);;\
      write(iounit) f;;\
      if (f) then;;\
        write(iounit) lbound(val,1), ubound(val,1);;\
        write(iounit) lbound(val,2), ubound(val,2);;\
        write(iounit) lbound(val,3), ubound(val,3);;\
        write(iounit) val(:,:,:);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(load_array3d_,ROUTINE_NAME)(iounit, val);;\
      implicit none;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(out) :: val(:,:,:);;\
      logical :: f;;\
      integer :: lb1, ub1;;\
      integer :: lb2, ub2;;\
      integer :: lb3, ub3;;\
      read(iounit) f;;\
      if (f) then;;\
        read(iounit) lb1, ub1;;\
        read(iounit) lb2, ub2;;\
        read(iounit) lb3, ub3;;\
        allocate(val(lb1:ub1,lb2:ub2,lb3:ub3));;\
        read(iounit) val(:,:,:);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(serialize_array3d_,ROUTINE_NAME)(mode, iounit, val);;\
      implicit none;;\
      logical, intent(in) :: mode;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(inout) :: val(:,:,:);;\
      if (mode) then;;\
        call CONCAT(save_array3d_,ROUTINE_NAME)(iounit, val);;\
      else;;\
        call CONCAT(load_array3d_,ROUTINE_NAME)(iounit, val);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(save_array4d_,ROUTINE_NAME)(iounit, val);;\
      implicit none;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(in) :: val(:,:,:,:);;\
      logical :: f;;\
      f = allocated(val);;\
      write(iounit) f;;\
      if (f) then;;\
        write(iounit) lbound(val,1), ubound(val,1);;\
        write(iounit) lbound(val,2), ubound(val,2);;\
        write(iounit) lbound(val,3), ubound(val,3);;\
        write(iounit) lbound(val,4), ubound(val,4);;\
        write(iounit) val(:,:,:,:);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(load_array4d_,ROUTINE_NAME)(iounit, val);;\
      implicit none;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(out) :: val(:,:,:,:);;\
      logical :: f;;\
      integer :: lb1, ub1;;\
      integer :: lb2, ub2;;\
      integer :: lb3, ub3;;\
      integer :: lb4, ub4;;\
      read(iounit) f;;\
      if (f) then;;\
        read(iounit) lb1, ub1;;\
        read(iounit) lb2, ub2;;\
        read(iounit) lb3, ub3;;\
        read(iounit) lb4, ub4;;\
        allocate(val(lb1:ub1,lb2:ub2,lb3:ub3,lb4:ub4));;\
        read(iounit) val(:,:,:,:);;\
      end if;;\
    end subroutine;;\
    subroutine CONCAT(serialize_array4d_,ROUTINE_NAME)(mode, iounit, val);;\
      implicit none;;\
      logical, intent(in) :: mode;;\
      integer, intent(in) :: iounit;;\
      DATA_TYPE , allocatable, intent(inout) :: val(:,:,:,:);;\
      if (mode) then;;\
        call CONCAT(save_array4d_,ROUTINE_NAME)(iounit, val);;\
      else;;\
        call CONCAT(load_array4d_,ROUTINE_NAME)(iounit, val);;\
      end if;;\
    end subroutine

  logical, parameter :: SERIALIZE_SAVE_MODE = .TRUE.
  logical, parameter :: SERIALIZE_LOAD_MODE = .FALSE.

  interface save_value
    DECLARE_SCALAR_INTERFACE(save,character)
    DECLARE_SCALAR_INTERFACE(save,logical)

    DECLARE_ARRAY_INTERFACE(save,real)
    DECLARE_ARRAY_INTERFACE(save,real8)
    DECLARE_ARRAY_INTERFACE(save,complex)
    DECLARE_ARRAY_INTERFACE(save,complex8)
    DECLARE_ARRAY_INTERFACE(save,integer)
  end interface

  interface load_value
    DECLARE_SCALAR_INTERFACE(load,character)
    DECLARE_SCALAR_INTERFACE(load,logical)

    DECLARE_ARRAY_INTERFACE(load,real)
    DECLARE_ARRAY_INTERFACE(load,real8)
    DECLARE_ARRAY_INTERFACE(load,complex)
    DECLARE_ARRAY_INTERFACE(load,complex8)
    DECLARE_ARRAY_INTERFACE(load,integer)
  end interface

  ! serialize or restore
  interface serialize_value
    DECLARE_SCALAR_INTERFACE(serialize,character)
    DECLARE_SCALAR_INTERFACE(serialize,logical)

    DECLARE_ARRAY_INTERFACE(serialize,real)
    DECLARE_ARRAY_INTERFACE(serialize,real8)
    DECLARE_ARRAY_INTERFACE(serialize,complex)
    DECLARE_ARRAY_INTERFACE(serialize,complex8)
    DECLARE_ARRAY_INTERFACE(serialize,integer)
  end interface

contains
  IMPLEMENT_SCALAR_INTERFACE(character,character(*))
  IMPLEMENT_SCALAR_INTERFACE(logical,logical)

  IMPLEMENT_ARRAY_INTERFACE(real,real)
  IMPLEMENT_ARRAY_INTERFACE(real8,double precision)
  IMPLEMENT_ARRAY_INTERFACE(complex,complex)
  IMPLEMENT_ARRAY_INTERFACE(complex8,complex(kind(0d0)))
  IMPLEMENT_ARRAY_INTERFACE(integer,integer)
end module
