! MIT License
!
! Copyright (c) 2018 Jim Madge
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

!> Demonstrate the use of the yaml module
Program main
  Use, Intrinsic :: iso_fortran_env, Only : ni=>int32, li=>int64, sp=>real32, &
    dp=>real64, qp=>real128
  Use iso_fortran_env, Only : ou=>output_unit
  Use yaml, Only : yaml_stream
  Implicit None

  !> yaml stream
  Type(yaml_stream) :: stream
  !> local integer
  Integer :: i

  ! Initialise the stream
  Call stream%init(ou, block_indent=1, indent_size=3)

  ! Create a block
  Call stream%new_block('block with comment')
  Call stream%put('integer', 1_li)
  Call stream%put('real', 5.0_qp)
  Call stream%put('complex', (1.0,-3.0_sp))
  Call stream%put('string', 'hello', comment='with optional inline comment')
  Call stream%put('logical true', .true.)
  Call stream%put('logical false', .false.)
  Call stream%put('integer list', [1,2,3])
  Call stream%put('real list', [2.4,5.7,9.0])
  Call stream%put('complex list', [(1.0,1.0_sp),(5.2,-3.4_sp),(-1.0,2.5_sp)])
  Call stream%put('string list', ['aa', 'bb', 'cc'])
  Call stream%put('logical list', [.true.,.false.,.true.])

  ! An object
  Call stream%line_break()
  Call stream%begin_object('Person', comment='A data structure, like a derived type')
  Call stream%put('name', 'Phyllis')
  Call stream%put('age', 36)
  Call stream%put('height', 163.0)
  Call stream%end_object()

  ! New block without a comment
  Call stream%line_break()
  Call stream%new_block()
  ! Create a list
  Call stream%begin_list('list')
  Do i = 1, 3
    Call stream%put('element', i)
    ! A non advancing item, not a new list item
    Call stream%begin_list('sub-list', advnce=.false.)
    Call stream%put('elem', 'a')
    Call stream%put('elem', 'b')
    Call stream%put('elem', 'c')
    Call stream%end_list()
  End Do
  Call stream%end_list()
End program main
