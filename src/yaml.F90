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

!> A module to write Fortran data in valid YAML
Module yaml
  Use, Intrinsic :: iso_fortran_env, Only : ni=>int32, li=>int64, sp=>real32, &
    dp=>real64, qp=>real128
  Implicit None

  Private

  !> Character string to indicate a list item
  Character(len=*), Parameter :: list_str = '- '
  !> Character string following a tag, preceding the value
  Character(len=*), Parameter :: tag_str = ': '
  !> Character string to precede comments
  Character(len=*), Parameter :: comment_str = repeat(' ',6)//'# '
  !> New line character
  Character(len=*), Parameter :: newline = new_line(newline)

  !> Temporary string length
  Integer, Parameter :: tmp_str_len = 80

  !> Normal integer format string
  Character(Len=*), Parameter :: fmt_int_ni = 'g0'
  !> Long integer format string
  Character(Len=*), Parameter ::fmt_int_li = 'g0'
  !> Single precision real format string
  Character(Len=*), Parameter :: fmt_real_sp = 'g0'
  !> Double precision real format string
  Character(Len=*), Parameter :: fmt_real_dp = 'g0'
  !> Quadruple precision real format string
  Character(Len=*), Parameter :: fmt_real_qp = 'g0'

  !> String used to represent a true boolean
  Character(len=*), Parameter :: true_str = 'true'
  !> String used to represent a false boolean
  Character(len=*), Parameter :: false_str = 'false'

  !> YAML stream type, with methods to write to a unit
  Type, Public :: yaml_stream
    Private

    !> Unit number the stream writes to
    Integer :: unit_no
    !> Current indent level
    Integer :: indent = 0
    !> The size in spaces of an indent
    Integer :: indent_size = 2
    !> The number of indent levels to advance in a block (may be 0)
    Integer :: block_indent = 1
    !> Current list level
    Integer :: list_level = 0
    !> Flag for whether the current element is advancing
    Logical :: advnce = .true.
  Contains
    Private
    Procedure, Public :: init

    Procedure, Public :: new_block
    Procedure, Public :: line_break

    Procedure :: put_int_ni, put_int_li
    Procedure :: put_real_sp, put_real_dp, put_real_qp
    Procedure :: put_complex_sp, put_complex_dp, put_complex_qp
    Procedure :: put_str
    Procedure :: put_logical
    Generic, Public :: put => put_int_ni, put_int_li, &
                              put_real_sp, put_real_dp, put_real_qp, &
                              put_complex_sp, put_complex_dp, put_complex_qp, &
                              put_str, put_logical

    Procedure :: put_int_list_ni, put_int_list_li
    Procedure :: put_real_list_sp, put_real_list_dp, put_real_list_qp
    Procedure :: put_complex_list_sp, put_complex_list_dp, put_complex_list_qp
    Procedure :: put_str_list
    Procedure :: put_logical_list
    Generic, Public :: put => put_int_list_ni, put_int_list_li, &
                              put_real_list_sp, put_real_list_dp, put_real_list_qp, &
                              put_complex_list_sp, put_complex_list_dp, put_complex_list_qp, &
                              put_str_list, put_logical_list

    Procedure, Public :: begin_object
    Procedure, Public :: end_object

    Procedure, Public :: begin_list
    Procedure, Public :: end_list

    Procedure :: indent_str
  End Type yaml_stream

Contains

  !> Initialisation routine for a yaml stream
  Subroutine init(T, unit_no, block_indent, indent_size)
    Class(yaml_stream) :: T
    !> Unit number the stream writes to
    Integer, Intent(In) :: unit_no
    !> The number of indent levels to advance in a block (may be 0)
    Integer, Intent(In), Optional :: block_indent
    !> The size in spaces of an indent
    Integer, Intent(In), Optional :: indent_size

    T%unit_no = unit_no

    ! Set indent options
    If (present(block_indent)) Then
      T%block_indent = block_indent
    End If
    If (present(indent_size)) Then
      T%indent_size = indent_size
    End If
  End Subroutine init

  !> Create a new yaml block
  Subroutine new_block(T, comment)
    Class(yaml_stream) :: T
    !> Block comment
    Character(Len=*), Intent(In), Optional :: comment

    ! Reset indent to block indent
    T%indent = T%block_indent

    ! Write block and optional comment
    If (present(comment)) Then
      write(T%unit_no,'(2a)') '---', comment_str//comment
    Else
      write(T%unit_no,'(a)') '---'
    End If
  End Subroutine new_block

  !> Leave a blank line
  Subroutine line_break(T)
    Class(yaml_stream) :: T

    write(T%unit_no,*)
  End Subroutine line_break

  !> Write a normal integer
  Subroutine put_int_ni(T, tag, i, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the datum
    Character(Len=*), Intent(In) :: tag
    !> Datum to write
    Integer(Kind=ni), Intent(In) :: i
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> i cast to a string
    Character(Len=tmp_str_len) :: str

    ! Write datum
    Write(str,'('//fmt_int_ni//')') i
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_int_ni

  !> Write a long integer
  Subroutine put_int_li(T, tag, i, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the datum
    Character(Len=*), Intent(In) :: tag
    !> Datum to write
    Integer(Kind=li), Intent(In) :: i
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> i cast to a string
    Character(Len=tmp_str_len) :: str

    ! Write datum
    Write(str,'('//fmt_int_li//')') i
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_int_li

  !> Write a single precision real
  Subroutine put_real_sp(T, tag, x, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the datum
    Character(Len=*), Intent(In) :: tag
    !> Datum to write
    Real(Kind=sp), Intent(In) :: x
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> x cast to a string
    Character(Len=tmp_str_len) :: str

    ! Write datum
    Write(str,'('//fmt_real_sp//')') x
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_real_sp

  !> Write a double precision real
  Subroutine put_real_dp(T, tag, x, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the datum
    Character(Len=*), Intent(In) :: tag
    !> Datum to write
    Real(Kind=dp), Intent(In) :: x
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> x cast to a string
    Character(Len=tmp_str_len) :: str

    ! Write datum
    Write(str,'('//fmt_real_dp//')') x
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_real_dp

  !> Write a quadruple precision real
  Subroutine put_real_qp(T, tag, x, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the datum
    Character(Len=*), Intent(In) :: tag
    !> Datum to write
    Real(Kind=qp), Intent(In) :: x
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> x cast to a string
    Character(Len=tmp_str_len) :: str

    ! Write datum
    Write(str,'('//fmt_real_qp//')') x
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_real_qp

  !> Write a single precision complex number
  Subroutine put_complex_sp(T, tag, z, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the datum
    Character(Len=*), Intent(In) :: tag
    !> Datum to write
    Complex(Kind=sp), Intent(In) :: z
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> z cast to a string
    Character(Len=tmp_str_len) :: str
    !> Write format
    Character(Len=:), Allocatable :: wfmt

    ! Write datum
    wfmt = '(a,'//fmt_real_sp//',a,'//fmt_real_sp//',a)'
    Write(str,wfmt) '!complex {r'//tag_str, real(z), ', i'//tag_str, aimag(z), '}'
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_complex_sp

  !> Write a double precision complex number
  Subroutine put_complex_dp(T, tag, z, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the datum
    Character(Len=*), Intent(In) :: tag
    !> Datum to write
    Complex(Kind=dp), Intent(In) :: z
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> z cast to a string
    Character(Len=tmp_str_len) :: str
    !> Write format
    Character(Len=:), Allocatable :: wfmt

    ! Write datum
    wfmt = '(a,'//fmt_real_dp//',a,'//fmt_real_dp//',a)'
    Write(str,wfmt) '!complex {r'//tag_str, real(z), ', i'//tag_str, aimag(z), '}'
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_complex_dp

  !> Write a quadruple precision complex number
  Subroutine put_complex_qp(T, tag, z, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the datum
    Character(Len=*), Intent(In) :: tag
    !> Datum to write
    Complex(Kind=qp), Intent(In) :: z
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> z cast to a string
    Character(Len=tmp_str_len) :: str
    !> Write format
    Character(Len=:), Allocatable :: wfmt

    ! Write datum
    wfmt = '(a,'//fmt_real_qp//',a,'//fmt_real_qp//',a)'
    Write(str,wfmt) '!complex {r'//tag_str, real(z), ', i'//tag_str, aimag(z), '}'
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_complex_qp

  !> Write a string
  Subroutine put_str(T, tag, str, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the datum
    Character(Len=*), Intent(In) :: tag
    !> Datum to write
    Character(Len=*), Intent(In) :: str
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    ! Set advance
    If (Present(advnce)) Then
      T%advnce = advnce
    End If
    ! Write datum
    If (present(comment)) Then
      write(T%unit_no,'(4a)') T%indent_str(), tag//tag_str, trim(str), comment_str//comment
    Else
      write(T%unit_no,'(3a)') T%indent_str(), tag//tag_str, trim(str)
    End If
  End Subroutine put_str

  !> Write a boolean
  Subroutine put_logical(T, tag, l, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the datum
    Character(Len=*), Intent(In) :: tag
    !> Datum to write
    Logical, Intent(In) :: l
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> x cast to a string
    Character(Len=:), Allocatable :: str

    ! Write datum
    If (l) Then
      str = true_str
    Else
      str = false_str
    End If
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_logical

  !> Write a normal integer list
  Subroutine put_int_list_ni(T, tag, ilist, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the data
    Character(Len=*), Intent(In) :: tag
    !> Data to write
    Integer(Kind=ni), Intent(In), Dimension(:) :: ilist
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> Length of list
    Integer :: l
    !> Local integer
    Integer :: i
    !> Temporary strings
    Character(Len=:), Allocatable :: str
    Character(Len=tmp_str_len) :: tmp

    l = size(ilist)
    str = '['
    Do i = 1, l
      write(tmp,'('//fmt_int_ni//')') ilist(i)
      str = str//trim(tmp)
      If (i == l) Then
        str = str//']'
      Else
        str = str//trim(tmp)//', '
      End If
    End Do

    ! Write data
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_int_list_ni

  !> Write a long integer list
  Subroutine put_int_list_li(T, tag, ilist, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the data
    Character(Len=*), Intent(In) :: tag
    !> Data to write
    Integer(Kind=li), Intent(In), Dimension(:) :: ilist
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> Length of list
    Integer :: l
    !> Local integer
    Integer :: i
    !> Temporary strings
    Character(Len=:), Allocatable :: str
    Character(Len=tmp_str_len) :: tmp

    l = size(ilist)
    str = '['
    Do i = 1, l
      write(tmp,'('//fmt_int_li//')') ilist(i)
      str = str//trim(tmp)
      If (i == l) Then
        str = str//']'
      Else
        str = str//trim(tmp)//', '
      End If
    End Do

    ! Write data
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_int_list_li

  !> Write a list of single precision reals
  Subroutine put_real_list_sp(T, tag, xlist, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the data
    Character(Len=*), Intent(In) :: tag
    !> Data to write
    Real(Kind=sp), Intent(In), Dimension(:) :: xlist
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> Length of list
    Integer :: l
    !> Local integer
    Integer :: i
    !> Temporary strings
    Character(Len=:), Allocatable :: str
    Character(Len=tmp_str_len) :: tmp

    l = size(xlist)
    str = '['
    Do i = 1, l
      write(tmp,'('//fmt_real_sp//')') xlist(i)
      str = str//trim(tmp)
      If (i == l) Then
        str = str//']'
      Else
        str = str//', '
      End If
    End Do

    ! Write data
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_real_list_sp

  !> Write a list of double precision reals
  Subroutine put_real_list_dp(T, tag, xlist, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the data
    Character(Len=*), Intent(In) :: tag
    !> Data to write
    Real(Kind=dp), Intent(In), Dimension(:) :: xlist
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> Length of list
    Integer :: l
    !> Local integer
    Integer :: i
    !> Temporary strings
    Character(Len=:), Allocatable :: str
    Character(Len=tmp_str_len) :: tmp

    l = size(xlist)
    str = '['
    Do i = 1, l
      write(tmp,'('//fmt_real_sp//')') xlist(i)
      str = str//trim(tmp)
      If (i == l) Then
        str = str//']'
      Else
        str = str//', '
      End If
    End Do

    ! Write data
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_real_list_dp

  !> Write a list of quadruple precision reals
  Subroutine put_real_list_qp(T, tag, xlist, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the data
    Character(Len=*), Intent(In) :: tag
    !> Data to write
    Real(Kind=qp), Intent(In), Dimension(:) :: xlist
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> Length of list
    Integer :: l
    !> Local integer
    Integer :: i
    !> Temporary strings
    Character(Len=:), Allocatable :: str
    Character(Len=tmp_str_len) :: tmp

    l = size(xlist)
    str = '['
    Do i = 1, l
      write(tmp,'('//fmt_real_qp//')') xlist(i)
      str = str//trim(tmp)
      If (i == l) Then
        str = str//']'
      Else
        str = str//', '
      End If
    End Do

    ! Write data
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_real_list_qp

  !> Write a list of single precision complex numbers
  Subroutine put_complex_list_sp(T, tag, zlist, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the data
    Character(Len=*), Intent(In) :: tag
    !> Data to write
    Complex(Kind=sp), Intent(In), Dimension(:) :: zlist
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> Length of list
    Integer :: l
    !> Local integer
    Integer :: i
    !> Temporary strings
    Character(Len=:), Allocatable :: str
    Character(Len=tmp_str_len) :: tmp
    !> Local indent
    Integer :: local_indent
    !> Write format
    Character(Len=:), Allocatable :: wfmt

    l = size(zlist)
    local_indent = len(tag) + len(tag_str) + 1
    wfmt = '(a,'//fmt_real_sp//',a,'//fmt_real_sp//',a)'
    str = '['
    Do i = 1, l
      Write(tmp,wfmt) '!complex {r'//tag_str, real(zlist(i)), ', i'//tag_str, aimag(zlist(i)), '}'
      str = str//trim(tmp)
      If (i == l) Then
        str = str//']'
      Else
        str = str//', '//newline//T%indent_str()//repeat(' ', local_indent)
      End If
    End Do

    ! Write data
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_complex_list_sp

  !> Write a list of double precision complex numbers
  Subroutine put_complex_list_dp(T, tag, zlist, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the data
    Character(Len=*), Intent(In) :: tag
    !> Data to write
    Complex(Kind=dp), Intent(In), Dimension(:) :: zlist
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> Length of list
    Integer :: l
    !> Local integer
    Integer :: i
    !> Temporary strings
    Character(Len=:), Allocatable :: str
    Character(Len=tmp_str_len) :: tmp
    !> Local indent
    Integer :: local_indent
    !> Write format
    Character(Len=:), Allocatable :: wfmt

    l = size(zlist)
    local_indent = len(tag) + len(tag_str) + 1
    wfmt = '(a,'//fmt_real_dp//',a,'//fmt_real_dp//',a)'
    str = '['
    Do i = 1, l
      Write(tmp,wfmt) '!complex {r'//tag_str, real(zlist(i)), ', i'//tag_str, aimag(zlist(i)), '}'
      str = str//trim(tmp)
      If (i == l) Then
        str = str//']'
      Else
        str = str//', '//newline//T%indent_str()//repeat(' ', local_indent)
      End If
    End Do

    ! Write data
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_complex_list_dp

  !> Write a list of quadruple precision complex numbers
  Subroutine put_complex_list_qp(T, tag, zlist, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the data
    Character(Len=*), Intent(In) :: tag
    !> Data to write
    Complex(Kind=qp), Intent(In), Dimension(:) :: zlist
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> Length of list
    Integer :: l
    !> Local integer
    Integer :: i
    !> Temporary strings
    Character(Len=:), Allocatable :: str
    Character(Len=tmp_str_len) :: tmp
    !> Local indent
    Integer :: local_indent
    !> Write format
    Character(Len=:), Allocatable :: wfmt

    l = size(zlist)
    local_indent = len(tag) + len(tag_str) + 1
    wfmt = '(a,'//fmt_real_qp//',a,'//fmt_real_qp//',a)'
    str = '['
    Do i = 1, l
      Write(tmp,wfmt) '!complex {r'//tag_str, real(zlist(i)), ', i'//tag_str, aimag(zlist(i)), '}'
      str = str//trim(tmp)
      If (i == l) Then
        str = str//']'
      Else
        str = str//', '//newline//T%indent_str()//repeat(' ', local_indent)
      End If
    End Do

    ! Write data
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_complex_list_qp

  !> Write a list of strings
  Subroutine put_str_list(T, tag, strlist, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the data
    Character(Len=*), Intent(In) :: tag
    !> Data to write
    Character(Len=*), Intent(In), Dimension(:) :: strlist
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> Length of list
    Integer :: l
    !> Local integer
    Integer :: i
    !> Temporary strings
    Character(Len=:), Allocatable :: str

    l = size(strlist)
    str = '['
    Do i = 1, l
      str = str//strlist(i)
      If (i == l) Then
        str = str//']'
      Else
        str = str//', '
      End If
    End Do

    ! Write data
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_str_list

  !> Write a list of booleans
  Subroutine put_logical_list(T, tag, llist, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the data
    Character(Len=*), Intent(In) :: tag
    !> Data to write
    Logical, Intent(In), Dimension(:) :: llist
    !> True if advancing a list
    Logical, Intent(In), Optional :: advnce
    !> Inline comment
    Character(Len=*), Intent(In), Optional :: comment

    !> Length of list
    Integer :: l
    !> Local integer
    Integer :: i
    !> Temporary strings
    Character(Len=:), Allocatable :: str

    l = size(llist)
    str = '['
    Do i = 1, l
      If (llist(i)) Then
        str = str//true_str
      Else
        str = str//false_str
      End If

      If (i == l) Then
        str = str//']'
      Else
        str = str//', '
      End If
    End Do

    ! Write data
    Call T%put_str(tag, str, advnce, comment)
  End Subroutine put_logical_list

  !> Begin an object
  Subroutine begin_object(T, tag, comment)
    Class(yaml_stream) :: T
    !> Tag for the list
    Character(Len=*), Intent(In) :: tag
    !> Optional in line comment
    Character(Len=*), Intent(In), Optional :: comment

    ! Write object header
    If (present(comment)) Then
      write(T%unit_no,'(3a)') T%indent_str(), tag//':', comment_str//comment
    Else
      write(T%unit_no,'(2a)') T%indent_str(), tag//':'
    End If

    T%indent = T%indent + 1
  End Subroutine begin_object

  !> End an object
  Subroutine end_object(T)
    Class(yaml_stream) :: T

    T%indent = T%indent - 1
  End Subroutine end_object

  !> Begin a list
  Subroutine begin_list(T, tag, advnce, comment)
    Class(yaml_stream) :: T
    !> Tag for the list
    Character(Len=*), Intent(In) :: tag
    !> True if advancing an embedding list
    Logical, Intent(In), Optional :: advnce
    !> Optional in line comment
    Character(Len=*), Intent(In), Optional :: comment

    If (Present(advnce)) Then
      T%advnce = advnce
    End If

    ! Write list header
    If (present(comment)) Then
      write(T%unit_no,'(3a)') T%indent_str(), tag//':', comment_str//comment
    Else
      write(T%unit_no,'(2a)') T%indent_str(), tag//':'
    End If

    T%indent = T%indent + 1
    T%list_level = T%list_level + 1
  End Subroutine begin_list

  !> End a list
  Subroutine end_list(T)
    Class(yaml_stream) :: T

    T%indent = T%indent - 1
    T%list_level = T%list_level - 1
  End Subroutine end_list

  !> Determine the string to be prepended to data for correct indent,
  !> including a hyphen for list items
  Function indent_str(T) Result(str)
    Class(yaml_stream) :: T
    !> Indent string
    Character(len=:), allocatable :: str

    ! Initial indent
    str = repeat(' ', T%indent_size*T%indent)

    If (T%list_level > 0) Then
      ! Extra indenting for list items
      str = str//repeat(' ', len(list_str)*(T%list_level-1))
      If (T%advnce) Then
        ! Advancing list item
        str = str//list_str
      Else
        ! Non-advancing item
        str = str//repeat(' ', len(list_str))
      End If
    End If
    T%advnce = .true.
  End Function indent_str
End Module yaml
