! Copyright (c) 2026 rf742
! SPDX-License-Identifier: MIT

!> A module for strings, containing conversion, case changing, and checking prefixes and suffixes.
module red_string
    use red_vector, only: string_vector
    implicit none
    private

    public :: to_lower, to_upper, tokenize, str_to_int, str_to_real, str_to_logical, int_to_str, real_to_str, starts_with, ends_with

contains

    pure function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i, ic

        do i = 1, len_trim(str)
            ic = iachar(str(i:i))
            if (ic >= iachar('A') .and. ic <= iachar('Z')) then
                lower_str(i:i) = achar(ic + 32)
            else
                lower_str(i:i) = str(i:i)
            end if
        end do
        if (len_trim(str) < len(str)) then
            lower_str(len_trim(str)+1:) = ' '
        end if
    end function to_lower

    pure function to_upper(str) result(upper_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: upper_str
        integer :: i, ic

        do i = 1, len_trim(str)
            ic = iachar(str(i:i))
            if (ic >= iachar('a') .and. ic <= iachar('z')) then
                upper_str(i:i) = achar(ic - 32)
            else
                upper_str(i:i) = str(i:i)
            end if
        end do
        if (len_trim(str) < len(str)) then
            upper_str(len_trim(str)+1:) = ' '
        end if
    end function to_upper

    pure function int_to_str(val) result(str)
        integer, intent(in) :: val
        character(len=:), allocatable :: str
        character(len=32) :: buffer
        write(buffer, '(I0)') val
        str = trim(adjustl(buffer))
    end function int_to_str

    pure function real_to_str(val, fmt) result(str)
        real, intent(in) :: val
        character(len=*), intent(in), optional :: fmt
        character(len=:), allocatable :: str
        character(len=64) :: buffer
        if (present(fmt)) then
            write(buffer, fmt) val
        else
            write(buffer, *) val
        end if
        str = trim(adjustl(buffer))
    end function real_to_str

    ! Tokenize a string into an array of strings,
    ! respecting a given delimiter (default space) and keeping double-quoted strings intact.
    subroutine tokenize(str, tokens, count, delimiter)
        use red_datastructures, only: string_vector
        character(len=*), intent(in) :: str
        type(string_vector), intent(out) :: tokens
        integer, intent(out) :: count
        character(len=1), intent(in), optional :: delimiter

        character(len=len(str)) :: token
        integer :: i, tok_len
        logical :: in_quotes
        character(len=1) :: delim
        logical :: last_was_delim
        
        if (present(delimiter)) then
            delim = delimiter
        else
            delim = ' '
        end if
        
        count = 0
        
        last_was_delim = .false.
        i = 1
        do while (i <= len_trim(str) .or. last_was_delim)
            last_was_delim = .false.
            
            ! Skip consecutive delimiters if the delimiter is space
            if (delim == ' ') then
                do while (i <= len_trim(str) .and. str(i:i) == delim)
                    i = i + 1
                end do
                if (i > len_trim(str)) exit
            end if
            
            in_quotes = .false.
            tok_len = 0
            
            do while (i <= len_trim(str))
                if (str(i:i) == '"') then
                    in_quotes = .not. in_quotes
                else if (str(i:i) == delim .and. .not. in_quotes) then
                    last_was_delim = .true.
                    exit
                else
                    tok_len = tok_len + 1
                    token(tok_len:tok_len) = str(i:i)
                end if
                i = i + 1
            end do
            
            count = count + 1
            if (tok_len > 0) then
                call tokens%append(token(1:tok_len))
            else
                call tokens%append("")
            end if
            
            if (last_was_delim) i = i + 1
        end do
    end subroutine tokenize

    subroutine str_to_int(str, val, stat)
        character(len=*), intent(in) :: str
        integer, intent(out) :: val
        integer, intent(out), optional :: stat
        integer :: ios
        read(str, *, iostat=ios) val
        if (present(stat)) stat = ios
    end subroutine str_to_int

    subroutine str_to_real(str, val, stat)
        character(len=*), intent(in) :: str
        real, intent(out) :: val
        integer, intent(out), optional :: stat
        integer :: ios
        read(str, *, iostat=ios) val
        if (present(stat)) stat = ios
    end subroutine str_to_real

    subroutine str_to_logical(str, val, stat)
        character(len=*), intent(in) :: str
        logical, intent(out) :: val
        integer, intent(out), optional :: stat

        character(len=:), allocatable :: lower_str

        val = .false.
        lower_str = trim(adjustl(to_lower(str)))

        select case (lower_str)
            case ("t", ".true.", "true", "yes", "y", "on", "1")
                val = .true.
                if (present(stat)) stat = 0
            case ("f", ".false.", "false", "no", "n", "off", "0")
                val = .false.
                if (present(stat)) stat = 0
            case default
                ! Unknown value — report parse failure
                if (present(stat)) stat = -1
        end select
    end subroutine str_to_logical

    !> Checks if a string starts with a given prefix
    !> Note: if prefix is longer than string, just returns false
    pure function starts_with(str, prefix) result(res)
        character(len=*), intent(in) :: str, prefix
        logical :: res
        
        if (len(prefix) > len(str)) then
            res = .false.
        else
            res = (str(1:len(prefix)) == prefix)
        end if
    end function starts_with

    !> Checks if a string ends with a given suffix
    !> Note: if suffix is longer than string, just returns false
    pure function ends_with(str, suffix) result(res)
        character(len=*), intent(in) :: str, suffix
        logical :: res
        integer :: len_str, len_suf
        
        len_str = len(str)
        len_suf = len(suffix)
        
        if (len_suf > len_str) then
            res = .false.
        else
            res = (str(len_str - len_suf + 1 : len_str) == suffix)
        end if
    end function ends_with

end module red_string
