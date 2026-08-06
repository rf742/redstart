! Copyright (c) 2026 rf742
! SPDX-License-Identifier: MIT

!> A module for high-level I/O operations, including file reading/writing and stream redirection.
module red_ioutil
    !! output_unit portably represents stdout
    use, intrinsic :: iso_fortran_env, only: output_unit
    use red_vector, only: string_vector
    implicit none
    private

    public :: io_context, new_io_context, read_file_lines, write_file_lines

    !> A context for abstracting output streams (e.g., stdout or a file unit)
    type :: io_context
        integer :: unit = output_unit
    contains
        procedure :: print_msg
        procedure :: print_err
    end type io_context

contains

    !> Creates a new io_context. Defaults to output_unit (stdout) if unit is not provided.
    function new_io_context(unit) result(ctx)
        integer, intent(in), optional :: unit
        type(io_context) :: ctx
        if (present(unit)) then
            ctx%unit = unit
        else
            ctx%unit = output_unit
        end if
    end function new_io_context

    !> Prints a standard message to the configured I/O unit
    subroutine print_msg(this, msg)
        class(io_context), intent(in) :: this
        character(len=*), intent(in) :: msg
        write(this%unit, *) trim(msg)
    end subroutine print_msg

    !> Prints an error message to the configured I/O unit with an "Error: " prefix
    subroutine print_err(this, msg)
        class(io_context), intent(in) :: this
        character(len=*), intent(in) :: msg
        write(this%unit, *) "Error: ", trim(msg)
    end subroutine print_err

    !> Reads an entire text file into a string_vector
    function read_file_lines(filename, stat) result(lines)
        use, intrinsic :: iso_fortran_env, only: IOSTAT_EOR, IOSTAT_END
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        type(string_vector) :: lines

        integer :: iunit, ios
        character(len=256) :: chunk
        character(len=:), allocatable :: current_line
        integer :: chunk_len

        if (present(stat)) stat = 0

        open(newunit=iunit, file=trim(filename), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            if (present(stat)) stat = ios
            return
        end if

        current_line = ""

        do
            read(iunit, '(A)', advance='no', iostat=ios, size=chunk_len) chunk

            if (ios == IOSTAT_EOR) then
                ! End of record: we hit the line ending.
                ! Append whatever partial chunk was read (chunk_len chars),
                ! then commit the completed line.
                if (chunk_len > 0) then
                    current_line = current_line // chunk(1:chunk_len)
                end if

                ! Strip trailing CR if present (Windows CRLF files)
                if (len(current_line) > 0) then
                    if (current_line(len(current_line):len(current_line)) == char(13)) then
                        current_line = current_line(1:len(current_line)-1)
                    end if
                end if

                call lines%append(current_line)
                current_line = ""

            else if (ios == IOSTAT_END) then
                ! End of file. Commit any remaining partial line.
                if (chunk_len > 0) then
                    current_line = current_line // chunk(1:chunk_len)
                end if
                if (len(current_line) > 0) then
                    ! Strip trailing CR
                    if (current_line(len(current_line):len(current_line)) == char(13)) then
                        current_line = current_line(1:len(current_line)-1)
                    end if
                    call lines%append(current_line)
                end if
                exit

            else if (ios /= 0) then
                ! A real I/O error occurred.
                if (present(stat)) stat = ios
                exit

            else
                ! ios == 0 means the buffer was filled completely and there is
                ! more data on this line. Append the full chunk and keep reading.
                current_line = current_line // chunk(1:chunk_len)
            end if
        end do

        close(iunit)
    end function read_file_lines

    !> Writes a string_vector to a text file
    subroutine write_file_lines(filename, lines, stat)
        character(len=*), intent(in) :: filename
        type(string_vector), intent(in) :: lines
        integer, intent(out), optional :: stat

        integer :: iunit, ios, i

        if (present(stat)) stat = 0

        open(newunit=iunit, file=trim(filename), status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            if (present(stat)) stat = ios
            return
        end if

        do i = 1, lines%length()
            write(iunit, '(A)', iostat=ios) lines%get(i)
            if (ios /= 0) then
                if (present(stat)) stat = ios
                close(iunit)
                return
            end if
        end do

        close(iunit)
    end subroutine write_file_lines

end module red_ioutil
