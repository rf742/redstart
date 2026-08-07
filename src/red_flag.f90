! Copyright (c) 2026 rf742
! SPDX-License-Identifier: MIT

module red_flag
    use red_string
    use red_vector, only: string_vector
    use red_errors, only: red_error, ERR_SUCCESS
    use iso_fortran_env, only: OUTPUT_UNIT
    implicit none
    private

    public :: cli_flag
    public :: flag_parser
    
    public :: FLAG_SUCCESS, FLAG_ERR_PARSE, FLAG_ERR_TYPE

    integer, parameter :: FLAG_SUCCESS   = ERR_SUCCESS
    integer, parameter :: FLAG_ERR_PARSE = 1
    integer, parameter :: FLAG_ERR_TYPE  = 2
    
    integer, parameter :: MAX_FLAGS = 256

    !> Represents a single registered command-line flag
    type :: cli_flag
        character(len=:), allocatable :: name
        character(len=:), allocatable :: help_text
        character(len=:), allocatable :: raw_value
        logical :: takes_arg = .true.
        logical :: was_passed = .false.
    end type cli_flag

    !> The main parser object
    type :: flag_parser
        type(cli_flag) :: flags(MAX_FLAGS)
        integer :: num_flags = 0
        type(string_vector) :: positionals
        type(red_error) :: err
    contains
        procedure :: add => flag_add
        procedure :: parse => flag_parse_args
        procedure :: parse_array => flag_parse_array
        procedure :: print_help => flag_print_help
        
        procedure :: is_set => flag_is_set
        procedure :: get_string => flag_get_string
        procedure :: get_int => flag_get_int
        procedure :: get_real => flag_get_real
        procedure :: get_logical => flag_get_logical

        procedure :: argc => flag_argc
        procedure :: get_arg => flag_get_arg
    end type flag_parser

contains

    !> Register a new flag
    subroutine flag_add(this, name, help_text, takes_arg, default_value)
        class(flag_parser), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: help_text
        logical, intent(in), optional :: takes_arg
        character(len=*), intent(in), optional :: default_value

        if (this%num_flags >= MAX_FLAGS) then
            call this%err%raise(FLAG_ERR_PARSE, "red_flag", "Maximum flag limit exceeded.")
            return
        end if

        this%num_flags = this%num_flags + 1
        this%flags(this%num_flags)%name = name
        this%flags(this%num_flags)%was_passed = .false.
        
        if (present(help_text)) this%flags(this%num_flags)%help_text = help_text
        
        if (present(takes_arg)) then
            this%flags(this%num_flags)%takes_arg = takes_arg
        else
            this%flags(this%num_flags)%takes_arg = .true.
        end if
        
        if (present(default_value)) this%flags(this%num_flags)%raw_value = default_value
    end subroutine flag_add

    !> Parse an array of string arguments
    subroutine flag_parse_array(this, args, stat)
        class(flag_parser), intent(inout) :: this
        character(len=*), dimension(:), intent(in) :: args
        type(red_error), intent(inout), optional :: stat
        
        integer :: num_args, i, k, eq_pos, start_idx
        character(len=:), allocatable :: arg, flag_name, flag_val
        logical :: found

        call this%err%clear()
        num_args = size(args)
        i = 1

        do while (i <= num_args)
            arg = trim(args(i))

            ! Check if it is a flag (starts with '-')
            if (len(arg) > 1 .and. arg(1:1) == "-") then
                
                ! Strip leading hyphens (treats -flag and --flag identically)
                start_idx = 2
                if (len(arg) > 2 .and. arg(2:2) == "-") start_idx = 3
                
                ! Check for '=' assignment
                eq_pos = index(arg, "=")
                if (eq_pos > 0) then
                    flag_name = arg(start_idx : eq_pos-1)
                    flag_val = arg(eq_pos+1 :)
                else
                    flag_name = arg(start_idx :)
                end if

                ! Search registry
                found = .false.
                do k = 1, this%num_flags
                    if (this%flags(k)%name == flag_name) then
                        found = .true.
                        this%flags(k)%was_passed = .true.
                        
                        if (this%flags(k)%takes_arg) then
                            if (eq_pos > 0) then
                                this%flags(k)%raw_value = flag_val
                            else
                                if (i < num_args) then
                                    i = i + 1
                                    this%flags(k)%raw_value = trim(args(i))
                                else
                                    call this%err%raise(FLAG_ERR_PARSE, "red_flag", "Flag -" // flag_name // " requires an argument.")
                                    if (present(stat)) stat = this%err
                                    return
                                end if
                            end if
                        else
                            ! Boolean flag
                            this%flags(k)%raw_value = "TRUE"
                            if (eq_pos > 0) then
                                call this%err%raise(FLAG_ERR_PARSE, "red_flag", "Flag -" // flag_name // " does not take a direct assignment.")
                                if (present(stat)) stat = this%err
                                return
                            end if
                        end if
                        exit
                    end if
                end do

                if (.not. found) then
                    call this%err%raise(FLAG_ERR_PARSE, "red_flag", "Unknown flag: " // flag_name)
                    if (present(stat)) stat = this%err
                    return
                end if

            else
                ! Positional argument
                call this%positionals%append(arg)
            end if
            
            i = i + 1
        end do
        
        if (present(stat)) stat = this%err
    end subroutine flag_parse_array

    !> Fetch arguments from the environment and parse them
    subroutine flag_parse_args(this, stat)
        class(flag_parser), intent(inout) :: this
        type(red_error), intent(inout), optional :: stat
        
        integer :: num_args, i, arg_len, max_len
        character(len=:), allocatable, dimension(:) :: cli_args
        
        call this%err%clear()
        
        num_args = command_argument_count()
        if (num_args == 0) then
            if (present(stat)) stat = this%err
            return
        end if
        
        max_len = 0
        do i = 1, num_args
            call get_command_argument(i, length=arg_len)
            if (arg_len > max_len) max_len = arg_len
        end do
        
        if (max_len > 0) then
            allocate(character(len=max_len) :: cli_args(num_args))
            do i = 1, num_args
                call get_command_argument(i, value=cli_args(i))
            end do
            call this%parse_array(cli_args)
        end if
        
        if (present(stat)) stat = this%err
    end subroutine flag_parse_args

    !> Check if a flag was passed on the command line
    logical function flag_is_set(this, name)
        class(flag_parser), intent(in) :: this
        character(len=*), intent(in) :: name
        integer :: i
        flag_is_set = .false.
        do i = 1, this%num_flags
            if (this%flags(i)%name == name) then
                flag_is_set = this%flags(i)%was_passed
                return
            end if
        end do
    end function flag_is_set

    !> Get string value
    function flag_get_string(this, name) result(val)
        class(flag_parser), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: val
        integer :: i
        val = ""
        do i = 1, this%num_flags
            if (this%flags(i)%name == name) then
                if (allocated(this%flags(i)%raw_value)) then
                    val = this%flags(i)%raw_value
                end if
                return
            end if
        end do
    end function flag_get_string

    !> Get integer value
    function flag_get_int(this, name, stat) result(val)
        class(flag_parser), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(red_error), intent(inout), optional :: stat
        integer :: val, ios
        character(len=:), allocatable :: str_val
        
        val = 0
        call this%err%clear()
        str_val = this%get_string(name)
        
        if (allocated(str_val)) then
            if (len_trim(str_val) > 0) then
                call str_to_int(str_val, val, ios)
                if (ios /= 0) then
                    call this%err%raise(FLAG_ERR_TYPE, "red_flag", "Expected integer for flag '" // name // "'")
                end if
            end if
        end if
        if (present(stat)) stat = this%err
    end function flag_get_int

    !> Get real value
    function flag_get_real(this, name, stat) result(val)
        class(flag_parser), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(red_error), intent(inout), optional :: stat
        real :: val
        integer :: ios
        character(len=:), allocatable :: str_val
        
        val = 0.0
        call this%err%clear()
        str_val = this%get_string(name)
        
        if (allocated(str_val)) then
            if (len_trim(str_val) > 0) then
                call str_to_real(str_val, val, ios)
                if (ios /= 0) then
                    call this%err%raise(FLAG_ERR_TYPE, "red_flag", "Expected real for flag '" // name // "'")
                end if
            end if
        end if
        if (present(stat)) stat = this%err
    end function flag_get_real

    !> Get logical value
    function flag_get_logical(this, name, stat) result(val)
        class(flag_parser), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(red_error), intent(inout), optional :: stat
        logical :: val
        integer :: ios
        character(len=:), allocatable :: str_val
        
        val = .false.
        call this%err%clear()
        str_val = this%get_string(name)
        
        if (allocated(str_val)) then
            if (len_trim(str_val) > 0) then
                call str_to_logical(str_val, val, ios)
                if (ios /= 0) then
                    call this%err%raise(FLAG_ERR_TYPE, "red_flag", "Expected logical for flag '" // name // "'")
                end if
            end if
        end if
        if (present(stat)) stat = this%err
    end function flag_get_logical

    !> Get the number of positional arguments
    integer function flag_argc(this)
        class(flag_parser), intent(in) :: this
        flag_argc = this%positionals%length()
    end function flag_argc

    !> Get a positional argument by index
    function flag_get_arg(this, index) result(val)
        class(flag_parser), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: val
        val = this%positionals%get(index)
    end function flag_get_arg

    !> Print formatted help to standard output
    subroutine flag_print_help(this)
        class(flag_parser), intent(in) :: this
        integer :: i, prog_len
        character(len=:), allocatable :: prog_name, flag_prefix
        
        call get_command_argument(0, length=prog_len)
        if (prog_len > 0) then
            allocate(character(len=prog_len) :: prog_name)
            call get_command_argument(0, value=prog_name)
        else
            prog_name = "program"
        end if
        
        write(OUTPUT_UNIT, *) ""
        write(OUTPUT_UNIT, *) "Usage: ", prog_name, " [flags] [args...]"
        write(OUTPUT_UNIT, *) ""
        write(OUTPUT_UNIT, *) "Flags:"
        
        if (this%num_flags > 0) then
            do i = 1, this%num_flags
                flag_prefix = "  -" // this%flags(i)%name
                if (this%flags(i)%takes_arg) flag_prefix = flag_prefix // " value"
                
                ! Pad to align help text
                do while (len(flag_prefix) < 25)
                    flag_prefix = flag_prefix // " "
                end do
                
                if (allocated(this%flags(i)%help_text)) then
                    write(OUTPUT_UNIT, *) flag_prefix, this%flags(i)%help_text
                else
                    write(OUTPUT_UNIT, *) flag_prefix
                end if
            end do
        else
            write(OUTPUT_UNIT, *) "  (No flags registered)"
        end if
    end subroutine flag_print_help

end module red_flag
