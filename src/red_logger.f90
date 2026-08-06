! Copyright (c) 2026 rf742
! SPDX-License-Identifier: MIT

module red_logger
    use red_datetime, only: datetime
    implicit none
    private

    public :: logger
    public :: LOG_DEBUG, LOG_INFO, LOG_WARN, LOG_ERROR, LOG_FATAL, LOG_OFF

    integer, parameter :: LOG_DEBUG = 1
    integer, parameter :: LOG_INFO  = 2
    integer, parameter :: LOG_WARN  = 3
    integer, parameter :: LOG_ERROR = 4
    integer, parameter :: LOG_FATAL = 5
    integer, parameter :: LOG_OFF = 999

    type :: logger
        integer :: file_level
        integer :: console_level
        integer :: file_unit = 0
    contains
        procedure, private :: init_split => logger_init_split
        procedure, private :: init_single => logger_init_single
        generic :: init => init_split, init_single
        procedure :: close => logger_close
        procedure :: debug => logger_debug
        procedure :: info => logger_info
        procedure :: warn => logger_warn
        procedure :: error => logger_error
        procedure :: fatal => logger_fatal
        procedure, private :: write_log => logger_write
    end type logger

contains

    ! Advanced Initialization (Split Thresholds)
    subroutine logger_init_split(this, file_level, console_level, filepath)
        class(logger), intent(inout) :: this
        integer, intent(in) :: file_level
        integer, intent(in) :: console_level
        character(len=*), intent(in), optional :: filepath
        integer :: iostat

        this%file_level = file_level
        this%console_level = console_level
        this%file_unit = 0

        if (present(filepath)) then
            if (len_trim(filepath) > 0) then
                open(newunit=this%file_unit, file=trim(filepath), action='write', position='append', iostat=iostat)
                if (iostat /= 0) then
                    print *, "Failed to open log file: ", trim(filepath)
                    this%file_unit = 0
                end if
            end if
        end if
    end subroutine logger_init_split

    ! Basic Initialization (Single Threshold for both)
    subroutine logger_init_single(this, level, to_console, filepath)
        class(logger), intent(inout) :: this
        integer, intent(in) :: level
        logical, intent(in) :: to_console
        character(len=*), intent(in), optional :: filepath
        
        integer :: file_lvl, console_lvl
        
        ! Map the single level to the internal split variables based on the toggle
        if (to_console) then
            console_lvl = level
        else
            console_lvl = LOG_OFF ! Disable console
        end if
        
        if (present(filepath)) then
            file_lvl = level
        else
            file_lvl = LOG_OFF ! Disable file
        end if
        
        ! Pass off to the split initializer to handle file opening safely
        call this%init_split(file_lvl, console_lvl, filepath)
    end subroutine logger_init_single

    subroutine logger_close(this)
        class(logger), intent(inout) :: this
        if (this%file_unit /= 0) then
            close(this%file_unit)
            this%file_unit = 0
        end if
    end subroutine logger_close

    subroutine logger_write(this, level, level_tag, msg)
        class(logger), intent(inout) :: this
        integer, intent(in) :: level
        character(len=*), intent(in) :: level_tag
        character(len=*), intent(in) :: msg
        type(datetime) :: dt
        character(len=:), allocatable :: ts, out_msg

        if (level >= this%file_level .or. level >= this%console_level) then
            call dt%now()
            ts = dt%to_string("ISO")
            out_msg = "[" // ts // "] [" // trim(level_tag) // "] " // trim(msg)

            if (level >= this%console_level) then
                print *, out_msg
            end if

            if (level >= this%file_level .and. this%file_unit /= 0) then
                write(this%file_unit, '(A)') out_msg
            end if
        end if
    end subroutine logger_write

    subroutine logger_debug(this, msg)
        class(logger), intent(inout) :: this
        character(len=*), intent(in) :: msg
        call this%write_log(LOG_DEBUG, "DEBUG", msg)
    end subroutine logger_debug

    subroutine logger_info(this, msg)
        class(logger), intent(inout) :: this
        character(len=*), intent(in) :: msg
        call this%write_log(LOG_INFO, "INFO", msg)
    end subroutine logger_info

    subroutine logger_warn(this, msg)
        class(logger), intent(inout) :: this
        character(len=*), intent(in) :: msg
        call this%write_log(LOG_WARN, "WARN", msg)
    end subroutine logger_warn

    subroutine logger_error(this, msg)
        class(logger), intent(inout) :: this
        character(len=*), intent(in) :: msg
        call this%write_log(LOG_ERROR, "ERROR", msg)
    end subroutine logger_error

    subroutine logger_fatal(this, msg)
        class(logger), intent(inout) :: this
        character(len=*), intent(in) :: msg
        call this%write_log(LOG_FATAL, "FATAL", msg)
    end subroutine logger_fatal

end module red_logger
