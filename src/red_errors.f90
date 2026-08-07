! Copyright (c) 2026 rf742
! SPDX-License-Identifier: MIT

module red_errors
    implicit none
    private

    public :: red_error
    public :: ERR_SUCCESS

    integer, parameter :: ERR_SUCCESS = 0

    type :: red_error
        integer :: code = ERR_SUCCESS
        character(len=:), allocatable :: domain
        character(len=:), allocatable :: message
    contains
        procedure :: raise => error_raise
        procedure :: clear => error_clear
        procedure :: is_active => error_is_active
    end type red_error

contains

    subroutine error_raise(this, code, domain, message)
        class(red_error), intent(inout) :: this
        integer, intent(in) :: code
        character(len=*), intent(in) :: domain
        character(len=*), intent(in) :: message

        this%code = code
        this%domain = domain
        this%message = message
    end subroutine error_raise

    subroutine error_clear(this)
        class(red_error), intent(inout) :: this
        this%code = ERR_SUCCESS
        if (allocated(this%domain)) deallocate(this%domain)
        if (allocated(this%message)) deallocate(this%message)
    end subroutine error_clear

    logical function error_is_active(this)
        class(red_error), intent(inout) :: this
        error_is_active = (this%code /= ERR_SUCCESS)
    end function error_is_active
    
end module red_errors
    
