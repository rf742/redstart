! Copyright (c) 2026 rf742
! SPDX-License-Identifier: MIT

module red_datetime
    use red_string, only: int_to_str
    implicit none
    private

    public :: datetime
    public :: stopwatch
    
    public :: operator(-)
    interface operator(-)
        module procedure datetime_diff
    end interface

    public :: operator(+)
    interface operator(+)
        module procedure datetime_add_seconds
        module procedure seconds_add_datetime
    end interface

    type :: datetime
        integer :: year, month, day
        integer :: hour, minute, second, millisecond
        integer :: timezone_offset_minutes
    contains
        procedure :: now => datetime_now
        procedure :: to_string => datetime_to_string
        procedure :: is_valid => datetime_is_valid
    end type datetime

    type :: stopwatch
        integer(8) :: start_count, stop_count, count_rate
    contains
        procedure :: start => stopwatch_start
        procedure :: stop => stopwatch_stop
        procedure :: elapsed_seconds => stopwatch_elapsed
    end type stopwatch

contains

    subroutine datetime_now(this)
        class(datetime), intent(inout) :: this
        integer :: values(8)
        
        call date_and_time(values=values)
        
        this%year = values(1)
        this%month = values(2)
        this%day = values(3)
        this%timezone_offset_minutes = values(4)
        this%hour = values(5)
        this%minute = values(6)
        this%second = values(7)
        this%millisecond = values(8)
    end subroutine datetime_now

    function pad_zero(val, width) result(res)
        integer, intent(in) :: val, width
        character(len=:), allocatable :: res
        
        res = int_to_str(val)
        do while (len(res) < width)
            res = "0" // res
        end do
    end function pad_zero

    function get_day_of_week(y, m, d) result(res)
        integer, intent(in) :: y, m, d
        character(len=:), allocatable :: res
        integer :: q, m_adj, k, j, h, y_adj
        
        q = d
        m_adj = m
        y_adj = y
        if (m_adj < 3) then
            m_adj = m_adj + 12
            y_adj = y_adj - 1
        end if
        
        k = mod(y_adj, 100)
        j = y_adj / 100
        
        h = mod(q + (13*(m_adj+1))/5 + k + k/4 + j/4 - 2*j, 7)
        if (h < 0) h = h + 7
        
        select case (h)
            case (0); res = "Sat"
            case (1); res = "Sun"
            case (2); res = "Mon"
            case (3); res = "Tue"
            case (4); res = "Wed"
            case (5); res = "Thu"
            case (6); res = "Fri"
            case default; res = "Unk"
        end select
    end function get_day_of_week

    function get_month_name(m) result(res)
        integer, intent(in) :: m
        character(len=:), allocatable :: res
        
        select case(m)
            case(1); res = "Jan"
            case(2); res = "Feb"
            case(3); res = "Mar"
            case(4); res = "Apr"
            case(5); res = "May"
            case(6); res = "Jun"
            case(7); res = "Jul"
            case(8); res = "Aug"
            case(9); res = "Sep"
            case(10); res = "Oct"
            case(11); res = "Nov"
            case(12); res = "Dec"
            case default; res = "Unk"
        end select
    end function get_month_name

    function datetime_to_string(this, format) result(res)
        class(datetime), intent(in) :: this
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: res
        character(len=:), allocatable :: fmt_choice
        
        character(len=:), allocatable :: yyyy, mm, dd, hh, mi, ss, mmm
        character(len=:), allocatable :: ampm
        integer :: h_12, tz_h, tz_m
        character(len=:), allocatable :: tz_str, tz_h_str, tz_m_str, tz_sign

        if (present(format)) then
            fmt_choice = trim(adjustl(format))
        else
            fmt_choice = "DEFAULT"
        end if

        yyyy = pad_zero(this%year, 4)
        mm = pad_zero(this%month, 2)
        dd = pad_zero(this%day, 2)
        hh = pad_zero(this%hour, 2)
        mi = pad_zero(this%minute, 2)
        ss = pad_zero(this%second, 2)
        mmm = pad_zero(this%millisecond, 3)

        ! Using a select case makes adding new formats highly modular and simple
        select case (trim(fmt_choice))
            case ("ISO")
                ! Example: 2026-07-24T15:04:05.123
                res = yyyy // "-" // mm // "-" // dd // "T" // hh // ":" // mi // ":" // ss // "." // mmm
                
            case ("KITCHEN")
                ! Example: 3:04 PM
                h_12 = this%hour
                if (h_12 == 0) then
                    h_12 = 12
                    ampm = "AM"
                else if (h_12 < 12) then
                    ampm = "AM"
                else if (h_12 == 12) then
                    ampm = "PM"
                else
                    h_12 = h_12 - 12
                    ampm = "PM"
                end if
                res = pad_zero(h_12, 1) // ":" // mi // " " // ampm

            case ("RFC5322")
                ! Example: Fri, 24 Jul 2026 15:04:05 -0400
                if (this%timezone_offset_minutes < 0) then
                    tz_sign = "-"
                    tz_h = abs(this%timezone_offset_minutes) / 60
                    tz_m = mod(abs(this%timezone_offset_minutes), 60)
                else
                    tz_sign = "+"
                    tz_h = this%timezone_offset_minutes / 60
                    tz_m = mod(this%timezone_offset_minutes, 60)
                end if
                tz_h_str = pad_zero(tz_h, 2)
                tz_m_str = pad_zero(tz_m, 2)
                tz_str = tz_sign // tz_h_str // tz_m_str

                res = get_day_of_week(this%year, this%month, this%day) // ", " // &
                      pad_zero(this%day, 2) // " " // &
                      get_month_name(this%month) // " " // &
                      yyyy // " " // &
                      hh // ":" // mi // ":" // ss // " " // tz_str

            case default
                ! Example: 2026-07-24 15:04:05
                res = yyyy // "-" // mm // "-" // dd // " " // hh // ":" // mi // ":" // ss
        end select

    end function datetime_to_string

    subroutine stopwatch_start(this)
        class(stopwatch), intent(inout) :: this
        call system_clock(this%start_count, this%count_rate)
    end subroutine stopwatch_start

    subroutine stopwatch_stop(this)
        class(stopwatch), intent(inout) :: this
        call system_clock(this%stop_count)
    end subroutine stopwatch_stop

    function stopwatch_elapsed(this) result(res)
        class(stopwatch), intent(in) :: this
        real :: res
        if (this%count_rate > 0) then
            res = real(this%stop_count - this%start_count) / real(this%count_rate)
        else
            res = 0.0
        end if
    end function stopwatch_elapsed

    function datetime_to_unix_seconds(dt) result(secs)
        type(datetime), intent(in) :: dt
        real(8) :: secs
        integer :: a, y, m, jdn, epoch_jdn

        a = (14 - dt%month) / 12
        y = dt%year + 4800 - a
        m = dt%month + 12 * a - 3

        jdn = dt%day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045
        ! 1970-01-01 JDN is 2440588
        epoch_jdn = 2440588

        secs = real(jdn - epoch_jdn, 8) * 86400.0_8
        secs = secs + real(dt%hour, 8) * 3600.0_8
        secs = secs + real(dt%minute, 8) * 60.0_8
        secs = secs + real(dt%second, 8)
        secs = secs + real(dt%millisecond, 8) / 1000.0_8
        
        ! Normalize for timezone offset to get absolute UTC seconds
        secs = secs - real(dt%timezone_offset_minutes, 8) * 60.0_8
    end function datetime_to_unix_seconds

    function datetime_diff(dt1, dt2) result(res)
        type(datetime), intent(in) :: dt1, dt2
        real(8) :: res
        res = datetime_to_unix_seconds(dt1) - datetime_to_unix_seconds(dt2)
    end function datetime_diff

    function unix_seconds_to_datetime(secs, tz_offset_mins) result(dt)
        real(8), intent(in) :: secs
        integer, intent(in) :: tz_offset_mins
        type(datetime) :: dt
        
        integer(8) :: total_secs
        integer :: ms
        integer :: jdn, a, b, c, d, e, m, y, day
        integer :: hrs, mins, scs
        real(8) :: local_secs
        
        ! Adjust for timezone to get local seconds
        local_secs = secs + real(tz_offset_mins, 8) * 60.0_8
        
        ! Extract milliseconds
        ms = nint( (local_secs - floor(local_secs)) * 1000.0_8 )
        
        total_secs = floor(local_secs)
        
        ! 1970-01-01 JDN is 2440588
        jdn = 2440588 + (total_secs / 86400_8)
        
        ! Extract time components
        total_secs = mod(total_secs, 86400_8)
        if (total_secs < 0) then
            total_secs = total_secs + 86400_8
            jdn = jdn - 1
        end if
        
        hrs = total_secs / 3600
        total_secs = mod(total_secs, 3600_8)
        mins = total_secs / 60
        scs = mod(total_secs, 60_8)
        
        ! Julian Day Number to Gregorian Calendar
        a = jdn + 32044
        b = (4 * a + 3) / 146097
        c = a - (146097 * b) / 4
        d = (4 * c + 3) / 1461
        e = c - (1461 * d) / 4
        m = (5 * e + 2) / 153
        day = e - (153 * m + 2) / 5 + 1
        m = m + 3 - 12 * (m / 10)
        y = 100 * b + d - 4800 + (m / 10)
        
        dt%year = y
        dt%month = m
        dt%day = day
        dt%hour = hrs
        dt%minute = mins
        dt%second = scs
        dt%millisecond = ms
        dt%timezone_offset_minutes = tz_offset_mins
    end function unix_seconds_to_datetime

    function datetime_add_seconds(dt1, secs) result(res)
        type(datetime), intent(in) :: dt1
        real(8), intent(in) :: secs
        type(datetime) :: res
        real(8) :: current_secs
        current_secs = datetime_to_unix_seconds(dt1)
        res = unix_seconds_to_datetime(current_secs + secs, dt1%timezone_offset_minutes)
    end function datetime_add_seconds

    function seconds_add_datetime(secs, dt1) result(res)
        real(8), intent(in) :: secs
        type(datetime), intent(in) :: dt1
        type(datetime) :: res
        res = datetime_add_seconds(dt1, secs)
    end function seconds_add_datetime

    function is_leap_year(y) result(res)
        integer, intent(in) :: y
        logical :: res
        res = (mod(y, 4) == 0 .and. mod(y, 100) /= 0) .or. (mod(y, 400) == 0)
    end function is_leap_year

    function days_in_month(y, m) result(res)
        integer, intent(in) :: y, m
        integer :: res
        integer, dimension(12) :: dim = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        if (m >= 1 .and. m <= 12) then
            res = dim(m)
            if (m == 2 .and. is_leap_year(y)) res = 29
        else
            res = 0
        end if
    end function days_in_month

    function datetime_is_valid(this) result(res)
        class(datetime), intent(in) :: this
        logical :: res
        res = .true.
        if (this%month < 1 .or. this%month > 12) res = .false.
        if (res .and. (this%day < 1 .or. this%day > days_in_month(this%year, this%month))) res = .false.
        if (this%hour < 0 .or. this%hour > 23) res = .false.
        if (this%minute < 0 .or. this%minute > 59) res = .false.
        if (this%second < 0 .or. this%second > 59) res = .false.
        if (this%millisecond < 0 .or. this%millisecond > 999) res = .false.
    end function datetime_is_valid

end module red_datetime
