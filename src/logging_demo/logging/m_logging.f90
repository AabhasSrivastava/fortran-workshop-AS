module m_logging
   implicit none(type, external)

   logical :: debug
   logical :: quiet
   logical :: should_log_to_file
   integer :: log_file

   type, abstract :: t_abstract_logger
   contains
      procedure(log_function_interface), deferred :: log
   end type t_abstract_logger

   interface
      subroutine log_function_interface(self, message)
         import t_abstract_logger
         implicit none

         class(t_abstract_logger), intent(in) :: self
         character(len=*), intent(in) :: message
      end subroutine log_function_interface
   end interface

   type, extends(t_abstract_logger) :: t_do_everything_logger
   contains
      procedure :: log => do_everytying_logger_log
   end type t_do_everything_logger

contains
   subroutine do_everytying_logger_log(self, message)
      class(t_do_everything_logger), intent(in) :: self
      character(len=*), intent(in) :: message

      if (debug) then
         if (should_log_to_file) then
            write (log_file, fmt=*) current_time()//message
         end if

         if (.not. quiet) then
            print *, current_time()//message
         end if
      end if
   end subroutine do_everytying_logger_log

   subroutine init_logging()
      integer :: open_status
      ! We could read this from CLI arguments
      ! --debug, --quiet, and --log-file
      debug = .true.
      quiet = .false.
      open (file='debug.log', newunit=log_file, status='unknown', &
            position='append', action='write', iostat=open_status)
      should_log_to_file = open_status == 0
   end subroutine init_logging

   function current_time() result(iso_time)
      character(len=8) :: date
      character(len=10) :: time
      character(len=5) :: zone
      character(len=26) :: iso_time

      call date_and_time(date, time, zone)

      iso_time = trim(date(1:4))//'-'//trim(date(5:6))//'-'//trim(date(7:8))//'T'// &
                 trim(time(1:2))//':'//trim(time(3:4))//':'//trim(time(5:6))//zone
   end function current_time
end module m_logging
