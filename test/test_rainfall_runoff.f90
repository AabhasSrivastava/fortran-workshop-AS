module test_rainfall_runoff
   use iso_fortran_env, only: int64, real64
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test, check
   use hydrology_rainfall_runoff, only: run
   use file_operations, only: create_or_open_file, &
                              file_unit_or_error_t, &
                              write_lines_to_file, &
                              read_all_lines, &
                              delete_file_if_exists, &
                              open_existing_file_for_reading
   use common_strings, only: string_list_t, string_t, string_list_or_error_t
   use common_error_handling, only: optional_error_t, some_error_t, no_error_t
   use test_strings, only: check
   use test_error_handling, only: check_no_error

   implicit none(type, external)

   private

   public :: collect_tests
   public :: setup_test_paths

   ! Test configuration - paths for executable and files
   character(len=:), allocatable :: executable_path
   character(len=:), allocatable :: input_file_path
   character(len=:), allocatable :: output_file_path

contains

   !> Setup test paths for executable and data files
   subroutine setup_test_paths(exec_path, input_path, output_path)
      character(len=*), intent(in), optional :: exec_path
      character(len=*), intent(in), optional :: input_path
      character(len=*), intent(in), optional :: output_path

      ! Set executable path (with platform-specific defaults)
      if (present(exec_path)) then
         executable_path = exec_path
      else
         executable_path = 'rainfall_runoff_demo.exe'
      end if

      ! Set input file path
      if (present(input_path)) then
         input_file_path = input_path
      else
         input_file_path = 'input.csv'
      end if

      ! Set output file path
      if (present(output_path)) then
         output_file_path = output_path
      else
         output_file_path = 'output.csv'
      end if
   end subroutine setup_test_paths

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_characterization_test", &
                               test_characterization_test) &
                  ]
   end subroutine collect_tests

   subroutine run_rainfall_runoff_executable(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: exit_status
      logical :: wait_for_completion
      character(len=:), allocatable :: command

      ! Initialize paths if not already set
      if (.not. allocated(executable_path)) then
         call setup_test_paths()
      end if

      ! Build command line
      command = trim(executable_path)//' '//trim(input_file_path)//' '//trim(output_file_path)

      ! Execute the rainfall_runoff_demo executable
      wait_for_completion = .true.
      call execute_command_line(command, &
                                wait=wait_for_completion, &
                                exitstat=exit_status)

      if (exit_status /= 0) then
         call test_failed(error, 'rainfall_runoff_demo executable failed with exit status: ' &
                          //trim(adjustl(char(exit_status))))
         return
      end if
   end subroutine run_rainfall_runoff_executable

   subroutine test_characterization_test(error)
      type(error_type), allocatable, intent(out) :: error

      type(string_list_t), allocatable :: input
      type(string_list_t), allocatable :: expected

      class(optional_error_t), allocatable :: file_deleted

      ! Initialize paths if not already set
      if (.not. allocated(output_file_path)) then
         call setup_test_paths()
      end if

      file_deleted = delete_file_if_exists(output_file_path)
      call check_no_error(error, file_deleted)

      ! Previously used input
      input = string_list_t([ &
                            string_t('date,precipitation,air temperature'), &
                            string_t('01-10-2025,5,20'), &
                            string_t('02-10-2025,30,21'), &
                            string_t('03-10-2025,40,10'), &
                            string_t('04-10-2025,50,5'), &
                            string_t('05-10-2025,60,-5'), &
                            string_t('06-10-2025,10,-10')])

      call write_input_csv(error, input_file_path, input)
      if (allocated(error)) then
         return
      end if

      ! Previously recorded output that is correct
      expected = string_list_t([ &
                               string_t('date,P,T,PET,AET,Q,S'), &
                               string_t('01-10-2025,5.000,20.000,20.000,20.000,.000,75.000'), &
                               string_t('02-10-2025,30.000,21.000,20.800,20.800,.000,84.200'), &
                               string_t('03-10-2025,40.000,10.000,12.000,12.000,.000,112.200'), &
                               string_t('04-10-2025,50.000,5.000,8.000,8.000,12.200,142.000'), &
                               string_t('05-10-2025,60.000,-5.000,.000,.000,52.000,150.000'), &
                               string_t('06-10-2025,10.000,-10.000,.000,.000,10.000,150.000') &
                               ])

      ! Run the rainfall_runoff simulation
      call run_rainfall_runoff_executable(error)
      if (allocated(error)) then
         return
      end if

      call check_output(error, output_file_path, expected)

      ! Cleanup: Remove test files
      file_deleted = delete_file_if_exists(input_file_path)
      file_deleted = delete_file_if_exists(output_file_path)
   end subroutine test_characterization_test

   subroutine write_input_csv(error, path, lines)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: path
      type(string_list_t), intent(in) :: lines

      type(file_unit_or_error_t) :: input_file
      class(optional_error_t), allocatable :: lines_written

      input_file = create_or_open_file(path)
      if (allocated(input_file%error)) then
         call test_failed(error, 'Expected no error, got '//input_file%error%to_string())
         return
      end if

      lines_written = write_lines_to_file(input_file%file_unit, &
                                          lines)
      close (input_file%file_unit)

      call check_no_error(error, lines_written)
      if (allocated(error)) then
         return
      end if
   end subroutine write_input_csv

   subroutine check_output(error, path, expected)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: path
      type(string_list_t), intent(in) :: expected

      type(file_unit_or_error_t) :: output_file
      type(string_list_or_error_t) :: actual

      output_file = open_existing_file_for_reading(path)
      if (allocated(output_file%error)) then
         call test_failed(error, 'Expected no error, got '//output_file%error%to_string())
         return
      end if

      actual = read_all_lines(output_file%file_unit)
      if (allocated(actual%error)) then
         call test_failed(error, 'Expected no error, got '//actual%error%to_string())
         return
      end if

      call check(error, actual%lines, expected)
   end subroutine check_output

end module test_rainfall_runoff
