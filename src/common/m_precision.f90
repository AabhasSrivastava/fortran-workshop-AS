module m_precision
   use iso_fortran_env, only: real64

   implicit none(type, external)

   private
   public :: dp

   integer, parameter :: dp = real64
end module m_precision
