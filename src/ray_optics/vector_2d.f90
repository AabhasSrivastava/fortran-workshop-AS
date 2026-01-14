module ray_optics_vector_2d
   use common_precision, only: dp

   implicit none(type, external)

   private
   public :: ray_t

   !! Create ray data type
   type :: ray_t
      real(dp) :: x0
      real(dp) :: y0
      real(dp) :: dx
      real(dp) :: dy
   end type ray_t
contains

end module ray_optics_vector_2d
