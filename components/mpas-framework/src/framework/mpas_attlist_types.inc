   integer, parameter :: MPAS_ATT_INT   = 1
   integer, parameter :: MPAS_ATT_INTA  = 2
   integer, parameter :: MPAS_ATT_REAL  = 3
   integer, parameter :: MPAS_ATT_REALA = 4
   integer, parameter :: MPAS_ATT_TEXT  = 5
   integer, parameter :: MPAS_ATT_REAL_4  = 6
   integer, parameter :: MPAS_ATT_REALA_4 = 7
   type att_lists_type
      type (att_list_type), pointer :: attList => null()
   end type att_lists_type

   ! Derived type for holding field attributes
   type att_list_type
      character (len=StrKIND) :: attName = ''
      integer :: attType = -1
      integer :: attValueInt
      integer, dimension(:), pointer :: attValueIntA => null()
      real (kind=RKIND) :: attValueReal
      real (kind=RKIND), dimension(:), pointer :: attValueRealA => null()
      real (kind=R4KIND) :: attValueReal_4
      real (kind=R4KIND), dimension(:), pointer :: attValueRealA_4 => null()
      character (len=StrKIND) :: attValueText
      type (att_list_type), pointer :: next => null()
   end type att_list_type
