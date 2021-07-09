!-----------------------------------------------------------------------
!  routine mpas_pool_add_field_0d_real
!
!> \brief MPAS Pool 0D Real field add routine
!> \author Michael Duda, Doug Jacobsen
!> \date   03/27/2014
!> \details
!> This routine inserts field into inPool when field is a 0D real field
!
!-----------------------------------------------------------------------
   subroutine mpas_pool_add_field_0d_real(inPool, key, field)!{{{

      implicit none

      type (mpas_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field0DReal), pointer :: field

      type (mpas_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = mpas_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = MPAS_POOL_FIELD

         allocate(newmem % data)
         newmem % data % contentsType = MPAS_POOL_REAL
         newmem % data % contentsDims = 0
         newmem % data % contentsTimeLevs = 1
         newmem % data % r0 => field
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine mpas_pool_add_field_0d_real!}}}


!-----------------------------------------------------------------------
!  routine mpas_pool_add_field_1d_real
!
!> \brief MPAS Pool 1D Real field add routine
!> \author Michael Duda, Doug Jacobsen
!> \date   03/27/2014
!> \details
!> This routine inserts field into inPool when field is a 1D real field
!
!-----------------------------------------------------------------------
   subroutine mpas_pool_add_field_1d_real(inPool, key, field)!{{{

      implicit none

      type (mpas_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field1DReal), pointer :: field

      type (mpas_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = mpas_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = MPAS_POOL_FIELD

         allocate(newmem % data)
         newmem % data % contentsType = MPAS_POOL_REAL
         newmem % data % contentsDims = 1
         newmem % data % contentsTimeLevs = 1
         newmem % data % r1 => field
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine mpas_pool_add_field_1d_real!}}}


!-----------------------------------------------------------------------
!  routine mpas_pool_add_field_2d_real
!
!> \brief MPAS Pool 2D Real field add routine
!> \author Michael Duda, Doug Jacobsen
!> \date   03/27/2014
!> \details
!> This routine inserts field into inPool when field is a 2D real field
!
!-----------------------------------------------------------------------
   subroutine mpas_pool_add_field_2d_real(inPool, key, field)!{{{

      implicit none

      type (mpas_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field2DReal), pointer :: field

      type (mpas_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = mpas_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = MPAS_POOL_FIELD

         allocate(newmem % data)
         newmem % data % contentsType = MPAS_POOL_REAL
         newmem % data % contentsDims = 2
         newmem % data % contentsTimeLevs = 1
         newmem % data % r2 => field
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine mpas_pool_add_field_2d_real!}}}


!-----------------------------------------------------------------------
!  routine mpas_pool_add_field_3d_real
!
!> \brief MPAS Pool 3D Real field add routine
!> \author Michael Duda, Doug Jacobsen
!> \date   03/27/2014
!> \details
!> This routine inserts field into inPool when field is a 3D real field
!
!-----------------------------------------------------------------------
   subroutine mpas_pool_add_field_3d_real(inPool, key, field)!{{{

      implicit none

      type (mpas_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field3DReal), pointer :: field

      type (mpas_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = mpas_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = MPAS_POOL_FIELD

         allocate(newmem % data)
         newmem % data % contentsType = MPAS_POOL_REAL
         newmem % data % contentsDims = 3
         newmem % data % contentsTimeLevs = 1
         newmem % data % r3 => field
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine mpas_pool_add_field_3d_real!}}}


!-----------------------------------------------------------------------
!  routine mpas_pool_add_field_4d_real
!
!> \brief MPAS Pool 4D Real field add routine
!> \author Michael Duda, Doug Jacobsen
!> \date   03/27/2014
!> \details
!> This routine inserts field into inPool when field is a 4D real field
!
!-----------------------------------------------------------------------
   subroutine mpas_pool_add_field_4d_real(inPool, key, field)!{{{

      implicit none

      type (mpas_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field4DReal), pointer :: field

      type (mpas_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = mpas_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = MPAS_POOL_FIELD

         allocate(newmem % data)
         newmem % data % contentsType = MPAS_POOL_REAL
         newmem % data % contentsDims = 4
         newmem % data % contentsTimeLevs = 1
         newmem % data % r4 => field
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine mpas_pool_add_field_4d_real!}}}


!-----------------------------------------------------------------------
!  routine mpas_pool_add_field_5d_real
!
!> \brief MPAS Pool 5D Real field add routine
!> \author Michael Duda, Doug Jacobsen
!> \date   03/27/2014
!> \details
!> This routine inserts field into inPool when field is a 5D real field
!
!-----------------------------------------------------------------------
   subroutine mpas_pool_add_field_5d_real(inPool, key, field)!{{{

      implicit none

      type (mpas_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field5DReal), pointer :: field

      type (mpas_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = mpas_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = MPAS_POOL_FIELD

         allocate(newmem % data)
         newmem % data % contentsType = MPAS_POOL_REAL
         newmem % data % contentsDims = 5
         newmem % data % contentsTimeLevs = 1
         newmem % data % r5 => field
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine mpas_pool_add_field_5d_real!}}}


!-----------------------------------------------------------------------
!  routine mpas_pool_add_field_0d_int
!
!> \brief MPAS Pool 0D Integer field add routine
!> \author Michael Duda, Doug Jacobsen
!> \date   03/27/2014
!> \details
!> This routine inserts field into inPool when field is a 0D integer field
!
!-----------------------------------------------------------------------
   subroutine mpas_pool_add_field_0d_int(inPool, key, field)!{{{

      implicit none

      type (mpas_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field0DInteger), pointer :: field

      type (mpas_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = mpas_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = MPAS_POOL_FIELD

         allocate(newmem % data)
         newmem % data % contentsType = MPAS_POOL_INTEGER
         newmem % data % contentsDims = 0
         newmem % data % contentsTimeLevs = 1
         newmem % data % i0 => field
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine mpas_pool_add_field_0d_int!}}}

