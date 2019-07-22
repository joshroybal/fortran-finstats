! Modern Fortran .csv data processing module
module csv_module
contains
   ! integer function computes no. of fields by counting header line commas
   function count_fields(header) result(noflds)
      implicit none
      ! dummy arguments
      character (len = *), intent(in) :: header
      ! function return location
      integer :: noflds
      ! local variables
      integer :: i, length
      ! processing
      noflds = 1
      length=LEN(TRIM(header))
      do i=1,length
         if (header(i:i) .eq. ',') noflds = noflds + 1
      end do
   end function count_fields

   ! subroutine fetches all fields from csv record
   subroutine read_fields(fields, record, noflds)
      implicit none
      ! dummy arguments
      integer, intent(in) :: noflds
      character (len = *), intent(in) :: record
      character (len = *), dimension(noflds), intent(out) :: fields
      ! local data
      integer :: i, k, reclen, fldno
      ! clear the fields array
      do i=1,noflds
        fields(i)=''
      end do
      i=1
      fldno=1
      k=1
      reclen=LEN(TRIM(record))
      do while (i .le. reclen)
         do while (record(i:i) .ne. ',' .and. i .le. reclen)
            if (record(i:i) .ne. '"') then
               fields(fldno)(k:k)=record(i:i)
               i=i+1
               k=k+1
            else
               ! skip through double quoted escaped sections of the csv record
               i=i+1
               do while (record(i:i) .ne. '"')
                  fields(fldno)(k:k)=record(i:i)
                  i=i+1
                  k=k+1
               end do
               i=i+1
            end if
         end do
         i=i+1
         fldno=fldno+1
         k=1
      end do
   end subroutine read_fields

   ! function fetches kth field from csv record
   function get_field(record, n) result(field)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      character (len = *), intent(in) :: record
      ! function return location
      character (len = 25) field
      ! local variables
      integer :: i, k, length, fldno
      ! processing
      ! clear function string
      field=''
      i=1
      fldno=1
      k=1
      length=LEN(TRIM(record))
      do while (i .le. length)
         do while (record(i:i) .ne. ',' .and. i .le. length)
            if (record(i:i) .ne. '"') then
               if (fldno == n) field(k:k)=record(i:i)
               i=i+1
               k=k+1
               if (k > 25) return
            else
               ! skip through double quoted escaped sections of the csv record
               i=i+1
               do while (record(i:i) .ne. '"')
                  if (fldno == n) field(k:k)=record(i:i)
                  i=i+1
                  k=k+1
                  if (k > 25) return
               end do
               i=i+1
            end if
         end do
         i=i+1
         fldno=fldno+1
         k=1
      end do
   end function get_field
end module
