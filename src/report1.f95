program report1
   use statistics_module
   use csv_module
   implicit none
   ! variable declarations
   integer, parameter :: LIM = 80000
   integer :: eof, err, n, i, j
   double precision :: x
   double precision, dimension(LIM) :: ax
   character (len = 50) :: infile
   character (len = 80) :: record
   character (len = 25) :: field
   ! input
   call getarg(1, infile)
   n = 0
   open (7,file=infile,status='old')
   read (7,'(a)',iostat=eof) record  ! eat header
   if ( eof .ne. 0 ) stop 'no data'
   do
      read (7,'(a)',iostat=eof) record
      if ( eof .ne. 0 ) exit
      field = get_field(record, 6)  ! read kth csv field as string
      read (field,*,iostat=err) x   ! read double from string
      if (err .ne. 0) cycle         ! skip bad records
      n = n + 1
      ax(n) = x
   end do
   close (7)
   write (*,*) 'n = ', n
   write (*,*) 'min = ', quick_select(1, n, ax)
   write (*,*) 'med = ', median(ax, n)
   write (*,*) 'max = ', quick_select(n, n, ax)
   write (*,*) 'avg = ', mean(ax, n)
   write (*,*) 'var = ', pop_var(ax, n)
   write (*,*) 'std = ', pop_std(ax, n)
   write (*,*) 'mean deviation = ', mean_deviation(ax, n)
   write (*,*) 'median deviation = ', median_deviation(ax, n)
   write (*,*) 'skewness = ', skewness(ax, n)
end program report1
