program report2
   use statistics_module
   use csv_module
   implicit none
   ! interface declartions
   interface
      subroutine compute_stats1(x, n, results, k)
         ! dummy arguments
         integer, intent(in) :: n, k
         double precision, dimension(n), intent(in) :: x
         double precision, dimension(k), intent(out) :: results
      end subroutine compute_stats1
      subroutine compute_stats2(x, y, n, results, k)
         ! dummy arguments
         integer, intent(in) :: n, k
         double precision, dimension(n), intent(in) :: x, y
         double precision, dimension(2,k), intent(out) :: results
      end subroutine compute_stats2
   end interface
   ! variable declarations
   integer, parameter :: LIM = 40000
   integer :: eof1, eof2, err1, err2, xn, yn, n, i
   double precision :: x, y
   double precision, dimension(LIM) :: ax, ay
   double precision, dimension(9) :: xresults, yresults
   double precision, dimension(2,10) :: xyresults
   logical :: flag
   character (len = 50) :: infile1, infile2
   character (len = 80) :: record1, record2
   character (len = 25) :: field
   character (len = 12) :: date1, date2
   character (len = 4), dimension(9) :: labels
   character (len = 35), dimension(2) :: headers

   ! initialization
   labels = (/ 'min', 'max', 'med', 'avg', 'var', 'std', 'aad', 'mad', 'skw' /) 
   ! input
   call getarg(1, infile1)
   call getarg(2, infile2)
   ! get x data and compute x statistics
   xn = 0
   open (7,file=infile1,status='old')
   read (7,'(a)',iostat=eof1) record1  ! eat header
   if ( eof1 .ne. 0 ) stop 'no data'
   do
      read (7,'(a)',iostat=eof1) record1
      if ( eof1 .ne. 0 ) exit
      field = get_field(record1, 6)    ! read kth csv field as string
      read (field,*,iostat=err1) x     ! read double from string
      if (err1 .ne. 0) cycle           ! skip bad records
      xn = xn + 1
      ax(xn) = x
   end do
   close (7)
   call compute_stats1(ax, xn, xresults, 9)
   ! get y data and compute y statistics
   yn = 0
   open (8,file=infile2,status='old')
   read (8,'(a)',iostat=eof2) record2  ! eat header
   if ( eof2 .ne. 0 ) stop 'no data'
   do
      read (8,'(a)',iostat=eof2) record2
      if ( eof2 .ne. 0 ) exit
      field = get_field(record2, 6)    ! read kth csv field as string
      read (field,*,iostat=err2) y     ! read double from string
      if (err2 .ne. 0) cycle           ! skip bad records
      yn = yn + 1
      ay(yn) = y
   end do
   close (8)
   call compute_stats1(ay, yn, yresults, 9)
   ! get merged data and compute xy statistics
   if ( yn > xn ) then                 ! optimization and labeling kludge
      open (7,file=infile1,status='old')
      open (8,file=infile2,status='old')
      headers(1)=infile1
      headers(2)=infile2
   else
      open (7,file=infile2,status='old')
      open (8,file=infile1,status='old')
      headers(1)=infile2
      headers(2)=infile1
   end if

   n = 0
   read (7,'(a)',iostat=eof2) record1  ! eat header
   read (8,'(a)',iostat=eof2) record2  ! eat header
   do
      read (7,'(a)',iostat=eof1) record1
      if ( eof1 .ne. 0 ) exit
      date1 = get_field(record1, 1)
      ! now read through file 2 until date matches
      do
         read (8,'(a)',iostat=eof2) record2
         if (eof2 .ne. 0) then
            flag = .true.
            rewind (8)
            exit
         end if
         date2 = get_field(record2, 1)
         if ( date1 == date2 ) exit
      end do
      if ( flag .eqv. .false. ) then
         field = get_field(record1, 6)
         read (field,*,iostat=err1) x
         if ( err1 .ne. 0 ) then
            flag = .false.
            cycle
         end if
         field = get_field(record2, 6)
         read (field,*,iostat=err2) y
         if ( err2 .ne. 0) then
            flag = .false.
            cycle
         end if
         n = n + 1
         ax(n) = x
         ay(n) = y
      end if
      flag = .false.
   end do
   close (7)
   close (8)
   call compute_stats2(ax, ay, n, xyresults, 10)
   ! display x results
   write (*,*) 'file = ', infile1
   write (*,*) 'n = ', xn
   do i = 1, 9
      write (*,*) labels(i), ' = ', xresults(i)
   end do
   ! display y results
   write (*,*) 'file = ', infile2
   write (*,*) 'n = ', yn
   do i = 1, 9
      write (*,*) labels(i), ' = ', yresults(i)
   end do   
   ! display xy results
   write (*,*) 'merged data'
   write (*,*) 'n = ', n
   write (*,*) headers(1), headers(2)
   do i = 1, 9
      write (*,*) 'x', labels(i), ' = ', xyresults(1,i), & 
      'y', labels(i), ' = ', xyresults(2,i)
   end do      
   write (*,*) 'cov(x,y) = ', xyresults(1,10)
   write (*,*) 'corr(x,y) = ', xyresults(2,10)
end program report2

subroutine compute_stats1(x, n, results, k)
   use statistics_module
   implicit none
   ! dummy arguments
   integer, intent(in) :: n, k
   double precision, dimension(n), intent(in) :: x
   double precision, dimension(k), intent(out) :: results
   ! processing
   results(1) = quick_select(1, n, x)
   results(2) = quick_select(n, n, x)
   results(3) = median(x, n)
   results(4) = mean(x, n)
   results(5) = pop_var(x, n)
   results(6) = pop_std(x, n)
   results(7) = mean_deviation(x, n)
   results(8) = median_deviation(x, n)
   results(9) = skewness(x, n)
end subroutine compute_stats1

subroutine compute_stats2(x, y, n, results, k)
   use statistics_module
   implicit none
   ! dummy arguments
   integer, intent(in) :: n, k
   double precision, dimension(n), intent(in) :: x, y
   double precision, dimension(2,k), intent(out) :: results
   ! processing
   results(1,1) = quick_select(1, n, x)
   results(2,1) = quick_select(1, n, y)
   results(1,2) = quick_select(n, n, x)
   results(2,2) = quick_select(n, n, y)
   results(1,3) = median(x, n)
   results(2,3) = median(y, n)   
   results(1,4) = mean(x, n)
   results(2,4) = mean(y, n)
   results(1,5) = sam_var(x, n)
   results(2,5) = sam_var(y, n)
   results(1,6) = sam_std(x, n)
   results(2,6) = sam_std(y, n)
   results(1,7) = mean_deviation(x, n)
   results(2,7) = mean_deviation(y, n)
   results(1,8) = median_deviation(x, n)
   results(2,8) = median_deviation(y, n)
   results(1,9) = skewness(x, n)
   results(2,9) = skewness(y, n)
   results(1,10) = cov(x, y, n)
   results(2,10) = corr(x, y, n)
end subroutine compute_stats2
