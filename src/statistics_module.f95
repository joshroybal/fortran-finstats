! Modern Fortran statistical computations module
module statistics_module
contains
   ! function returns double precision mean of all array elements
   function mean(x, n) result(m)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x
      ! function result location
      double precision :: m
      ! local variables
      integer :: i
      ! processing
      m = 0
      do i = 1, n
         m = m + x(i)
      end do
      m = m / n
   end function mean

   ! function returns double precision population variance of x
   function pop_var(x, n) result(v)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x
      ! function return location
      double precision :: v
      ! local variables
      integer :: i, m
      m = mean(x, n)
      v = 0.
      do i = 1, n
         v = v + (x(i) - m)**2
      end do
      v = v / n
   end function pop_var
   
   ! function returns double precision sample variance of x
   function sam_var(x, n) result(v)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x
      ! function return location
      double precision :: v
      ! local variables
      integer :: i, m
      m = mean(x, n)
      v = 0.
      do i = 1, n
         v = v + (x(i) - m)**2
      end do
      v = v / (n - 1)
   end function sam_var

   ! function returns double precision population standard deviation of x
   function pop_std(x, n) result(s)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x
      ! function return location
      double precision :: s
      ! processing
      s = sqrt(pop_var(x, n))
   end function pop_std
   
   ! function returns double precision sample standard deviation of x
   function sam_std(x, n) result(s)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x
      ! function return location
      double precision :: s
      ! processing
      s = sqrt(sam_var(x, n))
   end function sam_std

   ! functions returns index of kth element of x
   ! C. A. R. Hoare's algorithm
   ! implementation works on index array only - preserves data set array
   function quick_select(k, n, x) result(v)
      implicit none
      ! dummy arguments
      integer, intent(in) :: k, n
      double precision, dimension(n), intent(in) :: x
      ! function return location
      double precision :: v
      ! local variables
      integer :: i, j, left, right, tmp
      integer, dimension(n) :: idx
      double precision :: pivot
      ! processing
      do i=1,n
         idx(i)=i
      end do
      left=1
      right=n
      do while (left < right)
         pivot=x(idx(k))
         i=left
         j=right
         do
            do while (x(idx(i)) < pivot)
               i=i+1
            end do
            do while (pivot < x(idx(j)))
               j=j-1
            end do
            if (i <= j) then
               tmp=idx(i)
               idx(i)=idx(j)
               idx(j)=tmp
               i=i+1
               j=j-1
            end if
            if (i > j) exit
         end do
         if (j < k) left=i
         if (k < i) right=j
      end do
      v=x(idx(k))
   end function quick_select

   ! function returns median of x
   function median(x, n) result(mdn)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x
      ! function return location
      double precision :: mdn
      ! processing
      if (mod(n, 2) == 1) then
         mdn=quick_select(n/2+1, n, x)
      else
         mdn=(quick_select(n/2, n, x) + quick_select(n/2+1, n, x))/2
      end if
   end function median

   !funcition returns median absolute deviation of x
   function median_deviation(x, n) result(mad)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x
      ! function return location
      double precision :: mad
      ! local variables
      integer :: i
      double precision :: mdn
      double precision, dimension(n) :: y
      ! processing
      mdn = median(x, n)
      do i = 1, n
         y(i) = abs(x(i) - mdn)
      end do
      mad = median(y, n)
   end function median_deviation

   ! function computes the skewness of x (expected value 1/N)
   function skewness(x, n) result(skw)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x
      ! function return location
      double precision :: skw
      ! processing
      skw = (3 * (mean(x, n) - median(x, n))) / sam_std(x, n)
   end function skewness

   ! function computes the mean absolute deviation of x
   function mean_deviation(x, n) result(aad)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x
      ! function return location
      double precision :: aad
      ! local variables
      integer :: i
      double precision :: m
      ! processing
      m = mean(x, n)
      aad = 0.
      do i=1,n
         aad = aad + abs(x(i) - m)
      end do
      aad = aad / n
   end function mean_deviation

   ! function computes the covariance of data sets x and y
   function cov(x, y, n) result(covar)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x, y
      ! function return location
      double precision :: covar
      ! local variables
      integer :: i
      double precision mx, my, dx, dy
      ! processing
      mx = mean(x, n)
      my = mean(y, n)
      covar = 0
      do i = 1, n
         dx = x(i) - mx
         dy = y(i) - my
         covar = covar + dx * dy
      end do
      covar = covar / n
   end function cov

   function corr(x, y, n) result(p)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: x, y
      ! function return location
      double precision :: p
      ! processing
      p = cov(x, y, n) / (sam_std(x, n) * sam_std(y, n))
   end function corr
end module statistics_module
