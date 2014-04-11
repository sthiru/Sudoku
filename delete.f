 	program sudoku
	implicit integer (a-z)
	parameter (n=3,m=n*n)
	dimension r(m,m),s(m,n,n)
	open(unit=20, file='sudo.dat')

 20	format(9(I1,1x))
 21	format(9(I1,2x),//)

	unknown=0

c	**************************READ***********************************

	   do i=1,m
		read(20,20)(r(i,j), j=1,m)
	   end do


	do i=1,m
	do h=0,n-1
	do j=1,n
	do k=1,n
	if(i.gt.h*n.and.i.le.(h+1)*n)then
	s(i,j,k)=r((h*n)+j,(i-1-(h*n))*n+k)
	call rowcol(n,h,i,j,k,row,col)
	write(*,*) i, j, k,"==", row, col
	
	end if
	end do
	end do
	end do
	end do

	
	stop
	end

	subroutine rowcol(n,h,i,j,k,frow,fcol)
	implicit integer (a-z)
	frow=(h*n)+j
	fcol=(i-1-(h*n))*n+k
	end
