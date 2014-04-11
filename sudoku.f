 	program sudoku
 	integer r,s,i,j,k,a,h,t
	parameter (n=3,m=n*n)
	dimension r(m,m),s(m,n,n),a(m,m),t(m,n,n)
	open(unit=20, file='sudo.dat')
	open(unit=21, file='sudodiv.dat')
 20	format(9(I1,1x))
 21	format(9(I1,2x),//)

	unknown=0

c	**************************READ***********************************

 1	write(*,*)"enter 0 to feed the values manually "
	write(*,*)"anyother key to read the values from file sudo.dat"

	read(*,*),value
	if(value.eq.0)then	
	   write(*,*)"enter the values in ROWS"
	   do i=1,m
	   do j=1,m
		read(*,*),r(i,j)
	   end do
	   end do
	else
	   do i=1,m
		read(20,20)(r(i,j), j=1,m)
	   end do
	end if

c	**************************SPLIT**********************************

	do i=1,m
	do h=0,n-1
	do j=1,n
	do k=1,n
	if(i.gt.h*n.and.i.le.(h+1)*n)then
	s(i,j,k)=r((h*n)+j,(i-1-(h*n))*n+k)
	end if
	end do
	end do
	end do
	end do

c	**************************WRITE**********************************
	write(*,*)"the given puzzle is"
	do j=1,m
	write(*,*),(r(j,k), k=1,m)
	end do

	do i=1,m
	do j=1,n
	write(21,21),(s(i,j,k), k=1,n)
	end do
	end do

c	***********************VERIFY(VALUES)****************************

	do i=1,m
	do j=1,m
	if((r(i,j).ge.0.and.r(i,j).le.m))then
	else
	write(*,*)"entered values are not valid"
	goto 2
	end if

	end do
	end do

c	***********************VERIFY(MINI MATRIX)***********************

	do h=1,m
	do i=1,n
	do j=1,n
	do k=1,n
	do l=1,n

	if(i.ne.k.and.j.ne.l)then
	if(s(h,i,j).ne.0)then
	if(s(h,i,j).eq.s(h,k,l))then
  	write(*,*)"entered values are not valid"
	goto 2
	end if
	end if
	end if

	end do
	end do
	end do
	end do
	end do

c	***********************VERIFY(ROWS & COLUMNS)********************

	do i=1,m
	do j=1,m
	do k=1,m

	if(j.ne.k)then
	if(r(i,j).ne.0)then
	if(r(i,j).eq.r(i,k).and.r(j,i).eq.r(k,i))then
  	write(*,*)"entered values are not valid"
	goto 2
	end if
	end if
	end if

	end do
	end do
	end do
c	*************************ASSIGN**********************************
	do i=1,m
	do j=1,m
	a(i,j)=r(i,j)
	end do
	end do

	do i=1,m
	do j=1,n
	do k=1,n
	t(i,j,k)=s(i,j,k)
	end do
	end do
	end do

c	*******************FIND NO. OF UNKNOWN***************************

	do i=1,m
	do j=1,m
	if(r(i,j).eq.0)then
	unknown=unknown+1
	end if
	end do
	end do
	write(*,*)"no. of unknown values = ",unknown
	unknown=0

c	*************************SOLVE(1-m)******************************
	do 999 repeat=1,25
	do 50 i=1,m
	do 51 h=0,n-1
	do 52 j=1,n
	do 53 k=1,n
	do 54 x=1,m
	do 55 y=m,1,-1
	do 56 val=1,m
	do 57 jj=1,n
	do 58 kk=1,n

	if(s(i,j,k).eq.0)then
	if(i.gt.h*n.and.i.le.(h+1)*n)then
c	*********************************************
	if(x.eq.r((h*n)+j,val).or.x.eq.r(val,(i-1-(h*n))*n+k).or.x.eq.s(i
     1,jj,kk))then
	goto 54
	else

	if(val.eq.m)then
	s(i,j,k)=x
	r((h*n)+j,(i-1-(h*n))*n+k)=x
	goto 53
	end if
	end if


c	************************************************

	end if
	end if

 58	continue
 57	continue
 56	continue
 55	continue
 54	continue
 53	continue
 52	continue
 51	continue
 50	continue
 999	continue

c	*************************SOLVE(m-1)******************************

 	do 998 repeat=1,25
	do 508 i=1,m
	do 518 h=0,n-1
	do 528 j=1,n
	do 538 k=1,n
	do 548 x=1,m
	do 558 y=m,1,-1
	do 568 val=1,m
	do 578 jj=1,n
	do 588 kk=1,n

	if(t(i,j,k).eq.0)then
	if(i.gt.h*n.and.i.le.(h+1)*n)then
c	*********************************************
	if(y.eq.a((h*n)+j,val).or.y.eq.a(val,(i-1-(h*n))*n+k).or.y.eq.t(i
     1,jj,kk))then
	goto 558
	else

	if(val.eq.m)then
	t(i,j,k)=y
	a((h*n)+j,(i-1-(h*n))*n+k)=y
	goto 538
	end if
	end if

c	************************************************

	end if
	end if

 588	continue
 578	continue
 568	continue
 558	continue
 548	continue
 538	continue
 528	continue
 518	continue
 508	continue
 998	continue

c	************************SOLUTION*********************************

	do i=1,m
	do j=1,m
	if(r(i,j).eq.0)then
 		write(*,*)"Alaissham tried to solve the puzzle and found:"
		do l=1,m
		write(*,*),(r(l,k), k=1,m)
		end do
 		write(*,*)"The other possible values are:"
		do l=1,m
		write(*,*),(a(l,k), k=1,m)
		end do
	write(*,*)"for any feedback or suggestions, contact: alaissham@gmail.com"
		goto 2 	
	end if
	end do
	end do


	write(*,*)" Alaissham solved the puzzle and the solution is:"
	do l=1,m
	write(*,*),(r(l,k), k=1,m)
	end do
c	*************************LOOP************************************

	write(*,*)"for any feedback or suggestions, contact: alaissham@gmail.com"
 2	write(*,*)"enter 0 to continue any other value to quit"
	read(*,*)l
	if (l.eq.0) then
	goto 1
	end if

	stop
	end
