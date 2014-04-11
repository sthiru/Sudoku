 	program sudoku
	implicit integer (a-z)
	parameter (n=3,m=n*n)
	dimension r(m,m),s(m,n,n),a(m,m),t(m,n,n)
	open(unit=20, file='sudo.dat')
	open(unit=21, file='sudodiv.dat')
 20	format(9(I1,1x))
 21	format(9(I1,2x),//)

c	**************************READ***********************************

 1	unknown=0
	rate=0
	   do i=1,m
		read(20,20)(r(i,j), j=1,m)
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

	call unk(m,r,unknown)
 	write(*,*)"no. of unknown values = ",unknown

c	*************************ASSIGN**********************************
	call assign(m,n,r,s,a,t)


 	call fill(m,n,r,s,unknown)

c	************************SOLUTION*********************************
	do i=1,m
	do j=1,m

	if(r(i,j).eq.0)then
 	write(*,*)"Alaissham tried to solve(1) the puzzle and found:"
	do l=1,m
	write(*,*),(r(l,k), k=1,m)
	end do
	rate=rate+10
	write(*,*)"no. of unknown values = ",unknown
	
	goto 215

	else

	   if(i.eq.m.and.j.eq.m)then
	   write(*,*)"Alaissham solved the puzzle and the solution easily:"
	   do l=1,m
	   write(*,*),(r(l,k), k=1,m)
	   end do
	rate=rate+10
	write(*,*)"Alaissham rating of puzzle = ", rate
	goto 2
	   end if

	end if

	end do
	end do

	if(rate.le.10)then
 	call assign(m,n,r,s,a,t)
	end if

c	***********************INTELIGENT(START)*************************
 	
 215	do 244 repeat=1,50
	do 245 i=1,m
	do 246 h=0,n-1
	do 247 j=1,n
	do 248 k=1,n
 	do 249 y=1,m
	do 250 val=1,m
	do 251 jj=1,n
	do 252 kk=1,n

	if(s(i,j,k).eq.0)then
	if(i.gt.h*n.and.i.le.(h+1)*n)then

c	*********************************************
	if(y.eq.r((h*n)+j,val).or.y.eq.r(val,(i-1-(h*n))*n+k).or.y.eq.s(i
     1,jj,kk))then
	goto 249

	else
	if(val.eq.m)then
	row=(h*n)+j
	col=(i-1-(h*n))*n+k
	call intell(h,i,j,k,m,n,row,col,r,s,y)
	goto 249
	end if

	end if
c	************************************************

	end if
	end if

 252	continue
 251	continue
 250	continue
 249	continue
 248	continue
 247	continue
 246	continue
 245	continue
 244	continue


	call intel(m,n,r,s)
	call fill(m,n,r,s,unknown)



c	**************INTELIGENT SOLUTION****************

	do i=1,m
	do j=1,m
	if(r(i,j).eq.0)then
 		write(*,*)"Alaissham Inteligently found:"
		do l=1,m
		write(*,*),(r(l,k), k=1,m)
		end do
	call unk(m,r,iunknown)
	write(*,*)"no. of unknown values = ",iunknown
	rate=rate+10
	goto 180
	else

	if(i.eq.m.and.j.eq.m)then
	write(*,*)"Alaissham solved the puzzle using Inteligent Algorithm and the solution is:"
	do l=1,m
	write(*,*),(r(l,k), k=1,m)
	end do
	rate=rate+10
	write(*,*)"Alaissham rating of puzzle = ", rate
	goto 2
	end if

	end if
	end do
	end do
c	************************INTELIGENT(END)**************************

 180	call fill(m,n,r,s,unknown)
	call intel(m,n,r,s)


c	*************************LOOP************************************


c	*************************SOLVE(1-m)******************************

 	do 998 repeat=1,2
	do 508 i=1,m
	do 518 h=0,n-1
	do 528 j=1,n
	do 538 k=1,n
	do 558 y=m,1,-1
	do 568 val=1,m
	do 578 jj=1,n
	do 588 kk=1,n

	if(s(i,j,k).eq.0)then
	if(i.gt.h*n.and.i.le.(h+1)*n)then
c	*********************************************
	if(y.eq.r((h*n)+j,val).or.y.eq.r(val,(i-1-(h*n))*n+k).or.y.eq.s(i
     1,jj,kk))then
	goto 558
	else

	if(val.eq.m)then
	s(i,j,k)=y
	r((h*n)+j,(i-1-(h*n))*n+k)=y
	call unk(m,r,munknown)

	if(unknown.lt.munknown)then
	rate=rate+10
	call fill(m,n,r,s,unknown)
	goto 215
	end if

	end if
	end if

c	************************************************

	end if
	end if

 588	continue
 578	continue
 568	continue
 558	continue
 538	continue
 528	continue
 518	continue
 508	continue
 998	continue

c	************************SOLUTION(1-m)****************************

	do i=1,m
	do j=1,m
	if(r(i,j).eq.0)then
 		write(*,*)"Alaissham tried to solve(1-m) the puzzle and found:"
		do l=1,m
		write(*,*),(r(l,k), k=1,m)
		end do
	rate=rate+10

	call unk(m,r,munknown)
	write(*,*)"no. of unknown values = ",munknown


	goto 2
	else
	if(ii.eq.m.and.jj.eq.m)then
	write(*,*)" Alaissham solved(1-m) the puzzle and the solution is:"
	do l=1,m
	write(*,*),(r(l,k), k=1,m)
	end do
	rate=rate+10
	write(*,*)"Alaissham rating of puzzle = ", rate
	end if

	end if
	end do
	end do


 2	write(*,*)"enter 0 to continue any other value to quit"
	read(*,*)l
	if (l.eq.0) then
	goto 1
	end if
	write(*,*)"for any feedback or suggestions, contact: alaissham@gmail.com"
	stop
	end

c	*******************FIND NO. OF UNKNOWN***************************

	subroutine unk(a,r,zero)
	integer zero,r,a
	dimension r(a,a)
	zero = 0
	do ii=1,a
	do jj=1,a
	if(r(ii,jj).eq.0)then
	zero=zero+1
	end if
	end do
	end do

	end

c	*************************ASSIGN**********************************
	subroutine assign(m,n,r,s,a,t)
	integer i,j,k,r,s,t,a
	dimension a(m,m),r(m,m),s(m,n,n),t(m,n,n)
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

	end

c	******************FILL 1 POSSIBLE VALUE**************************
	subroutine fill(m,n,r,s,unknown)
	implicit integer (a-z)
	dimension r(m,m),s(m,n,n)

	do 316 repeat=1,500
	do 317 i=1,m
	do 318 h=0,n-1
	do 319 j=1,n
	do 320 k=1,n
	if(s(i,j,k).eq.0)then
	if(i.gt.h*n.and.i.le.(h+1)*n)then
	do 323 x=1,m
	do 324 val=1,m
	do 325 jj=1,n
	do 326 kk=1,n

c	********CHECK(1-m) ROW, COLUMN & MINI MATRIX***********

	if(x.eq.r((h*n)+j,val).or.x.eq.r(val,(i-1-(h*n))*n+k).or.x.eq.s(i
     1,jj,kk))then
	goto 323
	else

	if(val.eq.m)then
c	************************************************
	do 337 y=m,1,-1
	do 338 vai=1,m
	do 339 jjl=1,n
	do 340 kkl=1,n
c	********CHECK(m-1) ROW, COLUMN & MINI MATRIX*********
	if(y.eq.r((h*n)+j,vai).or.y.eq.r(vai,(i-1-(h*n))*n+k).or.y.eq.s(i
     1,jjl,kkl))then
	goto 337
	else

	if(vai.eq.m)then
	if(x.ne.y)then
	goto 320
	else
	s(i,j,k)=y
	r((h*n)+j,(i-1-(h*n))*n+k)=y
	goto 320
	end if
	end if

	end if

 340	continue
 339	continue
 338	continue
 337	continue
c	************************************************
	end if

	end if

 326	continue
 325	continue
 324	continue
 323	continue
	end if
	end if
 320	continue
 319	continue
 318	continue
 317	continue
 316	continue

	call unk(m,r,unknown)

	end

c	*******************SUBROUTINE ROW,COL,SUB************************

	subroutine rowcol(n,h,i,j,k,frow,fcol)
	implicit integer (a-z)
	frow=(h*n)+j
	fcol=(i-1-(h*n))*n+k
	end

	subroutine subfnd(n,row,col,mini,sr,sc)
	implicit integer (a-z)
	do a=0,n-1
	do b=0,n-1
	if(row.gt.(a*n).and.row.le.((a+1)*n))then
	if(col.gt.(b*n).and.col.le.((b+1)*n))then
	mini=(a*n)+(b+1)
	sr=row-(a*n)
	sc=col-(b*n)
	end if
	end if
	end do
	end do
	end

c	***************************INTELIGENT****************************
	subroutine intel(m,n,r,s)
	implicit integer (a-z)
	dimension r(m,m),s(m,n,n)
	count=0
 41	do y=1,m
	count=0
	do i=1,m
	do j=1,m

	if(y.eq.r(i,j))then
	count=count+1
	end if

	end do
	end do
	
	if(count.eq.(m-1))then
	write(*,*)"there are m-1 values of",y

	do i=1,m
 	do 40 j=1,m
	if(r(i,j).eq.0)then
	call subfnd(n,i,j,sub,sr,sc)

 	do 42 h=1,m
	do k=1,n
	do l=1,n
	if(y.eq.r(h,j).or.y.eq.r(i,h).or.y.eq.s(sub,k,l))then
	goto 40
	else
	if(h.eq.m)then
	r(i,j)=y
	s(sub,sr,sc)=y
	write(*,*)i,j,"=",sub,sr,sc,"=",y
	call fill(m,n,r,s,unknown)
	call intell(h,i,j,k,m,n,row,col,r,s,y)
	goto 41
	end if
	goto 42
	end if
	end do
	end do
 42	continue	

	end if
 40	continue
	end do

	end if
	count=0
	end do

	end

c	***************************INTELIGENT****************************
	subroutine intell(h,i,j,k,m,n,row,col,r,s,y)
	implicit integer (a-z)
	dimension r(m,m),s(m,n,n)
	zero=0

c	************************MINI MATRIX******************************

	do jl=1,n
 	do 85 kl=1,n

	   if(s(i,jl,kl).eq.0)then
	   if(j.ne.jl.or.k.ne.kl)then

	call rowcol(n,h,i,jl,kl,row1,col1)
	
	do var=1,m
	if(y.eq.r(row1,var).or.y.eq.r(var,col1))then
	goto 85
	else
	   if(var.eq.m)then
	   return
	   end if

	end if
	end do

	   end if
	   end if

 85	continue
	end do
	write(*,*) i,j,k,"=",y
	s(i,j,k)=y
	r(row,col)=y

	end
