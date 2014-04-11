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
	call unk(m,r,unknown)
 	write(*,*)"no. of unknown values = ",unknown

c	****************************SOLVE*********************************
 	call fill(m,n,r,s,unknown)

	write(*,*)"The possible values are"
	do j=1,m
	write(*,*),(r(j,k), k=1,m)
	end do
	call unk(m,r,unknown)
 	write(*,*)"no. of unknown values = ",unknown

c	***************************INTELIGENT****************************
 	do 998 repeat=1,2
	do 508 i=1,m
	do 518 h=0,n-1
	do 528 j=1,n
	do 538 k=1,n
	do 558 y=1,m
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
	call intel(h,i,j,k,m,n,row,col,r,s,y)
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

	write(*,*)"The possible values are"
	do j=1,m
	write(*,*),(r(j,k), k=1,m)
	end do
	call unk(m,r,unknown)
 	write(*,*)"no. of unknown values = ",unknown

	stop
	end

c	*****************************************************************
c	*****************************************************************



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
	subroutine intel(h,i,j,k,row,col,r,s,y)
	implicit integer (a-z)

c	************************MINI MATRIX******************************
	do jl=1,n
	do kl=1,n

	   if(j.ne.jl.and.k.ne.kl)then
	   if(s(i,jl,kl).eq.0)then

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
c	*****************************************************************
	s(i,j,k)=y
	r(row,col)=y
c	**************************COLUMN*********************************

	do var=1,m
	if(var.ne.row)then
	if(r(var,col).eq.0)then
	do val=1,m
	do jh=1,n
	do kh=1,n
	call subfnd(n,var,col,i1,sr,sc)
	row2=var
 	if(y.eq.r(row2,val).or.y.eq.s(i1,jh,kh))then
	goto 113
	else
	   if(val.eq.m)then
	   return
	   end if
	end if

	end do
	end do
	end do
	end if
	end if
 113	continue	
c	*****************************************************************
	s(i,j,k)=y
	r(row,col)=y
c	****************************ROW**********************************
	do var=1,m
	if(var.ne.col)then
	if(r(row,var).eq.0)then
	do val=1,m
	do jh=1,n
	do kh=1,n
	call subfnd(n,row,var,i2,sr,sc)
	col2=var
 	if(y.eq.r(val,col2).or.y.eq.s(i2,jh,kh))then
	goto 161
	else
	   if(var.eq.m)then
	   return
	   end if
	end if

	end do
	end do
	end do
	end if
	end if
 161	continue	
c	*****************************************************************

	s(i,j,k)=y
	r(row,col)=y
	
	end
