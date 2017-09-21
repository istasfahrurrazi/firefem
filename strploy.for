	SUBROUTINE strplt
	use msflib
C
      PARAMETER 	   ( NFONTS = 1 )
      INTEGER*2 	   dummy, x, y,iend
      INTEGER*4 	   ifont
      CHARACTER*10	   options(NFONTS)
      CHARACTER*20	   list
      CHARACTER*64	   fontpath,dm

C

C
	integer ierr
	character*12 stgfile
	character*10 dumtext
	character*5 dumx
	real time1(300),str(300)
	integer*2 nxc(600),nyc(600),x1,y1,nx,ny,x2,y2,
     .nxor,nyor,nxc1,nxc2,nyc1,nyc2,ndiv,nydiv
	integer*4 ncolor
      RECORD /xycoord/	   xy
C
C FOR TEXT
C
      RECORD /fontinfo/    fi
      DATA options / "t'courier'"/
C
	nx=540
	ny=370

C
C OPENING A STRENGTH FILE
C
7	continue
	print *,'To view the strength curve'
	print *,'--------------------------------------'
	PRINT *,'Please enter your strength filename :'
	PRINT *,'<The name is the same as output file name but with'
	PRINT *,' extesion .pst. For example, ibeam1.pst>'
	PRINT *,'TYPE QUIT TO RETURN TO MAIN MENU'
	print *,'--------------------------------------'
	READ(*,'(a12)') stgfile
	if(stgfile.eq.' ') then
	print *,'Your filename is not valid'
	goto 7
      endif
	if(stgfile.eq.'QUIT'.OR.stgfile.eq.'quit') then
	return
      endif

      OPEN(UNIT=1,FILE=stgfile,STATUS='OLD',iostat=ierr)
	if(ierr.ne.0) then
	print *,'Your filename is not valid'
	goto 7
	endif
      DO NCOD=1,10000
      READ(1,'(a5)',END=100) dumtext  !read title
	IF(DUMTEXT.EQ.'  (Mi'.or.DUMTEXT.EQ.'  (MI') THEN
C
	read(1,'(a5)') dumtext
	goto 1
	ENDIF
	strmax=0.0
	timemax=0.0
	end do
1	do ir=1,500
	read(1,*,end=100) time1(ir),str(ir)
	write(*,*) time1(ir),str(ir)
	if(str(ir).gt.strmax) strmax=str(ir)
	if(time1(ir).gt.timemax) timemax=time1(ir)
	end do
100	continue
	if(timemax.eq.0.0) then
	print *,'Your strength file contains invalid information'
	print *,'Check your strength file and rerun the program'
	pause 'Press enter'
	goto 999
	endif
	close(unit=1)
	nn=ir-1
	do ij=1,nn
	nxc(ij)=60+int2(time1(ij)/timemax*nx)
	nyc(ij)=430-int2(str(ij)/strmax*ny)
	nxor=20
	nyor=470
	nymax=0
	nxmax=620
c set for scale
	nxc1=58
	nxc2=600
	nyc1=60
	nyc2=432
	end do
c start draw lines
C
C     Find graphics mode.
C
      IF ( setvideomode( $MAXRESMODE ).EQ. 0 )
     +	   STOP 'Error:  cannot set graphics mode'
C
C     Draw lines.
C
c AXIS DRAW
	call moveto(0,0,xy)
	status=lineto(620,0)
	call moveto(620,0,xy)
	status=lineto(620,470)
	call moveto(620,470,xy)
	status=lineto(0,470)
	call moveto(0,470,xy)
	status=lineto(0,0)
	call moveto(60,430,xy)
	status=lineto(600,430)
	call moveto(60,430,xy)
	status=lineto(60,50)
	call moveto(60,50,xy)
	status=lineto(54,56)
	call moveto(60,50,xy)
	status=lineto(66,56)
	call moveto(600,430,xy)
	status=lineto(594,424)
	call moveto(600,430,xy)
	status=lineto(594,436)

c draw the scale lines
	open(unit=11,file='x.tmp',status='unknown')
	open(unit=12,file='y.tmp',status='unknown')
      status  = setcolor( 8 )
c   y-scale
	nodiv=20
	dival=strmax/real(nodiv)
	divi=0.0
	ndiv=int2(370/20)
	nydiv=430   ! origin of the graf
	do idiv=1,20
	divi=divi+dival
	write(12,12) divi
12	format(1x,e8.3)
	nydiv=nydiv-ndiv
	call moveto(nxc1,nydiv,xy)
	status=lineto(nxc2,nydiv)
	end do
c  x-scale
	nodiv=5
	dival=timemax/real(nodiv)
	divi=0.0
	ndiv=int2(540/5)
	nxdiv=60
	do idiv=1,5
	divi=divi+dival
	write(11,11) divi
11	format(1x,f5.1)
	nxdiv=nxdiv+ndiv
	call moveto(nxdiv,nyc1,xy)
	status=lineto(nxdiv,nyc2)
	end do
	close(unit=11)
	close(unit=12)
c draw the contents
	nnn=nn-1
	do ik=1,nnn
	x1=nxc(ik)
	y1=nyc(ik)
	x2=nxc(ik+1)
	y2=nyc(ik+1)
      ncolor = 15
      status  = setcolor( 12 )
      CALL moveto( x1, y1, xy )
	status=lineto(x2,y2)
      END DO
C
C TEXT STARTS HERE
C
      ifont = 1
      IF( registerfonts( '*.FON' ). LT. 0 ) THEN
	 WRITE (*, '(A/)') ' Enter directory for .FON files:'
	 READ (*, '(A )') fontpath
	 iend = INDEX( fontpath, ' ' )
	 fontpath( iend:iend + 5 ) = '\.FON'
	 IF( registerfonts( fontpath ). LT. 0 )
     +	    STOP 'Error: cannot find font files'
      ENDIF
	 list = options(ifont) // 'h12w8'
	 IF( setfont( list ) .GE. 0 ) THEN
	    IF( getfontinfo( fi ) .NE. 0 ) THEN
	       CALL outtext( 'Error:  cannot get font info' )
	       READ (*,*)
	    END IF
c text for x-axis
	    x=300
	    y=460
	    CALL moveto( x, y, xy )
	    dummy = setcolor( 15 )
	    CALL setgtextvector( 0, 0 )
	    CALL outgtext('Time (Minutes)')
c text for y axis
	    x=57
	    y=40
	    CALL moveto( x, y, xy )
	    dummy = setcolor( 15 )
	    CALL setgtextvector( 0, 0 )
	    CALL outgtext('Strength (Newton)')
C
C text for title
C
	    x=65
	    y=15
	    CALL moveto( x, y, xy )
	    dummy = setcolor( 10 )
	    CALL setgtextvector( 0, 0 )
	    dm='Graf Strength versus Time for file:'//stgfile
	    CALL outgtext(dm)
C
C text for axis
C
C write the text
C   y-scale
C
	nodiv=20
	dival=strmax/real(nodiv)
	divi=0.0
	ndiv=int2(370/20)
	nydiv=430   ! origin of the graf
	open(unit=12,file='y.tmp',status='old')
	do idiv=1,20
	read(12,'(a10)') dumtext
	nydiv=nydiv-ndiv
	call moveto(0,nydiv-5,xy)
	CALL setgtextvector( 0, 0 )
	CALL outgtext(dumtext)
	end do
	close(unit=12)
c  x-scale
	nodiv=5
	dival=timemax/real(nodiv)
	divi=0.0
	ndiv=int2(540/5)
	nxdiv=60
	open(unit=11,file='x.tmp',status='old')
	do idiv=1,5
	read(11,'(a5)') dumx
	divi=divi+dival
	read(dumtext,'(a10)') divi
	nxdiv=nxdiv+ndiv
	call moveto(nxdiv-25,445,xy)
	CALL setgtextvector( 0, 0 )
	CALL outgtext(dumx)
	end do
	close(unit=11)


ccccccc end text for axis
	 ELSE
	    CALL outtext( 'Error:  cannot set font' )
	 END IF
	read(*,*)
      CALL unregisterfonts()
      dummy = setvideomode( $DEFAULTMODE )
CCCCCCCCCC END OF TEXT CCCCCCCCCCCCC
      status = setvideomode( $DEFAULTMODE )
c
c	result=setexitqq(QWIN$EXITNOPERSIST)
999	return
	end
