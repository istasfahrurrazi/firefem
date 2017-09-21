C To visualize the temperature distribution.
	SUBROUTINE TEMPDIST
      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
      character*5    node(600)
      CHARACTER*4	   ELE,CRD
      CHARACTER*12   FNAME,FELE,FCRD
      CHARACTER*1	   BLANK
      CHARACTER      TCRT*60,TIMETX*10
      REAL xd(1000),yd(1000),t(1000)
      CHARACTER sinbad*12,DUMMY1*8,OOT*4
      INTEGER*2   x1, y1, x2, y2, x3, y3, NXC(1000)
     .		   ,NYC(1000),X12,X23,X13,
     .            Y13,Y23,Y12,XCD1,XCD2,XCD3,XCD4,
     .		    YCD1,YCD2,YCD3,YCD4,
     .			XR1,XR2,YR1,YR2
	INTEGER ID1
      REAL xc(1000),yc(1000),D2,TIME
      PARAMETER(BLANK=' ')
C
	print *,'---------------------------------------------'
	print *,'This program is to view the nodal temperature'
	print *,'distributiom from the main output file for '
	print *,'analysis of Istas'' MSc. project.'
	print *,'Make sure the computer is using 256 colors.'
	print *,'-----------------------------------------------'
	PRINT *,'TYPE QUIT TO RETURN TO MAIN MENU'
      PRINT 1,' Enter name of output file (without extension):'
1     FORMAT(A, \)
      READ(*,'(A12)') sinbad
	IF(SINBAD.EQ.'QUIT'.OR.SINBAD.EQ.'quit') RETURN
      FIRST=1
      OOT='.OUT'
      DO NEXT=1,14
	IF(sinbad(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    sinbad=sinbad(FIRST:MNEXT)//OOT
	    GO TO 4
	ENDIF
      END DO
4	print *,'-----------------------------------------------'
      PRINT 1,' Enter time to see (min) : '
      READ(*,*) time
      open(unit=1,file=sinbad,status='old')
	open(unit=5,file='check.tmp')
      OPEN(UNIT=3,FILE='TEMP.DEL',STATUS='UNKNOWN')
	WRITE(3,*) TIME
	REWIND 3
	READ(3,'(A10)') TIMETX
	CLOSE(UNIT=3)
	DO ID=1,10000
      READ(1,'(A8)') DUMMY1
      IF(DUMMY1.EQ.' FIRE  &') GOTO 3
      END DO
3     nnmax=0
      DO while(.true.)
      READ(1,*,end=999,iostat=ierr) d1,d2,id1,xdd,ydd,td,d3
      if(d2.eq.time) then
       xd(id1)=xdd
       yd(id1)=ydd
       t(id1)=td
       nnmax=id1
      endif
41    END DO
999	continue
C
      DO ij=1,nnmax
       if(xd(ij).gt.xmax) xmax=xd(ij)
       if(yd(ij).gt.ymax) ymax=yd(ij)
      END DO
	close(unit=1)
C ***********************************************************
	print *,'-----------------------------------------------'
	PRINT *,'Important :'
	PRINT *,'========='
	PRINT *,'Make sure the output and datafile are refering '
	PRINT *,'the same meshing or else the program will not'
	PRINT *,'work correctly and you have to repeat it once again'
	print *,'-----------------------------------------------'
	print *,'The program will automatically add extension ELE and'
	print *,'CRD into the name of datafile that you have given.'
      PRINT *,'Enter name of datafile (without extension) :'
      READ(*,'(a)') FNAME
	print *,'-----------------------------------------------'
      FIRST=1
      ELE='.ELE'
      CRD='.CRD'
      DO NEXT=1,14
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    FELE=FNAME(FIRST:MNEXT)//ELE
	    FCRD=FNAME(FIRST:MNEXT)//CRD
	    GO TO 2
	ENDIF
      END DO
C
C     Opening the coordinate datafile.
C
2     XMAX=0.0
      YMAX=0.0
C
      NX=450
      NY=450
      OPEN(UNIT=1,FILE=FCRD,STATUS='OLD')
      DO NCOD=1,1000
      READ(1,*,END=100) NDUM,XC(NCOD),YC(NCOD)
      IF(XC(NCOD).GT.XMAX) XMAX=XC(NCOD)
      IF(YC(NCOD).GT.YMAX) YMAX=YC(NCOD)
      END DO
100   CONTINUE
      DO NND=1,NDUM
      NXC(NND)=20+INT2(XC(NND)/XMAX*NX)
      NYC(NND)=470-INT2(YC(NND)/YMAX*NY)
      END DO
      rewind 1
      do nj=1,ndum
      read(1,'(2x,a3)') node(nj)
      end do
      CLOSE(UNIT=1)
C
C     Opening elemen data file.
C
      OPEN(Unit=2,File=FELE,Status='Old')
      DO NEL=1,1000
      READ(2,*,END=200) NELD
      END DO
200   CONTINUE
C
C
C     Find graphics mode.
C
      IF ( setvideomode( $MAXRESMODE ) .EQ. 0 )
     +	   STOP 'Error:  cannot set graphics mode'
      REWIND 2
C	print *, fname
C
C WRITE TITLE AT BOTTOM
C
	TCRT='TEMPERATURE DISTRIBUTION FIGURE OF FILE :'//sinbad
	XR1=20
	YR1=500
	CALL TEMPTEX(XR1,YR1,TCRT)

	TCRT='AT TIME :'//TIMETX//' MINUTES'
	XR1=20
	YR1=515
	CALL TEMPTEX(XR1,YR1,TCRT)
C
C DRAWING THE TEXT BOX THAT REPRESENTS THE COLOR TO TEMPERATURE
C
	XR1=480
	XR2=500
	YR1=10
	YR2=20
	TCR=850
	CALL LINEDR(XR1,XR2,YR1,YR2,TCR)

	XR1=480
	XR2=500
	YR1=30
	YR2=40
	TCR=700
	CALL LINEDR(XR1,XR2,YR1,YR2,TCR)

	XR1=480
	XR2=500
	YR1=50
	YR2=60
	TCR=600
	CALL LINEDR(XR1,XR2,YR1,YR2,TCR)

	XR1=480
	XR2=500
	YR1=70
	YR2=80
	TCR=500
	CALL LINEDR(XR1,XR2,YR1,YR2,TCR)

	XR1=480
	XR2=500
	YR1=90
	YR2=100
	TCR=400
	CALL LINEDR(XR1,XR2,YR1,YR2,TCR)
C
	XR1=480
	XR2=500
	YR1=110
	YR2=120
	TCR=300
	CALL LINEDR(XR1,XR2,YR1,YR2,TCR)

	XR1=480
	XR2=500
	YR1=130
	YR2=140
	TCR=200
	CALL LINEDR(XR1,XR2,YR1,YR2,TCR)

	XR1=480
	XR2=500
	YR1=150
	YR2=160
	TCR=100
	CALL LINEDR(XR1,XR2,YR1,YR2,TCR)

	XR1=480
	XR2=500
	YR1=170
	YR2=180
	TCR=30
	CALL LINEDR(XR1,XR2,YR1,YR2,TCR)

C
C DRAWING THE TRIANGLES
C
      DO NMSH=1,NELD
      READ(2,*) NDD,N1,N2,N3,NB1,NB2,NB3,M1
      X1=NXC(N1)
      Y1=NYC(N1)
      X2=NXC(N2)
      Y2=NYC(N2)
      X3=NXC(N3)
      Y3=NYC(N3)
C
      X12=INT2((X1+X2)/2.0)
      Y12=INT2((Y1+Y2)/2.0)
      X23=INT2((X3+X2)/2.0)
      Y23=INT2((Y3+Y2)/2.0)
      X13=INT2((X1+X3)/2.0)
      Y13=INT2((Y1+Y3)/2.0)
C
	XCD1=INT2((X1+X12+X13)/3.0)
	YCD1=INT2((Y1+Y12+Y13)/3.0)
	XCD2=INT2((X12+X23+X13)/3.0)
	YCD2=INT2((Y12+Y23+Y13)/3.0)
	XCD3=INT2((X12+X2+X23)/3.0)
	YCD3=INT2((Y12+Y2+Y23)/3.0)
	XCD4=INT2((X13+X23+X3)/3.0)
	YCD4=INT2((Y13+Y23+Y3)/3.0)
C
	T12=(T(N1)+T(N2))/2.0
	T23=(T(N3)+T(N2))/2.0
	T13=(T(N1)+T(N3))/2.0
C
	TC1=(T(N1)+T12+T13)/3.0
	TC2=(T12+T23+T13)/3.0
	TC3=(T12+T(N2)+T23)/3.0
	TC4=(T13+T23+T(N3))/3.0
C
C for part 1
	
C
C     Draw lines.
C
	call lined(x1,x12,x13,y1,y12,y13,tc1,xcd1,ycd1)
	call lined(x12,x23,x13,y12,y23,y13,tc2,xcd2,ycd2)
	call lined(x12,x2,x23,y12,y2,y23,tc3,xcd3,ycd3)
	call lined(x13,x23,x3,y13,y23,y3,tc4,xcd4,ycd4)

      END DO

C
	READ(*,*)
	RETURN
      END
C ************************************************************
C SUBROUTINE DRAW TRIANGLE LINES
C ************************************************************
	subroutine lined(x1,x2,x3,y1,y2,y3,tc,xcd,ycd)
      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
      INTEGER*2   status, x1, y1, x2, y2, x3, y3,xcd,ycd
      INTEGER*4   ncolor
      RECORD /xycoord/	xy
C
C     Draw lines.
C
      if(TC.ge.850.) ncolor=12 ! light red
      if(TC.lt.850.) ncolor=4  ! red
      if(TC.lt.650.) ncolor=5  ! magenta
      if(TC.lt.550.) ncolor=13 ! light magenta
      if(TC.lt.450.) ncolor=14 ! yellow
      if(TC.lt.350.) ncolor=11 ! light cyan
      if(TC.lt.250.) ncolor=3  ! cyan
      if(TC.lt.150.) ncolor=9  ! light blue   
      if(TC.lt.50.) ncolor=1   ! blue
      status = setcolor( ncolor )
      CALL moveto( x1, y1, xy )
      status = lineto(x2,y2)
      CALL moveto( x2, y2, xy )
      status = lineto(x3,y3)		  
      CALL moveto( x3, y3, xy )
      status = lineto(x1,y1)
	status = FLOODFILL( XCD,YCD,NCOLOR )
	return
	end

C ************************************************************
C SUBROUTINE DRAW RECTAGNGLE LINES
C ************************************************************
	subroutine LINEDR(x1,x2,y1,y2,tc)
      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
	CHARACTER TCRT*60
      INTEGER*2   status, x1, y1, x2, y2,XCD,YCD
	INTEGER*2   XT,YT
      INTEGER*4   ncolor
      RECORD /xycoord/	xy
C
C CENTER POINTS
	XCD=INT2(X1+(X2-X1)/2)
	YCD=INT2(Y1+(Y2-Y1)/2)

C
C
C     Draw lines.
C
      if(TC.ge.850.) ncolor=12 ! light red
      if(TC.lt.850.) ncolor=4  ! red
      if(TC.lt.650.) ncolor=5  ! magenta
      if(TC.lt.550.) ncolor=13 ! light magenta
      if(TC.lt.450.) ncolor=14 ! yellow
      if(TC.lt.350.) ncolor=11 ! light cyan
      if(TC.lt.250.) ncolor=3  ! cyan
      if(TC.lt.150.) ncolor=9  ! light blue   
      if(TC.lt.50.) ncolor=1   ! blue
      status = setcolor( ncolor )
      CALL moveto( x1, y1, xy )
      status = lineto(x2,y1)
      CALL moveto( x2, y1, xy )
      status = lineto(x2,y2)		  
      CALL moveto( x2, y2, xy )
      status = lineto(x1,y2)
      CALL moveto( x1, y2, xy )
      status = lineto(x1,y1)
	status = FLOODFILL( XCD,YCD,NCOLOR )
	XT=505
	YT=INT2(Y1+4)
      if(TC.ge.850.) TCRT='>850' ! light red
      if(TC.lt.850.) TCRT='650-850'  ! red
      if(TC.lt.650.) TCRT='550-650'  ! magenta
      if(TC.lt.550.) TCRT='450-550' ! light magenta
      if(TC.lt.450.) TCRT='350-450' ! yellow
      if(TC.lt.350.) TCRT='250-350' ! light cyan
      if(TC.lt.250.) TCRT='150-250'  ! cyan
      if(TC.lt.150.) TCRT='50-150'  ! light blue   
      if(TC.lt.50.)  TCRT='<150'   ! blue
	CALL TEMPTEX(XT,YT,TCRT)
	return
	end



C ************************************************************
C SUBROUTINE TEMPTEX
C ************************************************************
	SUBROUTINE TEMPTEX(XT,YT,TCRT)
      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
      PARAMETER 	   ( NFONTS = 1 )
      character*60 TCRT
      INTEGER*2 	   dummy,iend
      INTEGER*4 	   ifont
	CHARACTER*20   LIST
      CHARACTER*10   options(NFONTS)
	CHARACTER*64	FONTPATH
      INTEGER*2       XT, YT
      RECORD /xycoord/	   xy
      RECORD /fontinfo/    fi
C
      DATA OPTIONS / "t'courier'"/
C
C WRITING THE TEXT
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
	    CALL moveto( XT, YT, XY )
	    dummy = setcolor( 15 )
	    CALL setgtextvector( 0, 0 )
	    CALL outgtext( TCRT )
	 ELSE
	    CALL outtext( 'Error:  cannot set font' )
	 END IF
C
	RETURN
      END
