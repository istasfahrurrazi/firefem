	SUBROUTINE MAX
	DOUBLE PRECISION I1,I2,IT1,IT2,AREA,R1,R2,LE,PY
	DOUBLE PRECISION LAMDA1,LAMDA2,PC,THICK
      CHARACTER*12 NUMC
	CHARACTER*1 CONFIRM
	CHARACTER*12 OUTPUT
	CHARACTER*35 COMMENTS
C
C
C	NAME THE DATA FILE OUTPUT
 1000 CONTINUE
 40	CONTINUE
	PRINT *,'THIS WAS DEVELOPED BY HISHAMUDDIN ALHAM'
	PRINT *,'(ahisham@fkm.utm.my)'
	PRINT *,'--------------------------------------'
	PRINT *,'CALCULATING THE MAXIMUM ALLOWABLE LOAD'
	PRINT *,'--------------------------------------'
	PRINT '(A)',' PRINT IN THE DATA OUTPUT NAME FOR YOUR CASE (ext
     . TRY.MAX):'
      READ (*,'(A)')OUTPUT
	OPEN(UNIT=10,FILE=OUTPUT,STATUS='UNKNOWN')
	OPEN(UNIT=99,FILE='SUMMARY.TXT',STATUS='UNKNOWN')
C
C	PUT IN COMMENTS IN THE DATAFILE
C
	PRINT *,' '
	PRINT '(A)',' PRINT IN COMMENTS FOR YOUR FILE: '
      READ (*,'(A35)')COMMENTS
	WRITE(10,'(A35)')COMMENTS
	PRINT *,'  '
C
C Yield Strength of Steel at room temperature
	PY=300000000.0D0
C Compressive Strength of Concrete at room temperature
	FCU=35000000.0D0
C
C EFFECTIVE LENGTH OF COLUMN
      LE=2.0D0
C
   10 CONTINUE
	PRINT *,'THE VALUES BELOW ARE USED IN THIS PROGRAM'
	PRINT *,'---------------------------------------------- '
	PRINT *,'Yield Strength of Steel at Room Temp  =',PY
	PRINT *,'The Compressive Strength of Conc at RT=',FCU
	PRINT *,'       The Effective Length of Column =',LE
	PRINT *,' '
C
	PRINT *,'PRESS 1 IF YOU AGREE OR 2 TO CHANGE'
	READ *,LI
	IF(LI.EQ.1) THEN
	GO TO 20
	ELSE
	CONTINUE
	ENDIF
	PRINT *,'Yield Strength of Steel at Room Temp'
	READ *,PY
	PRINT *,'The Compressive Strength of Conc at RT'
	READ *,FCU
	PRINT *,'The Effective Length of Column'
	READ *,LE
	GO TO 10				
   20 CONTINUE			 
C
C	CHECK TO SEE WHETHER IT EXCEED CLAUSE 4.14.3(B) BS5950:PART 1: 1990
C
	IF(PY.GT.355000000)THEN
	PRINT *,'EXCEEDED ALLOWED VALUE OF YIELD STRESS-REFER 4.14.3(B)'
	PRINT *,' '
	PRINT *,'PRESS 1 IF YOU WANT TO CONTINUE ANYWAY'
	READ *,IIJ
	IF(IIJ.EQ.1)THEN
	GO TO 781
	ELSE
	PRINT *,'WILL INPUT NEW VALUES'
	write(*,*) 'Press Enter'
	GO TO 10
	ENDIF
	ENDIF
  781	CONTINUE
	IF(FCU.GT.40000000)THEN
	PRINT *,'EXCEEDED ALLOWED VALUE OF CONCRETE STR - REFER 4.14.3(B)'
	PRINT *,' '
	PRINT *,'PRESS 1 IF YOU WANT TO CONTINUE ANYWAY'
  	READ *,IIJ
	IF(IIJ.EQ.1)THEN
	GO TO 17
	ELSE
	PRINT *,'WILL INPUT NEW VALUES'
	write(*,*) 'Press Enter'
	GO TO 10
	ENDIF
	ENDIF
C
C
   17 CONTINUE
C	ENTERING THE DATA FOR COLUMN DIMENSION
C
C	ASKING WHETHER USING REINFORCEMENTS
C
   87 CONTINUE
	PRINT *,'PRESS 1 IF YOU WANT TO INCLUDE REINFORCEMENTS'
	PRINT *,'PRESS OTHERS IF NOT.'
	READ *,II
	IF(II.EQ.1)THEN
	GO TO 19
	ELSE
	GO TO 77
	ENDIF
C
   19 CONTINUE
	PRINT *,'INPUT DIAMETER OF REINFORCEMENTS (IN MM)'
	READ *,DR
	DR=DR/1000.
	PRINT *,'INPUT Y-DISTANCE OF TOP TO CENTRE OF RF'
	READ *,YDR
	YDR=YDR/1000.
	PRINT *,'INPUT X-DISTANCE OF SIDE TO CENTRE OF RF'
	READ *,XDR
	XDR=XDR/1000.
	PRINT *,' '
	PRINT *,'DIAMETER OF REINFORCEMENTS        =',DR
	PRINT *,'Y-DISTANCE OF TOP TO CENTRE OF RF =',YDR
	PRINT *,'X-DISTANCE OF TOP TO CENTRE OF RF =',XDR
	PRINT *,' '
	PRINT *,'PRESS 1 TO REINPUT DATA'
	READ *,II
	IF(II.EQ.1)THEN
	GO TO 19
	ELSE
	GO TO 77
	ENDIF 
C
C
   77 CONTINUE
      PRINT *,'ENTER DATA FILE FOR COLUMN DIMENSION' 
	PRINT *,'ENTER FILE NAME WITH EXTENSION' 
      PRINT *,' ' 
      PRINT *,'EXAMPLE:'
      PRINT *,'--> COLUMN1.DAT'
	PRINT *,' '
	PRINT 79,' OR TYPE ''IN'' TO INPUT OWN VALUES : '
      READ (*,'(A12)')NUMC
	IF(NUMC.EQ.'IN'.OR.NUMC.EQ.'in') THEN
	PRINT *,'ALL DATA INPUT SHOULD BE IN MM'
	PRINT *,'------------------------------'
	PRINT 79,' INPUT FLANGE THICKNESS :'
	READ *,THICK
	THICK=THICK/1000.
	PRINT 79,' INPUT D OR H2M:'
	READ *,H2M
	H2M=H2M/1000.
	PRINT 79,' INPUT B OR H4M: '
	READ *,H4M
	H4M=H4M/1000.
	PRINT 79,' INPUT COVER THICKNESS ALONG Y OR H2A: '
	READ *,H2A
	H2A=H2A/1000.
	PRINT 79,' INPUT COVER THICKNESS ALONG X OR H4A: '
	READ *,H4A
	H4A=H4A/1000.
	PRINT 79,' INPUT VALUE OF WEB OR W: '
	READ *,W
	W=W/1000
	ELSE
      OPEN(UNIT=100,FILE=NUMC,STATUS='OLD')
      READ (100,25)THICK
      READ (100,25)H2M
      READ (100,25)H4M
      READ (100,25)H2A
      READ (100,25)H4A
      READ (100,25)W
   25 FORMAT(10X,D8.5)
      CLOSE(UNIT=100)
	ENDIF
C
C
C
C

   50 CONTINUE
      CVRY=YDR-(0.5*DR)
	CVRX=XDR-(0.5*DR)
	PRINT *,' '
	PRINT *,'THE DATA ENTERED ARE AS FOLLOWS (IN SI)'
	PRINT *,'---------------------------------------'
	PRINT *,'FLANGE THICKNESS=',THICK
	PRINT *,'VALUE OF D OR H2M=',H2M
	PRINT *,'VALUE OF B OR H4M=',H4M
	PRINT *,'VALUE OF COVER THICKNESS ALONG Y OR H2A=',H2A
	PRINT *,'VALUE OF COVER THICKNESS ALONG X OR H4A=',H4A
	PRINT *,'VALUE OF WEB OR W=',W
	PRINT *,'COVER THICKNESS IN Y DRC TO REINFOR.=',CVRY
      PRINT *,'COVER THICKNESS IN X DRC TO REINFOR.=',CVRX
C
	PRINT *,' '
	PRINT *,'TO CHANGE ANYTHING - PRESS Y'
	PRINT *,'TO CONTINUE - PRESS ANY OTHER CHARACTER'
	READ (*,'(A1)')CONFIRM
	IF(CONFIRM.EQ.'Y'.OR.CONFIRM.EQ.'y') THEN
	GO TO 40
	ELSE
	ENDIF
C
   60 CONTINUE
C
	WRITE(10,70)H4M,H2M,THICK,H4A,H2A,W,CVRY,CVRX,PY,FCU,LE
   70 FORMAT(24X,        'ALL USE SI UNITS',/
     .,32X,                      'B OR H4M =',F12.6/
     .,32X,                            'D OR H2M =',F12.6/
     .,24X,                    'FLANGE THICKNESS =',F12.6/
     .,3X,'COVER THICKNESS IN X DIRECTION OR H4A =',F12.6/
     .,3X,'COVER THICKNESS IN Y DIRECTION OR H2A =',F12.6/
     .,23X,                   'WIDTH OF WEB OR W =',F12.6/
     .,5X,  'COVER THICKNESS IN Y DRC TO REINFOR.=',F12.6/
     .,5X,  'COVER THICKNESS IN X DRC TO REINFOR.=',F12.6/
     .,3X,'Yield Strength of Steel at Room Temp  =',E9.3/
     .,3X,'The Compressive Strength of Conc at RT=',E9.3/
     .,10X,      'The Effective Length of Column =',F12.6)
C
	B1=H4M
	D1=H2M
	TX=H4A-0.075
	TY=H2A-0.075
	IF(TX.GE.0.0)THEN
	H4A=0.075
	ELSE
	CONTINUE
	ENDIF
	IF(TY.GE.0.0)THEN
	H2A=0.075
	ELSE
	CONTINUE
	ENDIF	
	BC=H4M+2*H4A
	DC=H2M+2*H2A
C
	I1=(THICK*H4M**3)/12.
	I2=((H2M-(2*THICK))*W**3)/12.
	IT1=2*I1+I2
	IT2=((H4M*H2M**3)/12)-((H4M-W)*(H2M-(2*THICK))**3)/12.
	AREA=(H4M*THICK*2)+(H2M-(2*THICK))*W+(3.14*DR**2)
	AG=AREA
	AC=(BC*DC)-AG
	R1=SQRT(IT1/AREA)
	R2=SQRT(IT2/AREA)
	LAMDA1=LE/R1
	LAMDA2=LE/R2
	RMIN=R1-R2
	IF(RMIN.LT.0.0D0) THEN
	RMIN=R1
	ELSE
	RMIN=R2
	ENDIF
	IF(RMIN.EQ.R2) THEN
	PRINT *,'COLUMN BENDS ABOUT X-AXIS - NOT PERMISSIBLE'
	write(*,*) 'Press Enter'
	GO TO 17
	ELSE
	CONTINUE
	ENDIF
C
	WRITE(10,80)AG,AC,IT1,IT2,R1,R2,LAMDA1,LAMDA2
	WRITE(6,80)AG,AC,IT1,IT2,R1,R2,LAMDA1,LAMDA2
   80 FORMAT(/,30X,             'AREA OF STEEL SECTION, AG =',F12.6/
     .27X,                   'AREA OF CONCRETE SECTION, AC =',F12.6/
     .12X,    'SECOND MOMENT OF AREA OF STEEL ABOUT Y-AXIS =',2X,F14.10/
     .12X,    'SECOND MOMENT OF AREA OF STEEL ABOUT X-AXIS =',2X,F14.10/
     .15X,       'RADIUS OF GYRATION OF STEEL ABOUT Y-AXIS =',F12.6/
     .15X,       'RADIUS OF GYRATION OF STEEL ABOUT X-AXIS =',F12.6/
     .16X,        'SLENDERNESS RATIO OF STEEL ABOUT Y-AXIS =',F12.6/
     .16X,        'SLENDERNESS RATIO OF STEEL ABOUT X-AXIS =',F12.6)
C
	   XLA=40*BC
	   XLB=(100*BC**2)/DC
 	   XLC=250*RMIN
	   WRITE(10,120)XLA,XLB,XLC
      WRITE(6,120)XLA,XLB,XLC
  120 FORMAT(13X,   '40BC =',F12.6,
     .5X,     '100*BC**2/DC =',F12.6,    
     .9X,         '250*RMIN =',F12.6)
	   WRITE(10,*) LE
	   WRITE(6,*) LE

c130   FORMAT(16X,'THE VALUE OF EFFECTIVE LENGTH',8X,'SHOULD BE LIMITED TO THE LEAST OF THE',27X,
c     .'THREE VALUES ABOVE',3X,'THE VALUE OF EFFECTIVE LENGTH USED =',F5.1,3X,
c     .'THIS SATISFIES CLAUSE 4.14.1(i)  BS5950:PART1:1990')

	   write(*,*) 'Press Enter'
C
	Z1=0.2*BC
	Z2=0.2*(H4M+0.15)
	AZ=Z1-Z2
	IF(AZ.LT.0.0D0)THEN
	ROVY=Z1
	ELSE
	ROVY=Z2
	ENDIF
C	
C	ROVY=RADIUS OF GYRATION OF THE ENCASED COLUMN
C
	WRITE(10,210)Z1,Z2,ROVY
	WRITE(6,210)Z1,Z2,ROVY
  210 FORMAT(//,11X,'RADIUS OF GYRATION OF SECTION TAKEN AS 0.2*BC',/,
     .24X,                       'BUT NOT MORE THAN 0.2*(H4M+0.15)',//,
     .40X,                                         '0.2*BC =',F12.6,/,
     .33X,                                  '0.2*(H4M+0.15)=',F12.6,//,
     .6X,        'THEREFORE VALUE OF RADIUS OF GYRATION IS =',F12.6,/,
     .8X,        'TO SATISFY BS5950:PART 1: 1990 - 4.14.3(a)')
C
	WRITE(10,65)LAMDA1
	WRITE(6,65)LAMDA1
   65 FORMAT(/,39X,'LAMDA-Y =',F12.6)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC											CC
CC	CALLING SUBROUTINE TO GET VALUE OF PC	CC
CC											CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL PC1(LAMDA1,PC,PY,THICK)
C     **********
C
      WRITE(10,75)PC
	WRITE(6,75)PC
   75 FORMAT(21X,'VALUE OF PC (IN N/MM2) FROM TABLE IS =',F18.6)
	PC=PC*1000000.
C
	PPC=PC*(AG+(0.45*FCU*AC)/PY)
	PPCS=PY*(AG+(0.25*FCU*AC)/PY)
	PZ=PPC-PPCS
	IF(PZ.LT.0.0D0)THEN
	PZ=PPC
	ELSE
	PZ=PPCS
	ENDIF
	SMAX1=PY*(AG+(FCU*AC)/PY)
	SMAX2=PC*(AG+(FCU*AC)/PY)
	SMZ=SMAX1-SMAX2
	IF(SMZ.LT.0.0D0)THEN
	SMZ=SMAX1
	ELSE
	SMZ=SMAX2
	ENDIF
	WRITE(10,98)PPC,PPCS,PZ,SMAX1,SMAX2,SMZ
	WRITE(6,98)PPC,PPCS,PZ,SMAX1,SMAX2,SMZ
   98	FORMAT(//,5X,'ACCORDING TO CLAUSE 4.14.3(B) BS5950:PART:1990',/,
     .5X,    'THE COMPRESSIVE RESISTANCE, PC, OF THE CASED SECTION',/,
     .35X,    'IS GIVEN BY =',F18.6,//,
     .5X,    'BUT NOT GREATER THAN THE SHORT STRUT',/,
     .38X,    'GIVEN BY =',F18.6,//,
     .20X,    'THE MAXIMUM ALLOWABLE LOAD =',F18.6,//
     .5X,    'MAXIMUM LOAD1 AT SAFETY FACTOR EQUALS ZERO=',F18.6,/,
     .5X,    'MAXIMUM LOAD2 AT SAFETY FACTOR EQUALS ZERO=',F18.6,//,
     .4X,   'SO THE SELECTED MAX LOAD1 AT SF EQUALS ZERO=',F18.6,/) 
	WRITE(99,'(A35)')COMMENTS
	WRITE(99,78)PZ,SMZ,R1,R2
   78 FORMAT(3X,'          MAX. ALLOWABLE LAOD ='1X,F18.6,/,5X,
     .             'MAX. LOAD AT SF EQUALS ZERO =',1X,F18.6,/,1X,
     .         'RADIUS OF GYRATION ABOUT Y-AXIS =',1X,F18.6,/,1X,
     .         'RADIUS OF GYRATION ABOUT X-AXIS =',1X,F18.6,//)
	PRINT *,' '
	PRINT *,'PRESS 1 IF YOU WANT TO REPEAT THE ANALYSIS'
	READ *,II
	IF(II.EQ.1)THEN
	GO TO 1000
	ELSE
	CONTINUE
	ENDIF
79	FORMAT(A, /)
 2000 CONTINUE
	RETURN
	END
C     ***************************************
C     ********* SUBROUTINES STARTS***********      
C     ***************************************
	SUBROUTINE PC1(LAMDA,PC,PY,THICK)
C	******************
C
	DOUBLE PRECISION PI,E,LAMDA,PY,NU,PC,PE,THICK
      IF(THICK.LE.40.00D0) THEN
	A=5.5
	ELSE
	A=8.0
	ENDIF
	PI= 3.14159265359
	E=210.0D09
	LAMDA0=0.2*((((PI**2)*E)/PY)**0.5)
	NU=0.001*A*(LAMDA-LAMDA0)
      PE=PI**2*E/(LAMDA**2)
	PHI=(PY+((NU+1)*PE))/2.0D0
	PC=PE*PY/((PHI+((PHI**2)-PE*PY)**0.5))
	PC=PC/(1.0D06)
	PC=PC+0.5
	RETURN
	END
