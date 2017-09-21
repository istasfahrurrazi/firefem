      subroutine MESHV
c FILENAME AUTOMESH.FOR  for meshing subroutine         
c This program follows the method of GRID explained in Segerlind.
c Designed by Istaz'97::::::::::>
      INTEGER RS,CL,D
      PARAMETER (RS=700,CL=700,D=700)
      DIMENSION NDN(D),JT(RS,4),NN(RS,CL),NNRB(RS,4,CL),LB(3),NE(D)
     .,NR(4),ICOMP(4,4)
      DOUBLE PRECISION XP(D),YP(D),XRG(D),YRG(D),N(D)
      DOUBLE PRECISION YC(RS,CL),XC(RS,CL),TR,DETA,DSI,ETA,SI
      DOUBLE PRECISION XE(D),YE(D),XCC(D),YCC(D),DIAG1,DIAG2     
      CHARACTER TITLE*30,FNAME*13,ELE*4,CRD*4,BLANK*1,ELEDAT*13
     .,COORDAT*13,ANS*4,ANSW*13,MATL*2,NIBM*1,DIM*13,EDIM*4,YN*1,cn*1
      INTEGER LENGTH,FIRST
      PARAMETER (LENGTH=8,BLANK='.')
      COMMON /DIME/ DIM
      DATA ICOMP/-1,1,1,-1,1,-1,-1,1,1,-1,-1,1,-1,1,-1,1/
      DATA NBW/0/,NB/0/,NEL/0/        
C     DATA IN/60/,IO/61/,IP/62/,NBW/0/,NB/0/,NEL/0/
      FIRST=1      
109   PRINT * ,'******************************************************'
      PRINT * ,'Select type of CROSS SECTION desired :'
      PRINT * ,'******************************************************'
      PRINT * ,' 1  - Rectangular Concrete Reinforced With I-Beam and',
     .         ' Reinforcement'
      PRINT * ,' 2  - I-Beam Coated By Ceramic'
      PRINT * ,' 3  - I-Beam Without Coating'
      PRINT * ,' 4  - Hollow Rectangular Steel filled with concrete'
      PRINT * ,' 5  - Rectangular Ibeam with Concrete filled '
      PRINT * ,' 6  - Rectangular Concrete'
	PRINT * ,' 7  - Cylindrical Steel Column Filled With Concrete'
	PRINT * ,' 8  - Concrete Walls'
	PRINT * ,' 9  - H-Section full'
	PRINT * ,' 10 - Rectangular/Square Concrete full'
      PRINT * ,' 11 - I-Beam Coated By Concrete'
	PRINT * ,' 12 - View Mesh from Existing Element Datafile'
	PRINT * ,' Q  - Main Menu'
      PRINT * ,'******************************************************'
      READ(*,'(A2)') MATL
	print *,'1.'
c	      
      IF(MATL.EQ.'Q'.OR.MATL.EQ.'q') GOTO 999
      IF(MATL.EQ.'1') THEN
      GOTO 1
      ELSEIF(MATL.EQ.'2') THEN
      GOTO 1
      ELSEIF(MATL.EQ.'3') THEN
      GOTO 1 
      ELSEIF(MATL.EQ.'4') THEN
      GOTO 1
      ELSEIF(MATL.EQ.'5') THEN
      GOTO 1
      ELSEIF(MATL.EQ.'6') THEN
	GOTO 1
      ELSEIF(MATL.EQ.'7') THEN
      GOTO 1
      ELSEIF(MATL.EQ.'8') THEN
      GOTO 1
      ELSEIF(MATL.EQ.'9') THEN
      GOTO 1
      ELSEIF(MATL.EQ.'10') THEN
      GOTO 1
      ELSEIF(MATL.EQ.'11') THEN
      GOTO 1
      ELSEIF(MATL.EQ.'11') THEN
      GOTO 1
      ELSEIF(MATL.EQ.'12') THEN
      GOTO 1
	ELSE
      GOTO 109 
      ENDIF
1     CONTINUE      
	CLOSE(UNIT=1)
	CLOSE(UNIT=2)
	CLOSE(UNIT=3)
	CLOSE(UNIT=4)
	NBW=0
	NB=0
	NEL=0
C *********************************************
C      
C            MATL = 9 , H-SECTION FULL
C
C**********************************************
      IF(MATL.EQ.'9') THEN
C Entering the name of the file
      PRINT * ,'******************************************************'
      PRINT * ,'H-SECTION FULL'
	PRINT * ,'Enter the name of the newfile (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      EDIM='.DIM'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    DIM=FNAME(FIRST:MNEXT)//EDIM
	ENDIF
      END DO 
      PRINT *,'I-Beam Without Coating'
      PRINT *,'----------------------'
      PRINT *,'Do you want to use the available dimension? (y/n)'
      READ(*,'(A1)') NIBM
      IF(NIBM.EQ.'Y'.OR.NIBM.EQ.'y') THEN
      PRINT *,'Enter the desired cross section :-'
      PRINT *,'(please refer to my thesis for clarification)'
      PRINT *,'1- W250-149'
      PRINT *,'2- W250-101'
      PRINT *,'3- W250-89'
      PRINT *,'4- W250-80'
      PRINT *,'5- W250-73'
      PRINT *,'6- W250-49'
      PRINT *,'_________________________________________________'
      READ(*,'(A1)') NIBM
      IF(NIBM.EQ.'1') THEN
      DEPTH=282.0E-3
      WIDTH=263.0E-3
      THF=28.4D-3
      THW=17.3D-3
      ELSEIF(NIBM.EQ.'2') THEN
      DEPTH=264.0E-3
      WIDTH=257.0E-3
      THF=19.6D-3
      THW=11.9D-3
      ELSEIF(NIBM.EQ.'3') THEN
      DEPTH=260.0E-3
      WIDTH=256.0E-3
      THF=17.3D-3
      THW=10.7D-3
      ELSEIF(NIBM.EQ.'4') THEN
      DEPTH=256.0E-3
      WIDTH=255.0E-3
      THF=15.6D-3
      THW=9.4D-3
      ELSEIF(NIBM.EQ.'5') THEN
      DEPTH=253.0E-3
      WIDTH=254.0E-3
      THF=14.2D-3
      THW=8.6D-3
      ELSEIF(NIBM.EQ.'6') THEN
      DEPTH=247.0E-3
      WIDTH=202.0E-3
      THF=11.0D-3
      THW=7.40D-3
      ENDIF
      ELSE
      PRINT *,'Enter Depth (m):'
      READ(*,*) DEPTH
      PRINT *,'Enter Width (m):'
      READ(*,*) WIDTH
      PRINT *,'Enter Flange dimensions,THF (m):'
      READ(*,*) THF
      PRINT *,'Enter Web dimension,THW (m):'
      READ(*,*) THW
      ENDIF
      open(unit=8,file=dim,status='unknown')
      write(8,*) 'Ibeam only'
      write(8,*) 'Flange,m :',thf
      write(8,*) 'Depth,m  :',depth
      write(8,*) 'Width,m  :',width
      write(8,*) 'Web,m    :',thw
      close(unit=8)
	print *, '1x'
      CALL M9(fname,thf,thw,depth,width)
      x1m9=0.0
      x2m9=((width/2.0)-(thw/2.0))/2.0
      x3m9=x2m9+thw/2.0
      x4m9=width
      y1m9=0.0
      y2m9=thf
      y3m9=depth-thf
      y4m9=depth


C *********************************************
C      
C            MATL = 3 , I-BEAM WITHOUT COATING
C
C *********************************************
      ELSEIF(MATL.EQ.'3') THEN
C Entering the name of the file
      PRINT * ,'******************************************************'
	PRINT * ,'I-BEAM WITHOUT COATING'
      PRINT * ,'Enter the name of the newfile (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      EDIM='.DIM'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    DIM=FNAME(FIRST:MNEXT)//EDIM
	ENDIF
      END DO 
      PRINT *,'I-Beam Without Coating'
      PRINT *,'----------------------'
      PRINT *,'Do you want to use the available dimension? (y/n)'
      READ(*,'(A1)') NIBM
      IF(NIBM.EQ.'Y'.OR.NIBM.EQ.'y') THEN
      PRINT *,'Enter the desired cross section :-'
      PRINT *,'(please refer to my thesis for clarification)'
      PRINT *,'1- W250-149'
      PRINT *,'2- W250-101'
      PRINT *,'3- W250-89'
      PRINT *,'4- W250-80'
      PRINT *,'5- W250-73'
      PRINT *,'6- W250-49'
      PRINT *,'_________________________________________________'
      READ(*,'(A1)') NIBM
      IF(NIBM.EQ.'1') THEN
      DEPTH=282.0E-3
      WIDTH=263.0E-3
      THF=28.4D-3
      THW=17.3D-3
      ELSEIF(NIBM.EQ.'2') THEN
      DEPTH=264.0E-3
      WIDTH=257.0E-3
      THF=19.6D-3
      THW=11.9D-3
      ELSEIF(NIBM.EQ.'3') THEN
      DEPTH=260.0E-3
      WIDTH=256.0E-3
      THF=17.3D-3
      THW=10.7D-3
      ELSEIF(NIBM.EQ.'4') THEN
      DEPTH=256.0E-3
      WIDTH=255.0E-3
      THF=15.6D-3
      THW=9.4D-3
      ELSEIF(NIBM.EQ.'5') THEN
      DEPTH=253.0E-3
      WIDTH=254.0E-3
      THF=14.2D-3
      THW=8.6D-3
      ELSEIF(NIBM.EQ.'6') THEN
      DEPTH=247.0E-3
      WIDTH=202.0E-3
      THF=11.0D-3
      THW=7.40D-3
      ENDIF
      ELSE
      PRINT *,'Enter Depth (m):'
      READ(*,*) DEPTH
      PRINT *,'Enter Width (m):'
      READ(*,*) WIDTH
      PRINT *,'Enter Flange dimensions,THF (m):'
      READ(*,*) THF
      PRINT *,'Enter Web dimension,THW (m):'
      READ(*,*) THW
      ENDIF
      open(unit=8,file=dim,status='unknown')
      write(8,*) 'Ibeam only'
      write(8,*) 'Flange,m :',thf
      write(8,*) 'Depth,m  :',depth
      write(8,*) 'Width,m  :',width
      write(8,*) 'Web,m    :',thw
      close(unit=8)
      CALL M3(fname,thf,thw,depth,width)
      x1=thw/2.0d0
      x2=width/2.0d0
      y1=depth/2.0d0-thf
      y2=depth/2.0d0
C *********************************************
C      
C            MATL = 8 
C
C *********************************************
C
      ELSEIF(MATL.EQ.'8') THEN
      PRINT * ,'******************************************************'
	PRINT * ,'CONCRETE WALLS'
      PRINT * ,'Enter the name of the newfile (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      EDIM='.DIM'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    DIM=FNAME(FIRST:MNEXT)//EDIM
	ENDIF
      END DO
      PRINT *,'Concrete Walls'
      CALL M8(FNAME,h2m,h4m,dim)
C
C FOR CROSS SECTION NUMBER 1
C
      ELSEIF(MATL.EQ.'1') THEN
      PRINT * ,'******************************************************'
	PRINT * ,'H-SECTION ENCASED IN CONCRETE'
      PRINT * ,'Enter the name of the newfile (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      EDIM='.DIM'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    DIM=FNAME(FIRST:MNEXT)//EDIM
	ENDIF
      END DO
      PRINT *,'Concrete Reinforced With I-Beam and Reinforcement'
      CALL M1(FNAME,dim)
c     
C *********************************************
C      
C            MATL = 11
C
C**********************************************
      ELSEIF(MATL.EQ.'11') THEN
      PRINT * ,'******************************************************'
	PRINT * ,'I-BEAM COATED	WITH CONCRETE'
      PRINT * ,'Enter the name of the newfile (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      EDIM='.DIM'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    DIM=FNAME(FIRST:MNEXT)//EDIM
	ENDIF
      END DO
      PRINT *,'I-Beam Coated By Concrete'
      CALL M11(FNAME,dim)
C *********************************************
C      
C            MATL = 2, 
C
C**********************************************
      ELSEIF(MATL.EQ.'2') THEN
      PRINT * ,'******************************************************'
	PRINT * ,'I-BEAM COATED	WITH CERAMIC'
      PRINT * ,'Enter the name of the newfile (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      EDIM='.DIM'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    DIM=FNAME(FIRST:MNEXT)//EDIM
	ENDIF
      END DO
      PRINT *,'I-Beam Coated By Ceramic'
      CALL M2(FNAME,dim)
C *********************************************
C      
C            MATL = 4, 
C
C**********************************************
c     
      ELSEIF(MATL.EQ.'4') THEN
      PRINT * ,'******************************************************'
	PRINT * ,'HOLLOW RECTANGULAR FILLED WITH CONCRETE'
      PRINT * ,'Enter the name of the newfile (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      EDIM='.DIM'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    DIM=FNAME(FIRST:MNEXT)//EDIM
	ENDIF
      END DO
      PRINT *,'Concrete Filled Rectangular Steel'
      CALL M4(FNAME,dim)
c      
      ELSEIF(MATL.EQ.'5') THEN
      PRINT * ,'******************************************************'
	PRINT * ,'RECTANGULAR I-BEAM WITH CONCRETE FILLED'
      PRINT * ,'Enter the name of the newfile (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      EDIM='.DIM'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    DIM=FNAME(FIRST:MNEXT)//EDIM
	ENDIF
      END DO
      PRINT *,'Ibeam + concrete'
      CALL M5(FNAME,thick,h2m,h4m,h2a,h4a,w)
c
      ELSEIF(MATL.EQ.'6') THEN
      PRINT * ,'******************************************************'
	PRINT * ,'RECTANGULAR/SQUARE CONCRETE'
      PRINT * ,'Enter the name of the newfile (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      EDIM='.DIM'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    DIM=FNAME(FIRST:MNEXT)//EDIM
	ENDIF
      END DO
      PRINT *,'Concrete'
      CALL M6(FNAME,h2m,h4m,dim)
c
c Meshing column 10
c
      ELSEIF(MATL.EQ.'10') THEN

      PRINT * ,'******************************************************'
	PRINT * ,'FULL RECTANGULAR/SQUARE CONCRETE'
      PRINT * ,'Enter the name of the newfile (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      EDIM='.DIM'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    DIM=FNAME(FIRST:MNEXT)//EDIM
	ENDIF
      END DO
      PRINT *,'Concrete'
      CALL M10(FNAME,h2m,h4m,dim)
c
c View the existing datafile
c
      ELSEIF(MATL.EQ.'12') THEN

      PRINT * ,'******************************************************'
	PRINT * ,'View the existing datafile'
      PRINT * ,'Enter the name of the file (with extension .DAT): '
      PRINT * ,'such as BEAM.DAT'
      PRINT * ,'******************************************************'
      READ(*,'(A12)') FNAME
      ELE='.ELE'
      CRD='.CRD'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    ELEDAT=FNAME(FIRST:MNEXT)//ELE
	    COORDAT=FNAME(FIRST:MNEXT)//CRD
	ENDIF
      END DO
C	CALL VIEW(ELEDAT,COORDAT)
	GOTO 109
c
c Cylindrical Mesh
c
	ELSEIF(MATL.EQ.'7') THEN
	CALL CYLSTEEL(ELEDAT,COORDAT)
	GOTO 110
c
c Else if nothing is selected go back to top
c
      ELSE
      GOTO 109
      ENDIF
c     
      ELE='.ELE'
      CRD='.CRD'
      ANS='.ANS'
      DO NEXT=1,13
	IF(FNAME(NEXT:NEXT).EQ.BLANK) THEN
	    MNEXT=NEXT-1
	    ELEDAT=FNAME(FIRST:MNEXT)//ELE
	    COORDAT=FNAME(FIRST:MNEXT)//CRD
	    ANSW=FNAME(FIRST:MNEXT)//ANS
	    GO TO 2
	ENDIF
      END DO
c      
2     PRINT *, '_______________________________________'
      PRINT *, 'Datafile name      :',FNAME
      PRINT *, 'Element datafile   :',ELEDAT
      PRINT *, 'Coordinate Datafile:',COORDAT
      PRINT *, 'Answer file        :',ANSW
      PRINT *, 'Dimension file     :',DIM
      PRINT *, '_______________________________________'
c ***OPENING THE ANSWER FILE ******************      
      OPEN(UNIT=2,FILE=ANSW,STATUS='UNKNOWN')
      OPEN(UNIT=3,FILE=ELEDAT,STATUS='UNKNOWN')
      OPEN(UNIT=4,FILE=COORDAT,STATUS='UNKNOWN')
c *********************************************
      OPEN(UNIT=1,FILE=FNAME,STATUS='OLD')
c      PRINT *,'3X'
      READ(1,*) INRG,INBP,IPCH,ICROSS
c     
      IF(MATL.EQ.'1') THEN
      READ(1,*) X1
      READ(1,*) X2
      READ(1,*) Y1
      READ(1,*) Y2
      ENDIF
c      
      IF(MATL.EQ.'11') THEN
      READ(1,*) X1
      READ(1,*) X2
      READ(1,*) X3
      READ(1,*) X4
      READ(1,*) Y1
      READ(1,*) Y2
      READ(1,*) Y3
      READ(1,*) Y4
      ENDIF
c      
      IF(MATL.EQ.'2') THEN
      READ(1,*) X1
      READ(1,*) X2
      READ(1,*) X3
      READ(1,*) X4
      READ(1,*) Y1
      READ(1,*) Y2
      READ(1,*) Y3
      READ(1,*) Y4
      ENDIF
c
      IF(MATL.EQ.'4') THEN
      READ(1,*) X1
      READ(1,*) X2
      READ(1,*) Y1
      READ(1,*) Y2
      ENDIF
c
c Determining the fixed-temperature nodes
c
c      PRINT *,'4X'
      DO I1=1,INBP   
      READ(1,*) XP(I1),YP(I1)
      IF(XP(I1).GT.XMAX) XMAX=XP(I1)
      IF(YP(I1).GT.YMAX) YMAX=YP(I1)
      END DO
c      print *,'1'
c      
      DO I2=1,INRG
      READ(1,*) NRG,(JT(NRG,J),J=1,4)
      END DO
c      print *,'2'
      TITLE='ELEMENT GENERATION PROCEDURE'
      WRITE(2,201) TITLE
  201 FORMAT(//,1X,A40/1X,'GLOBAL COORDINATES  ',/1X,'NUMBER
     1 X COORD	   Y COORD  ')
      WRITE(2,202) (I,XP(I),YP(I),I=1,INBP)
  202 FORMAT(2X,I3,7X,F7.2,5X,F7.2)
      WRITE(2,203)
  203 FORMAT(//1X,17HCONNECTIVITY DATA/1X,'REGION    SIDE   1	   2
     1	    3	   4  ')
      DO 26 I=1,INRG
   26 WRITE(2,204) I,(JT(I,J),J=1,4)
  204 FORMAT(2X,I3,14X,4(I2,5X))      
      NX=0
c
      DO 16 KK=1,INRG
      READ(1,*) NRG,NROWS,NCOL,(NDN(I),I=1,8)
      WRITE(2,205) NRG,NROWS,NCOL,(NDN(I),I=1,8)
  205 FORMAT(1H1//1X,12H***  REGION ,I2,6H  ****/10X,I2,5H ROWS,10X,I2
     1,8H COLUMNS/10X,21HBOUNDARY NODE NUMBERS,10X,8I5)
c     
      DO 5 I=1,8
      II=NDN(I)
      XRG(I)=XP(II)
    5 YRG(I)=YP(II)
c      print *,'3XX'
c    
      XRG(9)=XRG(1)
      YRG(9)=YRG(1)
c      
      TR=DFLOAT(NROWS-1)
      DETA=2.D0/TR
c      print *,'4'
      TR=DFLOAT(NCOL-1)
      DSI=2.D0/TR
      DO 12 I=1,NROWS
      TR=DFLOAT(I-1)
      AA=TR
      ETA=1.D0-TR*DETA
      DO 12 J=1,NCOL
      TR=DFLOAT(J-1)
      BB=TR
      SI=-1.D0+TR*DSI 
c      print *,'5'
      N(1)=-0.25D0*(1.D0-SI)*(1.D0-ETA)*(SI+ETA+1.D0)
      N(2)=0.50D0*(1.D0-SI**2)*(1.D0-ETA)
      N(3)=0.25D0*(1.D0+SI)*(1.D0-ETA)*(SI-ETA-1.D0)
      N(4)=0.50D0*(1.D0+SI)*(1.D0-ETA**2)
      N(5)=0.25D0*(1.D0+SI)*(1.D0+ETA)*(SI+ETA-1.D0)
      N(6)=0.50D0*(1.D0-SI**2)*(1.D0+ETA)
      N(7)=0.25D0*(1.D0-SI)*(1.D0+ETA)*(ETA-SI-1.D0)
      N(8)=0.50D0*(1.D0-SI)*(1.D0-ETA**2)
      XC(I,J)=0.0D0
      YC(I,J)=0.0D0
      DO 12 K=1,8
      XC(I,J)=XC(I,J)+XRG(K)*N(K)
   12 YC(I,J)=YC(I,J)+YRG(K)*N(K)
c      print *,'6XX'
c   
      KN1=1
      KS1=1
      KN2=NROWS
      KS2=NCOL
      DO 50 I=1,4
      NRT=JT(NRG,I)
      IF(NRT.EQ.0.OR.NRT.GT.NRG) GO TO 50
      DO 56 J=1,4
   56 IF(JT(NRT,J).EQ.NRG) NRTS=J
      K=NCOL
      IF(I.EQ.2.OR.I.EQ.4) K=NROWS
      JL=1
      JK=ICOMP(I,NRTS)
      IF(JK.EQ.-1) JL=K
      DO 44 J=1,K
      GO TO (45,46,47,48),I
   45 NN(NROWS,J)=NNRB(NRT,NRTS,JL)
      KN2=NROWS-1
      GOTO 44
   46 NN(J,NCOL)=NNRB(NRT,NRTS,JL)
      KS2=NCOL-1
      GOTO 44
   47 NN(1,J)=NNRB(NRT,NRTS,JL)
      KN1=2
      GOTO 44
   48 NN(J,1)=NNRB(NRT,NRTS,JL)
      KS1=2
   44 JL=JL+JK
   50 CONTINUE
c      print *,'7XX'
      IF(KN1.GT.KN2) GOTO 105
      IF(KS1.GT.KS2) GOTO 105
      DO 10 I=KN1,KN2
      DO 10 J=KS1,KS2
      NB=NB+1
   10 NN(I,J)=NB
      DO 42 I=1,NCOL
      NNRB(NRG,1,I)=NN(NROWS,I)
   42 NNRB(NRG,3,I)=NN(1,I)
      DO 43 I=1,NROWS
      NNRB(NRG,2,I)=NN(I,NCOL)
   43 NNRB(NRG,4,I)=NN(I,1)
      WRITE(2,206)
  206 FORMAT(//1X,19HREGION NODE NUMBERS/)
      DO 52 I=1,NROWS
   52 WRITE(2,207) (NN(I,J),J=1,NCOL)
  207 FORMAT(1X,20I5)
  105 WRITE(2,208)
  208 FORMAT(//3X,17HNEL  NODE NUMBERS,9X,4HX(1),8X,4HY(1),8X,4HX(2),8X,
     14HY(2),8X,4HX(3),8X,4HY(3)   )
      K=1
c      print *,'8'
      DO 54 I=1,NROWS
      DO 54 J=1,NCOL
      XE(K)=XC(I,J)
      YE(K)=YC(I,J)
      NE(K)=NN(I,J)
   54 K=K+1
      L=NROWS-1
      DO 15 I=1,L
      DO 15 J=2,NCOL
      AXX=(XC(I,J)-XC(I+1,J-1))**2+(YC(I,J)-YC(I+1,J-1))**2
      BXX=(XC(I+1,J)-XC(I,J-1))**2+(YC(I+1,J)-YC(I,J-1))**2
C
      IF(AXX.EQ.0.0.OR.BXX.EQ.0.0) THEN
      PRINT *,'AXX or BXX is zero, program stop'
      STOP
      ENDIF
c      print *,'9x'
C
      DIAG1=DSQRT((XC(I,J)-XC(I+1,J-1))**2+(YC(I,J)-YC(I+1,J-1))**2)
      DIAG2=DSQRT((XC(I+1,J)-XC(I,J-1))**2+(YC(I+1,J)-YC(I,J-1))**2)
C      
      NR(1)=NCOL*I+J-1
      NR(2)=NCOL*I+J
      NR(3)=NCOL*(I-1)+J
      NR(4)=NCOL*(I-1)+J-1
      DO 15 IJ=1,2
      NEL=NEL+1
      IF((DIAG1/DIAG2).GT.1.02D0) GO TO 41
      J1=NR(1)
      J2=NR(IJ+1)
      J3=NR(IJ+2)
      GOTO 40
   41 J1=NR(IJ)
      J2=NR(IJ+1)
      J3=NR(4)
   40 LB(1)=IABS(NE(J1)-NE(J2))+1
      LB(2)=IABS(NE(J2)-NE(J3))+1
      LB(3)=IABS(NE(J1)-NE(J3))+1
      DO 107 IK=1,3
      IF(LB(IK).LE.NBW) GOTO 107
      NBW=LB(IK)
      NELBW=NEL
  107 CONTINUE
      WRITE(2,209) NEL,NE(J1),NE(J2),NE(J3),XE(J1),YE(J1),XE(J2),YE(J2),
     1             XE(J3),YE(J3)
      NBB1=0
      NBB2=0
      NBB3=0   
c      
c Determining the location of the boundary for M1 cross-section
c
      IF(MATL.EQ.'1') THEN
      XBC1=XMAX-1.E-6
      YBC1=YMAX-1.E-6
      IF(XE(J1).GT.XBC1.AND.XE(J2).GT.XBC1) NBB1=1
      IF(YE(J1).GT.YBC1.AND.YE(J2).GT.YBC1) NBB1=1
      IF(XE(J2).GT.XBC1.AND.XE(J3).GT.XBC1) NBB2=1
      IF(YE(J2).GT.YBC1.AND.YE(J3).GT.YBC1) NBB2=1
      IF(XE(J3).GT.XBC1.AND.XE(J1).GT.XBC1) NBB3=1
      IF(YE(J3).GT.YBC1.AND.YE(J1).GT.YBC1) NBB3=1
      ENDIF
c      
      IF(MATL.EQ.'5') THEN
      xx=h4m/2.0+h4a
      yy=h2m/2.0+h2a
      XBC1=xx-1.E-6
      YBC1=yy-1.E-6
      IF(XE(J1).GT.XBC1.AND.XE(J2).GT.XBC1) NBB1=1
      IF(YE(J1).GT.YBC1.AND.YE(J2).GT.YBC1) NBB1=1
      IF(XE(J2).GT.XBC1.AND.XE(J3).GT.XBC1) NBB2=1
      IF(YE(J2).GT.YBC1.AND.YE(J3).GT.YBC1) NBB2=1
      IF(XE(J3).GT.XBC1.AND.XE(J1).GT.XBC1) NBB3=1
      IF(YE(J3).GT.YBC1.AND.YE(J1).GT.YBC1) NBB3=1
      ENDIF
c
      IF(MATL.EQ.'6') THEN
      xx=h4m/2.0
      yy=h2m/2.0
      XBC1=xx-1.E-6
      YBC1=yy-1.E-6
      IF(XE(J1).GT.XBC1.AND.XE(J2).GT.XBC1) NBB1=1
      IF(YE(J1).GT.YBC1.AND.YE(J2).GT.YBC1) NBB1=1
      IF(XE(J2).GT.XBC1.AND.XE(J3).GT.XBC1) NBB2=1
      IF(YE(J2).GT.YBC1.AND.YE(J3).GT.YBC1) NBB2=1
      IF(XE(J3).GT.XBC1.AND.XE(J1).GT.XBC1) NBB3=1
      IF(YE(J3).GT.YBC1.AND.YE(J1).GT.YBC1) NBB3=1
      ENDIF
c
      IF(MATL.EQ.'10') THEN
      xx1=h4m
      yy1=h2m
	xx0=0.0
	yy0=0.0
      XBC0=xx0+1.E-6
      YBC0=yy0+1.E-6
      XBC1=xx1-1.E-6
      YBC1=yy1-1.E-6
      IF(XE(J1).GT.XBC1.AND.XE(J2).GT.XBC1) NBB1=1
      IF(YE(J1).GT.YBC1.AND.YE(J2).GT.YBC1) NBB1=1
      IF(XE(J2).GT.XBC1.AND.XE(J3).GT.XBC1) NBB2=1
      IF(YE(J2).GT.YBC1.AND.YE(J3).GT.YBC1) NBB2=1
      IF(XE(J3).GT.XBC1.AND.XE(J1).GT.XBC1) NBB3=1
      IF(YE(J3).GT.YBC1.AND.YE(J1).GT.YBC1) NBB3=1
      IF(XE(J1).LT.XBC0.AND.XE(J2).LT.XBC0) NBB1=1
      IF(YE(J1).LT.YBC0.AND.YE(J2).LT.YBC0) NBB1=1
      IF(XE(J2).LT.XBC0.AND.XE(J3).LT.XBC0) NBB2=1
      IF(YE(J2).LT.YBC0.AND.YE(J3).LT.YBC0) NBB2=1
      IF(XE(J3).LT.XBC0.AND.XE(J1).LT.XBC0) NBB3=1
      IF(YE(J3).LT.YBC0.AND.YE(J1).LT.YBC0) NBB3=1
      ENDIF
c
      IF(MATL.EQ.'8') THEN
      xx=h4m/2.0
      yy=h2m/2.0
      XBC1=xx-1.E-6
      YBC1=yy-1.E-6
      IF(XE(J1).GT.XBC1.AND.XE(J2).GT.XBC1) NBB1=0
      IF(YE(J1).GT.YBC1.AND.YE(J2).GT.YBC1) NBB1=0
      IF(XE(J2).GT.XBC1.AND.XE(J3).GT.XBC1) NBB2=0
      IF(YE(J2).GT.YBC1.AND.YE(J3).GT.YBC1) NBB2=1
      IF(XE(J3).GT.XBC1.AND.XE(J1).GT.XBC1) NBB3=1
      IF(YE(J3).GT.YBC1.AND.YE(J1).GT.YBC1) NBB3=1
      ENDIF
C
      IF(MATL.EQ.'11') THEN
      X11=X2-1.E-4
      X22=X4-1.E-4
      Y11=Y1+1.E-4
      Y22=Y4-1.E-4
c 
      IF(XE(J1).GT.X11.AND.XE(J2).GT.X11.AND.
     .   YE(J1).LT.Y11.AND.YE(J2).LT.Y11) NBB1=1
      IF(XE(J2).GT.X11.AND.XE(J3).GT.X11.AND.
     .   YE(J2).LT.Y11.AND.YE(J3).LT.Y11) NBB2=1
      IF(XE(J1).GT.X11.AND.XE(J3).GT.X11.AND.
     .   YE(J1).LT.Y11.AND.YE(J3).LT.Y11) NBB3=1
c 
      IF(XE(J1).GT.X22.AND.XE(J2).GT.X22) NBB1=1
      IF(YE(J1).GT.Y22.AND.YE(J2).GT.Y22) NBB1=1
      IF(XE(J2).GT.X22.AND.XE(J3).GT.X22) NBB2=1
      IF(YE(J2).GT.Y22.AND.YE(J3).GT.Y22) NBB2=1
      IF(XE(J3).GT.X22.AND.XE(J1).GT.X22) NBB3=1
      IF(YE(J3).GT.Y22.AND.YE(J1).GT.Y22) NBB3=1      
      ENDIF
c      
      IF(MATL.EQ.'2') THEN
      X11=X2-1.E-4
      X22=X4-1.E-4
      Y11=Y1+1.E-4
      Y22=Y4-1.E-4
c 
      IF(XE(J1).GT.X11.AND.XE(J2).GT.X11.AND.
     .   YE(J1).LT.Y11.AND.YE(J2).LT.Y11) NBB1=1
      IF(XE(J2).GT.X11.AND.XE(J3).GT.X11.AND.
     .   YE(J2).LT.Y11.AND.YE(J3).LT.Y11) NBB2=1
      IF(XE(J1).GT.X11.AND.XE(J3).GT.X11.AND.
     .   YE(J1).LT.Y11.AND.YE(J3).LT.Y11) NBB3=1
c 
      IF(XE(J1).GT.X22.AND.XE(J2).GT.X22) NBB1=1
      IF(YE(J1).GT.Y22.AND.YE(J2).GT.Y22) NBB1=1
      IF(XE(J2).GT.X22.AND.XE(J3).GT.X22) NBB2=1
      IF(YE(J2).GT.Y22.AND.YE(J3).GT.Y22) NBB2=1
      IF(XE(J3).GT.X22.AND.XE(J1).GT.X22) NBB3=1
      IF(YE(J3).GT.Y22.AND.YE(J1).GT.Y22) NBB3=1      
      ENDIF
c
      IF(MATL.EQ.'3') THEN
      XBC1=X1-1.E-4
      XBC2=X2-1.E-4
      YBC1=Y1+1.E-4
      YBC2=Y2-1.E-4
      X11=XBC1
      X22=XBC2
      Y11=YBC1
      Y22=YBC2
      IF(XE(J1).GT.X11.AND.XE(J2).GT.X11.AND.
     .   YE(J1).LT.Y11.AND.YE(J2).LT.Y11) NBB1=1
      IF(XE(J2).GT.X11.AND.XE(J3).GT.X11.AND.
     .   YE(J2).LT.Y11.AND.YE(J3).LT.Y11) NBB2=1
      IF(XE(J1).GT.X11.AND.XE(J3).GT.X11.AND.
     .   YE(J1).LT.Y11.AND.YE(J3).LT.Y11) NBB3=1
 
      IF(XE(J1).GT.X22.AND.XE(J2).GT.X22) NBB1=1
      IF(YE(J1).GT.Y22.AND.YE(J2).GT.Y22) NBB1=1
      IF(XE(J2).GT.X22.AND.XE(J3).GT.X22) NBB2=1
      IF(YE(J2).GT.Y22.AND.YE(J3).GT.Y22) NBB2=1
      IF(XE(J3).GT.X22.AND.XE(J1).GT.X22) NBB3=1
      IF(YE(J3).GT.Y22.AND.YE(J1).GT.Y22) NBB3=1      
      ENDIF
c
      IF(MATL.EQ.'9') THEN
      
	XBC1=X1m9+1.E-5
      XBC2=X2m9+1.E-5
      XBC3=X3m9-1.E-5
      XBC4=X4m9-1.E-5

      YBC1=Y1m9+1.E-5
      YBC2=Y2m9-1.E-5
      YBC3=Y3m9+1.E-5
      YBC4=Y4m9-1.E-5

      X11=XBC1
      X22=XBC2
      X33=XBC3
      X44=XBC4

      Y11=YBC1
      Y22=YBC2
      Y33=YBC3
      Y44=YBC4
 
C Left face
      IF(XE(J1).LT.X11.AND.XE(J2).LT.X11) NBB1=1
      IF(XE(J2).LT.X11.AND.XE(J3).LT.X11) NBB2=1
      IF(XE(J3).LT.X11.AND.XE(J1).LT.X11) NBB3=1

C Right Face
      IF(XE(J1).GT.X44.AND.XE(J2).GT.X44) NBB1=1
      IF(XE(J2).GT.X44.AND.XE(J3).GT.X44) NBB2=1
      IF(XE(J3).GT.X44.AND.XE(J1).GT.X44) NBB3=1

C Bottom
      IF(YE(J2).LT.Y11.AND.YE(J3).LT.Y11) NBB2=1
      IF(YE(J1).LT.Y11.AND.YE(J2).LT.Y11) NBB1=1
      IF(YE(J3).LT.Y11.AND.YE(J1).LT.Y11) NBB3=1      

C Top
      IF(YE(J1).GT.Y44.AND.YE(J2).GT.Y44) NBB1=1
      IF(YE(J2).GT.Y44.AND.YE(J3).GT.Y44) NBB2=1
      IF(YE(J3).GT.Y44.AND.YE(J1).GT.Y44) NBB3=1      

C Inside - Left
      IF(XE(J1).LT.X22.AND.XE(J2).LT.X22.AND.
     .   YE(J1).GT.Y22.AND.YE(J2).GT.Y22.AND.
     .   YE(J1).LT.Y33.AND.YE(J2).LT.Y33) NBB1=1
      IF(XE(J2).LT.X22.AND.XE(J3).LT.X22.AND.
     .   YE(J2).GT.Y22.AND.YE(J3).GT.Y22.AND.
     .   YE(J2).LT.Y33.AND.YE(J3).LT.Y33) NBB1=1
      IF(XE(J3).LT.X22.AND.XE(J1).LT.X22.AND.
     .   YE(J3).GT.Y22.AND.YE(J1).GT.Y22.AND.
     .   YE(J3).LT.Y33.AND.YE(J1).LT.Y33) NBB1=1

C Inside - Right
      IF(XE(J1).GT.X33.AND.XE(J2).GT.X33.AND.
     .   YE(J1).GT.Y22.AND.YE(J2).GT.Y22.AND.
     .   YE(J1).LT.Y33.AND.YE(J2).LT.Y33) NBB1=1
      IF(XE(J2).GT.X33.AND.XE(J3).GT.X33.AND.
     .   YE(J2).GT.Y22.AND.YE(J3).GT.Y22.AND.
     .   YE(J2).LT.Y33.AND.YE(J3).LT.Y33) NBB1=1
      IF(XE(J3).GT.X33.AND.XE(J1).GT.X33.AND.
     .   YE(J3).GT.Y22.AND.YE(J1).GT.Y22.AND.
     .   YE(J3).LT.Y33.AND.YE(J1).LT.Y33) NBB1=1

      ENDIF

c      
      IF(MATL.EQ.'4') THEN
      XBC1=X2-1.E-4
      YBC1=Y2-1.E-4
      IF(XE(J1).GT.XBC1.AND.XE(J2).GT.XBC1) NBB1=1
      IF(YE(J1).GT.YBC1.AND.YE(J2).GT.YBC1) NBB1=1
      IF(XE(J2).GT.XBC1.AND.XE(J3).GT.XBC1) NBB2=1
      IF(YE(J2).GT.YBC1.AND.YE(J3).GT.YBC1) NBB2=1
      IF(XE(J3).GT.XBC1.AND.XE(J1).GT.XBC1) NBB3=1
      IF(YE(J3).GT.YBC1.AND.YE(J1).GT.YBC1) NBB3=1
      ENDIF

c      
c           Center location for each element.
c
      XCNEL=(XE(J1)+XE(J2)+XE(J3))/3.0
      YCNEL=(YE(J1)+YE(J2)+YE(J3))/3.0       
c      
c           For M1 cross section.
c
      IF(MATL.EQ.'1') THEN
      X11=X1-1.E-4
      X22=X2-1.E-4
      Y11=Y1+1.E-4
      Y22=Y2-1.E-4
      MAT=1
      IF(XCNEL.LT.X11.AND.YCNEL.LT.Y11) MAT=2
      IF(XCNEL.LT.X22.AND.YCNEL.LT.Y22.AND.YCNEL.GT.Y11) MAT=2
      ENDIF                   
c      
c           For M5 cross section.
c
      IF(MATL.EQ.'5') THEN
      X11=w/2.0-1.E-4
      X22=h4m/2.0-1.E-4
      Y11=(h2m/2.0-thick)+1.E-4
      Y22=h2m/2.0-1.E-4
      MAT=1
      IF(XCNEL.LT.X11.AND.YCNEL.LT.Y11) MAT=2
      IF(XCNEL.LT.X22.AND.YCNEL.LT.Y22.AND.YCNEL.GT.Y11) MAT=2
      ENDIF                   
c      
c           For M6 cross section.
c
      IF(MATL.EQ.'6') THEN
	MAT=1
      ENDIF                   
c      
c           For M10 cross section.
c
      IF(MATL.EQ.'10') THEN
	MAT=1
      ENDIF                   
c      
c           For M8 cross section.
c
      IF(MATL.EQ.'8') THEN
	MAT=1
      ENDIF                   
c      
c           For M11 cross section.
c
      IF(MATL.EQ.'11') THEN
      MAT=1  ! 1 is for concrete
      IF(XCNEL.LT.X1.AND.YCNEL.LT.Y2) MAT=2
      IF(XCNEL.LT.X3.AND.YCNEL.GT.Y2.AND.YCNEL.LT.Y3) MAT=2
      ENDIF
c      
c           For M2 cross section.
c
      IF(MATL.EQ.'2') THEN
      MAT=3  ! 3 is for ceramic
      IF(XCNEL.LT.X1.AND.YCNEL.LT.Y2) MAT=2
      IF(XCNEL.LT.X3.AND.YCNEL.GT.Y2.AND.YCNEL.LT.Y3) MAT=2
      ENDIF
c      
c           For M3 cross section.
c
      IF(MATL.EQ.'3') THEN
      MAT=2
      ENDIF
c
      IF(MATL.EQ.'9') THEN
      MAT=2
      ENDIF

c      
c           For M4 cross section.
c
      IF(MATL.EQ.'4') THEN
      X11=X1
      Y11=Y1
      MAT=2
      IF(XCNEL.LT.X11.AND.YCNEL.LT.Y11) MAT=1
      ENDIF                   
c
      WRITE(3,108) NEL,NE(J1),NE(J2),NE(J3),NBB1,NBB2,NBB3,MAT
  108 FORMAT(1X,8(I4,1X))
c
      XCC(NE(J1))=XE(J1)
      YCC(NE(J1))=YE(J1)
      XCC(NE(J2))=XE(J2)
      YCC(NE(J2))=YE(J2)
      XCC(NE(J3))=XE(J3)
      YCC(NE(J3))=YE(J3)
c
      IF(NE(J1).GT.NX) NX=NE(J1)
      IF(NE(J2).GT.NX) NX=NE(J2)
      IF(NE(J3).GT.NX) NX=NE(J3)
c
  209 FORMAT(1X,4I5,3X,6F12.4)
c
   15 CONTINUE
   16 CONTINUE         
c
      PRINT *,'   '
      PRINT *,'--------------------------'    
      PRINT *,'NUMBER OF NODES    =',NX
      PRINT *,'NUMBER OF ELEMENTS =',NEL
      PRINT *,'--------------------------'    
      NDMAX=0
      DNMAX=0.0
      YNMAX=0.0
      XNMAX=0.0
c

c Determining the boundary flaq, IBCC for each node located
c at the boundary.
c
      DO 17 IK=1,NX
      IFT=0                                      
      IBCC=0
c
      IF(MATL.EQ.'1') THEN
      IF(XCC(IK).GT.(XMAX-1.0E-4)) IBCC=1 
      IF(YCC(IK).GT.(YMAX-1.0E-4)) IBCC=1
      ENDIF
c
      IF(MATL.EQ.'5') THEN
      IF(XCC(IK).GT.(XMAX-1.0E-4)) IBCC=1 
      IF(YCC(IK).GT.(YMAX-1.0E-4)) IBCC=1
      ENDIF
c
      IF(MATL.EQ.'11') THEN      
      IF(XCC(IK).GT.X11.AND.YCC(IK).LT.Y11) IBCC=1      
      IF(XCC(IK).GT.X22) IBCC=1
      IF(YCC(IK).GT.Y22) IBCC=1
      ENDIF
c
      IF(MATL.EQ.'2') THEN      
      IF(XCC(IK).GT.X11.AND.YCC(IK).LT.Y11) IBCC=1      
      IF(XCC(IK).GT.X22) IBCC=1
      IF(YCC(IK).GT.Y22) IBCC=1
      ENDIF
c      
      IF(MATL.EQ.'3') THEN      
      IF(XCC(IK).GE.X11.AND.YCC(IK).LE.Y11) IBCC=1 
      IF(YCC(IK).GE.Y22) IBCC=1
      IF(XCC(IK).GE.X22) IBCC=1 
      ENDIF
c
c      
      IF(MATL.EQ.'9') THEN      
      IF(XCC(IK).GE.X11.AND.YCC(IK).LE.Y11) IBCC=1 
      IF(YCC(IK).GE.Y22) IBCC=1
      IF(XCC(IK).GE.X22) IBCC=1 
      ENDIF

c
      IF(MATL.EQ.'4') THEN
      IF(XCC(IK).GT.(X2-1.0E-6)) IBCC=1 
      IF(YCC(IK).GT.(Y2-1.0E-6)) IBCC=1
      ENDIF
c
c TO CALCULATE EXTREME NODE      
c
      IF((XCC(IK)+YCC(IK)).GT.DNMAX) THEN
      YNMAX=YCC(IK)				   
C DRAWING THE TEXT BOX THAT REPRESENTS THE COLOR TO TEMPERATURE
C
	  XR1=480
	  XR2=500
	  YR1=10
	  YR2=20
	  TCR=850
C      CALL LINEDR(XR1,XR2,YR1,YR2,TCR)
C

      XNMAX=XCC(IK)
      NMAX=IK
      DNMAX=XCC(IK)+YCC(IK)
      ENDIF
c
      IFT=0
c
   17 WRITE(4,18) IK,XCC(IK),YCC(IK),IFT,IBCC
   18 FORMAT(1X,I4,2(F25.12,1X),I4,1X,I4)
      WRITE(*,210) NBW,NELBW
      WRITE(2,210) NBW,NELBW
  210 FORMAT(//1X,21HBANDWIDTH QUANTITY IS,I4,22H CALCULATED IN ELEMENT
     1       ,I4)
      PRINT *, '----------------------------------'
      PRINT *, 'Extreme node = ',NMAX
      PRINT *, 'X-coordinate = ',XNMAX
      PRINT *, 'Y-coordinate = ',YNMAX
      PRINT *, '----------------------------------'
110	CONTINUE
      write(*,*) 'Press ENTER to continue'
	CLOSE(UNIT=2)
	CLOSE(UNIT=3)
	CLOSE(UNIT=4)
999	RETURN
      END
C      		
C ****************************
C Start Subroutine 11 Here
C ****************************
C			
c Mesh for Column Type 11
c for ibeam with insulator
      subroutine m11(fm,dim)
      double precision x(60),y(60)
      integer r1,r2,r3,r4,c1,c2,c3,c4
      character fm*13,as*1,dim*13
c the dimensions
      print *,'Enter the parameters (y/n)- NO to use W250-101 (sample)'
      read(*,'(a1)') as
      if(as.eq.'y'.or.as.eq.'Y') then
      print *,'Enter the dimensions :'
      print *,'Depth (mm):'
      read(*,*) depth
      print *,'Width (mm):'
      read(*,*) width
      print *,'Flange thickness (mm):'
      read(*,*) thf
      print *,'Web thickness (mm):'
      read(*,*) thw
      print *,'Coating thickness (mm):'
      read(*,*) thp
	depth=depth/1000.0
	width=width/1000.0
	thf=thf/1000.0
	thw=thw/1000.0
	thp=thp/1000.0
      else
      print *,'Using the W250-101 (sample)'
      depth=264.e-03
      width=257.0e-03
      thf=19.6e-03
      thw=11.9e-03
      thp=38.1e-03
      endif      
c
      open(unit=2,file=dim,status='unknown')
      write(2,*) 'concrete coating + Ibeam'
      write(2,*) 'Filename :',dim
      write(2,*) 'Flange,m :',thf
      write(2,*) 'Depth,m  :',depth
      write(2,*) 'Width,m  :',width
      write(2,*) 'Web,m    :',thw
      close(unit=2)
c basic parameter
      x1=thw/2.0
      x2=thw/2.0+thp
      x3=width/2.0
      x4=width/2.0+thp
      y1=depth/2.0-thf-thp
      y2=depth/2.0-thf
      y3=depth/2.0
      y4=depth/2.0+thp
c the coordinate
      x(1)=0.0d0
      x(2)=x1/2.0d0
      x(3)=x1
      x(4)=x1+(x2-x1)/2.0d0
      x(5)=x2
      x(14)=x2+(x3-x2)/2.0d0
      x(15)=x3
      x(16)=x3+(x4-x3)/2.0d0
      x(17)=x4
c      
      x(6)=x(1)
      x(9)=x(1)
      x(18)=x(1)
      x(23)=x(1)
      x(32)=x(1)
      x(37)=x(1)
      x(46)=x(1)
      x(51)=x(1)
      x(10)=x(2)
      x(24)=x(2)
      x(38)=x(2)
      x(52)=x(2)
      x(7)=x(3)
      x(11)=x(3)
      x(19)=x(3)
      x(25)=x(3)
      x(33)=x(3)
      x(39)=x(3)
      x(47)=x(3)
      x(53)=x(3)
      x(12)=x(4)
      x(26)=x(4)
      x(40)=x(4)
      x(54)=x(4)
      x(8)=x(5)
      x(13)=x(5)
      x(20)=x(5)
      x(27)=x(5)
      x(34)=x(5)
      x(41)=x(5)
      x(48)=x(5)
      x(55)=x(5)
      x(28)=x(14)
      x(42)=x(14)
      x(56)=x(14)
      x(21)=x(15)
      x(29)=x(15)
      x(35)=x(15)
      x(43)=x(15)
      x(49)=x(15)
      x(57)=x(15)
      x(30)=x(16)
      x(44)=x(16)
      x(58)=x(16)
      x(22)=x(17)
      x(31)=x(17)
      x(36)=x(17)
      x(45)=x(17)
      x(50)=x(17)
      x(59)=x(17)
c y co-ordinate
      y(1)=0.0d0
      y(6)=y1/2.0d0
      y(9)=y1
      y(18)=y1+(y2-y1)/2.0d0
      y(23)=y2      
      y(32)=y2+(y3-y2)/2.0d0
      y(37)=y3      
      y(46)=y3+(y4-y3)/2.0d0    
      y(51)=y4
      y(2)=y(1)      
      y(3)=y(1)      
      y(4)=y(1)      
      y(5)=y(1)      
      y(7)=y(6)      
      y(8)=y(6)      
      y(10)=y(9)      
      y(11)=y(9)      
      y(12)=y(9)      
      y(13)=y(9)      
      y(14)=y(9)      
      y(15)=y(9)      
      y(16)=y(9)      
      y(17)=y(9)      
      y(19)=y(18)      
      y(20)=y(18)      
      y(21)=y(18)      
      y(22)=y(18)      
      y(24)=y(23)      
      y(25)=y(23)      
      y(26)=y(23)      
      y(27)=y(23)      
      y(28)=y(23)      
      y(29)=y(23)      
      y(30)=y(23)      
      y(31)=y(23)      
      y(33)=y(32)      
      y(34)=y(32)      
      y(35)=y(32)      
      y(36)=y(32)      
      y(38)=y(37)      
      y(39)=y(37)      
      y(40)=y(37)      
      y(41)=y(37)      
      y(42)=y(37)      
      y(43)=y(37)      
      y(44)=y(37)      
      y(45)=y(37)      
      y(47)=y(46)      
      y(48)=y(46)      
      y(49)=y(46)      
      y(50)=y(46)      
      y(52)=y(51)      
      y(53)=y(51)      
      y(54)=y(51)      
      y(55)=y(51)      
      y(56)=y(51)      
      y(57)=y(51)      
      y(58)=y(51)      
      y(59)=y(51)
c     
      print *,'Do you want to customise the rows/columns (y/n)'
      read(*,'(a1)') as   
      if(as.eq.'y'.or.as.eq.'Y') then
      print *,'Enter the dimensions (default):'
      print *,'Row 1 (13):'
      read(*,*) r1
      print *,'Row 2 (3):'
      read(*,*) r2
      print *,'Row 3 (4):'
      read(*,*) r3
      print *,'Row 4 (3):'
      read(*,*) r4
      print *,'Column 1 (2):'
      read(*,*) c1
      print *,'Column 2 (3):'
      read(*,*) c2
      print *,'Column 3 (17):'
      read(*,*) c3
      print *,'Column 4 (4):'
      read(*,*) c4
      else 
      print *,'Using the default'
      r1=13
      r2=3
      r3=4
      r4=3
      c1=2
      c2=3
      c3=17
      c4=4
      endif
c
      open(unit=1,file=fm,status='unknown')
      write(1,*) '14 59 1 2'
      write(1,*) x1
      write(1,*) x2
      write(1,*) x3
      write(1,*) x4
      write(1,*) y1
      write(1,*) y2
      write(1,*) y3
      write(1,*) y4
      do i=1,59
      write(1,*) x(i),y(i)
      end do
      write(1,*) '1 3 0 0 2'      
      write(1,*) '2 4 1 0 0'      
      write(1,*) '3 7 0 1 4'      
      write(1,*) '4 8 3 2 5'      
      write(1,*) '5 4 0 6 9'      
      write(1,*) '6 5 0 0 10'      
      write(1,*) '7 11 0 3 8'      
      write(1,*) '8 12 7 4 9'      
      write(1,*) '9 8 5 10 13'      
      write(1,*) '10 9 6 0 14'      
      write(1,*) '11 0 0 7 12'      
      write(1,*) '12 0 11 8 13'      
      write(1,*) '13 12 9 14 0'      
      write(1,*) '14 13 10 0 0'      
      write(1,*) '1  ',r1,c1,' 11 10  9  6  1  2  3  7'
      write(1,*) '2  ',r1,c2,' 13 12 11  7  3  4  5  8'
      write(1,*) '3  ',r2,c1,' 25 24 23 18  9 10 11 19'
      write(1,*) '4  ',r2,c2,' 27 26 25 19 11 12 13 20'
      write(1,*) '5  ',c3,r2,' 27 20 13 14 15 21 29 28'
      write(1,*) '6  ',c4,r2,' 29 21 15 16 17 22 31 30'
      write(1,*) '7  ',r3,c1,' 39 38 37 32 23 24 25 33'
      write(1,*) '8  ',r3,c2,' 41 40 39 33 25 26 27 34'
      write(1,*) '9  ',c3,r3,' 41 34 27 28 29 35 43 42'
      write(1,*) '10 ',c4,r3,' 43 35 29 30 31 36 45 44'
      write(1,*) '11 ',r4,c1,' 53 52 51 46 37 38 39 47'
      write(1,*) '12 ',r4,c2,' 55 54 53 47 39 40 41 48'
      write(1,*) '13 ',c3,r4,' 55 48 41 42 43 49 57 56'
      write(1,*) '14 ',c4,r4,' 57 49 43 44 45 50 59 58'
      CLOSE(UNIT=1)       
      return
      end      
C
C ****************************
C Start Subroutine 2 Here
C ****************************
C			
c Mesh for Column Type 2
c for ibeam with insulator
      subroutine m2(fm,dim)
      double precision x(60),y(60)
      integer r1,r2,r3,r4,c1,c2,c3,c4
      character fm*13,as*1,dim*13
c the dimensions
      print *,'Enter the parameters (y/n)- NO to use W250-101 (sample)'
      read(*,'(a1)') as
      if(as.eq.'y'.or.as.eq.'Y') then
      print *,'Enter the dimensions :'
      print *,'Depth (mm):'
      read(*,*) depth
      print *,'Width (mm):'
      read(*,*) width
      print *,'Flange thickness (mm):'
      read(*,*) thf
      print *,'Web thickness (mm):'
      read(*,*) thw
      print *,'Coating thickness (mm):'
      read(*,*) thp
	depth=depth/1000.0
	width=width/1000.0
	thf=thf/1000.0
	thw=thw/1000.0
	thp=thp/1000.0
      else
      print *,'Using the W250-101 (sample)'
      depth=264.e-03
      width=257.0e-03
      thf=19.6e-03
      thw=11.9e-03
      thp=38.1e-03
      endif      
c
      open(unit=2,file=dim,status='unknown')
      write(2,*) 'concrete coating + Ibeam'
      write(2,*) 'Filename :',dim
      write(2,*) 'Flange,m :',thf
      write(2,*) 'Depth,m  :',depth
      write(2,*) 'Width,m  :',width
      write(2,*) 'Web,m    :',thw
      close(unit=2)
c basic parameter
      x1=thw/2.0
      x2=thw/2.0+thp
      x3=width/2.0
      x4=width/2.0+thp
      y1=depth/2.0-thf-thp
      y2=depth/2.0-thf
      y3=depth/2.0
      y4=depth/2.0+thp
c the coordinate
      x(1)=0.0d0
      x(2)=x1/2.0d0
      x(3)=x1
      x(4)=x1+(x2-x1)/2.0d0
      x(5)=x2
      x(14)=x2+(x3-x2)/2.0d0
      x(15)=x3
      x(16)=x3+(x4-x3)/2.0d0
      x(17)=x4
c      
      x(6)=x(1)
      x(9)=x(1)
      x(18)=x(1)
      x(23)=x(1)
      x(32)=x(1)
      x(37)=x(1)
      x(46)=x(1)
      x(51)=x(1)
      x(10)=x(2)
      x(24)=x(2)
      x(38)=x(2)
      x(52)=x(2)
      x(7)=x(3)
      x(11)=x(3)
      x(19)=x(3)
      x(25)=x(3)
      x(33)=x(3)
      x(39)=x(3)
      x(47)=x(3)
      x(53)=x(3)
      x(12)=x(4)
      x(26)=x(4)
      x(40)=x(4)
      x(54)=x(4)
      x(8)=x(5)
      x(13)=x(5)
      x(20)=x(5)
      x(27)=x(5)
      x(34)=x(5)
      x(41)=x(5)
      x(48)=x(5)
      x(55)=x(5)
      x(28)=x(14)
      x(42)=x(14)
      x(56)=x(14)
      x(21)=x(15)
      x(29)=x(15)
      x(35)=x(15)
      x(43)=x(15)
      x(49)=x(15)
      x(57)=x(15)
      x(30)=x(16)
      x(44)=x(16)
      x(58)=x(16)
      x(22)=x(17)
      x(31)=x(17)
      x(36)=x(17)
      x(45)=x(17)
      x(50)=x(17)
      x(59)=x(17)
c y co-ordinate
      y(1)=0.0d0
      y(6)=y1/2.0d0
      y(9)=y1
      y(18)=y1+(y2-y1)/2.0d0
      y(23)=y2      
      y(32)=y2+(y3-y2)/2.0d0
      y(37)=y3      
      y(46)=y3+(y4-y3)/2.0d0    
      y(51)=y4
      y(2)=y(1)      
      y(3)=y(1)      
      y(4)=y(1)      
      y(5)=y(1)      
      y(7)=y(6)      
      y(8)=y(6)      
      y(10)=y(9)      
      y(11)=y(9)      
      y(12)=y(9)      
      y(13)=y(9)      
      y(14)=y(9)      
      y(15)=y(9)      
      y(16)=y(9)      
      y(17)=y(9)      
      y(19)=y(18)      
      y(20)=y(18)      
      y(21)=y(18)      
      y(22)=y(18)      
      y(24)=y(23)      
      y(25)=y(23)      
      y(26)=y(23)      
      y(27)=y(23)      
      y(28)=y(23)      
      y(29)=y(23)      
      y(30)=y(23)      
      y(31)=y(23)      
      y(33)=y(32)      
      y(34)=y(32)      
      y(35)=y(32)      
      y(36)=y(32)      
      y(38)=y(37)      
      y(39)=y(37)      
      y(40)=y(37)      
      y(41)=y(37)      
      y(42)=y(37)      
      y(43)=y(37)      
      y(44)=y(37)      
      y(45)=y(37)      
      y(47)=y(46)      
      y(48)=y(46)      
      y(49)=y(46)      
      y(50)=y(46)      
      y(52)=y(51)      
      y(53)=y(51)      
      y(54)=y(51)      
      y(55)=y(51)      
      y(56)=y(51)      
      y(57)=y(51)      
      y(58)=y(51)      
      y(59)=y(51)
c     
      print *,'Do you want to customise the rows/columns (y/n)'
      read(*,'(a1)') as   
      if(as.eq.'y'.or.as.eq.'Y') then
      print *,'Enter the dimensions :'
      print *,'Row 1 :'
      read(*,*) r1
      print *,'Row 2 :'
      read(*,*) r2
      print *,'Row 3 :'
      read(*,*) r3
      print *,'Row 4 :'
      read(*,*) r4
      print *,'Column 1 :'
      read(*,*) c1
      print *,'Column 2 :'
      read(*,*) c2
      print *,'Column 3 :'
      read(*,*) c3
      print *,'Column 4 :'
      read(*,*) c4
      else 
      print *,'Using the default'
      r1=8
      r2=8
      r3=6
      r4=8
      c1=2
      c2=7
      c3=9
      c4=7
      endif
c
      open(unit=1,file=fm,status='unknown')
      write(1,*) '14 59 1 2'
      write(1,*) x1
      write(1,*) x2
      write(1,*) x3
      write(1,*) x4
      write(1,*) y1
      write(1,*) y2
      write(1,*) y3
      write(1,*) y4
      do i=1,59
      write(1,*) x(i),y(i)
      end do
      write(1,*) '1 3 0 0 2'      
      write(1,*) '2 4 1 0 0'      
      write(1,*) '3 7 0 1 4'      
      write(1,*) '4 8 3 2 5'      
      write(1,*) '5 4 0 6 9'      
      write(1,*) '6 5 0 0 10'      
      write(1,*) '7 11 0 3 8'      
      write(1,*) '8 12 7 4 9'      
      write(1,*) '9 8 5 10 13'      
      write(1,*) '10 9 6 0 14'      
      write(1,*) '11 0 0 7 12'      
      write(1,*) '12 0 11 8 13'      
      write(1,*) '13 12 9 14 0'      
      write(1,*) '14 13 10 0 0'      
      write(1,*) '1  ',r1,c1,' 11 10  9  6  1  2  3  7'
      write(1,*) '2  ',r1,c2,' 13 12 11  7  3  4  5  8'
      write(1,*) '3  ',r2,c1,' 25 24 23 18  9 10 11 19'
      write(1,*) '4  ',r2,c2,' 27 26 25 19 11 12 13 20'
      write(1,*) '5  ',c3,r2,' 27 20 13 14 15 21 29 28'
      write(1,*) '6  ',c4,r2,' 29 21 15 16 17 22 31 30'
      write(1,*) '7  ',r3,c1,' 39 38 37 32 23 24 25 33'
      write(1,*) '8  ',r3,c2,' 41 40 39 33 25 26 27 34'
      write(1,*) '9  ',c3,r3,' 41 34 27 28 29 35 43 42'
      write(1,*) '10 ',c4,r3,' 43 35 29 30 31 36 45 44'
      write(1,*) '11 ',r4,c1,' 53 52 51 46 37 38 39 47'
      write(1,*) '12 ',r4,c2,' 55 54 53 47 39 40 41 48'
      write(1,*) '13 ',c3,r4,' 55 48 41 42 43 49 57 56'
      write(1,*) '14 ',c4,r4,' 57 49 43 44 45 50 59 58'
      CLOSE(UNIT=1)       
      return
      end      
C
C MESHING OF CROSS SECTION 1
C
      SUBROUTINE m1(fname,dim)
      DOUBLE PRECISION thick, h2m,h4m,h2a,h4a,w,x(1000),y(1000)
      CHARACTER FNAME*13,ID*1,dim*13,as*1
      INTEGER r(9),c(9)
      PRINT *,'Do you want to use new parameters ? (y/n) :'
	   PRINT 9,' *If no, the default dimension is given : ' 
      READ(*,'(A1)') ID
      IF(ID.EQ.'y'.OR.ID.EQ.'Y') THEN
       PRINT 9, ' Enter vertical thickness of concrete (meter) :'
       READ(*,*) H2A
       PRINT 9, ' Enter horizontal thickness of concrete (meter) :'
       READ(*,*) H4A
       PRINT 9, ' Enter ibeam height (meter) :'
       READ(*,*) H2M
       PRINT 9, ' Enter ibeam width (meter) :'
       READ(*,*) H4M
       PRINT 9, ' Enter the flange thickness (meter) :'
       READ(*,*) THICK
       PRINT 9, ' Enter web width (meter) :'
       READ(*,*) W
9     FORMAT(A)      
C       
      ELSE      
C      
       PRINT *,'This part is for the default column size'
       PRINT *,'---------------------------------------------------'
       PRINT *,'Enter the type of column to be analyzed (1/2/3/4/5):'
       READ(*,*) NCOL
c       
       IF(NCOL.EQ.1) THEN
C
C     MEASURED VALUES OF COLUMN 1
C				  
      THICK=0.0305D0
      H2M=0.2915D0
      H4M=0.266D0
      H2A=0.0795D0
      H4A=0.07875D0
      W=0.0195D0
      print *,'Thick =',thick
      print *,'Horizontal thickness of concrete =',h4a
      print *,'Vertical thickness of concrete =',h2a
      print *,'I-Beam height =',h2m
      print *,'I-Beam width =',h4m
      print *,'Web thickness =',w
      write(*,*)  'Press ENTER'

C      
       ELSEIF(NCOL.EQ.2) THEN
C
C     MEASURED VALUES FOR COLUMN 2   
C
      THICK=0.0315D0
      H2M=0.2915D0
      H4M=0.266D0
      H2A=0.07875D0
      H4A=0.07950D0
      W=0.019D0
      print *,'Thick =',thick
      print *,'Horizontal thickness of concrete =',h4a
      print *,'Vertical thickness of concrete =',h2a
      print *,'I-Beam height =',h2m
      print *,'I-Beam width =',h4m
      print *,'Web thickness =',w
      write(*,*)  'Press ENTER'
C      
       ELSEIF(NCOL.EQ.3) THEN
C
C     MEASURED VALUES FOR COLUMN 3
C      
      THICK=0.023D0
      H2M=0.232D0
      H4M=0.211D0
      H2A=0.07850D0
      H4A=0.07950D0
      W=0.014D0  
      print *,'Thick =',thick									 
      print *,'Horizontal thickness of concrete =',h4a
      print *,'Vertical thickness of concrete =',h2a
      print *,'I-Beam height =',h2m
      print *,'I-Beam width =',h4m
      print *,'Web thickness =',w
      write(*,*)  'Press ENTER'
      
       ELSEIF(NCOL.EQ.4) THEN
C
C     MEASURED VALUES FOR COLUMN 4
C                        
      THICK=0.031D0
      H2M=0.291D0
      H4M=0.266D0
      H2A=0.09D0
      H4A=0.09D0
      W=0.0196D0
      print *,'Thick =',thick
      print *,'Horizontal thickness of concrete =',h4a
      print *,'Vertical thickness of concrete =',h2a
      print *,'I-Beam height =',h2m
      print *,'I-Beam width =',h4m
      print *,'Web thickness =',w
      write(*,*)  'Press ENTER'
      
       ELSE
C      
C     MEASURED VALUES FOR COLUMN 5
C                        
      THICK=0.0305D0
      H2M=0.2915D0
      H4M=0.266D0
      H2A=0.07875D0
      H4A=0.07950D0
      W=0.0196D0
      print *,'Thick =',thick
      print *,'Horizontal thickness of concrete =',h4a
      print *,'Vertical thickness of concrete =',h2a
      print *,'I-Beam height =',h2m
      print *,'I-Beam width =',h4m
      print *,'Web thickness =',w
      write(*,*)  'Press ENTER'
C      
       ENDIF
      ENDIF                            
c
      open(unit=2,file=dim,status='unknown')
      write(2,*) 'Concrete + Ibeam + Reinforcement'
      write(2,*) 'Filename :',dim
      write(2,*) 'Flange,m :',thick
      write(2,*) 'Depth,m  :',h2m
      write(2,*) 'Width,m  :',h4m
      write(2,*) 'Web,m    :',w
      write(2,*) 'Vertical concrete from ibeam,m   :',h2a
      write(2,*) 'Horizontal concrete from ibeam,m :',h4a
      close(unit=2)
C
      y(1)=0.0d0
      y(23)=h2m/2.0d0
      y(34)=y(23)+h2a
      y(12)=y(23)-thick
      y(8)=y(12)/2.0d0
      y(19)=y(12)+(y(23)-y(12))/2.0d0
      y(30)=y(23)+(y(34)-y(23))/2.0d0
      do 1 i=2,7
1     y(i)=y(1)
      do 2 i=9,11
2     y(i)=y(8)
      do 3 i=13,18
3     y(i)=y(12)
      do 4 i=20,22
4     y(i)=y(19)
      do 5 i=24,29
5     y(i)=y(23)
      do 6 i=31,33
6     y(i)=y(30)
      do 7 i=35,40
7     y(i)=y(34)
      y(33)=y(30)+(y(34)-y(30))*0.250d0
c
      x(1)=0.0d0
      x(5)=h4m/2.0d0
      x(7)=x(5)+h4a
      x(3)=w/2.0d0
      x(2)=x(3)/2.0d0
      x(4)=x(3)+(x(5)-x(3))/2.0d0      
      x(6)=x(5)+(x(7)-x(5))/2.0d0    
      x(8)=x(1)
      x(12)=x(1)
      x(19)=x(1)
      x(23)=x(1)
      x(30)=x(1)
      x(34)=x(1)
      x(13)=x(2)
      x(24)=x(2)
      x(35)=x(2)
      x(9)=x(3)
      x(14)=x(3)
      x(20)=x(3)
      x(25)=x(3)
      x(31)=x(3)
      x(36)=x(3)
      x(15)=x(4)
      x(26)=x(4)
      x(37)=x(4)
      x(10)=x(5)
      x(16)=x(5)
      x(21)=x(5)
      x(27)=x(5)
      x(32)=x(5)
      x(38)=x(5)
      x(17)=x(6)
      x(28)=x(6)
      x(39)=x(6)+(x(7)-x(6))*0.250d0
      x(11)=x(7)
      x(18)=x(7)
      x(22)=x(7)
      x(29)=x(7)
      x(33)=x(7)
      x(40)=x(7)
      open(unit=9,file=FNAME,status='unknown')
      write(9,*) '9 40 1 1'
      write(9,*) x(3)
      write(9,*) x(5)
      write(9,*) y(12)
      write(9,*) y(23)
      do 8 i=1,40
8     write(9,*) x(i),y(i)
      write(9,*) '1 0 2 4 0'
      write(9,*) '2 0 3 5 1'
      write(9,*) '3 0 0 6 2'
      write(9,*) '4 1 5 7 0'
      write(9,*) '5 2 6 8 4'
      write(9,*) '6 3 0 9 5'
      write(9,*) '7 4 8 0 0'
      write(9,*) '8 5 9 0 7'
      write(9,*) '9 6 0 0 8'
C ***
      print *,'Do you want to customise the rows/columns (y/n)'
      read(*,'(a1)') as   
      if(as.eq.'y'.or.as.eq.'Y') then
      print *,'Enter the dimensions :'
      print *,'Row 1 ;r(3) ; default=7 :'   ! from buttom to top
      read(*,*) r(3)
      print *,'Row 2 ;r(6) ; default=4:'
      read(*,*) r(6)
      print *,'Row 3 ;r(9) ; default=7:'
      read(*,*) r(9)
      print *,'Column 1 ;c(7) ; default=3:'  ! from left to right
      read(*,*) c(7)
      print *,'Column 2 ;c(8) ; default=10:'
      read(*,*) c(8)
      print *,'Column 3 ;c(9) ; default=8:'
      read(*,*) c(9)
      else 
      print *,'Using the default'
      r(9)=7
      r(6)=4
      r(3)=7
      c(9)=8
      c(8)=10
      c(7)=3
      endif
c **********************
      r(7)=r(9)
      r(8)=r(9)
      r(4)=r(6)
      r(5)=r(6)
      r(1)=r(3)
      r(2)=r(3)
      c(6)=c(9)
      c(3)=c(9)
      c(5)=c(8)
      c(2)=c(8)
      c(4)=c(7)
      c(1)=c(7)
      write(9,*) '1',r(1),c(1),' 1 2 3 9 14 13 12 8'
      write(9,*) '2',r(2),c(2),' 3 4 5 10 16 15 14 9'
      write(9,*) '3',r(3),c(3),' 5 6 7 11 18 17 16 10'
      write(9,*) '4',r(4),c(4),' 12 13 14 20 25 24 23 19'
      write(9,*) '5',r(5),c(5),' 14 15 16 21 27 26 25 20'
      write(9,*) '6',r(6),c(6),' 16 17 18 22 29 28 27 21'
      write(9,*) '7',r(7),c(7),' 23 24 25 31 36 35 34 30'
      write(9,*) '8',r(8),c(8),' 25 26 27 32 38 37 36 31'
      write(9,*) '9',r(9),c(9),' 27 28 29 33 40 39 38 32'
      close(unit=9)
      return
      end            	
C *********************************************************
C MESHING OF CROSS SECTION 8
C *********************************************************
      subroutine m8(fname,h2m,h4m,dim)
      character*13 fname,dim
      character*1 yn
      real x(8),y(8)
      open(unit=1,file=fname,status='unknown')
      write(1,*) '1 8 1 2'
	print *,'Enter your selection'
	print *,'--------------------'
      print 9,' Yes - new dimension, No - sample dimension (y/n):'
      READ(*,'(A1)') yn
      if(yn.eq.'Y'.or.yn.eq.'y') then
	 Thick=0.0
       PRINT 9, ' Enter Concrete height (meter) :'
       READ(*,*) H2M
       PRINT 9, ' Enter Concrete width (meter) :'
       READ(*,*) H4M
       H2A=0.0
       H4A=0.0
	 W=0.0
9      FORMAT(A)      
      else 
       print *,'Use the sample dimension'
1      thick=0.0
       h2m=0.232
       h4m=0.211
       h2a=0.0
       h4a=0.0
       w=0.0
       write(*,*) 'Depth :',h2m
       write(*,*) 'Width :',h4m
      endif
      open(unit=2,file=dim,status='unknown')
      write(2,*) 'Concrete + Ibeam'
      write(2,*) 'Filename :',dim
      write(2,*) 'Depth :',h2m
      write(2,*) 'Width :',h4m
      close(unit=2)
      x1=h4m/2.0
      y1=h2m/2.0
      x(1)=0.0
      x(2)=x1/2.0
      x(3)=x1
      x(4)=x(1)
      x(6)=x(1)
      x(7)=x(2)
      x(5)=x(3)
      x(8)=x(3)
      y(1)=0.0
      y(4)=y1/2.0
      y(6)=y1
      y(2)=y(1)
      y(3)=y(1)
      y(5)=y(4)
      y(7)=y(6)
      y(8)=y(6)
      do i=1,8
      write(1,*) x(i),y(i)
      end do
      write(1,*) '1 0 0 0 0'
	ICOLXX=5
	IROWXX=50
	print *,'Enter number of columns and rows (default 5 50):'
	read(*,*) icolxx,irowxx
      write(1,*) '1 ',icolxx,irowxx,' 1 2 3 5 8 7 6 4'
      close(unit=1)
      return
      end


C *********************************************************
C MESHING OF CROSS SECTION 3
C *********************************************************
C
      subroutine M3(fn,thf,thw,depth,width)
c this program is to generate a dat file for all ibeam
c cross section.      
      character fn*13
      double precision x(41),y(41)
      nfactor=1.00
      x1=thw/2.0
      x2=width/2.0
      y1=depth/2.0-thf
      y2=depth/2.0
c *********************8      
      x(1)=0.0
      x(2)=x1/2.0
      x(3)=x1
      x(9)=x1+(x2-x1)/2.0
      x(10)=x2
      x(4)=x(1)
      x(5)=x(3)
      x(6)=x(1)
      x(7)=x(2)
      x(8)=x(3)
      x(11)=x(1)
      x(12)=x(3)
      x(13)=x(10)
      x(14)=x(1)
      x(15)=x(2)
      x(16)=x(3)
      x(17)=x(9)
      x(18)=x(10)
      y(1)=0.0
      y(4)=y1/2.0
      y(6)=y1
      y(11)=y1+(y2-y1)/2.0
      y(14)=y2
      y(2)=y(1)
      y(3)=y(1)
      y(5)=y(4)
      y(7)=y(6)
      y(8)=y(6)
      y(9)=y(6)
      y(10)=y(6)
      y(12)=y(11)
      y(13)=y(11)
      y(15)=y(14)
      y(16)=y(14)
      y(17)=y(14)
      y(18)=y(14)
      open(unit=1,file=fn,status='unknown')
      write(1,*) '3 18 1 2'
      do i=1,18
      write(1,*) x(i),y(i)
      end do
      write(1,*) '1 0 2 0 0'
      write(1,*) '2 3 0 0 1'
      write(1,*) '3 0 0 2 0'
      write(1,*) '1  8 15  3  5  8  7  6  4 1 2'
      write(1,*) '2  8  8  8 12 16 15 14 11 6 7'
      write(1,*) '3 15  8 10 13 18 17 16 12 8 9'
      close(unit=1)
      return
      end

C *********************************************************
C MESHING OF CROSS SECTION 9
C *********************************************************
C
      subroutine M9(fn,thf,thw,depth,width)
c this program is to generate a dat file for all ibeam
c cross section.      
      character fn*13
      double precision x(41),y(41)
	  print *, '2x'
      nfactor=1.00
      x1=thw
      x2=width
      y1=depth
      y2=depth
c *********************8      
      x(1)=0.0
      x(2)=((width/2.)-(thw/2.0))/2.0
      x(3)=(width/2.)-(thw/2.0)
      x(4)=width/2.0
      x(5)=x(3)+thw
      x(6)=x(5)+x(2)
      x(7)=width
      x(8)=x(1)
      x(9)=x(3)
      x(10)=x(5)
      x(11)=x(7)
      x(12)=x(1)
      x(13)=x(2)
      x(14)=x(3)
      x(15)=x(4)
      x(16)=x(5)
      x(17)=x(6)
      x(18)=x(7)
      x(19)=x(3)
      x(20)=x(5)
      x(21)=x(1)
      x(22)=x(2)
      x(23)=x(3)
      x(24)=x(4)
      x(25)=x(5)
      x(26)=x(6)
      x(27)=x(7)
      x(28)=x(1)
      x(29)=x(3)
      x(30)=x(5)
      x(31)=x(7)
      x(32)=x(1)
      x(33)=x(2)
      x(34)=x(3)
      x(35)=x(4)
      x(36)=x(5)
      x(37)=x(6)
      x(38)=x(7)
      y(1)=0.0
      y(8)=thf/2.0
      y(12)=thf
      y(19)=depth/2.0
      y(21)=depth-thf
      y(28)=y(21)+thf/2.0
      y(32)=depth
      y(2)=y(1)
      y(3)=y(1)
      y(4)=y(1)
      y(5)=y(1)
      y(6)=y(1)
      y(7)=y(1)
      y(9)=y(8)
      y(10)=y(8)
      y(11)=y(8)
      y(13)=y(12)
      y(14)=y(12)
      y(15)=y(12)
      y(16)=y(12)
      y(17)=y(12)
      y(18)=y(12)
      y(20)=y(19)
      y(22)=y(21)
      y(23)=y(21)
      y(24)=y(21)
      y(25)=y(21)
      y(26)=y(21)
      y(27)=y(21)
      y(29)=y(28)
      y(30)=y(28)
      y(31)=y(28)
      y(33)=y(32)
      y(34)=y(32)
      y(35)=y(32)
      y(36)=y(32)
      y(37)=y(32)
      y(38)=y(32)
      open(unit=1,file=fn,status='unknown')
      write(1,*) '7 38 1 2'
      do i=1,38
      write(1,*) x(i),y(i)
      end do
      write(1,*) '1 0 2 0 0'
      write(1,*) '2 0 3 4 1'
      write(1,*) '3 0 0 0 2'
      write(1,*) '4 2 0 6 0'
      write(1,*) '5 0 6 0 0'
      write(1,*) '6 4 7 0 5'
      write(1,*) '7 0 0 0 6'
      write(1,*) '1  6 14  1  2  3  9 14 13 12  8'
      write(1,*) '2  6  6  3  4  5 10 16 15 14  9'
      write(1,*) '3  6 14  5  6  7 11 18 17 16 10'
      write(1,*) '4 18  6 14 15 16 20 25 24 23 19'
      write(1,*) '5  6 14 21 22 23 29 34 33 32 28'
      write(1,*) '6  6  6 23 24 25 30 36 35 34 29'
      write(1,*) '7  6 14 25 26 27 31 38 37 36 30'
      close(unit=1)
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Mesh-4
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE M4(FNAME,dim)
      double precision X(21),Y(21)
      CHARACTER FNAME*13,dim*13
      OPEN(UNIT=1,FILE=FNAME,STATUS='UNKNOWN')
      PRINT 1,' Enter the width of the steel (m) :'
      READ(*,*) STEELW
      PRINT 1,' Enter the height of the steel (m):'      
      READ(*,*) STEELH
      PRINT 1,' Enter the thickness of the steel (m):'      
      READ(*,*) STEELT
1     FORMAT(A)      
      open(unit=2,file=dim,status='unknown')
      write(2,*) 'Concrete filled steel'
      write(2,*) 'Filename :',dim
      write(2,*) 'Width steel,m     :',steelw
      write(2,*) 'Depth steel,m     :',steelh
      write(2,*) 'Thickness steel,m :',steelt
      close(unit=2)
c      
      X1=STEELW/2.0-STEELT
      X2=STEELW/2.0
      Y1=STEELH/2.0-STEELT
      Y2=STEELH/2.0
      WRITE(1,*) '4 21 1 2'
      WRITE(1,*) X1
      WRITE(1,*) X2
      WRITE(1,*) Y1
      WRITE(1,*) Y2 
C X-coordinate
      X(1)=0.0
      X(2)=X1/2.0
      X(3)=X1
      X(4)=X1+(X2-X1)/2.0
      X(5)=X2
      X(6)=X(1)
      X(9)=X(1)
      X(14)=X(1)
      X(17)=X(1)
      X(10)=X(2)
      X(18)=X(2)
      X(7)=X(3)
      X(11)=X(3)
      X(15)=X(3)
      X(19)=X(3)
      X(12)=X(4)
      X(20)=X(4)
      X(8)=X(5)
      X(13)=X(5)
      X(16)=X(5)
      X(21)=X(5)
C Y-coordinate
      Y(1)=0.0
      Y(6)=Y1/2.0
      Y(9)=Y1
      Y(14)=Y1+(Y2-Y1)/2.0
      Y(17)=Y2
      Y(2)=Y(1)
      Y(3)=Y(1)
      Y(4)=Y(1)
      Y(5)=Y(1)
      Y(7)=Y(6)
      Y(8)=Y(6)
      Y(10)=Y(9)
      Y(11)=Y(9)
      Y(12)=Y(9)
      Y(13)=Y(9)
      Y(15)=Y(14)
      Y(16)=Y(14)
      Y(18)=Y(17)
      Y(19)=Y(17)
      Y(20)=Y(17)
      Y(21)=Y(17)
C
      DO IC=1,21
      WRITE(1,*) X(IC),Y(IC)
      END DO
      WRITE(1,*) '1 3 2 0 0'
      WRITE(1,*) '2 4 0 0 1'
      WRITE(1,*) '3 0 4 1 0'
      WRITE(1,*) '4 0 0 2 3'
      WRITE(1,*) '1  5  8   9 10 11 15 19 18 17 14'
      WRITE(1,*) '2  5  5  11 12 13 16 21 20 19 15'
      WRITE(1,*) '3  8  8   1  2  3  7 11 10  9  6'
      WRITE(1,*) '4  8  5   3  4  5  8 13 12 11  7'
      close(unit=1)
      return
      end        
c Meshing for column Number 5     
      subroutine m5(fname,thick,h2m,h4m,h2a,h4a,w)
      character*13 fname,dim
      character*1 yn
      real x(8),y(8)
      COMMON /DIME/ DIM
      open(unit=1,file=fname,status='unknown')
      write(1,*) '1 8 1 2'
	print *,'Enter your selection'
	print *,'--------------------'
      print 9,' Yes - new dimension, No - sample dimension (y/n):'
      READ(*,'(A1)') yn
      if(yn.eq.'Y'.or.yn.eq.'y') then
       PRINT 9, ' Enter the thickness (meter) :'
       READ(*,*) THICK
       PRINT 9, ' Enter ibeam height (meter) :'
       READ(*,*) H2M
       PRINT 9, ' Enter ibeam width (meter) :'
       READ(*,*) H4M
       H2A=0.0
       H4A=0.0
       PRINT 9, ' Enter web width (meter) :'
       READ(*,*) W
9      FORMAT(A)      
      else 
       print *,'Use the sample dimension'
1      thick=0.043
       h2m=0.232
       h4m=0.211
       h2a=0.0
       h4a=0.0
       w=0.034
       write(*,*) 'Flange :',thick
       write(*,*) 'Depth :',h2m
       write(*,*) 'Width :',h4m
       write(*,*) 'Web   :',w
       write(*,*) 'Vertical concrete from ibeam :',h2a
       write(*,*) 'Horizontal concrete from ibeam :',h4a
      endif
      open(unit=2,file=dim,status='unknown')
      write(2,*) 'Concrete + Ibeam'
      write(2,*) 'Filename :',dim
      write(2,*) 'Flange :',thick
      write(2,*) 'Depth :',h2m
      write(2,*) 'Width :',h4m
      write(2,*) 'Web   :',w
      write(2,*) 'Vertical concrete from ibeam :',h2a
      write(2,*) 'Horizontal concrete from ibeam :',h4a
      close(unit=2)
      x1=h4m/2.0
      y1=h2m/2.0
      x(1)=0.0
      x(2)=x1/2.0
      x(3)=x1
      x(4)=x(1)
      x(6)=x(1)
      x(7)=x(2)
      x(5)=x(3)
      x(8)=x(3)
      y(1)=0.0
      y(4)=y1/2.0
      y(6)=y1
      y(2)=y(1)
      y(3)=y(1)
      y(5)=y(4)
      y(7)=y(6)
      y(8)=y(6)
      do i=1,8
      write(1,*) x(i),y(i)
      end do
      write(1,*) '1 0 0 0 0'
      write(1,*) '1 20 20 1 2 3 5 8 7 6 4'
      close(unit=1)
      return
      end
C
C Meshing for column Number 6
C
      subroutine m6(fname,h2m,h4m,dim)
      character*13 fname,dim
      character*1 yn
      real x(8),y(8)
      open(unit=1,file=fname,status='unknown')
      write(1,*) '1 8 1 2'
	print *,'Enter your selection'
	print *,'--------------------'
      print 9,' Yes - new dimension, No - sample dimension (y/n):'
      READ(*,'(A1)') yn
      if(yn.eq.'Y'.or.yn.eq.'y') then
	 Thick=0.0
       PRINT 9, ' Enter Concrete height (meter) :'
       READ(*,*) H2M
       PRINT 9, ' Enter Concrete width (meter) :'
       READ(*,*) H4M
       H2A=0.0
       H4A=0.0
	 W=0.0
9      FORMAT(A)      
      else 
       print *,'Use the sample dimension'
1      thick=0.0
       h2m=0.232
       h4m=0.211
       h2a=0.0
       h4a=0.0
       w=0.0
       write(*,*) 'Depth :',h2m
       write(*,*) 'Width :',h4m
      endif
      open(unit=2,file=dim,status='unknown')
      write(2,*) 'Concrete + Ibeam'
      write(2,*) 'Filename :',dim
      write(2,*) 'Depth :',h2m
      write(2,*) 'Width :',h4m
      close(unit=2)
      x1=h4m/2.0
      y1=h2m/2.0
      x(1)=0.0
      x(2)=x1/2.0
      x(3)=x1
      x(4)=x(1)
      x(6)=x(1)
      x(7)=x(2)
      x(5)=x(3)
      x(8)=x(3)
      y(1)=0.0
      y(4)=y1/2.0
      y(6)=y1
      y(2)=y(1)
      y(3)=y(1)
      y(5)=y(4)
      y(7)=y(6)
      y(8)=y(6)
      do i=1,8
      write(1,*) x(i),y(i)
      end do
      write(1,*) '1 0 0 0 0'
	print *,'Enter number of columns and rows (default 20 20):'
	read(*,*) icolxx,irowxx
      write(1,*) '1 ',icolxx,irowxx,' 1 2 3 5 8 7 6 4'
      close(unit=1)
      return
      end
C
C Meshing for column Number 10
C
      subroutine m10(fname,h2m,h4m,dim)
      character*13 fname,dim
      character*1 yn
      real x(8),y(8)
      open(unit=1,file=fname,status='unknown')
      write(1,*) '1 8 1 2'
	print *,'Enter your selection'
	print *,'--------------------'
      print 9,' Yes - new dimension, No - sample dimension (y/n):'
      READ(*,'(A1)') yn
      if(yn.eq.'Y'.or.yn.eq.'y') then
	 Thick=0.0
       PRINT 9, ' Enter Concrete height (meter) :'
       READ(*,*) H2M
       PRINT 9, ' Enter Concrete width (meter) :'
       READ(*,*) H4M
       H2A=0.0
       H4A=0.0
	 W=0.0
9      FORMAT(A)      
      else 
       print *,'Use the sample dimension'
1      thick=0.0
       h2m=0.232
       h4m=0.211
       h2a=0.0
       h4a=0.0
       w=0.0
       write(*,*) 'Depth :',h2m
       write(*,*) 'Width :',h4m
      endif
      open(unit=2,file=dim,status='unknown')
      write(2,*) 'Concrete + Ibeam'
      write(2,*) 'Filename :',dim
      write(2,*) 'Depth :',h2m
      write(2,*) 'Width :',h4m
      close(unit=2)
      x1=h4m
      y1=h2m
      x(1)=0.0
      x(2)=x1/2.0
      x(3)=x1
      x(4)=x(1)
      x(6)=x(1)
      x(7)=x(2)
      x(5)=x(3)
      x(8)=x(3)
      y(1)=0.0
      y(4)=y1/2.0
      y(6)=y1
      y(2)=y(1)
      y(3)=y(1)
      y(5)=y(4)
      y(7)=y(6)
      y(8)=y(6)
      do i=1,8
      write(1,*) x(i),y(i)
      end do
      write(1,*) '1 0 0 0 0'
	print *,'Enter number of columns and rows (default 20 20):'
	read(*,*) icolxx,irowxx
      write(1,*) '1 ',icolxx,irowxx,' 1 2 3 5 8 7 6 4'
      close(unit=1)
      return
      end



C ****************************************************************
C
	SUBROUTINE CYLSTEEL(ELEF,CRDF)
	integer inum
	parameter (inum=3000)
	double precision x(inum),y(inum),radius,deff1,steelthick,
     .deff2,deff
	integer iel(inum),jel(inum),kel(inum)
	character ele*4,crd*4,elef*12,crdf*12,fname*12,blank*1
	character outf*12,out*4,dimf*12,dim*4
	parameter(blank=' ')
	print *,'Meshing a cylindrical concrete-filled steel column'
	print *,'-------------------------------------------------'
	print *,'Enter the new filename without extension, max 8 char.'
	print *,'-------------------------------------------------'
	read(*,'(a12)') fname
	dim='.dim'
       ele='.ele'
	crd='.crd'
	out='.mct'
	nfirst=1
      do next=1,10
	if(fname(next:next).eq.blank) then
	    mnext=next-1
	    elef=fname(nfirst:mnext)//ele
	    crdf=fname(nfirst:mnext)//crd 
	    outf=fname(nfirst:mnext)//out
	    dimf=fname(nfirst:mnext)//dim
	    go to 2
	endif
      end do
2	continue
	open(unit=1,file=outf,status='unknown')
	open(unit=2,file=elef,status='unknown')
	open(unit=3,file=crdf,status='unknown')
	open(unit=4,file=dimf,status='unknown')
	x(1)=0.0d0
	y(1)=0.0d0
	pi=3.141592654d0      ! pi
	node=2                ! starting node number
c ***************************
c Input datafile
c ***************************
 	print *,'-------------------------------------------------'
	print *,'Enter the radius of the cross section (mm)'
	print *,'-------------------------------------------------'
	read(*,*) radius
	print *,'-------------------------------------------------'
	print *,'Enter the steel thickness (mm)'
	print *,'-------------------------------------------------'
	read(*,*) steelthick
	if(radius.lt.1.0) then
	print *,'Radius is too small',radius,'mm'
	return
	endif
	if(steelthick.lt.1.0) then
	print *,'Steel thickness is too small',steelthick,'mm'
	return
	endif
	if(steelthick.gt.radius) then
	print *,'Steel thickness is greater than radius'
	return
	endif
	radius=radius/1000.0                ! maximum radius
	steelthick=steelthick/1000.0	 ! steel thickness
	ncon=14                             ! division for concrete section
	nstl=2					 ! division for steel section
	nsec=30				 ! integer of division (integer)
c *****************************************************
	write(4,*) 'Dimension characteristic'
	write(4,*) 'Filename :',elef,crdf
	write(4,*) 'Radius (m)         =',radius
	write(4,*) 'Steel thickness (m)=',steelthick
c *****************************************************
	secd=nsec*1.0				 ! division of one quarter section	(integer)
	nttl=ncon+nstl
	deg=90.0/secd
	deff1=(radius-steelthick)/(ncon*1.0d0)
	deff2=steelthick/(nstl*1.0d0)
      write(1,*) 'x(node),y(node),iangle,node,irad'
	write(1,*) x(1),y(1),'  1'
	write(3,*) '1 ', x(1),y(1),' 0  0'
      xrad=0.0d0
	do irad=1,nttl
        if(irad.le.ncon) deff=deff1
        if(irad.gt.ncon) deff=deff2
        xrad=xrad+deff
	  angled=0.0
	  do iangle=1,10000
	     if(angled.gt.90.0) goto 5
	     angle=angled*pi/180.0d0
	     x(node)=xrad*cos(angle)
	     if(abs(x(node)).lt.0.00001) x(node)=0.0d0
	     y(node)=xrad*sin(angle)
	     write(1,1) x(node),y(node),iangle,node,irad
1	     format(1x,2f15.8,1x,i3,i5,i3)
c
	     rnd=sqrt(x(node)**2+y(node)**2)
	     bcg=radius-deff2/2.0
	     nbnd=0
	     if(rnd.gt.bcg) nbnd=1
c
	     ndumy=0
	     write(3,18) node,x(node),y(node),ndumy,nbnd
   18      format(1x,i4,2(f25.12,1x),i4,1x,i4)
	     node=node+1
	     angled=deg+angled
	  end do
5	end do
	write(1,*) 'Element numbering'
c Element numbering
c for the first part
	do nel=1,nsec
	 iel(nel)= 1
	 jel(nel)= 1+nel
	 kel(nel)= 2+nel
	 write(1,*) nel,iel(nel),jel(nel),kel(nel)
	 write(2,*) nel,iel(nel),jel(nel),kel(nel),' 0 0 0 1'
	end do
	write(1,*) 'Next layer 1'
c for the second part
	nel=nsec
	nstart=2
      do ilayer=1,nttl-1
	 do isec=1,nsec
	  nel=nel+1
	  iel(nel)= nstart
	  jel(nel)= nstart+nsec+1
	  kel(nel)= nstart+nsec+2
	  write(1,*) nel,iel(nel),jel(nel),kel(nel)
c to determine the type of material
	  if(ilayer.gt.ncon) then 
	  mat=2
	  else
	  mat=1
	  endif
c to determine the boundary
	  nnbi=iel(nel)
	  nnbj=jel(nel)
	  nnbk=kel(nel)
	  xi=x(nnbi)
	  xj=x(nnbj)
	  xk=x(nnbk)
	  yi=y(nnbi)
	  yj=y(nnbj)
	  yk=y(nnbk)
	  r1=sqrt(xi**2+yi**2)
	  r2=sqrt(xj**2+yj**2)
	  r3=sqrt(xk**2+yk**2)
	  bcg=radius-deff2/2.0
	  nb1=0
	  nb2=0
	  nb3=0
	  if(r1.gt.bcg.and.r2.gt.bcg) nb1=1
	  if(r2.gt.bcg.and.r3.gt.bcg) nb2=1
	  if(r3.gt.bcg.and.r1.gt.bcg) nb3=1
	  write(2,3) nel,iel(nel),jel(nel),kel(nel),nb1,nb2,nb3,mat
3	  format(1x,8(i4,1x))
	  nel=nel+1
	  iel(nel)= nstart
	  jel(nel)= nstart+nsec+2
	  kel(nel)= nstart+1
	  write(1,*) nel,iel(nel),jel(nel),kel(nel)
c to determine the boundary
	  nnbi=iel(nel)
	  nnbj=jel(nel)
	  nnbk=kel(nel)
	  xi=x(nnbi)
	  xj=x(nnbj)
	  xk=x(nnbk)
	  yi=y(nnbi)
	  yj=y(nnbj)
	  yk=y(nnbk)
	  r1=sqrt(xi**2+yi**2)
	  r2=sqrt(xj**2+yj**2)
	  r3=sqrt(xk**2+yk**2)
	  bcg=radius-deff2/2.0
	  nb1=0
	  nb2=0
	  nb3=0
	  if(r1.gt.bcg.and.r2.gt.bcg) nb1=1
	  if(r2.gt.bcg.and.r3.gt.bcg) nb2=1
	  if(r3.gt.bcg.and.r1.gt.bcg) nb3=1
	  write(2,3) nel,iel(nel),jel(nel),kel(nel),nb1,nb2,nb3,mat
	  nstart=nstart+1
	 end do
	 nstart=kel(nel)+1
	 write(1,*) 'Next layer',ilayer+1
	end do
	print *,'Element filename   : ',elef
	print *,'Coordinate filename: ',crdf
	CLOSE(UNIT=1)
	CLOSE(UNIT=2)
	CLOSE(UNIT=3)
	CLOSE(UNIT=4)
	PRINT *, ELEF,CRDF
	RETURN
	end
