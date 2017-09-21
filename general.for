*****************************************************************************
*                                                                           *
*      ON THE NAME OF THE GOD, THE MOST GRACIOUS AND THE MOST MERCIFUL      *
*                                                                           *
*****************************************************************************
*                                                                           *
*           FINITE ELEMENT MODELING OF ESTIMATING STRENGTH OF               *
*       ENGINEERING COMPONENT UNDER HIGH TEMPERATURE, TIME DEPENDENT.       *
*                                                                           *
*****************************************************************************
*                                                                           *
*   DEVELOPED BY ISTAS FAHRURRAZI BIN NUSYIRWAN                             *
*                                                                           *
*****************************************************************************
*                                                                           *
* FORTRAN COMPILER - MICROSOFT FORTRAN POWERSTATION 4.0                     *
* NOTE :-                                                                   *
*                                                                           *
*****************************************************************************
c	USE MSFLIB
	CHARACTER*2 NS
      INTEGER(2) RESULT
C START
1     PRINT *, '  BY THE NAME OF THE GOD, THE MOST GRACIOUS AND    '
      PRINT *, '                 MOST MERCIFUL                     '
	PRINT *, ' '
	PRINT *, 'THIS IS AN EVALUATION PROGRAM OF EVALUATING '
	PRINT *, 'THE FIRE RESISTANCE OF COLUMNS '
	PRINT *, 'WRITTEN BY ISTAS FAHRURRAZI NUSYIRWAN -',
     .'(istaz@fkm.utm.my)'
	PRINT *, 'FAKULTI KEJURUTERAAN MEKANIKAL'
	PRINT *, 'UNIVERSITI TEKNOLOGI MALAYSIA'
	PRINT *, '25 AUGUST 1998'
	PRINT *, 'MAX ALLOWABLE LOAD - WRITTEN BY HISHAMUDDIN ALHAM'
	PRINT *,'STRENGTH SUBROUTINE - WRITTEN BY BADRI ABDUL GHANI'
	PRINT *,'GET THE MANUAL FROM HTTP://FKM.UTM.MY/~ISTAZ/'
	PRINT *,'--------------------------------------------'
	PRINT *,' WELCOME TO THE FINITE ELEMENT HEAT TRANSFER'
	PRINT *,' COMPUTER MODELLING BETA VERSION 1.1'
	PRINT *,'--------------------------------------------'
	PRINT *,' TYPE ALT-ENTER FOR FULL-SCREEN'
	PRINT *,' PLEASE CHOOSE INSTRUCTIONS BELOW :'
	PRINT *,' (FOLLOW THE SEQUENCE FOR NEW ANALYSIS)'
	PRINT *,' '
	PRINT *,' (1) CHOOSE YOUR CROSS SECTION AND MESH IT'
	PRINT *,' (2) CALCULATING THE TEMPERATURE AND STRENGTH'
	PRINT *,' (3) LOOKING THE TEMPERATURE DISTRIBUTION'
	PRINT *,' (4) SORTING THE NODE TEMPERATURE'
	PRINT *,' (5) PRINT THE RESULTS (TEMPERATURE OR STRENGTH)'
	PRINT *,' (6) VIEW THE STRENGTH CURVE'
	PRINT *,' (7) CALCULATE MAXIMUM ALLOWABLE LOAD',
     .' FOR H-STEEL SECTION ENCASED IN CONCRETE'
	PRINT *,' (8) EXIT'
	PRINT *,' (9) REMARK ON THIS PROGRAM'
	PRINT *,' '
	PRINT *,'     PLEASE ENTER YOUR SELECTION (1-9) :  '
	READ(*,'(A2)') NS
      IF(NS.EQ.'1') THEN
	call MESHV
	GOTO 1
	ELSEIF(NS.EQ.'2') THEN
	CALL FIN92
	GOTO 1
	ELSEIF(NS.EQ.'3') THEN
C	CALL TEMPDIST
	GOTO 1
	ELSEIF(NS.EQ.'4') THEN
C	CALL SORT
	GOTO 1
	ELSEIF(NS.EQ.'5') THEN
C	CALL PRINTG
	GOTO 1
	ELSEIF(NS.EQ.'6') THEN
C	CALL STRPLT
	GOTO 1
	ELSEIF(NS.EQ.'7') THEN
C	CALL MAX
	GOTO 1
	ELSEIF(NS.EQ.'9') THEN
	PRINT *,'-THIS PROGRAM IS WRITTEN BY ISTAS FAHRURRAZI.'
	PRINT *,' (istaz@fkm.utm.my)'
	PRINT *,'-THE MAXIMUM ALLOWABLE LOAD PROGRAM IS WRITTEN BY'
	PRINT *,' MR. HISHAMUDDIN ALHAM.'
	READ(*,*)
	GOTO 1
	ELSEIF(NS.EQ.'8') THEN
	print *, 'conti'
	ELSE
	GOTO 1
	ENDIF
3	FORMAT(A)
	PRINT *,'         ****************************************'
	PRINT *,'                     Written by Istaz''97'
	PRINT *,'             Thank you for using this program'
	PRINT *,'                   May GOD Bless Us All'
	PRINT *,'         ****************************************'
	write(*,*) '                  Press <ENTER> to exit'
c	RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
	END
	
