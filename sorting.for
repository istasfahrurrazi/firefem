	SUBROUTINE SORT
      character*12 fname,c,f
      character*60 e
	integer*4 ierr
      character*8 dum
42	continue
	print *,'-----------------------------------------------'
	print *,'This program is to sort the nodal temperature'
	print *,'from the main output file for use in EXCEL MS. '
	print *,'-----------------------------------------------'
44    print *,'Enter your OUTPUT filename (with extension OUT)'
      read(*,'(a12)') fname
      OPEN(UNIT=1,FILE=fname,STATUS='OLD',iostat=ierr)
	if(ierr.ne.0) then
	print *,'-----------------------------------------------'
	print *,'File not found, enter the valid filename'
	write(*,*) 'Press enter'
	goto 42
	endif
      print *,'Enter your output data (extension NDE):'
      read(*,'(a12)') f                   
      print *,'Enter your remark:'
      read(*,'(a60)') e
      open(unit=2,file=f,status='unknown')
      write(2,*) 'for file output named :',fname
      write(2,*) 'Remark :',e
      write(2,*) '------------------------------------------'
	write(2,*) 'Time(Min)   Nodal Temp.    Fire Temp.'
	write(2,*) '            (deg.Cel.)     (deg.Cel.)   node'
      write(2,*) '------------------------------------------'
10    print *,'Enter the nodal value to see'
      read(*,*) iix
      print *,'Node : ',iix               
      rewind 1        
      do 20 i=1,1000
      read(1,'(a8)') dum        
20    if(dum.eq.' FIRE  &') goto 30
c first attempt.
30	PRINT *,'Start sorting...'
      do while(.true.)
      read(1,*,end=5,iostat=ierr) firetemp,time1,node,x,y,ti,tt
	if(ierr.ne.0) goto 41
      if(node.eq.iix) then
      write(2,43) time1,ti,firetemp,node,x,y
      write(*,40) firetemp,time1,node,x,y,ti
	xn=x
	yn=y
40    format(1x,f10.2,1x,f6.1,1x,i3,1x,f8.5,1x,f8.5,1x,f10.3)      
43    format(1x,f6.1,1x,f10.2,1x,f10.2,1x,i4,1x,f10.2,1x,f10.2)
      endif
41    end do       
c done 
5     continue
      print *,'stop ? ( Yes / No or <ENTER> / neW)'
      read(*,'(a12)') c
	if(c.eq.'W'.or.c.eq.'w') goto 44
      if(c.eq.'Y'.or.c.eq.'y') then
	goto 999
	else
	goto 10
	endif
999   write(2,*) 'Written by Istas Fahrurrazi'
      write(2,*) 'in support for his MSc. project'
      write(2,*) 'date 1 August 1997'
	RETURN
      end
