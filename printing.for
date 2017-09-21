c This subroutine functions to print the result to the
c active printer
	subroutine printg
	character*12 filestg,filetemp,dum
	character*1 a,c
	character*60 e
	integer(2) result
c	result=RUNQQ('dir','/p')
	write(*,*) '------------------------------------'
	write(*,*) '!! Make sure the printer is on.'
	write(*,*) '------------------------------------'
	write(*,*) 'Enter value to print :'
	write(*,*) '   '
	write(*,*) '(1) Temperature value'
	write(*,*) '(2) Strength value'
	PRINT *,'(Q) TO RETURN TO MAIN MENU'
	read(*,'(a1)') a
	IF(A.EQ.'Q'.OR.A.EQ.'qt') RETURN
c	print *, a
c	stop
	open(unit=4,file='prn',iostat=ie)
	if(ie.gt.0) then
	write(*,*) 'The printer is not detected. Press <ENTER>'
	goto 9999
	endif
	if(a.eq.'1') then
44	 print *, 'Enter the filename :'
	 read(*,'(a12)') filetemp
	 open(UNIT=1,FILE=filetemp,STATUS='OLD',iostat=ie)
	 if(ie.gt.0) then
	 print *,'File not found'
	 stop
	 endif
       print *,'Enter your remark:'
       read(*,'(a60)') e
10     print *,'Enter the nodal value to see'
       read(*,*) iix
       print *, iix               
	 write(4,*) 'Sorting of nodal temperature of node,',iix      
       write(4,*) 'for file output named :',filetemp
       write(4,*) 'Remark :',e
       write(4,*) '------------------------------------------'
	 write(4,*) 'Time(Min)   Nodal Temp.    Fire Temp.'
	 write(4,*) '            (deg.Cel.)     (deg.Cel.)'
       write(4,*) '------------------------------------------'
       rewind 1        
       do 20 i=1,1000
       read(1,'(a8)') dum        
20     if(dum.eq.' FIRE  &') goto 30
c first attempt.
30	 PRINT *,'Start sorting...'
       do while(.true.)
       read(1,*,end=5,iostat=ierr) firetemp,time1,node,x,y,ti,tt
	 if(ierr.ne.0) goto 41
       if(node.eq.iix) then
       write(4,43) time1,ti,firetemp
       write(*,40) firetemp,time1,node,x,y,ti
	 xn=x
	 yn=y
40     format(1x,f10.2,1x,f6.1,1x,i3,1x,f8.5,1x,f8.5,1x,f10.3)      
43     format(1x,f6.1,1x,f10.2,5x,f10.2)
       endif
41     end do       
c done 
5      write(4,*) '--------------------------------------------'
	 write(4,50) 'Node (',iix,') = (',xn,',',yn,')'
50     format(1x,a,i5,a,f9.4,a,f9.4,a)
	 write(4,*) 'unit meter'
       print *,'stop ? (Yes/No/neW)'
       read(*,'(a1)') c
	 if(c.eq.'W'.or.c.eq.'w') goto 44
       if(c.eq.'Y'.or.c.eq.'y') then
	 goto 9999
	 else
	 goto 10
	 endif
999    continue
	elseif(a.eq.'2') then
	 print *,'Enter the strength filename :'
	 read(*,'(a12)') filestg
	 print *,filestg
	 open(unit=2,file=filestg,status='old',iostat=ie)
	 if(ie.gt.0) then
	 print *,'File not found'
	 stop
	 endif
	 do istg=1,1000
	 read(2,'(a60)',end=9999) e
	 write(4,*) e
	 end do
	endif
9999	RETURN
      end
