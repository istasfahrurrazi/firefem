	subroutine tempro(time,fire,timex,firex,imax)
	double precision time,fire,timex(1000),firex(1000)


	do i=1,imax-1

	if(time.ge.timex(i).and.time.lt.timex(i+1)) then
	fire=firex(i)+(firex(i+1)-firex(i))*(time-timex(i))/
     .(timex(i+1)-timex(i))
	return
	else
	fire=100.0
	endif

	end do

	return
	end
