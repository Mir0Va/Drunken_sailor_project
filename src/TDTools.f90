module TDTools !time and distance tools 
    implicit none
    
contains
    function min2time(time) result(retval)!returns given minutes to relevant time units in array 
        integer, intent(in) :: time
        integer :: retval(4) !1.years,2.days,3.hours,4.minutes
        retval(1)=floor(real(time)/(365*24*60))
        retval(2)=floor((real(time)-retval(1)*(365*24*60))/(24*60))
        retval(3)=floor((real(time)-retval(1)*(365*24*60)-retval(2)*(24*60))/(60))
        retval(4)=floor(real(time)-retval(1)*(365*24*60)-retval(2)*(24*60)-retval(3)*60)
    end function min2time

    function dist2km(dist) result(retval)
        integer, intent(in) :: dist 
        integer :: retval
        retval=dist/1000   
    end function dist2km

end module TDTools 