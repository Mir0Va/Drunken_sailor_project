module arrayTools !contains tools for array handling
    use mtmod
    implicit none

contains
    function containsLocation(arr,  x, y) result(tf)!checks if a certain point exists in array
        integer, intent(in) :: arr(:),x, y !here x and y are the checked positions
        integer :: i,tf !index and true/false
        tf=0
       
        do i=1,size(arr),2
            if (arr(i)==x.and.arr(i+1)==y) then
                tf=1 !returns 1 if array contains location, otherwise returns 0                
            endif
        enddo 

    end function containsLocation

    function availableDir(arr,x,y) result(retArr) !checks what directions are avilable
        integer, intent(in) :: arr(:), x, y !here x and y are current positions
        integer :: retArr(4)
        retArr=0
        if (containsLocation(arr,x+1,y)==0) then
            retArr(1)=1
        endif
        if (containsLocation(arr,x,y+1)==0) then
            retArr(2)=2
        endif
        if (containsLocation(arr,x-1,y)==0) then
            retArr(3)=3
        endif
        if (containsLocation(arr,x+1,y)==0) then
            retArr(4)=4
        endif
    end function availableDir

    function removeZeros(arr) result(retArr) !returns array w/o zeros
        integer, intent(in) :: arr(:)
        integer, allocatable :: retArr(:)
        integer :: i,j,zeros
        zeros=0
        j=1

        do i=1,size(arr)
            if (arr(i)==0) then
                zeros = zeros + 1
            endif
        enddo

        allocate(retArr(size(arr)-zeros))

        do i=1,size(arr)
            if (arr(i)/=0) then
                retArr(j)=arr(i)
                j=j+1
            endif
        enddo

    end function removeZeros

    function randValList(arr) result(retval) !returns random value from list
        integer, intent(in) :: arr(:)
        integer :: retval,i
        real(kind=8) :: rand 
        rand = grnd()
        i = 1 + floor(rand*size(arr))
        retval = arr(i)

    end function randValList

end module arrayTools