module getParams !gets the parameters from the commandline
    implicit none
    integer, public :: runs
    integer, private :: stat
    character (len=10), public:: save2File, walkType, temp
    
    contains
    subroutine getVals !gives values to variables
        Call get_command_argument(1,temp, status=stat)
        if (stat==0) then
            read(temp,*, iostat=stat) runs
            if (stat/=0) then
                print *, "1st commandline argument was not an Integer!"
            endif
            Call get_command_argument(2,save2File,status=stat)
            if (stat==0) then
                Call get_command_argument(3,walkType, status=stat)
                if (stat/=0) then
                    print*,"3rd commandline argument not found!"
                    print*,"1st parameter number of runs (integer), 2nd save results to file(Y/N), 3rd walk type (walk/SAW)"
                else
                    call correctTypes
                endif
            else
                print*,"2nd commandline argument not found!"
                print*,"1st parameter number of runs (integer), 2nd save results to file(Y/N), 3rd walk type (walk/SAW)"
            endif
        else
            print*,"There are no commandline parameters!"
            print*,"1st parameter number of runs (integer), 2nd save results to file(Y/N), 3rd walk type (walk/SAW)"
        endif
    end subroutine getVals
    
    subroutine correctTypes !checks if variables contain the right kind of arguments
        if (save2File/="Y".and.save2File/=("N")) then
            print*,"The second commandline argument has to be either Y or N!"
        endif

        if (walkType/="walk".and.walkType/="SAW") then
            print*,"The third commandline argument has to be either walk or SAW!"
        endif
    end subroutine correctTypes

end module getParams