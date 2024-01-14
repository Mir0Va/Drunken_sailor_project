module drunkenSailor
    use arrayTools
    use getParams
    use mtmod
    use TDTools
    implicit none
    integer :: ded,ship,shore,stuck,time,x,y,max,min !distance and time are the count of streets walked times different values
    integer, allocatable :: times(:), dists(:),hist(:)

contains
    function randWalk(arr,x,y) result(retArr)!given previous steps as an array, and current location as x and y
        integer, intent(in) :: arr(:),x,y
        integer :: retArr(2),retX,retY,move
        integer,allocatable :: dirs(:)!dirs are the possible directions, move is the movement
        retX=x
        retY=y
        move=0

        if (walkType=="SAW") then
            dirs=removeZeros(availableDir(arr,x,y))
            if (size(dirs)/=0) then
                move=randValList(dirs)
            endif
        else
            move=randValList((/1,2,3,4/))
        endif
        
        if (move/=0) then
            if (move==1) then
                retX=x+1
            elseif (move==2) then
                retY=y+1
            elseif (move==3) then
                retX=x-1
            elseif (move==4) then
                retY=y-1
            endif
        else
            retX=-1!if no directions available, retX will be set to -1, since x can never be <0 (shore)
        endif

        retArr(1)=retX;retArr(2)=retY
    end function randWalk


    subroutine leaveBarWalk() !1 run
        integer :: i,arr(2)
        x=10;y=10;hist(1:2)=(/x,y/);hist(3:)=0;time=0

        do i=1, 26280000
            arr=randWalk((/0/),x,y) !gets new coordinates
            x=arr(1);y=arr(2)
            time=time+1

            if (save2File=="Y") then
                write(30,*) x,y
            endif

            if (arr(1)==0) then
                if(time<600) then
                    ship=ship+1
                else
                    shore=shore+1
                endif
                exit
            endif
        enddo

        if (time>=26280000) then
            ded=ded+1
        endif
        if (time>max) then
            max=time
        endif
        if (time<min) then
            min=time
        endif

    end subroutine leaveBarwalk

    subroutine leaveBarSAW()
        integer :: i,arr(2)
        x=10;y=10;hist(1:2)=(/x,y/);hist(3:)=0;time=0
        do i=1, 600
            arr=randWalk(hist,x,y) !gets new coordinates
            time=time+1
            if (arr(1)/=-1) then !checks if not stuck
                hist(i*2+1:i*2+2)=arr
                x=arr(1);y=arr(2)
                if (save2File=="Y") then
                    write(30,*) x,y
                endif
            else
                stuck=stuck+1
                exit
            endif

            if (arr(1)==0) then
                if(time<600) then
                    ship=ship+1
                else
                    shore=shore+1
                endif
                exit
            endif
        enddo
        
        if (time>=600) then
            ded=ded+1 ! here ded is the value for not making it in time
        endif
        if (time>max) then
            max=time
        endif
        if (time<min) then
            min=time
        endif

    end subroutine leaveBarSAW

    subroutine start()
        integer :: seed,i,stat!formatted time
        call getVals
        seed=getseed() !gives the seed a "random" value                                            
        call sgrnd(seed) !gives mtmod a seed for its rng
        allocate(times(runs),dists(runs))
        allocate(hist(1202)) !2*600(from maximum time 10hrs->600min)+2(initial position)
        max=0; min=300000

        if (save2File=="Y") then
            open(30,file="output.dat",status='new',iostat=stat)
            if (stat/=0) then !if there is an old output.dat this deletes it
                open(30,file="output.dat",status='old')
                close(30,status="delete")
                open(30,file="output.dat",status='new',iostat=stat)
            endif
        endif

        if (walkType=="walk") then !for optimization reasons the if is not made inside the do-loop
            do i=1, runs
                call leaveBarwalk()
                times(i)=time;dists(i)=time !time times different values gives the time(60->min) and distance(1/10->km)
            enddo
        elseif (walkType=="SAW") then
            do i=1, runs
                call leaveBarSAW()
                times(i)=time;dists(i)=time
            enddo
        endif

        call printing()

        if (save2File=="Y") then
            close(30)
        endif

    end subroutine start

    subroutine printing() !all output information is printed to the terminal
        integer :: ary(4),avg
        print*,"Distance:"
        avg=sum(dists)/size(dists)/10
        if (avg<0) then
            avg=-avg
        endif
        print*,"Max:",max/10,"km   Min:",min/10,"km   Avg:",avg,"km"

        print*,"Time:"
        ary=min2time(max)
        print*,"Max:",ary(1),"yrs",ary(2),"days",ary(3),"hrs",ary(4),"mins"
        ary=min2time(min)
        print*,"Min:",ary(1),"yrs",ary(2),"days",ary(3),"hrs",ary(4),"mins"
        avg=sum(times)/size(times)
        if (avg<0) then
            avg=-avg
        endif
        ary=min2time(avg)
        print*,"Average:",ary(1),"yrs",ary(2),"days",ary(3),"hrs",ary(4),"mins"

        if (walkType=="SAW") then
            print*,"Sailors got to shore:", ship, ", Fraction:", real(ship)/runs
            print*,"Sailors that got to a dead end:", stuck, ", Fraction:", real(stuck)/runs
        elseif (walkType=="walk") then
            print*,"Sailors got to ship:", ship, ", Fraction:", real(ship)/runs
            print*,"Sailors got to shore:", shore, ", Fraction:", real(shore)/runs
            print*,"Sailors that died:", ded, ", Fraction:", real(ded)/runs
        endif
    end subroutine printing

end module drunkenSailor

program test
    use drunkenSailor
    implicit none
    call start()
end program test