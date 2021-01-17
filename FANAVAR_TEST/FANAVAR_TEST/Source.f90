   ! define variables
    Module Variables
       
        Integer :: i,j,k,m,n,ib,im
        Integer :: MaxObj
        Integer,Dimension(:),Allocatable :: Object,Box

    End Module Variables
    

    Program FANAVARD_TEST

    Use Variables

    Implicit None

    ! open file and read input data
    Open(101,file = 'INPUT.txt')
    
    Read(101,*),n
    Read(101,*),m
    Read(101,*),k
    Allocate(Object(n),Box(n))
    Do i = 1,n
        Read(101,*),Object(i)
    Enddo
    
    Close(101)

    ! solve problem and find max of object
    MaxObj = 0
    Do i = 1,n
        ib = 1
        Box = k
        Do j = i,n
            IF( Object(j) <= Box(ib) )Then
                Box(ib) = Box(ib)-Object(j)
            Else IF( Object(j) > Box(ib) .AND. ib < m )Then
                Box(ib+1) = Box(ib+1)-Object(j)
                ib = ib+1
            else
                EXIT
            EndIF
        Enddo
        j = j-1
        IF( j == n .AND. (j-i+1) > MaxObj )Then
            MaxObj = j-i+1
            im = i
        EndIF
    Enddo

    Deallocate(Object,Box)
    
    ! print the answer
    Print*,''
    Print*,''
    Print*,'        number of starting object : ',im
    Print*,'             max of object in box : ',MaxObj
    Print*,''
    Print*,''
    Print*,'+===================================================+'
    Print*,'|                                                   |'
    Print*,'|                     Mr. CODE.                     |'
    Print*,'|                                                   |'
    Print*,'|               #1 : Ali Zolfaghari                 |'
    Print*,'|               #2 : Hossein Najaf                  |'
    Print*,'|               #3 : Nima Manoochehri               |'
    Print*,'|               #4 : Sahar Khazali                  |'
    Print*,'|                                                   |'
    Print*,'+===================================================+'
    Print*,''
    Print*,''

    Pause

    End Program FANAVARD_TEST
