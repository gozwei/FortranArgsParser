PROGRAM ParserTest
    USE argsparser, ONLY: CheckForHelp, ParseArgumentInt, ParseArgumentReal, ParseArgumentLogical, ParseArgumentString

    IMPLICIT NONE

    
    INTEGER :: sizex, sizey
    REAL    :: StartTime
    REAL    :: StopTime

    LOGICAL :: UseStuff
    LOGICAL :: UseMoreStuff

    CHARACTER(len=128)  ::  FileName

    INTEGER :: StatusCorrect = 0
    INTEGER :: StatusIncorrect = 0
    INTEGER :: i
    LOGICAL :: PrintHelp = .FALSE.

    PrintHelp = CheckForHelp()
    DO i = 1,2
        sizex = ParseArgumentInt("--sizex", .FALSE.,0, StatusCorrect, StatusIncorrect,PrintHelp,&
                &"--sizex #: specify domain size in direction X")
        sizey = ParseArgumentInt("--sizey", .TRUE., 64, StatusCorrect, StatusIncorrect,PrintHelp,&
                &"--sizey #: specify domain size in direction Y")

        StartTime = ParseArgumentReal("--starttime", .TRUE., 0.0, StatusCorrect, StatusIncorrect,PrintHelp,&
                &"--starttime #: some real parameter")
        StopTime = ParseArgumentReal("--stoptime", .FALSE., 0.0, StatusCorrect, StatusIncorrect,PrintHelp,&
                &"--stoptime #: some real parameter")

        UseStuff = ParseArgumentLogical("-S", .TRUE., .FALSE., StatusCorrect, StatusIncorrect,PrintHelp,&
                &"-S: some logical flag")
        UseMoreStuff = ParseArgumentLogical("-M", .TRUE., .FALSE., StatusCorrect, StatusIncorrect,PrintHelp,&
                &"-M: some logical flag")

        FileName = ParseArgumentString("--filename", .FALSE., "", StatusCorrect, StatusIncorrect,PrintHelp,&
                &"--filename #: some string")


        IF (PrintHelp) THEN
            STOP 0
        END IF
        IF (StatusIncorrect > 0) THEN
            PrintHelp = .TRUE.
        ELSE
            EXIT
        END IF
    END DO


    write(*,"(A18,I8)") "sizex = ", sizex
    write(*,"(A18,I8)") "sizey = ", sizey

    write(*,"(A18,F8.4)") "starttime = ", StartTime
    write(*,"(A18,F8.4)") "stoptime = ", StopTime

    write(*,"(A18,L8)") "use stuff = ", UseStuff
    write(*,"(A18,L8)") "use more stuff = ", UseMoreStuff

    write(*,"(A18,A8)") "FileName = ", FileName

END PROGRAM ParserTest