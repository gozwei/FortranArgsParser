# FortranArgsParser

*FortranArgsParser* is very simple module for parsing command line arguments.

*FortranArgsParser* functions:
1. Parse integer arguments in format `--any_name VALUE` with support of default value
2. Parse real arguments in format `--any_name VALUE` with support of default value
3. Parse logical arguments in format `-X` or `--any_name`
4. Displaying help if `-h` is specified
5. Displaying help and exiting if required argument is missing

### Notes

1. Argument name format is user specified, does not have to start with `--` or `-`, however it should
2. Displaying help is also defined by user (see. `test.f90`)

### More motes

This project was created and developed as a part of other project and was developed for specific and limited functionality. It might require some work to be useful in other projects. 

## Usage 

See `test.f90` for full example. 

Module provides 4 functions:
1. `CheckForHelp()` - checks if user used `-h` flag and returns `.TRUE.`
2. `ParseArgumentInt(ArgName, UseDefault, Default, StatusCorrect, StatusIncorrect, PrintHelp, Help)` - returns integer value of argument
3. `ParseArgumentReal(ArgName, UseDefault, Default, StatusCorrect, StatusIncorrect, PrintHelp, Help)` - returns real value of argument
4. `ParseArgumentLogical(ArgName, UseDefault, Default, StatusCorrect, StatusIncorrect, PrintHelp, Help)` - returns logical value of flag
5. `ParseArgumentString(ArgName, UseDefault, Default, StatusCorrect, StatusIncorrect, PrintHelp, Help)` - returns `CHARACTER(len=128)` value of flag

### Function arguments:

* `ArgName`: strign with argument / flag name (ex. `--szie`, `-L`, `--ANY-NAME`...) 
* `UseDefault`: logical flag if this argument has default value (`.TRUE.` or `.FALSE.`)
* `Default`: integer, real or logical (depending on function used) default value
* `StatusCorrect`: integer container for number of correctly processed arguments. Must be initiated with 0
* `StatusIncorrect`: integer container for number of incorrectly processed arguments. Must be initiated with 0
* `PrintHelp`: logical flag if help should be printed: if help is printed arguments are not processed - see `test.f90` for idea of this approach
* `Help`: string with help

### Error handling
Ability to print full help if any of required arguments fail requires double pass of all `Parse...` functions. In provided example this is achieved by using a loop. 

## Example

For given code (part of `test.f90` example):

```
sizex = ParseArgumentInt("--sizex", .FALSE., 0, ...)
sizey = ParseArgumentInt("--sizey", .TRUE., 64, ...)

StartTime = ParseArgumentReal("--starttime", .TRUE., 0.0, ...)
StopTime = ParseArgumentReal("--stoptime", .FALSE., 0.0, ...)

UseStuff = ParseArgumentLogical("-S", .TRUE., .FALSE., ...)
UseMoreStuff = ParseArgumentLogical("-M", .TRUE., .FALSE., ...)

FileName = ParseArgumentString("--filename", .FALSE., "", ...)

write(*,"(A15,I8)") "sizex = ", sizex
write(*,"(A15,I8)") "sizey = ", sizey

write(*,"(A15,F8.4)") "starttime = ", StartTime
write(*,"(A15,F8.4)") "stoptime = ", StopTime

write(*,"(A15,L8)") "use stuff = ", UseStuff
write(*,"(A15,L8)") "use more stuff = ", UseMoreStuff

write(*,"(A18,A8)") "FileName = ", FileName
```  

Results might be like that:

```
$ ./test.exe --sizex 256 --stoptime 15.677 -M --filename test
          sizex =      256
          sizey =       64
      starttime =   0.0000
       stoptime =  15.6770
      use stuff =        F
 use more stuff =        T
       FileName = test   
```

```
$ ./test.exe --sizex 256 --sizey 32 -S --stoptime 15.677 -M --filename "A B C D"
          sizex =      256
          sizey =       32
      starttime =   0.0000
       stoptime =  15.6770
      use stuff =        T
 use more stuff =        T
       FileName = A B C D 
```

```
$ ./test.exe --starttime -32.5 --sizex 256 --sizey 32 -S --stoptime 15.677 --filename "A B C D"
          sizex =      256
          sizey =       32
      starttime = -32.5000
       stoptime =  15.6770
      use stuff =        T
 use more stuff =        F
       FileName = A B C D 
```
