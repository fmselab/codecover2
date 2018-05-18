
 IDENTIFICATION DIVISION.
*>=======================
 Program-id. EXAMPLE is initial.

*>**************************************************************
*> An example COBOL program that displays a list of squares.
*> @author Bernard Pinon
*>**************************************************************

 ENVIRONMENT DIVISION.
*>====================

 Data division.
*>=============

 WORKING-STORAGE SECTION.
*>-----------------------

 01 banner.
     02 filler pic X(20) value "Hello world version ".
     02 version-major pic 9 value 1.
     02 filler pic X value ".".
     02 version-minor pic 9 value zero.

 01 bye-bye.
      02 filler pic X(7) value "Bye-bye".

 77 i pic S9(4) comp value zero.
 77 j pic S9(4) comp value zero.

 77  FILLER PIC X(35) VALUE '$@#: 06.06.1996*08:40*PRO*A18O012  '.

 77 end-of-treatment-flag pic 9 value zero.
      88 end-of-treatment value 1.

 Procedure division.
*>==================

 Main section.
*>------------
     perform init
     perform treatment until end-of-treatment
     perform wrapup
     exit program.

 Init section.
*>-------------
*>   open files, take coffee, shower...
     display banner.

 Treatment section.
*>------------------
*>    ... do something useful here, then ...
     add 1 to i
     if i < 10 then
         call "MySquare" using by content i, by reference j
         display "Value of i is ", i, " square is ", j
     else
         set end-of-treatment to true
     end-if.

 Wrapup section.
*>--------------
*>   close files, clean up, brush teeth...
     display bye-bye.
*>   and eventually ...
     stop run.

*>**************************************************************
*> SUBPROGRAM
*>**************************************************************

 Identification division.
*>=======================
 Program-id. MySquare.

 Data division.
*>=============

 Linkage section.
*>---------------
 77 n pic S9(4) comp.
 77 result pic S9(4) comp.

 Procedure division using n, result.
*>==================================
     compute
       result = n * n
     end-compute


     exit program.

 End program MySquare.
 End program EXAMPLE.


