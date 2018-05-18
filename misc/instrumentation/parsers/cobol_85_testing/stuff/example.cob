IDENTIFICATION DIVISION.
PROGRAM-ID. TESTPROG1.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
01 E-FELDER.
  05 FAC PIC 9999 VALUE ZERO.
01 V-FELDER.
  05 ERG PIC 9999 VALUE 1.
  05 I PIC 9999.
*>01 TEST-CASE-DESCRIPTIONS.
  *>05 TEST-CASE-1 PIC X(50) VALUE "a test case name".
PROCEDURE DIVISION.
B100.
DISPLAY "Calculate faculty:".
ACCEPT FAC.
IF FAC >= 0 AND < 8 THEN
  MOVE 1 TO ERG
  *>STARTTESTCASE "test case name"
  PERFORM VARYING I FROM 1 BY 1 UNTIL I > FAC
    COMPUTE ERG = ERG * I
  END-PERFORM
  *>ENDTESTCASE
  DISPLAY "FACULTY: " ERG
ELSE
  DISPLAY "Fehlerhafte Eingabe"
END-IF.
*>STARTTESTCASE "test
PERFORM B100 UNTIL FAC = 9.
STOP RUN.
