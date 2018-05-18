IDENTIFICATION DIVISION.
PROGRAM-ID. TESTPROG.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
SELECT COVERAGE-LOG-FILE ASSIGN TO "coverage.clf"
  ORGANIZATION IS LINE SEQUENTIAL.
DATA DIVISION.
FILE SECTION.
FD COVERAGE-LOG-FILE.
01 COVERAGE-LOG-FILE-DATA.
  05 COVERAGE-LOG-FILE-HEADER.
    10 COVERAGE-SOURCE-FILE-TXT PIC X(18) VALUE "Program name: ".
    10 COVERAGE-SOURCE-FILE PIC X(11) VALUE "TESTPROG".
    10 COVERAGE-TXT-NEW-LINE PIC X VALUE X"0A".
  05 COVERAGE-LOG-FILE-COUNTER.
    10 COUNT-TEST-CASE-HEADER-TXT PIC X(16) VALUE "Test case name: ".
    10 COUNT-TEST-CASE-HEADER PIC X(50) VALUE "test case 1".
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-TXT-HORIZONTAL-LINE PIC X(66) VALUE ALL "-".
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-STATEMENT-1-TXT PIC X(4) VALUE "S-1 ".
    10 COUNT-STATEMENT-1 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-STATEMENT-2-TXT PIC X(4) VALUE "S-2 ".
    10 COUNT-STATEMENT-2 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-BRANCH-1-TXT PIC X(4) VALUE "B-1 ".
    10 COUNT-BRANCH-1 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-CONDITION-1-1010-TXT PIC X(9) VALUE "C-1-1010 ".
    10 COUNT-CONDITION-1-1010 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-CONDITION-1-1011-TXT PIC X(9) VALUE "C-1-1011 ".
    10 COUNT-CONDITION-1-1011 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-CONDITION-1-1110-TXT PIC X(9) VALUE "C-1-1110 ".
    10 COUNT-CONDITION-1-1110 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-CONDITION-1-1111-TXT PIC X(9) VALUE "C-1-1111 ".
    10 COUNT-CONDITION-1-1111 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-STATEMENT-3-TXT PIC X(4) VALUE "S-3 ".
    10 COUNT-STATEMENT-3 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-LOOPING-1-0-TXT PIC X(6) VALUE "L-0-1 ".
    10 COUNT-LOOPING-1-0 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-LOOPING-1-1-TXT PIC X(6) VALUE "L-1-1 ".
    10 COUNT-LOOPING-1-1 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-LOOPING-1-2-TXT PIC X(6) VALUE "L-2-1 ".
    10 COUNT-LOOPING-1-2 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-STATEMENT-4-TXT PIC X(4) VALUE "S-4 ".
    10 COUNT-STATEMENT-4 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-STATEMENT-5-TXT PIC X(4) VALUE "S-5 ".
    10 COUNT-STATEMENT-5 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-BRANCH-2-TXT PIC X(4) VALUE "B-2 ".
    10 COUNT-BRANCH-2 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-STATEMENT-6-TXT PIC X(4) VALUE "S-6 ".
    10 COUNT-STATEMENT-6 PIC 9(18) VALUE ZERO.
    10 COUNT-TXT-NEW-LINE PIC X VALUE X"0A".
    10 COUNT-TXT-HORIZONTAL-LINE PIC X(66) VALUE ALL "-".
WORKING-STORAGE SECTION.
01 COVERAGE-DATA-FIELDS.
  05 TEST-CASE-DESCRIPTIONS.
    10 TEST-CASE-1 PIC X(50) VALUE "Demo test case".
  05 LOOPING-COVERAGE-COUNTER.
    10 COUNT-LOOPING-T-1 PIC 9 VALUE ZERO.
  05 START-TEST-CASE-BOOLEAN PIC X VALUE "T".
    88 START-TEST-CASE-TRUE VALUE "T".
01 E-FELDER.
  05 FAC PIC 9999 VALUE ZERO.
01 V-FELDER.
  05 ERG PIC 9999 VALUE 1.
  05 I PIC 9999.
PROCEDURE DIVISION.
PERFORM WRITECOVERAGEFILETODISK.

B100.

DISPLAY "Calculate faculty:".
ADD 1 TO COUNT-STATEMENT-1.

ACCEPT FAC.
ADD 1 TO COUNT-STATEMENT-2.

IF FAC >= 0 THEN
  IF FAC < 8 THEN
    ADD 1 TO COUNT-CONDITION-1-1111
  ELSE
    ADD 1 TO COUNT-CONDITION-1-1110
  END-IF
ELSE
 IF FAC < 8 THEN
    ADD 1 TO COUNT-CONDITION-1-1011
  ELSE
    ADD 1 TO COUNT-CONDITION-1-1010
  END-IF
END-IF.
IF FAC >= 0 AND < 8 THEN
  ADD 1 TO COUNT-BRANCH-1

  MOVE 1 TO ERG
  ADD 1 TO COUNT-STATEMENT-3

  *> PERFORM STARTTESTCASE
  *> MOVE TEST-CASE-1 TO COUNT-TEST-CASE-HEADER

  MOVE 0 TO COUNT-LOOPING-T-1
  PERFORM VARYING I FROM 1 BY 1 UNTIL I > FAC
    ADD 1 TO COUNT-LOOPING-T-1

    COMPUTE ERG = ERG * I
    ADD 1 TO COUNT-STATEMENT-4

  END-PERFORM
  EVALUATE COUNT-LOOPING-T-1
    WHEN 0
      ADD 1 TO COUNT-LOOPING-0-1
    WHEN 1
      ADD 1 TO COUNT-LOOPING-1-1
    WHEN OTHER
      ADD 1 TO COUNT-LOOPING-2-1
  END-EVALUATE

  *> PERFORM ENDTESTCASE

  DISPLAY "FACULTY: " ERG
  ADD 1 TO COUNT-STATEMENT-5

ELSE
  ADD 1 TO COUNT-BRANCH-2

  DISPLAY "Fehlerhafte Eingabe"
  ADD 1 TO COUNT-STATEMENT-6

END-IF.

PERFORM B100 UNTIL FAC = 9.

PERFORM STOPTHEPROGRAMNOW.

STARTTESTCASE.
IF START-TEST-CASE-TRUE THEN
  PERFORM WRITEALLCOVERAGECOUNTERTOFILE
END-IF.
PERFORM SETALLCOVERAGECOUNTERTOZERO.
MOVE "T" TO START-TEST-CASE-BOOLEAN.

ENDTESTCASE.
PERFORM WRITEALLCOVERAGECOUNTERTOFILE.
PERFORM SETALLCOVERAGECOUNTERTOZERO.
MOVE "F" TO START-TEST-CASE-BOOLEAN.

SETALLCOVERAGECOUNTERTOZERO.
*> INITIALIZE COVERAGE-COUNTER REPLACING NUMERIC BY ZERO.
MOVE 0 TO COUNT-STATEMENT-1.
MOVE 0 TO COUNT-STATEMENT-2.
MOVE 0 TO COUNT-STATEMENT-3.
MOVE 0 TO COUNT-STATEMENT-4.
MOVE 0 TO COUNT-STATEMENT-5.
MOVE 0 TO COUNT-STATEMENT-6.
MOVE 0 TO COUNT-BRANCH-1.
MOVE 0 TO COUNT-BRANCH-2.
MOVE 0 TO COUNT-CONDITION-1-1010.
MOVE 0 TO COUNT-CONDITION-1-1011.
MOVE 0 TO COUNT-CONDITION-1-1110.
MOVE 0 TO COUNT-CONDITION-1-1111.
MOVE 0 TO COUNT-LOOPING-0-1.
MOVE 0 TO COUNT-LOOPING-1-1.
MOVE 0 TO COUNT-LOOPING-2-1.

WRITECOVERAGEFILETODISK.
OPEN OUTPUT COVERAGE-LOG-FILE.

WRITEALLCOVERAGECOUNTERTOFILE.
WRITE COVERAGE-LOG-FILE-DATA.

STOPTHEPROGRAMNOW.
IF START-TEST-CASE-TRUE THEN
  PERFORM WRITEALLCOVERAGECOUNTERTOFILE
END-IF.
CLOSE COVERAGE-LOG-FILE.
STOP RUN.

END PROGRAM TESTPROG.