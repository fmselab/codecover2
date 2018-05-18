IDENTIFICATION DIVISION.
PROGRAM-ID. BRANCHCOVERAGE.
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
PROCEDURE DIVISION.
B100.

ADD ZAHL1 TO ZAHL2
  ON SIZE ERROR
    DISPLAY "result field to small"
  NOT ON SIZE ERROR
    DISPLAY "result: ", ZAHL2
END-ADD.

IF FAC >= 0 AND < 8  OR (FAC >= 10 AND 11) THEN
  MOVE 1 TO ERG
  DISPLAY "Result: " ERG
ELSE
  DISPLAY "Fehlerhafte Eingabe"
END-IF.

IF FAC >= 0 AND < 8 THEN DISPLAY "something".


IF FAC >= 0 AND FAC < 8
  DISPLAY "another thing"
END-IF.


IF FAC >= 0 AND FAC < 8
  DISPLAY "another thing"
ELSE
  DISPLAY "this thing".


IF FAC >= 0 AND FAC < 8
  NEXT SENTENCE
ELSE
  DISPLAY "this thing".


IF FAC >= 0 AND FAC < 8
  IF ZAHL >= 0 AND ZAHL < 8
    DISPLAY "this thing"
  END-IF
ELSE
  NEXT SENTENCE
END-IF.

EVALUATE FAC
  WHEN 0 DISPLAY "a thing".


EVALUATE FAC
  WHEN 0 DISPLAY "non thing"
  WHEN 1 DISPLAY "one thing"
  WHEN OTHER DISPLAY "more things".


SEARCH ALL DATAFIELD1 VARYING DATAFIELD2
  AT END DISPLAY "a thing"
  WHEN DATAFIELD3 < 5 DISPLAY "this thing"
END-SEARCH.


SEARCH ALL DATAFIELD1 VARYING DATAFIELD2
  WHEN DATAFIELD3 < 5 NEXT SENTENCE
END-SEARCH.

STOP RUN.
