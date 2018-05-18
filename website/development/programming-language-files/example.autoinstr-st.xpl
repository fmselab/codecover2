// example.autoinstr-st.xpl
DECLARATION
    BOOLEAN b := FALSE

    INTEGER i := -1

    STRING characters := "unset"
PROGRAM
    i := 0

    CodeCoverCoverageCounter_S1 := CodeCoverCoverageCounter_S1 + 1
IF TRUE THEN
        i := 1
    CodeCoverCoverageCounter_S2 := CodeCoverCoverageCounter_S2 + 1
ELSE
        i := 0
    CodeCoverCoverageCounter_S3 := CodeCoverCoverageCounter_S3 + 1
ENDIF

    CodeCoverCoverageCounter_S4 := CodeCoverCoverageCounter_S4 + 1
WHILE ((i <> 0) AND (i <= 10)) DO
        i := i + 1
    CodeCoverCoverageCounter_S5 := CodeCoverCoverageCounter_S5 + 1
ENDWHILE

    CodeCoverCoverageCounter_S6 := CodeCoverCoverageCounter_S6 + 1
SWITCH i
        CASE 1 :
            characters := "i was 1"
        CodeCoverCoverageCounter_S7 := CodeCoverCoverageCounter_S7 + 1
ENDCASE
        CASE 10 :
            characters := "i was 10"
        CodeCoverCoverageCounter_S8 := CodeCoverCoverageCounter_S8 + 1
ENDCASE
        DEFAULT :
            characters := "i was not set to 1 or 10"
        CodeCoverCoverageCounter_S9 := CodeCoverCoverageCounter_S9 + 1
ENDCASE
    ENDSWITCH

    CodeCoverCoverageCounter_S10 := CodeCoverCoverageCounter_S10 + 1
FILE OVERWRITE "target.log" i
CodeCoverCoverageCounter_S11 := CodeCoverCoverageCounter_S11 + 1
ENDPROGRAM