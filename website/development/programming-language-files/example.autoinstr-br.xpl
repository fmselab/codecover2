// example-autoinstr-br.xpl
DECLARATION
    BOOLEAN b := FALSE

    INTEGER i := -1

    STRING characters := "unset"
    INTEGER CodeCoverCoverageCounter_S1 := 0
    INTEGER CodeCoverCoverageCounter_S2 := 0
    INTEGER CodeCoverCoverageCounter_S3 := 0
    INTEGER CodeCoverCoverageCounter_S4 := 0
    INTEGER CodeCoverCoverageCounter_S5 := 0
    INTEGER CodeCoverCoverageCounter_S6 := 0
    INTEGER CodeCoverCoverageCounter_S7 := 0
    INTEGER CodeCoverCoverageCounter_S8 := 0
    INTEGER CodeCoverCoverageCounter_S9 := 0
    INTEGER CodeCoverCoverageCounter_S10 := 0
    INTEGER CodeCoverCoverageCounter_S11 := 0

    INTEGER CodeCoverCoverageCounter_B1 := 0
    INTEGER CodeCoverCoverageCounter_B2 := 0
    INTEGER CodeCoverCoverageCounter_B3 := 0
    INTEGER CodeCoverCoverageCounter_B4 := 0
    INTEGER CodeCoverCoverageCounter_B5 := 0

PROGRAM
    i := 0

    CodeCoverCoverageCounter_S1 := CodeCoverCoverageCounter_S1 + 1
    IF TRUE THEN
        CodeCoverCoverageCounter_B1 := CodeCoverCoverageCounter_B1 + 1
    i := 1
    CodeCoverCoverageCounter_S2 := CodeCoverCoverageCounter_S2 + 1
    ELSE
        CodeCoverCoverageCounter_B2 := CodeCoverCoverageCounter_B2 + 1
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
            CodeCoverCoverageCounter_B3 := CodeCoverCoverageCounter_B3 + 1
    characters := "i was 1"
        CodeCoverCoverageCounter_S7 := CodeCoverCoverageCounter_S7 + 1
    ENDCASE
        CASE 10 :
            CodeCoverCoverageCounter_B4 := CodeCoverCoverageCounter_B4 + 1
    characters := "i was 10"
        CodeCoverCoverageCounter_S8 := CodeCoverCoverageCounter_S8 + 1
    ENDCASE
        DEFAULT :
            CodeCoverCoverageCounter_B5 := CodeCoverCoverageCounter_B5 + 1
        characters := "i was not set to 1 or 10"
        CodeCoverCoverageCounter_S9 := CodeCoverCoverageCounter_S9 + 1
    ENDCASE
    ENDSWITCH

    CodeCoverCoverageCounter_S10 := CodeCoverCoverageCounter_S10 + 1
    FILE OVERWRITE "target.log" i
CodeCoverCoverageCounter_S11 := CodeCoverCoverageCounter_S11 + 1
    ENDPROGRAM