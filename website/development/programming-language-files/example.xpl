// example.xpl
DECLARATION
    BOOLEAN b := FALSE

    INTEGER i := -1

    STRING characters := "unset"
PROGRAM
    i := 0

    IF TRUE THEN
        i := 1
    ELSE
        i := 0
    ENDIF

    WHILE ((i <> 0) AND (i <= 10)) DO
        i := i + 1
    ENDWHILE

    SWITCH i
        CASE 1 :
            characters := "i was 1"
        ENDCASE
        CASE 10 :
            characters := "i was 10"
        ENDCASE
        DEFAULT :
            characters := "i was not set to 1 or 10"
        ENDCASE
    ENDSWITCH

    FILE OVERWRITE "target.log" i
ENDPROGRAM
