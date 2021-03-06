// example.instr-br.xpl
DECLARATION
    BOOLEAN b

    INTEGER i

    STRING characters

    INTEGER countSt1 := 0
    INTEGER countSt2 := 0
    INTEGER countSt3 := 0
    INTEGER countSt4 := 0
    INTEGER countSt5 := 0
    INTEGER countSt6 := 0
    INTEGER countSt7 := 0
    INTEGER countSt8 := 0
    INTEGER countSt9 := 0
    INTEGER countSt10 := 0
    INTEGER countSt11 := 0

    INTEGER countBr1 := 0
    INTEGER countBr2 := 0
    INTEGER countBr3 := 0
    INTEGER countBr4 := 0
    INTEGER countBr5 := 0
PROGRAM
    i := 0
    countSt1 := countSt1 + 1

    IF TRUE THEN
        countBr1 := countBr1 + 1
        i := 1
        countSt2 := countSt2 + 1
    ELSE
        countBr2 := countBr2 + 1
        i := 0
        countSt3 := countSt3 + 1
    ENDIF
    countSt4 := countSt4 + 1

    WHILE ((i <> 0) AND (i <= 10)) DO
        i := i + 1
        countSt5 := countSt5 + 1
    ENDWHILE
    countSt6 := countSt6 + 1

    SWITCH i
        CASE 1 :
            countBr3 := countBr3 + 1
            characters := "i was 1"
            countSt7 := countSt7 + 1
        ENDCASE
        CASE 10 :
            countBr4 := countBr4 + 1
            characters := "i was 10"
            countSt8 := countSt8 + 1
        ENDCASE
        DEFAULT :
            countBr5 := countBr5 + 1
            characters := "i was not set to 1 or 10"
            countSt9 := countSt9 + 1
        ENDCASE
    ENDSWITCH
    countSt10 := countSt10 + 1

    FILE OVERWRITE "target.log" i
    countSt11 := countSt11 + 1
ENDPROGRAM
