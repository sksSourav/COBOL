IDENTIFICATION DIVISION.
PROGRAM-ID. PANNUMVALIDATION.
DATA DIVISION.
   WORKING-STORAGE SECTION.
   01 WA-SPACE-COUNER PIC 9(02) VALUE 0.
   01 WA-PAN PIC X(10) VALUE "EQDaS8345d".
   01 WS-PAN-VAL.
    02 WS-F4CHAR PIC X(03).
    02 WS-P1CHAR PIC X(01).
        88 WA-P1CHAR VALUES "C","P","H","F","A",
        "T","B","L","J","G","c","p","h","f","a",
        "t","b","l","j","g".
    02 WS-PICHAR-T REDEFINES WS-P1CHAR PIC X(01).
        88 WA-COMPANY VALUE "C","c".
        88 WA-PERSON VALUE "P","p".
        88 WA-HUF VALUE "H","h".
        88 WA-FIRM VALUE "F","f".
        88 WA-AOP VALUE "A","a".
        88 WA-TRUST VALUE "T","t".
        88 WA-BOI VALUE "B","b".
        88 WA-LA VALUE "L","l".
        88 WA-AJP VALUE "J","j".
        88 WA-GOV VALUE "G","g".
    02 WS-M1CHAR PIC X(01).
        88 WA-M1CHAR VALUES "A" THRU "Z",
        "a" THRU "z".
    02 WS-04NUMB PIC X(04).
    02 WS-L1CHAR PIC X(01).
        88 WA-L1CHAR VALUES "A" THRU "Z",
        "a" THRU "z".
PROCEDURE DIVISION.
    FIRST-PARA.
    MOVE WA-PAN TO WS-PAN-VAL.
    INSPECT WA-PAN TALLYING WA-SPACE-COUNER FOR ALL SPACE.
    IF WA-SPACE-COUNER > 0
     MOVE "X" TO WS-P1CHAR
    END-IF.
    IF WA-P1CHAR
    AND WS-F4CHAR IS ALPHABETIC
    AND WA-M1CHAR
    AND WS-04NUMB IS NUMERIC
    AND WA-L1CHAR
        EVALUATE TRUE
        WHEN WA-COMPANY
            DISPLAY "VALID COMPANY PAN " WA-PAN
        WHEN WA-PERSON
            DISPLAY "VALID PERSONAL PAN " WA-PAN
        WHEN WA-HUF
            DISPLAY "VALID HINDU UNDIVIDED FAMILY PAN " WA-PAN
        WHEN WA-FIRM
            DISPLAY "VALID FIRM PAN " WA-PAN
        WHEN WA-AOP
            DISPLAY "VALID ASSOCIATION OF PERSONS PAN " WA-PAN
        WHEN WA-TRUST
            DISPLAY "VALID TRUST PAN " WA-PAN
        WHEN WA-BOI
            DISPLAY "VALID BODY OF INDIVIDUALS PAN " WA-PAN
        WHEN WA-LA
            DISPLAY "VALID LOCAL AUTHORITY PAN " WA-PAN
        WHEN WA-AJP
            DISPLAY "VALID ARTIFICIAL JURIDICAL PERSON PAN " WA-PAN
        WHEN WA-GOV
            DISPLAY "VALID GOVERNMENT PAN " WA-PAN
        WHEN OTHER
            DISPLAY "INVALID PAN"
        END-EVALUATE
    ELSE
        DISPLAY "INVALID PAN"
    END-IF.
STOP RUN.
