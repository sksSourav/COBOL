IDENTIFICATION DIVISION.
PROGRAM-ID. PHONENUMVALIDATE.
DATA DIVISION.
   WORKING-STORAGE SECTION.
   01 WA-MOBILE PIC X(12) VALUE "0 7506088837".
   01 WS-MOBILE.
    02 WS-PREFIX PIC X(02).
        88 VALID-PREFIX VALUES "  ","00","91"," 0".
    02 WS-NUMBER PIC X(10).
   01 WX-MOBILE-NUMBER.
    02 WX-PREFIX PIC X(02) VALUE "91".
    02 WX-NUMBER PIC X(10).
PROCEDURE DIVISION.
    FIRST-PARA.
    MOVE WA-MOBILE TO WS-MOBILE
    EVALUATE TRUE
    WHEN (VALID-PREFIX) AND (WS-NUMBER IS NUMERIC) AND (WS-NUMBER NOT ZEROS)
        MOVE WS-NUMBER TO WX-NUMBER
    WHEN NOT VALID-PREFIX AND (WA-MOBILE(3:10) IS NUMERIC) AND (WS-NUMBER NOT ZEROS)
        MOVE WA-MOBILE(3:10) TO WX-NUMBER
    WHEN NOT VALID-PREFIX AND (WA-MOBILE(2:10) IS NUMERIC) AND (WS-NUMBER NOT ZEROS)
        MOVE WA-MOBILE(2:10) TO WX-NUMBER
    WHEN NOT VALID-PREFIX AND (WA-MOBILE(1:10) IS NUMERIC) AND (WS-NUMBER NOT ZEROS)
        MOVE WA-MOBILE(1:10) TO WX-NUMBER
    WHEN OTHER
        DISPLAY "INVALID MOBILE NUMBER"
    END-EVALUATE
    DISPLAY "VALID MOBILE NUMBER IS " WX-MOBILE-NUMBER
STOP RUN.