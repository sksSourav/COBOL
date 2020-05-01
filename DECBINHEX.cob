IDENTIFICATION DIVISION.
PROGRAM-ID. DECBINHEX.
DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 D2H.
            02 D2H-DEC-IP                 PIC 9(09).
            02 D2H-COMP                   PIC 9(09) COMP.
            02 D2H-HEX REDEFINES D2H-COMP PIC X(04).
        01 H2D.
            02 H2D-HEX-IP                              PIC X(04).
            02 H2D-HEX-WORD-X                          PIC X(08).
            02 H2D-HEX-WORD-9 REDEFINES H2D-HEX-WORD-X PIC S9(16) BINARY.
            02 H2D-DEC-X                               PIC X(09).
            02 H2D-DEC-9 REDEFINES H2D-DEC-X           PIC 9(09).
        01 HV.
            02 HV-HEX-IP        PIC X(04).
            02 HV-HEX-STR       PIC X(16) VALUES "0123456789ABCDEF".
            02 HV-DEC           PIC S9(4) COMP.
            02 FILLER REDEFINES HV-DEC.
                03 FILLER       PIC X.
                03 HV-DEC-BYTE  PIC X.
            02 I                PIC S9(8) COMP.
            02 J                PIC S9(8) COMP.
            02 Q                PIC S9(8) COMP.
            02 R                PIC S9(8) COMP.
            02 J1               PIC S9(8) COMP.
            02 Q1               PIC S9(8) COMP.
            02 R1               PIC S9(8) COMP.
            02 HV-HEX-VIEW      PIC X(08).
        01 BV1BY.
            02 BV-BIN-IP        PIC 9(01) BINARY.
            02 BV-COUNTER       PIC 9(03) VALUE 128.
            02 BV-INDEX         PIC 9(01) VALUE 1.
            02 BV-BIN-VIEW      PIC X(08).
PROCEDURE DIVISION.
    ACCEPT D2H-DEC-IP.
    PERFORM DECIMAL2HEX-PARA.
    MOVE D2H-HEX TO H2D-HEX-IP.
    PERFORM HEX2DECIMAL-PARA.
    MOVE D2H-HEX TO HV-HEX-IP.
    PERFORM HEX-VIEW-PARA.
    MOVE 4 TO BV-BIN-IP.
    PERFORM BIN-1-BYTE-VIEW-PARA.
STOP RUN.

DECIMAL2HEX-PARA.
    MOVE D2H-DEC-IP TO D2H-COMP.
    DISPLAY "TRUE HEX " D2H-HEX.
EXIT.

HEX2DECIMAL-PARA.
    MOVE LOW-VALUE  TO H2D-HEX-WORD-X(1:4).
    MOVE H2D-HEX-IP TO H2D-HEX-WORD-X(5:4).
    MOVE H2D-HEX-WORD-9 TO H2D-DEC-9.
    DISPLAY "TRUE DECIMAL " H2D-DEC-X.
EXIT.

HEX-VIEW-PARA.
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
        COMPUTE J = 2 * I - 1
        MOVE HV-HEX-IP(I:1) TO HV-DEC-BYTE
        DIVIDE HV-DEC BY 16 GIVING Q REMAINDER R
        COMPUTE J1 = J + 1
        COMPUTE Q1 = Q + 1
        COMPUTE R1 = R + 1
        MOVE HV-HEX-STR(Q1:1) TO HV-HEX-VIEW(J:1)
        MOVE HV-HEX-STR(R1:1) TO HV-HEX-VIEW(J1:1)
    END-PERFORM.
    DISPLAY "HEX VIEW " HV-HEX-VIEW.
EXIT.

BIN-1-BYTE-VIEW-PARA.
    PERFORM UNTIL BV-COUNTER <= 1
        DISPLAY BV-COUNTER
        IF BV-BIN-IP > (BV-COUNTER - 1)
            MOVE 1 TO BV-BIN-VIEW(BV-INDEX:1)
            COMPUTE BV-BIN-IP = BV-BIN-IP - (BV-COUNTER)
        ELSE
            MOVE 0 TO BV-BIN-VIEW(BV-INDEX:1)
        END-IF
        ADD 1 TO BV-INDEX
        COMPUTE BV-COUNTER = BV-COUNTER - (BV-COUNTER/2)
    END-PERFORM.
    IF BV-BIN-IP = 1
        MOVE 1 TO BV-BIN-VIEW(BV-INDEX:1)
    ELSE
        MOVE 0 TO BV-BIN-VIEW(BV-INDEX:1)
    END-IF
    DISPLAY BV-BIN-VIEW.
EXIT.
