       IDENTIFICATION DIVISION.
       PROGRAM-ID. LUHN.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CARD-NUMBER      PIC X(32).
       01 WS-CARD-DIGITS      PIC X(32).
       01 WS-CLEAN            PIC X(32).
       01 WS-LEN              PIC 99.
       01 WS-I                PIC 99.
       01 WS-J                PIC 99.
       01 WS-CHAR             PIC X.
       01 WS-DIGIT            PIC 99.
       01 WS-CHECKSUM         PIC 9(4) VALUE 0.
       01 WS-VALID            PIC X(5).
       01 WS-INVALID-FLAG     PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       LUHN.
           MOVE 0 TO WS-CHECKSUM
           MOVE 0 TO WS-INVALID-FLAG
           MOVE SPACES TO WS-CLEAN
           MOVE "FALSE" TO WS-VALID

           MOVE 0 TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > FUNCTION LENGTH(WS-CARD-NUMBER)
               MOVE WS-CARD-NUMBER(WS-I:1) TO WS-CHAR
               EVALUATE TRUE
                   WHEN WS-CHAR = ' '
                       CONTINUE
                   WHEN WS-CHAR >= '0' AND WS-CHAR <= '9'
                       ADD 1 TO WS-J
                       MOVE WS-CHAR TO WS-CLEAN(WS-J:1)
                   WHEN OTHER
                       MOVE 1 TO WS-INVALID-FLAG
               END-EVALUATE
           END-PERFORM

           IF WS-INVALID-FLAG = 1 OR WS-J <= 1
               MOVE "FALSE" TO WS-VALID
           ELSE
               MOVE WS-J TO WS-LEN

               PERFORM VARYING WS-I FROM WS-LEN BY -1
                   UNTIL WS-I < 1
                   COMPUTE WS-DIGIT =
                       FUNCTION NUMVAL(WS-CLEAN(WS-I:1))

                   IF FUNCTION MOD(WS-LEN - WS-I + 1, 2) = 0
                       COMPUTE WS-DIGIT = WS-DIGIT * 2
                       IF WS-DIGIT > 9
                           COMPUTE WS-DIGIT = WS-DIGIT - 9
                       END-IF
                   END-IF

                   ADD WS-DIGIT TO WS-CHECKSUM
               END-PERFORM

               *> Buoc 4: Kiem tra chia het cho 10
               IF FUNCTION MOD(WS-CHECKSUM, 10) = 0
                   MOVE "VALID" TO WS-VALID
               ELSE
                   MOVE "FALSE" TO WS-VALID
               END-IF
           END-IF
           .