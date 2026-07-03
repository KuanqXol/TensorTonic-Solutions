       IDENTIFICATION DIVISION.
       PROGRAM-ID. COLLATZ-CONJECTURE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER           PIC S9(8).
       01 WS-STEPS            PIC 9(4).
       01 WS-ERROR            PIC X(35).

       PROCEDURE DIVISION.
       COLLATZ-CONJECTURE.
           MOVE 0 TO WS-STEPS
           MOVE SPACES TO WS-ERROR

           IF WS-NUMBER <= 0
               MOVE "Only positive integers are allowed" TO WS-ERROR
           ELSE
               *> Lap cho den khi WS-NUMBER = 1
               PERFORM UNTIL WS-NUMBER = 1
                   IF FUNCTION MOD(WS-NUMBER, 2) = 1
                       COMPUTE WS-NUMBER = WS-NUMBER * 3 + 1
                   ELSE
                       COMPUTE WS-NUMBER = WS-NUMBER / 2
                   END-IF
                   ADD 1 TO WS-STEPS
               END-PERFORM
           END-IF
           .