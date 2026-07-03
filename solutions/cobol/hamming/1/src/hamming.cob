       IDENTIFICATION DIVISION.
       PROGRAM-ID. hamming.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DNA-1 PIC X(32).
       01 WS-DNA-2 PIC X(32).
       01 WS-HAMMING PIC 9(2).
       01 WS-ERROR PIC X(31).

         01 I PIC 99.
         01 LEN-1 PIC 99.
         01 LEN-2 PIC 99.

       PROCEDURE DIVISION.
       HAMMING.
         MOVE 0 TO WS-HAMMING
         MOVE SPACE TO WS-ERROR
         MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-1 TRAILING)) TO LEN-1
         MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-2 TRAILING)) TO LEN-2

         IF LEN-1 NOT = LEN-2
            MOVE 'Strands must be of equal length' TO WS-ERROR
         ELSE
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-1
               IF WS-DNA-1(I:1) NOT = WS-DNA-2(I:1)
                  ADD 1 TO WS-HAMMING
               END-IF
            END-PERFORM
         END-IF.
