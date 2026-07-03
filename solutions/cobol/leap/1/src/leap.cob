       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
         01 WS-YEAR   PIC 9999.
         01 WS-RESULT PIC 9.
         01 WS-REM    PIC 9999.
       
       PROCEDURE DIVISION.
       LEAP.
           COMPUTE WS-REM = FUNCTION MOD(WS-YEAR, 4)
           IF WS-REM = 0
               MOVE 1 TO WS-RESULT
               COMPUTE WS-REM = FUNCTION MOD(WS-YEAR, 100)
               IF WS-REM = 0
                   MOVE 0 TO WS-RESULT
               END-IF
               COMPUTE WS-REM = FUNCTION MOD(WS-YEAR, 400)
               IF WS-REM = 0
                   MOVE 1 TO WS-RESULT
               END-IF
           ELSE
               MOVE 0 TO WS-RESULT
           END-IF
           .