       IDENTIFICATION DIVISION.
       PROGRAM-ID. DARTS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-X PIC 99V9.
       01 WS-Y PIC 99V9.
       01 WS-RESULT PIC 99.
         01 VI-TRI PIC 9(5)V99.
       PROCEDURE DIVISION.
       DARTS.
         COMPUTE VI-TRI = FUNCTION SQRT(WS-X * WS-X + WS-Y * WS-Y)
         EVALUATE TRUE
            WHEN VI-TRI <= 1
               MOVE 10 TO WS-RESULT
            WHEN VI-TRI <= 5
                MOVE 5 TO WS-RESULT
            WHEN VI-TRI <= 10
                MOVE 1 TO WS-RESULT
            WHEN OTHER
                MOVE 0 TO WS-RESULT
         END-EVALUATE
      
               
