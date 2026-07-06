       IDENTIFICATION DIVISION.
       PROGRAM-ID. raindrops.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER PIC 9(4).
       01 WS-RESULT PIC X(20).
         01 X PIC 9.
         01 CNT PIC 99.
         01 LEN PIC 99.

       PROCEDURE DIVISION.
       RAINDROPS.
         MOVE 1 TO CNT
         MOVE SPACE TO WS-RESULT
         MOVE FUNCTION MOD(WS-NUMBER, 3) TO X
         IF X = 0
            MOVE 'Pling' TO WS-RESULT(CNT:5)
            ADD 5 TO CNT
         END-IF
         MOVE FUNCTION MOD(WS-NUMBER, 5) TO X
         IF X = 0
            MOVE 'Plang' TO WS-RESULT(CNT:5)
            ADD 5 TO CNT
         END-IF
         MOVE FUNCTION MOD(WS-NUMBER, 7) TO X
         IF X = 0
            MOVE 'Plong' TO WS-RESULT(CNT:5)
         END-IF
         MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-RESULT)) TO LEN
         IF LEN = 0
            MOVE WS-NUMBER TO WS-RESULT
         END-IF