       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISOGRAM.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PHRASE           PIC X(60).
       01 WS-PHRASE-LOW       PIC X(60).
       01 WS-RESULT           PIC 99.
       01 WS-CHAR             PIC X.
       01 WS-LEN              PIC 99.
       01 WS-I                PIC 99.
       01 WS-INDEX            PIC 99.
       01 WS-DUP-FLAG         PIC 9 VALUE 0.

       01 WS-LETTERS.
           05 WS-LETTER       PIC 9 OCCURS 26 TIMES VALUE 0.

       PROCEDURE DIVISION.
       ISOGRAM.
           MOVE 1 TO WS-RESULT
           MOVE 0 TO WS-DUP-FLAG

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 26
               MOVE 0 TO WS-LETTER(WS-I)
           END-PERFORM

           MOVE FUNCTION LOWER-CASE(WS-PHRASE) TO WS-PHRASE-LOW
           MOVE FUNCTION LENGTH(WS-PHRASE-LOW) TO WS-LEN

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN OR WS-DUP-FLAG = 1
               MOVE WS-PHRASE-LOW(WS-I:1) TO WS-CHAR
               IF WS-CHAR >= 'a' AND WS-CHAR <= 'z'
                   COMPUTE WS-INDEX = FUNCTION ORD(WS-CHAR) - FUNCTION ORD('a') + 1
                   ADD 1 TO WS-LETTER(WS-INDEX)
                   IF WS-LETTER(WS-INDEX) > 1
                       MOVE 0 TO WS-RESULT
                       MOVE 1 TO WS-DUP-FLAG
                   END-IF
               END-IF
           END-PERFORM
           .