       IDENTIFICATION DIVISION.
       PROGRAM-ID. PANGRAM.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SENTENCE       PIC X(60).
       01 WS-SEN-LOW        PIC X(60).
       01 WS-LEN            PIC 99.
       01 WS-CHAR           PIC X.
       01 WS-RESULT         PIC 9.
       01 WS-I              PIC 99 VALUE 1.
       01 WS-INDEX          PIC 99.
       01 WS-MISSING        PIC 9 VALUE 0.

       01 WS-LETTERS.
           05 WS-LETTER     PIC 9 OCCURS 26 TIMES VALUE 0.

       PROCEDURE DIVISION.
       PANGRAM.
           MOVE 1 TO WS-RESULT
           MOVE 0 TO WS-MISSING

           *> Reset mang - KHONG co dau cham ben trong
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 26
               MOVE 0 TO WS-LETTER(WS-I)
           END-PERFORM

           MOVE FUNCTION LOWER-CASE(WS-SENTENCE) TO WS-SEN-LOW
           MOVE FUNCTION LENGTH(WS-SEN-LOW) TO WS-LEN

           *> Duyet tung ky tu - KHONG co dau cham ben trong
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE WS-SEN-LOW(WS-I:1) TO WS-CHAR
               IF WS-CHAR >= 'a' AND WS-CHAR <= 'z'
                   COMPUTE WS-INDEX =
                       FUNCTION ORD(WS-CHAR) -
                       FUNCTION ORD('a') + 1
                   ADD 1 TO WS-LETTER(WS-INDEX)
               END-IF
           END-PERFORM

           *> Kiem tra chu cai thieu - dung co bien flag
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 26 OR WS-MISSING = 1
               IF WS-LETTER(WS-I) = 0
                   MOVE 0 TO WS-RESULT
                   MOVE 1 TO WS-MISSING
               END-IF
           END-PERFORM
           .