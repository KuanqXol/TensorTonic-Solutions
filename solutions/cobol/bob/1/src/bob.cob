       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-HEYBOB             PIC X(60).
       01 WS-RESULT             PIC X(40).
       01 WS-LEN                PIC 99.
       01 WS-I                  PIC 99.
       01 WS-CHAR               PIC X.
       01 WS-IS-QUESTION        PIC 9 VALUE 0.
       01 WS-HAS-LETTER         PIC 9 VALUE 0.
       01 WS-HAS-LOWER          PIC 9 VALUE 0.
       01 WS-IS-YELLING         PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       BOB.
           *> Reset tat ca bien
           MOVE 0 TO WS-IS-QUESTION
           MOVE 0 TO WS-HAS-LETTER
           MOVE 0 TO WS-HAS-LOWER
           MOVE 0 TO WS-IS-YELLING

           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-HEYBOB TRAILING)) TO WS-LEN

           IF WS-LEN = 0
               MOVE "Fine. Be that way!" TO WS-RESULT
           ELSE
               MOVE WS-HEYBOB(WS-LEN:1) TO WS-CHAR
               IF WS-CHAR = '?'
                   MOVE 1 TO WS-IS-QUESTION
               END-IF

               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-LEN
                   MOVE WS-HEYBOB(WS-I:1) TO WS-CHAR
                   IF WS-CHAR >= 'a' AND WS-CHAR <= 'z'
                       MOVE 1 TO WS-HAS-LOWER
                   END-IF
                   IF (WS-CHAR >= 'a' AND WS-CHAR <= 'z') OR
                      (WS-CHAR >= 'A' AND WS-CHAR <= 'Z')
                       MOVE 1 TO WS-HAS-LETTER
                   END-IF
               END-PERFORM

               IF WS-HAS-LETTER = 1 AND WS-HAS-LOWER = 0
                   MOVE 1 TO WS-IS-YELLING
               END-IF

               EVALUATE WS-IS-QUESTION ALSO WS-IS-YELLING
                   WHEN 1 ALSO 1
                       MOVE "Calm down, I know what I'm doing!" TO WS-RESULT
                   WHEN 1 ALSO 0
                       MOVE "Sure." TO WS-RESULT
                   WHEN 0 ALSO 1
                       MOVE "Whoa, chill out!" TO WS-RESULT
                   WHEN 0 ALSO 0
                       MOVE "Whatever." TO WS-RESULT
               END-EVALUATE
           END-IF
           .