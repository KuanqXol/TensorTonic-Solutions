       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWO-FER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME             PIC X(16).
       01 WS-RESULT           PIC X(64).
       01 WS-LEN              PIC 99.

       PROCEDURE DIVISION.
       TWO-FER.
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-NAME TRAILING)) TO WS-LEN
           IF WS-LEN = 0
               MOVE 'you' TO WS-NAME
           END-IF
           MOVE SPACES TO WS-RESULT
           STRING 'One for '             DELIMITED BY SIZE
                  FUNCTION TRIM(WS-NAME) DELIMITED BY SIZE
                  ', one for me.'        DELIMITED BY SIZE
                  INTO WS-RESULT
           END-STRING
           .