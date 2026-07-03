       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVERSE-STRING.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STRING         PIC X(64).
       01 WS-TMP            PIC X.
       01 WS-LEN            PIC 99.
       01 WS-J              PIC 99.
       01 WS-I              PIC 99.
       01 WS-HALF           PIC 99.

       PROCEDURE DIVISION.
       REVERSE-STRING.
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-STRING TRAILING)) TO WS-LEN
           COMPUTE WS-HALF = WS-LEN / 2
           
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-HALF
               COMPUTE WS-J = WS-LEN - WS-I + 1
               MOVE WS-STRING(WS-I:1) TO WS-TMP
               MOVE WS-STRING(WS-J:1) TO WS-STRING(WS-I:1)
               MOVE WS-TMP TO WS-STRING(WS-J:1)
           END-PERFORM
           .