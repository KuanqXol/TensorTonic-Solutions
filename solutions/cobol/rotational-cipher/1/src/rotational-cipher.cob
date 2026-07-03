       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROTATIONAL-CIPHER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-KEY          PIC 9(2).
       01 WS-TEXT         PIC X(128).
       01 WS-CIPHER       PIC X(128).
       01 WS-TEXT-LOW     PIC X(128).
       01 WS-CHAR         PIC X.
       01 WS-LEN          PIC 999.
       01 WS-I            PIC 999.
       01 WS-INDEX        PIC 999.

       PROCEDURE DIVISION.
       ROTATIONAL-CIPHER.
           MOVE SPACES TO WS-CIPHER
           MOVE FUNCTION LOWER-CASE(WS-TEXT) TO WS-TEXT-LOW
           MOVE FUNCTION LENGTH(WS-TEXT) TO WS-LEN

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE WS-TEXT-LOW(WS-I:1) TO WS-CHAR
               
               IF WS-CHAR >= 'a' AND WS-CHAR <= 'z'
                   COMPUTE WS-INDEX =
                       FUNCTION ORD(WS-CHAR) -
                       FUNCTION ORD('a') + WS-KEY
                   COMPUTE WS-INDEX =
                       FUNCTION MOD(WS-INDEX, 26) +
                       FUNCTION ORD('A')
                   MOVE FUNCTION CHAR(WS-INDEX)
                       TO WS-CIPHER(WS-I:1)
               ELSE
                   MOVE WS-CHAR TO WS-CIPHER(WS-I:1)
               END-IF
           END-PERFORM
           .