       IDENTIFICATION DIVISION.
       PROGRAM-ID. RNA-TRANSCRIPTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COMPLEMENT     PIC X(60).
       01 WS-LEN            PIC 999.
       01 WS-I              PIC 999.
       01 WS-CHAR           PIC X.

       PROCEDURE DIVISION.
       RNA-TRANSCRIPTION.
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-COMPLEMENT TRAILING))
               TO WS-LEN
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-LEN
               MOVE WS-COMPLEMENT(WS-I:1) TO WS-CHAR
               EVALUATE WS-CHAR
                   WHEN 'G'
                       MOVE 'C' TO WS-COMPLEMENT(WS-I:1)
                   WHEN 'C'
                       MOVE 'G' TO WS-COMPLEMENT(WS-I:1)
                   WHEN 'T'
                       MOVE 'A' TO WS-COMPLEMENT(WS-I:1)
                   WHEN 'A'
                       MOVE 'U' TO WS-COMPLEMENT(WS-I:1)
               END-EVALUATE
           END-PERFORM
           .