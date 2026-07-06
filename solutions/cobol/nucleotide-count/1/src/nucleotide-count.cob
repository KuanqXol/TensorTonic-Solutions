       IDENTIFICATION DIVISION.
       PROGRAM-ID. nucleotide-count.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DNA PIC X(128).
       01 WS-A PIC 9(4).
       01 WS-C PIC 9(4).
       01 WS-G PIC 9(4).
       01 WS-T PIC 9(4).
       01 WS-ERROR PIC X(36).

         01 I   PIC 99.
         01 WS-CHAR   PIC X.
         01 LEN   PIC 99.

       PROCEDURE DIVISION.
       NUCLEOTIDE-COUNT.
         MOVE 0 TO WS-A
         MOVE 0 TO WS-C
         MOVE 0 TO WS-G
         MOVE 0 TO WS-T
         MOVE SPACES TO WS-ERROR
         MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-DNA)) TO LEN
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN
            MOVE WS-DNA(I:1) TO WS-CHAR
            EVALUATE WS-CHAR
               WHEN 'A'
                  ADD 1 TO WS-A
               WHEN 'C'
                  ADD 1 TO WS-C
               WHEN 'G'
                  ADD 1 TO WS-G
               WHEN 'T'
                  ADD 1 TO WS-T
               WHEN OTHER
                  MOVE "ERROR: Invalid nucleotide in strand" TO WS-ERROR
                  EXIT PERFORM
            END-EVALUATE
         END-PERFORM
      

