       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT     PIC 99 VALUE 0.
       01 WS-CATEGORY   PIC X(15).
       01 WS-DICE       PIC 9(5).
       01 WS-I          PIC 9 VALUE 1.
       
       *> Tach 5 con xuc xac
       01 WS-DICE-D1    PIC 9.
       01 WS-DICE-D2    PIC 9.
       01 WS-DICE-D3    PIC 9.
       01 WS-DICE-D4    PIC 9.
       01 WS-DICE-D5    PIC 9.
       
       *> Dem so luong tung mat (1-6)
       01 WS-COUNT-1    PIC 9 VALUE 0.
       01 WS-COUNT-2    PIC 9 VALUE 0.
       01 WS-COUNT-3    PIC 9 VALUE 0.
       01 WS-COUNT-4    PIC 9 VALUE 0.
       01 WS-COUNT-5    PIC 9 VALUE 0.
       01 WS-COUNT-6    PIC 9 VALUE 0.
       
       *> Bien tinh toan
       01 WS-TOTAL      PIC 99 VALUE 0.
       
       PROCEDURE DIVISION.
       YACHT.
           *> Reset ket qua
           MOVE 0 TO WS-RESULT
           MOVE 0 TO WS-COUNT-1
           MOVE 0 TO WS-COUNT-2
           MOVE 0 TO WS-COUNT-3
           MOVE 0 TO WS-COUNT-4
           MOVE 0 TO WS-COUNT-5
           MOVE 0 TO WS-COUNT-6
           
           *> Tach 5 con xuc xac
           MOVE WS-DICE(1:1) TO WS-DICE-D1
           MOVE WS-DICE(2:1) TO WS-DICE-D2
           MOVE WS-DICE(3:1) TO WS-DICE-D3
           MOVE WS-DICE(4:1) TO WS-DICE-D4
           MOVE WS-DICE(5:1) TO WS-DICE-D5
           
           *> Dem so luong tung mat
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               EVALUATE WS-DICE(WS-I:1)
                   WHEN '1' ADD 1 TO WS-COUNT-1
                   WHEN '2' ADD 1 TO WS-COUNT-2
                   WHEN '3' ADD 1 TO WS-COUNT-3
                   WHEN '4' ADD 1 TO WS-COUNT-4
                   WHEN '5' ADD 1 TO WS-COUNT-5
                   WHEN '6' ADD 1 TO WS-COUNT-6
               END-EVALUATE
           END-PERFORM
           
           *> Tinh tong tat ca
           COMPUTE WS-TOTAL = WS-DICE-D1 + WS-DICE-D2 + 
                              WS-DICE-D3 + WS-DICE-D4 + WS-DICE-D5
           
           *> Xu ly theo category
           EVALUATE WS-CATEGORY
               WHEN 'ones'
                   COMPUTE WS-RESULT = WS-COUNT-1 * 1
               
               WHEN 'twos'
                   COMPUTE WS-RESULT = WS-COUNT-2 * 2
               
               WHEN 'threes'
                   COMPUTE WS-RESULT = WS-COUNT-3 * 3
               
               WHEN 'fours'
                   COMPUTE WS-RESULT = WS-COUNT-4 * 4
               
               WHEN 'fives'
                   COMPUTE WS-RESULT = WS-COUNT-5 * 5
               
               WHEN 'sixes'
                   COMPUTE WS-RESULT = WS-COUNT-6 * 6
               
               WHEN 'full house'
                   PERFORM CHECK-FULL-HOUSE
               
               WHEN 'four of a kind'
                   PERFORM CHECK-FOUR-OF-A-KIND
               
               WHEN 'little straight'
                   PERFORM CHECK-LITTLE-STRAIGHT
               
               WHEN 'big straight'
                   PERFORM CHECK-BIG-STRAIGHT
               
               WHEN 'choice'
                   MOVE WS-TOTAL TO WS-RESULT
               
               WHEN 'yacht'
                   PERFORM CHECK-YACHT
           END-EVALUATE
           .

       CHECK-FULL-HOUSE.
           *> Full House: 3 of a kind + 2 of a kind
           IF (WS-COUNT-1 = 3 OR WS-COUNT-2 = 3 OR 
               WS-COUNT-3 = 3 OR WS-COUNT-4 = 3 OR 
               WS-COUNT-5 = 3 OR WS-COUNT-6 = 3)
               AND (WS-COUNT-1 = 2 OR WS-COUNT-2 = 2 OR 
                    WS-COUNT-3 = 2 OR WS-COUNT-4 = 2 OR 
                    WS-COUNT-5 = 2 OR WS-COUNT-6 = 2)
               MOVE WS-TOTAL TO WS-RESULT
           END-IF
           .

       CHECK-FOUR-OF-A-KIND.
           *> Four of a Kind: 4 con giong nhau
           IF WS-COUNT-1 >= 4
               COMPUTE WS-RESULT = 1 * 4
           ELSE IF WS-COUNT-2 >= 4
               COMPUTE WS-RESULT = 2 * 4
           ELSE IF WS-COUNT-3 >= 4
               COMPUTE WS-RESULT = 3 * 4
           ELSE IF WS-COUNT-4 >= 4
               COMPUTE WS-RESULT = 4 * 4
           ELSE IF WS-COUNT-5 >= 4
               COMPUTE WS-RESULT = 5 * 4
           ELSE IF WS-COUNT-6 >= 4
               COMPUTE WS-RESULT = 6 * 4
           END-IF
           .

       CHECK-LITTLE-STRAIGHT.
           *> Little Straight: 1-2-3-4-5
           IF WS-COUNT-1 = 1 AND WS-COUNT-2 = 1 AND 
              WS-COUNT-3 = 1 AND WS-COUNT-4 = 1 AND 
              WS-COUNT-5 = 1
               MOVE 30 TO WS-RESULT
           END-IF
           .

       CHECK-BIG-STRAIGHT.
           *> Big Straight: 2-3-4-5-6
           IF WS-COUNT-2 = 1 AND WS-COUNT-3 = 1 AND 
              WS-COUNT-4 = 1 AND WS-COUNT-5 = 1 AND 
              WS-COUNT-6 = 1
               MOVE 30 TO WS-RESULT
           END-IF
           .

       CHECK-YACHT.
           *> Yacht: 5 con giong nhau
           IF WS-COUNT-1 = 5 OR WS-COUNT-2 = 5 OR 
              WS-COUNT-3 = 5 OR WS-COUNT-4 = 5 OR 
              WS-COUNT-5 = 5 OR WS-COUNT-6 = 5
               MOVE 50 TO WS-RESULT
           END-IF
           .