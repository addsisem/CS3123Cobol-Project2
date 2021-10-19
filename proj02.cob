       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ02.
       AUTHOR. Addyson Sisemore
      * PROJECT  2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'NEWEMP'.
           SELECT PRNT-FILE ASSIGN TO 'UR-S-PRNT'.

       DATA DIVISION.

       FILE SECTION.
       FD INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01 INPUT-REC     PIC X(110).
       FD PRNT-FILE
           LABEL RECORDS ARE OMITTED.
       01 PRNT-REC      PIC X(125).
       WORKING-STORAGE SECTION.
       01 PAGE-CT       PIC 9999        VALUE '0001'.
       01 REC-CT        PIC 99          VALUE '00'.
       01 TOTAL-REC     PIC 9999        VALUE '0000'.
       01 H-EMP         PIC 9999.
       01 S-EMP         PIC 9999.
       01 AVG-H         PIC 9(6)V99.
       01 AVG-S         PIC 9(6)V99.
       01 WS-TYPE1      PIC 999.
       01 WS-TYPE2      PIC 999.
       01 WS-TYPE3      PIC 999.
       01 WS-TYPE4      PIC 999.
       01 WS-TYPE5      PIC 999.
       01 WS-TYPE6      PIC 999.
       01 WS-TYPE7      PIC 999.
       01 WS-TYPE8      PIC 999.
       01 WS-TYPE9      PIC 999.
       01 WS-TYPE10     PIC 999.
      **************************************************************
      * LAYOUT FOR THE INPUT FILE *
      **************************************************************
       01 INPUT-DATA.
         03 I-EID        PIC X(7).
         03 I-LAST       PIC X(15).
         03 I-FIRST      PIC X(15).
         03 I-TYPE       PIC X(2).
         03 I-TITLE      PIC X(17).
         03 I-SSN        PIC X(9).
         03 FILLER       PIC X(24)      VALUE '.'.
         03 I-DATE       PIC X(8).
         03 FILLER       PIC X(2)       VALUE SPACES.
         03 I-RATE       PIC 9(4)V99.
         03 I-STATUS     PIC X(1).
         03 FILLER       PIC X(4)       VALUE SPACES.
      **************************************************************
      * LAYOUT FOR THE 1ST DATA LINE OF REPORT PRNTING *
      **************************************************************
       01 PRNT-DATA1.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-SSN1        PIC XXXBXXBXXXX.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-LAST1       PIC X(15).
         03 FILLER        PIC X(1)              VALUE SPACES.
         03 L-FIRST1      PIC X(15).
         03 FILLER        PIC X(7)              VALUE SPACES.
         03 L-EID1        PIC X(7).
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-TITLE1      PIC X(17).
         03 FILLER        PIC X(5)              VALUE SPACES.
         03 L-TYPE1       PIC X(2).
         03 FILLER        PIC X(5)              VALUE SPACES.
         03 L-DATE1       PIC 99/99/9999.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-RATE1       PIC ZZZZ.99.
         03 FILLER        PIC X(2)              VALUE SPACES.
         03 L-STATUS1     PIC X(1).
      **************************************************************
      * LAYOUT FOR LAST PAGE OF REPORT PRINTING
      **************************************************************
       01 PRNT-LAST1.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        VALUE 'NUMBER OF EMPLOYEE RECORDS READ:'.
         03 FILLER        PIC X(10)     VALUE SPACES.
         03 L-TOTAL-REC1  PIC ZZZ9.

       01 PRNT-LAST2.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        VALUE 'NUMBER OF HOURLY EMPLOYEES:'.
         03 FILLER        PIC X(15)     VALUE SPACES.
         03 L-H-EMP1      PIC ZZZ9.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        VALUE 'AVERAGE HOURLY RATE:'.
         03 FILLER        PIC X(7)      VALUE SPACES.
         03 L-AVG-H-EMP   PIC $ZZ9.99.

       01 PRNT-LAST3.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        VALUE 'NUMBER OF SALARIED EMPLOYEES:'.
         03 FILLER        PIC X(13)     VALUE SPACES.
         03 L-S-EMP1      PIC ZZZ9.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        VALUE 'AVERAGE SALARIED RATE:'.
         03 FILLER        PIC X(5)      VALUE SPACES.
         03 L-AVG-S-EMP   PIC $ZZZ9.99.

       01 PRNT-LAST4.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        PIC X(11)     VALUE 'TYPE 1:'.
         03 L-LP-TYPE1    PIC ZZ9.
         03 FILLER        PIC X(3)      VALUE SPACES.
         03 FILLER        PIC X(11)     VALUE 'TYPE 2:'.
         03 L-LP-TYPE2    PIC ZZ9.
         03 FILLER        PIC X(3)      VALUE SPACES.
         03 FILLER        PIC X(11)     VALUE 'TYPE 3:'.
         03 L-LP-TYPE3    PIC ZZ9.
         03 FILLER        PIC X(3)      VALUE SPACES.
         03 FILLER        PIC X(11)     VALUE 'TYPE 4:'.
         03 L-LP-TYPE4    PIC ZZ9.
         03 FILLER        PIC X(3)      VALUE SPACES.
         03 FILLER        PIC X(11)     VALUE 'TYPE 5:'.
         03 L-LP-TYPE5    PIC ZZ9.

       01 PRNT-LAST5.
         03 FILLER        PIC X(2)      VALUE SPACES.
         03 FILLER        PIC X(11)     VALUE 'TYPE 6:'.
         03 L-LP-TYPE6    PIC ZZ9.
         03 FILLER        PIC X(3)      VALUE SPACES.
         03 FILLER        PIC X(11)     VALUE 'TYPE 7:'.
         03 L-LP-TYPE7    PIC ZZ9.
         03 FILLER        PIC X(3)      VALUE SPACES.
         03 FILLER        PIC X(11)     VALUE 'TYPE 8:'.
         03 L-LP-TYPE8    PIC ZZ9.
         03 FILLER        PIC X(3)      VALUE SPACES.
         03 FILLER        PIC X(11)     VALUE 'TYPE 9:'.
         03 L-LP-TYPE9    PIC ZZ9.
         03 FILLER        PIC X(3)      VALUE SPACES.
         03 FILLER        PIC X(11)     VALUE 'TYPE 10:'.
         03 L-LP-TYPE10   PIC ZZ9.
         03 FILLER        PIC X(3)      VALUE SPACES.
      **************************************************************
      * LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRINTING *
      **************************************************************
       01 PRNT-HEADING1.
         03 HL-CUR-YR   PIC 99.
         03 FILLER      PIC X(1)    VALUE '/'.
         03 HL-CUR-MO   PIC 99.
         03 FILLER      PIC X(1)    VALUE '/'.
         03 HL-CUR-DAY  PIC 99.
         03 FILLER      PIC X(51).
         03 FILLER      PIC X(28)   VALUE 'THE BEST IS YET TO COME,INC'.
         03 FILLER      PIC X(19)   VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'PAGE'.
         03 HL-PAGE-CT  PIC ZZZ9.
      **************************************************************
      * LAYOUT FOR THE 2ND HEADING LINE OF REPORT PRINTING *
      **************************************************************
       01 PRNT-HEADING2.
         03 FILLER   PIC X(58)  VALUE SPACES.
         03 FILLER   PIC X(31)  VALUE 'EMPLOYEE CLASSIFICATION AND PAY'.
      **************************************************************
      * LAYOUT FOR THE 4TH HEADING LINE OF REPORT PRINTING *
      **************************************************************
       01 PRNT-HEADING3.
         03 FILLER      PIC X(2)    VALUE SPACES.
         03 FILLER      PIC X(3)    VALUE 'SSN'.
         03 FILLER      PIC X(10)   VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'LAST'.
         03 FILLER      PIC X(12)   VALUE SPACES.
         03 FILLER      PIC X(5)    VALUE 'FIRST'.
         03 FILLER      PIC X(17)   VALUE SPACES.
         03 FILLER      PIC X(6)    VALUE 'EMP ID'.
         03 FILLER      PIC X(3)    VALUE SPACES.
         03 FILLER      PIC X(5)    VALUE 'TITLE'.
         03 FILLER      PIC X(16)   VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'TYPE'.
         03 FILLER      PIC X(4)    VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'DATE'.
         03 FILLER      PIC X(8)    VALUE SPACES.
         03 FILLER      PIC X(4)    VALUE 'RATE'.
         03 FILLER      PIC X(5)    VALUE SPACES.
         03 FILLER      PIC X(2)    VALUE 'ST'.
         03 FILLER      PIC X(2)    VALUE SPACES.
      **************************************************************
      * LAYOUT FOR DATE
      **************************************************************
       01 CUR-DATE.
          05      CUR-YR      PIC 99.
          05      CUR-MO      PIC 99.
          05      CUR-DAY     PIC 99.
       01 MISC.
      **************************************************************
      *       END OF FILE (EOF) SWITCHES *
      *       0 = NOT AT EOF 1 = AT EOF *
      **************************************************************
         03 EOF-I      PIC 9   VALUE 0.
      **************************************************************
      *       START OF PROCEDURE DIVISION       *
      **************************************************************
       PROCEDURE DIVISION.
           ACCEPT CUR-DATE FROM DATE.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
                OUTPUT PRNT-FILE.
           PERFORM 2000-READ-INPUT.
           PERFORM 1400-PRINT-HEAD.
           PERFORM 1500-LOOP
             UNTIL EOF-I = 1.
           PERFORM 1700-LAST-PAGE.
           CLOSE INPUT-FILE
             PRNT-FILE.
           STOP RUN.

       1400-PRINT-HEAD.
           MOVE CUR-YR TO HL-CUR-YR.
           MOVE CUR-MO TO HL-CUR-MO.
           MOVE CUR-DAY TO HL-CUR-DAY.
           MOVE PAGE-CT TO HL-PAGE-CT.
      /
           WRITE PRNT-REC FROM PRNT-HEADING1
             AFTER ADVANCING 1 LINE.
           ADD 1 TO PAGE-CT.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC.

           WRITE PRNT-REC FROM PRNT-HEADING2
             AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC.

           WRITE PRNT-REC FROM PRNT-HEADING3
             AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
             AFTER ADVANCING 1 LINE.

       1450-PRINT-HEAD.
           MOVE PAGE-CT TO HL-PAGE-CT

           WRITE PRNT-REC FROM PRNT-HEADING1
             AFTER ADVANCING 2 LINES.
           ADD 1 TO PAGE-CT.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC.

           WRITE PRNT-REC FROM PRNT-HEADING2
             AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC.

           IF EOF-I IS NOT EQUAL TO 1 THEN
             WRITE PRNT-REC FROM PRNT-HEADING3
               AFTER ADVANCING 1 LINE
             MOVE SPACES TO PRNT-REC
             WRITE PRNT-REC
             AFTER ADVANCING 1 LINE.

       1500-LOOP.
           PERFORM 1800-COUNT-STATUS.
           PERFORM 1850-COUNT-TYPE.
           PERFORM 1600-PRINT-DATA.
           ADD 1 TO REC-CT.
           PERFORM 2000-READ-INPUT.

           IF REC-CT IS EQUAL TO 10 THEN
              ADD REC-CT TO TOTAL-REC
              SUBTRACT 10 FROM REC-CT
              PERFORM 1450-PRINT-HEAD.
      **************************************************************
      * PRINTS THE SCHEDULE INFORMATION *
      **************************************************************
       1600-PRINT-DATA.
           MOVE I-SSN           TO L-SSN1.
           INSPECT L-SSN1 REPLACING ALL ' ' BY '-'.
           MOVE I-LAST          TO L-LAST1.
           MOVE I-FIRST         TO L-FIRST1.
           MOVE I-EID           TO L-EID1.
           MOVE I-TITLE         TO L-TITLE1.
           MOVE I-TYPE          TO L-TYPE1.
           MOVE I-DATE          TO L-DATE1.
           MOVE I-RATE          TO L-RATE1.
           MOVE I-STATUS        TO L-STATUS1.
             WRITE PRNT-REC FROM PRNT-DATA1
               AFTER ADVANCING 1 LINE.
      **************************************************************
      * PRINTS TOTALS AND AVERAGES TO THE LAST PAGE
      **************************************************************
       1700-LAST-PAGE.
           PERFORM 1450-PRINT-HEAD.
           ADD REC-CT TO TOTAL-REC.
           MOVE TOTAL-REC TO L-TOTAL-REC1.
           MOVE H-EMP TO L-H-EMP1.
           MOVE S-EMP TO L-S-EMP1.
           MOVE WS-TYPE1 TO L-LP-TYPE1.
           MOVE WS-TYPE2 TO L-LP-TYPE2.
           MOVE WS-TYPE3 TO L-LP-TYPE3.
           MOVE WS-TYPE4 TO L-LP-TYPE4.
           MOVE WS-TYPE5 TO L-LP-TYPE5.
           MOVE WS-TYPE6 TO L-LP-TYPE6.
           MOVE WS-TYPE7 TO L-LP-TYPE7.
           MOVE WS-TYPE8 TO L-LP-TYPE8.
           MOVE WS-TYPE9 TO L-LP-TYPE9.
           MOVE WS-TYPE10 TO L-LP-TYPE10.
           PERFORM 1900-CALC-AVERAGES.
             WRITE PRNT-REC FROM PRNT-LAST1
               AFTER ADVANCING 1 LINE.
             WRITE PRNT-REC FROM PRNT-LAST2
               AFTER ADVANCING 1 LINE.
             WRITE PRNT-REC FROM PRNT-LAST3
               AFTER ADVANCING 1 LINE.
             WRITE PRNT-REC FROM PRNT-LAST4
               AFTER ADVANCING 2 LINES.
             WRITE PRNT-REC FROM PRNT-LAST5
               AFTER ADVANCING 1 LINE.
      **************************************************************
      * COUNT # OF HOURLY VS SALARIED EMPLOYEES
      **************************************************************
       1800-COUNT-STATUS.
           IF I-STATUS IS EQUAL TO 'H' THEN
             ADD 1 TO H-EMP
             ADD I-RATE TO AVG-H.
           IF I-STATUS IS EQUAL TO 'S' THEN
             ADD 1 TO S-EMP
             ADD I-RATE TO AVG-S.
      **************************************************************
      * COUNT # OF EMPLOYEES IN EACH TYPE
      **************************************************************
       1850-COUNT-TYPE.
           IF I-TYPE IS EQUAL TO '01' THEN
             ADD 1 TO WS-TYPE1.
           IF I-TYPE IS EQUAL TO '02' THEN
             ADD 1 TO WS-TYPE2.
           IF I-TYPE IS EQUAL TO '03' THEN
             ADD 1 TO WS-TYPE3.
           IF I-TYPE IS EQUAL TO '04' THEN
             ADD 1 TO WS-TYPE4.
           IF I-TYPE IS EQUAL TO '05' THEN
             ADD 1 TO WS-TYPE5.
           IF I-TYPE IS EQUAL TO '06' THEN
             ADD 1 TO WS-TYPE6.
           IF I-TYPE IS EQUAL TO '07' THEN
             ADD 1 TO WS-TYPE7.
           IF I-TYPE IS EQUAL TO '08' THEN
             ADD 1 TO WS-TYPE8.
           IF I-TYPE IS EQUAL TO '09' THEN
             ADD 1 TO WS-TYPE9.
           IF I-TYPE IS EQUAL TO '10' THEN
             ADD 1 TO WS-TYPE10.
      **************************************************************
      * CALCULATE AVERAGE RATES
      **************************************************************
       1900-CALC-AVERAGES.
           DIVIDE AVG-H BY H-EMP
             GIVING L-AVG-H-EMP.
           DIVIDE AVG-S BY S-EMP
             GIVING L-AVG-S-EMP.
      **************************************************************
      * READS THE INPUT FILE *
      **************************************************************
       2000-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
             AT END MOVE 1 TO EOF-I.                                                                                                                                                                                                                                                               
                                                          
