000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.           CACBALUB.
000300 AUTHOR.               LEENA DASGUPTA
000400 DATE-WRITTEN.         1996-09-05.
000500****************************************************************
000080*   $Modtime::   10 Oct 2018 10:15:16  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CACBALUB.COV  $
000100*
000110*   Rev 1.0.1.10   10 Oct 2018 11:45:22   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.9   19 Apr 2011 17:27:22   tom
000160*LSM5259
000170*Replace call to CUTDTCAL
000180*
000190*   Rev 1.0.1.8   23 Sep 2005 11:41:18   robin
000200*FDM3514
000210*Module Impact Analysis Tool - WS-REVISION awk script fix.
000220*
000230*   Rev 1.0.1.7   19 Sep 2005 18:58:14   robin
000240*FDM3514
000250*Module Impact Analysis Tool
000260*
000270*   Rev 1.0.1.6   22 Nov 2004 18:27:22   gabriela
000280*FLM0001
000290*scheme level currency
000300*
000310*   Rev 1.0.1.5   20 Dec 2002 10:55:56   rieko
000320*FDM3175
000330*COBOL modifications to support the Oracle 8 pre-complier.
000340*
000350*   Rev 1.0.1.4   Mar 24 2000 17:42:28   miker
000360*FDM1959
000370*Expanded Keys project
*******
*******   Rev 1.0.1.3   10 Sep 1997 17:02:42   dan
*******lmm0984
*******
*******   Rev 1.0.1.2   10 Jul 1997 14:42:56   paulm
*******Mass Check In for Currency Field Changes
*******
*******   Rev 1.0.1.1   09 Jul 1997 13:50:42   paulm
*******FDM0786
*******
*******   Rev 1.0.1.0   May 03 21:09:50 1997   paulm
*******No ERF
*******
*******Initial Check In for FDP Branch
*******3 May 1997
*******
*******   Rev 1.0   03 May 1997 14:28:48   paulm
*******No ERF
*******
*******Initial Check-In   3 May 1997
000600* MONTH END ACCOUNT BALANCING
000700*
000800****************************************************************
000900* THIS PROGRAM CALCULATES BEGINING AND ENDING ACCOUNT BALANCES AND
001000* UPDATES/INSERTS INTO THE COMPANY_ACCTS TABLE THESE VALUES. IT ALSO
001100* UPDATES THE LAST ACCOUNT BALANCE DATE ON SYSTEM_DATA. IF A COMPANY
001101* ACCOUNT IS NOT FOUND FOR A LEDGER ACCOUNT, IT INSERTS A COMPANY
001102* ACCOUNT FOR THAT LEDGER ACCOUNT. THIS PROGRAM WILL BE RUN ON A
001110* MONTHLY BASIS IN BATCH. THIS PROGRAM IS CALLED BY THE SUPER STUB.
001120*
001200* THE FOLLOWING TABLES ARE UTILIZED AS INPUT:
001300* 1.) COMPANY_ACCTS
001400* 2.) SYSTEM_DATA
001500* 3.) LEDGER_ACCOUNTS
001600* 4.) JOURNAL_LOGS
001700*
001800****************************************************************
001900*    THE FOLLOWING ERROR CODES MAY BE RETURNED IN RTN-ERROR-CODE
002000*    FROM THIS PROGRAM...
002100*
002200*    999X SEVERE SQL ERROR IN RISK PROCESSING
002300*
002400****************************************************************
002500*                     CHANGES LOG
002600****************************************************************
002700*
002800*   DATE     PRG   PROJ#      DESCRIPTION
002900*  --------  --- ---------  ----------------------------------------
003000*  10-04-96  LJD DVAC001    DATABASE CHANGES
003010*  11-04-96  LJD DVAC001    ADDED LOGIC TO UPDATE NEXT_ZERO_DT
003100*                           ON LEDGER_ACCOUNTS
003110*  03-21-97  DEW AFI0038    DATABASE CHANGES TO LEDGER_ACCOUNTS
003120*                           CO-BAL-CD, CASE-BAL-CD, AND PROD-BAL-CD
003130*                           WERE REMOVED AND BAL-LVL-CD WAS ADDED
003110*  09-10-97  DEW LMM0984    CHANGED PROGRAM TO USE MONTH END DATES
000930* 17-NOV-04  GVC  FLM0001   ADD EDIT FOR NEW SCHEME CURRENCY CD
003200*
003300****************************************************************
003400 ENVIRONMENT DIVISION.
003500 CONFIGURATION SECTION.
003600*
003700 DATA DIVISION.
003800 WORKING-STORAGE SECTION.
003900
004000 01 BEG-WORK-STOR.
004100    05 FILLER                             PIC X(40) VALUE
004200       'THIS IS THE BEGINNING OF WORKING STORAGE'.
004300
004400 01  WS-PGM-NAME                          PIC X(8)
004500                                          VALUE 'CACBALUB'.
001080
001090 01  WS-COMPILED                   PIC X(20).                     FLM0001
004600
004700 01  WS-SUBSCRIPTS.
004900     05  IACC                             PIC S9(4) COMP.
004600
005100 01  WS-MISC-VARIABLES.
006400     05  WS-NXTMNTH-ACCT-BAL-DT           PIC X(10).
006400     05  WS-CURRENT-ACCT-BAL-DT.
006500         10  WS-ISO-YYYY2                 PIC 9999.
006600         10  FILLER                       PIC X.
006700         10  WS-ISO-MM2                   PIC 99.
006800         10  FILLER                       PIC X.
006900         10  WS-ISO-DD2                   PIC 99.
006902
006901     05  WS-YRS-DIFF                      PIC S9(4)V9(5) COMP-3.
006902
007001     05  WS-CO-ACCT-FLAG             PIC X(1) VALUE SPACES.
007002         88 CREATE-CO-ACCT                VALUE 'Y'.
007003         88 NO-CREATE-CO-ACCT             VALUE 'N'.
005000
004700     05  WS-BALANCE-LEVEL            PIC X(2) VALUE SPACES.
004900         88 LEDGER-LEVEL                  VALUE '01'.
004900         88 COMPANY-LEVEL                 VALUE '02'.
004900         88 PRODUCT-LEVEL                 VALUE '03'.
004900         88 SCHEME-LEVEL                  VALUE '04'.
001340
001350
001360 01  WS-ERROR-MESSAGES.                                           FLM0001
001370     05  WS-ERR-MSG1.                                             FLM0001
001380         10  FILLER                       PIC X(15)               FLM0001
001390             VALUE 'BEG BAL DATE: '.                              FLM0001
001400         10  WS-ERR-BEG-BAL-DT            PIC X(10).              FLM0001
001410
001420     05  WS-ERR-MSG2.                                             FLM0001
001430         10  FILLER                       PIC X(15)               FLM0001
001440             VALUE 'END BAL DATE: '.                              FLM0001
001450         10  WS-ERR-END-BAL-DT            PIC X(10).              FLM0001
001460
007005
007010*    SYSTEM DATA COPYBOOK
007100     COPY CL1SYSDD.
007110*    LEDGER ACCOUNT COPYBOOK
007200     COPY CL2ACCTD.
007201*    LEDGER ACCOUNT COPYBOOK
007202     COPY CL1ACCTD.
007210*    COMPANY ACCOUNTS COPYBOOK
007300     COPY CL1COACD.
001560**   JOURNAL_LOGS                                                 FLM0001
001570     COPY CL1JLOGD.                                               FLM0001
007301*    JOURNAL LOG COPYBOOK
007310     COPY CL3JLOGD.
007311*    DATE CALCULATION COPYBOOK
007320     COPY CWSDTCAL.
007400
001630**  REVISION AREA **                                              FDM3514
001640 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
001650     ' $Revision::   1.0.1.10       $'.                           
007500 LINKAGE SECTION.
007600*    RMESSAGE AREA
007700     COPY CLKFDPNC.
007800*    CASE  DATA COPYBOOK
007900     COPY CL1CASDD.
008000*    STANDARD ERROR BLOCK
008100     COPY CLKERRBC.
008200
008300*********************************************************
008400**   P R O C E D U R E    D I V I S I O N              **
008500*********************************************************
008600
008700 PROCEDURE DIVISION USING RMESSAGE,
008800                          CASD1-DATA,
008900                          RTN-OUTPUT-FIELDS.
009000
001820     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514                                             FLM0001
001830        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
001840        MOVE WHEN-COMPILED TO                                     FDM3514
001850             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
001860        MOVE WS-REVISION (16:10) TO                               FDM3514
001870             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
001880        MOVE 'CACBALUB' TO                                        FDM3514
001890           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
001900        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
001910                              RTN-OUTPUT-FIELDS                   FDM3514
001920     END-IF.                                                      FDM3514
009000
009100******************************************************************
009200**                MAIN ROUTINE                                  **
009300******************************************************************
009400
009500     PERFORM A000-INITIALIZE
009600        THRU A000-EXIT.
009700
002010     IF RTN-NO-ERROR                                              FLM0001
002020*       CHECK FOR MULTI-CURRENCY                                  FLM0001
002030        PERFORM A120-CHECK-ISO-CURR                               FLM0001
002040           THRU A120-EXIT                                         FLM0001
002050     END-IF.                                                      FLM0001
002060
002070     IF RTN-NO-ERROR                                              FLM0001
002080        PERFORM B000-MAIN
002090           THRU B000-MAIN-EXIT                                    FLM0001
002100     END-IF.                                                      FLM0001
002110
010000
010100     GOBACK.
010200
010300 A000-INITIALIZE.
009100******************************************************************
009200**
009300******************************************************************
011410*    GET LAST MONTH ACCOUNT BALANCE DATE FROM SYSTEM_DATA
011600     CALL 'CS1SYSDD' USING RMESSAGE,
011700                           SYSD1-DATA,
011800                           RTN-OUTPUT-FIELDS.
002230
002240* VALIDATE THE LAST MONTH ACCOUNT BALANCE DATE FROM SYSTEM_DATA
020300* IT SHOULD BE A MONTH END DATE
002260     IF RTN-NO-ERROR
002270        CALL 'CS2SYSDD' USING RMESSAGE,
002280                              SYSD1-DATA,
002290                              RTN-OUTPUT-FIELDS
002300     END-IF.
020400**
011900
012020*    CALCULATE NEXT MONTH END DATE, SAVE IN WORKING STORAGE
012020*    THIS DATE WILL BE USED FOR THE CURRENT MONTH BALANCING
012030     IF RTN-NO-ERROR
012040        INITIALIZE DATEPARMS
012050        MOVE 110 TO REQUEST-TYPE
012060        MOVE SYSD1-LAST-ACCT-BAL-DT TO INDATE
012070        MOVE +1 TO MONTHS-DAY-CHG
012080
002410        CALL 'CUTDTCAB' USING RMESSAGE,                           LSM5259
002420                              DATEPARMS,                          LSM5259
002430                              RTN-OUTPUT-FIELDS                   LSM5259
012091
012092        IF LK-ERROR-CD NOT = ZERO
012093           MOVE 'UT'        TO RTN-ERR-PREFIX
012094           MOVE 1100        TO RTN-ERROR-CODE
012095           MOVE 'CACBALUB'  TO RTN-PROG-NAME
002490           MOVE 'A000-INIT' TO RTN-PARA-NAME                      FLM0001
012097           CALL 'CUTSVARB' USING RMESSAGE,
012098                                RTN-OUTPUT-FIELDS
012099           DISPLAY 'ERROR IN DATE CALCULATION PROGRAM'
012100           DISPLAY 'CALLED FROM CACBALUB'
012101        ELSE
012102           MOVE ODATE TO WS-CURRENT-ACCT-BAL-DT
012110        END-IF
012030     END-IF.
012200
012020*    CALCULATE MONTH END DATE TWO MONTHS AHEAD, SAVE IN WORKING
012020*    STORAGE.  THIS DATE WILL BE USED FOR SETTING UP COMPANY
012020*    ACCOUNTS FOR NEXT MONTH'S BALANCING JOB
012030     IF RTN-NO-ERROR
012040        INITIALIZE DATEPARMS
012050        MOVE 110 TO REQUEST-TYPE
012060        MOVE WS-CURRENT-ACCT-BAL-DT TO INDATE
012070        MOVE +1 TO MONTHS-DAY-CHG
012080
002680        CALL 'CUTDTCAB' USING RMESSAGE,                           LSM5259
002690                              DATEPARMS,                          LSM5259
002700                              RTN-OUTPUT-FIELDS                   LSM5259
012091
012092        IF LK-ERROR-CD NOT = ZERO
012093           MOVE 'UT'        TO RTN-ERR-PREFIX
012094           MOVE 1100        TO RTN-ERROR-CODE
012095           MOVE 'CACBALUB'  TO RTN-PROG-NAME
002760           MOVE 'A000-INIT' TO RTN-PARA-NAME                      FLM0001
012097           CALL 'CUTSVARB' USING RMESSAGE,
012098                                RTN-OUTPUT-FIELDS
012099           DISPLAY 'ERROR IN DATE CALCULATION PROGRAM'
012100           DISPLAY 'CALLED FROM CACBALUB'
012101        ELSE
012102           MOVE ODATE TO WS-NXTMNTH-ACCT-BAL-DT
012110        END-IF
012030     END-IF.
010700
010800 A000-EXIT.
010900     EXIT.
011000
002890
002900
002910**********************************************************        FLM0001
002920*  CHECK FOR MULTIPLE CURRENCY CODES ON JLOGS TO BALANCE          FLM0001
002930**********************************************************        FLM0001
002940
002950 A120-CHECK-ISO-CURR.                                             FLM0001
002960
002970     INITIALIZE JLOG1-DATA.                                       FLM0001
002980     MOVE SYSD1-LAST-ACCT-BAL-DT TO JLOG1-BEGIN-DT.               FLM0001
002990     MOVE WS-CURRENT-ACCT-BAL-DT TO JLOG1-END-DT.                 FLM0001
003000     CALL 'CS6JLOGD' USING RMESSAGE,                              FLM0001
003010                           JLOG1-DATA,                            FLM0001
003020                           RTN-OUTPUT-FIELDS.                     FLM0001
003030
003040     IF RTN-NO-ERROR                                              FLM0001
003050        IF JLOG1-ROWS-FOUND > 1                                   FLM0001
003060           MOVE 'UT'    TO RTN-ERR-PREFIX                         FLM0001
003070           MOVE 317     TO RTN-ERROR-CODE                         FLM0001
003080           MOVE 'CACBALUB' TO RTN-PROG-NAME                       FLM0001
003090           MOVE 'A120'  TO RTN-PARA-NAME                          FLM0001
003100           MOVE SYSD1-LAST-ACCT-BAL-DT TO WS-ERR-BEG-BAL-DT       FLM0001
003110           MOVE WS-CURRENT-ACCT-BAL-DT TO WS-ERR-END-BAL-DT       FLM0001
003120           MOVE WS-ERR-MSG1          TO RTN-VAR-TEXT   (1)        FLM0001                             000167300
003130           MOVE 1                    TO RTN-VAR-SEQ-NO (1)        FLM0001                           000167400
003140           MOVE WS-ERR-MSG2          TO RTN-VAR-TEXT   (2)        FLM0001                             000167500
003150           MOVE 2                    TO RTN-VAR-SEQ-NO (2)        FLM0001                           000167600
003160           MOVE 2                    TO RTN-VAR-DETL-CNT          FLM0001
003170           CALL 'CUTSVARB' USING RMESSAGE,                        FLM0001
003180                                 RTN-OUTPUT-FIELDS                FLM0001
003190        END-IF                                                    FLM0001
003200     END-IF.                                                      FLM0001
003210
003220 A120-EXIT.                                                       FLM0001
003230     EXIT.
003240**********************************************************
011100
011200 B000-MAIN.
009100******************************************************************
009200**
009300******************************************************************
015110*    START LOOP TO PROCESS JOURNAL LOG ENTRIES FOR EACH ACCOUNT
015130     INITIALIZE ACCT2-DATA.
015140
015150     PERFORM UNTIL ACCT2-END-FLAG = 'Y'
015160     OR RTN-ERROR
015170
015190*       FETCH NEXT BLOCK OF LEDGER_ACCOUNTS
015191*       GET ALL ACCOUNT NUMBERS FROM LEDGER_ACCOUNTS
015192
015193        CALL 'CF1ACCTD'    USING RMESSAGE,
015194                                 ACCT2-DATA,
015195                                 RTN-OUTPUT-FIELDS
015196
015208**      PROCESS EACH ACCOUNT
015210        PERFORM VARYING IACC FROM 1 BY 1
015211        UNTIL IACC > ACCT2-ROWS-FOUND OR RTN-ERROR
015212
015226**      THE COMPANY, PRODUCT AND SCHEME
015227**      BALANCING ARE NOT AVAILABLE IN THIS RELEASE  (TX 1001)
015228**
015229**      01 = LEDGER ACCOUNT BALANCING
015230           MOVE ACCT2-BAL-LVL-CD (IACC) TO WS-BALANCE-LEVEL
015230           IF LEDGER-LEVEL
015231              CONTINUE
015232           ELSE
015233**            ISSUE A WARNING AND FALL THRU TO END-PERFORM
015233              MOVE 'TX'              TO RTN-ERR-PREFIX
015234              MOVE 1001              TO RTN-ERROR-CODE
015235              MOVE 'CACBALUB'        TO RTN-PROG-NAME
015236              MOVE 'B000-MAIN '      TO RTN-PARA-NAME
015237              CALL 'CUTSVARB' USING RMESSAGE,
015238                                    RTN-OUTPUT-FIELDS
015239           END-IF
015240
015244           IF RTN-NO-ERROR  AND  LEDGER-LEVEL
015241              PERFORM B100-GET-COMPANY-ACCTS
015242                 THRU B100-EXIT
015249           END-IF
015243
015244           IF RTN-NO-ERROR  AND  LEDGER-LEVEL
015245              IF COAC1-ROWS-FOUND = ZERO
015246                 SET CREATE-CO-ACCT  TO TRUE
015247              ELSE
015248                 SET NO-CREATE-CO-ACCT  TO TRUE
015249           END-IF
015250
015251           IF RTN-NO-ERROR AND CREATE-CO-ACCT AND LEDGER-LEVEL
015252              PERFORM B110-INSERT-COMPANY-ACCTS
015253                 THRU B110-EXIT
015254           END-IF
015255
015256           IF RTN-NO-ERROR AND LEDGER-LEVEL
015257              PERFORM B125-SUM-JLOG-ENTRIES
015258                 THRU B125-EXIT
015259           END-IF
015260
015261           IF RTN-NO-ERROR AND LEDGER-LEVEL
015262              PERFORM B150-UPDATE-COMPANY-ACCTS
015263                 THRU B150-EXIT
015264           END-IF
015265
015266           IF RTN-NO-ERROR AND LEDGER-LEVEL
015267              PERFORM B175-CREATE-NXTMNTH-ACCTS
015268                 THRU B175-EXIT
015269           END-IF
015270
015271        END-PERFORM
015273     END-PERFORM.
015274
015266     IF RTN-NO-ERROR
015275*       UPDATE SYSTEM_DATA WITH THE NEW ACCOUNT BALANCE DATE
015277        MOVE WS-CURRENT-ACCT-BAL-DT TO SYSD1-LAST-ACCT-BAL-DT
015278
015279        CALL 'CU1SYSDD' USING RMESSAGE,
015280                              SYSD1-DATA,
015281                              RTN-OUTPUT-FIELDS
015282
015269     END-IF.
015282
004090 B000-MAIN-EXIT.                                                  FLM0001
015300     EXIT.
015400
015410 B100-GET-COMPANY-ACCTS.
009100******************************************************************
015501*    GET THE COMPANY_ACCTS  TABLE  FOR THE ACCOUNT KEY FROM
015502*    LEDGER_ACCOUNTS
009300******************************************************************
015504     MOVE ACCT2-ACCT-KEY (IACC)   TO  COAC1-ACCT-KEY.
015506     MOVE WS-CURRENT-ACCT-BAL-DT  TO  COAC1-ACCT-BAL-DT.
015508     CALL 'CS1COACD' USING RMESSAGE,
015509                           COAC1-DATA,
015510                           RTN-OUTPUT-FIELDS.
015511
015523 B100-EXIT.
015524     EXIT.
015525
015526 B110-INSERT-COMPANY-ACCTS.
009100******************************************************************
015528*    INSERT A NEW ROW INTO COMPANY_ACCTS WITH THE CURRENT ACCOUNT
015529*    BALANCE DATE, AND ZEROES FOR THE BEGINING AND ENDING BALANCES
015530*    IF A COMPANY ACCOUNT IS NOT FOUND FOR A LEDGER ACCOUNT
009300******************************************************************
015532     MOVE ACCT2-ACCT-KEY (IACC)       TO  COAC1-ACCT-KEY.
015533     MOVE ZEROES                      TO  COAC1-END-BAL-AMT.
015534     MOVE ZEROES                      TO  COAC1-BEG-BAL-AMT.
015535     MOVE WS-CURRENT-ACCT-BAL-DT      TO  COAC1-EFF-DT.
015536
015537     CALL 'CI1COACD' USING RMESSAGE,
015538                           COAC1-DATA,
015539                           RTN-OUTPUT-FIELDS.
015540
015541 B110-EXIT.
015542     EXIT.
015544
015545 B125-SUM-JLOG-ENTRIES.
009100******************************************************************
015547*    SUM ALL THE DEBITS AND CREDITS BETWEEN THE LAST MONTH
015548*    ACCOUNT BALANCE DATE(FROM SYSTEM_DATA) AND THE NEW
015549*    ACCOUNT BALANCE DATE(FROM WORKING STORAGE)
009100******************************************************************
015551     MOVE  ACCT2-ACCT-KEY (IACC)   TO  JLOG3-ACCT-KEY.
015552     MOVE  SYSD1-LAST-ACCT-BAL-DT  TO  JLOG3-LAST-ACCT-BAL-DT1.
015553     MOVE  WS-CURRENT-ACCT-BAL-DT  TO  JLOG3-NEW-ACCT-BAL-DT.
015554
015555     CALL 'CS1JLOGD' USING RMESSAGE,
015556                           JLOG3-DATA,
015557                           RTN-OUTPUT-FIELDS.
015558
015560 B125-EXIT.
015561     EXIT.
015570
015580**
015581 B150-UPDATE-COMPANY-ACCTS.
009100******************************************************************
015583*    UPDATE COMPANY_ACCTS WITH THE ENDING BALANCE AMOUNT
009100******************************************************************
015585     MOVE ACCT2-ACCT-KEY (IACC)         TO  COAC1-ACCT-KEY.
015587     MOVE WS-CURRENT-ACCT-BAL-DT        TO  COAC1-EFF-DT.
015588
015589*    COMPUTE ENDING BALANCE AMOUNT
015590     COMPUTE COAC1-TOT-END-BAL-AMT = COAC1-BEG-BAL-AMT
015591                                     + JLOG3-DB-CR-TOTAL.
015592
015593     CALL 'CU1COACD' USING RMESSAGE,
015594                           COAC1-DATA,
015595                           RTN-OUTPUT-FIELDS.
015596
015597 B150-EXIT.
015598     EXIT.
015599
015600 B175-CREATE-NXTMNTH-ACCTS.
009100******************************************************************
015602*    INSERT A NEW ROW INTO COMPANY_ACCTS WITH THE NEW ACCOUNT
015603*    BALANCE DATE AND BEGINING BALANCE AMOUNT(THIS IS THE END
015604*    BALANCE AMOUNT FROM THE PREVIOUS MONTH'S ACCOUNT BALANCING)
009100******************************************************************
015606     MOVE ACCT2-ACCT-KEY (IACC)       TO  COAC1-ACCT-KEY.
015608     MOVE ZEROES                      TO  COAC1-END-BAL-AMT.
015609     MOVE COAC1-TOT-END-BAL-AMT       TO  COAC1-BEG-BAL-AMT.
015610     MOVE WS-NXTMNTH-ACCT-BAL-DT      TO  COAC1-EFF-DT.
015611
015614*    IF THE DIFFERENCE BETWEEN THE NEXT ZERO DATE IN LEDGER
015615*    ACCOUNTS AND THE NEW ACCOUNT BALANCE DATE IS LESS THAN 1 YR,
015616*    UPDATE THE NEXT ZERO DATE IN LEDGER ACCOUNTS
015617
015618     IF ACCT2-NEXT-ZERO-DT(IACC) > SPACES
015619        IF ACCT2-NEXT-ZERO-DT(IACC) <=  WS-CURRENT-ACCT-BAL-DT
015644*          UPDATE THE NEXT ZERO DATE (ADVANCE 1 YEAR)
015644           PERFORM  B200-UPDATE-LEDGER-ACCTS
015645              THRU B200-EXIT
015620*          SET BEGINNING BALANCE FOR NEXT MONTH COAC TO ZERO
015620           MOVE ZEROES       TO  COAC1-BEG-BAL-AMT
015646        END-IF
015647     END-IF.
015648
015649     CALL 'CI1COACD' USING RMESSAGE,
015650                           COAC1-DATA,
015651                           RTN-OUTPUT-FIELDS.
015652
015653 B175-EXIT.
015654     EXIT.
015655
015660 B200-UPDATE-LEDGER-ACCTS.
009100******************************************************************
009100* DAL WILL ADVANCE DATE BY 12 MONTHS AND UPDATE IT
009100******************************************************************
015662     INITIALIZE ACCT1-DATA.
015663     MOVE ACCT2-ACCT-KEY (IACC)       TO  ACCT1-ACCT-KEY.
015664
015665     CALL 'CU1ACCTD' USING RMESSAGE,
015666                           ACCT1-DATA,
015667                           RTN-OUTPUT-FIELDS.
015668
015670 B200-EXIT.
015680     EXIT.
015900
015690**
015700**   E N D     O F    P R O G R A M
015800**
