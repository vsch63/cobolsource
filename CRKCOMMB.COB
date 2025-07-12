000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.
000040 PROGRAM-ID.           CRKCOMMB.
000050 AUTHOR.               ERIC MILLER.
000060 DATE-WRITTEN.         14-NOV-08.
000070****************************************************************
000080*   $Modtime::   10 Oct 2018 17:06:48                          $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CRKCOMMB.COV      $
000100*
000110*   Rev 1.0.1.3   10 Oct 2018 17:29:34   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.2   03 Apr 2009 15:31:20   paul
000160*LPM0675
000170*Process/Cancel Wrapups by Benefit
000180*
000190*   Rev 1.0.1.1   30 Mar 2009 08:23:56   paramita
000200*LPM0629
000210*Notional Comms
000220*
000230* NEW STEP PROGRAM TO DETERMINE COMMISSION FOR BENEFIT DECRESE
000240******************************************************************
000250*    REQUIRES: FDP-REF-KEY
000260
000270******************************************************************
000280*                           CHANGES LOG
000290******************************************************************
000300*   DATE    PRG  PROJ#      DESCRIPTION
000310* --------- --- -------  -----------------------------------------
000320* 24-MAR-09 PAR LPM0629  NEW STEP PROGRAM
000330* 21-JAN-09 PDT LPM0675  ADD LOGIC TO INSERT BNTP SESSION VARIABLE
000340*                        ADD LOGIC FOR MULTIPLE BENEFITS
000350*                        (LPM0720).
000360******************************************************************
000370 ENVIRONMENT DIVISION.
000380 CONFIGURATION SECTION.
000390
000400 DATA DIVISION.
000410 WORKING-STORAGE SECTION.
000420
000430 01  MISC-VARS.
000440     05  WS-PROG-NM                PIC X(8) VALUE 'CRKCOMMB'.
000450
000460 01  WS-VARIABLES.
000470     05  SAVE-FDP-CASE-MBR-KEY     PIC S9(15) COMP-3.
000480     05  SAVE-FDP-EFF-DT           PIC X(10).
000490     05  WS-SENTENCE-AMT           PIC S9(15) COMP-3.
000500     05  WS-KEY                    PIC  X(15).
000510     05  WS-LPRL-KEY REDEFINES WS-KEY PIC S9(15).
000520
000530 01  WS-FLAGS.
000540     05  PERFORM-MS-DQ-FLAG        PIC X.
000550         88  PERFORM-MS-DQ         VALUE 'Y'.
000560         88  DO-NOT-PERFORM-MS-DQ  VALUE 'N'.
000570* COPY BOOKS
000580*    SNTCLK-DATA
000590     COPY CLKSNTCC.
000600*
000610     COPY CL1SNRLD.
000620*
000630     COPY CL1LPATD.
000640*
000650     COPY CL2SCAKD.
000660*
000670     COPY CL1LPTRD.
000680* SESSION VARIABLE
000690     COPY CL1SESSD.
000700* GTMP_SESS_AREA                                                  LPM0720
000710     COPY CL1GTSAD.                                               LPM0720
000720
000730**  REVISION AREA **
000740 01  WS-REVISION           PIC X(35)  VALUE
000750     ' $Revision::   1.0.1.3        $'.
000760 LINKAGE SECTION.
000770     COPY CLKFDPNC.
000780     COPY CL1CASDD.
000790     COPY CLKERRBC.
000800
000810******************************************************************
000820**         P R O C E D U R E    D I V I S I O N        **
000830******************************************************************
000840
000850 PROCEDURE DIVISION USING RMESSAGE,
000860                          CASD1-DATA,
000870                          RTN-OUTPUT-FIELDS.
000880
000890     IF FDP-DEBUG-RUNSTAT > '00'
000900        ADD +1                       TO RTN-DIMT-CNT
000910        MOVE WHEN-COMPILED           TO
000920             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)
000930        MOVE WS-REVISION (16:10)     TO
000940             RTN-DIMT-REVISION (RTN-DIMT-CNT)
000950        MOVE WS-PROG-NM              TO
000960           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)
000970        CALL 'CUTDIMTD' USING RMESSAGE,
000980                              RTN-OUTPUT-FIELDS
000990     END-IF.
001000
001010******************************************************************
001020*                       MAIN ROUTINE
001030******************************************************************
001040 MAIN-ROUTINE.
001050
001060     PERFORM A100-EDIT-LINKAGE
001070        THRU A100-EXIT.
001080
001090     IF RTN-NO-ERROR
001100        PERFORM A200-GET-SESSION-VARIABLE
001110           THRU A200-EXIT
001120     END-IF.
001130
001140     IF RTN-NO-ERROR AND PERFORM-MS-DQ
001150        PERFORM A300-CALC
001160           THRU A300-EXIT
001170     END-IF.
001180
001190     GOBACK.
001200
001210 MAIN-ROUTINE-EXIT.
001220     EXIT.
001230**
001240**         E N D     O F    P R O G R A M
001250**
001260******************************************************************
001270*    EDIT INPUT LINKAGE DATA
001280******************************************************************
001290 A100-EDIT-LINKAGE.
001300
001310* DEFINED BY BENEFIT DECREASE (BNTP-KEY)
001320     IF FDP-REF-KEY <= ZEROES
001330        MOVE 'UT'                    TO RTN-ERR-PREFIX
001340        MOVE 32                      TO RTN-ERROR-CODE
001350        MOVE 'A100'                  TO RTN-PARA-NAME
001360        PERFORM Z999-ERROR
001370            THRU Z999-EXIT
001380     END-IF.
001390
001400 A100-EXIT.
001410     EXIT.
001420
001430******************************************************************
001440*     GET SESSION VARIABLE
001450******************************************************************
001460 A200-GET-SESSION-VARIABLE.
001470     SET DO-NOT-PERFORM-MS-DQ TO TRUE
001480     INITIALIZE SESS1-DATA.
001490
001500     MOVE 'GET'                      TO SESS1-TYP-CD.
001510     MOVE 'LPM0629NETSURRENDERVALUE' TO SESS1-VAR-NAME.
001520
001530     PERFORM U100-ACCESS-SESSION-VAR
001540         THRU U100-EXIT.
001550     IF RTN-NO-ERROR
001560* SESSION VARIABLE CONTAINS LPRL-KEY
001570         EVALUATE TRUE
001580             WHEN SESS1-VAR-VALUE = 'NOT SET'
001590* IF NOT FOUND, THEN NOT CALLED FROM LAPSE OR NO DQ OR NO MS
001600* ACTIVITY SETUP, EXIT PROGRAM
001610                 SET DO-NOT-PERFORM-MS-DQ TO TRUE
001620             WHEN OTHER
001630                 SET PERFORM-MS-DQ    TO TRUE
001640                 MOVE SESS1-VAR-VALUE TO WS-KEY
001650         END-EVALUATE
001660     END-IF.
001670     IF PERFORM-MS-DQ
001680         INITIALIZE SCAK2-DATA
001690         MOVE FDP-SCAT-KEY TO SCAK2-SCAT-KEY (1)
001700         MOVE 2            TO SCAK2-DEF-BY-NUM (1)
001710         CALL 'CB1SCAKD' USING RMESSAGE,
001720                               SCAK2-DATA,
001730                               RTN-OUTPUT-FIELDS
001740         IF RTN-NO-ERROR AND SCAK2-ROWS-FOUND > 1
001750* MAKE SURE ONLY ONE BNTP SCAK (CB1SCAKD), IF MORE THAN ONE, ERROR
001760             MOVE 'RK'               TO RTN-ERR-PREFIX
001770             MOVE 980                TO RTN-ERROR-CODE
001780             MOVE 'A200'             TO RTN-PARA-NAME
001790             PERFORM Z999-ERROR
001800                 THRU Z999-EXIT
001810         END-IF
001820     END-IF.
001830 A200-EXIT.
001840     EXIT.
001850******************************************************************
001860 A300-CALC.
001870
001880* INITIALIZE SESSION VARIABLE                                     LPM0720
001890     INITIALIZE SESS1-DATA.                                       LPM0720
001900     MOVE 'SET'                TO SESS1-TYP-CD.                   LPM0720
001910     MOVE 'LPM0629COMMISSION'  TO SESS1-VAR-NAME.                 LPM0720
001920     MOVE ' '                  TO SESS1-VAR-VALUE.                LPM0720
001930                                                                  LPM0720
001940     PERFORM U100-ACCESS-SESSION-VAR                              LPM0720
001950        THRU U100-EXIT.                                           LPM0720
001960
001970     MOVE ZEROS                TO WS-SENTENCE-AMT.
001980
001990     PERFORM B100-SELECT-LPRL
002000        THRU B100-EXIT.
002010
002020     IF RTN-NO-ERROR AND LPAT1-ROWS-FOUND > 0                     LPM0675
002030         INITIALIZE SESS1-DATA
002040         MOVE 'SET'                TO SESS1-TYP-CD
002050         MOVE 'LPM0629COMMISSION'  TO SESS1-VAR-NAME
002060
002070         EVALUATE TRUE
002080******************************************************************
002090**(I.E. THIS IS A 100% REDEMPTION CAUSING BENEFIT TO GO TO ZERO) *
002100******************************************************************
002110             WHEN WS-SENTENCE-AMT = 0
002120******************************************************************
002130*      1 = (BYPASS CREATION OF COMMISSION RDS)
002140******************************************************************
002150                 MOVE '1'          TO SESS1-VAR-VALUE
002160             WHEN WS-SENTENCE-AMT > 0
002170******************************************************************
002180*      2 = (CREATE NOTIONAL COMMISSION REC)
002190******************************************************************
002200                 MOVE '2'          TO SESS1-VAR-VALUE
002210             WHEN OTHER
002220******************************************************************
002230*      RK 981 SHOULD NEVER HAPPEN
002240******************************************************************
002250                 MOVE 'RK'         TO RTN-ERR-PREFIX
002260                 MOVE 981          TO RTN-ERROR-CODE
002270                 MOVE 'A300'       TO RTN-PARA-NAME
002280                 PERFORM Z999-ERROR
002290                     THRU Z999-EXIT
002300         END-EVALUATE
002310
002320         IF RTN-NO-ERROR
002330            PERFORM U100-ACCESS-SESSION-VAR
002340               THRU U100-EXIT
002350         END-IF
002360
002370         IF RTN-NO-ERROR                                          LPM0675
002380* FOR 100% REDEMPTION OF BENEFIT SAVE THE                         LPM0675
002390* BENEFIT FOR LATER USE IN LAPSE SWRP PROCESSING                  LPM0675
002400            PERFORM U200-INSERT-GTSA                              LPM0720
002410               THRU U200-EXIT                                     LPM0720
002420         END-IF                                                   LPM0675
002430
002440     END-IF.
002450
002460 A300-EXIT.
002470     EXIT.
002480
002490 B100-SELECT-LPRL.
002500
002510     INITIALIZE LPAT1-DATA
002520                LPTR1-DATA.
002530
002540     MOVE WS-LPRL-KEY                TO LPTR1-LPRL-KEY.
002550     MOVE FDP-REF-KEY                TO LPAT1-BNTP-KEY.
002560******************************************************************
002570* MBBS - NET SURRENDER VALUE
002580******************************************************************
002590     MOVE 'MS'                       TO LPAT1-TYP-CD.
002600
002610     CALL 'CS5VLPAD'         USING RMESSAGE,
002620                                   LPAT1-DATA,
002630                                   LPTR1-DATA,
002640                                   RTN-OUTPUT-FIELDS.
002650
002660     IF RTN-NO-ERROR AND LPAT1-ROWS-FOUND > 0
002670******************************************************************
002680** DETERMINE IF SENTENCE_RULES EXISTS FOR THIS LAPSE_ACTIVITIES
002690******************************************************************
002700        PERFORM C100-GET-LPAT-SENTENCE
002710            THRU C100-EXIT
002720     END-IF.
002730 B100-EXIT.
002740     EXIT.
002750
002760 C100-GET-LPAT-SENTENCE.
002770     INITIALIZE SNRL1-DATA,
002780                SAVE-FDP-CASE-MBR-KEY,
002790                WS-SENTENCE-AMT,
002800                SAVE-FDP-EFF-DT.
002810     MOVE LPAT1-LPAT-KEY             TO SNRL1-REF-KEY.
002820*    LAPSE ACTIVITY
002830     MOVE 'LPAT'                     TO SNRL1-REF-TYP-CD.
002840*    00 = Standard
002850     MOVE '00'                       TO SNRL1-TYP-CD
002860     CALL 'CS1SNRLD' USING RMESSAGE,
002870                           SNRL1-DATA,
002880                           RTN-OUTPUT-FIELDS
002890     IF RTN-NO-ERROR
002900         IF SNRL1-ROWS-FOUND = 0
002910******************************************************************
002920* UL 4365 - SAME ERROR AS PUT OUT IN LAPSE MS LOGIC FOR NO SENTENCES
002930******************************************************************
002940             MOVE 'UL'               TO RTN-ERR-PREFIX
002950             MOVE 4365               TO RTN-ERROR-CODE
002960             MOVE 'C100'             TO RTN-PARA-NAME
002970             PERFORM Z999-ERROR
002980                 THRU Z999-EXIT
002990         ELSE
003000******************************************************************
003010** CALL THE GENERIC SENTENCE BLACKBOX USING THE LPAT-KEY.
003020** THIS BLACKBOX WILL COMPUTE AND RETURN AN AMOUNT THAT WILL BE
003030** USED DURING THE LAPSE ACTIVITIES PROCESS.
003040******************************************************************
003050             INITIALIZE SNTCLK-DATA
003060             MOVE LPAT1-LPAT-KEY           TO SNTCLK-REF-KEY
003070*    LAPSE ACTIVITY
003080             MOVE 'LPAT'                   TO SNTCLK-REF-TYP-CD
003090             MOVE '3'                      TO SNTCLK-SRC-CD
003100             MOVE '5 '                     TO SNTCLK-WD-TYP-CD
003110             MOVE '00'                     TO SNTCLK-BEN-TYP-CD
003120             MOVE '00'                     TO SNTCLK-RPT-TYP-CD
003130             MOVE '00'                     TO SNTCLK-TYP-CD
003140             SET SNTC-STANDARD-PROCESSING  TO TRUE
003150             SET MISC-AMOUNTS-NOT-USED     TO TRUE
003160             MOVE FDP-EFF-DT               TO SNTCLK-PAST-DUE-DT
003170             MOVE FDP-EFF-DT               TO SAVE-FDP-EFF-DT
003180
003190******************************************************************
003200*          SAVE CASE-MBR-KEY, WE MIGHT ZERO OUT FOR SENTENCE CALL
003210******************************************************************
003220             MOVE FDP-CASE-MBR-KEY         TO
003230                 SAVE-FDP-CASE-MBR-KEY
003240
003250******************************************************************
003260*          SETUP SENTENCE PARAMATERS BASED ON LPAT-TYP-CD
003270******************************************************************
003280
003290             MOVE 'EBAL'                   TO SNTCLK-TR-CD
003300
003310             SET CALC-DETAILS-DONT-EXIST   TO TRUE
003320             SET SNTC-STANDARD-PROCESSING  TO TRUE
003330             SET MISC-AMOUNTS-NOT-USED     TO TRUE
003340
003350             CALL 'CTXSNTCB' USING RMESSAGE,
003360                                   CASD1-DATA,
003370                                   SNTCLK-DATA,
003380                                   SNRL1-DATA,
003390                                   RTN-OUTPUT-FIELDS
003400             IF RTN-NO-ERROR
003410                 MOVE SNTCLK-RESULTING-AMT TO WS-SENTENCE-AMT
003420             END-IF
003430         END-IF
003440     END-IF.
003450
003460******************************************************************
003470*   RESTORE THE FDP-EFF-DT
003480******************************************************************
003490
003500     IF SAVE-FDP-EFF-DT <> SPACES
003510        MOVE SAVE-FDP-EFF-DT       TO FDP-EFF-DT
003520     END-IF.
003530
003540******************************************************************
003550*    RESTORE FDP-CASE-MBR-KEY
003560******************************************************************
003570
003580     IF SAVE-FDP-CASE-MBR-KEY > ZEROES
003590        MOVE SAVE-FDP-CASE-MBR-KEY TO FDP-CASE-MBR-KEY
003600     END-IF.
003610
003620 C100-EXIT.
003630     EXIT.
003640
003650 U100-ACCESS-SESSION-VAR.
003660
003670**   CREATE SESSION EVENT VARIABLE
003680
003690     CALL 'CP1SESSD' USING RMESSAGE,
003700                           SESS1-DATA,
003710                           RTN-OUTPUT-FIELDS.
003720
003730 U100-EXIT.
003740     EXIT.
003750
003760 U200-INSERT-GTSA.                                                LPM0720
003770                                                                  LPM0720
003780* FOR 100% REDEMPTION OF BENEFIT SAVE THE                         LPM0720
003790* BENEFIT FOR LATER USE IN LAPSE SWRP PROCESSING                  LPM0720
003800                                                                  LPM0720
003810* FDP-REF-KEY = BNTP-KEY                                          LPM0720
003820     MOVE FDP-REF-KEY          TO GTSA1-REF-KEY.                  LPM0720
003830                                                                  LPM0720
003840     IF SESS1-VAR-VALUE = '1'                                     LPM0720
003850* FULL REDEMPTION OF BENEFIT                                      LPM0720
003860        MOVE 'BNT1'               TO GTSA1-REF-TYP-CD             LPM0720
003870     ELSE                                                         LPM0720
003880* PARTIAL REDEMPTION OF BENEFIT                                   LPM0720
003890        MOVE 'BNT2'               TO GTSA1-REF-TYP-CD             LPM0720
003900     END-IF.                                                      LPM0720
003910                                                                  LPM0720
003920     CALL 'CI2GTSAD' USING RMESSAGE,                              LPM0720
003930                           GTSA1-DATA,                            LPM0720
003940                           RTN-OUTPUT-FIELDS.                     LPM0720
003950                                                                  LPM0720
003960 U200-EXIT.                                                       LPM0720
003970     EXIT.                                                        LPM0720
003980
003990 Z999-ERROR.
004000        MOVE WS-PROG-NM            TO RTN-PROG-NAME
004010        CALL 'CUTSVARB' USING RMESSAGE,
004020                              RTN-OUTPUT-FIELDS.
004030 Z999-EXIT.
004040     EXIT.
004050
