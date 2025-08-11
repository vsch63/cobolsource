000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.                                                 0
000040 PROGRAM-ID.           CACREV1B.                                          0
000050 AUTHOR.               DANIEL WILLIAMS                                    0
000060 DATE-WRITTEN.         1997-05-08.                                        0
000070****************************************************************          0
000080*   $Modtime::   10 Oct 2018 10:15:18  $                          00000600
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CACREV1B.COV  $      00000700
000100*
000110*   Rev 1.0.1.11   10 Oct 2018 11:45:32   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.10   19 Apr 2011 17:27:28   tom
000160*LSM5259
000170*Replace call to CUTDTCAL
000180*
000190*   Rev 1.0.1.9   23 Sep 2005 11:41:24   robin
000200*FDM3514
000210*Module Impact Analysis Tool - WS-REVISION awk script fix.
000220*
000230*   Rev 1.0.1.8   19 Sep 2005 18:58:22   robin
000240*FDM3514
000250*Module Impact Analysis Tool
000260*
000270*   Rev 1.0.1.7   20 Dec 2002 10:56:26   rieko
000280*FDM3175
000290*COBOL modifications to support the Oracle 8 pre-complier.
000300*
000310*   Rev 1.0.1.6   Mar 24 2000 17:43:04   miker
000320*FDM1959
000330*Expanded Keys project
000340*
000350*   Rev 1.0.1.5   Sep 25 09:07:40 1998   rickd
000360*AFA/Z 2154 - Added error checking to program
000370*
*******
*******   Rev 1.0.1.4   Dec 15 11:13:20 1997   dan
*******afz0314
*******
*******   Rev 1.1   03 Oct 1997 16:41:24   paulm
*******Initial Check in to LSA
*******
*******   Rev 1.0.1.3   10 Jul 1997 12:20:14   paulm
*******Mass Check In for Currency Field Changes
*******
*******   Rev 1.0.1.2   20 May 1997 14:39:00   dan
*******AFM0105
*******
*******   Rev 1.0.1.1   20 May 1997 09:20:20   dan
*******AFM0105
*******
*******   Rev 1.0.1.0   16 May 1997 15:35:54   paulm
*******initial check in for fdp branch
*******
*******16 may 1997
000580*                                                                         0
000590****************************************************************          0
000600* MONTH END ACCOUNT BALANCING REVERSAL                                    0
000610****************************************************************          0
000620* THIS PROGRAM WILL DELETE COMPANY ACCOUNTS CREATED BY THE                0
000630* GL ACCOUNT BALANCING JOB.  THE SYSTEM LAST ACCOUNT BALANCE              0
000640* DATE WILL BE ROLLED BACK ONE MONTH.  THE STATUS ON THE SCAB             0
000650* RECORD WILL BE SET TO REVERSED.                                         0
000660*                                                                         0
000670* THE FOLLOWING TABLES ARE UTILIZED:                                      0
000680* 1.) COMPANY_ACCTS                                                       0
000690* 2.) SYSTEM_DATA                                                         0
000700* 3.) SCHED_ACCT_BAL                                                      0
000710* 4.) BAL_ACCT_RULES
000720* 5.) JOURNAL_LOGS                                             0
000730****************************************************************          0
000740*                     CHANGES LOG                                         0
000750****************************************************************          0
000760*                                                                         0
000770*   DATE     PRG   PROJ#      DESCRIPTION                                 0
000780*  --------  --- ---------  --------------------------------------        0
000790*  12-05-97  KG   AFA0314    ADDED LOGIC FOR ADHOC REVERSALS.
000800*  12-12-97  DEW  AFA0314    ADDED LOGIC FOR ADHOC REVERSALS.
000810*  09-24-98  RMD  AFA2154    ADDED ERROR HANDLING
000820****************************************************************          0
000830 ENVIRONMENT DIVISION.                                                    0
000840 CONFIGURATION SECTION.                                                   0
000850*                                                                         0
000860 DATA DIVISION.                                                           0
000870 WORKING-STORAGE SECTION.                                                 0
000880                                                                          0
000890 01  BEG-WORK-STOR.                                                       0
000900     05 FILLER                             PIC X(40) VALUE                0
000910        'THIS IS THE BEGINNING OF WORKING STORAGE'.                       0
000920                                                                          0
000930 01  WS-PGM-NAME                          PIC X(8)                        0
000940                                          VALUE 'CACREV1B'.               0
000950                                                                          0
000960 01  WS-MISC-VARIABLES.                                                   0
000970     05  WS-PRIOR-ACCT-BAL-DT.                                            0
000980         10  WS-ISO-YYYY2                 PIC 9999.                       0
000990         10  FILLER                       PIC X.                          0
001000         10  WS-ISO-MM2                   PIC 99.                         0
001010         10  FILLER                       PIC X.                          0
001020         10  WS-ISO-DD2                   PIC 99.                         0
001030     05  WS-YRS-DIFF                      PIC S9(4)V9(5) COMP-3.          0
001040     05  WS-LAST-ACCT-BAL-DT              PIC X(10).
001050     05  ADHOC-FLAG                       PIC X.
001060*    SYSTEM DATA COPYBOOK                                                 0
001070     COPY CL1SYSDD.                                                       0
001080*    COMPANY ACCOUNTS COPYBOOK                                            0
001090     COPY CL1COACD.                                                       0
001100*    DATE CALCULATION COPYBOOK                                            0
001110     COPY CWSDTCAL.                                                       0
001120*    SCHED_ACCT_BAL COPYBOOK                                              0
001130     COPY CL1SCABD.                                                       0
001140*    BAL_ACCT_RULES COPYBOOK
001150     COPY CL1BARLD.                                                       0
001160*    JOURNAL_LOGS   COPYBOOK
001170     COPY CL5JLOGD.                                                       0
001180                                                                          0
001190**  REVISION AREA **                                              FDM3514
001200 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
001210     ' $Revision::   1.0.1.11       $'.                           
001220 LINKAGE SECTION.                                                         0
001230*    RMESSAGE AREA                                                        0
001240     COPY CLKFDPNC.                                                       0
001250*    CASE  DATA COPYBOOK                                                  0
001260     COPY CL1CASDD.                                                       0
001270*    STANDARD ERROR BLOCK                                                 0
001280     COPY CLKERRBC.                                                       0
001290                                                                          0
001300*********************************************************                 0
001310**   P R O C E D U R E    D I V I S I O N              **                 0
001320*********************************************************                 0
001330                                                                          0
001340 PROCEDURE DIVISION USING RMESSAGE,                                       0
001350                          CASD1-DATA,                                     0
001360                          RTN-OUTPUT-FIELDS.
001370
001380       IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514
001390        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
001400        MOVE WHEN-COMPILED TO                                     FDM3514
001410             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
001420        MOVE WS-REVISION (16:10) TO                               FDM3514
001430             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
001440        MOVE 'CACREV1B' TO                                        FDM3514
001450           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
001460        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
001470                              RTN-OUTPUT-FIELDS                   FDM3514
001480     END-IF.                                                      FDM3514
001490                                                                          0
001500******************************************************************        0
001510**                MAIN ROUTINE                                  **        0
001520******************************************************************        0
001530     PERFORM A000-INITIALIZE                                              0
001540        THRU A000-EXIT.                                                   0

001560     IF RTN-NO-ERROR                                              AFA2154
001570        IF ADHOC-FLAG = "Y"
001580           PERFORM C000-MAIN                                                    0
001590              THRU C000-EXIT                                                  0
001600        ELSE
001610           PERFORM B000-MAIN                                                    0
001620              THRU B000-EXIT                                                 0
001630     END-IF.                                                      AFA2154
001640                                                                          0
001650     GOBACK.                                                              0
001660                                                                          0
001670 A000-INITIALIZE.                                                         0
001680*********************************************************                 0
001690**                                                     **                 0
001700*********************************************************                 0
001710                                                                          0
001720     MOVE SPACE TO ADHOC-FLAG.
001730** EDIT INCOMING PARAMETERS                                               0
001740     IF FDP-USER = SPACES                                                 0
001750        MOVE 'UT'            TO RTN-ERR-PREFIX                            0
001760        MOVE 39              TO RTN-ERROR-CODE                            0
001770        MOVE 'CACREV1B'      TO RTN-PROG-NAME                             0
001780        MOVE 'A000-'         TO RTN-PARA-NAME                             0
001790        CALL 'CUTSVARB' USING RMESSAGE,                                   0
001800                              RTN-OUTPUT-FIELDS                           0
001810     END-IF.                                                              0
001820                                                                          0
001830     IF RTN-NO-ERROR                                              AFA2154
001840        IF FDP-TR-REF-NO = SPACES                                            0
001850           MOVE 'UT'            TO RTN-ERR-PREFIX                            0
001860           MOVE 34              TO RTN-ERROR-CODE                            0
001870           MOVE 'CACREV1B'      TO RTN-PROG-NAME                             0
001880           MOVE 'A000-'         TO RTN-PARA-NAME                             0
001890           CALL 'CUTSVARB' USING RMESSAGE,                                   0
001900                              RTN-OUTPUT-FIELDS                           0
001910        END-IF                                                              0
001920     END-IF.                                                      AFA2154
001930                                                                          0
001940     IF RTN-NO-ERROR                                              AFA2154
001950        IF FDP-REF-KEY  <= ZERO                                              0
001960           MOVE 'UT'            TO RTN-ERR-PREFIX                            0
001970           MOVE 32              TO RTN-ERROR-CODE                            0
001980           MOVE 'CACREV1B'      TO RTN-PROG-NAME                             0
001990           MOVE 'A000-'         TO RTN-PARA-NAME                             0
002000           CALL 'CUTSVARB' USING RMESSAGE,                                   0
002010                                 RTN-OUTPUT-FIELDS                           0
002020        END-IF                                                              0
002030     END-IF.                                                      AFA2154
002040                                                                          0
002050     IF RTN-NO-ERROR                                              AFA2154
002060         INITIALIZE SCAB1-DATA                                               0
002070         MOVE FDP-REF-KEY TO SCAB1-SCAB-KEY
002080         CALL 'CS1SCABD' USING RMESSAGE,                                      0
002090                               SCAB1-DATA,                                    0
002100                               RTN-OUTPUT-FIELDS                           0
002110          IF SCAB1-BAL-TYP-CD  = '03'
002120             MOVE "Y" TO ADHOC-FLAG
002130          END-IF
002140      END-IF.                                                     AFA2154
002150                                                                  AFA2154
002160 A000-EXIT.                                                               0
002170     EXIT.                                                                0
002180                                                                          0
002190 B000-MAIN.                                                               0
002200*********************************************************                 0
002210** FDP-REF-KEY   = SCAB-KEY        FROM THE FORM                          0
002220** FDP-TR-REF-NO = SCAB-TR-REF-NO  FROM THE FORM                          0
002230*********************************************************                 0
002240                                                                          0
C18800** DELETE THE COMPANY ACCOUNTS THAT WERE CREATED BY THE                   0
C18800** GL ACCOUNT BALANCE JOB                                                 0
002270     IF RTN-NO-ERROR                                                      0
002280        PERFORM B100-DELETE-COMPANY-ACCOUNTS  THRU  B100-EXIT             0
002290     END-IF.                                                              0
002300                                                                          0
C18800** CALCULATE MONTH PRIOR DATE                                             0
002320     IF RTN-NO-ERROR
002330        PERFORM B200-CALC-MONTHPRIOR-DATE  THRU  B200-EXIT                0
002340     END-IF.
002350*UPDATE JOURNAL_LOGS TABLE
002360     IF RTN-NO-ERROR                                                                   0
002370        PERFORM C300-UPDATE-JOURNAL-LOG THRU C300-EXIT
002380     END-IF.
002390** UPDATE SYSTEM_DATA WITH THE PRIOR ACCOUNT BALANCE DATE                 0
002400     IF RTN-NO-ERROR
002410        PERFORM B300-UPDATE-SYSTEM-DATA    THRU  B300-EXIT                0
002420     END-IF.                                                              0
002430                                                                          0
002440*    RECORD THAT THE REVERSAL RAN SUCCESSFULLY IN                         0
002450*    THE SCHED_ACCT_BAL TABLE                                             0
002460     IF RTN-NO-ERROR                                                      0
002470         PERFORM B400-UPDATE-SCHED-ACCT-BAL                               0
002480            THRU B400-EXIT                                                0
002490     END-IF.                                                              0
002500  B000-EXIT.                                                              0
002510     EXIT.                                                                0
002520                                                                          0
002530 C000-MAIN.                                                               0
002540* UPDATE BAL_ACCT_RULES
002550     IF RTN-NO-ERROR
002560         PERFORM C100-ADHOC-UPDATE THRU C100-EXIT                                                   0
002570     END-IF.
002580                                                                          0
C18800** DELETE THE COMPANY ACCOUNTS THAT WERE CREATED BY THE                   0
C18800** GL ACCOUNT BALANCE JOB                                                 0
002610     IF RTN-NO-ERROR                                                      0
002620        PERFORM B100-DELETE-COMPANY-ACCOUNTS  THRU  B100-EXIT             0
002630     END-IF.                                                              0
002640                                                                          0
C18800** CALCULATE MONTH PRIOR DATE                                             0
002660     IF RTN-NO-ERROR
002670        PERFORM C200-ADH-CALC-MONTHPRIOR-DATE THRU C200-EXIT                                                                  0
002680     END-IF.
002690*UPDATE JOURNAL_LOGS TABLE
002700     IF RTN-NO-ERROR                                                                   0
002710        PERFORM C300-UPDATE-JOURNAL-LOG THRU C300-EXIT
002720     END-IF.
002730                                                                          0
002740*    RECORD THAT THE REVERSAL RAN SUCCESSFULLY IN                         0
002750*    THE SCHED_ACCT_BAL TABLE                                             0
002760     IF RTN-NO-ERROR                                                      0
002770         PERFORM B400-UPDATE-SCHED-ACCT-BAL                               0
002780            THRU B400-EXIT                                                0
002790     END-IF.                                                              0
002800  C000-EXIT.                                                              0
002810     EXIT.                                                                0
002820 B100-DELETE-COMPANY-ACCOUNTS.                                            0
002830***************************************************************           0
002840* DELETE ALL COMPANY ACCOUNTS THAT CREATED UNDER THIS TR-REF-NO           0
002850***************************************************************           0
002860     INITIALIZE COAC1-DATA.                                               0
002870     MOVE FDP-TR-REF-NO TO COAC1-TR-REF-NO.                               0
002880                                                                          0
002890     CALL 'CD2COACD' USING RMESSAGE,                                      0
002900                           COAC1-DATA,                                    0
002910                           RTN-OUTPUT-FIELDS.                             0
002920                                                                          0
002930 B100-EXIT.                                                               0
002940     EXIT.                                                                0
002950
002960 B200-CALC-MONTHPRIOR-DATE.                                               0
002970*********************************************************                 0
002980* GET LAST MONTH ACCOUNT BALANCE DATE FROM SYSTEM_DATA                    0
002990*********************************************************                 0
003000                                                                          0
003010     CALL 'CS1SYSDD' USING RMESSAGE,                                      0
003020                           SYSD1-DATA,                                    0
003030                           RTN-OUTPUT-FIELDS.                             0
003040                                                                          0
003050*    CALCULATE PRIOR MONTH END DATE, SAVE IN WORKING STORAGE              0
003060     INITIALIZE DATEPARMS.                                                0
003070     MOVE 110 TO REQUEST-TYPE.                                            0
003080     MOVE SYSD1-LAST-ACCT-BAL-DT TO INDATE.                               0
003090     MOVE SYSD1-LAST-ACCT-BAL-DT TO WS-LAST-ACCT-BAL-DT.                  0
003100     MOVE -1 TO MONTHS-DAY-CHG.                                           0
003110                                                                          0
003120     CALL 'CUTDTCAB' USING RMESSAGE,                              LSM5259
003130                           DATEPARMS,                             LSM5259
003140                           RTN-OUTPUT-FIELDS.                     LSM5259
003150                                                                          0
003160     IF LK-ERROR-CD NOT = ZERO                                            0
003170        MOVE 'UT'        TO RTN-ERR-PREFIX                                0
003180        MOVE 1100        TO RTN-ERROR-CODE                                0
003190        MOVE 'CACREV1B'  TO RTN-PROG-NAME                                 0
003200        MOVE 'A100-CALC' TO RTN-PARA-NAME                                 0
003210        CALL 'CUTSVARB' USING RMESSAGE,                                   0
003220                             RTN-OUTPUT-FIELDS                            0
003230        DISPLAY 'ERROR IN DATE CALCULATION PROGRAM'                       0
003240        DISPLAY 'CALLED FROM CACREV1B'                                    0
003250     ELSE                                                                 0
003260        MOVE ODATE TO WS-PRIOR-ACCT-BAL-DT                                0
003270     END-IF.                                                              0
003280 B200-EXIT.                                                               0
003290     EXIT.                                                                0
003300                                                                          0
003310                                                                          0
003320 B300-UPDATE-SYSTEM-DATA.                                                 0
003330***************************************************************           0
003340*                                                                         0
003350***************************************************************           0
003360     MOVE WS-PRIOR-ACCT-BAL-DT  TO  SYSD1-LAST-ACCT-BAL-DT.               0
003370     CALL 'CU1SYSDD' USING RMESSAGE,                                      0
003380                           SYSD1-DATA,                                    0
003390                           RTN-OUTPUT-FIELDS.                             0
003400                                                                          0
003410 B300-EXIT.                                                               0
003420     EXIT.                                                                0
003430                                                                          0
003440 B400-UPDATE-SCHED-ACCT-BAL.                                              0
003450*********************************************************                 0
003460**   UPDATE RECORD IN THE SCHED_ACCT_BAL TABLE FOR THE                    0
003470**   REVERSAL JOB                                                         0
003480*********************************************************                 0
003490     INITIALIZE SCAB1-DATA.                                               0
003500*    REVERSED                                                             0
003510     MOVE '02'                  TO SCAB1-STAT-CD.                         0
003520     MOVE WS-LAST-ACCT-BAL-DT   TO SCAB1-EFF-DT.
003530     MOVE FDP-REF-KEY           TO SCAB1-SCAB-KEY.                        0
003540                                                                          0
003550     CALL 'CU1SCABD' USING RMESSAGE,                                      0
003560                           SCAB1-DATA,                                    0
003570                           RTN-OUTPUT-FIELDS.                             0
003580                                                                          0
003590 B400-EXIT.                                                               0
003600     EXIT.                                                                0
003610                                                                          0
003620 C100-ADHOC-UPDATE.
003630     INITIALIZE BARL1-DATA.
003640*REVERSED                                                                 0
003650     MOVE '02'             TO BARL1-STAT-CD.
003660     MOVE SCAB1-BARL-KEY   TO BARL1-BARL-KEY.
003670     CALL 'CU1BARLD'  USING RMESSAGE,
003680                            BARL1-DATA,                                    0
003690                            RTN-OUTPUT-FIELDS.                             0
003700 C100-EXIT.
003710     EXIT.

003730 C200-ADH-CALC-MONTHPRIOR-DATE.                                               0
003740*********************************************************                 0
003750* GET LAST MONTH ACCOUNT BALANCE DATE FROM BAL_ACCT_RULES                    0
003760*********************************************************                 0
003770     INITIALIZE BARL1-DATA.
003780     MOVE SCAB1-BARL-KEY TO BARL1-BARL-KEY.
003790     CALL 'CS1BARLD' USING RMESSAGE,                                      0
003800                           BARL1-DATA,                                    0
003810                           RTN-OUTPUT-FIELDS.                             0
003820                                                                          0
003830*    CALCULATE PRIOR MONTH END DATE, SAVE IN WORKING STORAGE              0
003840     INITIALIZE DATEPARMS.                                                0
003850     MOVE 110 TO REQUEST-TYPE.                                            0
003860     MOVE BARL1-NXT-BAL-DT TO INDATE
003870     MOVE BARL1-NXT-BAL-DT TO WS-LAST-ACCT-BAL-DT.
003880     MOVE -1 TO MONTHS-DAY-CHG.                                           0
003890                                                                          0
003900     CALL 'CUTDTCAB' USING RMESSAGE,                              LSM5259
003910                           DATEPARMS,                             LSM5259
003920                           RTN-OUTPUT-FIELDS.                     LSM5259
003930                                                                          0
003940     IF LK-ERROR-CD NOT = ZERO                                            0
003950        MOVE 'UT'        TO RTN-ERR-PREFIX                                0
003960        MOVE 1100        TO RTN-ERROR-CODE                                0
003970        MOVE 'CACREV1B'  TO RTN-PROG-NAME                                 0
003980        MOVE 'C200-CALC' TO RTN-PARA-NAME                                 0
003990        CALL 'CUTSVARB' USING RMESSAGE,                                   0
004000                             RTN-OUTPUT-FIELDS                            0
004010        DISPLAY 'ERROR IN DATE CALCULATION PROGRAM'                       0
004020        DISPLAY 'CALLED FROM CACREV1B'                                    0
004030     ELSE                                                                 0
004040        MOVE ODATE TO WS-PRIOR-ACCT-BAL-DT                                0
004050     END-IF.                                                              0
004060 C200-EXIT.                                                               0
004070     EXIT.                                                                0

004090 C300-UPDATE-JOURNAL-LOG.
004100     INITIALIZE JLOG5-DATA.
004110     MOVE WS-PRIOR-ACCT-BAL-DT  TO JLOG5-LAST-ACCT-BAL-DT1.
004120     MOVE WS-LAST-ACCT-BAL-DT   TO JLOG5-NEW-ACCT-BAL-DT.
004130     MOVE '0'                  TO JLOG5-HOLD-CD.
004140     IF ADHOC-FLAG = "Y"
004150*ON-LINE PROCESS
004160        MOVE '1'                 TO JLOG5-FROM-HOLD-CD
004170        MOVE BARL1-CASE-KEY       TO JLOG5-CASE-KEY
004180        MOVE BARL1-MKEY-CO-NUM    TO JLOG5-MKEY-CO-NUM
004190        MOVE BARL1-MKEY-PROD-NUM  TO JLOG5-MKEY-PROD-NUM
004200     ELSE
004210*BATCH PROCESS
004220        MOVE '2'                 TO JLOG5-FROM-HOLD-CD.
004230     IF ADHOC-FLAG = "Y"
004240        CALL 'CU3JLOGD' USING RMESSAGE
004250                              JLOG5-DATA,                                    0
004260                              RTN-OUTPUT-FIELDS                             0
004270     ELSE
004280        CALL 'CU2JLOGD' USING RMESSAGE
004290                              JLOG5-DATA,                                    0
004300                              RTN-OUTPUT-FIELDS.                            0

004320     IF JLOG5-ROWS-FOUND = 0
004330        MOVE 'TX'            TO RTN-ERR-PREFIX
004340        MOVE 1013            TO RTN-ERROR-CODE
004350        MOVE 'CACREV1B'      TO RTN-PROG-NAME
004360        MOVE 'C300'          TO RTN-PARA-NAME
004370        CALL 'CUTSVARB' USING RMESSAGE,
004380                             RTN-OUTPUT-FIELDS
004390     END-IF.

004410 C300-EXIT.
004420     EXIT.
004430**                                                                        0
004440**   E N D     O F    P R O G R A M                                       0
004450**                                                                        0
004460                                                                          0
