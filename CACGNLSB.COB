000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.
000040 PROGRAM-ID.           CACGNLSB.
000050 AUTHOR.               STEVE DONOIAN.
000060 DATE-WRITTEN.         2002-01-05.
000070****************************************************************
000080*   $Modtime::   10 Oct 2018 10:15:18  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CACGNLSB.COV  $
000100*
000110*   Rev 1.0.1.13   10 Oct 2018 11:45:30   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.12   19 Apr 2011 17:27:28   tom
000160*LSM5259
000170*Replace call to CUTDTCAL
000180*
000190*   Rev 1.0.1.11   15 Mar 2006 10:32:28   robin
000200*OMM0963
000210*Create ABAL TDTL when End Balance is less than zero
000220*
000230*   Rev 1.0.1.10   23 Sep 2005 11:41:22   robin
000240*FDM3514
000250*Module Impact Analysis Tool - WS-REVISION awk script fix.
000260*
000270*   Rev 1.0.1.9   19 Sep 2005 18:58:22   robin
000280*FDM3514
000290*Module Impact Analysis Tool
000300*
000310*   Rev 1.0.1.8   20 Oct 2003 08:37:10   laura
000320*OMM0480
000330*add uval-ovrd-cd logic
000340*
000350*   Rev 1.0.1.7   06 Oct 2003 15:26:44   laura
000360*OMM0480
000370*pass case-key to cutuviid
000380*
000390*   Rev 1.0.1.6   20 Dec 2002 10:56:16   rieko
000400*FDM3175
000410*COBOL modifications to support the Oracle 8 pre-complier.
000420*
000430*   Rev 1.0.1.5   01 Feb 2002 11:11:18   steved
000440*OMM0101
000450*ledger unrealized gain/loss
000460*
000470*   Rev 1.0.1.4   31 Jan 2002 09:04:12   steved
000480*OMM0101
000490*ledger unrealized gain/loss
000500*
000510*   Rev 1.0.1.3   09 Jan 2002 15:18:26   steved
000520*OMM0101
000530*ledger unrealized gain/loss
000540*
000550*   Rev 1.0.1.2   07 Jan 2002 16:23:20   steved
000560*OMA0101
000570*ledger unrealized gain/loss
000580*
000590*   Rev 1.0.1.1   04 Jan 2002 15:35:48   steved
000600*OMA0101
000610*calculate new ledger unrealized gain/loss
000620*
000630****************************************************************
000640* ledger unrealized gains and losses
000650****************************************************************
000660* THIS MODULE WILL:
000670* determine unrealized gains and losses and post these to
000680* to transact detail. first it determines if there was a
000690* beginning balance by reading for max date from transact details
000700* where tr-cd = 'abal'.  then we will read and sum all unit-ct
000710* and amt grouping by charge band and and fund and save to an
000720* internal table.
000730* the next step is to read all invest buy sell records with price
000740* date between commencement/begin bal date and fdp eff dt and
000750* sum all unit-ct and amt grouping by charge band and and fund
000760* and accumulating it to our internal table.
000770* for each row, calulate the ending balance and write to
000780* transact detail a Notional record with the the tr-cd set to
000790* 'abal' and the ending balance.  then for the same row in our
000800* table, we determine the gain/loss by calulating the gain/loss=
000810* (ending bal) - (net transactions) - (beginning bal).  we then
000820* write this amount out to transact detail with the tr-cd = the
000830* value retrieved from tr-cd-tranche with the appropriate
000840* gain(17) or loss(18).
000850****************************************************************
000860*                     CHANGES LOG
000870****************************************************************
000880*
000890*   DATE     PRG    PROJ#   DESCRIPTION
000900*  --------  ---   -------  --------------------------------------
000910*  01/05/01 steved oma0101  INITIAL CHECK IN
000920*  06/OCT/03 LED   OMM0480  PASS CASE-KEY TO CUTUVIID
000930*                           MOVE INITIALIZE OF UVII1-DATA UP, SO
000940*                           THAT CFDR-UVAL-OVRD IS READ ONLY
000950*                           ONCE.
000960*  15/MAR/06 RAM   OMM0963  ADDED LOGIC IN B400 TO CALCULATE
000970*                           ENDING BALANCE WHEN UNIT <> 0
000980*                           AND INSERT TDTL WHEN END-BALANCE <> 0.
000990*
001000****************************************************************
001010 ENVIRONMENT DIVISION.
001020 CONFIGURATION SECTION.
001030*
001040 DATA DIVISION.
001050 WORKING-STORAGE SECTION.
001060
001070 01  WS-PGM-NAME                          PIC X(08)
001080                                          VALUE 'CACGNLSB'.
001090 01  WS-COMPILED                          PIC X(20).
001100
001110 01  WS-SUBSCRIPTS.
001120     05  WS-TD-SUB                        PIC S9(4) COMP.
001130     05  ITDTL                            PIC S9(4) COMP.
001140     05  IINBS                            PIC S9(4) COMP.
001150
001160 01  WS-MISC-VARS.
001170     05  WS-BEG-BAL-DT                    PIC X(10).
001180     05  WS-TR-CD-GAIN-17                 PIC X(04).
001190     05  WS-TR-CD-LOSS-18                 PIC X(04).
001200     05  WS-GAIN-LOSS-AMT                 PIC S9(13)V9(2) COMP-3.
001210     05  WS-DAYS-DIFF-1                   PIC S9(04) COMP.
001220     05  WS-DAYS-DIFF-2                   PIC S9(04) COMP.
001230
001240 01  WS-LOCAL-FLAGS.
001250     05  WS-FOUND-TDTL-MATCH              PIC X.
001260     05  ROW-FOUND-FLAG                   PIC X.
001270         88  ROW-FOUND                    VALUE 'Y'.
001280         88  ROW-NOT-FOUND                VALUE 'N'.
001290     05  TR-CD-FOUND-FLAG                 PIC X.
001300         88  TR-CD-FOUND                  VALUE 'Y'.
001310         88  TR-CD-NOT-FOUND              VALUE 'N'.
001320
001330***      INDEXED BY WS-TD-NDX.
001340 01  WS-TD-OUTPUT-ARRAY.
001350     05  WS-TD-MAX-SIZE               PIC S9(4) COMP VALUE +5000.
001360     05  WS-TD-CURR-SIZE              PIC S9(4) COMP VALUE ZERO.
001370     05  WS-TD-ARRAY-RECORD OCCURS 5000 TIMES
001380         DEPENDING ON WS-TD-CURR-SIZE.
001390         10  WS-TD-FD-DESC-ID             PIC X(04).
001400         10  WS-TD-CHRL-KEY               PIC 9(15).
001410         10  WS-TD-BEG-BAL                PIC S9(13)V9(2) COMP-3.
001420         10  WS-TD-UNIT-CT                PIC S9(12)V9(6) COMP-3.
001430         10  WS-TD-AMT                    PIC S9(13)V9(2) COMP-3.
001440         10  WS-TD-END-BAL                PIC S9(13)V9(2) COMP-3.
001450
001460 01  SYS-VAR-DETL.
001470     05  WS-ERROR1.
001480         10  FILLER                       PIC X(15)
001490             VALUE ' CASE-KEY: '.
001500         10  WS-CASE-KEY                  PIC 9(15).
001510**
001520**   COPYBOOKS
001530**
001540
001550*  GOES WITH EXISTING CS1TRTRD (TRANCHES = 17 OR 18)
001560*  TR_CODE_TRNCH
001570     COPY CL1TRTRD.
001580
001590*  GOES WITH NEW CB4SCATD(SELECT SCAT-KEY, EFF-DT)
001600*  TRANSACT_DETAILS
001610     COPY CL2SCATD.
001620
001630*  GOES WITH NEW CF15TDTD (SUM GROUP BY)
001640*  TRANSACT_DETAILS
001650     COPY CL7TDTLD.
001660
001670*  GOES WITH NEW CFHINBSD
001680*  INVEST_BUY_SELLS
001690     COPY CL2INBSD.
001700
001710*  GOES WITH EXISTING CI1TDTLD INSERT TRANSACT_DETAILS GROUP 100
001720*  TRANSACT_DETAILS                                                                                                                                                      04410000
001730     COPY CL2TDTLD.                                                                                                                                                                                    04420000
001740
001750*  GOES WITH EXISTING CUTUVIID GET UNIT VALUE
001760*  UNIT VALUE
001770     COPY CL1UVIID.                                                                                                                                                                                    04420000
001780
001790*  CUTDTCAL                                                                                                                                                              04410000
001800     COPY CWSDTCAL.                                                                                                                                                                                    04420000
001810*  XREFS                                                                                                                                                                 04410000
001820     COPY CL1XREFD.                                                                                                                                                                                    04420000
001830
001840**  REVISION AREA **                                              FDM3514
001850 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
001860     ' $Revision::   1.0.1.13       $'.
001870 LINKAGE SECTION.
001880     COPY CLKFDPNC.
001890     COPY CL1CASDD.
001900     COPY CLKERRBC.
001910
001920*********************************************************
001930**   P R O C E D U R E    D I V I S I O N              **
001940*********************************************************
001950
001960 PROCEDURE DIVISION USING RMESSAGE,
001970                          CASD1-DATA,
001980                          RTN-OUTPUT-FIELDS.
001990
002000     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514
002010        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
002020        MOVE WHEN-COMPILED TO                                     FDM3514
002030             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
002040        MOVE WS-REVISION (16:10) TO                               FDM3514
002050             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
002060        MOVE 'CACGNLSB' TO                                        FDM3514
002070           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
002080        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
002090                              RTN-OUTPUT-FIELDS                   FDM3514
002100     END-IF.                                                      FDM3514
002110
002120******************************************************************
002130**                MAIN ROUTINE                                  **
002140******************************************************************
002150
002160*    THIS PROGRAM IS ONLY RUN FOR UNIT VALUES II
002170     IF FDP-UNITVL-CD = '1'
002180        PERFORM A000-INITIALIZE
002190           THRU A000-INITIALIZE-EXIT
002200
002210        IF RTN-NO-ERROR
002220           PERFORM B000-MAIN
002230              THRU B000-MAIN-EXIT
002240        END-IF
002250
002260        IF RTN-NO-ERROR AND TDTL2-ROWS-FOUND > 0
002270           PERFORM C000-TDTL-INSERT
002280              THRU C000-EXIT
002290        END-IF
002300     END-IF.
002310
002320     GOBACK.
002330
002340******************************************************************
002350**                END MAIN ROUTINE                              **
002360******************************************************************
002370
002380 A000-INITIALIZE.
002390
002400
002410     INITIALIZE WS-MISC-VARS
002420*************** WS-TD-OUTPUT-ARRAY
002430                WS-SUBSCRIPTS
002440
002450     INITIALIZE UVII1-DATA.                                       OMM0480
002460
002470     PERFORM A050-EDIT-PARMS
002480        THRU A050-EDIT-PARMS-EXIT.
002490
002500 A000-INITIALIZE-EXIT.
002510     EXIT.
002520
002530 A050-EDIT-PARMS.
002540
002550     IF FDP-CASE-KEY <= ZERO
002560        MOVE 'UT'                       TO RTN-ERR-PREFIX
002570        MOVE 30                         TO RTN-ERROR-CODE
002580        MOVE WS-PGM-NAME                TO RTN-PROG-NAME
002590        MOVE 'A050-EDIT-PARMS'          TO RTN-PARA-NAME
002600        CALL 'CUTSVARB' USING RMESSAGE,
002610                              RTN-OUTPUT-FIELDS
002620     END-IF.
002630*
002640     IF FDP-SCAT-KEY <= ZERO  AND RTN-NO-ERROR
002650        MOVE 'UT'                       TO RTN-ERR-PREFIX
002660        MOVE 42                         TO RTN-ERROR-CODE
002670        MOVE WS-PGM-NAME                TO RTN-PROG-NAME
002680        MOVE 'A050-EDIT-PARMS'          TO RTN-PARA-NAME
002690        CALL 'CUTSVARB' USING RMESSAGE,
002700                              RTN-OUTPUT-FIELDS
002710     END-IF.
002720*
002730     IF FDP-TR-REF-NO = SPACES AND RTN-NO-ERROR
002740        MOVE 'UT'                    TO RTN-ERR-PREFIX
002750        MOVE 34                      TO RTN-ERROR-CODE
002760        MOVE WS-PGM-NAME             TO RTN-PROG-NAME
002770        MOVE 'A050-EDIT-PARMS'       TO RTN-PARA-NAME
002780        CALL 'CUTSVARB'           USING RMESSAGE,
002790                                        RTN-OUTPUT-FIELDS
002800     END-IF.
002810*
002820     IF FDP-EFF-DT = SPACES AND RTN-NO-ERROR
002830        MOVE 'UT'                 TO RTN-ERR-PREFIX
002840        MOVE 35                   TO RTN-ERROR-CODE
002850        MOVE WS-PGM-NAME          TO RTN-PROG-NAME
002860        MOVE 'A050-EDIT-PARMS'    TO RTN-PARA-NAME
002870        CALL 'CUTSVARB'        USING RMESSAGE,
002880                                     RTN-OUTPUT-FIELDS
002890     END-IF.
002900*
002910 A050-EDIT-PARMS-EXIT.
002920     EXIT.
002930
002940 B000-MAIN.
002950
002960     PERFORM B100-LOAD-TR-CODES
002970        THRU B100-EXIT.
002980
002990     IF RTN-NO-ERROR
003000        PERFORM B200-PROCESS-BEG-BAL
003010           THRU B200-EXIT
003020     END-IF.
003030
003040     IF RTN-NO-ERROR
003050        PERFORM B300-PROCESS-INBS
003060           THRU B300-EXIT
003070     END-IF.
003080
003090     IF RTN-NO-ERROR
003100        PERFORM B400-PROCESS-ARRAY
003110           THRU B400-EXIT
003120     END-IF.
003130
003140 B000-MAIN-EXIT.
003150     EXIT.
003160
003170 B100-LOAD-TR-CODES.
003180
003190*  RETRIEVE TR CODES ASSOCIATED WITH ACCOUNTING ACTIVITY NUMS 17 AND 18
003200
003210     INITIALIZE TRTR1-DATA
003220     MOVE 'ABAL' TO TRTR1-TR-CD
003230     MOVE 17     TO TRTR1-ACTIVITY-NUM
003240     CALL 'CS1TRTRD'  USING RMESSAGE
003250                            TRTR1-DATA
003260                            RTN-OUTPUT-FIELDS
003270
003280**   THIS DAL HANDLES 'NOT FOUND' AS RTN-NO-ERROR
003290     IF RTN-NO-ERROR
003300        IF TRTR1-ROWS-FOUND > 0
003310           MOVE TRTR1-TDTL-TR-CD TO WS-TR-CD-GAIN-17
003320           MOVE 18               TO TRTR1-ACTIVITY-NUM
003330           CALL 'CS1TRTRD'  USING RMESSAGE
003340                                  TRTR1-DATA
003350                                  RTN-OUTPUT-FIELDS
003360           IF RTN-NO-ERROR
003370              IF TRTR1-ROWS-FOUND > 0
003380                 MOVE TRTR1-TDTL-TR-CD TO WS-TR-CD-LOSS-18
003390              ELSE
003400                 MOVE 'UT'     TO RTN-ERR-PREFIX
003410                 MOVE 79       TO RTN-ERROR-CODE
003420                 MOVE WS-PGM-NAME TO RTN-PROG-NAME
003430                 MOVE 'B100-LOAD-TR-CODES' TO RTN-PARA-NAME
003440                 MOVE 'TR_CD_TRNCH       ' TO RTN-TABLE-NAME
003450**              ** VARIANCE DETAILS
003460                 INITIALIZE REQUIRED-SYS-VAR-DETL-DATA
003470                 MOVE 1                   TO RTN-VAR-SEQ-NO (1)
003480                 MOVE 'FOR TR CODE "ABAL", ACTIVITY NBR 18 NOT FOU
003490-                     'nd'                TO RTN-VAR-TEXT (1)
003500                 MOVE 1                   TO RTN-VAR-DETL-CNT
003510                 CALL 'CUTSVARB' USING RMESSAGE,
003520                               RTN-OUTPUT-FIELDS
003530              END-IF
003540           END-IF
003550        ELSE
003560           MOVE 'UT'     TO RTN-ERR-PREFIX
003570           MOVE 79       TO RTN-ERROR-CODE
003580           MOVE WS-PGM-NAME TO RTN-PROG-NAME
003590           MOVE 'B100-LOAD-TR-CODES' TO RTN-PARA-NAME
003600           MOVE 'TR_CD_TRNCH       ' TO RTN-TABLE-NAME
003610**         ** VARIANCE DETAILS
003620           INITIALIZE REQUIRED-SYS-VAR-DETL-DATA
003630           MOVE 1                   TO RTN-VAR-SEQ-NO (1)
003640           move 'For TR CODE "ABAL", activity nbr 17 not found'
003650                                    TO RTN-VAR-TEXT (1)
003660           MOVE 1                   TO RTN-VAR-DETL-CNT
003670           CALL 'CUTSVARB' USING RMESSAGE,
003680                         RTN-OUTPUT-FIELDS
003690        END-IF
003700     END-IF
003710
003720     .
003730 B100-EXIT.
003740     EXIT.
003750
003760 B200-PROCESS-BEG-BAL.
003770
003780*****GET THE SCAT-KEY AND MOST RECENT EFF-DT FROM SCHED-ACT WHICH
003790*****IS THE EQUIVALENT OF GETTING THE MAX DATE FROM THE TRANSACT
003800*****DETAILS WHERE TR-CD = 'ABAL'
003810     INITIALIZE SCAT2-DATA
003820     MOVE FDP-CASE-KEY TO SCAT2-CASE-KEY(1)
003830     MOVE FDP-PRDF-KEY TO SCAT2-PRDF-KEY(1)
003840     MOVE FDP-SCAT-KEY TO SCAT2-SCAT-KEY(1)
003850
003860     CALL 'CB4SCATD'  USING RMESSAGE
003870                            SCAT2-DATA
003880                            RTN-OUTPUT-FIELDS
003890
003900*****IF BEG BAL DT > FDP-EFF-DT, THEN ERROR OUT.
003910*****IF THE BEGINNING BALANCE DATE IS BLANK, OR BEG BAL DT =
003920*****CMNCMNT DT, SET IT TO BE COMMENCEMENT DATE MINUS 1 DAY AND
003930*****ASSUME BALANCES ARE ZERO AND THEN EXIT. OTHERWISE
003940*****MAX(EFF-DT) WAS RETURNED AND IT'S NOT = TO COMMENCEMENT
003950*****DATE, SO DO CALCULATION TO GET ALL ROWS FROM TDTL TO
003960*****DETERMINE INITIAL ARRAY OF BEGGINING BALANCES
003970     IF RTN-ERROR
003980        GO TO B200-EXIT
003990     END-IF
004000
004010     IF SCAT2-EFF-DT(1) > FDP-EFF-DT
004020        MOVE 'JL'     TO RTN-ERR-PREFIX
004030        MOVE +75      TO RTN-ERROR-CODE
004040        MOVE WS-PGM-NAME TO RTN-PROG-NAME
004050        MOVE 'B200-PROCESS-BEG-BAL' TO RTN-PARA-NAME
004060        MOVE 'TRANSACT_DETAILS    ' TO RTN-TABLE-NAME
004070**      ** VARIANCE DETAILS
004080        INITIALIZE REQUIRED-SYS-VAR-DETL-DATA
004090        MOVE 1                   TO RTN-VAR-SEQ-NO (1)
004100        MOVE 'MAX "ABAL" EFF DT > FDP RUN DATE             '
004110                                 TO RTN-VAR-TEXT (1)
004120        MOVE 1                   TO RTN-VAR-DETL-CNT
004130        CALL 'CUTSVARB' USING RMESSAGE,
004140                      RTN-OUTPUT-FIELDS
004150     ELSE
004160        MOVE SCAT2-EFF-DT(1) TO WS-BEG-BAL-DT
004170        IF WS-BEG-BAL-DT = SPACES
004180        OR WS-BEG-BAL-DT = CASD1-CMNC-DT
004190****    COMPUTE WS-BEG-BAL-DT = CASD1-CMNC-DT - 1
004200           PERFORM B210-CALC-BEG-DT
004210              THRU B210-EXIT
004220        ELSE
004230           PERFORM B220-CALC-BEG-BAL
004240              THRU B220-EXIT
004250        END-IF
004260     END-IF
004270
004280     .
004290 B200-EXIT.
004300     EXIT.
004310
004320 B210-CALC-BEG-DT.
004330
004340*    NEED TO SUBTRACT A DAY FROM COMMENCEMENT DAY
004350     INITIALIZE DATEPARMS
004360     MOVE CASD1-CMNC-DT  TO INDATE
004370     MOVE -1             TO MONTHS-DAY-CHG
004380     MOVE 100            TO REQUEST-TYPE
004390     PERFORM X000-CALL-CUTDTCAL
004400        THRU X000-EXIT
004410     IF RTN-NO-ERROR
004420        MOVE ODATE TO WS-BEG-BAL-DT
004430     END-IF
004440     .
004450 B210-EXIT.
004460     exit.
004470
004480 B220-CALC-BEG-BAL.
004490
004500     MOVE SPACES TO TDTL7-END-FLAG.
004510     MOVE ZEROES TO TDTL7-CUMUL-ROWS.
004520*** READ ALL TDTLS
004530     PERFORM UNTIL TDTL7-END-FLAG = 'Y'
004540            OR RTN-ERROR
004550**
004560**      FETCH NEXT BLOCK OF TDTLS
004570**
004580       MOVE 'ABAL' TO TDTL7-TR-CD (1)
004590       MOVE WS-BEG-BAL-DT TO TDTL7-EFF-DT (1)
004600       MOVE FDP-CASE-KEY  TO TDTL7-CASE-KEY(1)
004610       MOVE SCAT2-SCAT-KEY(1)  TO TDTL7-SCAT-KEY(1)
004620       CALL 'CF15TDTD' USING RMESSAGE,
004630                             TDTL7-DATA,
004640                             RTN-OUTPUT-FIELDS
004650
004660***  FOR EACH ROW READ, WRITE OUT AN ARRAY ROW
004670       IF RTN-NO-ERROR
004680          PERFORM
004690          VARYING ITDTL FROM 1 BY 1
004700          UNTIL   ITDTL > TDTL7-ROWS-FOUND
004710          OR      RTN-ERROR
004720***********  SET WS-TD-CURR-SIZE UP BY +1
004730***********  SET WS-TD-SUB TO WS-TD-CURR-SIZE
004740             ADD +1 TO WS-TD-CURR-SIZE
004750             MOVE WS-TD-CURR-SIZE TO WS-TD-SUB
004760             MOVE TDTL7-FD-DESC-ID(ITDTL)
004770                                 TO WS-TD-FD-DESC-ID(WS-TD-SUB)
004780             MOVE TDTL7-CHRL-KEY  (ITDTL)
004790                                 TO WS-TD-CHRL-KEY  (WS-TD-SUB)
004800             MOVE TDTL7-UNIT-CT   (ITDTL)
004810                                 TO WS-TD-UNIT-CT   (WS-TD-SUB)
004820             MOVE TDTL7-AMT       (ITDTL)
004830                                 TO WS-TD-BEG-BAL   (WS-TD-SUB)
004840             MOVE ZERO           TO WS-TD-AMT       (WS-TD-SUB)
004850                                    WS-TD-END-BAL   (WS-TD-SUB)
004860          END-PERFORM
004870       END-IF
004880
004890     END-PERFORM.
004900
004910 B220-EXIT.
004920     exit.
004930
004940 B300-PROCESS-INBS.
004950*
004960     INITIALIZE INBS2-DATA
004970
004980     PERFORM UNTIL INBS2-END-FLAG = 'Y'
004990          OR RTN-ERROR
005000           MOVE FDP-EFF-DT    TO INBS2-EFF-DT(1)
005010           MOVE WS-BEG-BAL-DT TO INBS2-EFF-DT(2)
005020           MOVE FDP-CASE-KEY  TO INBS2-CASE-KEY(1)
005030           CALL 'CFHINBSD' USING RMESSAGE
005040                                 INBS2-DATA
005050                                 RTN-OUTPUT-FIELDS
005060       IF RTN-NO-ERROR
005070          PERFORM
005080          VARYING IINBS FROM 1 BY 1
005090          UNTIL   IINBS > INBS2-ROWS-FOUND
005100          OR      RTN-ERROR
005110             PERFORM B310-MATCH-TDTL-ARRAY
005120                THRU B310-EXIT
005130          END-PERFORM
005140       END-IF
005150
005160     END-PERFORM
005170     .
005180 B300-EXIT.
005190     EXIT.
005200
005210 B310-MATCH-TDTL-ARRAY.
005220
005230     MOVE 'N' TO WS-FOUND-TDTL-MATCH
005240     PERFORM
005250     VARYING WS-TD-SUB FROM 1 BY 1
005260       UNTIL WS-TD-SUB > WS-TD-CURR-SIZE
005270          OR WS-FOUND-TDTL-MATCH = 'Y'
005280        IF  INBS2-FD-DESC-ID(IINBS) =
005290                            WS-TD-FD-DESC-ID(WS-TD-SUB)
005300        AND INBS2-CHRL-KEY(IINBS) =
005310                            WS-TD-CHRL-KEY(WS-TD-SUB)
005320           MOVE 'Y' TO WS-FOUND-TDTL-MATCH
005330           IF  INBS2-BUY-SELL-CD (IINBS) = '1'
005340               COMPUTE INBS2-CASH-AMT(IINBS) =
005350                       INBS2-CASH-AMT(IINBS) * -1
005360               COMPUTE INBS2-UNIT-CT(IINBS) =
005370                       INBS2-UNIT-CT(IINBS) * -1
005380           END-IF
005390           COMPUTE WS-TD-AMT(WS-TD-SUB) =
005400              WS-TD-AMT(WS-TD-SUB) + INBS2-CASH-AMT(IINBS)
005410           COMPUTE WS-TD-UNIT-CT(WS-TD-SUB) =
005420              WS-TD-UNIT-CT(WS-TD-SUB) + INBS2-UNIT-CT(IINBS)
005430        END-IF
005440     END-PERFORM
005450
005460     IF WS-FOUND-TDTL-MATCH = 'N'
005470        ADD +1 TO WS-TD-CURR-SIZE
005480        MOVE WS-TD-CURR-SIZE TO WS-TD-SUB
005490        IF  INBS2-BUY-SELL-CD (IINBS) = '1'
005500            COMPUTE INBS2-CASH-AMT(IINBS) =
005510                    INBS2-CASH-AMT(IINBS) * -1
005520            COMPUTE INBS2-UNIT-CT(IINBS) =
005530                    INBS2-UNIT-CT(IINBS) * -1
005540        END-IF
005550        MOVE ZERO   TO WS-TD-BEG-BAL (WS-TD-SUB)
005560                       WS-TD-END-BAL (WS-TD-SUB)
005570                       WS-TD-AMT (WS-TD-SUB)
005580                       WS-TD-UNIT-CT(WS-TD-SUB)
005590        COMPUTE WS-TD-AMT(WS-TD-SUB) =
005600           WS-TD-AMT(WS-TD-SUB) + INBS2-CASH-AMT(IINBS)
005610        COMPUTE WS-TD-UNIT-CT(WS-TD-SUB) =
005620           WS-TD-UNIT-CT(WS-TD-SUB) + INBS2-UNIT-CT(IINBS)
005630        MOVE INBS2-FD-DESC-ID(IINBS) TO
005640                       WS-TD-FD-DESC-ID(WS-TD-SUB)
005650        MOVE INBS2-CHRL-KEY(IINBS) TO
005660                       WS-TD-CHRL-KEY(WS-TD-SUB)
005670     END-IF
005680
005690     .
005700 B310-EXIT.
005710     EXIT.
005720
005730 B400-PROCESS-ARRAY.
005740
005750     INITIALIZE TDTL2-DATA
005760
005770     PERFORM
005780     VARYING WS-TD-SUB FROM 1 BY 1
005790       UNTIL WS-TD-SUB > WS-TD-CURR-SIZE
005800        IF WS-TD-UNIT-CT(WS-TD-SUB) <> ZERO                       OMM0963
005810           PERFORM B410-GET-UNIT-PRICE
005820              THRU B410-EXIT
005830          IF RTN-NO-ERROR
005840             COMPUTE WS-TD-END-BAL(WS-TD-SUB) ROUNDED =
005850             WS-TD-UNIT-CT(WS-TD-SUB) *  UVII1-EFF-DT-UNIT-VALUE
005860             IF WS-TD-END-BAL (WS-TD-SUB) <> ZERO                 OMM0963
005870                PERFORM B420-INSERT-NOTIONAL-TDTL
005880                   THRU B420-EXIT
005890             END-IF
005900          END-IF
005910        END-IF
005920
005930        IF RTN-NO-ERROR
005940           PERFORM B430-INSERT-GAIN-LOSS-TDTL
005950              THRU B430-EXIT
005960        END-IF
005970
005980     END-PERFORM
005990
006000     .
006010 B400-EXIT.
006020     EXIT.
006030
006040 B410-GET-UNIT-PRICE.
006050
006060**   IF THE FD CHANGES FROM PREVIOUS CALL,                        OMM0480
006070**   SET THE OVRD FLAG TO SPACES AGAIN, SO                        OMM0480
006080**   CUTUVIID LOOKS IT UP AGAIN.                                  OMM0480
006090     IF WS-TD-FD-DESC-ID (WS-TD-SUB) <>                           OMM0480
006100        UVII1-FD-DESC-ID                                          OMM0480
006110        MOVE SPACES TO UVII1-UVAL-OVRD-CD                         OMM0480
006120     END-IF.                                                      OMM0480
006130     MOVE WS-TD-FD-DESC-ID(WS-TD-SUB)
006140                TO UVII1-FD-DESC-ID
006150     MOVE WS-TD-CHRL-KEY  (WS-TD-SUB)
006160                TO UVII1-CHRL-KEY
006170     MOVE FDP-EFF-DT TO UVII1-EFF-DT
006180     MOVE 'ABAL'     TO UVII1-TR-CD
006190*    UVII1-STAT-CD   = '0' => REGULAR PROCESSING
006200     MOVE '0'        TO UVII1-STAT-CD
006210*    UVII1-OVRD-PROC-CD = '00' => N/A
006220     MOVE '00'       TO UVII1-OVRD-PROC-CD
006230     MOVE FDP-CASE-KEY TO UVII1-CASE-KEY                          OMM0480
006240     CALL 'CUTUVIID' USING RMESSAGE,
006250                           UVII1-DATA,
006260                           RTN-OUTPUT-FIELDS
006270     .
006280 B410-EXIT.
006290     EXIT.
006300
006310 B420-INSERT-NOTIONAL-TDTL.
006320
006330** SETUP TDTL
006340     ADD 1      TO  TDTL2-ROWS-FOUND
006350
006360     MOVE 'ABAL' TO TDTL2-TR-CD (TDTL2-ROWS-FOUND)
006370
006380     MOVE WS-TD-FD-DESC-ID(WS-TD-SUB) TO
006390                       TDTL2-FD-DESC-ID (TDTL2-ROWS-FOUND)
006400
006410     MOVE WS-TD-CHRL-KEY(WS-TD-SUB) TO
006420                       TDTL2-CHRL-KEY (TDTL2-ROWS-FOUND)
006430
006440     MOVE FDP-CASE-KEY TO
006450                       TDTL2-CASE-KEY(TDTL2-ROWS-FOUND)
006460
006470     MOVE WS-TD-END-BAL (WS-TD-SUB) TO
006480                       TDTL2-AMT (TDTL2-ROWS-FOUND)
006490                       TDTL2-PRNC-AMT(TDTL2-ROWS-FOUND)
006500
006510     MOVE WS-TD-UNIT-CT (WS-TD-SUB) TO
006520                       TDTL2-UNIT-CT(TDTL2-ROWS-FOUND)
006530
006540*    NOTIONAL
006550     MOVE '2' TO TDTL2-STAT-CD (TDTL2-ROWS-FOUND)
006560
006570*    APPLY NORMAL PROCESSING RULES
006580*    SET TO N/A
006590     MOVE '0'
006600     TO TDTL2-INT-INCL-CD        (TDTL2-ROWS-FOUND)
006610
006620*    DATES
006630     MOVE FDP-EFF-DT TO
006640                         TDTL2-EFF-DT(TDTL2-ROWS-FOUND)
006650                         TDTL2-DUE-DT(TDTL2-ROWS-FOUND)
006660
006670     MOVE FDP-CYC-DT TO
006680                         TDTL2-TX-DT (TDTL2-ROWS-FOUND)
006690                         TDTL2-PROC-DT (TDTL2-ROWS-FOUND)
006700
006710     MOVE SPACES TO TDTL2-BOOK-DT(TDTL2-ROWS-FOUND)
006720
006730     MOVE FDP-TR-REF-NO
006740        TO TDTL2-TR-REF-NO          (TDTL2-ROWS-FOUND)
006750
006760     MOVE FDP-SCAT-KEY
006770        TO TDTL2-SCAT-KEY           (TDTL2-ROWS-FOUND)
006780
006790     MOVE ZEROS TO
006800           TDTL2-BKT-NO             (TDTL2-ROWS-FOUND)
006810           TDTL2-FD-NO              (TDTL2-ROWS-FOUND)
006820           TDTL2-CASE-MBR-KEY       (TDTL2-ROWS-FOUND)
006830           TDTL2-FDRT-KEY           (TDTL2-ROWS-FOUND)
006840           TDTL2-TRNCH-KEY          (TDTL2-ROWS-FOUND)
006850           TDTL2-ORIG-SCAT-KEY      (TDTL2-ROWS-FOUND)
006860           TDTL2-MNY-TYP-NO         (TDTL2-ROWS-FOUND)
006870
006880     MOVE SPACES TO
006890           TDTL2-MNY-CAT-NUM        (TDTL2-ROWS-FOUND)
006900           TDTL2-UNALLOC-ACCT-CD    (TDTL2-ROWS-FOUND)
006910           TDTL2-RVSL-CYC-DT        (TDTL2-ROWS-FOUND)
006920
006930     MOVE ZERO TO TDTL2-REF-KEY(TDTL2-ROWS-FOUND)
006940                  TDTL2-DEF-BY-NUM (TDTL2-ROWS-FOUND)
006950
006960** INSERT A BLOCK OF 100
006970     IF TDTL2-ROWS-FOUND = 100 AND RTN-NO-ERROR
006980        PERFORM C000-TDTL-INSERT
006990           THRU C000-EXIT
007000        INITIALIZE TDTL2-DATA
007010     END-IF
007020
007030     .
007040 B420-EXIT.
007050     EXIT.
007060
007070 B430-INSERT-GAIN-LOSS-TDTL.
007080
007090     COMPUTE WS-GAIN-LOSS-AMT =
007100             WS-TD-END-BAL (WS-TD-SUB)
007110           - WS-TD-AMT (WS-TD-SUB)
007120           - WS-TD-BEG-BAL (WS-TD-SUB)
007130
007140     IF WS-GAIN-LOSS-AMT = ZERO
007150        GO TO B430-EXIT
007160     END-IF
007170
007180** SETUP TDTL
007190     ADD 1      TO  TDTL2-ROWS-FOUND
007200
007210     IF WS-GAIN-LOSS-AMT > ZERO
007220        MOVE WS-TR-CD-GAIN-17 TO TDTL2-TR-CD (TDTL2-ROWS-FOUND)
007230     ELSE
007240        MOVE WS-TR-CD-LOSS-18 TO TDTL2-TR-CD (TDTL2-ROWS-FOUND)
007250        COMPUTE WS-GAIN-LOSS-AMT =
007260                WS-GAIN-LOSS-AMT * -1
007270     END-IF
007280
007290     MOVE WS-GAIN-LOSS-AMT TO
007300                       TDTL2-AMT (TDTL2-ROWS-FOUND)
007310
007320     MOVE WS-TD-FD-DESC-ID(WS-TD-SUB) TO
007330                       TDTL2-FD-DESC-ID (TDTL2-ROWS-FOUND)
007340
007350     MOVE WS-TD-CHRL-KEY(WS-TD-SUB) TO
007360                       TDTL2-CHRL-KEY (TDTL2-ROWS-FOUND)
007370
007380     MOVE FDP-CASE-KEY TO
007390                       TDTL2-CASE-KEY(TDTL2-ROWS-FOUND)
007400
007410*    NOTIONAL
007420     MOVE '2' TO TDTL2-STAT-CD (TDTL2-ROWS-FOUND)
007430
007440*    APPLY NORMAL PROCESSING RULES
007450*    SET TO N/A
007460     MOVE '0'
007470        TO TDTL2-INT-INCL-CD        (TDTL2-ROWS-FOUND)
007480
007490*    DATES
007500     MOVE FDP-EFF-DT TO
007510                         TDTL2-EFF-DT(TDTL2-ROWS-FOUND)
007520                         TDTL2-DUE-DT(TDTL2-ROWS-FOUND)
007530
007540     MOVE FDP-CYC-DT TO
007550                         TDTL2-TX-DT (TDTL2-ROWS-FOUND)
007560                         TDTL2-PROC-DT (TDTL2-ROWS-FOUND)
007570                         TDTL2-BOOK-DT(TDTL2-ROWS-FOUND)
007580
007590
007600     MOVE FDP-TR-REF-NO
007610        TO TDTL2-TR-REF-NO          (TDTL2-ROWS-FOUND)
007620
007630     MOVE FDP-SCAT-KEY
007640        TO TDTL2-SCAT-KEY           (TDTL2-ROWS-FOUND)
007650
007660     MOVE ZEROS TO
007670           TDTL2-BKT-NO             (TDTL2-ROWS-FOUND)
007680           TDTL2-FD-NO              (TDTL2-ROWS-FOUND)
007690           TDTL2-CASE-MBR-KEY       (TDTL2-ROWS-FOUND)
007700           TDTL2-FDRT-KEY           (TDTL2-ROWS-FOUND)
007710           TDTL2-TRNCH-KEY          (TDTL2-ROWS-FOUND)
007720           TDTL2-ORIG-SCAT-KEY      (TDTL2-ROWS-FOUND)
007730           TDTL2-MNY-TYP-NO         (TDTL2-ROWS-FOUND)
007740           TDTL2-PRNC-AMT(TDTL2-ROWS-FOUND)
007750           TDTL2-UNIT-CT(TDTL2-ROWS-FOUND)
007760
007770     MOVE SPACES TO
007780           TDTL2-MNY-CAT-NUM        (TDTL2-ROWS-FOUND)
007790           TDTL2-UNALLOC-ACCT-CD    (TDTL2-ROWS-FOUND)
007800           TDTL2-RVSL-CYC-DT        (TDTL2-ROWS-FOUND)
007810
007820     MOVE ZERO TO TDTL2-REF-KEY(TDTL2-ROWS-FOUND)
007830                     TDTL2-DEF-BY-NUM (TDTL2-ROWS-FOUND)
007840
007850** INSERT A BLOCK OF 100
007860     IF TDTL2-ROWS-FOUND = 100 AND RTN-NO-ERROR
007870        PERFORM C000-TDTL-INSERT
007880           THRU C000-EXIT
007890        INITIALIZE TDTL2-DATA
007900     END-IF
007910
007920     .
007930 B430-EXIT.
007940     EXIT.
007950
007960 C000-TDTL-INSERT.
007970
007980     CALL 'CI1TDTLD' USING RMESSAGE,
007990                           TDTL2-DATA,
008000                           RTN-OUTPUT-FIELDS.
008010
008020 C000-EXIT.
008030     EXIT.
008040
008050 X000-CALL-CUTDTCAL.
008060
008070     CALL 'CUTDTCAB' USING RMESSAGE,                              LSM5259
008080                           DATEPARMS,                             LSM5259
008090                           RTN-OUTPUT-FIELDS                      LSM5259
008100
008110     IF LK-ERROR-CD <> ZEROES
008120        MOVE WS-PGM-NAME  TO RTN-PROG-NAME
008130        MOVE 'X000'       TO RTN-PARA-NAME
008140        MOVE 'UT'         TO RTN-ERR-PREFIX
008150        MOVE 25           TO RTN-ERROR-CODE
008160        CALL 'CUTSVARB' USING RMESSAGE,
008170                              RTN-OUTPUT-FIELDS
008180     END-IF.
008190
008200 X000-EXIT.
008210     EXIT.
008220
008230**
008240**   E N D     O F    P R O G R A M
008250**
