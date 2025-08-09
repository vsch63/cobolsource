000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.
000040 PROGRAM-ID.           CS1BEOPD.
000050 AUTHOR.               GENE PALUMBO.
000060 DATE-WRITTEN.         2001-06-22.
000070****************************************************************
000080*   $Modtime::   10 Oct 2018 17:07:52  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CS1BEOPD.COV  $
000100*
000110*   Rev 1.0.1.19   10 Oct 2018 17:33:00   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.18   24 Jun 2015 14:56:56   prasuna
000160*CTM0005
000170*CTM0005 Modifications
000180*
000190*   Rev 1.0.1.17   21 Jul 2010 02:50:04   shiv
000200*PDM2142
000210*Add Para A250-SELECT-MBGP-AND-BNTP
000220*
000230*   Rev 1.0.1.16   14 May 2010 08:47:04   jegadeesan
000240*FDM3874
000250*Add BGRP_KEY
000260*
000270*   Rev 1.0.1.15   27 Jul 2009 14:16:46   shane
000280*MOM0531
000290*Add NMBD-KEY to A200 where
000300*
000310*   Rev 1.0.1.14   13 Nov 2006 16:31:58   haripriya
000320*FDM3579
000330*CHANGED RPAD LENGTH
000340*
000350*   Rev 1.0.1.13   07 Nov 2006 10:17:08   tom
000360*FDM3579
000370*Fix for NVL problem when invoked from external procedure
000380*
000390*   Rev 1.0.1.12   07 Feb 2006 17:17:16   paul
000400*PFM0091
000410*Free Cover Limits
000420*
000430*   Rev 1.0.1.11   23 Sep 2005 12:22:16   robin
000440*FDM3514
000450*Module Impact Analysis Tool - WS-REVISION awk script fix.
000460*
000470*   Rev 1.0.1.10   21 Sep 2005 17:19:30   robin
000480*FDM3514
000490*Module Impact Analysis Tool - awk script fix
000500*
000510*   Rev 1.0.1.7   20 May 2005 08:36:04   sahiba
000520*MOW0150
000530*use typ_cd where passed in
000540*
000550*   Rev 1.0.1.6   11 Apr 2005 07:13:14   michelle
000560*MOW0135
000570*Add CMRS_KEY
000580*
000590*   Rev 1.0.1.5   22 Jan 2003 14:50:58   francesm
000600*CAQ0164
000610*Allow for Member/Benefit Benefit Options
000620*
000630*   Rev 1.0.1.4   20 Dec 2002 16:58:28   rieko
000640*FDM3175
000650*COBOL modifications to support the Oracle 8 pre-complier.
000660*
000670*   Rev 1.0.1.3   25 Oct 2002 14:50:54   francesm
000680*CAM0042
000690*Select Member, Benefit, or Scheme level
000700*
000710*   Rev 1.0.1.2   16 May 2002 16:06:26   mark
000720*FTS0397
000730*Benefit Options Data
000740*
000750*   Rev 1.0.1.1   09 Jul 2001 14:44:52   gene
000760*FTM0002
000770*Central Data Repository
000780****************************************************************
000790* THIS PROGRAM WILL SELECT DATA FROM BEN_OPTS
000800*
000810*  INPUT PARMS:  BELA_KEY AND BNTP_KEY AND CASE_KEY
000820*
000830****************************************************************
000840*                     CHANGES LOG
000850****************************************************************
000860*
000870*   DATE      PRG    PROJ#      DESCRIPTION
000880*  ---------  ---  ---------  ----------------------------------
000890*  23-JUN-01  GDP  FTM0002    INITIAL CHECK-IN
000900*  15-MAY-02  MCD  FTS0397    ALLOW FOR NULL BEN_OPT_DATA         FTS0397
000910*  15-OCT-02  FRM  CAM0042    ALLOW FOR MEMBER LEVEL              CAM0042
000920*  07-JAN-03  FRM  CAQ0164    ALLOW FOR MEMBER/BENEFIT LEVEL      CAQ0164
000930*  11-MAPR-05  ML  MOW0135    ALLOW FOR DEPENDANT LEVEL           MOW0135
000940*  18-MAY-05  SA   MOW0150    EXCLUDE CMRS-KEY                    MOW0150
000950*  11-JAN-06  PDT  PFM0091    ALLOW FOR MEMBER GROUP LEVEL
000960*  10-NOV-06  HM   FDM3579    CHANGE RPAD LENGTH
000970*  29-MAY-09  SM   MOM0531    IN SECTION A200 ADD NMBD-KEY IS NULLMOM0531
000980*                             TO STOP MULTIPLE ROWS.              MOM0531
000990*  09-JUl-10  SNG  PDM2142    ADD SELECT FOR MBGP WITH BNTP  AND
001000*                             CHANGE THE CONDITION SO THAT MBGP
001010*                             IS TESTED BEFORE BNTP
001020*  15-JUN-15  PS   CTM0005    ADDED SELECT FOR CSMB AND TRNCH-KEY CTM0005
001030****************************************************************
001040
001050 ENVIRONMENT DIVISION.
001060 CONFIGURATION SECTION.
001070*
001080 DATA DIVISION.
001090 WORKING-STORAGE SECTION.
001100
001110 01 WS-PROG-NM                    PIC X(08) VALUE 'CS1BEOPD'.
001120
001130     EXEC SQL BEGIN DECLARE SECTION   END-EXEC.
001140
001150 01 HV-MISC-VARS.
001160    05 HV-BELA-KEY                PIC S9(15) COMP-3.
001170    05 HV-CASE-KEY                PIC S9(15) COMP-3.
001180    05 HV-CASE-MBR-KEY            PIC S9(15) COMP-3.              CAM0042
001190    05 HV-CMRS-KEY                PIC S9(15) COMP-3.              MOW0135
001200    05 HV-BNTP-KEY                PIC S9(15) COMP-3.
001210    05 HV-MBGP-KEY                PIC S9(15) COMP-3.              PFM0091
001220    05 HV-BGRP-KEY                PIC S9(15) COMP-3.              FDM3874
001230    05 HV-TRNCH-KEY               PIC S9(15) COMP-3.              CTM0005
001240    05 HV-BEN-OPT-DATA            PIC X(300).                     CAM0042
001250    05 HV-EFF-DT                  PIC X(10).
001260
001270     EXEC SQL END DECLARE SECTION END-EXEC.                       FDM3175
001280**
001290**   OTHER WORKING STORAGE COPYBOOKS
001300**
001310     COPY SQLCA.
001320     COPY CWSUTSQL.
001330
001340*********************************************************
001350**   P R O C E D U R E    D I V I S I O N              **
001360*********************************************************
001370
001380**  REVISION AREA **                                              FDM3514
001390 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
001400     ' $Revision::   1.0.1.19       $'.
001410 LINKAGE SECTION.
001420     EXEC SQL BEGIN DECLARE SECTION END-EXEC.                     FDM3175
001430     COPY CLKFDPNC.
001440     EXEC SQL INCLUDE CL1BEOPD.COB END-EXEC.                      FDM3175
001450     EXEC SQL END DECLARE SECTION END-EXEC.
001460     COPY CLKERRBC.
001470
001480 PROCEDURE DIVISION USING RMESSAGE,
001490                          BEOP1-DATA,
001500                          RTN-OUTPUT-FIELDS.
001510     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514
001520        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
001530        MOVE WHEN-COMPILED TO                                     FDM3514
001540             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
001550        MOVE WS-REVISION (16:10) TO                               FDM3514
001560             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
001570        MOVE WS-PROG-NM TO                                        MOM0531
001580           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
001590        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
001600                              RTN-OUTPUT-FIELDS                   FDM3514
001610     END-IF.                                                      FDM3514
001620
001630* CHANGED NESTED IF ELSE CONDITION TO EVALUATE                    PDM2142
001640     EVALUATE TRUE                                                PDM2142
001650        WHEN BEOP1-CMRS-KEY > ZERO AND BEOP1-BNTP-KEY > ZERO      PDM2142
001660           PERFORM A500-SELECT-DBEN-LEVEL THRU                    MOW0135
001670                   A500-EXIT                                      MOW0135
001680        WHEN BEOP1-CMRS-KEY > ZERO AND BEOP1-BNTP-KEY = ZERO      PDM2142
001690           PERFORM A600-SELECT-DPDT-LEVEL THRU                    MOW0135
001700                   A600-EXIT                                      MOW0135
001710        WHEN BEOP1-CASE-MBR-KEY > ZERO AND BEOP1-BNTP-KEY > ZERO  PDM2142
001720           PERFORM A400-SELECT-MBEN-LEVEL THRU                    CAQ0164
001730                   A400-EXIT                                      CAQ0164
001740        WHEN BEOP1-CASE-MBR-KEY > ZERO
001750           PERFORM A300-SELECT-WITH-CSMB-KEY THRU
001760                   A300-EXIT
001770        WHEN BEOP1-MBGP-KEY > ZERO AND BEOP1-BNTP-KEY > ZEROS     PDM2142
001780           PERFORM A250-SELECT-MBGP-AND-BNTP THRU                 PDM2142
001790                   A250-EXIT                                      PDM2142
001800        WHEN BEOP1-MBGP-KEY > ZERO                                PDM2142
001810           PERFORM A150-SELECT-WITH-MBGP-KEY THRU                 PFM0091
001820                   A150-EXIT                                      PFM0091
001830        WHEN BEOP1-BNTP-KEY > ZERO                                PDM2142
001840           PERFORM A200-SELECT-WITH-BNTP-KEY THRU                 PDM2142
001850                   A200-EXIT                                      PDM2142
001860        WHEN BEOP1-BGRP-KEY > ZERO                                PDM2142     
001870           PERFORM A700-SELECT-WITH-BGRP-KEY THRU                 FDM3874
001880                   A700-EXIT                                      FDM3874
001890        WHEN  BEOP1-TRNCH-KEY > ZERO                              CTM0005
001900           PERFORM A800-SELECT-TRNCH-LEVEL THRU                   CTM0005
001910                   A800-EXIT                                      CTM0005
001920        WHEN OTHER                                                PDM2142
001930           PERFORM A100-SELECT-WITH-CASE-KEY THRU
001940                   A100-EXIT
001950     END-EVALUATE.                                                PDM2142
001960
001970**
001980**   E N D     O F    P R O G R A M
001990**
002000     GOBACK.
002010
002020
002030 A100-SELECT-WITH-CASE-KEY.
002040
002050     MOVE BEOP1-CASE-KEY     TO HV-CASE-KEY.
002060     MOVE BEOP1-BELA-KEY     TO HV-BELA-KEY.
002070     MOVE BEOP1-BEN-OPT-DATA TO HV-BEN-OPT-DATA.                  CAM0042
002080     MOVE BEOP1-EFF-DT       TO HV-EFF-DT.
002090
002100     EXEC SQL
002110        SELECT
002120            BEOP_KEY
002130           ,RPAD(NVL(BEN_OPT_DATA,' '),300,' ')                   FDM3579
002140           ,TO_CHAR(EFF_DT)
002150           ,RPAD(NVL(BEN_CAT_NO,' '),2,' ')                       FDM3874
002160           ,RPAD(NVL(TO_CHAR(XPIR_DT),' '),10,' ')                FDM3874
002170        INTO
002180            :BEOP1-BEOP-KEY
002190           ,:BEOP1-BEN-OPT-DATA
002200           ,:BEOP1-EFF-DT
002210           ,:BEOP1-BEN-CAT-NO
002220           ,:BEOP1-XPIR-DT
002230       FROM BEN_OPTS
002240         WHERE
002250             CASE_KEY      = :HV-CASE-KEY AND
002260             CASE_KEY      IS NOT NULL    AND                     CAM0042
002270             BELA_KEY      = :HV-BELA-KEY AND
002280             BNTP_KEY      IS NULL AND
002290             CASE_MBR_KEY  IS NULL AND                            CAM0042
002300             CMRS_KEY      IS NULL AND                            MOW0150
002310             BEN_OPT_DATA  = :HV-BEN-OPT-DATA     AND             CAM0042
002320             EFF_DT        <= TO_DATE(:HV-EFF-DT) AND
002330            (XPIR_DT       >= TO_DATE(:HV-EFF-DT) OR
002340             XPIR_DT       IS NULL)
002350     END-EXEC.
002360
002370     MOVE SQLCODE TO SQL-ERROR-FLAG.
002380
002390     IF SQL-ERROR
002400        IF SQ-NOT-FOUND
002410           MOVE 0                      TO BEOP1-ROWS-FOUND
002420        ELSE
002430           MOVE 'SQ'                   TO RTN-ERR-PREFIX
002440           MOVE WS-PROG-NM             TO RTN-PROG-NAME
002450           MOVE 'A100'                 TO RTN-PARA-NAME
002460           MOVE SQLCODE                TO RTN-SQLCODE
002470           MOVE 'BEN_OPTS'             TO RTN-TABLE-NAME
002480           MOVE SQ-SELECT-ERR          TO RTN-ERROR-CODE
002490           MOVE 1                      TO RTN-VAR-SEQ-NO (1)      FDM3874       
002500                                          RTN-VAR-DETL-CNT        FDM3874      
002510           MOVE SQLERRMC               TO RTN-VAR-TEXT (1)        FDM3874
002520           CALL 'CUTSVARB' USING RMESSAGE,
002530                                 RTN-OUTPUT-FIELDS
002540     ELSE
002550        MOVE +1 TO BEOP1-ROWS-FOUND
002560        IF FDP-DEBUG > 0
002570           DISPLAY WS-PROG-NM ' ' BEOP1-ROWS-FOUND
002580                   ' BEN_OPTS DAL'
002590        END-IF
002600     END-IF.
002610
002620 A100-EXIT.
002630     EXIT.
002640
002650 A150-SELECT-WITH-MBGP-KEY.                                       PFM0091
002660                                                                  PFM0091
002670     MOVE BEOP1-MBGP-KEY     TO HV-MBGP-KEY.                      PFM0091
002680     MOVE BEOP1-BELA-KEY     TO HV-BELA-KEY.                      PFM0091
002690     MOVE BEOP1-BEN-OPT-DATA TO HV-BEN-OPT-DATA.                  PFM0091
002700     MOVE BEOP1-EFF-DT       TO HV-EFF-DT.                        PFM0091
002710                                                                  PFM0091
002720     EXEC SQL                                                     PFM0091
002730        SELECT                                                    PFM0091
002740            BEOP_KEY                                              PFM0091
002750           ,RPAD(NVL(BEN_OPT_DATA,' '),300,' ')                   FDM3579
002760           ,TO_CHAR(EFF_DT)                                       PFM0091
002770           ,RPAD(NVL(BEN_CAT_NO,' '),2,' ')                       FDM3874
002780           ,RPAD(NVL(TO_CHAR(XPIR_DT),' '),10,' ')                FDM3874
002790        INTO                                                      PFM0091
002800            :BEOP1-BEOP-KEY                                       PFM0091
002810           ,:BEOP1-BEN-OPT-DATA                                   PFM0091
002820           ,:BEOP1-EFF-DT                                         PFM0091
002830           ,:BEOP1-BEN-CAT-NO                                     PFM0091
002840           ,:BEOP1-XPIR-DT                                        PFM0091
002850       FROM BEN_OPTS                                              PFM0091
002860         WHERE                                                    PFM0091
002870             MBGP_KEY      = :HV-MBGP-KEY AND                     PFM0091
002880             CASE_KEY      IS NULL        AND                     PFM0091
002890             BELA_KEY      = :HV-BELA-KEY AND                     PFM0091
002900             BNTP_KEY      IS NULL AND                            PFM0091
002910             CASE_MBR_KEY  IS NULL AND                            PFM0091
002920             BEN_OPT_DATA  = :HV-BEN-OPT-DATA     AND             PFM0091
002930             EFF_DT        <= TO_DATE(:HV-EFF-DT) AND             PFM0091
002940            (XPIR_DT       >= TO_DATE(:HV-EFF-DT) OR              PFM0091
002950             XPIR_DT       IS NULL)                               PFM0091
002960     END-EXEC.                                                    PFM0091
002970                                                                  PFM0091
002980     MOVE SQLCODE TO SQL-ERROR-FLAG.                              PFM0091
002990                                                                  PFM0091
003000     IF SQL-ERROR                                                 PFM0091
003010        IF SQ-NOT-FOUND                                           PFM0091
003020           MOVE 0                      TO BEOP1-ROWS-FOUND        PFM0091
003030        ELSE                                                      PFM0091
003040           MOVE 'SQ'                   TO RTN-ERR-PREFIX          PFM0091
003050           MOVE WS-PROG-NM             TO RTN-PROG-NAME           PFM0091
003060           MOVE 'A150'                 TO RTN-PARA-NAME           PFM0091
003070           MOVE SQLCODE                TO RTN-SQLCODE             PFM0091
003080           MOVE 'BEN_OPTS'             TO RTN-TABLE-NAME          PFM0091
003090           MOVE SQ-SELECT-ERR          TO RTN-ERROR-CODE          PFM0091
003100           MOVE 1                      TO RTN-VAR-SEQ-NO (1)      FDM3874       
003110                                          RTN-VAR-DETL-CNT        FDM3874      
003120           MOVE SQLERRMC               TO RTN-VAR-TEXT (1)        FDM3874
003130           CALL 'CUTSVARB' USING RMESSAGE,                        PFM0091
003140                                 RTN-OUTPUT-FIELDS                PFM0091
003150     ELSE                                                         PFM0091
003160        MOVE +1 TO BEOP1-ROWS-FOUND                               PFM0091
003170        IF FDP-DEBUG > 0                                          PFM0091
003180           DISPLAY WS-PROG-NM ' ' BEOP1-ROWS-FOUND                PFM0091
003190                   ' BEN_OPTS DAL'                                PFM0091
003200        END-IF                                                    PFM0091
003210     END-IF.                                                      PFM0091
003220                                                                  PFM0091
003230 A150-EXIT.                                                       PFM0091
003240     EXIT.                                                        PFM0091
003250
003260 A200-SELECT-WITH-BNTP-KEY.
003270
003280     MOVE BEOP1-BELA-KEY     TO HV-BELA-KEY.
003290     MOVE BEOP1-BNTP-KEY     TO HV-BNTP-KEY.
003300     MOVE BEOP1-BEN-OPT-DATA TO HV-BEN-OPT-DATA.                  CAM0042
003310     MOVE BEOP1-EFF-DT       TO HV-EFF-DT.
003320
003330     EXEC SQL
003340        SELECT
003350            BEOP_KEY
003360           ,RPAD(NVL(BEN_OPT_DATA,' '),300,' ')                   FDM3579
003370           ,TO_CHAR(EFF_DT)
003380           ,RPAD(NVL(BEN_CAT_NO,' '),2,' ')                       FDM3874
003390           ,RPAD(NVL(TO_CHAR(XPIR_DT),' '),10,' ')                FDM3874
003400        INTO
003410            :BEOP1-BEOP-KEY
003420           ,:BEOP1-BEN-OPT-DATA
003430           ,:BEOP1-EFF-DT
003440           ,:BEOP1-BEN-CAT-NO
003450           ,:BEOP1-XPIR-DT
003460       FROM BEN_OPTS
003470         WHERE
003480             BNTP_KEY      = :HV-BNTP-KEY AND
003490             BNTP_KEY      IS NOT NULL AND                        CAM0042
003500             BELA_KEY      = :HV-BELA-KEY AND
003510             CASE_MBR_KEY  IS NULL AND                            CAM0042
003520             CMRS_KEY      IS NULL AND                            MOW0150
003530             NMBD_KEY      IS NULL AND                            MOM0531
003540             MBGP_KEY      IS NULL AND                            PDM2142
003550             BEN_OPT_DATA  = :HV-BEN-OPT-DATA    AND              CAM0042
003560             EFF_DT       <= TO_DATE(:HV-EFF-DT) AND
003570            (XPIR_DT      >= TO_DATE(:HV-EFF-DT) OR
003580             XPIR_DT       IS NULL)
003590     END-EXEC.
003600
003610     MOVE SQLCODE TO SQL-ERROR-FLAG.
003620
003630     IF SQL-ERROR
003640        IF SQ-NOT-FOUND
003650           MOVE 0                      TO BEOP1-ROWS-FOUND
003660        ELSE
003670           MOVE 'SQ'                   TO RTN-ERR-PREFIX
003680           MOVE WS-PROG-NM             TO RTN-PROG-NAME
003690           MOVE 'A200'                 TO RTN-PARA-NAME
003700           MOVE SQLCODE                TO RTN-SQLCODE
003710           MOVE 'BEN_OPTS'             TO RTN-TABLE-NAME
003720           MOVE SQ-SELECT-ERR          TO RTN-ERROR-CODE
003730           MOVE 1                      TO RTN-VAR-SEQ-NO (1)      FDM3874       
003740                                          RTN-VAR-DETL-CNT        FDM3874      
003750           MOVE SQLERRMC               TO RTN-VAR-TEXT (1)        FDM3874
003760           CALL 'CUTSVARB' USING RMESSAGE,
003770                                 RTN-OUTPUT-FIELDS
003780     ELSE
003790        MOVE +1 TO BEOP1-ROWS-FOUND
003800        IF FDP-DEBUG > 0
003810           DISPLAY WS-PROG-NM ' ' BEOP1-ROWS-FOUND
003820                   ' BEN_OPTS DAL'
003830        END-IF
003840     END-IF.
003850
003860 A200-EXIT.
003870     EXIT.
003880
003890 A250-SELECT-MBGP-AND-BNTP.                                       PDM2142
003900                                                                  PDM2142
003910     MOVE BEOP1-MBGP-KEY     TO HV-MBGP-KEY.                      PDM2142
003920     MOVE BEOP1-BELA-KEY     TO HV-BELA-KEY.                      PDM2142
003930     MOVE BEOP1-BEN-OPT-DATA TO HV-BEN-OPT-DATA.                  PDM2142
003940     MOVE BEOP1-EFF-DT       TO HV-EFF-DT.                        PDM2142
003950     MOVE BEOP1-BNTP-KEY     TO HV-BNTP-KEY.                      PDM2142 
003960                                                                  PDM2142
003970     EXEC SQL                                                     PDM2142
003980        SELECT                                                    PDM2142
003990            BEOP_KEY                                              PDM2142
004000           ,RPAD(NVL(BEN_OPT_DATA,' '),300,' ')                   PDM2142
004010           ,TO_CHAR(EFF_DT)                                       PDM2142
004020           ,RPAD(NVL(BEN_CAT_NO,' '),2,' ')                       PDM2142
004030           ,RPAD(NVL(TO_CHAR(XPIR_DT),' '),10,' ')                PDM2142
004040        INTO                                                      PDM2142
004050            :BEOP1-BEOP-KEY                                       PDM2142
004060           ,:BEOP1-BEN-OPT-DATA                                   PDM2142
004070           ,:BEOP1-EFF-DT                                         PDM2142
004080           ,:BEOP1-BEN-CAT-NO                                     PDM2142
004090           ,:BEOP1-XPIR-DT                                        PDM2142
004100       FROM BEN_OPTS                                              PDM2142
004110         WHERE                                                    PDM2142
004120             MBGP_KEY      = :HV-MBGP-KEY AND                     PDM2142
004130             CASE_KEY      IS NULL        AND                     PDM2142
004140             BELA_KEY      = :HV-BELA-KEY AND                     PDM2142
004150             BNTP_KEY      = :HV-BNTP-KEY AND                     PDM2142
004160             BNTP_KEY      IS NOT NULL AND                        PDM2142
004170             CASE_MBR_KEY  IS NULL AND                            PDM2142
004180             BEN_OPT_DATA  = :HV-BEN-OPT-DATA     AND             PDM2142
004190             EFF_DT        <= TO_DATE(:HV-EFF-DT) AND             PDM2142
004200            (XPIR_DT       >= TO_DATE(:HV-EFF-DT) OR              PDM2142
004210             XPIR_DT       IS NULL)                               PDM2142
004220     END-EXEC.                                                    PDM2142
004230                                                                  PDM2142
004240     MOVE SQLCODE TO SQL-ERROR-FLAG.                              PDM2142
004250                                                                  PDM2142
004260     IF SQL-ERROR                                                 PDM2142
004270        IF SQ-NOT-FOUND                                           PDM2142
004280           MOVE 0                      TO BEOP1-ROWS-FOUND        PDM2142
004290        ELSE                                                      PDM2142
004300           MOVE 'SQ'                   TO RTN-ERR-PREFIX          PDM2142
004310           MOVE WS-PROG-NM             TO RTN-PROG-NAME           PDM2142
004320           MOVE 'A250'                 TO RTN-PARA-NAME           PDM2142
004330           MOVE SQLCODE                TO RTN-SQLCODE             PDM2142
004340           MOVE 'BEN_OPTS'             TO RTN-TABLE-NAME          PDM2142
004350           MOVE SQ-SELECT-ERR          TO RTN-ERROR-CODE          PDM2142
004360           MOVE 1                      TO RTN-VAR-SEQ-NO (1)      PDM2142
004370                                          RTN-VAR-DETL-CNT        PDM2142
004380           MOVE SQLERRMC               TO RTN-VAR-TEXT (1)        PDM2142
004390           CALL 'CUTSVARB' USING RMESSAGE,                        PDM2142
004400                                 RTN-OUTPUT-FIELDS                PDM2142
004410     ELSE                                                         PDM2142
004420        MOVE +1 TO BEOP1-ROWS-FOUND                               PDM2142
004430        IF FDP-DEBUG > 0                                          PDM2142
004440           DISPLAY WS-PROG-NM ' ' BEOP1-ROWS-FOUND                PDM2142
004450                   ' BEN_OPTS DAL'                                PDM2142
004460        END-IF                                                    PDM2142
004470     END-IF.                                                      PDM2142
004480                                                                  PDM2142
004490 A250-EXIT.                                                       PDM2142
004500     EXIT.                                                        PDM2142
004510                                                                  PDM2142
004520 A300-SELECT-WITH-CSMB-KEY.                                       CAM0042
004530
004540     MOVE BEOP1-BELA-KEY     TO HV-BELA-KEY.                      CAM0042
004550     MOVE BEOP1-CASE-MBR-KEY TO HV-CASE-MBR-KEY.                  CAM0042
004560     MOVE BEOP1-BEN-OPT-DATA TO HV-BEN-OPT-DATA.                  CAM0042
004570     MOVE BEOP1-EFF-DT       TO HV-EFF-DT.                        CAM0042
004580
004590     EXEC SQL                                                     CAM0042
004600        SELECT                                                    CAM0042
004610            BEOP_KEY                                              CAM0042
004620           ,RPAD(NVL(BEN_OPT_DATA,' '),300,' ')                   FDM3579
004630           ,TO_CHAR(EFF_DT)                                       CAM0042
004640           ,RPAD(NVL(BEN_CAT_NO,' '),2,' ')                       FDM3874
004650           ,RPAD(NVL(TO_CHAR(XPIR_DT),' '),10,' ')                FDM3874
004660        INTO                                                      CAM0042
004670            :BEOP1-BEOP-KEY                                       CAM0042
004680           ,:BEOP1-BEN-OPT-DATA                                   CAM0042
004690           ,:BEOP1-EFF-DT                                         CAM0042
004700           ,:BEOP1-BEN-CAT-NO                                     CAM0042
004710           ,:BEOP1-XPIR-DT                                        CAM0042
004720       FROM BEN_OPTS                                              CAM0042
004730         WHERE                                                    CAM0042
004740             CASE_MBR_KEY  = :HV-CASE-MBR-KEY    AND              CAM0042
004750             CASE_MBR_KEY  IS NOT NULL AND                        CAM0042
004760             BELA_KEY      = :HV-BELA-KEY AND                     CAM0042
004770             BNTP_KEY      IS NULL AND                            CAM0042
004780             BEN_OPT_DATA  = :HV-BEN-OPT-DATA    AND              CAM0042
004790             EFF_DT       <= TO_DATE(:HV-EFF-DT) AND              CAM0042
004800            (XPIR_DT      >= TO_DATE(:HV-EFF-DT) OR               CAM0042
004810             XPIR_DT       IS NULL)                               CAM0042
004820     END-EXEC.                                                    CAM0042
004830                                                                  CAM0042
004840     MOVE SQLCODE TO SQL-ERROR-FLAG.                              CAM0042
004850                                                                  CAM0042
004860     IF SQL-ERROR                                                 CAM0042
004870        IF SQ-NOT-FOUND                                           CAM0042
004880           MOVE 0                      TO BEOP1-ROWS-FOUND        CAM0042
004890        ELSE                                                      CAM0042
004900           MOVE 'SQ'                   TO RTN-ERR-PREFIX          CAM0042
004910           MOVE WS-PROG-NM             TO RTN-PROG-NAME           CAM0042
004920           MOVE 'A300'                 TO RTN-PARA-NAME           CAM0042
004930           MOVE SQLCODE                TO RTN-SQLCODE             CAM0042
004940           MOVE 'BEN_OPTS'             TO RTN-TABLE-NAME          CAM0042
004950           MOVE SQ-SELECT-ERR          TO RTN-ERROR-CODE          CAM0042
004960           MOVE 1                      TO RTN-VAR-SEQ-NO (1)      FDM3874       
004970                                          RTN-VAR-DETL-CNT        FDM3874      
004980           MOVE SQLERRMC               TO RTN-VAR-TEXT (1)        FDM3874
004990           CALL 'CUTSVARB' USING RMESSAGE,                        CAM0042
005000                                 RTN-OUTPUT-FIELDS                CAM0042
005010     ELSE                                                         CAM0042
005020        MOVE +1 TO BEOP1-ROWS-FOUND                               CAM0042
005030        IF FDP-DEBUG > 0                                          CAM0042
005040           DISPLAY WS-PROG-NM ' ' BEOP1-ROWS-FOUND                CAM0042
005050                   ' BEN_OPTS DAL'                                CAM0042
005060        END-IF                                                    CAM0042
005070     END-IF.                                                      CAM0042
005080
005090 A300-EXIT.                                                       CAM0042
005100     EXIT.                                                        CAM0042
005110
005120 A400-SELECT-MBEN-LEVEL.                                          CAQ0164
005130
005140     MOVE BEOP1-BELA-KEY     TO HV-BELA-KEY.                      CAQ0164
005150     MOVE BEOP1-CASE-MBR-KEY TO HV-CASE-MBR-KEY.                  CAQ0164
005160     MOVE BEOP1-BNTP-KEY     TO HV-BNTP-KEY.                      CAQ0164
005170     MOVE BEOP1-BEN-OPT-DATA TO HV-BEN-OPT-DATA.                  CAQ0164
005180     MOVE BEOP1-EFF-DT       TO HV-EFF-DT.                        CAQ0164
005190
005200     EXEC SQL                                                     CAQ0164
005210        SELECT                                                    CAQ0164
005220            BEOP_KEY                                              CAQ0164
005230           ,RPAD(NVL(BEN_OPT_DATA,' '),300,' ')                   FDM3579
005240           ,TO_CHAR(EFF_DT)                                       CAQ0164
005250           ,RPAD(NVL(BEN_CAT_NO,' '),2,' ')                       FDM3874
005260           ,RPAD(NVL(TO_CHAR(XPIR_DT),' '),10,' ')                FDM3874
005270        INTO                                                      CAQ0164
005280            :BEOP1-BEOP-KEY                                       CAQ0164
005290           ,:BEOP1-BEN-OPT-DATA                                   CAQ0164
005300           ,:BEOP1-EFF-DT                                         CAQ0164
005310           ,:BEOP1-BEN-CAT-NO                                     CAQ0164
005320           ,:BEOP1-XPIR-DT                                        CAQ0164
005330       FROM BEN_OPTS                                              CAQ0164
005340         WHERE                                                    CAQ0164
005350             CASE_MBR_KEY  = :HV-CASE-MBR-KEY    AND              CAQ0164
005360             BNTP_KEY      = :HV-BNTP-KEY        AND              CAQ0164
005370             BELA_KEY      = :HV-BELA-KEY        AND              CAQ0164
005380             BEN_OPT_DATA  = :HV-BEN-OPT-DATA    AND              CAQ0164
005390             EFF_DT       <= TO_DATE(:HV-EFF-DT) AND              CAQ0164
005400            (XPIR_DT      >= TO_DATE(:HV-EFF-DT) OR               CAQ0164
005410             XPIR_DT       IS NULL)                               CAQ0164
005420     END-EXEC.                                                    CAQ0164
005430                                                                  CAQ0164
005440     MOVE SQLCODE TO SQL-ERROR-FLAG.                              CAQ0164
005450                                                                  CAQ0164
005460     IF SQL-ERROR                                                 CAQ0164
005470        IF SQ-NOT-FOUND                                           CAQ0164
005480           MOVE 0                      TO BEOP1-ROWS-FOUND        CAQ0164
005490        ELSE                                                      CAQ0164
005500           MOVE 'SQ'                   TO RTN-ERR-PREFIX          CAQ0164
005510           MOVE WS-PROG-NM             TO RTN-PROG-NAME           CAQ0164
005520           MOVE 'A400'                 TO RTN-PARA-NAME           CAQ0164
005530           MOVE SQLCODE                TO RTN-SQLCODE             CAQ0164
005540           MOVE 'BEN_OPTS'             TO RTN-TABLE-NAME          CAQ0164
005550           MOVE SQ-SELECT-ERR          TO RTN-ERROR-CODE          CAQ0164
005560           MOVE 1                      TO RTN-VAR-SEQ-NO (1)      FDM3874       
005570                                          RTN-VAR-DETL-CNT        FDM3874      
005580           MOVE SQLERRMC               TO RTN-VAR-TEXT (1)        FDM3874
005590           CALL 'CUTSVARB' USING RMESSAGE,                        CAQ0164
005600                                 RTN-OUTPUT-FIELDS                CAQ0164
005610     ELSE                                                         CAQ0164
005620        MOVE +1 TO BEOP1-ROWS-FOUND                               CAQ0164
005630        IF FDP-DEBUG > 0                                          CAQ0164
005640           DISPLAY WS-PROG-NM ' ' BEOP1-ROWS-FOUND                CAQ0164
005650                   ' BEN_OPTS DAL'                                CAQ0164
005660        END-IF                                                    CAQ0164
005670     END-IF.                                                      CAQ0164
005680                                                                  CAQ0164
005690 A400-EXIT.                                                       CAQ0164
005700     EXIT.                                                        CAQ0164
005710
005720
005730 A500-SELECT-DBEN-LEVEL.                                          MOW0135
005740
005750     MOVE BEOP1-BELA-KEY     TO HV-BELA-KEY.                      MOW0135
005760     MOVE BEOP1-CMRS-KEY     TO HV-CMRS-KEY.                      MOW0135
005770     MOVE BEOP1-BNTP-KEY     TO HV-BNTP-KEY.                      MOW0135
005780     MOVE BEOP1-BEN-OPT-DATA TO HV-BEN-OPT-DATA.                  MOW0135
005790     MOVE BEOP1-EFF-DT       TO HV-EFF-DT.                        MOW0135
005800
005810     EXEC SQL                                                     MOW0135
005820        SELECT                                                    MOW0135
005830            BEOP_KEY                                              MOW0135
005840           ,RPAD(NVL(BEN_OPT_DATA,' '),300,' ')                   FDM3579
005850           ,TO_CHAR(EFF_DT)                                       MOW0135
005860           ,RPAD(NVL(BEN_CAT_NO,' '),2,' ')                       FDM3874
005870           ,RPAD(NVL(TO_CHAR(XPIR_DT),' '),10,' ')                FDM3874
005880        INTO                                                      MOW0135
005890            :BEOP1-BEOP-KEY                                       MOW0135
005900           ,:BEOP1-BEN-OPT-DATA                                   MOW0135
005910           ,:BEOP1-EFF-DT                                         MOW0135
005920           ,:BEOP1-BEN-CAT-NO                                     MOW0135
005930           ,:BEOP1-XPIR-DT                                        MOW0135
005940       FROM BEN_OPTS                                              MOW0135
005950         WHERE                                                    MOW0135
005960             CMRS_KEY      = :HV-CMRS-KEY        AND              MOW0135
005970             CASE_MBR_KEY  IS NULL               AND              MOW0135
005980             BNTP_KEY      = :HV-BNTP-KEY        AND              MOW0135
005990             BELA_KEY      = :HV-BELA-KEY        AND              MOW0135
006000             BEN_OPT_DATA  = :HV-BEN-OPT-DATA    AND              MOW0135
006010             EFF_DT       <= TO_DATE(:HV-EFF-DT) AND              MOW0135
006020            (XPIR_DT      >= TO_DATE(:HV-EFF-DT) OR               MOW0135
006030             XPIR_DT       IS NULL)                               MOW0135
006040     END-EXEC.                                                    CAQ0164
006050                                                                  MOW0135
006060     MOVE SQLCODE TO SQL-ERROR-FLAG.                              MOW0135
006070                                                                  MOW0135
006080     IF SQL-ERROR                                                 MOW0135
006090        IF SQ-NOT-FOUND                                           MOW0135
006100           MOVE 0                      TO BEOP1-ROWS-FOUND        MOW0135
006110        ELSE                                                      MOW0135
006120           MOVE 'SQ'                   TO RTN-ERR-PREFIX          MOW0135
006130           MOVE WS-PROG-NM             TO RTN-PROG-NAME           MOW0135
006140           MOVE 'A500'                 TO RTN-PARA-NAME           MOW0135
006150           MOVE SQLCODE                TO RTN-SQLCODE             MOW0135
006160           MOVE 'BEN_OPTS'             TO RTN-TABLE-NAME          MOW0135
006170           MOVE SQ-SELECT-ERR          TO RTN-ERROR-CODE          MOW0135
006180           MOVE 1                      TO RTN-VAR-SEQ-NO (1)      FDM3874       
006190                                          RTN-VAR-DETL-CNT        FDM3874      
006200           MOVE SQLERRMC               TO RTN-VAR-TEXT (1)        FDM3874
006210           CALL 'CUTSVARB' USING RMESSAGE,                        MOW0135
006220                                 RTN-OUTPUT-FIELDS                MOW0135
006230     ELSE                                                         MOW0135
006240        MOVE +1 TO BEOP1-ROWS-FOUND                               MOW0135
006250        IF FDP-DEBUG > 0                                          MOW0135
006260           DISPLAY WS-PROG-NM ' ' BEOP1-ROWS-FOUND                MOW0135
006270                   ' BEN_OPTS DAL'                                MOW0135
006280        END-IF                                                    MOW0135
006290     END-IF.                                                      MOW0135
006300                                                                  MOW0135
006310 A500-EXIT.                                                       MOW0135
006320     EXIT.                                                        MOW0135
006330
006340
006350 A600-SELECT-DPDT-LEVEL.                                          MOW0135
006360
006370     MOVE BEOP1-BELA-KEY     TO HV-BELA-KEY.                      MOW0135
006380     MOVE BEOP1-CMRS-KEY     TO HV-CMRS-KEY.                      MOW0135
006390     MOVE BEOP1-BEN-OPT-DATA TO HV-BEN-OPT-DATA.                  MOW0135
006400     MOVE BEOP1-EFF-DT       TO HV-EFF-DT.                        MOW0135
006410
006420     EXEC SQL                                                     MOW0135
006430        SELECT                                                    MOW0135
006440            BEOP_KEY                                              MOW0135
006450           ,RPAD(NVL(BEN_OPT_DATA,' '),300,' ')                   FDM3579
006460           ,TO_CHAR(EFF_DT)                                       MOW0135
006470           ,RPAD(NVL(BEN_CAT_NO,' '),2,' ')                       FDM3874
006480           ,RPAD(NVL(TO_CHAR(XPIR_DT),' '),10,' ')                FDM3874
006490        INTO                                                      MOW0135
006500            :BEOP1-BEOP-KEY                                       MOW0135
006510           ,:BEOP1-BEN-OPT-DATA                                   MOW0135
006520           ,:BEOP1-EFF-DT                                         MOW0135
006530           ,:BEOP1-BEN-CAT-NO                                     MOW0135
006540           ,:BEOP1-XPIR-DT                                        MOW0135
006550       FROM BEN_OPTS                                              MOW0135
006560         WHERE                                                    MOW0135
006570             CMRS_KEY      = :HV-CMRS-KEY        AND              MOW0135
006580             CASE_MBR_KEY  IS NULL               AND              MOW0135
006590             BNTP_KEY      IS NULL               AND              MOW0135
006600             BELA_KEY      = :HV-BELA-KEY        AND              MOW0135
006610             BEN_OPT_DATA  = :HV-BEN-OPT-DATA    AND              MOW0135
006620             EFF_DT       <= TO_DATE(:HV-EFF-DT) AND              MOW0135
006630            (XPIR_DT      >= TO_DATE(:HV-EFF-DT) OR               MOW0135
006640             XPIR_DT       IS NULL)                               MOW0135
006650     END-EXEC.                                                    CAQ0164
006660                                                                  MOW0135
006670     MOVE SQLCODE TO SQL-ERROR-FLAG.                              MOW0135
006680                                                                  MOW0135
006690     IF SQL-ERROR                                                 MOW0135
006700        IF SQ-NOT-FOUND                                           MOW0135
006710           MOVE 0                      TO BEOP1-ROWS-FOUND        MOW0135
006720        ELSE                                                      MOW0135
006730           MOVE 'SQ'                   TO RTN-ERR-PREFIX          MOW0135
006740           MOVE WS-PROG-NM             TO RTN-PROG-NAME           MOW0135
006750           MOVE 'A600'                 TO RTN-PARA-NAME           MOW0135
006760           MOVE SQLCODE                TO RTN-SQLCODE             MOW0135
006770           MOVE 'BEN_OPTS'             TO RTN-TABLE-NAME          MOW0135
006780           MOVE SQ-SELECT-ERR          TO RTN-ERROR-CODE          MOW0135
006790           MOVE 1                      TO RTN-VAR-SEQ-NO (1)      FDM3874       
006800                                          RTN-VAR-DETL-CNT        FDM3874      
006810           MOVE SQLERRMC               TO RTN-VAR-TEXT (1)        FDM3874
006820           CALL 'CUTSVARB' USING RMESSAGE,                        MOW0135
006830                                 RTN-OUTPUT-FIELDS                MOW0135
006840     ELSE                                                         MOW0135
006850        MOVE +1 TO BEOP1-ROWS-FOUND                               MOW0135
006860        IF FDP-DEBUG > 0                                          MOW0135
006870           DISPLAY WS-PROG-NM ' ' BEOP1-ROWS-FOUND                MOW0135
006880                   ' BEN_OPTS DAL'                                MOW0135
006890        END-IF                                                    MOW0135
006900     END-IF.                                                      MOW0135
006910                                                                  MOW0135
006920 A600-EXIT.                                                       MOW0135
006930     EXIT.                                                        MOW0135
006940
006950 A700-SELECT-WITH-BGRP-KEY.                                       FDM3874
006960                                                                  FDM3874
006970     MOVE BEOP1-BGRP-KEY               TO HV-BGRP-KEY.            FDM3874
006980     MOVE BEOP1-BELA-KEY               TO HV-BELA-KEY.            FDM3874
006990     MOVE BEOP1-BEN-OPT-DATA           TO HV-BEN-OPT-DATA.        FDM3874
007000     MOVE BEOP1-EFF-DT                 TO HV-EFF-DT.              FDM3874
007010                                                                  FDM3874
007020     EXEC SQL                                                     FDM3874
007030        SELECT                                                    FDM3874
007040            BEOP_KEY                                              FDM3874
007050           ,RPAD(NVL(BEN_OPT_DATA,' '),300,' ')                   FDM3874
007060           ,TO_CHAR(EFF_DT)                                       FDM3874
007070           ,RPAD(NVL(BEN_CAT_NO,' '),2,' ')                       FDM3874
007080           ,RPAD(NVL(TO_CHAR(XPIR_DT),' '),10,' ')                FDM3874
007090        INTO                                                      FDM3874
007100            :BEOP1-BEOP-KEY                                       FDM3874
007110           ,:BEOP1-BEN-OPT-DATA                                   FDM3874
007120           ,:BEOP1-EFF-DT                                         FDM3874
007130           ,:BEOP1-BEN-CAT-NO                                     FDM3874
007140           ,:BEOP1-XPIR-DT                                        FDM3874
007150       FROM BEN_OPTS                                              FDM3874
007160         WHERE                                                    FDM3874
007170             BGRP_KEY      = :HV-BGRP-KEY AND                     FDM3874
007180             CASE_KEY      IS NULL        AND                     FDM3874
007190             BELA_KEY      = :HV-BELA-KEY AND                     FDM3874
007200             BNTP_KEY      IS NULL AND                            FDM3874
007210             CASE_MBR_KEY  IS NULL AND                            FDM3874
007220             BEN_OPT_DATA  = :HV-BEN-OPT-DATA     AND             FDM3874
007230             EFF_DT        <= TO_DATE(:HV-EFF-DT) AND             FDM3874
007240            (XPIR_DT       >= TO_DATE(:HV-EFF-DT) OR              FDM3874
007250             XPIR_DT       IS NULL)                               FDM3874
007260     END-EXEC.                                                    FDM3874
007270                                                                  FDM3874
007280     MOVE SQLCODE TO SQL-ERROR-FLAG.                              FDM3874
007290                                                                  FDM3874
007300     IF SQL-ERROR                                                 FDM3874
007310        IF SQ-NOT-FOUND                                           FDM3874
007320           MOVE 0                      TO BEOP1-ROWS-FOUND        FDM3874
007330        ELSE                                                      FDM3874
007340           MOVE 'SQ'                   TO RTN-ERR-PREFIX          FDM3874
007350           MOVE WS-PROG-NM             TO RTN-PROG-NAME           FDM3874
007360           MOVE 'A700'                 TO RTN-PARA-NAME           FDM3874
007370           MOVE SQLCODE                TO RTN-SQLCODE             FDM3874
007380           MOVE 'BEN_OPTS'             TO RTN-TABLE-NAME          FDM3874
007390           MOVE SQ-SELECT-ERR          TO RTN-ERROR-CODE          FDM3874
007400           MOVE 1                      TO RTN-VAR-SEQ-NO (1)      FDM3874       
007410                                          RTN-VAR-DETL-CNT        FDM3874      
007420           MOVE SQLERRMC               TO RTN-VAR-TEXT (1)        FDM3874
007430           CALL 'CUTSVARB'          USING RMESSAGE,               FDM3874
007440                                          RTN-OUTPUT-FIELDS       FDM3874
007450     ELSE                                                         FDM3874
007460        MOVE +1 TO BEOP1-ROWS-FOUND                               FDM3874
007470        IF FDP-DEBUG > 0                                          FDM3874
007480           DISPLAY WS-PROG-NM ' ' BEOP1-ROWS-FOUND                FDM3874
007490                   ' BEN_OPTS DAL'                                FDM3874
007500        END-IF                                                    FDM3874
007510     END-IF.                                                      FDM3874
007520                                                                  FDM3874
007530 A700-EXIT.                                                       FDM3874
007540     EXIT.                                                        FDM3874
007550
007560 A800-SELECT-TRNCH-LEVEL.                                         CTM0005
007570
007580     MOVE BEOP1-BELA-KEY     TO HV-BELA-KEY.                      CTM0005
007590     MOVE BEOP1-TRNCH-KEY    TO HV-TRNCH-KEY.                     CTM0005
007600     MOVE BEOP1-BEN-OPT-DATA TO HV-BEN-OPT-DATA.                  CTM0005
007610     MOVE BEOP1-EFF-DT       TO HV-EFF-DT.                        CTM0005
007620                                                                  CTM0005
007630     EXEC SQL                                                     CTM0005
007640        SELECT                                                    CTM0005
007650            BEOP_KEY                                              CTM0005
007660           ,RPAD(NVL(BEN_OPT_DATA,' '),300,' ')                   CTM0005
007670           ,TO_CHAR(EFF_DT)                                       CTM0005
007680           ,RPAD(NVL(BEN_CAT_NO,' '),2,' ')                       CTM0005
007690           ,RPAD(NVL(TO_CHAR(XPIR_DT),' '),10,' ')                CTM0005
007700        INTO                                                      CTM0005
007710            :BEOP1-BEOP-KEY                                       CTM0005
007720           ,:BEOP1-BEN-OPT-DATA                                   CTM0005
007730           ,:BEOP1-EFF-DT                                         CTM0005
007740           ,:BEOP1-BEN-CAT-NO                                     CTM0005
007750           ,:BEOP1-XPIR-DT                                        CTM0005
007760       FROM BEN_OPTS                                              CTM0005
007770         WHERE                                                    CTM0005
007780             TRNCH_KEY     = :HV-TRNCH-KEY       AND              CTM0005
007790             BELA_KEY      = :HV-BELA-KEY        AND              CTM0005
007800             BEN_OPT_DATA  = :HV-BEN-OPT-DATA    AND              CTM0005
007810             EFF_DT       <= TO_DATE(:HV-EFF-DT) AND              CTM0005
007820            (XPIR_DT      >= TO_DATE(:HV-EFF-DT) OR               CTM0005
007830             XPIR_DT       IS NULL)                               CTM0005
007840     END-EXEC.                                                    CTM0005
007850                                                                  CTM0005
007860     MOVE SQLCODE TO SQL-ERROR-FLAG.                              CTM0005
007870                                                                  CTM0005
007880     IF SQL-ERROR                                                 CTM0005
007890        IF SQ-NOT-FOUND                                           CTM0005
007900           MOVE 0                      TO BEOP1-ROWS-FOUND        CTM0005
007910        ELSE                                                      CTM0005
007920           MOVE 'SQ'                   TO RTN-ERR-PREFIX          CTM0005
007930           MOVE WS-PROG-NM             TO RTN-PROG-NAME           CTM0005
007940           MOVE 'A800'                 TO RTN-PARA-NAME           CTM0005
007950           MOVE SQLCODE                TO RTN-SQLCODE             CTM0005
007960           MOVE 'BEN_OPTS'             TO RTN-TABLE-NAME          CTM0005
007970           MOVE SQ-SELECT-ERR          TO RTN-ERROR-CODE          CTM0005
007980           MOVE 1                      TO RTN-VAR-SEQ-NO (1)      CTM0005       
007990                                          RTN-VAR-DETL-CNT        CTM0005      
008000           MOVE SQLERRMC               TO RTN-VAR-TEXT (1)        CTM0005
008010           CALL 'CUTSVARB' USING RMESSAGE,                        CTM0005
008020                                 RTN-OUTPUT-FIELDS                CTM0005
008030     ELSE                                                         CTM0005
008040        MOVE +1 TO BEOP1-ROWS-FOUND                               CTM0005
008050        IF FDP-DEBUG > 0                                          CTM0005
008060           DISPLAY WS-PROG-NM ' ' BEOP1-ROWS-FOUND                CTM0005
008070                   ' BEN_OPTS DAL'                                CTM0005
008080        END-IF                                                    CTM0005
008090     END-IF.                                                      CTM0005
008100                                                                  CTM0005
008110 A800-EXIT.                                                       CTM0005
008120     EXIT.                                                        CTM0005
008130

