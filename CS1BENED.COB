000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.           CS1BENED.
000300 AUTHOR.               PAUL WIKSTROM.
000400 DATE-WRITTEN.         1996-12-18.
000500****************************************************************
000080*   $Modtime::   10 Oct 2018 17:07:52  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CS1BENED.COV  $
000100*
000110*   Rev 1.0.1.11   10 Oct 2018 17:33:00   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.10   13 Nov 2006 16:53:46   haripriya
000160*FDM3579
000170*CHANGED RPAD LENGTH
000180*
000190*   Rev 1.0.1.9   07 Nov 2006 10:17:04   tom
000200*FDM3579
000210*Fix for NVL problem when invoked from external procedure
000220*
000230*   Rev 1.0.1.8   23 Sep 2005 12:22:16   robin
000240*FDM3514
000250*Module Impact Analysis Tool - WS-REVISION awk script fix.
000260*
000270*   Rev 1.0.1.7   21 Sep 2005 14:07:36   rieko
000280*FDM3514
000290*Fix the ERF tag pushed off Columns 73-79.
000300*Fixed PVCS key word.
000310*
000320*   Rev 1.0.1.6   19 Sep 2005 14:25:24   robin
000330*FDM3514
000340*Module Impact Analysis Tool
000350*
000360*   Rev 1.0.1.5   28 Mar 2005 15:46:36   sueh
000370*MLM5821
000380*Issue when underwriting is processing for a risk calc
000390*
000400*   Rev 1.0.1.4   20 Dec 2002 16:58:22   rieko
000410*FDM3175
000420*COBOL modifications to support the Oracle 8 pre-complier.
000430*
000440*   Rev 1.0.1.3   15 Dec 2000 10:18:30   steven
000450*LRM0002
000460*NEW BENEFICIARY FIELDS
000470*
000480*   Rev 1.0.1.2   Mar 24 2000 19:36:46   miker
000490*FDM1959
000500*Expanded Keys project
*******
*******   Rev 1.0.1.1   10 Jul 1997 13:33:16   paulm
*******Mass Check In for Currency Field Changes
*******
*******   Rev 1.0.1.0   May 04 00:56:56 1997   paulm
*******No ERF
*******
*******Initial Check In for FDP Branch
*******3 May 1997
*******
*******   Rev 1.0   03 May 1997 14:41:38   paulm
*******No ERF
*******
*******Initial Check-In   3 May 1997
000600*   DATA ACCESS LAYER
000700*     - BENEFICIARIES
000800****************************************************************
000900* THIS PROGRAM WILL READ DATA AND STORE THE SELECTED VALUES
001000* IN LINKAGE.
001100*
001200*  REQUIRES :  BENE3-BENE-KEY
001300*
001400****************************************************************
001500*                     CHANGES LOG
001600****************************************************************
001700*
001800*   DATE     PRG   PROJ#      DESCRIPTION
001900*  --------  --- ---------  ----------------------------------------
002000*  3-14-97   AEC   FDP509    ADDED EFF_DT LOGIC.
000800* 11-17-00   SH   LRM0002    ADD NEW DB FIELDS.
000810*                            BRING PROG UP TO STANDARDS.
000820* 03-25-05   SAH  MLM5821    UPDATE NVL FOR NUMERIC FIELD TO
000830*                            USE 0 VS. '0'
000840* 11-13-06   HM   FDM3579    CHANGE RPAD LENGTH
002300****************************************************************
002400 ENVIRONMENT DIVISION.
002500 CONFIGURATION SECTION.
002600*
002700 DATA DIVISION.
002800 WORKING-STORAGE SECTION.
002900
003000     EXEC SQL BEGIN DECLARE SECTION   END-EXEC.
003100
003200
000950*
000960 01  WS-COMPILED                  PIC X(20).                      LRM0002
000970*
003300 01  HV-MISC-VARS.
000990*    05  HV-BENE-KEY               PIC S9(8) COMP-3.              FDM1959
001000     05  HV-BENE-KEY               PIC S9(15) COMP-3.             FDM1959
003410     05  HV-EFF-DT                 PIC X(10).
003500
006500
006600     COPY  SQLCA.
006700
006800**
006900**   OTHER WORKING STORAGE COPYBOOKS
007000**
007100     COPY CWSUTSQL.
007200
007300*********************************************************
007400**   P R O C E D U R E    D I V I S I O N              **
007500*********************************************************
007600
001150     EXEC SQL END DECLARE SECTION END-EXEC.                       FDM3175
001160**  REVISION AREA **                                              FDM3514
001170 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
001180     ' $Revision::   1.0.1.11 $'.
007700 LINKAGE SECTION.
001200     EXEC SQL BEGIN DECLARE SECTION END-EXEC.                     FDM3175
007800     COPY CLKFDPNC.
001220     EXEC SQL INCLUDE CL3BENED.COB END-EXEC.                      FDM3175
001230     EXEC SQL END DECLARE SECTION     END-EXEC.                   LRM0002
008000     COPY CLKERRBC.
008100
008200 PROCEDURE DIVISION USING RMESSAGE,
008300                          BENE3-DATA,
008400                          RTN-OUTPUT-FIELDS.
001290
001300     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514
001310        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
001320        MOVE WHEN-COMPILED TO                                     FDM3514
001330             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
001340        MOVE WS-REVISION (16:10) TO                               FDM3514
001350             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
001360        MOVE 'CS1BENED' TO                                        FDM3514
001370           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
001380        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
001390                              RTN-OUTPUT-FIELDS                   FDM3514
001400     END-IF.                                                      FDM3514
008500
008600 A100-SELECT-DATA.
008900
009000     MOVE BENE3-BENE-KEY TO HV-BENE-KEY.
009010     MOVE FDP-EFF-DT     TO HV-EFF-DT.
009100
009200     EXEC SQL
009300       SELECT
009400         BENE_KEY
009500        ,AGE_EVID_CD
009600        ,BENE_PCT
009700        ,BENE_SEQ_NO
009900        ,TO_CHAR(EFF_DT)
010000        ,MARR_EVID_CD
010100        ,QUAL_SP_CD
010200        ,RELT_CD
010210        ,NVL(CASE_MBR_KEY, 0)
001580        ,RPAD(NVL(FNCL_DPDT_CD, ' '),120,' ')                     FDM3579
010220        ,NVL(NMBD_KEY, 0)
001600        ,RPAD(NVL(TO_CHAR(XPIR_DT), ' '),120,' ')                 FDM3579
001610        ,TYP_CD                                                   LRM0002
001620        ,RPAD(NVL(TO_CHAR(TRUST_DT), ' '),120,' ')                FDM3579
001630        ,NVL(BNTP_KEY, 0)                                         LRM0002/MLM5821
001640        ,DIST_MODE_CD                                             LRM0002
001650        ,RPAD(NVL(SHORT_MSG, ' '),2000,' ')                       FDM3579
010400        INTO
001670         :BENE3-BENE-KEY
001680        ,:BENE3-AGE-EVID-CD
001690        ,:BENE3-BENE-PCT
001700        ,:BENE3-BENE-SEQ-NO
001710        ,:BENE3-EFF-DT
001720        ,:BENE3-MARR-EVID-CD
001730        ,:BENE3-QUAL-SP-CD
001740        ,:BENE3-RELT-CD
001750        ,:BENE3-CASE-MBR-KEY
001760        ,:BENE3-FNCL-DPDT-CD
001770        ,:BENE3-NMBD-KEY
001780        ,:BENE3-XPIR-DT
001790        ,:BENE3-TYP-CD                                            LRM0002
001800        ,:BENE3-TRUST-DT                                          LRM0002
001810        ,:BENE3-BNTP-KEY                                          LRM0002
001820        ,:BENE3-DIST-MODE-CD                                      LRM0002
001830        ,:BENE3-SHORT-MSG                                         LRM0002
011500       FROM BENEFICIARIES
011600       WHERE
011700           BENE_KEY = :HV-BENE-KEY           AND
011710           EFF_DT  <= TO_DATE(:HV-EFF-DT)    AND
011720            (XPIR_DT IS NULL OR
011730              XPIR_DT >= TO_DATE(:HV-EFF-DT))
011800     END-EXEC.
011900
012100     MOVE SQLCODE TO SQL-ERROR-FLAG.
012200
012400     IF SQL-ERROR
012500        IF SQ-NOT-FOUND
001960           MOVE ZEROES          TO BENE3-ROWS-FOUND
012800        ELSE
012900           MOVE 'CS1BENED'      TO RTN-PROG-NAME
013000           MOVE 'A100'          TO RTN-PARA-NAME
013100           MOVE 'BENEFICIARIES' TO RTN-TABLE-NAME
013200           MOVE 'SQ'            TO RTN-ERR-PREFIX
013300           MOVE SQLCODE         TO RTN-SQLCODE
013400           MOVE SQ-SELECT-ERR   TO RTN-ERROR-CODE
013500           CALL 'CUTSVARB' USING RMESSAGE,
013600                                 RTN-OUTPUT-FIELDS
013700        END-IF
013710     ELSE
002080        MOVE +1            TO BENE3-ROWS-FOUND
013740     END-IF.
013800
013900
014000**
014100**   E N D     O F    P R O G R A M
014200**
014300
014400     GOBACK.
014500
014600 A100-EXIT.
014700     EXIT.
002200
