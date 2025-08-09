000010*Property of FIS©. or its affiliates
000020*all rights reserved. FIS Confidential
000030 IDENTIFICATION DIVISION.
000040 PROGRAM-ID.           CS1BGRPD.
000050 AUTHOR.               MICHELLE LOUWRENS.
000060 DATE-WRITTEN.         15 JAN 1999.
000070****************************************************************
000080*   $Modtime::   10 Oct 2018 17:07:52  $
000090*   $Log::   P:/pvcsproj/DONOTUSE.prj/source/CS1BGRPD.COV  $
000100*
000110*   Rev 1.0.1.13   10 Oct 2018 17:33:02   alex
000120*FDM4167
000130*fis copyright
000140*
000150*   Rev 1.0.1.12   07 Oct 2011 04:35:50   maheshs
000160*PFM0939
000170*ADD HOLD-REAS-CD, PRE-LAPSE-HOLD-CD.
000180*
000190*   Rev 1.0.1.11   07 Nov 2006 10:17:18   tom
000200*FDM3579
000210*Fix for NVL problem when invoked from external procedure
000220*
000230*   Rev 1.0.1.10   23 Sep 2005 12:22:18   robin
000240*FDM3514
000250*Module Impact Analysis Tool - WS-REVISION awk script fix.
000260*
000270*   Rev 1.0.1.9   21 Sep 2005 14:07:38   rieko
000280*FDM3514
000290*Fix the ERF tag pushed off Columns 73-79.
000300*Fixed PVCS key word. 
000310*
000320*   Rev 1.0.1.8   19 Sep 2005 14:25:26   robin
000330*FDM3514
000340*Module Impact Analysis Tool
000350*
000360*   Rev 1.0.1.7   28 Mar 2005 15:46:48   sueh
000370*MLM5821
000380*Issue when underwriting is processing for a risk calc
000390*
000400*   Rev 1.0.1.6   07 Aug 2003 14:28:52   leena
000410*PRM0152
000420*Removed references to CASE_MBR_KEY
000430*for d/b changes
000440*
000450*   Rev 1.0.1.5   20 Dec 2002 16:58:44   rieko
000460*FDM3175
000470*COBOL modifications to support the Oracle 8 pre-complier.
000480*
000490*   Rev 1.0.1.4   29 Jun 2001 15:07:44   francesm
000500*FTM0019
000510*Removed PYRL-FREQ-CD
000520*
000530*   Rev 1.0.1.3   31 Mar 2001 09:59:24   gene
000540*LRM0008
000550*Member direct billing
000560*
000570*   Rev 1.0.1.2   Mar 24 2000 19:36:50   miker
000580*FDM1959
000590*Expanded Keys project
000600*
000610*   Rev 1.0.1.1   21 Jan 1999 03:41:06   michelle
000620*FDM0352
000630*   DATA ACCESS LAYER
000640*     - BILL_GRP
000650****************************************************************
000660* THIS PROGRAM WILL READ THE RECORDS FOR NUMBER_COUNTERS
000670*
000680*  INPUT PARMS:  BGRP1-BGRP-KEY
000690*
000700*****************************************************************
000710**              C H A N G E S   L O G                          **
000720**-------------------------------------------------------------**
000730**  PGMMER :   DATE   : PROJECT : DESCRIPTION                  **
000740**-------------------------------------------------------------**
000750*   GDP      01/30/01   LRM0008   ADD TYP_CD, BRING UP TO STAND**
000760*   FRM      05/01/01   FTM0019   REMOVE PYRL-FREQ-CD          **
000770*   LJD      08/07/03   PRM0152   REMOVE CASE-MBR-KEY REFERENCES  PRM0152
000780*                                 FOR D/B CHANGE                  PRM0152
000790*   SAH      03/25/05   MLM5821   UPDATE NVL FOR NUMERIC FIELD TO
000800*                                 USE 0 VS. '0'
000810*   MCS     26-SEP-11   PFM0939   ADD HOLD-REAS-CD,               
000820*                                 PRE-LAPSE-HOLD-CD.              
000830*****************************************************************
000840
000850 ENVIRONMENT DIVISION.
000860 CONFIGURATION SECTION.
000870*
000880 DATA DIVISION.
000890 WORKING-STORAGE SECTION.
000900
000910 01  WS-LOCAL-VARS.                                               LRM0008
000920     05 WS-PROG-NAME              PIC X(08) VALUE 'CS1BGRPD'.     LRM0008
000930
000940     EXEC SQL BEGIN DECLARE SECTION   END-EXEC.
000950
000960 01  HV-MISC-DATA.
000970*    05  HV-BGRP-KEY               PIC S9(08) COMP-3.             FDM1959
000980     05  HV-BGRP-KEY               PIC S9(15) COMP-3.             FDM1959
000990
001000     COPY SQLCA.
001010**
001020**   OTHER WORKING STORAGE COPYBOOKS
001030**
001040     COPY CWSUTSQL.
001050
001060*********************************************************
001070**   P R O C E D U R E    D I V I S I O N              **
001080*********************************************************
001090
001100     EXEC SQL END DECLARE SECTION END-EXEC.                       FDM3175
001110**  REVISION AREA **                                              FDM3514
001120 01  WS-REVISION           PIC X(35)  VALUE                       FDM3514
001130     ' $Revision::   1.0.1.13 $'.                                 
001140 LINKAGE SECTION.
001150     EXEC SQL BEGIN DECLARE SECTION END-EXEC.                     FDM3175
001160     COPY CLKFDPNC.
001170     EXEC SQL INCLUDE CL1BGRPD.COB END-EXEC.                      FDM3175
001180     EXEC SQL END DECLARE SECTION     END-EXEC.                   LRM0008
001190     COPY CLKERRBC.
001200
001210 PROCEDURE DIVISION USING RMESSAGE,
001220                          BGRP1-DATA,
001230                          RTN-OUTPUT-FIELDS.
001240
001250     IF FDP-DEBUG-RUNSTAT > '00'                                  FDM3514
001260        ADD +1  TO RTN-DIMT-CNT                                   FDM3514
001270        MOVE WHEN-COMPILED TO                                     FDM3514
001280             RTN-DIMT-COMPILE-DATE (RTN-DIMT-CNT)                 FDM3514
001290        MOVE WS-REVISION (16:10) TO                               FDM3514
001300             RTN-DIMT-REVISION (RTN-DIMT-CNT)                     FDM3514
001310        MOVE 'CS1BGRPD' TO                                        FDM3514
001320           RTN-DIMT-PROGRAM-NAME (RTN-DIMT-CNT)                   FDM3514
001330        CALL 'CUTDIMTD' USING RMESSAGE,                           FDM3514
001340                              RTN-OUTPUT-FIELDS                   FDM3514
001350     END-IF.                                                      FDM3514
001360
001370 A100-SELECT-DATA.
001380
001390
001400     MOVE BGRP1-BGRP-KEY          TO HV-BGRP-KEY.
001410
001420        EXEC SQL
001430           SELECT
001440               BGRP_KEY
001450              ,DESCRIPT
001460              ,NVL(OVRD_BGRP_KEY, 0)                              MLM5821
001470              ,RPAD(NVL(BEN_CAT_NO, ' '),2,' ')                   FDM3579/PFM0939
001480              ,NVL(CASE_KEY, 0)                                   MLM5821
001490              ,NVL(PROD_TYP_CD, 0)                                MLM5821
001500              ,NVL(PROTO_NO, 0)                                   MLM5821
001510              ,TYP_CD                                             LRM0008
001520              ,PRE_LAPSE_HOLD_CD                                  PFM0939
001530              ,HOLD_REAS_CD                                       PFM0939
001540           INTO
001550               :BGRP1-BGRP-KEY                                    LRM0008
001560              ,:BGRP1-DESCRIPT                                    LRM0008
001570              ,:BGRP1-OVRD-BGRP-KEY                               LRM0008
001580              ,:BGRP1-BEN-CAT-NO                                  LRM0008
001590              ,:BGRP1-CASE-KEY                                    LRM0008
001600              ,:BGRP1-PROD-TYP-CD                                 LRM0008
001610              ,:BGRP1-PROTO-NO                                    LRM0008
001620              ,:BGRP1-TYP-CD                                      LRM0008
001630              ,:BGRP1-PRE-LAPSE-HOLD-CD                           PFM0939
001640              ,:BGRP1-HOLD-REAS-CD                                PFM0939
001650            FROM BILL_GRP
001660            WHERE
001670              BGRP_KEY= :HV-BGRP-KEY
001680        END-EXEC
001690
001700     MOVE SQLCODE TO SQL-ERROR-FLAG.
001710
001720     IF SQL-ERROR
001730        MOVE 'SQ'                TO RTN-ERR-PREFIX
001740        MOVE 'A100'              TO RTN-PARA-NAME
001750        MOVE WS-PROG-NAME        TO RTN-PROG-NAME                 LRM0008
001760        MOVE 'BILL_GRP'          TO RTN-TABLE-NAME
001770        MOVE SQLCODE             TO RTN-SQLCODE
001780        MOVE SQ-SELECT-ERR       TO RTN-ERROR-CODE
001790        MOVE 1                   TO RTN-VAR-SEQ-NO (1)            PFM0939
001800                                    RTN-VAR-DETL-CNT              PFM0939
001810        MOVE SQLERRMC            TO RTN-VAR-TEXT (1)              PFM0939
001820        CALL 'CUTSVARB' USING RMESSAGE,                          
001830                              RTN-OUTPUT-FIELDS
001840     ELSE
001850        MOVE 1                   TO BGRP1-ROWS-FOUND
001860     END-IF.
001870
001880     GOBACK.
001890
001900 A100-EXIT.
001910     EXIT.
001920
001930**
001940**   E N D     O F    P R O G R A M
001950**
001960
001970
