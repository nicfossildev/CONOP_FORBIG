using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace CONOP.NET
{
    public class DOINPUT : IRunIt
    {
        #region Var.

        private int HEVENT, HTYPE, HSECTION, UNPRDF, LSTSECT, LSTEVNT;
        private int IEVENT, ITYPE, ISECTION, ILEVEL, IMOVES, IPRD, IUNPRD;
        private int JPLACE, JINC, Ityp, Ilbl, LSTTYPE, MFAD, MLAD;
        private int IPAIR;
        private int I, J, K, N, M, IROW, JOPT, NENDS, JTAG, ITAG, NTAG, NSCAL;
        private int A, L;
        private double EVENT, TYP, SECTION, VALUE, LEVEL, MOVES, WTUP, WTDN;
        private double MNSPACE, MUSPACE, MINC;
        private string JNAME, TAGSTRING;
        private string JNICK;
        private string INAME;
        private string INICK;
        private string[] DIGIT = new string[10];

        #endregion

        #region Singleton

        private static DOINPUT g_singleton = null;
        private DOINPUT() { }
        public static DOINPUT Singleton()
        {
            if (g_singleton == null)
                g_singleton = new DOINPUT();

            return g_singleton;
        }

        #endregion

        #region IRunIt 成员

        public void RunIt()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            //Initialization (IFORT 11.1 even more sensitive than 9.0)
            DIGIT[0] = "0";
            DIGIT[1] = "1";
            DIGIT[2] = "2";
            DIGIT[3] = "3";
            DIGIT[4] = "4";
            DIGIT[5] = "5";
            DIGIT[6] = "6";
            DIGIT[7] = "7";
            DIGIT[8] = "8";
            DIGIT[9] = "9";
            MINC = 0.00;
            A = 0;
            I = 0;
            L = 0;
            Ilbl = 0;
            Ityp = 0;
            UNPRDF = 0;
            NSCAL = 0;
            NTAG = 0;
            ITAG = 0;
            JTAG = 0;

            COMMOD9.IRCOUNT = 0;

            if (COMMOD9.CDF != 1)
            {
                //TODO:DOINPUT.CS
                //IF(CDF.ne.1) OPEN(11, FILE=RUNLOG, STATUS='OLD', POSITION='APPEND')
            }

            Helper.Write("          \n");
            Helper.Write("  {0}\n", COMMOD9.PROJNAME);
            if (COMMOD9.SOLVEF == 0 && !(COMMOD9.NUDGER))
            {
                Helper.Write("  Simulated annealing\n");
            }
            else if (COMMOD9.SOLVEF == 0 && COMMOD9.NUDGER)
            {
                Helper.Write("  Simulated annealing with nudges\n");
            }
            else if (COMMOD9.SOLVEF == 1)
            {
                Helper.Write("  Greedy Algorithm\n");
            }

            if (COMMOD9.PAUSF == 5 && COMMOD9.SOLVEF == 0) Helper.Write("     - with auto cooling\n");

            if (COMMOD9.PENF == -1)
            {
                Helper.Write("  Optimize on Eventual Misfit\n");
            }
            else if (COMMOD9.PENF == 0)
            {
                Helper.Write("  Optimize on Interval Misfit\n");
            }
            else if (COMMOD9.PENF == 1)
            {
                Helper.Write("  Optimize on Level Misfit\n");
            }
            else if (COMMOD9.PENF == 2)
            {
                Helper.Write("  Optimize on Ordinal Misfit\n");
            }
            else if (COMMOD9.PENF == 3)
            {
                Helper.Write("  Optimize on Spatial Misfit\n");
            }
            else if (COMMOD9.PENF == 4)
            {
                if (COMMOD9.RASCon)
                {
                    Helper.Write("  Optimize on Rascal Misfit with RASC() array\n");
                }
                else
                {
                    Helper.Write("  Optimize on Rascal Misfit without RASC() array\n");
                }

            }
            else if (COMMOD9.PENF == 5)
            {
                Helper.Write("  Optimize on Royal Misfit\n");
            }
            else if (COMMOD9.PENF == 6 && COMMOD9.FB4LF != 0)//PENF=6 should have forced Fb4LF=1 or 2
            {
                Helper.Write("  Optimize on Sequel Misfit\n");
            }

            if (COMMOD9.DFLTPEN)
            {
                //TODO:DOINPUT.CS
                //    WRITE(11,*)
                //1  'DEFAULT penalty! Entry in conop.cfg not valid!'
            }

            if (COMMOD9.COXSTF == 4)
            {
                Helper.Write("  No Coexistences Enforced\n");
            }
            else if (COMMOD9.COXSTF == 3)
            {
                Helper.Write("  Strict Coexistence Criteria Enforced\n");
            }
            else if (COMMOD9.COXSTF == 2)
            {
                Helper.Write("  Loose Coexistence Criteria Enforced\n");
            }
            else if (COMMOD9.COXSTF == 1)
            {
                Helper.Write("  Coexistences Read from File\n");
            }
            else if (COMMOD9.COXSTF == 0)
            {
                Helper.Write("  All taxa forced to co-exist\n");
            }

            if (COMMOD9.DFLTCOEX)
            {
                //TODO:DOINPUT.CS
                //WRITE(11,*)
                //1  'DEFAULT coex constraint! Entry in conop.cfg not valid!'
            }

            if (COMMOD9.CONTRACT == 0)
            {
                Helper.Write("  No Range Contractions Permitted\n");
            }
            else if (COMMOD9.CONTRACT == 3)
            {
                Helper.Write("  All Ranges Allowed to Contract and Shift\n");
            }
            else if (COMMOD9.CONTRACT == 2)
            {
                Helper.Write("  All Ranges Allowed to Contract, but not Shift\n");
            }
            else if (COMMOD9.CONTRACT == 1)
            {
                Helper.Write("  Ranges Contract or Shift only as Permitted by Input File\n");
            }

            Helper.Write("------------------------------------------------------\n");
            Helper.Write("  Reading data file (" +
                COMMOD9.INFILE.Trim() + ") and checking for anomalies . . .  \n");

            //-------------------------------------------------------------------
            //          look for snags in conop.cfg requests
            //  use of allocatable arrays makes it impossible
            //  to exceed compiled limits for numbers of sections and events
            //-------------------------------------------------------------------
            if (COMMOD9.JSTART >= COMMOD9.NSCT)//<=HXD
            {
                COMMOD9.STOPF = 1;
                //TODO:INPUT.CS
                //WRITE(*,16) JSTART
                //16    FORMAT(1X,'* Start requested from section out of range',
                //1   ' (Section',I5,')')
                goto Label9999;
            }

            //CPMS*******************************************************************
            //C      READ IN THE DATA AND GENERATE SOME MATRICES
            //C      THE DATA ELEMENTS ARE:
            //C         EVENT, TYP, SECTION, VALUE, LEVEL, MOVES, WTUP, AND WTDN
            //C----------------------------------------------------------------------
            //C      build:
            //C         IROWS -- 1 row for each event number/type combination
            //C            col 1 is event number
            //C            col 2 is event type   
            //CPMS         col 3 is consecutive numbering used in for unpaired and
            //CPMS               paired events in coex and prtld matrices
            //CPMS         col 4 is the paired event for FADs and LADs (and MAXs and MINs)
            //CPMS         col 5 is the label number
            //CPMS         col 6 is the tag number
            //C             IRCOUNT is the row number when done, it gives the number of
            //C                     events.
            //C                 it only increments when a new event or type is 
            //C                     encountered             
            //C
            //C             to check, HEVENT and HTYPE hold the values from the 
            //C                 last record read
            //C                                    
            //CPMS     SECTNAME -- list of section names in order of section numbers
            //CPMS
            //CPMS     EVNTNAME -- list of event names in order of event numbers
            //CPMS
            //C        ISTATIC -- rows are same as IROWS
            //C                     second index is section
            //C                     third index is 1=level
            //C                                    2=moves
            //C                         level is level # from bottom of section 
            //C                             for level type being used in this analysis
            //C                             level is also used to identify certain
            //C                                 things about events in this section
            //C                                     0 - not recovered in section
            //C                                    -1 - extends down out of section  NOT USED
            //C                                    -2 - extends up out of section    NOT USED
            //C
            //C                         moves is code for allowable moves of this 
            //C                             event in this section
            //C                                     0 - no moves
            //C                                     1 - below datum only
            //C                                     2 - above datum only
            //C                                     3 - above or below datum
            //C
            //C         RSTATIC -- rows are same as IROWS
            //C                     second index is section
            //C                     third index is 1= weight for move up from datum
            //C                                    2= weight for move down from datum
            //C
            //C         VALEVEL -- row # is level#, corresponding values are
            //C                     listed for each section 
            //C                         VALEVEL(LEVEL, SECTION) = VALUE
            //c                     if we want to include other measures, 
            //C                         we'll add another dimension 
            //CPMS      IPRD -- consecutive number for paired events
            //CPMS    IUNPRD -- consecutive number for unpaired events
            //C-----------------------------------------------------------------------

            //  zero integers       
            LSTSECT = 0;
            LSTEVNT = 0;
            LSTTYPE = 0;
            IEVENT = 0;
            ISECTION = 0;
            NENDS = 0;
            COMMOD9.NROWS = 0;
            COMMOD9.NOBS = 0;
            COMMOD9.NMIDS = 0;
            COMMOD9.NBOXS = 0;
            IPRD = 1;//<=HXD IPRD = 0
            IUNPRD = 0;
            //	zero integer matrices
            Helper.SetVal(COMMOD9.HLEVEL, -1);//<=HXD
            //Helper.SetVal(COMMOD9.ELEVEL, 0);//<=HXD
            //  zero real matrices
            Helper.SetVal(COMMOD9.VALEVEL, 0.0);

            Helper.SetVal(COMMOD9.ISTATIC, -1);//<=HXD
            //Helper.SetVal(COMMOD9.IROWS, -1);//<=HXD
            
          

            //--FIRST PASS THROUGH INPUT FILE
            //--Check input file for errors            
            bool hasFile = File.Exists(COMMOD9.INFILE);
            if (!hasFile)
            {
                Helper.Write(" Unable to find file {0}\n", COMMOD9.INFILE);
                Helper.Write("  check .CFG file and file manager");
                COMMOD9.STOPF = 1;
                goto Label9999;
            }

            //TODO:DOINPUT.CS
            TextReader reader = new StreamReader(File.Open(COMMOD9.INFILE, FileMode.Open));

            //Label1:
            //COMMOD9.NROWS = COMMOD9.NROWS + 1;

           
            //READ(2,*,END=2) EVENT, TYP, SECTION, VALUE, LEVEL,
            //1                  MOVES, WTUP, WTDN
            string dataLine;
            while ((dataLine = reader.ReadLine()) != null)
            {
                string[] dataRowString = dataLine.Split(new string[] { " " }, StringSplitOptions.RemoveEmptyEntries);
                EVENT = double.Parse(dataRowString[0].ToString());
                TYP= double.Parse(dataRowString[1].ToString());
                SECTION= double.Parse(dataRowString[2].ToString());
                VALUE= double.Parse(dataRowString[3].ToString());
                LEVEL= double.Parse(dataRowString[4].ToString());
                MOVES= double.Parse(dataRowString[5].ToString());
                WTUP = double.Parse(dataRowString[6].ToString());
                WTDN = double.Parse(dataRowString[7].ToString());

                ILEVEL = (int)LEVEL;
                ISECTION = (int)SECTION;

                //fill the HLEVEL matrix to count levels by section
                COMMOD9.HLEVEL[ISECTION - 1] = Math.Max(COMMOD9.HLEVEL[ISECTION - 1], ILEVEL-1);

                //INFILE should be sorted so that event code numbers don't increase by more than 1
                if (((int)EVENT != IEVENT) && ((int)EVENT != IEVENT + 1))
                {
                    COMMOD9.STOPF = 1;

                    //TODO:DOINPUT.CS
                    //        WRITE(*,101)1+IEVENT
                    //101     FORMAT(1X,'* taxon/event ',I5,' missing or out of order')
                }

                // Mid events use the taxon number as event code so their count cannot be monitored
                // by new event code numbers; NMIDS must be incremented separately
                if ((int)TYP == 3)
                {
                    switch ((int)MOVES)
                    {
                        case 1:
                        case 2:
                            COMMOD9.STOPF = 1;
                            UNPRDF = 1;
                            //TODO:DOINPUT.CS
                            //            IF(INT(MOVES).EQ.1) WRITE(*,102)1+IEVENT
                            //            IF(INT(MOVES).EQ.2) WRITE(*,103)1+IEVENT
                            //102         FORMAT(1X,'* taxon ',I5,' entered as bottom-only')        
                            //103         FORMAT(1X,'* taxon ',I5,' entered as top-only')
                            break;
                        case 3:
                            if ((int)TYP != ITYPE) COMMOD9.NMIDS = COMMOD9.NMIDS + 1;
                            break;
                    }
                }

                if (COMMOD9.NMIDS > COMMOD9.NOTHR)
                {
                    COMMOD9.STOPF = 1;
                    Helper.Write("Number of MIDs exceeds EVENTS in config file!\n");
                }

                //       Boxed events use the same event number for both MAX and MIN limits of box
                //       so an extra event must be added for each boxed event
                //       count to NBOXS and then add to LSTEVNT like NMIDS
                //       count NBOXs using MAX only (type 11)
                //       and only once for each run of 11's
                if ((int)TYP == 11)
                {
                    if ((int)TYP != ITYPE) COMMOD9.NBOXS = COMMOD9.NBOXS + 1;
                }

                if ((COMMOD9.NBOXS * 2) > COMMOD9.NOTHR)
                {
                    COMMOD9.STOPF = 1;
                    Helper.Write("Number of MAX & MIN events exceeds EVENTS in config file!\n");
                }

                if ((int)LEVEL <= 0)
                {
                    COMMOD9.STOPF = 1;
                    //TODO:DOINPUT.CS
                    //       WRITE(*,105) INT(LEVEL), INT(SECTION)
                    //105	     FORMAT(1X,'* ',I5,'!!  - level below 1 in section ',I5)
                    Helper.Write("        revise column 5 in file \n");
                }

                if ((int)LEVEL > COMMOD9.MXLVL)
                {
                    COMMOD9.STOPF = 1;
                    //TODO:DOINPUT.CS
                    //	     WRITE(*,104)  MXLVL, INT(SECTION)
                    //104	     FORMAT(1X,'* ',I5,'-level limit exceded in section ',I5)
                    Helper.Write("    increase limit in configuration file!\n");
                }


                IEVENT = (int)EVENT;
                ITYPE = (int)TYP;
                LSTSECT = Math.Max(LSTSECT, (int)SECTION);
                LSTEVNT = Math.Max(LSTEVNT, (int)EVENT);

                //goto Label1;
            
            }
            reader.Close();
            


            if (COMMOD9.STOPF == 1) goto Label9999;

            //   make sure that the largest section and event numbers match 
            //   request from CONOP3.CFG
            //-------------------------------------------------------------------  
            if (LSTEVNT != COMMOD9.NEVNT - COMMOD9.NSPC - COMMOD9.NMIDS - COMMOD9.NBOXS)
            {
                COMMOD9.STOPF = 1;
                //TODO:DOINPUT.CS
                //      WRITE(*,811)NEVNT-NSPC-NMIDS-NBOXS,LSTEVNT
                //811   FORMAT(1X,'* ',I5,' taxa+events expected, ',
                //   1          'but taxon+event numbers reach', I5)  
            }

            if (LSTSECT != COMMOD9.NSCT)
            {
                COMMOD9.STOPF = 1;
                //TODO:DOINPUT.CS
                //      WRITE(*,812)NSCT,LSTSECT
                //812   FORMAT(1X,'* ',I5,' sections expected,',
                //   1          'but section numbers reach', I5)  
            }

            if (COMMOD9.STOPF == 1) goto Label9999;


            //  reset NROWS for array-loading loops
            COMMOD9.NROWS = 0;
            //C********************************************************
            //CPMS***LOOP THROUGH THE INPUT FILE TWICE MORE************
            //C      First to extract unpaired events (NEVNT-(NSPC*2))
            //C	   and to standardize base levels at 1000
            //C      Second to extract paired events  (NSPC)
            //C        this ensures that the (NEVNT-(NSPC*2))unpaired 
            //C        events appear at the top of the IROWS array
            //c
            //c      Boxed MAX and MIN events will appear at the top
            //c      among the unpaired events!  
            //c      This might not matter; they are not subject to 
            //c      coxistence rules, and therefore not in the 
            //c      COEX() matrix.
            //C--------------------------------------------------------
            Helper.Write("  Counting levels in each section . . . \n");

            //CPMS********************
            //CPMS***FIRST LOOP***
            //CPMS*************

            //TODO:DOINPUT.CS
            //      REWIND 2
            //  reset event counter
            IEVENT = 0;

        //C     format statement numbers begin with 8000
        //C     the following is a big loop that executes until the EOF marker is
        //C     hit (END=3). The program then jumps to statement #4, the first
        //C         statement after "go to 3", the bottom statement in the loop.
        //C     it is like a "while not eof do" with the endwhile just above 4
        //C         in this case, the go to stmt.
        //C-----------------------------------------------------------------------
        //C
        //CPMS  NROWS counts the number of rows in the input file
        //C     IRCOUNT counts the number of events entered into IROWS
        //C     NROWS>IRCOUNT because the same event can occur in many sections
        //C     NROWS may not be needed!! may be same as NENDS!!
        //C     NENDS is local;  NOBS is the global equivalent

             reader= new StreamReader(File.Open(COMMOD9.INFILE, FileMode.Open));
        Label3:
            COMMOD9.NROWS = COMMOD9.NROWS + 1;

            //----------------------------------------------------------------------
            //       read in data, including allowed moves for each event and
            //       weights for placing it above or below its datum         
            //----------------------------------------------------------------------

            //      READ(2, * ,END=4) EVENT, TYP, SECTION, VALUE, LEVEL,
            //1                    MOVES, WTUP, WTDN           
            dataLine=null;
            while ((dataLine = reader.ReadLine()) != null)
            {
                string[] dataRowString = dataLine.Split(new string[] { " " }, StringSplitOptions.RemoveEmptyEntries);
                EVENT = double.Parse(dataRowString[0].ToString());
                TYP = double.Parse(dataRowString[1].ToString());
                SECTION = double.Parse(dataRowString[2].ToString());
                VALUE = double.Parse(dataRowString[3].ToString());
                LEVEL = double.Parse(dataRowString[4].ToString());
                MOVES = double.Parse(dataRowString[5].ToString());
                WTUP = double.Parse(dataRowString[6].ToString());
                WTDN = double.Parse(dataRowString[7].ToString());
                //-------convert reals to integer indices
                IEVENT = (int)EVENT;
                ITYPE = (int)TYP;
                ISECTION = (int)SECTION;
                ILEVEL = (int)LEVEL;


                //----fill up the VALEVEL matrix
                if (ILEVEL > 0)
                {
                    if (COMMOD9.VALEVEL[ILEVEL - 1, ISECTION - 1] == 0.0)
                    {
                        COMMOD9.VALEVEL[ILEVEL - 1, ISECTION - 1] = VALUE;
                    }
                    else if(COMMOD9.VALEVEL[ILEVEL-1,ISECTION-1]!=VALUE)
                    {
                        COMMOD9.STOPF = 1;
                        //TODO:DOINPUT.CS
                        //           WRITE(*,800) ILEVEL,ISECTION,VALEVEL(ILEVEL,ISECTION),
                        //   1                    VALUE          
                        //800        FORMAT(1X,'* level ',I4,' in section ',I4,
                        //   1       ' entered as ',F11.2,' and ',F11.2) 
                    }
                }

                //----fill up the ELEVEL matrix
                COMMOD9.ELEVEL[ILEVEL - 1, ISECTION - 1] = COMMOD9.ELEVEL[ILEVEL - 1, ISECTION - 1] + 1;
                if (ITYPE <= 3)
                {
                    COMMOD9.ELEVEL[COMMOD9.MXLVL, ISECTION - 1] = COMMOD9.ELEVEL[COMMOD9.MXLVL, ISECTION - 1] + 1;
                }
                else if (ITYPE > 3)
                {
                    COMMOD9.ELEVEL[COMMOD9.MXLVL + 1, ISECTION - 1] = COMMOD9.ELEVEL[COMMOD9.MXLVL + 1, ISECTION - 1] + 1;
                }

                //C-------------------------------------------------
                //CPMS--  load other arrays only for unpaired events
                //CPMS--  .GT.2 because 3(MID) is an unpaired event         
                //CPMS--      i.e. events that share codes and have limits to movement
                //CPMS--           like MID, MAX, and MIN, are not necessarily "paired"
                //
                //CPMS--  THIS LOOP DOES NOT OPERATE ON FADs and LADs
                if (ITYPE > 2 || ITYPE < 1)
                {
                    IMOVES = (int)MOVES;

                    //check for illegal combinations
                    if (((ITYPE > 3) && (ITYPE < 10) && (IMOVES != 0)) ||
                    ((ITYPE == 11) && (IMOVES != 2)) ||
                    ((ITYPE == 12) && (IMOVES != 1)) ||
                    ((ITYPE == 3) && (IMOVES != 3)) ||
                    ((ITYPE == -1) && (IMOVES != 1)) ||
                    ((ITYPE == -2) && (IMOVES != 2)))
                    {
                        COMMOD9.STOPF = 1;
                        //TODO:DOINPUT.CS
                        //           IF(ITYPE.lt.10) WRITE(*,801) IEVENT,ITYPE,IMOVES
                        //801        FORMAT(1X,'* unpaired event ',I4,' type ',I4,
                        //   1       ' allows illegal moves ',I2) 
                        //           IF(ITYPE.gt.10) WRITE(*,810) IEVENT,ITYPE,IMOVES
                        //810        FORMAT(1X,'* boxed event ',I4,' type ',I4,
                        //   1       ' allows illegal moves ',I2) 

                    }

                    //count numbers of events
                    NENDS = NENDS + 1;
                    IUNPRD = IUNPRD + 1;

                    if (COMMOD9.NROWS == 1)
                    {
                        COMMOD9.IRCOUNT = 1;
                    }
                    else if ((IEVENT-1 != HEVENT) || (ITYPE != HTYPE))//<=HXD
                    {
                        //C   -------------------------------------------
                        //C   subsequent records: if different event or type,
                        //C   make new row in IROWS
                        COMMOD9.IRCOUNT = COMMOD9.IRCOUNT + 1;
                    }


                    //C-----    ------------------------------------------------------------------
                    //C         in either case, put data into arrays.
                    //C         note that if event or type has not changed we don't really need
                    //C         to redo the whole assignment. it's just easier this way. 
                    //C-----    ------------------------------------------------------------------

                    HEVENT = IEVENT-1;//<=HXD
                    HTYPE = ITYPE;
                    COMMOD9.IROWS[COMMOD9.IRCOUNT - 1, 0] = IEVENT - 1;//<=HXD
                    COMMOD9.IROWS[COMMOD9.IRCOUNT - 1, 1] = ITYPE;
                    COMMOD9.IROWS[COMMOD9.IRCOUNT - 1, 2] = IUNPRD;
                    COMMOD9.ISTATIC[COMMOD9.IRCOUNT - 1, ISECTION - 1, 0] = ILEVEL - 1;//<=HXD
                    COMMOD9.ISTATIC[COMMOD9.IRCOUNT - 1, ISECTION-1, 1] = IMOVES;
                    COMMOD9.RSTATIC[COMMOD9.IRCOUNT - 1, ISECTION-1, 0] = WTUP;
                    COMMOD9.RSTATIC[COMMOD9.IRCOUNT - 1, ISECTION-1, 1] = WTDN;


                }
                else
                {
                    //event was paired and NROWS should not have been counted        
                    COMMOD9.NROWS = COMMOD9.NROWS - 1;
                }

                //go get next record
                goto Label3;

            }
            reader.Close();

            //---------------------------------------
            COMMOD9.NROWS = COMMOD9.NROWS - 1;//<=HXD



            if (COMMOD9.STOPF == 1) goto Label9999;

            //CPMS*******************************************
            //CPMS*************************************
            //CPMS***BETWEEN LOOP STUFF****      
            //CPMS********************* 
            //C	standardize the base levels to 1000
            //CPMS
            //CPMS  Correct VALEVEL so that base of section is always 1000 
            //CPMS  not less - to correct for negative well depths
            //CPMS  not greater - so that plotting routines can make quick assumptions!
            //CPMS  Store correction factor at MXLVL+3
            //CPMS--------------------------
            Helper.Write("  Standardizing base levels to 1000 . . . \n");

            //Loop through all sections
            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                //CPMS     Find difference between elevation of section base and 1000
                //CPMS     Store correction factor:  
                COMMOD9.VALEVEL[COMMOD9.MXLVL + 2, J] = 1000.00 - COMMOD9.VALEVEL[0, J];

                for (I = 0; I <= COMMOD9.HLEVEL[J]; I++)//<=HXD  HLEVEL[] boundary is in [0,ILEVEL-1]
                {
                    COMMOD9.VALEVEL[I, J] = COMMOD9.VALEVEL[I, J] + COMMOD9.VALEVEL[COMMOD9.MXLVL + 2, J];
                                        
                }
            }

            

            //cpms  cumulate event counts in elevel
            //cpms  cumulative counts behave like stratigraphic distance
            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                if (COMMOD9.HLEVEL[J] < 1) continue;//<=HXD

                for (I = 1; I <= COMMOD9.HLEVEL[J]; I++)//<=HXD
                {
                    COMMOD9.ELEVEL[I, J] = COMMOD9.ELEVEL[I, J] + COMMOD9.ELEVEL[I - 1, J];
                }
            }

           

            if (COMMOD9.IRCOUNT != COMMOD9.NEVNT - (COMMOD9.NSPC * 2))
            {
                COMMOD9.STOPF = 1;
                //TODO:DOINPUT.CS
                //      WRITE(*,111)NEVNT-(NSPC*2),IRCOUNT
                //111   FORMAT(1X,'* ',I5,' unpaired events expected,',
                //   1          'but unpaired events reach', I5) 
                goto Label9999;
            }


            //TODO:DOINPUT.CS
            //CLOSE(2, STATUS='KEEP')
            //OPEN(2,FILE=INFILE)
            //CPMS*****************************
            //CPMS**********************
            //CPMS***SECOND LOOP*** 
            //CPMS**************
            //CPMS  reset event counter
            IEVENT = 0;

            //C     the following is a big loop that executes until the EOF marker is
        //C     hit (END=7). The program then jumps to statement #7, the first
        //C         statement after "go to 5", the bottom statement in the loop.
        //C     it is like a "while not eof do" with the endwhile just above 7
        //C         in this case, the go-to stmt.
            if (COMMOD9.NROWS == 0) IPRD = 1; else IPRD = 0;//<=HXD
            reader = new StreamReader(File.Open(COMMOD9.INFILE, FileMode.Open));
        Label5:
            COMMOD9.NROWS = COMMOD9.NROWS + 1;
            
            //C     ----------------------------------------------------
            //C     read in data, including allowed moves for each event
            //C     and weights for placing it above or below its datum         
            //      READ(2, * ,END=7) EVENT, TYP, SECTION, VALUE, LEVEL,
            //     1                    MOVES, WTUP, WTDN

            //C----------------------------------------
            //C        this block ought to be redundant
            //C        it was all done on first LOOP
            //c
            //c          IF((EVENT.NE.IEVENT).AND.(EVENT.NE.1+IEVENT)) THEN
            //c            STOPF=1
            //c            WRITE(*,201)1+IEVENT
            //c  201         FORMAT(1X,'taxon/event ',I5,' missing or out of order')        
            //c          ENDIF
            //c          LSTSECT  = MAX(LSTSECT,SECTION)
            //c          LSTEVNT  = MAX(LSTEVNT,EVENT)
            //C----------------------------------------------------------------------
            //C       convert reals to integers to use as indices
            //c       THIS LOOP ONLY WORKS ON FADs and LADs            

            dataLine=null;
            while ((dataLine = reader.ReadLine()) != null)
            {
                string[] dataRowString = dataLine.Split(new string[] { " " }, StringSplitOptions.RemoveEmptyEntries);
                EVENT = double.Parse(dataRowString[0].ToString());
                TYP = double.Parse(dataRowString[1].ToString());
                SECTION = double.Parse(dataRowString[2].ToString());
                VALUE = double.Parse(dataRowString[3].ToString());
                LEVEL = double.Parse(dataRowString[4].ToString());
                MOVES = double.Parse(dataRowString[5].ToString());
                WTUP = double.Parse(dataRowString[6].ToString());
                WTDN = double.Parse(dataRowString[7].ToString());

                if ((TYP < 3) && (TYP > 0))
                {
                    IEVENT = (int)EVENT;
                    ITYPE = (int)TYP;
                    ISECTION = (int)SECTION;
                    ILEVEL = (int)LEVEL;
                    IMOVES = (int)MOVES;
                    NENDS = NENDS + 1;

                    //cpms 	    check for illegal combinations
                    //cpms      note: types 11 and 12 should never be considered in this loop
                    if (((ITYPE == 1) && (IMOVES < 1)) ||
                        ((ITYPE == 1) && (IMOVES > 3)) ||
                        ((ITYPE != 2) && (IMOVES == 2)) ||
                        ((ITYPE == 2) && (IMOVES < 2)) ||
                        ((ITYPE == 2) && (IMOVES > 3)) ||
                        ((ITYPE == 11) && (IMOVES != 2)) ||
                        ((ITYPE == 12) && (IMOVES != 1)))
                    {
                        COMMOD9.STOPF = 1;
                        //TODO:DOINPUT.CS
                        //           WRITE(*,802) IEVENT,ITYPE,IMOVES
                        //802        FORMAT(1X,'* paired event ',I4,' type ',I4,
                        //   1       ' allows illegal moves ',I2) 
                    }

                    if (COMMOD9.NROWS == 1)
                    {
                        //first record
                        COMMOD9.IRCOUNT = 1;
                    }
                    else if ((IEVENT-1 != HEVENT) || (ITYPE != HTYPE))
                    {
                        //subsequent records: if different event or type,
                        //make new row in IROWS 
                        COMMOD9.IRCOUNT = COMMOD9.IRCOUNT + 1;
                        if (IEVENT-1 != HEVENT) IPRD = IPRD + 1;
                    }

                    //C-----    ----------------------------------------------------
                    //C   in either case, put data into arrays.
                    //C   note that if event or type has not changed we don't really need
                    //C   to redo the whole assignment. it's just easier this way. 
                    HEVENT = IEVENT-1;//<=HXD
                    HTYPE = ITYPE;
                    COMMOD9.IROWS[COMMOD9.IRCOUNT - 1, 0] = IEVENT-1;//<=HXD
                    COMMOD9.IROWS[COMMOD9.IRCOUNT - 1, 1] = ITYPE;
                    COMMOD9.IROWS[COMMOD9.IRCOUNT - 1, 2] = IPRD-1;//<=HXD
                    COMMOD9.ISTATIC[COMMOD9.IRCOUNT - 1, ISECTION - 1, 0] = ILEVEL-1;//<=HXD
                    COMMOD9.ISTATIC[COMMOD9.IRCOUNT - 1, ISECTION - 1, 1] = IMOVES;
                    COMMOD9.RSTATIC[COMMOD9.IRCOUNT - 1, ISECTION - 1, 0] = WTUP;
                    COMMOD9.RSTATIC[COMMOD9.IRCOUNT - 1, ISECTION - 1, 1] = WTDN;
                    COMMOD9.NEGATIVE[IPRD - 1, ISECTION - 1] = 1-1;//<=HXD
                    COMMOD9.NEGATIVE[IPRD - 1, COMMOD9.NSCT + ITYPE - 1] = COMMOD9.IRCOUNT-1;//<=HXD

                    //cpms   iprd counts paired events consecutively
                    //cpms   this gives coexst and negative the same structure 
                    //C---------------------------------------------------------------
                    //CPMS   allow CONTRACT setting to over-ride input MOVES
                    if (COMMOD9.CONTRACT == 0)
                    {
                        if ((ITYPE == 1) || (ITYPE == 2))
                        {
                            COMMOD9.ISTATIC[COMMOD9.IRCOUNT - 1, ISECTION - 1, 1] = ITYPE;
                        }
                    }
                    else if ((COMMOD9.CONTRACT == 2) || (COMMOD9.CONTRACT == 3))
                    {
                        //* difference between 2 and 3 cannot be implemented
                        //* because amoves options 4,5,6 have been abandoned
                        if ((ITYPE == 1) || (ITYPE == 2))
                        {
                            COMMOD9.ISTATIC[COMMOD9.IRCOUNT - 1, ISECTION - 1, 1] = 3;
                        }
                    }

                    //  an unpaired event
                    //  NROWS must be reset        
                    //HACK:COMMOD9.NROWS = COMMOD9.NROWS - 1;

                }

                //CPMS**************************
                //C-----------------------
                //C     go get next record       
                //C-----------------------
                goto Label5;

            }
            
            //CPMS*********************************
            //CPMS****END OF SECOND LOOP*****
            //CPMS************************

            //  set the global NOBS variable - observed events
            COMMOD9.NOBS = NENDS;
            //--fill the culllist colummns for number of sections in which
            //  events are observed (1)
            //  and observed away from section ends(2)
            Helper.EVENTSUM(1, MINC);
            Helper.EVENTSUM(2, MINC);

            
            //  total up the number of exclusive taxa
            COMMOD9.NEXCL = 0;

            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                if (COMMOD9.CULLIST[I, 2] == 1) COMMOD9.NEXCL = COMMOD9.NEXCL + 1;
            }

            //CPMS---------------------------------------------------------------
            //c     note which sections have exclusive events at ends
            //c     cumulate number of them in SECTPROP(J,3)
            //c     single event may float free unless constrained by TEASER=COEX 
            //c     note which sections have singleton exclusive taxa at ends
            //c     cumulate number of them in SECTPROP(J,2) 
            //c     both events may float free to level of minimal COEX
            COMMOD9.JEXCL = 0;
            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                for (I = 0; I < COMMOD9.NEVNT; I++)
                {
                    //if not an exclusive event, cycle
                    if (COMMOD9.CULLIST[I, 2] != 1) continue;

                    //cpms       if a FAD at top, or a LAD bottom, of section
                    //cpms       FAD and LAD must be at same level
                    //cpms       range is an unconstrained exclusive singleton
                    //cpms       count will include only taxa with range entirely at section end
                    //cpms       reset JSPANF and mark sectprop_2
                    if (((COMMOD9.ISTATIC[I, J, 0] == 0) && (COMMOD9.IROWS[I, 1] == 2)) ||
                        ((COMMOD9.ISTATIC[I, J, 0] == COMMOD9.HLEVEL[J]) &&
                        (COMMOD9.IROWS[I, 1] == 1)))//<=HXD
                    {
                        if (COMMOD9.JSPANF > 0) COMMOD9.JSPANF = COMMOD9.JSPANF + 1;
                        COMMOD9.SECTPROP[J, 1] = COMMOD9.SECTPROP[J, 1] + 1;
                    }

                    //cpms       if a LAD/DIS at top, or a FAD/APP bottom, of section
                    //cpms       FAD and LAD not necessarily at same level
                    //cpms       count will include taxa already in SECTPROP(J,2)
                    //cpms       plus those with only one range end at section end
                    //cpms       mark sectprop_3  (JSPANF already augmented)
                    if (((COMMOD9.ISTATIC[I, J, 0] == 0) && (Math.Abs(COMMOD9.IROWS[I, 1]) == 1)) ||
                        ((COMMOD9.ISTATIC[I, J, 0] == COMMOD9.HLEVEL[J]) &&
                        (Math.Abs(COMMOD9.IROWS[I, 1]) == 2)))//<=HXD
                    {
                        COMMOD9.SECTPROP[J, 2] = COMMOD9.SECTPROP[J, 2] + 1;
                    }
                }

                //augment count of exclusive sections
                if (COMMOD9.SECTPROP[J, 1] > 0) COMMOD9.JEXCL = COMMOD9.JEXCL + 1;

            }//End for J


            //i.e. exclusives 'on' and needed
            if (COMMOD9.JSPANF > 1)
            {
                COMMOD9.JSPANF = 1;
            }
            else  if (COMMOD9.JSPANF == 1)
            {
                COMMOD9.JSPANF = 0;
            }

           

           
             

            ////i.e. exclusives 'on' and needed
            //if (COMMOD9.JSPANF > 1) COMMOD9.JSPANF = 1;

            ////i.e. exclusives 'on' but not needed
            //if (COMMOD9.JSPANF == 1) COMMOD9.JSPANF = 0;//???<=HXD

            //C------------------------------------------------------
            //CPMS   determine maximum total penalty that would accrue if 
            //CPMS   all ranges extended to section tops and bases 
            COMMOD9.MAXPEN = 0.0;
            if (COMMOD9.PENF < 2)
            {
                for (M = 0; M < COMMOD9.NSCT; M++)
                {
                    for (N = 0; N < COMMOD9.NEVNT; N++)
                    {
                        //if not found, cycle, else calculate penalty
                        if (Helper.IORZERO(N, M)) continue;

                        if (Math.Abs(COMMOD9.IROWS[N, 1]) == 1)
                        {
                            if (COMMOD9.PENF == 0)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[N, M, 1] *
                                    (COMMOD9.VALEVEL[COMMOD9.ISTATIC[N, M, 0], M] - 1000));
                            }
                            else if (COMMOD9.PENF == 1)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[N, M, 1] *
                                    (double)(COMMOD9.ISTATIC[N, M, 0]));//<=HXD
                            }
                            else if (COMMOD9.PENF == -1)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[N, M, 1] *
                                    (double)(COMMOD9.ELEVEL[COMMOD9.ISTATIC[N, M, 0], M]));//<=HXD
                            }
                        }

                        if (Math.Abs(COMMOD9.IROWS[N, 1]) == 2)
                        {
                            if (COMMOD9.PENF == 0)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[N, M, 0] *
                                    (COMMOD9.VALEVEL[COMMOD9.HLEVEL[M], M] -
                                    COMMOD9.VALEVEL[COMMOD9.ISTATIC[N, M, 0], M]));//HACK:
                            }
                            else if (COMMOD9.PENF == 1)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[N, M, 0] *
                                    (double)(COMMOD9.HLEVEL[M] - COMMOD9.ISTATIC[N, M, 0]));//HACK:
                            }
                            else if (COMMOD9.PENF == -1)
                            {
                                COMMOD9.MAXPEN = COMMOD9.MAXPEN + (COMMOD9.RSTATIC[N, M, 0] *
                                    (double)(COMMOD9.ELEVEL[COMMOD9.HLEVEL[M], M] -
                                    COMMOD9.ELEVEL[COMMOD9.ISTATIC[N, M, 0], M]));//HACK:
                            }
                        }
                    }//End for N
                }//Enf for M
            }//End if
                        

            //CPMS  maximum number of contradictable event-pairs per section 
            //CPMS  is stored in PAIRJ(NSCT)
            //CPMS  if optimized penalty is ordinal, MAXPEN is determined also
            //*     if a first is not seen, its last may still determine the pairwise
            //*     ordering; so this count is an underestimate!
            //CPMS  also need to calculate maxpen for PENF=3
            //CPMS  i.e. sum of the separations of all contradictable pairs
            //CPMS
            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                JOPT = 0;

                for (I = 0; I < COMMOD9.NEVNT - 1; I++)
                {
                    if (COMMOD9.ISTATIC[I, J, 0] == -1) continue;//<=HXD

                    for (K = I + 1; K < COMMOD9.NEVNT; K++)
                    {
                        if (COMMOD9.ISTATIC[K, J, 0] == -1) continue;//<=HXD

                        if (COMMOD9.PENF == 2) COMMOD9.MAXPEN = COMMOD9.MAXPEN + 1;

                        if (COMMOD9.PENF == 3) COMMOD9.MAXPEN = COMMOD9.MAXPEN +
                                Math.Abs(COMMOD9.VALEVEL[COMMOD9.ISTATIC[I, J, 0], J] -
                                COMMOD9.VALEVEL[COMMOD9.ISTATIC[K, J, 0], J]);

                        JOPT = JOPT + 1;
                    }
                }

                COMMOD9.PAIRJ[J] = JOPT;
            }// End for J

            if (COMMOD9.PENF == 4) COMMOD9.MAXPEN = (COMMOD9.NEVNT * (COMMOD9.NEVNT - 1)) / 2;
            //C     i.e. every pairwise placement violates all observations
            //c     adding 1.00 to the penalty     
            if ((COMMOD9.PENF == 5) || (COMMOD9.PENF == 6)) COMMOD9.MAXPEN =
                        (COMMOD9.NEVNT * (COMMOD9.NEVNT - 1));
            //C**********************************************************************       
            //
            //C     SAVE THE NUMBER OF ROWS IN "DATA" FOR LATER USE
            //C----------------------------------------------------------------------
            COMMOD9.NROWS = COMMOD9.NROWS - 1;

            //CPMS--------------------------------------------------------------------
            //CPMS  Look for missing levels, section-by-section
            //CPMS  Levels that are missing from input file will have values left at
            //CPMS  zero in VALEVEL().  This is OK only for levels beyond the highest
            //CPSM  assigned in each section
            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                for (I = 0; I < COMMOD9.MXLVL; I++)
                {
                    if ((COMMOD9.VALEVEL[I, J] < 999.0) && (COMMOD9.VALEVEL[I, J] != 0))
                    {
                        COMMOD9.STOPF = 1;
                        //TODO:DOINPUT.CS
                        //              WRITE(*,894)I,J,VALEVEL(I,J)
                        //894           FORMAT(1X,'* level ',I4,' in section ',I4,
                        //   1                 ' placed below 1000: ',F11.3)
                    }

                    //CPMS----In the next line GT is used rather than GE.  This allows for the 
                    //CPMS----possibility that two very closely spaced samples arrive at the
                    //CPMS----same value due to rounding to two decimal places.  For the meter
                    //CPMS----system two decimal places limits precision to one centimeter.
                    //CPMS----It would be desirable to work to F11.4; but some part of the program
                    //CPMS----insists upon F9.2.  I have not found it yet; it happens in the 
                    //CPMS----CONSORT.EXE routines and, perhaps, elswhere.
                    if ((COMMOD9.VALEVEL[I, J] > COMMOD9.VALEVEL[I + 1, J]) &&
                        ((COMMOD9.VALEVEL[I + 2, J] != 0.0) || (COMMOD9.VALEVEL[I + 1, J] != 0.0)))
                    {
                        COMMOD9.STOPF = 1;
                        //              WRITE(*,895) I+1,J
                        //895           FORMAT(1X,'* level ',I4,' in section ',I4,
                        //   1                 ' missing or out of order')

                        //Helper.Write("I={0} J={1} {2} {3} {4}\n",I,J, COMMOD9.VALEVEL[I, J], COMMOD9.VALEVEL[I + 1, J], COMMOD9.VALEVEL[I + 2, J]);
                    }
                }//End for I
            }//End for J

            

            if (COMMOD9.STOPF == 1) goto Label9999;
            

            //CPMS*******************************
            //CPMS	 fill the ZLEVEL array
            //CPMS   ZLEVEL(J,1) = mean level spacing
            //CPMS   ZLEVEL(J,2) = Std Deviation level spacing
            //CPMS   if less than 3 levels; plug in defaults and cycle out!
            for (J = 0; J < COMMOD9.NSCT; J++)
            {
                if (COMMOD9.HLEVEL[J] == 0)//<=HXD
                {
                    COMMOD9.ZLEVEL[J, 0] = 0.0;
                    COMMOD9.ZLEVEL[J, 1] = 0.0;
                    continue;
                }

                if (COMMOD9.HLEVEL[J] == 1)//<=HXD
                {
                    COMMOD9.ZLEVEL[J, 0] = COMMOD9.VALEVEL[1, J] - COMMOD9.VALEVEL[0, J];
                    COMMOD9.ZLEVEL[J, 1] = 0.0;
                    continue;
                }

                MNSPACE = 0.0;
                for (I = 1; I <= COMMOD9.HLEVEL[J]; I++)//<=HXD
                {
                    MNSPACE = MNSPACE + COMMOD9.VALEVEL[I, J] - COMMOD9.VALEVEL[I - 1, J];
                }
                MNSPACE = MNSPACE / (COMMOD9.HLEVEL[J]);//<=HXD
                COMMOD9.ZLEVEL[J, 0] = MNSPACE;

                MUSPACE = 0.0;
                for (I = 1; I <= COMMOD9.HLEVEL[J]; I++)//<=HXD
                {
                    MUSPACE = MUSPACE + (Math.Pow((COMMOD9.VALEVEL[I, J] - COMMOD9.VALEVEL[I - 1, J] - MNSPACE), 2));
                }
                MUSPACE = Math.Pow((MUSPACE / (COMMOD9.HLEVEL[J])), 0.5);//<=HXD
                COMMOD9.ZLEVEL[J, 1] = MUSPACE;
            }

            

            //CPMS****************************************************
            //CPMS  fill the SECTNAME and EVNTNAME arrays 
            //CPMS
            //CPMS  load the section names if available
            if ((COMMOD9.SCTFILE.Substring(0, 3) == "OFF") || (COMMOD9.SCTFILE.Substring(0, 3) == "off"))
            {
                //CPMS     graphics routines still need a section order       
                Helper.Write("  Section Names NOT Provided; using numbers only\n");
                for (ISECTION = 0; ISECTION < COMMOD9.NSCT; ISECTION++)
                {
                    COMMOD9.SECTPERM[ISECTION] = ISECTION;
                    JNICK = "(" + ISECTION + ")";
                    COMMOD9.SECTNAME[ISECTION] = "SECTION " + JNICK;
                    COMMOD9.SECTNICK[ISECTION] = JNICK;
                    COMMOD9.SECTPROP[ISECTION, 0] = 1;
                }
            }
            else
            {
                Helper.Write("  Loading Section Names from " + COMMOD9.SCTFILE.Trim() + " ...\n");
                ISECTION = -1;//HACK:ISECTION=-1 IN FORTRAN
            //TODO:DOINPUT.CS
                reader = new StreamReader(File.Open(COMMOD9.SCTFILE, FileMode.Open));
            //OPEN(48,FILE=SCTFILE)
            Label201:
                ISECTION = ISECTION + 1;
                //TODO:DOINPUT.CS
                //READ(48,*,END=202) HSECTION, JNICK, JPLACE, JNAME, JINC
                dataLine=null;
                while ((dataLine = reader.ReadLine()) != null)
                {
                    string[] dataRowString = dataLine.Split(new string[] { "'" }, StringSplitOptions.RemoveEmptyEntries);
                    HSECTION = int.Parse(dataRowString[0].ToString().Trim());
                    JNICK = dataRowString[1].ToString().Trim();
                    JPLACE = int.Parse(dataRowString[2].ToString().Trim());
                    JNAME = dataRowString[3].ToString().Trim();
                    JINC = int.Parse(dataRowString[4].ToString().Trim());

                    //Helper.Write("{0} | {1} | {2} | {3} | {4}\n", HSECTION, JNICK, JPLACE, JNAME, JINC);

                    COMMOD9.SECTNAME[ISECTION] = JNAME;
                    COMMOD9.SECTNICK[ISECTION] = JNICK;
                    COMMOD9.SECTPERM[JPLACE-1] = ISECTION;//HACK:Maybe COMMOD9.SECTPERM[JPLACE-1]=ISECTION
                    COMMOD9.SECTPROP[ISECTION, 0] = JINC;

                    //if JINC out of bounds, prevent use 
                    if ((JINC > 1) || (JINC < 0))
                    {
                        COMMOD9.STOPF = 1;
                        //TODO:DOINPUT.CS
                        //         WRITE(*,205) ISECTION
                        //205      FORMAT(3X,'* column 5 missing or out-of-bounds in SECTFILE',
                        //   1            1X,'  Section: ',I4)
                        goto Label206;
                    }
                    else if (JINC == 1)
                    {
                        NSCAL = NSCAL + 1;
                    }

                    //if numbering error found, prevent use
                    if (HSECTION-1 != ISECTION)
                    {
                        COMMOD9.SCTFILE = "OFF";
                        COMMOD9.STOPF = 1;
                        //TODO:DOINPUT.CS
                        //         WRITE(*,203) HSECTION, ISECTION
                        //203      FORMAT(3X,'* section number ',I4,' appears in row ',I4,
                        //   1                 ' of section name file')
                        goto Label206;
                    }

                    goto Label201;
                }
                reader.Close();

                

                //check the number of sections   
                if (ISECTION  != COMMOD9.NSCT)//HACK:Maybe ISECTION!=COMMOD9.NSCT
                {
                    COMMOD9.SCTFILE = "OFF";
                    COMMOD9.STOPF = 1;
                    //TODO:DOINPUT.CS
                    //         WRITE(*,204) ISECTION, NSCT
                    //204      FORMAT(1X,'* ',I4,' section names found ',I4,' expected') 
                }

                

                //check SECTPERM for missing positions
                for (J = 0; J < COMMOD9.NSCT; J++)
                {
                    if (COMMOD9.SECTPERM[J] == -1)//<=HXD
                    {
                        COMMOD9.STOPF = 1;
                        //TODO:DOINPUT.CS
                        //           WRITE(*,207) J
                        //207        FORMAT(3X,'* no section assigned to position ',I4)
                    }
                }

                
            }

        Label206:
            if (COMMOD9.SCTFILE.Substring(0, 3) != "OFF")
            {
                //TODO:DOINPUT.CS
                //CLOSE(48, STATUS='KEEP') 
            }

            if (NSCAL == 1)
            {
                Helper.Write("   WARNING: No sections allocated for scaling\n");
                Helper.Write("            [col 5 of section list all 0s]\n");
                Helper.Write("            Scaled composites not possible\n");
            }

            //load the section tag names if available
            if ((COMMOD9.SCTTAGS.Substring(0, 3) != "OFF") &&
                (COMMOD9.SCTTAGS.Substring(0, 3) != "off"))
            {
                Helper.Write("  Loading Section Tag Names . . .\n");
            //TODO:DOINPUT.CS
            //OPEN(54,FILE=SCTTAGS)
            Label741:
                //TODO:DOINPUT.CS
                //READ(54,*,END=742) Ilbl, TAGSTRING
                COMMOD9.STAGNAME[Ilbl] = TAGSTRING.Trim();
                goto Label741;
            Label742:
                COMMOD9.MXJTAG = Ilbl;
            }
                       

            if (COMMOD9.SCTTAGS.Substring(0, 3) != "OFF")
            {
                //TODO:DOINPUT.CS
                //CLOSE(54, STATUS='KEEP')
            }

            //load the section tags if available
            if ((COMMOD9.STAGFILE.Substring(0, 3) != "OFF") &&
                (COMMOD9.STAGFILE.Substring(0, 3) != "off"))
            {
                NTAG = 0;
                Helper.Write("  Loading Tags for each Section. . .\n");
            //TODO:DOINPUT.CS
            //OPEN(57,FILE=STAGFILE)
            Label711:
                //TODO:DOINPUT.CS
                //READ(57,*,END=712) Ilbl, JTAG 
                COMMOD9.SECTPROP[Ilbl, 3] = JTAG;//HACK:Maybe Ilbl-1
                NTAG = Math.Max(NTAG, JTAG);
                goto Label711;
            Label712:
                ;
            }

            if (COMMOD9.STAGFILE.Substring(0, 3) != "OFF")
            {
                //TODO:DOINPUT.CS
                //CLOSE(57, STATUS='KEEP')
            }

            if (NTAG > COMMOD9.MXJTAG)
            {
                COMMOD9.STOPF = 1;
                //TODO:DOINPUT.CS
                //       WRITE(*,209) MXJTAG, NTAG
                //209    FORMAT(1X,'* ',I4,' section tags defined ',I4,' used!')   
            }
            else if (NTAG < COMMOD9.MXJTAG)
            {
                //TODO:DOINPUT.CS
                //       WRITE(*,210) MXJTAG+1, MXJTAG-NTAG
                //210    FORMAT(1X,'* ',I0,' section tags defined; highest ',I0,
                //   1                     ' not used!')
            }

            
            //load the label names if available
            if ((COMMOD9.LBLFILE.Substring(0, 3) != "OFF") && (COMMOD9.LBLFILE.Substring(0, 3) != "off"))
            {
                Helper.Write("  Loading Label Names " + COMMOD9.LBLFILE.Trim() + " ...\n");
            //TODO:DOINPUT.CS
            //OPEN(50,FILE=LBLFILE)
            Label211:
                //TODO:DOINPUT.CS
                //READ(50,*,END=212) Ilbl, JNAME 
                COMMOD9.LABLNAME[Ilbl] = JNAME;//HACK: Maybe Ilbl-1;
                // go to 211
                //212    continue
            }

            if (COMMOD9.LBLFILE.Substring(0, 3) != "OFF")
            {
                //TODO:DOINPUT.CS
                //CLOSE(50, STATUS='KEEP')
            }


            //load the event names if available
            if ((COMMOD9.EVTFILE.Substring(0, 3) == "OFF") || (COMMOD9.EVTFILE.Substring(0, 3) == "off"))
            {
                Helper.Write("  Event Names NOT Provided; using numbers only\n");
                for (IEVENT = 0; IEVENT < COMMOD9.NEVNT - COMMOD9.NSPC; IEVENT++)
                {
                    INICK = "(" + IEVENT + ")";
                    COMMOD9.EVNTNAME[IEVENT] = "EVENT" + INICK;
                    COMMOD9.EVNTNICK[IEVENT] = "Evt." + INICK;
                }
            }
            else
            {
                Helper.Write("  Loading Event Names from " + COMMOD9.EVTFILE.Trim() + "\n");
                IEVENT = -1;//HACK: IEVENT=0 IN FORTRAN
                reader = new StreamReader(File.Open(COMMOD9.EVTFILE, FileMode.Open));

            Label301:
                IEVENT = IEVENT + 1;

                //TODO:DOINPUT.CS
                //READ(49,*,END=302) HEVENT, INICK, INAME
                dataLine = null;
                while ((dataLine = reader.ReadLine()) != null)
                {
                    string[] dataRowString = dataLine.Split(new string[] { "'" }, StringSplitOptions.RemoveEmptyEntries);
                    HEVENT = int.Parse(dataRowString[0].Trim());
                    INICK = dataRowString[1].Trim();
                    INAME = dataRowString[3].Trim();

                    //Helper.Write("{0} | {1} | {2}\n",HEVENT,INICK,INAME);

                    COMMOD9.EVNTNICK[IEVENT] = INICK;

                    //cpms    check for label as prefix to name
                    //cpms    labels have 5 character format *nnA*
                    //cpms    nn is label number; 
                    //cpms    A:      F=FAD L=LAD M=MID X=ASH/BED N=AGE A=MAX I=MIN
                    if (INAME.Substring(0, 1) == "*")
                    {
                        A = 0;
                        Ilbl = 0;
                        COMMOD9.EVNTNAME[IEVENT] = INAME.Substring(5);

                        if (INAME.Substring(3, 1) == "F") Ityp = 1;
                        if (INAME.Substring(3, 1) == "L") Ityp = 2;
                        if (INAME.Substring(3, 1) == "M") Ityp = 3;
                        if (INAME.Substring(3, 1) == "X") Ityp = 4;
                        if (INAME.Substring(3, 1) == "N") Ityp = 5;

                        for (L = 0; L < 10; L++)
                        {
                            if (DIGIT[L] == INAME.Substring(1, 1))
                            {
                                Ilbl = Ilbl + ((L - 1) * 10);
                                A = A + 1;
                            }

                            if (DIGIT[L] == INAME.Substring(2, 1))
                            {
                                Ilbl = Ilbl + (L - 1);
                                A = A + 1;
                            }
                        }

                        if ((A != 2) || (Ityp == 0))
                        {
                            COMMOD9.STOPF = 1;
                            Helper.Write("* Invalid label for event:{0}\n", HEVENT);
                        }
                        else
                        {
                            Helper.FINDROW(HEVENT - 1, Ityp, ref IROW);//<=HXD HEVENT read from evt file base from 1
                            COMMOD9.IROWS[IROW, 4] = Ilbl;
                        }
                    }
                    else
                    {
                        COMMOD9.EVNTNAME[IEVENT] = INAME;
                    }

                    if (HEVENT-1 != IEVENT)//<=HXD HEVENT read from evt file base from 1
                    {
                        COMMOD9.EVTFILE = "OFF";
                        COMMOD9.STOPF = 1;
                        //TODO:DOINPUT.CS
                        //           WRITE(*,303) HEVENT, IEVENT
                        //303        FORMAT(1X,'* event number ',I4,' appears in row ',I4,
                        //   1              ' of event name file')
                    }

                    if (COMMOD9.STOPF == 1) goto Label333;

                    goto Label301;
                }
                reader.Close();

            }

            
        Label333:
            if (COMMOD9.EVTFILE.Substring(0, 3) != "OFF")
            {
                //TODO:DOINPUT.CS
                //CLOSE(49, STATUS='KEEP')
            }

            //load the event tag names if available
            if ((COMMOD9.EVTTAGS.Substring(0, 3) != "OFF") &&
                (COMMOD9.EVTTAGS.Substring(0, 3) != "off"))
            {
                Helper.Write("  Loading Event Tag Names . . .\n");

                //TODO:DOINPUT.CS
            //OPEN(55,FILE=EVTTAGS)
            Label751:
                //TODO:DOINPUT.CS
                //READ(55,*,END=752) Ilbl, TAGSTRING 
                COMMOD9.ETAGNAME[Ilbl + 1] = TAGSTRING.Trim();//HACK:May be ETAGNAME[Ilbl]

                goto Label751;
                //TODO:DOINPUT.CS
                //752    continue 

                COMMOD9.MXITAG = Ilbl;
            }

            if (COMMOD9.EVTTAGS.Substring(0, 3) != "OFF")
            {
                //TODO:DOINPUT.CS
                //CLOSE(55, STATUS='KEEP')
            }


            //load the event tags if available
            //  default tag is 0 
            //  replace default values is possible
            if ((COMMOD9.ETAGFILE.Substring(0, 3) != "OFF") &&
                (COMMOD9.ETAGFILE.Substring(0, 3) != "off"))
            {
                Helper.Write("  Loading Tags for each Event . . .\n");
            //TODO:DOINPUT.CS
            //OPEN(56,FILE=ETAGFILE)
            Label761:
                //TODO:DOINPUT.CS
                //READ(56,*,END=762) Ilbl, ITAG 

                for (I = 0; I < COMMOD9.NEVNT; I++)
                {
                    if (COMMOD9.IROWS[I, 0] == Ilbl) COMMOD9.IROWS[I, 5] = ITAG;
                }

                goto Label761;

                //TODO:DOINPUT.CS
                //762    continue
            }

            if (COMMOD9.ETAGFILE.Substring(0, 3) != "OFF")
            {
                //TODO:DOINPUT.CS
                //CLOSE(56, STATUS='KEEP')
            }


            //----------------------------------------------
            //--fill IROWS(col4) with mates to boxed events
            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                switch (COMMOD9.IROWS[I, 1])
                {
                    case 11:
                        Helper.FINDROW(COMMOD9.IROWS[I, 0], 12, ref IPAIR);
                        COMMOD9.IROWS[I, 3] = IPAIR;
                        break;
                    case 12:
                        Helper.FINDROW(COMMOD9.IROWS[I, 0], 11, ref IPAIR);
                        COMMOD9.IROWS[I, 3] = IPAIR;
                        break;
                    default:
                        COMMOD9.IROWS[I, 3] = COMMOD9.IROWS[I, 0];
                        break;
                }
            }


            //CPMS--COMPLETE IROWS(  ,4) to show mates of paired events
            //cpms--the following is faster than the loop used of MAX and MIN
            //cpms-- but cannot be run until NEGATIVE() is filled
            //cpms-- remember: COEXST() and NEGATIVE() have IROWS(*,3) order
            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                switch (COMMOD9.IROWS[I, 1])
                {
                    case 1:
                        if (COMMOD9.NEGATIVE[COMMOD9.IROWS[I, 2], COMMOD9.NSCT + 1] >= 0)//<=HXD
                        {
                            COMMOD9.IROWS[I, 3] = COMMOD9.NEGATIVE[COMMOD9.IROWS[I, 2], COMMOD9.NSCT + 1];
                        }
                        else
                        {
                            COMMOD9.STOPF = 1;
                            //TODO:DOINPUT.CS
                            //WRITE(*,116) IROWS(I,1)
                            goto Label9999;
                        }
                        break;
                    case 2:
                        if (COMMOD9.NEGATIVE[COMMOD9.IROWS[I, 2], COMMOD9.NSCT] >= 0)//<=HXD
                        {
                            COMMOD9.IROWS[I, 3] = COMMOD9.NEGATIVE[COMMOD9.IROWS[I, 2], COMMOD9.NSCT];
                        }
                        else
                        {
                            COMMOD9.STOPF = 1;
                            //TODO:DOINPUT.CS
                            //WRITE(*,116) IROWS(I,1)
                            goto Label9999;
                        }
                        break;
                    default:
                        COMMOD9.IROWS[I, 3] = COMMOD9.IROWS[I, 0];
                        break;
                }
            }

            //TODO:DOINPUT.CS
            //116  FORMAT(1X,'* taxon ', I4,' missing a range end!')

            //cpms  now that every FAD has a LAD,
            //cpms  check that every MID has a FAD and a LAD
            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                if (COMMOD9.IROWS[I, 1] != 3) continue;

                IEVENT = COMMOD9.IROWS[I, 0];
                MFAD = 0;
                MLAD = 0;

                for (M = 0; M < COMMOD9.NEVNT; M++)
                {
                    if ((COMMOD9.IROWS[M, 0] == IEVENT) && (COMMOD9.IROWS[M, 1] == 1)) MFAD = M;

                    if ((COMMOD9.IROWS[M, 0] == IEVENT) && (COMMOD9.IROWS[M, 1] == 2)) MLAD = M;
                }

                if (MFAD == 0)
                {
                    COMMOD9.STOPF = 1;
                    Helper.Write(" * taxon',I, ' has MID but no FAD\n");
                }
                else if (MLAD == 0)
                {
                    COMMOD9.STOPF = 1;
                    Helper.Write(" * taxon',I, ' has MID but no LAD\n");
                }
            }

            //check that every MAX has a MIN
            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                if ((COMMOD9.IROWS[I, 1] != 11) && (COMMOD9.IROWS[I, 1] != 12)) continue;

                IEVENT = COMMOD9.IROWS[I, 0];
                //use MFAD for MAX and MIN

                MFAD = 0;
                MLAD = 0;

                for (M = 0; M < COMMOD9.NEVNT; M++)
                {
                    if ((COMMOD9.IROWS[M, 0] == IEVENT) && (COMMOD9.IROWS[M, 1] == 11)) MFAD = M;

                    if ((COMMOD9.IROWS[M, 0] == IEVENT) && (COMMOD9.IROWS[M, 1] == 12)) MLAD = M;
                }

                if ((MFAD == 0) && (MLAD != 0))
                {
                    COMMOD9.STOPF = 1;
                    Helper.Write(" * boxed event {0} has MIN but no MAX\n", I);
                }
                else if ((MLAD == 0) && (MFAD != 0))
                {
                    COMMOD9.STOPF = 1;
                    Helper.Write(" * boxed event {0} has MAX but no MIN\n", I);
                }
            }


            //--now correct IROWS(col4) for mates to boxed events
            for (I = 0; I < COMMOD9.NEVNT; I++)
            {
                switch (COMMOD9.IROWS[I, 1])
                {
                    case 11:
                        Helper.FINDROW(COMMOD9.IROWS[I, 0], 12, ref IPAIR);
                        COMMOD9.IROWS[I, 3] = IPAIR;                        
                        break;
                    case 12:
                        Helper.FINDROW(COMMOD9.IROWS[I, 0], 11, ref IPAIR);
                        COMMOD9.IROWS[I, 3] = IPAIR;                       
                        break;
                }
            }


            //--following should find YEVNT faster:
            COMMOD9.YEVNT = COMMOD9.IROWS[COMMOD9.XEVNT, 3];//HACK: YEVNT = IROWS(XEVNT,4) IN FORTRAN
                        
//-----------------------------------------------------------
        Label9999:
            //TODO:DOINPUT.CS
            //CLOSE(2, STATUS='KEEP')
            //if any invalid input has been found, stop the program

            if (UNPRDF == 1)
            {
                Helper.Write("_______________________________________________\n");
                Helper.Write("Input includes top-only and/or bottom-only taxa\n");
                Helper.Write("Change name/extension of current input file \n");
                Helper.Write("ENTER THE NEW NAME AS PREPFILE IN CONOP9.CFG\n");
                Helper.Write("AND THEN RUN CONSORT9 BEFORE RETRYING CONOP9\n");

                COMMOD9.STOPF = 1;
            }


            if (COMMOD9.STOPF == 1)
            {
                Helper.Write("______________________\n");
                Helper.Write("**INVALID INPUT DATA**\n");
                Helper.Write("\n");
#if OUTDATA
                System.Windows.Forms.MessageBox.Show("***INVALID INPUT DATA***\n", "CONOP C# export");
                Environment.Exit(1);
#endif
            }


            //for (int r = 0; r < COMMOD9.IROWS.GetLength(0); r++)
            //{
            //    Helper.Write("row[{0}]: {1} | {2} | {3} | {4} | {5} | {6} | \n", r,
            //        COMMOD9.IROWS[r,0],COMMOD9.IROWS[r,1],COMMOD9.IROWS[r,2]
            //        , COMMOD9.IROWS[r,3], COMMOD9.IROWS[r,4], COMMOD9.IROWS[r,5]);
            //}

            //double[, ,] T = COMMOD9.RSTATIC;

            //for (int x = 0; x < T.GetLength(0); x++)
            //{
            //    for (int y = 0; y < T.GetLength(1); y++)
            //    {
            //        Helper.Write("[{0} , {1}]\n", T[x, y, 0], T[x, y, 1]);
            //    }
            //    Helper.Write("\n");
            //}

            //TODO:DOINPUT.CS
            //CLOSE(11, STATUS='KEEP')
        }

        #endregion
    }
}
