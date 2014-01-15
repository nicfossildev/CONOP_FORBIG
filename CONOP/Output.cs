using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace CONOP.NET
{
    public static class Output
    {
        #region private functions
        //C*************************************
        //CPMS     A SUBROUTINE TO FIND SPECIES 
        //CPMS     AND TYPE FROM AN IROWS LABEL
        //C
        //C      PROGRAMMERS:  PETE SADLER  
        //C      LAST UPDATE:  April 3rd 1999
        //C*************************************
        private static void GETI(int I, out int ISPC, out int ITYP)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            ISPC = COMMOD9.IROWS[I, 0];
            ITYP = COMMOD9.IROWS[I, 1];
        }

        //C***********************************************
        //C     A SUBROUTINE TO GET observed and placed 
        //C     levels and values from IROWS
        //C     given section and irows numbers
        //C             
        //C         PROGRAMMERS:  PETE SADLER  
        //C         LAST UPDATE:  April 3rd 1999
        //C**********************************************
        private static void GETIJ(int I, int J, out int LVLF, out int LVLP, out double VALF, out double VALP)
        {
            //lvlf,lvlp:  level found,placed
            //valf,valp:  value found,placed 

            COMMOD COMMOD9 = COMMOD.Singleton();
            LVLF = COMMOD9.ISTATIC[I, J, 0];
            LVLP = COMMOD9.HSCTSOL[I, J];
            if (LVLF < 0)//<=HXD LVLF =-1
            {
                VALF = 0.0;
            }
            else
            {
                VALF = COMMOD9.VALEVEL[LVLF, J];
            }

            VALP = COMMOD9.VALEVEL[LVLP, J];
        }

        #endregion

        //C**********************************************************************
        //C
        //CPMS     A SUBROUTINE TO WRITE SELECTED OUTPUT TO TEXT and DATA FILES
        //CPMS     DURING AND AFTER CORRELATION OF A STRATIGRAPHIC DATA SET
        //CPMS     USING CONOP.FOR
        //C             
        //C
        //C         PROGRAMMERS:  PETE SADLER & BILL KEMPLE  
        //C         LAST UPDATE:  June 9th 1997
        //C
        //C**********************************************************************
        public static void BESTOUT()
        {
            //int L;
            COMMOD COMMOD9 = COMMOD.Singleton();
            COMMOD9.BestSolnSB.Length = 0;

            if ((COMMOD9.BESTSOL.Substring(0, 3) != "OFF") && (COMMOD9.BSTPEN == COMMOD9.VERYBEST))
            {

                foreach (int L in COMMOD9.BSTPERM)
                {
                    COMMOD9.BestSolnSB.AppendLine(string.Format(" {0,7}  {1,7}  {2,7}",
                        COMMOD9.IROWS[COMMOD9.BSTPERM[L], 0],
                        COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1],
                        L)
                        );
                }

                File.WriteAllText(COMMOD9.BESTSOL, COMMOD9.BestSolnSB.ToString());
            }


        }

        //**********************************************************************
        //     A SUBROUTINE TO WRITE A|SOLUTION TO START NEXT RUN
        //             
        //         PROGRAMMERS:  PETE SADLER & BILL KEMPLE  
        //         LAST UPDATE:  June 9th 1997
        //**********************************************************************
        public static void STARTOUT(int[] myperm)
        {
            int L;
            COMMOD COMMOD9 = COMMOD.Singleton();

            if ((COMMOD9.INITSOL.Substring(0, 3) != "OFF") && (COMMOD9.CDF != 1))
            {
                TextWriter writer = new StreamWriter(File.Open(COMMOD9.INITSOL, FileMode.Create));

                for (L = 0; L < COMMOD9.NEVNT; L++)
                {
                    writer.WriteLine(string.Format(" {0,7}  {1,7}  {2,7}",
                        COMMOD9.IROWS[myperm[L], 0] + 1,
                        COMMOD9.IROWS[myperm[L], 1],
                        L + 1));
                }

                writer.Flush();
                writer.Close();
            }
        }

        //C***********************************************
        //C
        //CPMS    A SUBROUTINE TO WRITE MISFIT STATISTICS
        //CPMS    TO THE MAIN OUTPUT FILE
        //C            
        //C
        //C        PROGRAMMERS:  PETE SADLER  
        //C        LAST UPDATE:  July 11th 2001
        //C
        //C**************************************
        public static void FITOUT()
        {
            int ISCT, ISPECIES, ITYPE, JSPC;
            int LVLFOUND, LVLPLACED, LVLTTL, NENDS, NEXTS, NPENS, NZRO;
            int NCTR, NPEN, NIMP, NEXT;
            int L, I;
            double DELTA, TDELTA, VALFOUND, VALPLACED, WEIGHT, THKTTL, EVNTPEN, TEMPPEN;
            string str = "";

            COMMOD COMMOD9 = COMMOD.Singleton();

            COMMOD9.OutmainSB.AppendLine();//new row

            switch (COMMOD9.PENM)
            {
                case -1:
                    COMMOD9.OutmainSB.AppendLine("OPTIMIZED ON Eventual PENALTY");
                    COMMOD9.OutmainSB.AppendLine("to minimize events within range extensions");
                    break;
                case 0:
                    COMMOD9.OutmainSB.AppendLine("OPTIMIZED ON Interval PENALTY");
                    COMMOD9.OutmainSB.AppendLine("to minimize thickness of range extensions");
                    break;
                case 1:
                    COMMOD9.OutmainSB.AppendLine("OPTIMIZED ON Level PENALTY");
                    COMMOD9.OutmainSB.AppendLine("to minimize levels within range extensions");
                    break;
                case 2:
                    COMMOD9.OutmainSB.AppendLine("OPTIMIZED ON Ordinal PENALTY");
                    COMMOD9.OutmainSB.AppendLine("to minimize number of pairwise event reversals");
                    break;
                case 3:
                    COMMOD9.OutmainSB.AppendLine("OPTIMIZED ON Spatial PENALTY");
                    COMMOD9.OutmainSB.AppendLine("to minimize size of pairwise event reversals");
                    break;
                case 4:
                    COMMOD9.OutmainSB.AppendLine("OPTIMIZED ON Rascal PENALTY");
                    COMMOD9.OutmainSB.AppendLine("to minimize probability of pairwise reversals");
                    break;
                case 5:
                    COMMOD9.OutmainSB.AppendLine("OPTIMIZED ON Royal PENALTY");
                    COMMOD9.OutmainSB.AppendLine("to minimize number of ghost coexistences");
                    break;
                case 6:
                    if (COMMOD9.FB4LF != 0)
                    {
                        COMMOD9.OutmainSB.AppendLine("OPTIMIZED ON Sequel PENALTY");
                        COMMOD9.OutmainSB.AppendLine("to minimize number of FAD\\LAD assumptions");
                    }
                    break;
            }

            if (COMMOD9.DFLTPEN)
            {
                COMMOD9.OutmainSB.AppendLine("*** this is a DEFAULT penalty option because");
                COMMOD9.OutmainSB.AppendLine("*** the entry in conop.cfg is not valid!");
            }

            if (COMMOD9.JSPANF > 0) COMMOD9.OutmainSB.AppendLine(" plus exclusive event penalty");
            if (COMMOD9.KSM > 0.00) COMMOD9.OutmainSB.AppendLine(" with smoothing factor");
            if (COMMOD9.KSH > 0.00) COMMOD9.OutmainSB.AppendLine(" with shrinking factor");
            if (COMMOD9.KSQ > 0.00) COMMOD9.OutmainSB.AppendLine(" with squeezing factor");
            if (COMMOD9.KTS > 0.00) COMMOD9.OutmainSB.AppendLine(" with teasing factor");

            COMMOD9.OutmainSB.AppendLine(string.Format(" RUN TIME (min) = {0:000}", COMMOD9.CPUTM));//HACK:CPUTM DOESN'T CALC

            COMMOD9.OutmainSB.AppendLine("_____________________________");
            COMMOD9.OutmainSB.AppendLine(" THE BEST WEIGHTED PENALTIES:");

            //list primary penalty
            switch (COMMOD9.PENF)
            {
                case -1:
                case 0:
                case 1:
                    if (COMMOD9.PENF == -1)
                    {
                        str = " events";
                        COMMOD9.OutmainSB.AppendLine(" Eventual penalties: ");
                    }
                    else if (COMMOD9.PENF == 0)
                    {
                        str = " meters";
                        COMMOD9.OutmainSB.AppendLine(" Interval penalties: ");
                    }
                    else if (COMMOD9.PENF == 1)
                    {
                        str = " levels";
                        COMMOD9.OutmainSB.AppendLine(" Level penalties: ");
                    }

                    COMMOD9.OutmainSB.AppendLine("Section  Net-Penalty     (Section Statistics   )");

                    for (I = 0; I < COMMOD9.NSCT; I++)
                    {
                        COMMOD9.OutmainSB.AppendLine(
                            string.Format("  {0} {{{1}}} {2:0.00} {3}  ({4:0.00} meters with {5}" +
                            " event levels at mean spacing of {6:0.00} +/- {7:0.00}) ",
                            (I + 1), 
                            //COMMOD9.SECTNICK[I].Substring(0, 3), COMMOD9.SCJPEN[I], str.Trim().Length >= 7 ? str.Trim().Substring(0, 7) : str.Trim(), -- disable FORBIG version
                            COMMOD9.SECTNICK[I].Length >= 3 ? COMMOD9.SECTNICK[I].Substring(0, 3) : COMMOD9.SECTNICK[I], COMMOD9.SCJPEN[I], str.Trim().Length >= 7 ? str.Trim().Substring(0, 7) : str.Trim(),
                            COMMOD9.VALEVEL[COMMOD9.HLEVEL[I], I] - COMMOD9.VALEVEL[0, I],
                            (COMMOD9.HLEVEL[I] + 1), COMMOD9.ZLEVEL[I, 0], COMMOD9.ZLEVEL[I, 1]));

                        if (COMMOD9.SECTPROP[I, 2] > 0)
                        {
                            COMMOD9.OutmainSB.AppendLine(string.Format("{0,23}  ({1} unshared range end(s) at section limit)", "", COMMOD9.SECTPROP[I, 2]));
                        }

                        if (COMMOD9.SECTPROP[I, 1] > 0)
                        {
                            COMMOD9.OutmainSB.AppendLine(string.Format("{0,23}  ({1} complete unshared taxon range(s) at section limit)", "", COMMOD9.SECTPROP[I, 1]));
                        }

                        COMMOD9.OutmainSB.AppendLine(string.Format("{0,23}  ({1} events;     penalty per event: {2:0.00}", "", COMMOD9.ELEVEL[COMMOD9.HLEVEL[I], I],
                            COMMOD9.SCJPEN[I] / (double)(COMMOD9.ELEVEL[COMMOD9.HLEVEL[I], I])));

                        COMMOD9.OutmainSB.AppendLine(string.Format("{0,23}  (rescaled range in best sequence: {1} events)", "", (int)(COMMOD9.VALEVEL[COMMOD9.MXLVL + 1, I])));

                    }

                    COMMOD9.OutmainSB.AppendLine();

                    break;
                case 5:
                case 6:
                    COMMOD9.OutmainSB.AppendLine(" Royal and Sequel penalties ");
                    COMMOD9.OutmainSB.AppendLine(" cannot usefully be subdivided by section.");
                    COMMOD9.OutmainSB.AppendLine();
                    COMMOD9.OutmainSB.AppendLine(" Other penalties calculated on a solution");
                    COMMOD9.OutmainSB.AppendLine(" optimized to royal or sequal penalties ");
                    COMMOD9.OutmainSB.AppendLine(" may appear negative or meaninglessly large, ");
                    COMMOD9.OutmainSB.AppendLine(" especially if the sequence polarity is reversed!");
                    COMMOD9.OutmainSB.AppendLine();
                    COMMOD9.OutmainSB.AppendLine();
                    break;
                case 2:
                case 3:
                case 4:
                    COMMOD9.OutmainSB.AppendLine("Section  Net-Penalty  Possible-Penalty");

                    if (COMMOD9.PENF == 2)
                    {
                        COMMOD9.OutmainSB.AppendLine(" Ordinal penalties: ");
                        for (I = 0; I < COMMOD9.NSCT; I++)
                        {
                            COMMOD9.OutmainSB.AppendLine(string.Format("{0,2}{1} {{{2}}} {3} pairs of {4} possible pairs in {5} section levels i.e. {6:0.0000}",
                                "", (I + 1), COMMOD9.SECTNICK[I], (int)COMMOD9.SCJPEN[I], (COMMOD9.PAIRJ[I] + 1),
                                (COMMOD9.HLEVEL[I] + 1), COMMOD9.SCJPEN[I] / (double)(COMMOD9.PAIRJ[I] + 1)));
                        }
                        COMMOD9.OutmainSB.AppendLine("pairs: number of contradicted pairs in section");

                    }
                    else if (COMMOD9.PENF == 3)
                    {
                        COMMOD9.OutmainSB.AppendLine(" Spatial penalties: ");
                        for (I = 0; I < COMMOD9.NSCT; I++)
                        {
                            //COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2} levels from {3} possible pairs in {4} section levels", (I + 1), COMMOD9.SECTNICK[I].Substring(0, 3), (int)COMMOD9.SCJPEN[I], (COMMOD9.PAIRJ[I] + 1),
                            //    (COMMOD9.HLEVEL[I] + 1)));
                            COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2} levels from {3} possible pairs in {4} section levels", 
                                (I + 1), 
                                COMMOD9.SECTNICK[I].Length >= 3 ? COMMOD9.SECTNICK[I].Substring(0, 3) : COMMOD9.SECTNICK[I], 
                                (int)COMMOD9.SCJPEN[I], 
                                (COMMOD9.PAIRJ[I] + 1),
                                (COMMOD9.HLEVEL[I] + 1)
                                ));
                        }
                        COMMOD9.OutmainSB.AppendLine("levels: number of levels between contradicted pairs");
                    }
                    else if (COMMOD9.PENF == 4)
                    {
                        COMMOD9.OutmainSB.AppendLine(" Rascal penalties: ");
                        for (I = 0; I < COMMOD9.NSCT; I++)
                        {
                            //COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2} contributing pairs from {3} possible pairs in {4} section levels", (I + 1), COMMOD9.SECTNICK[I].Substring(0, 3), (int)COMMOD9.SCJPEN[I], (COMMOD9.PAIRJ[I] + 1),
                            //         (COMMOD9.HLEVEL[I] + 1)));
                            COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2} contributing pairs from {3} possible pairs in {4} section levels", 
                                (I + 1), 
                                COMMOD9.SECTNICK[I].Length >= 3 ? COMMOD9.SECTNICK[I].Substring(0, 3) : COMMOD9.SECTNICK[I], 
                                (int)COMMOD9.SCJPEN[I], 
                                (COMMOD9.PAIRJ[I] + 1),
                                (COMMOD9.HLEVEL[I] + 1)
                            ));
                        }
                        COMMOD9.OutmainSB.AppendLine("contributing pairs: number of contradictions in section;");
                        COMMOD9.OutmainSB.AppendLine("  rascal penalty calculates contradiction ratio");
                        COMMOD9.OutmainSB.AppendLine("  pair-by-pair, not section by section!");
                    }

                    COMMOD9.OutmainSB.AppendLine("possible: number of contradictable pairs in section");
                    COMMOD9.OutmainSB.AppendLine("section levels: number of levels within section");
                    COMMOD9.OutmainSB.AppendLine();

                    break;
            }//End switch PENF

            //list interval penalty, if alternate
            switch (COMMOD9.PENF)
            {
                case 2:
                case 4:
                case 5:
                case 6:
                    COMMOD9.OutmainSB.AppendLine("Interval penalties for comparison");
                    COMMOD9.OutmainSB.AppendLine("using solution optimized as above:");
                    for (I = 0; I < COMMOD9.NSCT; I++)
                    {
                        //COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2:0.00} meters  ({3:0.00} meters with {4} event levels)", (I + 1), COMMOD9.SECTNICK[I].Substring(0, 3), COMMOD9.COLPEN[I],
                        //    COMMOD9.VALEVEL[COMMOD9.HLEVEL[I], I] - COMMOD9.VALEVEL[0, I],
                        //    (COMMOD9.HLEVEL[I] + 1)));
                        COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2:0.00} meters  ({3:0.00} meters with {4} event levels)", 
                            (I + 1), 
                            COMMOD9.SECTNICK[I].Length >= 3 ? COMMOD9.SECTNICK[I].Substring(0, 3) : COMMOD9.SECTNICK[I], COMMOD9.COLPEN[I],
                            COMMOD9.VALEVEL[COMMOD9.HLEVEL[I], I] - COMMOD9.VALEVEL[0, I],
                            (COMMOD9.HLEVEL[I] + 1)
                        ));
                    }
                    COMMOD9.OutmainSB.AppendLine();
                    break;
            }

            //list ordinal penalty, if alternate
            switch (COMMOD9.PENF)
            {
                case -1:
                case 0:
                case 1:
                case 5:
                case 6:
                    COMMOD9.OutmainSB.AppendLine("Ordinal penalties for comparison");
                    COMMOD9.OutmainSB.AppendLine("using solution optimized as above:");
                    if ((COMMOD9.PENF == 5) || (COMMOD9.PENF == 6))
                    {
                        for (I = 0; I < COMMOD9.NSCT; I++)
                        {
                            COMMOD9.OutmainSB.AppendLine(string.Format("{0,2}{1} {{{2}}} {3} pairs of {4} possible pairs in {5} section levels i.e. {6:0.0000}", "", (I + 1), COMMOD9.SECTNICK[I], (int)COMMOD9.SCJPEN[I], (COMMOD9.PAIRJ[I] + 1),
                                    (COMMOD9.HLEVEL[I] + 1), COMMOD9.SCJPEN[I] / (double)(COMMOD9.PAIRJ[I] + 1)));
                        }
                    }
                    else
                    {
                        for (I = 0; I < COMMOD9.NSCT; I++)
                        {
                            //COMMOD9.OutmainSB.AppendLine(string.Format("{0,2}{1} {{{2}}} {3} pairs of {4} possible pairs in {5} section levels i.e. {6:0.0000}", "", (I + 1),
                            //    COMMOD9.SECTNICK[I].Substring(0, 3), (int)COMMOD9.COLPEN[I], (COMMOD9.PAIRJ[I] + 1),
                            //        (COMMOD9.HLEVEL[I] + 1), COMMOD9.COLPEN[I] / (double)(COMMOD9.PAIRJ[I] + 1)));
                            COMMOD9.OutmainSB.AppendLine(string.Format("{0,2}{1} {{{2}}} {3} pairs of {4} possible pairs in {5} section levels i.e. {6:0.0000}", "", 
                                (I + 1),
                                COMMOD9.SECTNICK[I].Length >= 3 ? COMMOD9.SECTNICK[I].Substring(0, 3) : COMMOD9.SECTNICK[I], 
                                (int)COMMOD9.COLPEN[I], 
                                (COMMOD9.PAIRJ[I] + 1),
                                (COMMOD9.HLEVEL[I] + 1), 
                                COMMOD9.COLPEN[I] / (double)(COMMOD9.PAIRJ[I] + 1)
                            ));
                        }
                    }
                    COMMOD9.OutmainSB.AppendLine();
                    break;
            }

            COMMOD9.OutmainSB.AppendLine(" ___________________________________");
            COMMOD9.OutmainSB.AppendLine(string.Format("\r\n TOTAL PENALTY FOR BEST SEQUENCE:   {0:0.0000}", COMMOD9.BSTPEN));
            if (COMMOD9.JSPANF > 0) COMMOD9.OutmainSB.AppendLine("  includes exclusive event term");
            if (COMMOD9.KSM > 0.00) COMMOD9.OutmainSB.AppendLine("  includes smoothing term");
            if (COMMOD9.KSH > 0.00) COMMOD9.OutmainSB.AppendLine("  includes shrinking term");
            if (COMMOD9.KSQ > 0.00) COMMOD9.OutmainSB.AppendLine("  includes squeezing term");
            if (COMMOD9.KTS > 0.00) COMMOD9.OutmainSB.AppendLine("  includes teasing term");
            COMMOD9.OutmainSB.AppendLine(string.Format("\r\n SECONDARY PENALTY FOR BEST SEQUENCE:  {0:0.0000}", COMMOD9.BS2PEN));

            COMMOD9.OutmainSB.AppendLine();
            COMMOD9.OutmainSB.AppendLine("ALL PENALTIES FIT TO BEST SEQUENCE");
            COMMOD9.OutmainSB.AppendLine(string.Format("\r\n    Initial Penalty:   {0:0.0000} meters", COMMOD9.INIPEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("   Interval Penalty:   {0:0.0000} meters", COMMOD9.INTPEN - COMMOD9.BS2PEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("      Level Penalty:   {0:0.0000} levels", COMMOD9.LVLPEN - COMMOD9.BS2PEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("   Eventual Penalty:   {0:0.0000} events", COMMOD9.RVLPEN - COMMOD9.BS2PEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("    Ordinal Penalty:   {0:0.0000} pairs", COMMOD9.ORDPEN - COMMOD9.BS2PEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("    Spatial Penalty:   {0:0.0000} levels", COMMOD9.SPTPEN - COMMOD9.BS2PEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("     Rascal Penalty:   {0:0.0000}", COMMOD9.RSCPEN - COMMOD9.BS2PEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("      Royal Penalty:   {0:0.0000}", COMMOD9.ROYPEN - COMMOD9.BS2PEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("   Momental Penalty:   {0:0.0000}", COMMOD9.MOMPEN - COMMOD9.BS2PEN));
            if (COMMOD9.FB4LF != 0) COMMOD9.OutmainSB.AppendLine(string.Format("     Sequel Penalty:   {0:0.0000}", COMMOD9.SEQPEN - COMMOD9.BS2PEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("   Negative Penalty:   {0:0.0000}", COMMOD9.NGTPEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("    Teasing Penalty:   {0:0.000} ranks", COMMOD9.TSPEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("     Teasing factor:   {0:0.0000}", COMMOD9.KTS));
            COMMOD9.OutmainSB.AppendLine(string.Format("  Shrinking Penalty:   {0:0.0000} ranks", COMMOD9.SHPEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("   Shrinking factor:   {0:0.0000}", COMMOD9.KSH));
            COMMOD9.OutmainSB.AppendLine(string.Format("  Squeezing Penalty:   {0:0.0000} ranks", COMMOD9.SQPEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("   Squeezing factor:   {0:0.0000}", COMMOD9.KSQ));
            COMMOD9.OutmainSB.AppendLine(string.Format("  Smoothing Penalty:   {0:0.0000} meters", COMMOD9.SMPEN));
            COMMOD9.OutmainSB.AppendLine(string.Format("   Smoothing factor:   {0:0.0000}", COMMOD9.KSM));
            COMMOD9.OutmainSB.AppendLine(string.Format("   Net Section Span:   {0:0.0000}", COMMOD9.SPANPEN));

            //CPMS---------------------------------------------------
            //CPMS     Write out some standardized penalty statistics
            //CPMS---------------------------------------------------
            LVLTTL = 0;
            THKTTL = 0;
            NENDS = 0;
            NEXT = 0;
            NPEN = 0;
            NCTR = 0;
            NIMP = 0;
            NEXTS = 0;
            NPENS = 0;

            for (L = COMMOD9.NEVNT - 1; L >= 0; L--)
            {
                GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);
                DELTA = 0.0;
                EVNTPEN = 0.0;
                JSPC = 0;
                NZRO = 0;

                //Loop to total all sections
                for (ISCT = 0; ISCT < COMMOD9.NSCT; ISCT++)
                {
                    GETIJ(COMMOD9.BSTPERM[L], ISCT, out LVLFOUND, out LVLPLACED, out VALFOUND, out VALPLACED);

                    if (L == 0)//<=HXD
                    {
                        LVLTTL += (COMMOD9.HLEVEL[ISCT] + 1);
                        THKTTL += COMMOD9.VALEVEL[COMMOD9.MXLVL, ISCT];
                    }

                    if (LVLFOUND > -1)//<=HXD
                    {
                        NENDS++;
                        if ((LVLFOUND > 0) && (LVLFOUND < COMMOD9.HLEVEL[ISCT]))//<=HXD
                        {
                            NEXT++;
                            if (COMMOD9.RSTATIC[COMMOD9.BSTPERM[L], ISCT, 1] != 0.0) NPEN++;
                            NIMP++;
                        }

                        JSPC++;
                        TDELTA = Math.Abs(VALFOUND - VALPLACED);
                        if (TDELTA > 0.0)
                        {
                            NEXTS++;
                            DELTA += TDELTA;
                            if (ITYPE == 1)
                            {
                                WEIGHT = COMMOD9.RSTATIC[COMMOD9.BSTPERM[L], ISCT, 1];

                                if (WEIGHT > 0) NPENS++;
                                else if (WEIGHT == 0) NZRO++;

                                EVNTPEN += (TDELTA * WEIGHT);
                            }
                            else if (ITYPE == 2)
                            {
                                WEIGHT = COMMOD9.RSTATIC[COMMOD9.BSTPERM[L], ISCT, 0];

                                if (WEIGHT > 0) NPENS++;
                                else if (WEIGHT == 0) NZRO++;

                                EVNTPEN += (TDELTA * WEIGHT);
                            }

                        }
                    }
                }//End for ISCT

                if (JSPC > 1) NCTR += JSPC;
                else if (JSPC == 1) NIMP--;
            }//End for L

            COMMOD9.OutmainSB.AppendLine("\r\n_________________________________________________");
            COMMOD9.OutmainSB.AppendLine("STANDARDIZED MEASURES OF QUALITY OF SOLUTION");
            COMMOD9.OutmainSB.AppendLine(string.Format("           Number of different events: {0,12}", COMMOD9.NEVNT));
            COMMOD9.OutmainSB.AppendLine(string.Format("           Number of exclusive events: {0,12}", COMMOD9.NEXCL));
            COMMOD9.OutmainSB.AppendLine(string.Format("                   Number of sections: {0,12}", COMMOD9.NSCT));
            COMMOD9.OutmainSB.AppendLine(string.Format(" Sections with exclusive event at end: {0,12}", COMMOD9.JEXCL));
            COMMOD9.OutmainSB.AppendLine(string.Format("  Number of local events to be placed: {0,12}", COMMOD9.NSCT * COMMOD9.NEVNT));
            COMMOD9.OutmainSB.AppendLine(string.Format("      Number of observed local events: {0,12}", NENDS));
            COMMOD9.OutmainSB.AppendLine(string.Format("     i.e. number of lines in loadfile: {0,12}", COMMOD9.NOBS));
            COMMOD9.OutmainSB.AppendLine(string.Format("    Number of extensible local events: {0,12}", NEXT));
            COMMOD9.OutmainSB.AppendLine(" (i.e. observed away from section end)");
            COMMOD9.OutmainSB.AppendLine(string.Format("   Number of penalizable local events: {0,12}", NPEN));
            COMMOD9.OutmainSB.AppendLine(" (and carrying non-zero weight factor)");
            COMMOD9.OutmainSB.AppendLine(string.Format("Number of contradictable local events: {0,12}", NCTR));
            COMMOD9.OutmainSB.AppendLine("(i.e. observed in at least 2 sections)");
            COMMOD9.OutmainSB.AppendLine(string.Format("    Number of improvable local events: {0,12}", NIMP));
            COMMOD9.OutmainSB.AppendLine("  (i.e. extensible and contradictable)");
            COMMOD9.OutmainSB.AppendLine(string.Format("      Number of extended local events: {0,12}", NEXTS));
            COMMOD9.OutmainSB.AppendLine(string.Format("     Number of penalized local events: {0,12}", NPENS));
            COMMOD9.OutmainSB.AppendLine(string.Format("        Number of proven coexistences: {0,12}", COMMOD9.NCOEX));
            COMMOD9.OutmainSB.AppendLine(string.Format("     Number of combinations of 2 taxa: {0,12}", COMMOD9.MXCOEX));

            if (COMMOD9.FB4LF == 1)
            {
                COMMOD9.OutmainSB.AppendLine(string.Format("       Number of observed FA\\LA pairs: {0,12}", COMMOD9.NFALA));
            }
            else if (COMMOD9.FB4LF == 0)// =>ELSEIF(FB4LF.eq.1) THEN  IN FORTRAN
            {
                COMMOD9.OutmainSB.AppendLine("       Number of observed FA\\LA pairs not counted");
            }

            COMMOD9.OutmainSB.AppendLine(string.Format(" Penalized fraction of improvable events: {0,9:0.000}", (double)NPENS / (double)NPEN));

            COMMOD9.OutmainSB.AppendLine("(if greater than 1.0, solution is NOT optimal)\r\n");
            COMMOD9.OutmainSB.AppendLine(string.Format(" Total Thickness (all sections): {0}", THKTTL));
            COMMOD9.OutmainSB.AppendLine(string.Format("Number of Levels (all sections): {0}", LVLTTL));
            COMMOD9.OutmainSB.AppendLine(string.Format("          Average Level Spacing: {0:0.00000}", THKTTL / (LVLTTL - COMMOD9.NSCT)));
            COMMOD9.OutmainSB.AppendLine("\r\n____________________________________");
            COMMOD9.OutmainSB.AppendLine("PENALTY USED TO OPTIMIZE SEQUENCE -");
            COMMOD9.OutmainSB.AppendLine(" (with weights and secondaries)");
            if (NENDS > 0) COMMOD9.OutmainSB.AppendLine(string.Format("{0,11}per observed event: {1:0.00000}", "", COMMOD9.BSTPEN / (double)NENDS));
            if (NCTR > 0) COMMOD9.OutmainSB.AppendLine(string.Format("{0,5}per contradictable event: {1:0.00000}", "", COMMOD9.BSTPEN / (double)NCTR));
            if (NPEN > 0) COMMOD9.OutmainSB.AppendLine(string.Format("{0,8}per penalizable event: {1:0.00000}", "", COMMOD9.BSTPEN / (double)NPEN));
            if (NEXT > 0) COMMOD9.OutmainSB.AppendLine(string.Format("{0,9}per extensible event: {1:0.00000}", "", COMMOD9.BSTPEN / (double)NEXT));
            if (NEXTS > 0) COMMOD9.OutmainSB.AppendLine(string.Format("{0,11}per extended event: {1:0.00000}", "", COMMOD9.BSTPEN / (double)NEXTS));
            COMMOD9.OutmainSB.AppendLine();
            if (COMMOD9.PENF < 2)
            {
                COMMOD9.OutmainSB.AppendLine();
                COMMOD9.OutmainSB.AppendLine(string.Format("         MAXIMUM Range Extension: {0}", COMMOD9.MAXPEN));
            }

            TEMPPEN = 0.0;
            for (L = 1; L <= 3; L++)
            {
                COMMOD9.OutmainSB.AppendLine();
                if (L == 1)
                {
                    COMMOD9.OutmainSB.AppendLine(" OPTIMAL  NET  EXTENSIONS  -  ");
                    COMMOD9.OutmainSB.AppendLine(" (no secondary penalties)");
                    COMMOD9.OutmainSB.AppendLine(string.Format("     Measured in DISTANCE: {0}", (COMMOD9.INTPEN - COMMOD9.BS2PEN)));
                    TEMPPEN = COMMOD9.INTPEN;
                }
                else if (L == 2)
                {
                    COMMOD9.OutmainSB.AppendLine();
                    COMMOD9.OutmainSB.AppendLine(string.Format("       Measured in LEVELS: {0}", (COMMOD9.LVLPEN - COMMOD9.BS2PEN)));
                    TEMPPEN = COMMOD9.LVLPEN;
                }
                else if (L == 3)
                {
                    COMMOD9.OutmainSB.AppendLine();
                    COMMOD9.OutmainSB.AppendLine(string.Format("       Measured in EVENTS: {0}", (COMMOD9.RVLPEN - COMMOD9.BS2PEN)));
                    TEMPPEN = COMMOD9.RVLPEN;
                }

                if (NENDS > 0) COMMOD9.OutmainSB.AppendLine(string.Format("{0,11}per observed event: {1:0.00000}", "", (TEMPPEN - COMMOD9.BS2PEN) / (double)NENDS));
                if (NCTR > 0) COMMOD9.OutmainSB.AppendLine(string.Format("{0,5}per contradictable event: {1:0.00000}", "", (TEMPPEN - COMMOD9.BS2PEN) / (double)NCTR));
                if (NPEN > 0) COMMOD9.OutmainSB.AppendLine(string.Format("{0,8}per penalizable event: {1:0.00000}", "", (TEMPPEN - COMMOD9.BS2PEN) / (double)NPEN));
                if (NEXT > 0) COMMOD9.OutmainSB.AppendLine(string.Format("{0,9}per extensible event: {1:0.00000}", "", (TEMPPEN - COMMOD9.BS2PEN) / (double)NEXT));
                if (NEXTS > 0) COMMOD9.OutmainSB.AppendLine(string.Format("{0,11}per extended event: {1:0.00000}", "", (TEMPPEN - COMMOD9.BS2PEN) / (double)NEXTS));

                if ((L == 1) && (THKTTL > 0.0)) COMMOD9.OutmainSB.AppendLine(string.Format(" Net Extension  /Total Thickness: {0:0.000000}", (COMMOD9.INTPEN - COMMOD9.BS2PEN) / THKTTL));
                if ((L == 2) && (LVLTTL > 0.0)) COMMOD9.OutmainSB.AppendLine(string.Format("  Extension Levels/Total Levels: {0:0.000000}", (COMMOD9.LVLPEN - COMMOD9.BS2PEN) / (double)LVLTTL));
            }



        }


        public static void RUNOUT()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            COMMOD9.OutmainSB.AppendLine("______________________________________________");
            COMMOD9.OutmainSB.AppendLine(" THE RUN PARAMETERS\r\n");
            COMMOD9.OutmainSB.AppendLine(" The initial solution - ");
            if (COMMOD9.INIGEN == 1) COMMOD9.OutmainSB.AppendLine(string.Format("  sequence in section: {0}", COMMOD9.JSTART));
            else if (COMMOD9.INIGEN == 3) COMMOD9.OutmainSB.AppendLine("      random sequence  ");
            else if (COMMOD9.INIGEN == 2) COMMOD9.OutmainSB.AppendLine("    from previous run: ");

            if ((COMMOD9.SOLVEF <= 1) || (COMMOD9.SOLVEF == 5) || (COMMOD9.SOLVEF == 7) || (COMMOD9.SOLVEF == 9))
            {
                COMMOD9.OutmainSB.AppendLine(" Annealing Schedule - ");
                COMMOD9.OutmainSB.AppendLine(string.Format("   Initial temperature: {0:0.0000000}", COMMOD9.STARTT));
                COMMOD9.OutmainSB.AppendLine(string.Format("            10% cooled: {0:0.0000000}",
                    COMMOD9.STARTT * Math.Pow(COMMOD9.R, COMMOD9.NOUTER / 10)));
                COMMOD9.OutmainSB.AppendLine(string.Format("            30% cooled: {0:0.00000000}",
                    COMMOD9.STARTT * Math.Pow(COMMOD9.R, 3 * COMMOD9.NOUTER / 10)));
                COMMOD9.OutmainSB.AppendLine(string.Format("            50% cooled: {0:0.00000000}",
                    COMMOD9.STARTT * Math.Pow(COMMOD9.R, 5 * COMMOD9.NOUTER / 10)));
                COMMOD9.OutmainSB.AppendLine(string.Format("            70% cooled: {0:0.000000000}",
                    COMMOD9.STARTT * Math.Pow(COMMOD9.R, 7 * COMMOD9.NOUTER / 10)));
                COMMOD9.OutmainSB.AppendLine(string.Format("            90% cooled: {0:0.000000000}",
                    COMMOD9.STARTT * Math.Pow(COMMOD9.R, 9 * COMMOD9.NOUTER / 10)));
                COMMOD9.OutmainSB.AppendLine(string.Format("     Final temperature: {0:0.000000000}",
                    COMMOD9.STARTT * Math.Pow(COMMOD9.R, COMMOD9.NOUTER)));
                COMMOD9.OutmainSB.AppendLine(string.Format("         Cooling ratio: {0:0.000}", COMMOD9.R));
                COMMOD9.OutmainSB.AppendLine(string.Format("         Cooling steps: {0}", (int)COMMOD9.NOUTER));
                COMMOD9.OutmainSB.AppendLine(string.Format("       Trials per step: {0}", (int)COMMOD9.NINNER));
                COMMOD9.OutmainSB.AppendLine(string.Format("          Total trials: {0}", (int)(COMMOD9.NOUTER * COMMOD9.NINNER)));
                COMMOD9.OutmainSB.AppendLine(string.Format("      Smoothing Factor: {0:0.000}", COMMOD9.KSM));
                COMMOD9.OutmainSB.AppendLine(string.Format("      Squeezing Factor: {0:0.000}", COMMOD9.KSQ));
                COMMOD9.OutmainSB.AppendLine(string.Format("      Shrinking Factor: {0:0.000}", COMMOD9.KSH));
                COMMOD9.OutmainSB.AppendLine(string.Format("        Teasing Factor: {0:0.000}", COMMOD9.KTS));
            }
            else if (COMMOD9.SOLVEF == 2)
            {
                COMMOD9.OutmainSB.AppendLine(" Greedy Algorithm ");
            }
            else if (COMMOD9.SOLVEF == 3)
            {
                COMMOD9.OutmainSB.AppendLine(" Tempering Schedule - ");
                COMMOD9.OutmainSB.AppendLine(string.Format(" {0} quenching cycles of", (int)COMMOD9.NOUTER));
                COMMOD9.OutmainSB.AppendLine(string.Format(" {0} trials)", (int)COMMOD9.NINNER));
                COMMOD9.OutmainSB.AppendLine(string.Format("   Initial temperature up to {0:0.000}", COMMOD9.STARTT));
                COMMOD9.OutmainSB.AppendLine(string.Format(" Final temperature not above {0:0.00000}",
                    COMMOD9.STARTT * Math.Pow(COMMOD9.R, COMMOD9.NOUTER)));
            }
            else if (COMMOD9.SOLVEF == 4)
            {
                COMMOD9.OutmainSB.AppendLine(" Greedy Algorithm with Squeeze");
            }
            else if (COMMOD9.SOLVEF == 6)
            {
                COMMOD9.OutmainSB.AppendLine(" Greedy Algorithm with Shrink");
            }
            else if (COMMOD9.SOLVEF == 8)
            {
                COMMOD9.OutmainSB.AppendLine(" Greedy Algorithm with Tease");
            }

            if (COMMOD9.SOLVEF == 5)
            {
                COMMOD9.OutmainSB.AppendLine(" Anneal with Squeeze");
            }
            else if (COMMOD9.SOLVEF == 7)
            {
                COMMOD9.OutmainSB.AppendLine(" Anneal with Shrink");
            }
            else if (COMMOD9.SOLVEF == 9)
            {
                COMMOD9.OutmainSB.AppendLine(" Anneal with Tease");
            }

            COMMOD9.OutmainSB.AppendLine(" Neighborhood size -    ");

            switch (COMMOD9.NABRGEN)
            {
                case 1:
                    COMMOD9.OutmainSB.AppendLine(" big neighborhood: one random event moved");
                    COMMOD9.OutmainSB.AppendLine("                   to random new position");
                    break;
                case 2:
                    COMMOD9.OutmainSB.AppendLine(" small neighborhood: one pair of adjacent");
                    COMMOD9.OutmainSB.AppendLine("                     events switched");
                    break;
                case 3:
                    COMMOD9.OutmainSB.AppendLine(" double neighborhood: any two random events");
                    COMMOD9.OutmainSB.AppendLine("                      switch position");
                    break;
            }

            COMMOD9.OutmainSB.AppendLine();

            switch (COMMOD9.COXSTF)
            {
                case 3:
                    COMMOD9.OutmainSB.AppendLine(" Observed (sensu stricto) co-existences enforced");
                    break;
                case 2:
                    COMMOD9.OutmainSB.AppendLine(" Observed (sensu lato) co-existences enforced");
                    break;
                case 4:
                    COMMOD9.OutmainSB.AppendLine(" Co-existences not enforced");
                    break;
                case 1:
                    COMMOD9.OutmainSB.AppendLine(" Co-existences enforced from user file");
                    break;
            }


        }

        //C**********************************************************************
        //CPMS     A SUBROUTINE TO WRITE SOLUTION TO 
        //CPMS     SEQUENCING PROBLEM TO MAIN 
        //CPMS     OUTUT FILE
        //C
        //C         PROGRAMMERS:  PETE SADLER 
        //C         LAST UPDATE:  April 3rd 1999
        //C**********************************************************************
        public static void SEQOUT()
        {
            string xtitle = string.Empty;
            int H, L;

            //CPMS---------------------------------------------------------
            //CPMS       write out the global
            //CPMS       SOLUTION TO THE SEQUENCING PROBLEM
            //CPMS         i.e. -- BSTPERM -- with youngest on top!
            //CPMS---------------------------------------------------------

            COMMOD COMMOD9 = COMMOD.Singleton();

            for (H = 1; H <= 3; H++)
            {
                COMMOD9.OutmainSB.AppendLine("______________________________________________");
                COMMOD9.OutmainSB.AppendLine("  THE BEST PERMUTATION FOUND IS:");
                COMMOD9.OutmainSB.AppendLine("  Event:Type   RANK and % above base (rank 1)");
                COMMOD9.OutmainSB.AppendLine("       % permits comparison of different sequences");
                if (H == 2) COMMOD9.OutmainSB.AppendLine("  ------ FAD EVENTS ONLY ------");
                if (H == 3) COMMOD9.OutmainSB.AppendLine("  ------ LAD EVENTS ONLY ------");

                for (L = COMMOD9.NEVNT - 1; L >= 0; L--)
                {
                    Helper.GETEVNT(COMMOD9.BSTPERM[L], ref xtitle, 1, 1, 1, 1, 1, 1);
                    if (((COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 1) && (H != 3)) ||
                        ((COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 2) && (H != 2)) ||
                        (H == 1))
                    {
                        COMMOD9.OutmainSB.AppendLine(string.Format(" {0}: {1}  {2} ({3:0.00}%)  {{{4})",
                            COMMOD9.IROWS[COMMOD9.BSTPERM[L], 0] + 1, COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1], L + 1,
                            (double)(L+1) * 100.0 / (double)COMMOD9.NEVNT, xtitle.Length > 100 ? xtitle.Substring(0, 100) : xtitle));
                    }
                }
            }
        }

        //C**********************************************************************
        //CPMS     A SUBROUTINE TO WRITE SELECTED OUTPUT TO TEXT and DATA FILES
        //CPMS     DURING AND AFTER CORRELATION OF A STRATIGRAPHIC DATA SET
        //CPMS     USING CONOP.FOR
        //C             
        //C         PROGRAMMERS:  PETE SADLER  
        //C         LAST UPDATE:  January 22nd 2005
        //C**********************************************************************
        public static void INCROUT()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            int ISCT, ISPECIES, ITYPE, I, L;
            string[] OUTPUT = new string[COMMOD9.NSCT];
            string xtitle = string.Empty;

            COMMOD9.OutmainSB.AppendLine("\r\n______________________________________________");
            COMMOD9.OutmainSB.AppendLine(" INCREMENTAL RANGE EXTENSIONS AS THICKNESSES, ");
            COMMOD9.OutmainSB.AppendLine(" IN BEST SEQUENCE, BY EVENT NUMBER AND SECTION");
            COMMOD9.OutmainSB.AppendLine(" extension = unweighted interval penalty");
            if (COMMOD9.PENF == 1)
            {
                COMMOD9.OutmainSB.AppendLine("\r\nWARNING: optimized on level penalty!  ");
                COMMOD9.OutmainSB.AppendLine("Range extensions measured in levels are tabulated");
                COMMOD9.OutmainSB.AppendLine("in the next section of this report.  \r\n");
            }
            else if ((COMMOD9.PENF != 0) && (COMMOD9.PENF != 1))
            {
                COMMOD9.OutmainSB.AppendLine("\r\nWARNING:  not optimized on interval penalty!  ");
                COMMOD9.OutmainSB.AppendLine("These range extensions result when best sequence");
                COMMOD9.OutmainSB.AppendLine("is subsequently fit to individual sections.  \r\n");
            }

            //outer loop that considers sections NCOL at a time        
            for (I = 0; I < COMMOD9.NSCT; I += COMMOD9.NCOL)
            {
                xtitle = "  ";

                for (ISCT = I; ISCT < Math.Min(ISCT + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)//<=HXD
                {
                    xtitle = xtitle.Trim() + " " + COMMOD9.SECTNICK[ISCT];
                }

                ISCT = I;

                COMMOD9.OutmainSB.AppendLine(string.Format("\r\nEvent and Extension in Sections{0,3} -{1,3}",
                    ISCT+1, Math.Min(ISCT + COMMOD9.NCOL, COMMOD9.NSCT)));//<=HXD

                COMMOD9.OutmainSB.AppendLine(string.Format("  (sections: {0})", xtitle.Trim()));
                COMMOD9.OutmainSB.AppendLine(string.Format("xxxx - no extension because not seen in section"));
                COMMOD9.OutmainSB.AppendLine(string.Format("neg  - penalty chargable on negative evidence"));
                COMMOD9.OutmainSB.AppendLine(string.Format("this data table, unlabelled, = {0}", COMMOD9.DELTAFILE));
                COMMOD9.OutmainSB.AppendLine("unless optimized on level penalty");

                //inner loop scans all events in best order
                for (L = COMMOD9.NEVNT - 1; L >= 0; L--)
                {
                    GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);
                    Helper.GETEVNT(COMMOD9.BSTPERM[L], ref xtitle, 1, 0, 0, 0, 0, 0);
                    //write out deltas
                    for (ISCT = I; ISCT < Math.Min(I + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)//<=HXD
                    {
                        if ((COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0] == -1) &&//<=HXD
                            (COMMOD9.NEGATF == 0))
                        {
                            OUTPUT[ISCT] = "    xxxx";
                            //try to label the negative penalties!
                        }
                        else if ((COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0] == -1) &&//<=HXD
                        (COMMOD9.NEGATF > 0))
                        {
                            if ((COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], ISCT] != 0) &&//<=HXD
                                (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], ISCT] != COMMOD9.HLEVEL[ISCT]))
                            {
                                OUTPUT[ISCT] = "    neg";
                            }
                            else
                            {
                                OUTPUT[ISCT] = "   xxxx";
                            }
                        }
                        else
                        {
                            OUTPUT[ISCT] = string.Format("{0:0.00}",
                                            Math.Abs(Helper.PLACE(L, ISCT) -
                                            //Math.Abs(COMMOD9.VALEVEL[COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], ISCT], ISCT] -
                                            COMMOD9.VALEVEL[COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0], ISCT]));
                        }
                    }//End for ISCT

                    COMMOD9.OutmainSB.Append(string.Format(" {0,4} {1,3} {2} ", ISPECIES + 1, ITYPE, xtitle.Trim().Length > 12 ? xtitle.Trim().Substring(0, 13) : xtitle.Trim()));
                    for (ISCT = I; ISCT < Math.Min(I + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)
                    {
                        COMMOD9.OutmainSB.Append(string.Format("{0,9} ", OUTPUT[ISCT].Length > 9 ? OUTPUT[ISCT].Substring(0, 9) : OUTPUT[ISCT]));
                    }
                    COMMOD9.OutmainSB.AppendLine();

                }//End for L

            }//End for I

            //CPMS------------------------------------------------------------------- 
            //CPMS      now rewrite the range extensions in level penalty form
            COMMOD9.OutmainSB.AppendLine("\r\n_________________________________________________");
            COMMOD9.OutmainSB.AppendLine(" INCREMENTAL RANGE EXTENSION MEASURED BY LEVELS, ");
            COMMOD9.OutmainSB.AppendLine(" IN BEST SEQUENCE, BY EVENT NUMBER AND SECTION");
            COMMOD9.OutmainSB.AppendLine(" extension = unweighted level penalty");
            if (COMMOD9.PENF == 0)
            {
                COMMOD9.OutmainSB.AppendLine("\r\nWARNING: optimized on interval penalty!  ");
                COMMOD9.OutmainSB.AppendLine("Range extensions measured in thickness intervals");
                COMMOD9.OutmainSB.AppendLine("were in the previous section of this report.  \r\n");
            }
            else if ((COMMOD9.PENF != 0) && (COMMOD9.PENF != 1))
            {
                COMMOD9.OutmainSB.AppendLine("\r\nWARNING:  not optimized on level penalty!  ");
                COMMOD9.OutmainSB.AppendLine("These range extensions result when best sequence");
                COMMOD9.OutmainSB.AppendLine(" fit to individual sections.  \r\n");
            }

            //outer loop that considers sections NCOL at a time
            for (I = 0; I < COMMOD9.NSCT; I += COMMOD9.NCOL)
            {
                xtitle = "";
                for (ISCT = I; ISCT < Math.Min(ISCT + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)
                {
                    xtitle += " " + COMMOD9.SECTNICK[ISCT];
                }

                ISCT = I;

                COMMOD9.OutmainSB.AppendLine(string.Format("\r\nEvent and Extension in Sections {0,3} -{1,3}",
                    ISCT+1, Math.Min(ISCT + COMMOD9.NCOL, COMMOD9.NSCT)));

                COMMOD9.OutmainSB.AppendLine("  (sections: " + xtitle.Trim() + ")");
                COMMOD9.OutmainSB.AppendLine("xxx - no extension because not seen in section");
                COMMOD9.OutmainSB.AppendLine("neg  - penalty chargable on negative evidence");
                COMMOD9.OutmainSB.AppendLine("if optimized on level penalty");
                COMMOD9.OutmainSB.AppendLine(string.Format("this data table, unlabelled, = {0}\r\n", COMMOD9.DELTAFILE));

                //inner loop scans all events in best order
                for (L = COMMOD9.NEVNT - 1; L >= 0; L--)
                {
                    GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);
                    Helper.GETEVNT(COMMOD9.BSTPERM[L], ref xtitle, 1, 0, 0, 0, 0, 0);

                    //write out deltas
                    for (ISCT = I; ISCT < Math.Min(I + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)
                    {
                        if ((COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0] == -1) &&
                            (COMMOD9.NEGATF == 0))
                        {
                            OUTPUT[ISCT] = "    xxx";
                        }
                        else if ((COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0] == -1) &&//try to label the negative penalties!
                            (COMMOD9.NEGATF > 0))
                        {
                            if ((COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], ISCT] != 0) &&
                                (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], ISCT] != COMMOD9.HLEVEL[ISCT]))
                            {
                                OUTPUT[ISCT] = "    neg";
                            }
                            else
                            {
                                OUTPUT[ISCT] = "   xxx";
                            }
                        }
                        else
                        {
                            OUTPUT[ISCT] = string.Format("{0}", Math.Abs(COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], ISCT] -
                                COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0]));
                        }
                    }//End for ISCT

                    COMMOD9.OutmainSB.Append(string.Format(" {0,4} {1,3} {2} ", ISPECIES + 1, ITYPE, xtitle.Trim().Length > 12 ? xtitle.Trim().Substring(0, 13) : xtitle.Trim()));
                    for (ISCT = I; ISCT < Math.Min(I + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)
                    {
                        COMMOD9.OutmainSB.Append(string.Format("{0,9} ", OUTPUT[ISCT].Length > 9 ? OUTPUT[ISCT].Substring(0, 9) : OUTPUT[ISCT]));
                    }
                    COMMOD9.OutmainSB.AppendLine();

                    //Write out delta file, uncommented, for use in other programs
                    if (COMMOD9.DELTAFILE.Substring(0, 3) != "OFF")
                    {
                        for (L = COMMOD9.NEVNT - 1; L >= 0; L--)
                        {
                            if (COMMOD9.PENF == 1)
                            {
                                GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);
                                COMMOD9.DeltaSB.Append(string.Format(" {0,4} {1,3} ", ISPECIES + 1, ITYPE));
                                for (ISCT = 0; ISCT < COMMOD9.NSCT; ISCT++)
                                {
                                    COMMOD9.DeltaSB.Append(string.Format("{0,9} ",
                                        Math.Min(1000000 * (COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0] + 1),
                                        Math.Abs(COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], ISCT] -
                                        COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0]))));
                                }
                                COMMOD9.DeltaSB.AppendLine();
                            }
                            else
                            {
                                //GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);
                                //COMMOD9.DeltaSB.AppendLine(string.Format(" {0,4} {1,3} ", ISPECIES + 1, ITYPE));
                                //for (ISCT = 0; ISCT < COMMOD9.NSCT; ISCT++)
                                //{
                                //    COMMOD9.DeltaSB.Append(string.Format("{0,9:0.00} ",
                                //        Math.Min(1000000.0 * (COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0] + 1),
                                //        Math.Abs(Helper.PLACE(L, ISCT)  -
                                //        COMMOD9.VALEVEL[COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0], ISCT]))));
                                //}
                                //COMMOD9.DeltaSB.AppendLine();
                            }
                        }

                        File.WriteAllText(COMMOD9.DELTAFILE, COMMOD9.DeltaSB.ToString());
                    }

                }

            }
        }

        //C**********************************************************************
        //CPMS     A SUBROUTINE TO WRITE SELECTED OUTPUT TO TEXT and DATA FILES
        //CPMS     DURING AND AFTER CORRELATION OF A STRATIGRAPHIC DATA SET
        //CPMS     USING CONOP.FOR
        //C
        //C         PROGRAMMERS:  PETE SADLER  
        //C         LAST UPDATE:  Jan 25th 2000
        //C*****************************************
        public static void LOCOUT(int smooth)
        {
            double THREE, FIVE, SEVEN, PLACE;
            int ISCT, ISPECIES, ITYPE, L, I;
            string xtitle = "";
            COMMOD COMMOD9 = COMMOD.Singleton();

            switch (smooth)
            {
                case 1:
                    xtitle = "without any";
                    break;
                case 3:
                    //HACK:
                    //OPEN(3, FILE='LOC3.txt', RECL=250)
                    break;
                case 5:
                    //HACK:
                    //xtitle = '   with 5-event'
                    //OPEN(3, FILE='LOC5.txt', RECL=250)
                    break;
                case 7:
                    //HACK:
                    //xtitle = '   with 7-event'
                    //OPEN(3, FILE='LOC7.txt', RECL=250)
                    break;
            }

            //Write out the matrix of alphas and betas
            //
            //outer loop that considers sections NCOL at a time
            for (I = 0; I < COMMOD9.NSCT; I += COMMOD9.NCOL)
            {
                COMMOD9.OutmainSB.AppendLine("_____________________________________________");
                COMMOD9.OutmainSB.AppendLine("  THE LINE OF CORRELATION");
                COMMOD9.OutmainSB.AppendLine(xtitle.Trim() + " moving average\r\n");

                xtitle = "";

                for (ISCT = I; ISCT < Math.Min(ISCT + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)
                {
                    xtitle = xtitle.Trim() + " " + COMMOD9.SECTNICK[ISCT];
                }

                ISCT = I;
                COMMOD9.OutmainSB.AppendLine(string.Format(" Event and Placed Levels in Sections {0,3} -{1,3}",
                    ISCT + 1, Math.Min(ISCT + COMMOD9.NCOL, COMMOD9.NSCT)));
                COMMOD9.OutmainSB.AppendLine("    (sections: " + xtitle.Trim() + ")\r\t");

                if (smooth == 1)
                {
                    COMMOD9.OutmainSB.AppendLine(string.Format("next data table, unlabelled, = {0}", COMMOD9.ALBETFILE));
                }

                //inner loop scans all events in best order
                for (L = COMMOD9.NEVNT - 1; L >= 0; L--)
                {
                    GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);
                    Helper.GETEVNT(COMMOD9.BSTPERM[L], ref xtitle, 1, 0, 0, 0, 0, 0);

                    switch (smooth)
                    {
                        case 1:
                            COMMOD9.OutmainSB.Append(string.Format(" {0,4} {1,3} {2} ", ISPECIES + 1, ITYPE,
                                xtitle.Trim().Length > 11 ? xtitle.Trim().Substring(0, 11) : xtitle.Trim()));
                            for (ISCT = I; ISCT < Math.Min(I + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)
                            {
                                COMMOD9.OutmainSB.Append(string.Format("{0,9:0.00} ", Helper.PLACE(L, ISCT)));
                            }
                            COMMOD9.OutmainSB.AppendLine();
                            break;
                        case 3:
                            //HACK:
                            // WRITE(3,9322) ISPECIES,ITYPE,TRIM(xtitle),
                            //(THREE(L,ISCT), ISCT=I, MIN(I+NCOL-1,NSCT))
                            break;
                        case 5:
                            //HACK:
                            // WRITE(3,9322) ISPECIES,ITYPE,TRIM(xtitle),
                            //(FIVE(L,ISCT), ISCT=I, MIN(I+NCOL-1,NSCT))
                            break;
                        case 7:
                            //HACK:
                            // WRITE(3,9322) ISPECIES,ITYPE,TRIM(xtitle),
                            //(SEVEN(L,ISCT), ISCT=I, MIN(I+NCOL-1,NSCT))
                            break;
                    }
                }

            }//End for I

            if (smooth == 1)
            {
                //Write out the albet file, uncommented, for use in other programs
                if (COMMOD9.ALBETFILE.Substring(0, 3) != "OFF")
                {
                    for (L = COMMOD9.NEVNT - 1; L >= 0; L--)
                    {
                        GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);
                        COMMOD9.AlbetSB.Append(string.Format(" {0,4} {1,3} ", ISPECIES + 1, ITYPE));
                        for (ISCT = 0; ISCT < COMMOD9.NSCT; ISCT++)
                        {
                            COMMOD9.AlbetSB.Append(string.Format("{0,9:0.00} ",
                                COMMOD9.VALEVEL[COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], ISCT], ISCT]));
                        }
                        COMMOD9.AlbetSB.AppendLine();
                    }

                    File.WriteAllText(COMMOD9.ALBETFILE, COMMOD9.AlbetSB.ToString());
                }
            }


        }

        //C**********************************************************************
        //CPMS     A SUBROUTINE TO WRITE the observed levels 
        //CPMS     to the main output file
        //C
        //C         PROGRAMMERS:  PETE SADLER & BILL KEMPLE  
        //C         LAST UPDATE:  June 9th 1997
        //C**********************************************************************
        public static void OBSOUT()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            int ISCT, ISPECIES, ITYPE, L, I;
            double[] ABVALS = new double[COMMOD9.NSCT];
            string[] OUTPUT = new string[COMMOD9.NSCT];
            string xtitle = string.Empty;

            //outer loop that considers sections NCOL at a time
            for (I = 0; I < COMMOD9.NSCT; I += COMMOD9.NCOL)
            {
                COMMOD9.OutmainSB.AppendLine("_____________________________________________");
                COMMOD9.OutmainSB.AppendLine("\r\n OBSERVED HORIZONS:");

                xtitle = string.Empty;
                for (ISCT = I; ISCT < Math.Min(ISCT + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)
                {
                    xtitle = xtitle.Trim() + " " + COMMOD9.SECTNICK[ISCT];
                }

                ISCT = I;

                COMMOD9.OutmainSB.AppendLine(string.Format(" Event and Observed Level in Sections {0,3} -{1,3}",
                    ISCT + 1, Math.Min(ISCT + COMMOD9.NCOL, COMMOD9.NSCT)));

                COMMOD9.OutmainSB.AppendLine("  (sections: " + xtitle.Trim() + ")");
                COMMOD9.OutmainSB.AppendLine(string.Format("next data table, unlabelled = {0}", COMMOD9.ABFILE));

                //inner loop scans all events in best order
                for (L = COMMOD9.NEVNT - 1; L >= 0; L--)
                {
                    GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);
                    Helper.GETEVNT(COMMOD9.BSTPERM[L], ref xtitle, 1, 0, 0, 0, 0, 0);

                    for (ISCT = I; ISCT < Math.Min(I + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)
                    {
                        if (COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0] == -1)
                        {
                            OUTPUT[ISCT] = "  none";
                        }
                        else
                        {
                            OUTPUT[ISCT] = string.Format("{0,9:0.00}", COMMOD9.VALEVEL[COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0], ISCT]);
                        }
                    }

                    COMMOD9.OutmainSB.Append(string.Format(" {0,4} {1,3} {2} ", ISPECIES + 1, ITYPE, xtitle.Trim().Length > 11 ? xtitle.Trim().Substring(0, 11) : xtitle.Trim()));
                    for (ISCT = I; ISCT < Math.Min(I + COMMOD9.NCOL, COMMOD9.NSCT); ISCT++)
                    {
                        COMMOD9.OutmainSB.Append(string.Format("{0,9} ", OUTPUT[ISCT].Trim().Length > 9 ? OUTPUT[ISCT].Trim().Substring(0, 9) : OUTPUT[ISCT].Trim()));
                    }
                    COMMOD9.OutmainSB.AppendLine();

                }
            }

            //CPMS------------------------------------------------------------------- 
            //CPMS     Write out the ab file, uncommented, for use in other programs
            //CPMS
            if (COMMOD9.ABFILE.Substring(0, 3) != "OFF")
            {
                for (L = COMMOD9.NEVNT - 1; L >= 0; L--)
                {
                    Helper.SetVal(ABVALS, 0.0);

                    for (ISCT = 0; ISCT < COMMOD9.NSCT; ISCT++)
                    {
                        if (COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0] > -1)
                        {
                            ABVALS[ISCT] = COMMOD9.VALEVEL[COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], ISCT, 0], ISCT];
                        }

                    }

                    GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);
                    COMMOD9.AbSB.Append(string.Format(" {0,4} {1,3} ", ISPECIES + 1, ITYPE));
                    for (ISCT = 0; ISCT < COMMOD9.NSCT; ISCT++)
                    {
                        COMMOD9.AbSB.Append(string.Format("{0,9:0.00} ", ABVALS[ISCT]));
                    }
                    COMMOD9.AbSB.AppendLine();

                    File.WriteAllText(COMMOD9.ABFILE, COMMOD9.AbSB.ToString());

                }
            }
        }


        //CPMS***********************************
        //CPMS  Returns name of Composite Section
        //C
        //CPMS      Programmer:  Pete Sadler
        //CPMS      Last Modfied: July 27 2000
        //CPMS***********************************
        public static void COMPLBL(int sec, ref string ttl)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            if (sec == COMMOD9.NSCT + 1) ttl = " ORDINAL COMPOSITE ";
            if (sec == COMMOD9.NSCT + 2) ttl = " MEAN STANDARD COMPOSITE ";
            if (sec == COMMOD9.NSCT + 3) ttl = " ZERO-MEAN STD COMPOSITE ";
            if (sec == COMMOD9.NSCT + 4) ttl = " MAXIMUM COMPOSITE ";
            if (sec == COMMOD9.NSCT + 5) ttl = " Z-MAXIMUM COMPOSITE ";
            if (sec == COMMOD9.NSCT + 6) ttl = " AVERAGE COMPOSITE ";
            if (sec == COMMOD9.NSCT + 7) ttl = " MINIMUM COMPOSITE ";
            if (sec == COMMOD9.NSCT + 8) ttl = " ZERO MEAN COMPOSITE ";
            if (sec == COMMOD9.NSCT + 9) ttl = " MAX STANDARD COMPOSITE ";
            if (sec == COMMOD9.NSCT + 10) ttl = " Z-MAX STD COMPOSITE ";

        }

        //***********************************************
        //CPMS     A SUBROUTINE TO WRITE COMPOSITE SECTION
        //CPMS     TO MAIN OUTPUT TEXT FILE
        //CPMS     AND TO FILL THE COMPOSITE COLUMNS IN
        //CPMS     HSCSOL FOR THE GRAPHICAL ROUTINES
        //C
        //C         PROGRAMMERS:  PETE SADLER  
        //C         LAST UPDATE:  January 18th 2000
        public static void COMPOUT()
        {
            string SCORE = string.Empty;
            string xtitle = string.Empty;
            string title = string.Empty;
            int ISPECIES = 0, ITYPE = 0, IMOVES = 0;
            int LVLPLACED = 0, NSLN = 0;
            int COMPS = 0, L = 0, M = 0, J = 0, N = 0;
            double THKTTL = 0.0, VALPLACED = 0.0, DELTA = 0.0;
            double EVENT = 0.0, TYP = 0.0, SECTION = 0.0, VALUE = 0.0, LEVEL = 0.0, RANK = 0.0, WTUP = 0.0, WTDN = 0.0;

            COMMOD COMMOD9 = COMMOD.Singleton();
            if (COMMOD9.CDF == 1) goto Label66;

            if ((COMMOD9.CMPOUTF == 1) && (COMMOD9.OUTMAIN.Substring(0, 3).ToUpper() != "OFF"))
            {
                COMMOD9.OutmainSB.AppendLine("______________________________________________");
                COMPLBL(COMMOD9.NSCT + COMMOD9.COMPF, ref title);
                title = title.Trim() + " SECTION";
                COMMOD9.OutmainSB.AppendLine(title.Trim());

                COMMOD9.OutmainSB.AppendLine(" Event   Type   Space-Below   Level-Placed   Sections     Name");
                //C----   Loop through all events in stratigraphic best sequence

                for (L = 0; L < COMMOD9.NEVNT; L++)
                {
                    DELTA = 0.0;
                    N = 0;
                    GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);
                    Helper.GETEVNT(COMMOD9.BSTPERM[L], ref xtitle, 1, 1, 1, 0, 1, 1);

                    //CPMS     Loop to scan all sections
                    //CPMS     Find maximum of standardized distance between adjacent horizons
                    //CPMS     L and L-1
                    //CPMS     Standardized means divide by total section thickness which is
                    //CPMS     found in array VALEVEL at(MXOTHR+1) this gives all sections
                    //CPMS     a thickness of 1.00
                    //CPMS     Then multiply by the number of event spaces spanned by the section, 
                    //CPMS     i.e the number of events in the problem minus those placed at the 
                    //CPMS     ends of the section.  Now every section has a total thickness equal
                    //CPMS     its span of event spaces.

                    switch (COMMOD9.COMPF)
                    {
                        case 1:
                            DELTA = 1000.0 / (COMMOD9.NEVNT - 1);
                            N = 0;
                            break;
                        case 2:
                            STDSEP(L, ref DELTA, ref N);
                            break;
                        case 3:
                            ZSTSEP(L, ref DELTA, ref N);
                            break;
                        case 4:
                            MAXSEP(L, ref DELTA);
                            N = COMMOD9.NSCT;
                            break;
                        case 5:
                            ZMXSEP(L, ref DELTA);
                            N = COMMOD9.NSCT;
                            break;
                        case 6:
                            AVGSEP(L, ref DELTA, ref N);
                            break;
                        case 7:
                            MINSEP(L, ref DELTA);
                            N = COMMOD9.NSCT;
                            break;
                        case 8:
                            ZROSEP(L, ref DELTA, ref N);
                            break;
                        case 9:
                            STMSEP(L, ref DELTA);
                            N = COMMOD9.NSCT;
                            break;
                        case 10:
                            ZSMSEP(L, ref DELTA);
                            N = COMMOD9.NSCT;
                            break;
                    }

                    //CPMS      Set base to 1000 and total the composite section upward from
                    //CPMS      the base.  The total thickness of the composite is not 
                    //CPMS      fixed, but varies with instance.  The greatest thicknesses 
                    //CPMS      result where
                    //CPMS         1.  there are many events, and
                    //CPMS         2.  the interval of greatest resolution varies from section
                    //CPMS         3.  to section.
                    //CPMS      The compositing method favors the most rapidly accumulating 
                    //CPMS      facies.  It is possible (manually, for now) to extract 
                    //CPMS      composite increments for only one facies.  i.e. we can imagine
                    //CPMS      making a different composite for each facies.
                    //CPMS      Of course, this places a considerable demand on the variability
                    //CPMS      of facies from section to section.
                    //CPMS      To achieve this goal with the current program, one would need 
                    //CPMS      to edit the input sections -- all intervals of "unwanted" facies
                    //CPMS      would need to be collapsed to a single horizon!!
                    //CPMS      -----------------------------------------------

                    if (L == 0)
                    {
                        VALPLACED = 1000.00;
                    }
                    else
                    {
                        VALPLACED += DELTA;
                    }

                    COMMOD9.OutmainSB.AppendLine(string.Format(" {0,6} {1,6} {2,11:0.0000} {3,11:0.0000} {4,6}  {{{5,-100}",
                        ISPECIES + 1, ITYPE, DELTA, VALPLACED, N, xtitle));
                }
            }

            if (COMMOD9.CMPSTFILE.Substring(0, 3) != "OFF") COMMOD9.CmpstSB.Length = 0;

            //Add new composite to Solutions File
            if ((COMMOD9.SLNF != 0) && (COMMOD9.COMPNO > 0))
            {
                NSLN = 1;
                //check to see if file exists already
                if (File.Exists(COMMOD9.SLNFILE) && (File.Exists(COMMOD9.TTLFILE)))
                {

                    //read through file to see how many solutions it contains

                    //TODO:
                    //21        READ(10,*,END=22) EVENT,TYP,SECTION,VALUE,LEVEL,RANK
                    //          NSLN = MAX(NSLN,INT(SECTION)+1)

                    //          GO TO 21
                    //22        BACKSPACE(10)
                }
                else
                {
                    COMMOD9.SlnSB.Length = 0;
                    COMMOD9.TtlSB.Length = 0;
                }

                //CPMS      write the title to TTLFILE
                COMMOD9.TtlSB.AppendLine(string.Format(" {0,3}  -----------", NSLN));
                COMMOD9.TtlSB.AppendLine(string.Format(" {0,60} ", COMMOD9.PROJNAME));
                SCORE = string.Format("{0,7}", (int)COMMOD9.BSTPEN);
                COMMOD9.TtlSB.AppendLine(COMMOD9.PROJTYP1.Trim() + "  " + SCORE.Trim());
                COMMOD9.TtlSB.AppendLine(COMMOD9.PROJTYP2.Trim());
                COMMOD9.TtlSB.AppendLine("----------------------------------------");
            }

        Label66:

            for (COMPS = 1; COMPS <= 10; COMPS++)
            {
                THKTTL = 0.0;
                for (L = 0; L < COMMOD9.NEVNT; L++)
                {
                    //L is position in sequence; BESTPERM(L) is irows row
                    DELTA = 0.0;
                    GETI(COMMOD9.BSTPERM[L], out ISPECIES, out ITYPE);

                    //Correct for latest allowed_move combinations!
                    if (ITYPE < 4)
                    {
                        IMOVES = ITYPE;
                    }
                    else
                    {
                        IMOVES = 0;
                    }

                    WTUP = 1.00;
                    WTDN = 1.00;

                    if (ITYPE == 5)
                    {
                        for (J = 0; J < COMMOD9.NSCT; J++)
                        {
                            if (COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], J, 0] > 0)
                            {
                                WTUP = COMMOD9.RSTATIC[COMMOD9.BSTPERM[L], J, 0];
                                WTDN = COMMOD9.RSTATIC[COMMOD9.BSTPERM[L], J, 1];
                                break;
                            }
                        }
                    }

                    switch (COMPS)
                    {
                        case 1:
                            DELTA = 1000.0 / (COMMOD9.NEVNT - 1);
                            break;
                        case 2:
                            STDSEP(L, ref DELTA, ref N);
                            break;
                        case 3:
                            ZSTSEP(L, ref DELTA, ref N);
                            break;
                        case 4:
                            MAXSEP(L, ref DELTA);
                            break;
                        case 5:
                            ZMXSEP(L, ref DELTA);
                            break;
                        case 6:
                            AVGSEP(L, ref DELTA, ref N);
                            break;
                        case 7:
                            MINSEP(L, ref DELTA);
                            break;
                        case 8:
                            ZROSEP(L, ref DELTA, ref N);
                            break;
                        case 9:
                            STMSEP(L, ref DELTA);
                            break;
                        case 10:
                            ZSMSEP(L, ref DELTA);
                            break;
                    }

                    if (L == 0)
                    {
                        VALPLACED = 1000.00;
                        LVLPLACED = 0;
                    }
                    else
                    {
                        VALPLACED += DELTA;
                        if (DELTA > 0.0) LVLPLACED++;
                    }

                    THKTTL = Math.Max(THKTTL, VALPLACED);
                    //fill composite column in basic data arrays 

                    COMMOD9.ISTATIC[COMMOD9.BSTPERM[L], COMMOD9.NSCT + COMPS - 1, 0] = LVLPLACED;
                    COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], COMMOD9.NSCT + COMPS - 1] = LVLPLACED;

                    //fill HSCTRNG from CULLIST
                    //CULLIST(M,1) is highest position;  CULLIST(M,2) is lowest
                    if ((COMMOD9.RUNGRF >= 4) || (COMMOD9.GRIDF == 1))
                    {
                        for (M = 0; M < COMMOD9.NEVNT; M++)
                        {
                            if (COMMOD9.CULLIST[M, 0] == L)
                            {
                                COMMOD9.HSCTRNG[M, COMMOD9.NSCT + COMPS - 1, 0] = LVLPLACED;
                            }

                            if (COMMOD9.CULLIST[M, 1] == L)
                            {
                                COMMOD9.HSCTRNG[M, COMMOD9.NSCT + COMPS - 1, 1] = LVLPLACED;
                            }
                        }
                    }

                    //composite has its own array of level values
                    COMMOD9.COMPLVL[LVLPLACED, COMPS - 1] = VALPLACED;
                    if (L == COMMOD9.NEVNT - 1)
                    {
                        COMMOD9.HLEVEL[COMMOD9.NSCT + COMPS - 1] = LVLPLACED;
                        COMMOD9.COMPLVL[COMMOD9.NEVNT, COMPS - 1] = THKTTL;
                    }

                    if (COMMOD9.CDF != 1)
                    {
                        if (COMMOD9.COMPF == COMPS)
                        {
                            if (COMMOD9.CMPSTFILE.Substring(0, 3) != "OFF")
                            {
                                COMMOD9.CmpstSB.AppendLine(string.Format(" {0,5} {1,4} {2,5} {3,11:0.00} {4,5} {5,5} {6,7:0.00} {7,7:0.00} ",
                                    ISPECIES + 1, ITYPE, NSLN, VALPLACED, LVLPLACED, IMOVES, WTUP, WTDN));
                            }
                        }

                    }
                }
            }//End for COMPS

            if ((COMMOD9.CMPSTFILE.Substring(0, 3) != "OFF") && (COMMOD9.CDF != 1))
            {
                //CLOSE(17, STATUS='KEEP') //COMMOD9.CmpstSB
            }

            if ((COMMOD9.SLNF != 0) && (COMMOD9.COMPNO > 0) && (COMMOD9.CDF != 1))
            {
                //CLOSE(10,STATUS='KEEP')
                //CLOSE(16,STATUS='KEEP')
            }



            return;
        }

        private static void STDSEP(int L, ref double del, ref int ndel)
        {
            //cpms	find the mean standard separation between adjacent
            //cpms  in the best sequence;  search all sections
            //cpms  standard separation means that the local thickness
            //cpms  is divided by the total section thickness (to 
            //cpms  counter local thickness differences) and then
            //cpms  multiplied by the span of events (to move to
            //cpms  a time-based separation)

            int SCT;
            double tdel;

            tdel = 0.0;
            ndel = 0;

            COMMOD COMMOD9 = COMMOD.Singleton();

            if (L > 0)//<=HXD
            {
                for (SCT = 0; SCT < COMMOD9.NSCT; SCT++)
                {
                    if (COMMOD9.SECTPROP[SCT, 0] == 0) continue;

                    if (COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] > 0.0)
                    {
                        del = (Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT)) /
                            COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] * COMMOD9.VALEVEL[COMMOD9.MXLVL + 1, SCT];
                    }

                    if (del > 0.0)
                    {
                        tdel += del;
                        ndel += 1;
                    }
                }

                if (ndel > 0)
                {
                    del = tdel / ndel;
                }
                else
                {
                    del = 0.0;
                }
            }

        }

        private static void ZSTSEP(int L, ref double del, ref int ndel)
        {
            //cpms	find the mean standard separation between adjacent
            //cpms  in the best sequence;  search all sections
            //cpms  include zero spacing

            int SCT;
            double tdel;

            tdel = 0.0;
            ndel = 0;

            COMMOD COMMOD9 = COMMOD.Singleton();

            if (L > 0)
            {
                for (SCT = 0; SCT < COMMOD9.NSCT; SCT++)
                {
                    if (COMMOD9.SECTPROP[SCT, 0] == 0) continue;

                    if (COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] > 0.0)
                    {
                        del = (Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT)) /
                            COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] * COMMOD9.VALEVEL[COMMOD9.MXLVL + 1, SCT];
                    }

                    if ((COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], SCT] != 0) &&
                        (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L - 1], SCT] != COMMOD9.HLEVEL[SCT]))
                    {
                        tdel += del;
                        ndel++;
                    }
                }

                if (ndel > 0)
                {
                    del = tdel / ndel;
                }
                else
                {
                    del = 0.0;
                }
            }
        }

        private static void MAXSEP(int L, ref double del)
        {
            //cpms  find the largest absolute separation between adjacent
            //cpms  in the best sequence;  search all sections
            //cpms  absolute separation means that the local thickness
            //cpms  is not corrected for the total section thickness (to 
            //cpms  counter local thickness differences) and or for the
            //cpms  the differences in the span of events (to move to
            //cpms  a time-based separation)

            int SCT;

            del = 0.0;
            COMMOD COMMOD9 = COMMOD.Singleton();

            if (L > 0)
            {
                for (SCT = 0; SCT < COMMOD9.NSCT; SCT++)
                {
                    if (COMMOD9.SECTPROP[SCT, 0] == 0) continue;

                    if (COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] > 0.0)
                    {
                        del = Math.Max(del, Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT));
                    }
                }
            }

        }


        private static void ZMXSEP(int L, ref double del)
        {
            //cpms	find the largest z-score separation between adjacent
            //cpms  in the best sequence;  search all sections by z-score
            //cpms  z-score separation means that the local thickness
            //cpms  is corrected for the local mean separation (to 
            //cpms  counter local sampling differences) and or for the
            //cpms  the differences in the standard deviation;  i.e.
            //cpms  tight sampling schemes are favored

            int SCT;
            double zdl, zsc;
            zdl = -9999999.0;
            del = 0.0;

            COMMOD COMMOD9 = COMMOD.Singleton();

            if (L >0)
            {
                for (SCT = 0; SCT < COMMOD9.NSCT; SCT++)
                {
                    if (COMMOD9.ZLEVEL[SCT, 1] == 0.0) continue;
                    if (COMMOD9.SECTPROP[SCT, 0] == 0) continue;

                    if (COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] > 0.0)
                    {
                        zsc = (Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT) - COMMOD9.ZLEVEL[SCT, 0]) / COMMOD9.ZLEVEL[SCT, 1];

                        if (zsc > zdl)
                        {
                            zdl = zsc;
                            del = Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT);
                        }
                    }
                }
            }

        }

        private static void MINSEP(int L, ref double del)
        {
            //cpms	find the largest absolute separation between adjacent
            //cpms  in the best sequence;  search all sections
            //cpms  absolute separation means that the local thickness
            //cpms  is not corrected for the total section thickness (to 
            //cpms  counter local thickness differences) and or for the
            //cpms  the differences in the span of events (to move to
            //cpms  a time-based separation)

            int SCT;
            double mdel = 0;

            del = 99999999.9;

            COMMOD COMMOD9 = COMMOD.Singleton();

            if (L > 0)
            {
                for (SCT = 0; SCT < COMMOD9.NSCT; SCT++)
                {
                    if (COMMOD9.SECTPROP[SCT, 0] == 0) continue;

                    if (COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] > 0.0)
                    {
                        mdel = Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT);
                    }

                    if (mdel > 0) del = Math.Min(mdel, del);
                }
            }

            if (del == 99999999.9) del = 0.0;
        }


        private static void AVGSEP(int L, ref double del, ref int ndel)
        {
            //cpms	find the mean absolute separation between adjacent
            //cpms  events in the best sequence;  search all sections
            //cpms  do not include zero spacings
            //cpms  absolute separation means that the local thickness
            //cpms  is not corrected for the total section thickness (to 
            //cpms  counter local thickness differences) and or for the
            //cpms  the differences in the span of events (to move to
            //cpms  a time-based separation)

            int SCT;
            double tdel;

            tdel = 0.0;
            ndel = 0;

            COMMOD COMMOD9 = COMMOD.Singleton();

            if (L > 0)
            {
                for (SCT = 0; SCT < COMMOD9.NSCT; SCT++)
                {
                    if (COMMOD9.SECTPROP[SCT, 0] == 0) continue;

                    if (COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] > 0.0)
                    {
                        del = Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT);
                    }

                    if (del > 0.0)
                    {
                        tdel += del;
                        ndel++;
                    }

                }

                if (ndel > 0)
                {
                    del = tdel / ndel;
                }
                else
                {
                    del = 0.0;
                }
            }

        }

        private static void ZROSEP(int L, ref double del, ref int ndel)
        {
            //cpms	find the mean absolute separation between adjacent
            //cpms  in the best sequence;  search all sections
            //cpms  include zero spacings as longas they are not
            //cpms  at the limits of the section
            //cpms  absolute separation means that the local thickness
            //cpms  is not corrected for the total section thickness (to 
            //cpms  counter local thickness differences) and or for the
            //cpms  the differences in the span of events (to move to
            //cpms  a time-based separation)

            int SCT;
            double tdel;

            tdel = 0.0;
            ndel = 0;
            COMMOD COMMOD9 = COMMOD.Singleton();

            if (L > 0)
            {
                for (SCT = 0; SCT < COMMOD9.NSCT; SCT++)
                {
                    if (COMMOD9.SECTPROP[SCT, 0] == 0) continue;
                    if (COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] > 0.0)
                    {
                        del = Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT);
                    }

                    if ((COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L], SCT] != 0) &&
                        (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[L - 1], SCT] != COMMOD9.HLEVEL[SCT]))
                    {
                        tdel += del;
                        ndel++;
                    }
                }

                if (ndel > 0)
                {
                    del = tdel / ndel;
                }
                else
                {
                    del = 0.0;
                }
            }
        }

        private static void STMSEP(int L, ref double del)
        {
            //cpms	find the largest standard separation between adjacent
            //cpms  in the best sequence;  search all sections
            //cpms  standard separation means that the local thickness
            //cpms  is divided by the total section thickness (to 
            //cpms  counter local thickness differences) and then
            //cpms  multiplied by the span of events (to move to
            //cpms  a time-based separation)

            int SCT;
            COMMOD COMMOD9 = COMMOD.Singleton();

            if (L > 0)
            {
                for (SCT = 0; SCT < COMMOD9.NSCT; SCT++)
                {
                    if (COMMOD9.SECTPROP[SCT, 0] == 0) continue;
                    if (COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] > 0.0)
                    {
                        del = Math.Max(del, (Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT)) / COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] * COMMOD9.VALEVEL[COMMOD9.MXLVL + 1, SCT]);
                    }
                }
            }
        }

        private static void ZSMSEP(int L, ref double del)
        {
            //cpms	find the largest z-score separation between adjacent
            //cpms  in the best sequence;  search all sections by z-score
            //cpms  z-score separation means that the local thickness
            //cpms  is corrected for the local mean separation (to 
            //cpms  counter local sampling differences) and or for the
            //cpms  the differences in the standard deviation;  i.e.
            //cpms  tight sampling schemes are favored
            //cpms  S means that the section thickness is standardized
            //cpms  for the range of events

            int SCT;
            double zdl, zsc;
            zdl = -9999999.0;
            del = 0.0;
            COMMOD COMMOD9 = COMMOD.Singleton();

            if (L > 0)
            {
                for (SCT = 0; SCT < COMMOD9.NSCT; SCT++)
                {
                    if (COMMOD9.ZLEVEL[SCT, 1] == 0.0) continue;
                    if (COMMOD9.SECTPROP[SCT, 0] == 0) continue;
                    if (COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] > 0.0)
                    {
                        zsc = (Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT) - COMMOD9.ZLEVEL[SCT, 0]) / COMMOD9.ZLEVEL[SCT, 1];

                        if (zsc > zdl)
                        {
                            zdl = zsc;
                            del = Math.Max(del, (Helper.PLACE(L, SCT) - Helper.PLACE(L - 1, SCT)) / COMMOD9.VALEVEL[COMMOD9.MXLVL, SCT] * COMMOD9.VALEVEL[COMMOD9.MXLVL + 1, SCT]);

                        }
                    }
                }
            }
        }


    }
}





