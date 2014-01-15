using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

using CONOP.NET;

namespace CONOP
{
    public class DRAWINCR
    {

        int[] GRAND = new int[10000];
        int FADLVL, LADLVL, IG, KG, GG, RNG;
        int SampleN, COMPFmem;
        int[,] INCRALL;
        int[,] SINCRALL;
        //stores (INCRALL) histogram bar heights for all taxa
        //one row per taxon, in COEX order
        //coex order number is IROWS(I,3)
        //and (SINCRALL) for all sections, one row per section
        //INCRALL() supports rarefaction on observed ranges

        int[,] PLCDALL;
        //PLCDALL() stores full composite ranges for rarefaction

        int[] INCR, SINCR, INCRN, SINCRN;
        //stores histogram bar heights for one taxon or one section
        //used to draw histogram and load one (S)INCRALL row

        double[] INCREAL;
        double[] SINCREAL;
        //stores average support per taxon
        //stores average per section

        double MAXYVAL;
        //MXYVAL stores maxval on y-axis scale

        double RRNG;
        //RRNG is range in composite distance

        bool[,] SPCSCT;
        //tracks occurence of species by section
        //but only as scanned in this process
        //also guards against multiple entries
        //? this should also be a master array from DOINPUT! 

        int dinc, horscl, font, topfont;

        int I, J, K, L, S, M, N, G, H, F, P, Hprev, Slocl, Rich, Jcst;
        //F     totals richness using range-thru
        //I     counts events
        //J     counts composite sections
        //Jcst  user-selected composite scaling
        //S     counts other sections
        //Slocl counts sections that observe event
        //L     counts position in best permutation
        //K     is for short term use
        //G,H,  used to write out INCRALL(NSPC,NEVNT)
        //Hprev is previous value of H
        //N     is sample size for rarefaction 
        //E     midpoint of 11  

        int ISP, stp, lvlover, lvlunder, lvlcover, lvlcunder, dum1, dum2;
        int NLOCL, Nfit, TOPEVT;
        //NLOCL = number of local ranges
        //Nfit  = number of sections that fit on screen
        //lvlover lvlunder = bracketing position in ORD composite
        //lvlcover lvlcunder = bracketing position in custom composite

        int ac, acst, ax, alphx, cmpax, cstax, bc, bcst, bx, betax, cmpbx, cstbx;
        //   ac - bc    = FAD - LAD range in ORD composite
        // acst - bcst  = FAD - LAD range in custom composite (CMPF)
        //   ax - bx    = observed local range
        //alphx - betax = adjusted local range 
        //cmpax - cmpbx = observed range mapped into ORD composite
        //cstax - cstbx = observed range mapped into custom composite (CMPF)

        double scalx, topx, btmx, valx, valcx, val2x, val2cx, valbx, valbcx, valtx, valtcx, valdif;
        //"x" reals refer to local sections; "c" reals refer to composites

        double valc, val2c, valcst, val2cst;
        //valc   ORD composite FAD
        //val2c  ORD composite LAD
        //valcst   custom composite FAD
        //val2cst  custom composite LAD

        double stepup, stepdn;
        string ltr;
        bool upsection, topf, basf;
        //upsection = true if walking upsection
        //topf,basf = true if local range to top, base of section

        //
        //  limits of screen resolution:   maxx  maxy
        //    margins outside graph box:   top  btm  lft  rit
        //

        string Abr;
        string Icon;
        string ntitle, word;
        string xttl;
        string xtitle, ytitle;
        string rareline;
        /////RECORD /xycoord/ xy    <=?????

        //WINDOWS Font Management
        int numfonts, index;
        int horiz, vert;

        public DRAWINCR()
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            INCRALL = new int[COMMOD.Singleton().NSPC, COMMOD.Singleton().NEVNT];
            SINCRALL = new int[COMMOD.Singleton().NSCT, COMMOD.Singleton().NEVNT];
            PLCDALL = new int[COMMOD.Singleton().NSPC, COMMOD.Singleton().NEVNT];
            INCR = new int[COMMOD.Singleton().NEVNT];
            SINCR = new int[COMMOD.Singleton().NEVNT];
            INCRN = new int[COMMOD.Singleton().NEVNT];
            SINCRN = new int[COMMOD.Singleton().NEVNT];
            INCREAL = new double[COMMOD.Singleton().NEVNT];
            SINCREAL = new double[COMMOD.Singleton().NEVNT];
            SPCSCT = new bool[COMMOD.Singleton().NSPC, COMMOD.Singleton().NSCT];

            Helper.SetVal(INCR, 0);
            Helper.SetVal(SINCR, 0);
            Helper.SetVal(INCRALL, 0);
            Helper.SetVal(PLCDALL, 0);
            Helper.SetVal(SINCRALL, 0);

            SampleN = 0;
            Rich = 0;
            //SPCSCT   = false;
            Helper.SetVal(GRAND, 0);

            Icon = "+++++";

            horiz = 0;
            vert = -90;//<=HXD 900 in fortran

            ISP = 0;


            COMMOD9.scl = 0;
            COMMOD9.top = 3;
            COMMOD9.btm = 47;
            COMMOD9.rit = 100;
            COMMOD9.lft = 100;
            ltr = "A";
            //initialize level variables
            ac = 0;
            acst = 0;
            ax = 0;
            alphx = 0;
            cmpax = 0;
            cstax = 0;
            bc = 0;
            bcst = 0;
            bx = 0;
            betax = 0;
            cmpbx = 0;
            cstbx = 0;
            //initialize value variables      
            topx = 0.0;
            btmx = 0.0;
            scalx = 0.0;
            topx = 0.0;
            btmx = 0.0;
            valx = 0.0;
            val2x = 0.0;
            valbx = 0.0;
            valtx = 0.0;
            valdif = 0.0;
            valc = 0.0;
            val2c = 0.0;
            valcst = 0.0;
            val2cst = 0.0;
            valbcx = 0.0;
            valtcx = 0.0;
        }


        public void DO1(frmCONOP frm, Graphics g)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();

            g.DrawString("PROGRAM  WILL  FIRST  SCAN  RAPIDLY  THROUGH  ALL  TAXA.",
                frm.MSG_FONT, frm.MSG_BRUSH, (int)COMMOD9.lft, (int)COMMOD9.btm);

            g.DrawString("THEN PROMPT FOR RAREFACTION CALCULATIONS.",
                frm.MSG_FONT, frm.MSG_BRUSH, (int)COMMOD9.lft, (int)COMMOD9.btm + 25);

            g.DrawString("  After screen returns to first taxon and pauses,",
                frm.MSG_FONT, frm.MSG_BRUSH, (int)COMMOD9.lft, (int)COMMOD9.btm + 60);

            g.DrawString("  use [←] [→] keys to review taxa manually,",
               frm.MSG_FONT, frm.MSG_BRUSH, (int)COMMOD9.lft, (int)COMMOD9.btm + 80);

            g.DrawString("  or use [Esc] key to advance to summary screens",
                frm.MSG_FONT, frm.MSG_BRUSH, (int)COMMOD9.lft, (int)COMMOD9.btm + 100);

            g.DrawString("   [strike [Enter] key to continue]",
                frm.MSG_FONT, frm.MSG_BRUSH, (int)COMMOD9.lft, (int)COMMOD9.btm + 130);

            if (COMMOD9.CONTRACT != 0)
            {
                g.DrawString("  RANGE CONTRACTIONS ALLOWED!     ",
                    frm.MSG_FONT, Brushes.Magenta, (int)COMMOD9.lft, (int)COMMOD9.btm + 220);

                g.DrawString("         - observed ranges may extend beyond composite range",
                    frm.MSG_FONT, Brushes.Magenta, (int)COMMOD9.lft * 2, (int)COMMOD9.btm + 240);

                g.DrawString("         - the histograms and graphs may be compromised",
                    frm.MSG_FONT, Brushes.Magenta, (int)COMMOD9.lft * 2, (int)COMMOD9.btm + 260);
            }



        }


        private void RANGEBARINC(frmCONOP frm, Graphics g, Pen pen, double v1x, double v2x, int Jx, int Jy, double btx, double sclx, int flg, bool bsf, bool tpf)
        {
            //Jx = section number; Jy = section total
            //flg=0: local range
            //flg=1: composite range (thicker line; grey limit bars) 	
            //bsf, tpf = flags for ranges to base and top of section	

            pen.Width = 2;
            COMMOD COMMOD9 = COMMOD.Singleton();

            //draw range
            if (((bsf) || (tpf)) && (flg == 0)) { pen.DashStyle = System.Drawing.Drawing2D.DashStyle.Dash; }//CALL  SETLINESTYLE(INT2(#8888))

            int xpos = Math.Min((int)(COMMOD9.lft + ((v1x - btx) * sclx) - 1), (int)(COMMOD9.lft + ((v2x - btx) * sclx) - 1));
            int ypos = Math.Min((int)(COMMOD9.top + Jx * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(Jy + 2)),
                (int)(flg + COMMOD9.top + Jx * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(Jy + 2)));

            if (flg == 0)
            {
                g.DrawLine(pen, (int)(COMMOD9.lft + ((v1x - btx) * sclx) - 1),
                (int)(COMMOD9.top + Jx * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(Jy + 2)),
                (int)(COMMOD9.lft + ((v2x - btx) * sclx) - 1),
                (int)(flg + COMMOD9.top + Jx * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(Jy + 2)));
            }
            else
            {

                g.DrawRectangle(pen, xpos, ypos,
                    Math.Abs((int)((v2x - v1x) * sclx + 2)), Math.Abs((int)(flg)));
            }



            //CALL SETLINESTYLE(INT2(#FFFF))
            pen.DashStyle = System.Drawing.Drawing2D.DashStyle.Solid;

            //draw range ends (if within section)
            if (bsf == false)
            {

                g.DrawRectangle(pen, (int)(COMMOD9.lft + ((v1x - btx) * sclx) - 1),
                    (int)(COMMOD9.top - 1 + Jx * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(Jy + 2)),
                    2, Math.Abs((int)(flg + 2)));
            }

            if (tpf == false)
            {
                g.DrawRectangle(pen, (int)(COMMOD9.lft + ((v2x - btx) * sclx) - 1),
                   (int)(COMMOD9.top - 1 + Jx * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(Jy + 2)),
                   2, Math.Abs((int)(flg + 2)));
            }

            pen.Width = 1;

            //draw vertical grey bars at composite range limit
            if (flg == 1)
            {
                //g.DrawRectangle(frm.DRAW_PEN_7, (int)(COMMOD9.lft + ((v1x - btx) * sclx) - 1),
                //    (int)(COMMOD9.top),
                //    0, Math.Abs((int)(10 + Jx * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(Jy + 2))));

                g.DrawLine(frm.DRAW_PEN_7, (int)(COMMOD9.lft + ((v1x - btx) * sclx) - 1), (int)(COMMOD9.top),
                    (int)(COMMOD9.lft + ((v1x - btx) * sclx) - 1),
                    (int)(10 + COMMOD9.top + Jx * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(Jy + 2)));

                g.DrawLine(frm.DRAW_PEN_7, (int)(COMMOD9.lft + ((v2x - btx) * sclx) - 1), (int)(COMMOD9.top),
                    (int)(COMMOD9.lft + ((v2x - btx) * sclx) - 1),
                    (int)(10 + COMMOD9.top + Jx * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(Jy + 2)));

            }

            

        }


        public void BeforeDO2(frmCONOP frm, Graphics g)
        {
            try
            {
                COMMOD COMMOD9 = COMMOD.Singleton();

                //set up two loops
                //first loop scans automatically through all taxa
                //second loop is under manual control
                //Note: outer loop is not indented
                for (M = 1; M <= 1; M++)
                {
                    
                        font = 1;
                        topfont = 1;
                        dinc = 0;
                        horscl = 0;
                        COMMOD9.XEVNT = 0;//<=HXD?

                        I = 0;//<=HXD?
                        J = 0;//<=HXD?
                        K = 0;//<=HXD?
                        S = 1;//<=HXD?<=HXD
                        L = 0;//<=HXD?

                        Nfit = 50;

                        //CPMS---------------------------------------------------------
                        //CPMS  Consider only the composite sections
                        //cpms  go to the first taxon in the best permutation
                        //c     do not change composite section until requested by {+/-}
                        I = COMMOD9.BSTPERM[0];

                        //CPMS  do only FAD/MAXs and unpaired events
                        //CPMS  find the highest non-LAD/MIN it replaces NEVNT
                        //cpms  as the limiting upsection scan  
                        K = COMMOD9.NEVNT - 1;
                        while ((COMMOD9.IROWS[COMMOD9.BSTPERM[K], 1] == 2) || (COMMOD9.IROWS[COMMOD9.BSTPERM[K], 1] == 12))
                        {
                            K = K - 1;
                        }
                        TOPEVT = K;

                        COMPFmem = COMMOD9.COMPF;
                        COMMOD9.COMPF = 1;  // over-ride COMPF for duration of this routine
                        J = COMMOD9.NSCT - 1 + COMMOD9.COMPF ;//<=HXD
                        Jcst = COMMOD9.NSCT - 1 + COMPFmem;////<=HXD

                        if (M == 1) { }
                    

                    while (J > COMMOD9.NSCT - 1)//<=HXD
                    {                       

                        g.Clear(frm.BackColor);

                        topf = false;
                        basf = false;

                        //CPMS-----draw the horizontal section bar--------------
                        g.FillRectangle(frm.MSG_BRUSH_7, COMMOD9.lft, COMMOD9.maxy - COMMOD9.btm,
                            COMMOD9.maxx - COMMOD9.rit - COMMOD9.lft, 10);

                        g.DrawRectangle(frm.DRAW_PEN, COMMOD9.lft, COMMOD9.maxy - COMMOD9.btm,
                            COMMOD9.maxx - COMMOD9.rit - COMMOD9.lft, 10);

                        //CPMS-----scale the section--------------------------
                        topx = COMMOD9.COMPLVL[COMMOD9.HLEVEL[J], J - COMMOD9.NSCT];//?
                        btmx = 1000;
                        scalx = (COMMOD9.maxx - COMMOD9.lft - COMMOD9.rit) / (topx - btmx);

                        //CPMS-----mark the collection levels--------------
                        for (K = 0; K <= COMMOD9.HLEVEL[J]; K++)
                        {
                            valx = COMMOD9.COMPLVL[K, J - COMMOD9.NSCT];

                            g.DrawLine(frm.DRAW_PEN, (int)(COMMOD9.lft + ((valx - btmx) * scalx)), (int)(COMMOD9.maxy - COMMOD9.btm + 10),
                                (int)(COMMOD9.lft + ((valx - btmx) * scalx)), (int)(COMMOD9.maxy - COMMOD9.btm + 13));
                        }

                        //CPMS  a needless mirror in the custom composite
                        for (K = 0; K <= COMMOD9.HLEVEL[Jcst]; K++)
                        {
                            valcx = COMMOD9.COMPLVL[K, Jcst - COMMOD9.NSCT];
                        }


                        //CPMS-----draw the composite event range--------------
                        I = COMMOD9.BSTPERM[L];
                        S = COMMOD9.NSCT + 2 - 1;
                        //label the current event on the graph
                        EVNTLBLINC(frm, g, I, J);

                        if (((COMMOD9.IROWS[I, 1] > 2) && (COMMOD9.IROWS[I, 1] < 11)) || (COMMOD9.IROWS[I, 1] < 1))
                        {
                            //CPMS  if the event was observed, draw black dash 
                            if (COMMOD9.ISTATIC[I, J, 0] > -1)//<=HXD
                            {
                                valx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[I, J, 0], J - COMMOD9.NSCT];
                                valcx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[I, Jcst, 0], Jcst - COMMOD9.NSCT];
                                RANGEBARINC(frm, g, frm.DRAW_PEN, valx, valx, (S + 1), COMMOD9.NSCT, btmx, scalx, 1, basf, topf);//<=HXD
                            }
                        }
                        else if (COMMOD9.IROWS[I, 1] == 1)//IF a FAD (oldest of a stretching pair)	
                        {
                            ac = COMMOD9.ISTATIC[I, J, 0];
                            bc = COMMOD9.ISTATIC[COMMOD9.IROWS[I, 3], J, 0];
                            acst = COMMOD9.ISTATIC[I, Jcst, 0];
                            bcst = COMMOD9.ISTATIC[COMMOD9.IROWS[I, 3], Jcst, 0];

                            if (ac > -1)
                            {
                                //If observed! this is a composite; must be observed!
                                //store composite range
                                valc = COMMOD9.COMPLVL[ac, J - COMMOD9.NSCT];
                                val2c = COMMOD9.COMPLVL[bc, J - COMMOD9.NSCT];
                                valdif = val2c - valc;
                                valcst = COMMOD9.COMPLVL[ac, Jcst - COMMOD9.NSCT];
                                val2cst = COMMOD9.COMPLVL[bc, Jcst - COMMOD9.NSCT];

                                //draw black range
                                RANGEBARINC(frm, g, frm.DRAW_PEN, valc, val2c, (S + 1), COMMOD9.NSCT, btmx, scalx, 1, basf, topf);//<=HXD
                            }
                        }
                        else if (COMMOD9.IROWS[I, 1] == 11)//IF a MAX (older of a shrinking pair)
                        {
                            ac = COMMOD9.ISTATIC[I, J, 0];
                            bc = COMMOD9.ISTATIC[COMMOD9.IROWS[I, 3], J, 0];
                            acst = COMMOD9.ISTATIC[I, Jcst, 0];
                            bcst = COMMOD9.ISTATIC[COMMOD9.IROWS[I, 3], Jcst, 0];

                            if (ac > -1)
                            {
                                //If observed! this is a composite; must be observed!
                                //store composite range
                                valc = COMMOD9.COMPLVL[ac, J - COMMOD9.NSCT];
                                val2c = COMMOD9.COMPLVL[bc, J - COMMOD9.NSCT];
                                valdif = val2c - valc;
                                valcst = COMMOD9.COMPLVL[ac, Jcst - COMMOD9.NSCT];
                                val2cst = COMMOD9.COMPLVL[bc, Jcst - COMMOD9.NSCT];

                                //draw black range
                                RANGEBARINC(frm, g, frm.DRAW_PEN, valc, val2c, (S + 1), COMMOD9.NSCT, btmx, scalx, 1, basf, topf);//<=HXD

                            }
                        }


                        g.DrawString("COMPOSITE", frm.MSG_FONT_S, frm.MSG_BRUSH,
                            6, (int)(COMMOD9.top - 6 + (S + 1) * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(COMMOD9.NSCT + 2)));//<=HXD


                        //----------------------------------------
                        //draw the individual section event ranges

                        //count the number of range bars
                        //if NSCT is large
                        if (COMMOD9.NSCT <= Nfit)
                        {
                            NLOCL = COMMOD9.NSCT;
                            Slocl = COMMOD9.NSCT;
                        }
                        else
                        {
                            Slocl = 0;
                            NLOCL = 0;

                            for (S = 0; S < COMMOD9.NSCT; S++)
                            {
                                if (COMMOD9.ISTATIC[I, S, 0] > -1) NLOCL++;
                            }
                        }

                        for (S = 0; S < COMMOD9.NSCT; S++)//START-DO-S-loop  sections 1 to NSCT
                        {
                            topf = false;
                            basf = false;
                            // If unpaired MID
                            // IF(IROWS(I,2).eq.3) THEN
                            //  do nothing; should not get this far 
                            //  should already have responded to prior cycle 
                            //   CYCLE casues trouble - continuous looping
                            //ELSEIF unpaired (not FAD-LAD or MAX-MIN)

                            if (((COMMOD9.IROWS[I, 1] > 2) && (COMMOD9.IROWS[I, 1] < 11)) || (COMMOD9.IROWS[I, 1] < 1))
                            {
                                //if the event was observed, draw black dash 
                                if (COMMOD9.ISTATIC[I, S, 0] == -1)
                                {
                                    continue;
                                }
                                else
                                {
                                    if (COMMOD9.NSCT > Nfit)
                                    {
                                        Slocl++;
                                    }
                                    else
                                    {
                                        Slocl = S + 1;//<=?
                                    }

                                    valx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[I, J, 0], J - COMMOD9.NSCT];
                                    val2x = valx;
                                    valcx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[I, Jcst, 0], Jcst - COMMOD9.NSCT];
                                    val2cx = valcx;

                                    RANGEBARINC(frm, g, frm.DRAW_PEN_12, valx, val2x, Slocl, NLOCL, btmx, scalx, 0, basf, topf);

                                }
                            }
                            else if ((COMMOD9.IROWS[I, 1] == 1) || (COMMOD9.IROWS[I, 1] == 11)) //IF a FAD or a MAX		  
                            {
                                ax = COMMOD9.ISTATIC[I, S, 0];
                                if (ax <= -1) continue;//<=HXD
                                if (COMMOD9.NSCT > Nfit)
                                {
                                    Slocl++;
                                }
                                else
                                {
                                    Slocl = S + 1;//<=?
                                }

                                alphx = (int)(COMMOD9.HSCTSOL[I, S]);
                                //locate observed LAD/MIN level 
                                bx = COMMOD9.ISTATIC[COMMOD9.IROWS[I, 3], S, 0];
                                betax = (int)(COMMOD9.HSCTSOL[COMMOD9.IROWS[I, 3], S]);

                                //Examine possible three possible configurations 
                                //for each end of observed range
                                //__________
                                //FIND CMPAX 
                                if (ax == alphx)
                                {
                                    //CASE 1: FAD/MAX not adjusted; i.e. on LOC
                                    cmpax = ac;
                                    cstax = acst;
                                    valx = COMMOD9.COMPLVL[cmpax, J - COMMOD9.NSCT];
                                    valcx = COMMOD9.COMPLVL[cstax, Jcst - COMMOD9.NSCT];
                                }
                                else
                                {
                                    for (K = 0; K < COMMOD9.NEVNT + 1; K++)
                                    {
                                        if (K == L) continue;
                                        if (K > COMMOD9.NEVNT - 1) break;

                                        if ((COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], S, 0] == ax) &&
                                            (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] == ax))
                                        {
                                            //----CASE 2. FAD/MAX adjusted, another event at FAD/MAX-level is unadjusted)
                                            cmpax = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                            cstax = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                            valx = COMMOD9.COMPLVL[cmpax, J - COMMOD9.NSCT];
                                            valcx = COMMOD9.COMPLVL[cstax, Jcst - COMMOD9.NSCT];
                                            break;
                                        }
                                    }

                                    if (K > COMMOD9.NEVNT - 1)
                                    {
                                        //----CASE 3: FAD/MAX adjusted; no event at FA/MAXD-level lies on LOC
                                        //place level between next lower and higher composite levels
                                        //divide according to number of events in between?
                                        //try half-way first
                                        lvlunder = 0;
                                        lvlover = 0;
                                        lvlcunder = 0;
                                        lvlcover = 0;

                                        for (K = 0; K < COMMOD9.NEVNT; K++)
                                        {
                                            if (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] < ax)
                                            {
                                                lvlunder = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                                lvlcunder = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                            }
                                            else if (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] > ax)
                                            {
                                                lvlover = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                                lvlcover = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                                cmpax = (lvlunder + lvlover + 2) / 2;//<=?
                                                cstax = (lvlcunder + lvlcover + 2) / 2;//<=?
                                                valx = (COMMOD9.COMPLVL[lvlunder, J - COMMOD9.NSCT] +
                                                    COMMOD9.COMPLVL[lvlover, J - COMMOD9.NSCT]) / 2;
                                                valcx = (COMMOD9.COMPLVL[lvlcunder, Jcst - COMMOD9.NSCT] +
                                                    COMMOD9.COMPLVL[lvlcover, Jcst - COMMOD9.NSCT]) / 2;

                                                break;
                                            }
                                        }

                                        if (K > COMMOD9.NEVNT - 1) continue;

                                    }
                                }

                                //CASE BASE: FA/MAXD at section base
                                //would be placed at composite default by default
                                //move to age of section base
                                if (ax == 0)//attempt to place at composite age of section base      
                                {
                                    basf = true;
                                    //DOES NOT YET WORK CONSISTENTLY [see also LIMITS() in CONOP.FOR]
                                    cmpax = COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 0], J, 0];
                                    cstax = COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 0], Jcst, 0];
                                    valbx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 0], J, 0], J - COMMOD9.NSCT];
                                    valbcx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 0], Jcst, 0], Jcst - COMMOD9.NSCT];
                                }

                                //__________
                                //FIND CMPBX 
                                if (bx == betax)
                                {
                                    //----CASE 4: LAD/MIN not adjusted; i.e. on LOC
                                    cmpbx = bc;
                                    cstbx = bcst;
                                    val2x = COMMOD9.COMPLVL[cmpbx, J - COMMOD9.NSCT];
                                    val2cx = COMMOD9.COMPLVL[cstbx, Jcst - COMMOD9.NSCT];
                                }
                                else
                                {
                                    for (K = COMMOD9.NEVNT - 1; K >= -1; K--)
                                    {
                                        if (K == COMMOD9.IROWS[L, 3]) continue;
                                        if (K < 0) break;
                                        if ((COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], S, 0] == bx) &&
                                            (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] == bx))
                                        {
                                            //----CASE 5: LAD/MIN adjusted, another event at LAD/MIN-level is unadjusted)
                                            cmpbx = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                            cstbx = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                            val2x = COMMOD9.COMPLVL[cmpbx, J - COMMOD9.NSCT];
                                            val2cx = COMMOD9.COMPLVL[cstbx, Jcst - COMMOD9.NSCT];
                                            break;
                                        }
                                    }

                                    if (K < 0)
                                    {
                                        //----CASE 6: LAD/MIN adjusted; no event at LAD/MIN-level lies on LOC
                                        //place level between next lower and higher composite levels
                                        //divide according to number of events in between?
                                        //try half-way first
                                        lvlunder = COMMOD9.HLEVEL[J];
                                        lvlover = COMMOD9.HLEVEL[J];
                                        lvlcunder = COMMOD9.HLEVEL[J];
                                        lvlcover = COMMOD9.HLEVEL[J];

                                        for (K = COMMOD9.NEVNT - 1; K >= 0; K--)
                                        {
                                            if (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] > bx)
                                            {
                                                lvlover = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                                lvlcover = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                            }
                                            else if (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] < bx)
                                            {
                                                lvlunder = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                                lvlcunder = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                                cmpbx = (lvlunder + lvlover + 2) / 2;//<=?
                                                cstbx = (lvlcunder + lvlcover + 2) / 2;//<=?
                                                val2x = (COMMOD9.COMPLVL[lvlunder, J - COMMOD9.NSCT] +
                                                    COMMOD9.COMPLVL[lvlover, J - COMMOD9.NSCT]) / 2;
                                                val2cx = (COMMOD9.COMPLVL[lvlcunder, Jcst - COMMOD9.NSCT] +
                                                    COMMOD9.COMPLVL[lvlcover, Jcst - COMMOD9.NSCT]) / 2;
                                                break;
                                            }
                                        }

                                        if (K < 0) continue;
                                    }
                                }

                                //CASE TOP. LAD/MIN at section top
                                if (bx == COMMOD9.HLEVEL[S])
                                {
                                    topf = true;
                                    //attempt to place at composite age of section top
                                    //DOES NOT YET WORK CONSISTENTLY [see also LIMITS() in CONOP.FOR] 
                                    cmpbx = COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 1], J, 0];
                                    cstbx = COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 1], Jcst, 0];
                                    valtx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 1], J, 0], J - COMMOD9.NSCT];
                                    valtcx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 1], Jcst, 0], Jcst - COMMOD9.NSCT];
                                }

                                //clean up the open-ended top
                                if (topf)
                                {
                                    if (valtx < valx)
                                    {
                                        val2x = valx;
                                    }
                                    else
                                    {
                                        val2x = valtx;
                                    }

                                    if (valtcx < valcx)
                                    {
                                        val2cx = valcx;
                                    }
                                    else
                                    {
                                        val2cx = valtcx;
                                    }
                                }

                                //clean up the open-ended base
                                if (basf)
                                {
                                    if (valbx > val2x)
                                    {
                                        valx = val2x;
                                    }
                                    else
                                    {
                                        valx = valbx;
                                    }

                                    if (valbcx > val2cx)
                                    {
                                        valcx = val2cx;
                                    }
                                }

                                //Set default Icon and adjust if needed
                                Icon = "+++++";
                                if (basf && topf)
                                {
                                    Icon = "<+++>";
                                }
                                else if (basf)
                                {
                                    Icon = "<++++";
                                }
                                else if (topf)
                                {
                                    Icon = "++++>";
                                }

                                if (COMMOD9.IROWS[I, 1] == 1)
                                {// red for local taxon ranges
                                    RANGEBARINC(frm, g, frm.DRAW_PEN_12, valx, val2x, Slocl, NLOCL, btmx, scalx, 0, basf, topf);
                                }
                                else if (COMMOD9.IROWS[I, 1] == 11)
                                {// dark red for local uncertainty intervals
                                    RANGEBARINC(frm, g, frm.DRAW_PEN_4, valx, val2x, Slocl, NLOCL, btmx, scalx, 0, basf, topf);
                                }
                                else
                                {//grey for unpaired events (default)
                                    RANGEBARINC(frm, g, frm.DRAW_PEN_8, valx, val2x, Slocl, NLOCL, btmx, scalx, 0, basf, topf);
                                }

                                if ((COMMOD9.IROWS[I, 1] != 11) && (COMMOD9.IROWS[I, 1] != 12))
                                {
                                    // - should not be necessry to check for 12 (MIN)
                                    //load FAD and LAD positions to GRAND

                                    if ((M == 1) && (J == (COMMOD9.NSCT + COMMOD9.COMPF - 1)))
                                    {
                                        valdif = val2c - valc;
                                        //proportional distance of local FAD up into range
                                        //scaled to 100 units and based at 100
                                        if (valx < valc)
                                        {
                                            IG = 99;
                                        }
                                        else if (valx == valc)
                                        {
                                            IG = 100;
                                        }
                                        else if (valx == val2c)
                                        {
                                            IG = 200;
                                        }
                                        else if (valx > val2c)
                                        {
                                            IG = 201;
                                        }
                                        else
                                        {
                                            IG = (int)((double)(valx - valc) * 100.0 / (double)(valdif)) + 100;
                                        }

                                        if (!basf) GRAND[IG]++;
                                        //proportional distance of local LAD up into range
                                        //scaled to 100 units and based at 300

                                        if (val2x < valc)
                                        {
                                            KG = 299;
                                        }
                                        else if (val2x == valc)
                                        {
                                            KG = 300;
                                        }
                                        else if (val2x == val2c)
                                        {
                                            KG = 400;
                                        }
                                        else if (val2x > val2c)
                                        {
                                            KG = 401;
                                        }
                                        else
                                        {
                                            KG = 400 - (int)((double)(val2c - val2x) * 100.0 / (double)(valdif));
                                        }

                                        if (!topf) GRAND[KG]++;
                                    }
                                }
                            }


                            g.DrawString(COMMOD9.SECTNICK[S].Trim(), frm.MSG_FONT_S, frm.MSG_BRUSH,
                                (int)(COMMOD9.lft / 2),
                                (int)(COMMOD9.top - 6 + Slocl * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(NLOCL + 2)));

                            //add current section to the increment array
                            //by taxon and by section  
                            //NOT for MAX-MIN pairs

                            if ((COMMOD9.IROWS[I, 1] != 11) && (COMMOD9.IROWS[I, 1] != 12))
                            {
                                for (K = Math.Min(cmpax, cmpbx); K <= Math.Max(cmpax, cmpbx); K++)
                                {
                                    INCR[K]++;
                                    if (!(SPCSCT[COMMOD9.IROWS[I, 2], S])) SINCRALL[S, K]++;
                                    //note - because we loop though all sections for each taxon
                                    //INCR can be loaded to INCRALL later to avoid duplicates
                                    //SINCRALL must track duplicates differently because no section
                                    //is complete until all taxa are completed; hence use of LOGICAL
                                    //array SPCSCT(), which is a good generic array and might be moved
                                    //to CONMOD for access by all routines.                       
                                }

                                //note: loop through entire partial range before setting SPCSCT flag!
                                SPCSCT[COMMOD9.IROWS[I, 2], S] = true;
                            }
                        }//END-DO-S-loop  sections 1 to NSCT
                        //---------------------------------------

                        //draw the histogram of section increments 
                        //and while looping, take opportunity to
                        //load the taxon row in the increment array for all taxa
                        //this will rewrite any existing row entry
                        //and thus avoid adding multiple passes to same row total
                        for (K = 0; K < COMMOD9.NEVNT; K++)
                        {
                            //plot histogram for FAD_LAD pairs only 
                            //NOT  MAX-MIN pairs; they do not appear in COEX order!!
                            if (COMMOD9.IROWS[I, 1] != 1) continue;

                            if (COMMOD9.IROWS[I, 1] == 1) INCRALL[COMMOD9.IROWS[I, 2], K] = (int)(INCR[K]);

                            //incrall stores only FAD-LAD paired events
                            //incrall 1 row per taxon, in coex order, 
                            //hence (IROWS,3) 

                            if (INCR[K] == 0) continue;

                            valx = COMMOD9.COMPLVL[K, J - COMMOD9.NSCT];
                            stepup = (COMMOD9.COMPLVL[Math.Min(COMMOD9.NEVNT - 1, K + 1), J - COMMOD9.NSCT] +
                                COMMOD9.COMPLVL[K, J - COMMOD9.NSCT]) / 2.00;
                            stepdn = (COMMOD9.COMPLVL[K, J - COMMOD9.NSCT] +
                                COMMOD9.COMPLVL[Math.Max(0, K - 1), J - COMMOD9.NSCT]) / 2.00;

                            int xpos = Math.Min((int)(COMMOD9.lft + ((stepup - btmx) * scalx)), (int)(COMMOD9.lft + ((stepdn - btmx) * scalx)));
                            int ypos = Math.Min((int)(COMMOD9.maxy - COMMOD9.btm - 1),
                                (int)((COMMOD9.maxy - COMMOD9.btm - 1) - (((COMMOD9.btm * 4) - 10) * INCR[K] / Helper.MAXVal(INCR))));

                            //scale histogram bar to number of counts in INCR()
                            //and to space between composite sections and composite range
                            g.FillRectangle(frm.MSG_BRUSH_CCCCFF, xpos, ypos,
                                Math.Abs((int)(COMMOD9.lft + ((stepup - btmx) * scalx)) - (int)(COMMOD9.lft + ((stepdn - btmx) * scalx))),
                                Math.Abs((int)(-(((COMMOD9.btm * 4) - 10) * INCR[K] / Helper.MAXVal(INCR)))));
                        }

                        g.DrawString("  DISTRIBUTION", frm.MSG_FONT_S, frm.MSG_BRUSH_3333FF,
                            6, (int)(COMMOD9.maxy - COMMOD9.btm - 30));

                        g.DrawString("  OF SUPPORT", frm.MSG_FONT_S, frm.MSG_BRUSH_3333FF,
                            6, (int)(COMMOD9.maxy - COMMOD9.btm - 15));


                        // ----------------------------------------
                        //do titles
                        Output.COMPLBL(J + 1, ref xtitle);
                        xtitle = xtitle.Trim() + ": composite range in black";

                        g.RotateTransform(horiz);
                        g.DrawString(xtitle, frm.MSG_FONT, frm.MSG_BRUSH,
                            (int)(COMMOD9.lft + (COMMOD9.maxx - COMMOD9.lft) / 2),
                            (int)(COMMOD9.maxy - COMMOD9.btm / 2));

                        if (M == 1)
                        {
                            g.DrawString("WAIT FOR SCAN CYCLE TO COMPLETE", frm.MSG_FONT, frm.MSG_BRUSH,
                            (int)(COMMOD9.lft),
                            (int)(COMMOD9.btm));
                        }

                        g.ResetTransform();

                        stp = 1;

                        K = L;
                        upsection = true;

                        if (M == 1) L++;
                        if ((M == 1) && (L > TOPEVT))
                        {
                            Helper.SetVal(INCR, 0);
                            break;
                        }

                        if (L <= 0)
                        {
                            break;
                        }                       

                        if (L < K)
                        {
                            while ((COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 2) || (COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 12))
                            {
                                L = L - 1;
                                if (L < 0) L = TOPEVT;
                                upsection = false;
                            }
                        }
                        else if (L > K)
                        {
                            while ((COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 2) || (COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 12))
                            {
                                L = L + 1;
                                if (L > TOPEVT)
                                    L = 0;
                            }
                        }

                        //evtDelta = L;


                        Helper.SetVal(INCR, 0);
                    }


                }//End for M


                
            }
            catch
            { }
        }

        public void DO2(frmCONOP frm, Graphics g, ref int JDelta, ref int evtDelta)
        {
            try
            {
                COMMOD COMMOD9 = COMMOD.Singleton();
                Helper.SetVal(INCR, 0);
                //set up two loops
                //first loop scans automatically through all taxa
                //second loop is under manual control
                //Note: outer loop is not indented
                for (M = 2; M <= 2; M++)
                {
                    if (evtDelta == 0)
                    {
                        font = 1;
                        topfont = 1;
                        dinc = 0;
                        horscl = 0;
                        COMMOD9.XEVNT = 0;//<=HXD?

                        I = 0;//<=HXD?
                        J = 0;//<=HXD?
                        K = 0;//<=HXD?
                        S = 1;//<=HXD?<=HXD
                        L = 0;//<=HXD?

                        Nfit = 50;

                        //CPMS---------------------------------------------------------
                        //CPMS  Consider only the composite sections
                        //cpms  go to the first taxon in the best permutation
                        //c     do not change composite section until requested by {+/-}
                        I = COMMOD9.BSTPERM[0];

                        //CPMS  do only FAD/MAXs and unpaired events
                        //CPMS  find the highest non-LAD/MIN it replaces NEVNT
                        //cpms  as the limiting upsection scan  
                        K = COMMOD9.NEVNT - 1;
                        while ((COMMOD9.IROWS[COMMOD9.BSTPERM[K], 1] == 2) || (COMMOD9.IROWS[COMMOD9.BSTPERM[K], 1] == 12))
                        {
                            K = K - 1;
                        }
                        TOPEVT = K;

                        COMPFmem = COMMOD9.COMPF;
                        COMMOD9.COMPF = 1;  // over-ride COMPF for duration of this routine
                        J = COMMOD9.NSCT - 1 + COMMOD9.COMPF+JDelta;//<=HXD
                        Jcst = COMMOD9.NSCT - 1 + COMPFmem;////<=HXD

                        if (M == 1) { }
                    }

                    if (J > COMMOD9.NSCT - 1)//<=HXD
                    {

                        if (evtDelta != 0)
                        {
                            K = L;
                            L = evtDelta;

                            if (L < K)
                            {
                                while ((COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 2) || (COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 12))
                                {
                                    L = L - 1;
                                    if (L < 0) L = TOPEVT;
                                    upsection = false;
                                }
                            }
                            else if (L > K)
                            {
                                while ((COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 2) || (COMMOD9.IROWS[COMMOD9.BSTPERM[L], 1] == 12))
                                {
                                    L = L + 1;
                                    if (L > TOPEVT)
                                        L = 0;
                                }
                            }

                            evtDelta = L;
                        }




                        g.Clear(frm.BackColor);

                        topf = false;
                        basf = false;

                        //CPMS-----draw the horizontal section bar--------------
                        g.FillRectangle(frm.MSG_BRUSH_7, COMMOD9.lft, COMMOD9.maxy - COMMOD9.btm,
                            COMMOD9.maxx - COMMOD9.rit - COMMOD9.lft, 10);

                        g.DrawRectangle(frm.DRAW_PEN, COMMOD9.lft, COMMOD9.maxy - COMMOD9.btm,
                            COMMOD9.maxx - COMMOD9.rit - COMMOD9.lft, 10);

                        //CPMS-----scale the section--------------------------
                        topx = COMMOD9.COMPLVL[COMMOD9.HLEVEL[J], J - COMMOD9.NSCT];//?
                        btmx = 1000;
                        scalx = (COMMOD9.maxx - COMMOD9.lft - COMMOD9.rit) / (topx - btmx);

                        //CPMS-----mark the collection levels--------------
                        for (K = 0; K <= COMMOD9.HLEVEL[J]; K++)
                        {
                            valx = COMMOD9.COMPLVL[K, J - COMMOD9.NSCT];

                            g.DrawLine(frm.DRAW_PEN, (int)(COMMOD9.lft + ((valx - btmx) * scalx)), (int)(COMMOD9.maxy - COMMOD9.btm + 10),
                                (int)(COMMOD9.lft + ((valx - btmx) * scalx)), (int)(COMMOD9.maxy - COMMOD9.btm + 13));
                        }

                        //CPMS  a needless mirror in the custom composite
                        for (K = 0; K <= COMMOD9.HLEVEL[Jcst]; K++)
                        {
                            valcx = COMMOD9.COMPLVL[K, Jcst - COMMOD9.NSCT];
                        }


                        //CPMS-----draw the composite event range--------------
                        I = COMMOD9.BSTPERM[L];
                        S = COMMOD9.NSCT + 2 - 1;
                        //label the current event on the graph
                        EVNTLBLINC(frm, g, I, J);

                        if (((COMMOD9.IROWS[I, 1] > 2) && (COMMOD9.IROWS[I, 1] < 11)) || (COMMOD9.IROWS[I, 1] < 1))
                        {
                            //CPMS  if the event was observed, draw black dash 
                            if (COMMOD9.ISTATIC[I, J, 0] > -1)//<=HXD
                            {
                                valx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[I, J, 0], J - COMMOD9.NSCT];
                                valcx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[I, Jcst, 0], Jcst - COMMOD9.NSCT];
                                RANGEBARINC(frm, g, frm.DRAW_PEN, valx, valx, (S + 1), COMMOD9.NSCT, btmx, scalx, 1, basf, topf);//<=HXD
                            }
                        }
                        else if (COMMOD9.IROWS[I, 1] == 1)//IF a FAD (oldest of a stretching pair)	
                        {
                            ac = COMMOD9.ISTATIC[I, J, 0];
                            bc = COMMOD9.ISTATIC[COMMOD9.IROWS[I, 3], J, 0];
                            acst = COMMOD9.ISTATIC[I, Jcst, 0];
                            bcst = COMMOD9.ISTATIC[COMMOD9.IROWS[I, 3], Jcst, 0];

                            if (ac > -1)
                            {
                                //If observed! this is a composite; must be observed!
                                //store composite range
                                valc = COMMOD9.COMPLVL[ac, J - COMMOD9.NSCT];
                                val2c = COMMOD9.COMPLVL[bc, J - COMMOD9.NSCT];
                                valdif = val2c - valc;
                                valcst = COMMOD9.COMPLVL[ac, Jcst - COMMOD9.NSCT];
                                val2cst = COMMOD9.COMPLVL[bc, Jcst - COMMOD9.NSCT];

                                //draw black range
                                RANGEBARINC(frm, g, frm.DRAW_PEN, valc, val2c, (S + 1), COMMOD9.NSCT, btmx, scalx, 1, basf, topf);//<=HXD
                            }
                        }
                        else if (COMMOD9.IROWS[I, 1] == 11)//IF a MAX (older of a shrinking pair)
                        {
                            ac = COMMOD9.ISTATIC[I, J, 0];
                            bc = COMMOD9.ISTATIC[COMMOD9.IROWS[I, 3], J, 0];
                            acst = COMMOD9.ISTATIC[I, Jcst, 0];
                            bcst = COMMOD9.ISTATIC[COMMOD9.IROWS[I, 3], Jcst, 0];

                            if (ac > -1)
                            {
                                //If observed! this is a composite; must be observed!
                                //store composite range
                                valc = COMMOD9.COMPLVL[ac, J - COMMOD9.NSCT];
                                val2c = COMMOD9.COMPLVL[bc, J - COMMOD9.NSCT];
                                valdif = val2c - valc;
                                valcst = COMMOD9.COMPLVL[ac, Jcst - COMMOD9.NSCT];
                                val2cst = COMMOD9.COMPLVL[bc, Jcst - COMMOD9.NSCT];

                                //draw black range
                                RANGEBARINC(frm, g, frm.DRAW_PEN, valc, val2c, (S + 1), COMMOD9.NSCT, btmx, scalx, 1, basf, topf);//<=HXD

                            }
                        }


                        g.DrawString("COMPOSITE", frm.MSG_FONT_S, frm.MSG_BRUSH,
                            6, (int)(COMMOD9.top - 6 + (S + 1) * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(COMMOD9.NSCT + 2)));//<=HXD


                        //----------------------------------------
                        //draw the individual section event ranges

                        //count the number of range bars
                        //if NSCT is large
                        if (COMMOD9.NSCT <= Nfit)
                        {
                            NLOCL = COMMOD9.NSCT;
                            Slocl = COMMOD9.NSCT;
                        }
                        else
                        {
                            Slocl = 0;
                            NLOCL = 0;

                            for (S = 0; S < COMMOD9.NSCT; S++)
                            {
                                if (COMMOD9.ISTATIC[I, S, 0] > -1) NLOCL++;
                            }
                        }

                        for (S = 0; S < COMMOD9.NSCT; S++)//START-DO-S-loop  sections 1 to NSCT
                        {
                            topf = false;
                            basf = false;
                            // If unpaired MID
                            // IF(IROWS(I,2).eq.3) THEN
                            //  do nothing; should not get this far 
                            //  should already have responded to prior cycle 
                            //   CYCLE casues trouble - continuous looping
                            //ELSEIF unpaired (not FAD-LAD or MAX-MIN)

                            if (((COMMOD9.IROWS[I, 1] > 2) && (COMMOD9.IROWS[I, 1] < 11)) || (COMMOD9.IROWS[I, 1] < 1))
                            {
                                //if the event was observed, draw black dash 
                                if (COMMOD9.ISTATIC[I, S, 0] == -1)
                                {
                                    continue;
                                }
                                else
                                {
                                    if (COMMOD9.NSCT > Nfit)
                                    {
                                        Slocl++;
                                    }
                                    else
                                    {
                                        Slocl = S + 1;//<=?
                                    }

                                    valx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[I, J, 0], J - COMMOD9.NSCT];
                                    val2x = valx;
                                    valcx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[I, Jcst, 0], Jcst - COMMOD9.NSCT];
                                    val2cx = valcx;

                                    RANGEBARINC(frm, g, frm.DRAW_PEN_12, valx, val2x, Slocl, NLOCL, btmx, scalx, 0, basf, topf);

                                }
                            }
                            else if ((COMMOD9.IROWS[I, 1] == 1) || (COMMOD9.IROWS[I, 1] == 11)) //IF a FAD or a MAX		  
                            {
                                ax = COMMOD9.ISTATIC[I, S, 0];
                                if (ax <= -1) continue;//<=HXD
                                if (COMMOD9.NSCT > Nfit)
                                {
                                    Slocl++;
                                }
                                else
                                {
                                    Slocl = S+1;//<=?
                                }

                                alphx = (int)(COMMOD9.HSCTSOL[I, S]);
                                //locate observed LAD/MIN level 
                                bx = COMMOD9.ISTATIC[COMMOD9.IROWS[I, 3], S, 0];
                                betax = (int)(COMMOD9.HSCTSOL[COMMOD9.IROWS[I, 3], S]);

                                //Examine possible three possible configurations 
                                //for each end of observed range
                                //__________
                                //FIND CMPAX 
                                if (ax == alphx)
                                {
                                    //CASE 1: FAD/MAX not adjusted; i.e. on LOC
                                    cmpax = ac;
                                    cstax = acst;
                                    valx = COMMOD9.COMPLVL[cmpax, J - COMMOD9.NSCT];
                                    valcx = COMMOD9.COMPLVL[cstax, Jcst - COMMOD9.NSCT];
                                }
                                else
                                {
                                    for (K = 0; K < COMMOD9.NEVNT + 1; K++)
                                    {
                                        if (K == L) continue;
                                        if (K > COMMOD9.NEVNT - 1) break;

                                        if ((COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], S, 0] == ax) &&
                                            (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] == ax))
                                        {
                                            //----CASE 2. FAD/MAX adjusted, another event at FAD/MAX-level is unadjusted)
                                            cmpax = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                            cstax = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                            valx = COMMOD9.COMPLVL[cmpax, J - COMMOD9.NSCT];
                                            valcx = COMMOD9.COMPLVL[cstax, Jcst - COMMOD9.NSCT];
                                            break;
                                        }
                                    }

                                    if (K > COMMOD9.NEVNT - 1)
                                    {
                                        //----CASE 3: FAD/MAX adjusted; no event at FA/MAXD-level lies on LOC
                                        //place level between next lower and higher composite levels
                                        //divide according to number of events in between?
                                        //try half-way first
                                        lvlunder = 0;
                                        lvlover = 0;
                                        lvlcunder = 0;
                                        lvlcover = 0;

                                        for (K = 0; K < COMMOD9.NEVNT; K++)
                                        {
                                            if (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] < ax)
                                            {
                                                lvlunder = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                                lvlcunder = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                            }
                                            else if (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] > ax)
                                            {
                                                lvlover = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                                lvlcover = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                                cmpax = (lvlunder + lvlover + 2) / 2;//<=?
                                                cstax = (lvlcunder + lvlcover + 2) / 2;//<=?
                                                valx = (COMMOD9.COMPLVL[lvlunder, J - COMMOD9.NSCT] +
                                                    COMMOD9.COMPLVL[lvlover, J - COMMOD9.NSCT]) / 2;
                                                valcx = (COMMOD9.COMPLVL[lvlcunder, Jcst - COMMOD9.NSCT] +
                                                    COMMOD9.COMPLVL[lvlcover, Jcst - COMMOD9.NSCT]) / 2;

                                                break;
                                            }
                                        }

                                        if (K > COMMOD9.NEVNT - 1) continue;

                                    }
                                }

                                //CASE BASE: FA/MAXD at section base
                                //would be placed at composite default by default
                                //move to age of section base
                                if (ax == 0)//attempt to place at composite age of section base      
                                {
                                    basf = true;
                                    //DOES NOT YET WORK CONSISTENTLY [see also LIMITS() in CONOP.FOR]
                                    cmpax = COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 0], J, 0];
                                    cstax = COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 0], Jcst, 0];
                                    valbx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 0], J, 0], J - COMMOD9.NSCT];
                                    valbcx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 0], Jcst, 0], Jcst - COMMOD9.NSCT];
                                }

                                //__________
                                //FIND CMPBX 
                                if (bx == betax)
                                {
                                    //----CASE 4: LAD/MIN not adjusted; i.e. on LOC
                                    cmpbx = bc;
                                    cstbx = bcst;
                                    val2x = COMMOD9.COMPLVL[cmpbx, J - COMMOD9.NSCT];
                                    val2cx = COMMOD9.COMPLVL[cstbx, Jcst - COMMOD9.NSCT];
                                }
                                else
                                {
                                    for (K = COMMOD9.NEVNT - 1; K >= -1; K--)
                                    {
                                        if (K == COMMOD9.IROWS[L, 3]) continue;
                                        if (K < 0) break;
                                        if ((COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], S, 0] == bx) &&
                                            (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] == bx))
                                        {
                                            //----CASE 5: LAD/MIN adjusted, another event at LAD/MIN-level is unadjusted)
                                            cmpbx = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                            cstbx = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                            val2x = COMMOD9.COMPLVL[cmpbx, J - COMMOD9.NSCT];
                                            val2cx = COMMOD9.COMPLVL[cstbx, Jcst - COMMOD9.NSCT];
                                            break;
                                        }
                                    }

                                    if (K < 0)
                                    {
                                        //----CASE 6: LAD/MIN adjusted; no event at LAD/MIN-level lies on LOC
                                        //place level between next lower and higher composite levels
                                        //divide according to number of events in between?
                                        //try half-way first
                                        lvlunder = COMMOD9.HLEVEL[J];
                                        lvlover = COMMOD9.HLEVEL[J];
                                        lvlcunder = COMMOD9.HLEVEL[J];
                                        lvlcover = COMMOD9.HLEVEL[J];

                                        for (K = COMMOD9.NEVNT-1; K >= 0; K--)
                                        {
                                            if (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] > bx)
                                            {
                                                lvlover = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                                lvlcover = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                            }
                                            else if (COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], S] < bx)
                                            {
                                                lvlunder = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], J, 0];
                                                lvlcunder = COMMOD9.ISTATIC[COMMOD9.BSTPERM[K], Jcst, 0];
                                                cmpbx = (lvlunder + lvlover + 2) / 2;//<=?
                                                cstbx = (lvlcunder + lvlcover + 2) / 2;//<=?
                                                val2x = (COMMOD9.COMPLVL[lvlunder, J - COMMOD9.NSCT] +
                                                    COMMOD9.COMPLVL[lvlover, J - COMMOD9.NSCT]) / 2;
                                                val2cx = (COMMOD9.COMPLVL[lvlcunder, Jcst - COMMOD9.NSCT] +
                                                    COMMOD9.COMPLVL[lvlcover, Jcst - COMMOD9.NSCT]) / 2;
                                                break;
                                            }
                                        }

                                        if (K < 0) continue;
                                    }
                                }

                                //CASE TOP. LAD/MIN at section top
                                if (bx == COMMOD9.HLEVEL[S])
                                {
                                    topf = true;
                                    //attempt to place at composite age of section top
                                    //DOES NOT YET WORK CONSISTENTLY [see also LIMITS() in CONOP.FOR] 
                                    cmpbx = COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 1], J, 0];
                                    cstbx = COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 1], Jcst, 0];
                                    valtx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 1], J, 0], J - COMMOD9.NSCT];
                                    valtcx = COMMOD9.COMPLVL[COMMOD9.ISTATIC[COMMOD9.LIMITS[S, 1], Jcst, 0], Jcst - COMMOD9.NSCT];
                                }

                                //clean up the open-ended top
                                if (topf)
                                {
                                    if (valtx < valx)
                                    {
                                        val2x = valx;
                                    }
                                    else
                                    {
                                        val2x = valtx;
                                    }

                                    if (valtcx < valcx)
                                    {
                                        val2cx = valcx;
                                    }
                                    else
                                    {
                                        val2cx = valtcx;
                                    }
                                }

                                //clean up the open-ended base
                                if (basf)
                                {
                                    if (valbx > val2x)
                                    {
                                        valx = val2x;
                                    }
                                    else
                                    {
                                        valx = valbx;
                                    }

                                    if (valbcx > val2cx)
                                    {
                                        valcx = val2cx;
                                    }
                                }

                                //Set default Icon and adjust if needed
                                Icon = "+++++";
                                if (basf && topf)
                                {
                                    Icon = "<+++>";
                                }
                                else if (basf)
                                {
                                    Icon = "<++++";
                                }
                                else if (topf)
                                {
                                    Icon = "++++>";
                                }

                                if (COMMOD9.IROWS[I, 1] == 1)
                                {// red for local taxon ranges
                                    RANGEBARINC(frm, g, frm.DRAW_PEN_12, valx, val2x, Slocl, NLOCL, btmx, scalx, 0, basf, topf);
                                }
                                else if (COMMOD9.IROWS[I, 1] == 11)
                                {// dark red for local uncertainty intervals
                                    RANGEBARINC(frm, g, frm.DRAW_PEN_4, valx, val2x, Slocl, NLOCL, btmx, scalx, 0, basf, topf);
                                }
                                else
                                {//grey for unpaired events (default)
                                    RANGEBARINC(frm, g, frm.DRAW_PEN_8, valx, val2x, Slocl, NLOCL, btmx, scalx, 0, basf, topf);
                                }

                                if ((COMMOD9.IROWS[I, 1] != 11) && (COMMOD9.IROWS[I, 1] != 12))
                                {
                                    // - should not be necessry to check for 12 (MIN)
                                    //load FAD and LAD positions to GRAND

                                    if ((M == 1) && (J == (COMMOD9.NSCT + COMMOD9.COMPF - 1)))
                                    {
                                        valdif = val2c - valc;
                                        //proportional distance of local FAD up into range
                                        //scaled to 100 units and based at 100
                                        if (valx < valc)
                                        {
                                            IG = 99;
                                        }
                                        else if (valx == valc)
                                        {
                                            IG = 100;
                                        }
                                        else if (valx == val2c)
                                        {
                                            IG = 200;
                                        }
                                        else if (valx > val2c)
                                        {
                                            IG = 201;
                                        }
                                        else
                                        {
                                            IG = (int)((double)(valx - valc) * 100.0 / (double)(valdif)) + 100;
                                        }

                                        if (!basf) GRAND[IG]++;
                                        //proportional distance of local LAD up into range
                                        //scaled to 100 units and based at 300

                                        if (val2x < valc)
                                        {
                                            KG = 299;
                                        }
                                        else if (val2x == valc)
                                        {
                                            KG = 300;
                                        }
                                        else if (val2x == val2c)
                                        {
                                            KG = 400;
                                        }
                                        else if (val2x > val2c)
                                        {
                                            KG = 401;
                                        }
                                        else
                                        {
                                            KG = 400 - (int)((double)(val2c - val2x) * 100.0 / (double)(valdif));
                                        }

                                        if (!topf) GRAND[KG]++;
                                    }
                                }
                            }


                            g.DrawString(COMMOD9.SECTNICK[S].Trim(), frm.MSG_FONT_S, frm.MSG_BRUSH,
                                (int)(COMMOD9.lft / 2),
                                (int)(COMMOD9.top - 6 + Slocl * (COMMOD9.maxy - COMMOD9.top - (COMMOD9.btm * 5)) / (double)(NLOCL + 2)));

                            //add current section to the increment array
                            //by taxon and by section  
                            //NOT for MAX-MIN pairs

                            if ((COMMOD9.IROWS[I, 1] != 11) && (COMMOD9.IROWS[I, 1] != 12))
                            {
                                for (K = Math.Min(cmpax, cmpbx); K <= Math.Max(cmpax, cmpbx); K++)
                                {
                                    INCR[K]++;
                                    if (!(SPCSCT[COMMOD9.IROWS[I, 2], S])) SINCRALL[S, K]++;
                                    //note - because we loop though all sections for each taxon
                                    //INCR can be loaded to INCRALL later to avoid duplicates
                                    //SINCRALL must track duplicates differently because no section
                                    //is complete until all taxa are completed; hence use of LOGICAL
                                    //array SPCSCT(), which is a good generic array and might be moved
                                    //to CONMOD for access by all routines.                       
                                }

                                //note: loop through entire partial range before setting SPCSCT flag!
                                SPCSCT[COMMOD9.IROWS[I, 2], S] = true;
                            }
                        }//END-DO-S-loop  sections 1 to NSCT
                        //---------------------------------------

                        //draw the histogram of section increments 
                        //and while looping, take opportunity to
                        //load the taxon row in the increment array for all taxa
                        //this will rewrite any existing row entry
                        //and thus avoid adding multiple passes to same row total
                        for (K = 0; K < COMMOD9.NEVNT; K++)
                        {
                            //plot histogram for FAD_LAD pairs only 
                            //NOT  MAX-MIN pairs; they do not appear in COEX order!!
                            if (COMMOD9.IROWS[I, 1] != 1) continue;

                            if (COMMOD9.IROWS[I, 1] == 1) INCRALL[COMMOD9.IROWS[I, 2], K] = (int)(INCR[K]);

                            //incrall stores only FAD-LAD paired events
                            //incrall 1 row per taxon, in coex order, 
                            //hence (IROWS,3) 

                            if (INCR[K] == 0) continue;

                            valx = COMMOD9.COMPLVL[K, J - COMMOD9.NSCT];
                            stepup = (COMMOD9.COMPLVL[Math.Min(COMMOD9.NEVNT - 1, K + 1), J - COMMOD9.NSCT] +
                                COMMOD9.COMPLVL[K, J - COMMOD9.NSCT]) / 2.00;
                            stepdn = (COMMOD9.COMPLVL[K, J - COMMOD9.NSCT] +
                                COMMOD9.COMPLVL[Math.Max(0, K - 1), J - COMMOD9.NSCT]) / 2.00;

                            int xpos = Math.Min((int)(COMMOD9.lft + ((stepup - btmx) * scalx)), (int)(COMMOD9.lft + ((stepdn - btmx) * scalx)));
                            int ypos = Math.Min((int)(COMMOD9.maxy - COMMOD9.btm - 1),
                                (int)((COMMOD9.maxy - COMMOD9.btm - 1) - (((COMMOD9.btm * 4) - 10) * INCR[K] / Helper.MAXVal(INCR))));

                            //scale histogram bar to number of counts in INCR()
                            //and to space between composite sections and composite range
                            g.FillRectangle(frm.MSG_BRUSH_CCCCFF, xpos, ypos,
                                Math.Abs((int)(COMMOD9.lft + ((stepup - btmx) * scalx)) - (int)(COMMOD9.lft + ((stepdn - btmx) * scalx))),
                                Math.Abs((int)(-(((COMMOD9.btm * 4) - 10) * INCR[K] / Helper.MAXVal(INCR)))));
                        }

                        g.DrawString("  DISTRIBUTION", frm.MSG_FONT_S, frm.MSG_BRUSH_3333FF,
                            6, (int)(COMMOD9.maxy - COMMOD9.btm - 30));

                        g.DrawString("  OF SUPPORT", frm.MSG_FONT_S, frm.MSG_BRUSH_3333FF,
                            6, (int)(COMMOD9.maxy - COMMOD9.btm - 15));


                        // ----------------------------------------
                        //do titles
                        Output.COMPLBL(J + 1, ref xtitle);
                        xtitle = xtitle.Trim() + ": composite range in black";

                        g.RotateTransform(horiz);
                        g.DrawString(xtitle, frm.MSG_FONT, frm.MSG_BRUSH,
                            (int)(COMMOD9.lft + (COMMOD9.maxx - COMMOD9.lft) / 2),
                            (int)(COMMOD9.maxy - COMMOD9.btm / 2));

                        if (M == 1)
                        {
                            g.DrawString("WAIT FOR SCAN CYCLE TO COMPLETE", frm.MSG_FONT, frm.MSG_BRUSH,
                            (int)(COMMOD9.lft),
                            (int)(COMMOD9.btm));
                        }

                        g.ResetTransform();
                                        

                    }


                }//End for M

                upsection = true;
                Helper.SetVal(INCR, 0);
                
            }
            catch
            { }
        }        

        private void EVNTLBLINC(frmCONOP frm, Graphics g, int mI, int mJ)
        {
            COMMOD COMMOD9 = COMMOD.Singleton();
            string etitl = "";
            Brush brush = frm.MSG_BRUSH_7;

            if (COMMOD9.ISTATIC[mI, mJ, 0] > -1) brush = frm.MSG_BRUSH_12;
            Helper.GETEVNT(mI, ref etitl, 1, 1, 0, 0, 0, 0);
            if (COMMOD9.IROWS[mI, 1] == 3)
            {
                etitl = etitl.Trim() + ": MID-range event";
            }
            else if ((COMMOD9.IROWS[mI, 1] == 11) || (COMMOD9.IROWS[mI, 1] == 12))
            {
                etitl = etitl.Trim() + ": local uncertainties in red";
            }
            else if ((COMMOD9.IROWS[mI, 1] != 1) && (COMMOD9.IROWS[mI, 1] != 2))
            {
                etitl = etitl.Trim() + ": unpaired event";
            }
            else
            {
                etitl = etitl.Trim() + ":  local ranges in red";
            }

            g.DrawString(etitl, frm.MSG_FONT, brush, (int)COMMOD9.lft, (int)(COMMOD9.maxy - COMMOD9.btm * 3 / 4));

            g.DrawString("[←] [→] keys change taxon;   [↑] [↓] keys change composite scale;  [Esc] key exits loop",
                frm.MSG_FONT_S, frm.MSG_BRUSH, 10, (int)(COMMOD9.maxy - COMMOD9.btm / 3));
        }

        public void BeforeDO3()
        {
            try
            {

                COMMOD COMMOD9 = COMMOD.Singleton();

                S = 0;
                Helper.SetVal(INCR, 0);
                Helper.SetVal(SINCR, 0);
                Helper.SetVal(INCRN, 0);
                Helper.SetVal(SINCRN, 0);
                Helper.SetVal(INCREAL, 0.0);
                Helper.SetVal(SINCREAL, 0.0);

                for (K = 0; K < COMMOD9.NEVNT; K++)
                {
                    //total by species:
                    for (I = 0; I < COMMOD9.NSPC; I++)
                    {
                        if (INCRALL[I, K] > 0)
                        {
                            INCR[K] += INCRALL[I, K];
                            //INCR() is the total number of partial ranges at each K (taxa and sections)

                            INCRN[K]++;
                            //INCRN() is number of taxa at each K
                        }
                    }

                    //total by section:
                    for (I = 0; I < COMMOD9.NSCT; I++)
                    {
                        if (SINCRALL[I, K] > 0)
                        {
                            SINCR[K] += SINCRALL[I, K];
                            // SINCR() should be the same as INCR, both total sections and taxa 

                            SINCRN[K]++;
                            //SINCRN() is number of sections at each K
                        }
                    }

                    if (INCRN[K] == 0)
                    {
                        INCREAL[K] = 0.0;
                    }
                    else
                    {
                        INCREAL[K] = (double)(INCR[K]) / (double)(INCRN[K]);
                    }

                    if (SINCRN[K] == 0)
                    {
                        SINCREAL[K] = 0.0;
                    }
                    else
                    {
                        SINCREAL[K] = (double)(SINCR[K]) / (double)(SINCRN[K]);
                    }

                }
            }
            catch
            { }
        }

        public void DO3(frmCONOP frm, Graphics g,ref int JDelta,ref int LDelta)
        {
            try
            {

                //CPMS------------------------------------------------------------------
                //c    Upon {Esc} the routine moves on to summary statistics screens
                //c    It is possible to move back and forth among these screens
                //c    but not to go back to the individual taxa, except via the 
                //c    GRAPHICAL OUTPUT menu.
                //CPMS------------------------------------------------------------------
                //c     NOW plot the INCRALL totals in here
                //cpms  first, total INCRALL() into INCR()
                //cpms  and taxon count (richness!) into INCRN()
                //cpms  and section count from SINCRALL() into SINCRN()

                COMMOD COMMOD9 = COMMOD.Singleton();

                //CPMS     ----------------------------------------------
                //cpms  five loops
                //cpms      1st (L=2): plots number of taxa (richness!) at each level INCRN()
                //cpms      2nd (L=3): plots number of range increments at each level (sections and taxa) INCR()
                //cpms      3th (L=4): plots per-taxon increment count at each level INCREAL() 
                //cpms      4th (L=5): plots per-section increment count at each level SINCREAL()
                //cpms      5rd (L=6): plots number of sections at each level SINCRN()
                //cpms  L starts at 2 to allow looping forward and backward
                //cpms  L goes to 0 only to exit loop

                L = 2 + LDelta;

                if (L > 0)
                {
                    //go to full screen view

                    //start with the ORDINAL composite (J) in order to use all events
                    //starting with the chosen composite causes too few event levels in histogram
                    J = COMMOD9.NSCT + 1 - 1 + JDelta;

                    //do not change composite section until requested by {{X}{+/-}} or {ESc}
                    if (J > COMMOD9.NSCT - 1)
                    {
                        g.Clear(frm.BackColor);

                        //draw the horizontal section bar--------------
                        g.FillRectangle(frm.MSG_BRUSH_7, COMMOD9.lft, COMMOD9.maxy - COMMOD9.btm,
                            COMMOD9.maxx - COMMOD9.rit - COMMOD9.lft, 10);

                        g.DrawRectangle(frm.DRAW_PEN, COMMOD9.lft, COMMOD9.maxy - COMMOD9.btm,
                            COMMOD9.maxx - COMMOD9.rit - COMMOD9.lft, 10);

                        //-----scale the section--------------------------                    
                        topx = COMMOD9.COMPLVL[COMMOD9.HLEVEL[J], J - COMMOD9.NSCT];//?
                        btmx = 1000;
                        scalx = (COMMOD9.maxx - COMMOD9.lft - COMMOD9.rit) / (topx - btmx);

                        //CPMS-----mark the collection levels--------------
                        for (K = 0; K <= COMMOD9.HLEVEL[J]; K++)
                        {
                            valx = COMMOD9.COMPLVL[K, J - COMMOD9.NSCT];

                            g.DrawLine(frm.DRAW_PEN, (int)(COMMOD9.lft + ((valx - btmx) * scalx)), (int)(COMMOD9.maxy - COMMOD9.btm + 10),
                                (int)(COMMOD9.lft + ((valx - btmx) * scalx)), (int)(COMMOD9.maxy - COMMOD9.btm + 13));
                        }

                        //Axis titles
                        //X-AXIS TITLE     do composite title
                        Output.COMPLBL(J + 1, ref xtitle);
                        g.RotateTransform(horiz);
                        g.DrawString(xtitle, frm.MSG_FONT, frm.MSG_BRUSH, (int)(COMMOD9.lft + (COMMOD9.maxx - COMMOD9.lft) / 2),
                            (int)(COMMOD9.maxy - COMMOD9.btm / 2));

                        //X-AXIS SUB-TITLE     do composite title
                        g.DrawString("[←] [→] keys reveal 4 more plots;   " +
                            "[↑] [↓] keys change composite scale;  " +
                            "[Esc] key exits loop", frm.MSG_FONT_S, frm.MSG_BRUSH, 10, (int)(COMMOD9.maxy - COMMOD9.btm / 2));

                        //Y-AXIS TITLE                     

                        if (L == 2)
                        {
                            ntitle = string.Format("{0}", Helper.MAXVal(INCRN));
                            ytitle = "   RAW TAXON RICHNESS:     i.e." +
                                " sum of observed partial ranges, at each level " +
                                "(not composite range-through ranges)";
                        }
                        else if (L == 3)
                        {
                            ntitle = string.Format("{0}", Helper.MAXVal(INCR));
                            ytitle = "    CUMULATIVE SUPPORT:     i.e. " +
                                " range increments summed across all taxa and sections";
                        }
                        else if (L == 4)
                        {
                            ntitle = string.Format("{0,6:0.00}", Helper.MAXVal(INCREAL));
                            ytitle = "   AVERAGE TAXON SUPPORT:     i.e. " +
                                "number of local range increments divided by " +
                                "number of taxa";
                        }
                        else if (L == 5)
                        {
                            ntitle = string.Format("{0,6:0.00}", Helper.MAXVal(SINCREAL));
                            ytitle = "   AVERAGE SECTION CONTRIBUTION:     i.e. " +
                                "number of local range increments divided by " +
                                "number of sections";
                        }
                        else if (L == 6)
                        {
                            ntitle = string.Format("{0}", Helper.MAXVal(SINCRN));
                            ytitle = "   SECTION RICHNESS:     i.e." +
                                " number of sections that contribute at least " +
                                "one partial range at each level";
                        }

                        g.TranslateTransform((int)(COMMOD9.lft / 4),
                            (int)(COMMOD9.maxy - COMMOD9.btm));
                        g.RotateTransform(vert);

                        Font strFont = new Font("Arial", 12);
                        SizeF strSize = g.MeasureString(ytitle, strFont);
                        while ((strSize.Width + (int)(COMMOD9.lft / 4) + 18) > COMMOD9.maxy)
                        {
                            strFont = new Font("Arial", strFont.Size - 1);
                            strSize = g.MeasureString(ytitle, strFont);
                        }

                        g.DrawString(ytitle, strFont, frm.MSG_BRUSH, 0, 0);
                        g.ResetTransform();

                        //Y-AXIS SUBTITLE   
                        if (L == 2)
                        {
                            ytitle = "   uncorrected taxon count at each composite level";
                        }
                        else if (L == 3)
                        {
                            ytitle = "  increases with section count and taxon count";
                        }
                        else if (L == 4)
                        {
                            ytitle = "   average number of sections per taxon at each level";
                        }
                        else if (L == 5)
                        {
                            ytitle = "   average number of taxa per section at each level";
                        }
                        else if (L == 6)
                        {
                            ytitle = "   may be less than number of sections that span level";
                        }

                        g.TranslateTransform((int)(COMMOD9.lft / 2),
                            (int)(COMMOD9.maxy - COMMOD9.btm));
                        g.RotateTransform(-90);
                        g.DrawString(ytitle, frm.MSG_FONT_S, frm.MSG_BRUSH, 0, 0);
                        g.ResetTransform();

                        g.ResetTransform();
                        g.RotateTransform(horiz);
                        g.DrawString("max = " + ntitle, frm.MSG_FONT_S, frm.MSG_BRUSH, (int)(COMMOD9.lft / 4),
                            (int)(COMMOD9.top + 3));

                        g.ResetTransform();

                        //----------------------------------------------
                        //draw the histogram of sum of array values
                        //DO K=1,HLEVEL(J)  Not by levels, but by event: may be many events at one level!
                        for (K = 0; K < COMMOD9.NEVNT; K++)
                        {
                            if ((L == 2) && (INCRN[K] == 0)) continue;
                            if ((L == 3) && (INCR[K] == 0)) continue;
                            if ((L == 4) && (INCREAL[K] == 0.0)) continue;
                            if ((L == 5) && (SINCREAL[K] == 0.0)) continue;
                            if ((L == 6) && (SINCRN[K] == 0)) continue;

                            //NOTE: the following lines must use the complex HSCTSOL(BSTPERM(... 
                            //      structure because only the ORDINAL composite has one level
                            //      for every event; the other composites tend to have fewer 
                            //      levels because the zero-spacing collapses events into same
                            //      level.  Therefore, each event must 'find' its level in 
                            //      each composite. (Only for the ORDINAL composite is the 
                            //      array key equal to the level.) 

                            //NOTE: To make the histogram bars contiguous, they should extend between 
                            //       midpoints of adjacent event levels.  Therefore the code finds the
                            //       average of two levels and guards against (MAX, MIN) looking 
                            //       beyond the section limits

                            valx = COMMOD9.COMPLVL[COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], J], J - COMMOD9.NSCT];
                            stepup = (COMMOD9.COMPLVL[Math.Min(COMMOD9.HLEVEL[J], COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K + 1], J]), J - COMMOD9.NSCT] +
                                    COMMOD9.COMPLVL[COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], J], J - COMMOD9.NSCT]) / 2.00;
                            stepdn = (COMMOD9.COMPLVL[COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K], J], J - COMMOD9.NSCT] +
                                COMMOD9.COMPLVL[Math.Max(0, COMMOD9.HSCTSOL[COMMOD9.BSTPERM[K - 1], J]), J - COMMOD9.NSCT]) / 2.00;

                            //scale histogram bar to number of counts in INCR() for L.eq.1
                            //or to average count in INCREAL() for L.eq.2
                            //and to space between composite sections and composite range
                            if ((int)(COMMOD9.lft + ((stepup - btmx) * scalx)) < 1) continue;

                            if (L == 2)
                            {
                                int x1 = (int)(COMMOD9.lft + ((stepup - btmx)) * scalx);
                                int y1 = (int)(COMMOD9.maxy - COMMOD9.btm - 1);
                                int x2 = (int)(COMMOD9.lft + ((stepdn - btmx)) * scalx);
                                int y2 = (int)((double)(COMMOD9.maxy - COMMOD9.btm - 1) - ((double)(COMMOD9.maxy - COMMOD9.top - COMMOD9.btm - 1) *
                                    (double)(INCRN[K]) / (double)(Helper.MAXVal(INCRN))));

                                g.FillRectangle(frm.MSG_BRUSH_CCFFCC, Math.Min(x1, x2), Math.Min(y1, y2),
                                    Math.Abs(x1 - x2), Math.Abs(y1 - y2));
                            }
                            else if (L == 3)
                            {
                                int x1 = (int)(COMMOD9.lft + ((stepup - btmx)) * scalx);
                                int y1 = (int)(COMMOD9.maxy - COMMOD9.btm - 1);
                                int x2 = (int)(COMMOD9.lft + ((stepdn - btmx)) * scalx);
                                int y2 = (int)((double)(COMMOD9.maxy - COMMOD9.btm - 1) - ((double)(COMMOD9.maxy - COMMOD9.top - COMMOD9.btm - 1) *
                                    (double)(INCR[K]) / (double)(Helper.MAXVal(INCR))));

                                g.FillRectangle(frm.MSG_BRUSH_CCCCFF, Math.Min(x1, x2), Math.Min(y1, y2),
                                    Math.Abs(x1 - x2), Math.Abs(y1 - y2));
                            }
                            else if (L == 4)
                            {
                                int x1 = (int)(COMMOD9.lft + ((stepup - btmx)) * scalx);
                                int y1 = (int)(COMMOD9.maxy - COMMOD9.btm - 1);
                                int x2 = (int)(COMMOD9.lft + ((stepdn - btmx)) * scalx);
                                int y2 = (int)((double)(COMMOD9.maxy - COMMOD9.btm - 1) - ((double)(COMMOD9.maxy - COMMOD9.top - COMMOD9.btm - 1) *
                                    (double)(INCREAL[K]) / (double)(Helper.MAXVal(INCREAL))));

                                g.FillRectangle(frm.MSG_BRUSH_CCCCFF, Math.Min(x1, x2), Math.Min(y1, y2),
                                    Math.Abs(x1 - x2), Math.Abs(y1 - y2));
                            }
                            else if (L == 5)
                            {
                                int x1 = (int)(COMMOD9.lft + ((stepup - btmx)) * scalx);
                                int y1 = (int)(COMMOD9.maxy - COMMOD9.btm - 1);
                                int x2 = (int)(COMMOD9.lft + ((stepdn - btmx)) * scalx);
                                int y2 = (int)((double)(COMMOD9.maxy - COMMOD9.btm - 1) - ((double)(COMMOD9.maxy - COMMOD9.top - COMMOD9.btm - 1) *
                                    (double)(SINCREAL[K]) / (double)(Helper.MAXVal(SINCREAL))));

                                g.FillRectangle(frm.MSG_BRUSH_FFCCCC, Math.Min(x1, x2), Math.Min(y1, y2),
                                    Math.Abs(x1 - x2), Math.Abs(y1 - y2));
                            }
                            else if (L == 6)
                            {
                                int x1 = (int)(COMMOD9.lft + ((stepup - btmx)) * scalx);
                                int y1 = (int)(COMMOD9.maxy - COMMOD9.btm - 1);
                                int x2 = (int)(COMMOD9.lft + ((stepdn - btmx)) * scalx);
                                int y2 = (int)((double)(COMMOD9.maxy - COMMOD9.btm - 1) - ((double)(COMMOD9.maxy - COMMOD9.top - COMMOD9.btm - 1) *
                                    (double)(SINCRN[K]) / (double)(Helper.MAXVal(SINCRN))));

                                g.FillRectangle(frm.MSG_BRUSH_FFCCCC, Math.Min(x1, x2), Math.Min(y1, y2),
                                    Math.Abs(x1 - x2), Math.Abs(y1 - y2));
                            }

                        }//End for K

                        //scale the y-axis
                        g.DrawLine(frm.DRAW_PEN, (int)(COMMOD9.lft), (int)(COMMOD9.maxy - COMMOD9.btm),
                            (int)(COMMOD9.lft), (int)(COMMOD9.maxy - COMMOD9.btm + 2));

                        g.DrawLine(frm.DRAW_PEN, (int)(COMMOD9.lft), (int)(COMMOD9.maxy - COMMOD9.btm),
                           (int)(COMMOD9.lft), (int)(COMMOD9.top));

                        //set appropriate top value
                        if (L == 2)
                        {
                            MAXYVAL = (double)(Helper.MAXVal(INCRN));
                        }
                        else if (L == 3)
                        {
                            MAXYVAL = (double)(Helper.MAXVal(INCR));
                        }
                        else if (L == 4)
                        {
                            MAXYVAL = (double)(Helper.MAXVal(INCREAL));
                        }
                        else if (L == 5)
                        {
                            MAXYVAL = (double)(Helper.MAXVal(SINCREAL));
                        }
                        else if (L == 6)
                        {
                            MAXYVAL = (double)(Helper.MAXVal(SINCRN));
                        }

                        N = 1;
                        while ((double)N < MAXYVAL)
                        {
                            g.DrawLine(frm.DRAW_PEN, (int)(COMMOD9.lft), (int)((double)(COMMOD9.maxy - COMMOD9.btm) -
                                (double)(N) * (COMMOD9.maxy - COMMOD9.btm - COMMOD9.top) / MAXYVAL),
                            (int)(COMMOD9.lft - 5), (int)((double)(COMMOD9.maxy - COMMOD9.btm) -
                                (double)(N) * (COMMOD9.maxy - COMMOD9.btm - COMMOD9.top) / MAXYVAL));

                            //limit number of scale ticks

                            if (MAXYVAL <= 50)
                            {
                                N++;
                            }
                            else if (MAXYVAL <= 100)
                            {
                                N += 10;
                            }
                            else if (MAXYVAL <= 500)
                            {
                                N += 50;
                            }
                            else if (MAXYVAL <= 1000)
                            {
                                N += 100;
                            }
                            else if (MAXYVAL <= 10000)
                            {
                                N += 500;
                            }
                        }
                    }

                    S = 0;
                }
            }
            catch
            { }


        }

        public void DO4(frmCONOP frm, Graphics g,ref int LDelta)
        {
            //now plot the diversity against the per taxon average support.
            COMMOD COMMOD9=COMMOD.Singleton();
            L=2+LDelta;

            if(L!=0)
            {
                if(L==2)
                {
                    g.Clear(frm.BackColor);

                    //draw axes
                    g.DrawLine(frm.DRAW_PEN,(int)(COMMOD9.maxx-COMMOD9.rit),(int)(COMMOD9.maxy-COMMOD9.btm),
                        (int)(COMMOD9.lft),(int)(COMMOD9.maxy-COMMOD9.btm));
                    g.DrawLine(frm.DRAW_PEN, (int)(COMMOD9.lft), (int)(COMMOD9.maxy - COMMOD9.btm),
                        (int)(COMMOD9.lft),(int)(COMMOD9.top));

                    ntitle=string.Format("{0,6:0.00}",Helper.MAXVal(INCREAL));
                    g.DrawString("max = "+ntitle.Trim(),frm.MSG_FONT,frm.MSG_BRUSH,
                        (int)(COMMOD9.maxx-COMMOD9.rit-10),(int)(COMMOD9.maxy-COMMOD9.btm+10));

                    g.DrawString("AVERAGE NUMBER OF OBSERVED RANGE INCREMENTS PER TAXON",frm.MSG_FONT,frm.MSG_BRUSH,
                        (int)(COMMOD9.maxx/4),(int)(COMMOD9.maxy-(COMMOD9.btm/2)));

                    ntitle=string.Format("{0}",Helper.MAXVal(INCRN));
                    g.DrawString("max = "+ntitle.Trim(),frm.MSG_FONT,frm.MSG_BRUSH,
                        (int)(COMMOD9.lft/4),(int)(COMMOD9.top+3));

                    g.TranslateTransform((int)(COMMOD9.lft/4),(int)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.btm));
                    g.RotateTransform(vert);
                    g.DrawString("TAXON RICHNESS (not composite range-through)",frm.MSG_FONT,frm.MSG_BRUSH,0,0);
                    g.ResetTransform();

                    g.TranslateTransform((int)(COMMOD9.lft/2),(int)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.btm));
                    g.RotateTransform(vert);
                    g.DrawString(" one symbol for each event-level in composite sequence",frm.MSG_FONT_S,frm.MSG_BRUSH,0,0);
                    g.ResetTransform();

                    for(K=0;K<COMMOD9.NEVNT;K++)
                    {
                        if(INCRN[K]==0)continue;
                        if(INCREAL[K]==0.0)continue;

                        int x1=(int)((double)(COMMOD9.lft)-3+(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)*
                            INCREAL[K]/Helper.MAXVal(INCREAL));
                        int y1=(int)((double)(COMMOD9.maxy-COMMOD9.btm-3)-(double)(COMMOD9.maxy-COMMOD9.top-COMMOD9.btm)*
                            (double)(INCRN[K])/(double)(Helper.MAXVal(INCRN)));
                        int x2=(int)((double)(COMMOD9.lft)+3+(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)*
                            INCREAL[K]/Helper.MAXVal(INCREAL));
                        int y2=(int)((double)(COMMOD9.maxy-COMMOD9.btm+3)-(double)(COMMOD9.maxy-COMMOD9.top-COMMOD9.btm)*
                            (double)(INCRN[K])/(double)(Helper.MAXVal(INCRN)));

                        g.DrawRectangle(frm.DRAW_PEN,Math.Min(x1,x2),Math.Min(y1,y2),Math.Abs(x1-x2),Math.Abs(y1-y2));

                         x1=(int)((double)(COMMOD9.lft)-2+(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)*
                            INCREAL[K]/Helper.MAXVal(INCREAL));
                         y1=(int)((double)(COMMOD9.maxy-COMMOD9.btm-2)-(double)(COMMOD9.maxy-COMMOD9.top-COMMOD9.btm)*
                            (double)(INCRN[K])/(double)(Helper.MAXVal(INCRN)));
                         x2=(int)((double)(COMMOD9.lft)+2+(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)*
                            INCREAL[K]/Helper.MAXVal(INCREAL));
                         y2=(int)((double)(COMMOD9.maxy-COMMOD9.btm+2)-(double)(COMMOD9.maxy-COMMOD9.top-COMMOD9.btm)*
                            (double)(INCRN[K])/(double)(Helper.MAXVal(INCRN)));
  
                        g.FillRectangle(frm.MSG_BRUSH_CCCCFF,Math.Min(x1,x2),Math.Min(y1,y2),Math.Abs(x1-x2),Math.Abs(y1-y2));

                    }

                    //cpms  scale the x-axis
                    g.DrawLine(frm.DRAW_PEN,(int)(COMMOD9.lft),(int)(COMMOD9.maxy-COMMOD9.btm),
                        (int)(COMMOD9.lft),(int)(COMMOD9.maxy-COMMOD9.btm+2));

                    J=1;
                    while((double)J<Helper.MAXVal(INCREAL))
                    {
                        g.DrawLine(frm.DRAW_PEN,
                            (int)(COMMOD9.lft+(double)J*(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)/Helper.MAXVal(INCREAL)),
                            (int)(COMMOD9.maxy-COMMOD9.btm),
                            (int)(COMMOD9.lft+(double)J*(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)/Helper.MAXVal(INCREAL)),
                            (int)(COMMOD9.maxy-COMMOD9.btm+5)
                            );

                        J++;
                    }

                    //cpms  scale the y-axis
                    g.DrawLine(frm.DRAW_PEN,(int)(COMMOD9.lft),(int)(COMMOD9.maxy-COMMOD9.btm),
                        (int)(COMMOD9.lft),(int)(COMMOD9.maxy-COMMOD9.btm+2));

                    J=1;
                    while((double)J<Helper.MAXVal(INCRN))
                    {
                        g.DrawLine(frm.DRAW_PEN,
                            (int)(COMMOD9.lft),
                            (int)((double)(COMMOD9.maxy-COMMOD9.btm)-(double)J*(double)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.top)/Helper.MAXVal(INCRN)),
                            (int)(COMMOD9.lft-5),
                            (int)((double)(COMMOD9.maxy-COMMOD9.btm)-(double)J*(double)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.top)/Helper.MAXVal(INCRN))
                            );

                        J++;
                    }

                }
                else if(L==3)
                {
                    //CPMS   ------------------------------------------------------------------
                    //cpms   now plot the diversity against the per section average support.
                    g.Clear(frm.BackColor);

                    //draw axes
                    g.DrawLine(frm.DRAW_PEN,(int)(COMMOD9.maxx-COMMOD9.rit),(int)(COMMOD9.maxy-COMMOD9.btm),
                        (int)(COMMOD9.lft),(int)(COMMOD9.maxy-COMMOD9.btm));
                    g.DrawLine(frm.DRAW_PEN, (int)(COMMOD9.lft), (int)(COMMOD9.maxy - COMMOD9.btm),
                        (int)(COMMOD9.lft),(int)(COMMOD9.top));

                    ntitle=string.Format("{0,6:0.00}",Helper.MAXVal(SINCREAL));
                    g.DrawString("max = "+ntitle.Trim(),frm.MSG_FONT,frm.MSG_BRUSH,
                        (int)(COMMOD9.maxx-COMMOD9.rit-10),(int)(COMMOD9.maxy-COMMOD9.btm+10));

                    g.DrawString("AVERAGE SINGLE-SECTION TAXON RICHNESS;   i.e. "+
                    "number of range increments per section",frm.MSG_FONT,frm.MSG_BRUSH,
                        (int)(COMMOD9.maxx/4),(int)(COMMOD9.maxy-(COMMOD9.btm/2)));

                    ntitle=string.Format("{0}",Helper.MAXVal(INCRN));
                    g.DrawString("max = "+ntitle.Trim(),frm.MSG_FONT,frm.MSG_BRUSH,
                        (int)(COMMOD9.lft/4),(int)(COMMOD9.top+3));

                    g.TranslateTransform((int)(COMMOD9.lft/4),(int)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.btm));
                    g.RotateTransform(vert);
                    g.DrawString("COMPOSITE TAXON RICHNESS (not composite range-through)",frm.MSG_FONT,frm.MSG_BRUSH,0,0);
                    g.ResetTransform();

                    g.TranslateTransform((int)(COMMOD9.lft/2),(int)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.btm));
                    g.RotateTransform(vert);
                    g.DrawString(" one symbol for each event-level in composite sequence",frm.MSG_FONT_S,frm.MSG_BRUSH,0,0);
                    g.ResetTransform();       

                    for(K=0;K<COMMOD9.NEVNT;K++)
                    {
                        if(SINCRN[K]==0)continue;
                        if(SINCREAL[K]==0.0)continue;

                        int x1=(int)((double)(COMMOD9.lft)-3+(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)*
                            SINCREAL[K]/Helper.MAXVal(SINCREAL));
                        int y1=(int)((double)(COMMOD9.maxy-COMMOD9.btm-3)-(double)(COMMOD9.maxy-COMMOD9.top-COMMOD9.btm)*
                            (double)(INCRN[K])/(double)(Helper.MAXVal(INCRN)));
                        int x2=(int)((double)(COMMOD9.lft)+3+(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)*
                            SINCREAL[K]/Helper.MAXVal(SINCREAL));
                        int y2=(int)((double)(COMMOD9.maxy-COMMOD9.btm+3)-(double)(COMMOD9.maxy-COMMOD9.top-COMMOD9.btm)*
                            (double)(INCRN[K])/(double)(Helper.MAXVal(INCRN)));

                        g.DrawRectangle(frm.DRAW_PEN,Math.Min(x1,x2),Math.Min(y1,y2),Math.Abs(x1-x2),Math.Abs(y1-y2));

                         x1=(int)((double)(COMMOD9.lft)-2+(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)*
                            SINCREAL[K]/Helper.MAXVal(SINCREAL));
                         y1=(int)((double)(COMMOD9.maxy-COMMOD9.btm-2)-(double)(COMMOD9.maxy-COMMOD9.top-COMMOD9.btm)*
                            (double)(INCRN[K])/(double)(Helper.MAXVal(INCRN)));
                         x2=(int)((double)(COMMOD9.lft)+2+(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)*
                            SINCREAL[K]/Helper.MAXVal(SINCREAL));
                         y2=(int)((double)(COMMOD9.maxy-COMMOD9.btm+2)-(double)(COMMOD9.maxy-COMMOD9.top-COMMOD9.btm)*
                            (double)(INCRN[K])/(double)(Helper.MAXVal(INCRN)));
  
                        g.FillRectangle(frm.MSG_BRUSH_FFCCCC,Math.Min(x1,x2),Math.Min(y1,y2),Math.Abs(x1-x2),Math.Abs(y1-y2));

                    }

                    //cpms  show 45-degree line
                    frm.DRAW_PEN_7.DashStyle=System.Drawing.Drawing2D.DashStyle.Dash;
                    g.DrawLine(frm.DRAW_PEN_7,(int)(COMMOD9.lft+1),(int)(COMMOD9.maxy-COMMOD9.btm-1),
                        (int)(COMMOD9.maxx-COMMOD9.rit),(int)((double)(COMMOD9.maxy - COMMOD9.btm)-
                        (double)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.top)*
                        Helper.MAXVal(SINCREAL)/(double)(Helper.MAXVal(INCRN))));
                    frm.DRAW_PEN_7.DashStyle=System.Drawing.Drawing2D.DashStyle.Solid;

                    g.DrawString(" slope = 1",frm.MSG_FONT_S,frm.MSG_BRUSH_7,
                        (int)(COMMOD9.maxx - COMMOD9.rit + 1), (int)((double)(COMMOD9.maxy - COMMOD9.btm) -
                        (double)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.top)*
                        Helper.MAXVal(SINCREAL)/(double)(Helper.MAXVal(INCRN))-5));

                    g.DrawString(" PLOTS IMPOSSIBLE BELOW LINE",frm.MSG_FONT_S,frm.MSG_BRUSH_7,
                        (int)(COMMOD9.maxx*3/4),(int)((double)(COMMOD9.maxy-COMMOD9.btm)-
                        (double)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.top)*Helper.MAXVal(SINCREAL)/
                        (double)(Helper.MAXVal(INCRN))/2-5));

                    //cpms  scale the x-axis
                    g.DrawLine(frm.DRAW_PEN,(int)(COMMOD9.lft),(int)(COMMOD9.maxy-COMMOD9.btm),
                        (int)(COMMOD9.lft),(int)(COMMOD9.maxy-COMMOD9.btm+2));

                    J=1;
                    while((double)J<Helper.MAXVal(SINCREAL))
                    {
                        g.DrawLine(frm.DRAW_PEN,
                            (int)(COMMOD9.lft+(double)J*(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)/Helper.MAXVal(SINCREAL)),
                            (int)(COMMOD9.maxy-COMMOD9.btm),
                            (int)(COMMOD9.lft+(double)J*(double)(COMMOD9.maxx-COMMOD9.lft-COMMOD9.rit)/Helper.MAXVal(SINCREAL)),
                            (int)(COMMOD9.maxy-COMMOD9.btm+5)
                            );

                        J++;
                    }

 
                    //cpms  scale the y-axis
                    g.DrawLine(frm.DRAW_PEN,(int)(COMMOD9.lft),(int)(COMMOD9.maxy-COMMOD9.btm),
                        (int)(COMMOD9.lft),(int)(COMMOD9.maxy-COMMOD9.btm+2));

                    J=1;
                    while((double)J<Helper.MAXVal(INCRN))
                    {
                        g.DrawLine(frm.DRAW_PEN,
                            (int)(COMMOD9.lft),
                            (int)((double)(COMMOD9.maxy-COMMOD9.btm)-(double)J*(double)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.top)/Helper.MAXVal(INCRN)),
                            (int)(COMMOD9.lft-5),
                            (int)((double)(COMMOD9.maxy-COMMOD9.btm)-(double)J*(double)(COMMOD9.maxy-COMMOD9.btm-COMMOD9.top)/Helper.MAXVal(INCRN))
                            );

                        J++;
                    }

                }

                // X-AXIS SUB-TITLE     do composite title
                g.DrawString("[←] [→] keys reveal 1 more plot;   [Esc] key exits loop",
                    frm.MSG_FONT_S,frm.MSG_BRUSH,10,(int)(COMMOD9.maxy-COMMOD9.btm/2));

            }
        }

        public void DO5(frmCONOP frm, Graphics g,ref int JDelta, ref int evtDelta)
        {
            
            //draw the grand dimensionless range support
            
            COMMOD COMMOD9=COMMOD.Singleton();

            L = 1 + evtDelta;
	        M=2;
            J = COMMOD9.NSCT + 1 - 1 + JDelta;

            //LOAD GRAND() in three loops
            //1.Section Loop------
            if (J > COMMOD9.NSCT-1)
            {
                //calculate the grand-average dimensionless support into GRAND() 
                Helper.SetVal(GRAND, 0);
                N = 0;
                RNG = 0;
                RRNG = 0.00;

                //----set the composite scale--------------------------
                topx = COMMOD9.COMPLVL[COMMOD9.HLEVEL[J], J - COMMOD9.NSCT];
                btmx = 1000;
                scalx = 10000 / (topx - btmx);

                //2.Taxon Loop-----------
                for (IG = 0; IG < COMMOD9.NSPC; IG++)
                {
                    for (KG = 0; KG < COMMOD9.NEVNT; KG++)
                    {
                        if (INCRALL[IG, KG] > 0) break;
                    }
                    FADLVL = KG;

                    for (KG = COMMOD9.NEVNT - 1; KG >= 0; KG--)
                    {
                        if (INCRALL[IG, KG] > 0) break;
                    }
                    LADLVL = KG;

                    //toss out taxa with ranges extending beyond the composite
                    //use a 2-level cushion
                    if ((J - COMMOD9.NSCT == 0) &&
                        ((FADLVL <= 1) || (LADLVL >= COMMOD9.NEVNT - 2))) continue;

                    //toss out obvious errors 
                    if (FADLVL > LADLVL) continue;

                    RNG = LADLVL - FADLVL + 1;
                    RRNG = COMMOD9.COMPLVL[LADLVL, J - COMMOD9.NSCT] - COMMOD9.COMPLVL[FADLVL, J - COMMOD9.NSCT];

                    //cull the taxon ranges  
                    if (RNG < (5 * (L - 1))) continue;

                    N++;

                    //3.Event Level Loop------------
                    for (K = 0; K < COMMOD9.NEVNT; K++)
                    {
                        //don't plot histogram for non-range (unpaired) events

                        if (INCRALL[IG, K] == 0) continue;

                        valx = COMMOD9.COMPLVL[K, J - COMMOD9.NSCT];
                        stepup = (COMMOD9.COMPLVL[Math.Min(COMMOD9.NEVNT - 1, K + 1), J - COMMOD9.NSCT] +
                            COMMOD9.COMPLVL[K, J - COMMOD9.NSCT]) / 2.00;
                        stepdn = (COMMOD9.COMPLVL[Math.Max(0, K - 1), J - COMMOD9.NSCT] +
                            COMMOD9.COMPLVL[K, J - COMMOD9.NSCT]) / 2.00;

                        //4. GRAND() Loop
                        for (GG = Math.Max(0, (int)((stepdn - COMMOD9.COMPLVL[FADLVL, J - COMMOD9.NSCT]) * 10000 / RRNG));
                            GG < Math.Min(10000, (int)((stepup - COMMOD9.COMPLVL[FADLVL, J - COMMOD9.NSCT]) * 10000 / RRNG));
                            GG++)
                        {
                            if (M == 2)
                            {
                                GRAND[GG] += INCRALL[IG, K];
                            }
                            else if (M == 3)
                            {
                                GRAND[GG] += (int)(100.0 * (double)(INCRALL[IG, K]) / (double)(Math.Max(COMMOD9.COVER[K], 1)));
                            }
                        }
                    }
                }


                g.Clear(frm.BackColor);

                //draw axes
                g.DrawLine(frm.DRAW_PEN, (int)(COMMOD9.maxx - COMMOD9.rit), (int)(COMMOD9.maxy - COMMOD9.btm),
                    (int)COMMOD9.lft, (int)(COMMOD9.maxy - COMMOD9.btm));

                //do titles
                g.DrawString("first-appearance", frm.MSG_FONT_S, frm.MSG_BRUSH, (int)(COMMOD9.lft - 10), (int)(COMMOD9.maxy - COMMOD9.btm + 10));
                g.DrawString("last-appearance", frm.MSG_FONT_S, frm.MSG_BRUSH, (int)(COMMOD9.maxx - COMMOD9.rit - 20), (int)(COMMOD9.maxy - COMMOD9.btm + 10));

                Output.COMPLBL(J+1, ref xtitle);
                g.DrawString(xtitle, frm.MSG_FONT, frm.MSG_BRUSH, (int)(COMMOD9.lft + (COMMOD9.maxx - COMMOD9.lft) / 2),
                    (int)(COMMOD9.maxy - COMMOD9.btm / 2));

                g.DrawString("[←] [→] keys change taxon;   [↑] [↓] keys change composite scale;  [Esc] key exits loop",
                    frm.MSG_FONT_S, frm.MSG_BRUSH, (int)(COMMOD9.lft * 2), (int)(COMMOD9.maxy - COMMOD9.btm / 2));

                g.DrawString("DIMENSIONLESS RANGE SUPPORT -  Stacked for All Taxa", frm.MSG_FONT, frm.MSG_BRUSH,
                    (int)(COMMOD9.maxx / 5), (int)(COMMOD9.maxy / 10));

                if (M == 2)
                {
                    g.DrawString("Y-Axis:  Number of Sections", frm.MSG_FONT_S, frm.MSG_BRUSH,
                        (int)(COMMOD9.maxx / 5), (int)(COMMOD9.maxy / 10 + 20));
                }
                else if (M == 3)
                {
                    g.DrawString("Y-Axis:  Proportion of Sections", frm.MSG_FONT_S, frm.MSG_BRUSH,
                       (int)(COMMOD9.maxx / 5), (int)(COMMOD9.maxy / 10 + 20));
                }

                ntitle = string.Format("{0}", 5 * (L - 1));


                if (Helper.MAXVal(GRAND) <= 0)
                {
                    g.DrawString("No taxon ranges are longer than " + ntitle.Trim() + " events", frm.MSG_FONT_S, frm.MSG_BRUSH,
                        (int)(COMMOD9.maxx / 2), (int)(COMMOD9.maxy / 2));
                }
                else
                {
                    g.DrawString("Eliminates taxon ranges shorter than " + ntitle.Trim() + " events;     [←] [→] keys change threshold",
                        frm.MSG_FONT_S, frm.MSG_BRUSH, (int)(COMMOD9.maxx / 4), (int)(COMMOD9.maxy / 5));

                    ntitle = string.Format("{0}", N);
                    g.DrawString(ntitle + " taxa in stack", frm.MSG_FONT_S, frm.MSG_BRUSH, (int)(COMMOD9.maxx / 4), (int)((COMMOD9.maxy / 5) + 20));

                    for (GG = 0; GG < 10000; GG++)
                    {
                        int x1 = (int)(COMMOD9.lft + (double)(COMMOD9.maxx - COMMOD9.lft - COMMOD9.rit) * (double)(GG) / 10000.0);
                        int y1 = (int)(COMMOD9.maxy - COMMOD9.btm - 1);
                        int x2 = (int)(COMMOD9.lft + (double)(COMMOD9.maxx - COMMOD9.lft - COMMOD9.rit) * (double)(GG + 1) / 10000.0);
                        int y2 = (int)((double)(COMMOD9.maxy - COMMOD9.btm - 1) -
                            (double)(COMMOD9.maxy / 2) * (double)(GRAND[GG]) / (double)(Helper.MAXVal(GRAND)));

                        if (x1 != x2)
                        {
                            g.FillRectangle(frm.MSG_BRUSH_AAAAAA, Math.Min(x1, x2), Math.Min(y1, y2),
                                Math.Abs(x1 - x2), Math.Abs(y1 - y2));
                        }
                        else
                        {
                            g.DrawLine(frm.DRAW_PEN_AAAAAA, x1, y1, x2, y2);
                        }
                    }

                }
            }

        }

    }

}
