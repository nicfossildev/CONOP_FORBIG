1, Macro definition
OUTDATA - generate csv into bin/Debug/outData
REPACEPERM - replace initperm from bin/Debug/inData

2, SEC19 interval algorithm
Summary:the formal compilation symbols
test_initPerm;SEC19;REPACEPERM;FAKE_IJRANKE_DOUBLE

Read from file: test_initPerm;SEC19;REPACEPERM;FAKE_IJRANKE_DOUBLE
Write into csv file: OUTDATA
Write into ijrank file: OUTDATA_IJRANK;test_initPerm;SEC19;REPACEPERM;

3, SWITH from interval to level
just ***Need to change file cfg on Line 24 from ***

 PENALTY='interval'
 =>
 PENALTY='level'

 4, check-up result with CONOP.NET_REFER
	1) ANNEAL.cs - done [no change]
	2) COMMOD.cs - done [no change]
	3) DOCOEX.cs - done [no change]
	4) DOFB4L.cs - done [no change]
	5) DOINPUT.cs - done [no change]
	6) DOPTLORD.cs - done [no change]
	7) DORASC.cs - done [no change]
	8）DRAWINCR.cs - done [no change]
	9) frmCONOP.cs - done [change : disable this.Load += new System.EventHandler(this.frmCONOP_Load);]
	10) GETSTART.cs - done [no change]
	11) Helper.cs - done [Line 251 - Console.Write(s); does it impact performance or UI update?]
	12) HelperEX.cs - done [no change, only add comments]
	13) IRunIt.cs - done [no change]
	14) Output.cs 
		changes:
			1> Line 228  
			 //COMMOD9.SECTNICK[I].Substring(0, 3), COMMOD9.SCJPEN[I], str.Trim().Length >= 7 ? str.Trim().Substring(0, 7) : str.Trim(), -- disable FORBIG version
			 COMMOD9.SECTNICK[I].Length >= 3 ? COMMOD9.SECTNICK[I].Substring(0, 3) : COMMOD9.SECTNICK[I], COMMOD9.SCJPEN[I], str.Trim().Length >= 7 ? str.Trim().Substring(0, 7) : str.Trim(),
			2> Line 287
			//COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2} levels from {3} possible pairs in {4} section levels", (I + 1), COMMOD9.SECTNICK[I].Substring(0, 3), (int)COMMOD9.SCJPEN[I], (COMMOD9.PAIRJ[I] + 1), 
			//    (COMMOD9.HLEVEL[I] + 1)));
			COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2} levels from {3} possible pairs in {4} section levels", 
                                (I + 1), 
                                COMMOD9.SECTNICK[I].Length >= 3 ? COMMOD9.SECTNICK[I].Substring(0, 3) : COMMOD9.SECTNICK[I], 
                                (int)COMMOD9.SCJPEN[I], 
                                (COMMOD9.PAIRJ[I] + 1),
                                (COMMOD9.HLEVEL[I] + 1)
			));
			3> Line 304 -                             
			//COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2} contributing pairs from {3} possible pairs in {4} section levels", (I + 1), COMMOD9.SECTNICK[I].Substring(0, 3), (int)COMMOD9.SCJPEN[I], (COMMOD9.PAIRJ[I] + 1),
			//         (COMMOD9.HLEVEL[I] + 1)));
            COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2} contributing pairs from {3} possible pairs in {4} section levels", (I + 1), COMMOD9.SECTNICK[I].Length >= 3 ? COMMOD9.SECTNICK[I].Substring(0, 3) : COMMOD9.SECTNICK[I], (int)COMMOD9.SCJPEN[I], (COMMOD9.PAIRJ[I] + 1),
                                     (COMMOD9.HLEVEL[I] + 1)));
			4> Line 337 -
            //COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2:0.00} meters  ({3:0.00} meters with {4} event levels)", (I + 1), COMMOD9.SECTNICK[I].Substring(0, 3), COMMOD9.COLPEN[I],
            //    COMMOD9.VALEVEL[COMMOD9.HLEVEL[I], I] - COMMOD9.VALEVEL[0, I],
            //    (COMMOD9.HLEVEL[I] + 1)));
            COMMOD9.OutmainSB.AppendLine(string.Format("  {0} {{{1}}} {2:0.00} meters  ({3:0.00} meters with {4} event levels)", 
                            (I + 1), 
                            COMMOD9.SECTNICK[I].Length >= 3 ? COMMOD9.SECTNICK[I].Substring(0, 3) : COMMOD9.SECTNICK[I], COMMOD9.COLPEN[I],
                            COMMOD9.VALEVEL[COMMOD9.HLEVEL[I], I] - COMMOD9.VALEVEL[0, I],
                            (COMMOD9.HLEVEL[I] + 1)
            ));
			5> Line 373 - 
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

	5, 287sec testing setting
	test_initPerm;SEC7;REPACEPERM;FAKE_IJRANKE_DOUBLE

	6, self-validation for sec7
	 STEPS=500
		TRIALS=100 => both to 0

		 STARTYPE='RAND' => 'FILE'
		 
  STARTFILE='soln.dat' => STARTFILE='bestsoln_7sec.dat'

  ;WRITESULT -> write content into file to avoid GUI+ bug

  
	7, self-validation for sec7, already fixed the bugs for parallelization