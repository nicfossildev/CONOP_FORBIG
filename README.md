CONOP_FORBIG
============
1, CONOP_FORBIG repository description
		*) CONOP - CONOP C# with GUI mode
		As GDI+ in GUI is drawn via strong referenced Windows Form object in STAThread, any events with in-validation of GUI			device will freeze/destory screen attribute, so we migrated source code in GUI mode to pure console mode
		
		*) CONOP_CONSOLE - CONOP C# with CONSOLE mode
		by default, CONOP_CONSOLE application will only run under Console mode. **** however, if we want to recovery to GUI 			mode, you can add "GUIMODE" in compliation macro. - not to recommended *** 
		
2, Macro control for multi-input data support
		take CONOP_CONSOLE project as example, select "CONOP_CONSOLE" project-> "properties" -> "Build", for "Condtional compliation symbols", you can add customized macro for project control
		
		*) section inputs control
		macro "SEC7", "SEC19", "SEC20", "SEC50", "SEC195", "SEC286", "SEC287" for running test case, target input configuration has been stored into "bin/Debug/<your-input-section-case>-setting" subfolder to avoid repeated changes to switch under different cases
		
		*) enable Parallelization or not
		macro "PARALLEL" is used to enable getSctPen parallelization via Microsoft TPL
		
		*) write out penality and permuation
		macro "WRITERESULT" is used to ouptut best permutation and penality into BestPerm.dat and Penality.dat files after calcuatoin
		
		for normal case, you can select CONOP_CONSOLE, add "SEC<N>;PARALLEL;WRITERESULT" for contional macro to run case
		
		>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> following for HANA CONOP context generation >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		*) others : for CONOP hana data context generation and context-comparsion
		test_initPerm - for read initperm in local C# folder for SA algorithm;
		REPACEPERM - for replace initperm for SA algorithm from local input data csv; 
		FAKE_IJRANKE_DOUBLE - for reading fake i,jrank double from csv to generate the same context between C# and C++
		
		
		
