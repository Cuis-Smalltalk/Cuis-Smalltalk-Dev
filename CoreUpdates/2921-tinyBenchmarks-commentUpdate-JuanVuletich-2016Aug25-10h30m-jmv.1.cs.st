'From Cuis 4.2 of 25 July 2013 [latest update: #2919] on 25 August 2016 at 10:47:08 am'!

!Integer methodsFor: 'benchmarks' stamp: 'jmv 8/25/2016 10:46:48'!
tinyBenchmarks
	"Report the results of running the two tiny Squeak benchmarks.
	ar 9/10/1999: Adjusted to run at least 1 sec to get more stable results
	0 tinyBenchmarks
		292 MHz G3 Mac: 				        			 22,727,272 bytecodes/sec; 			    984,169 sends/sec
		400 MHz PII/Win98:  			        			 18,028,169 bytecodes/sec; 			1,081,272 sends/sec

		RasPi2 - StackVM: 								 37,758,112 bytecodes/sec; 		       2,412,667 sends/sec
		Raspi2- Spur: 									     157,441,574 bytecodes/sec; 		     10,946,039 sends/sec
		
		C.H.I.P. (*1) - StackVM: 							55,315,471 bytecodes/sec;  			3,348,667 sends/sec
		C.H.I.P. (*1) - Cog: 					 		    253,716,551 bytecodes/sec; 		    16,853,816 sends/sec
			
		1.6GHz Atom/Win7 - Cog: 					     201,099,764 bytecodes/sec; 		    28,197,075 sends/sec
		1.5GHz AMD A4-5000 APU - Cog: 			    390,243,902 bytecodes/sec; 		    47,507,997 sends/sec

		2.3GHz Tegra (*2) - StackVM: 				    258,847,320 bytecodes/sec; 		    13,014,759 sends/sec	
		2.3GHz Tegra (*2) - Spur: 					1,083,024,854 bytecodes/sec; 		    64,289,750 sends/sec

		3.1GHz Core i3-2100 - Cog:  				1,203,290,246 bytecodes/sec; 		165,723,327 sends/sec
		3.1GHz Core i3-2100 - Spur:  				2,042,892,768 bytecodes/sec; 		127,837,794 sends/sec

		(*1) C.H.I.P. $9 Computer [Next Thing Co], Allwinner R8 Single-Core ARM Cortex-A8 - 1 GHz
		(*1) ACER Chromebook CB5-311 - NVIDIA Tegra K1 Quad-Core ARM Cortex-A15 'r3'  - 2.3 GHz
		
	It is interesting to note that Spur for the first time brings ARM hardware (RasPi and C.H.I.P.) into a 'Morphic is confortable and nice to use' level of performance.
	"
	| t1 t2 r n1 n2 |
	n1 _ 1.
	[
		t1 _ Time millisecondsToRun: [n1 benchmark].
		t1 < 1000] 
			whileTrue:[n1 _ n1 * 2]. "Note: #benchmark's runtime is about O(n)"

	n2 _ 28.
	[
		t2 _ Time millisecondsToRun: [r _ n2 benchFib].
		t2 < 1000] 
			whileTrue:[n2 _ n2 + 1]. 
	"Note: #benchFib's runtime is about O(k^n),
		where k is the golden number = (1 + 5 sqrt) / 2 = 1.618...."

	^ ((n1 * 500000 * 1000) // t1) printStringWithCommas, ' bytecodes/sec; ',
	  ((r * 1000) // t2) printStringWithCommas, ' sends/sec'! !

