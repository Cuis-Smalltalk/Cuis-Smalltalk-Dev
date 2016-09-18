'From Cuis 4.2 of 25 July 2013 [latest update: #2918] on 23 August 2016 at 11:38:26 am'!

!Integer methodsFor: 'benchmarks' stamp: 'jmv 8/23/2016 11:37:09'!
tinyBenchmarks
	"Report the results of running the two tiny Squeak benchmarks.
	ar 9/10/1999: Adjusted to run at least 1 sec to get more stable results
	0 tinyBenchmarks
		292 MHz G3 Mac: 				        			 22,727,272 bytecodes/sec; 			    984,169 sends/sec
		400 MHz PII/Win98:  			        			 18,028,169 bytecodes/sec; 			1,081,272 sends/sec
		RasPi2 - StackVM: 								 37,758,112 bytecodes/sec; 		       2,412,667 sends/sec
		Raspi2- Spur: 									     157,441,574 bytecodes/sec; 		     10,946,039 sends/sec
		1.6GHz Atom/Win7 - Cog: 					     201,099,764 bytecodes/sec; 		    28,197,075 sends/sec
		1.5GHz AMD A4-5000 APU - Cog: 			    390,243,902 bytecodes/sec; 		    47,507,997 sends/sec
		3.1GHz Core i3-2100 - Cog:  				1,203,290,246 bytecodes/sec; 		165,723,327 sends/sec
		3.1GHz Core i3-2100 - Spur:  				2,042,892,768 bytecodes/sec; 		127,837,794 sends/sec
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

