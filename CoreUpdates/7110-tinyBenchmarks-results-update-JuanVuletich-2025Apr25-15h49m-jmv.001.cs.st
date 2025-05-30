'From Cuis7.3 [latest update: #7149] on 25 April 2025 at 3:51:42 pm'!

!Integer methodsFor: 'benchmarks' stamp: 'jmv 4/25/2025 15:50:31'!
tinyBenchmarks
	"Report the results of running the two tiny Squeak benchmarks.
	ar 9/10/1999: Adjusted to run at least 1 sec to get more stable results
	0 tinyBenchmarks

For Pre-Squeak systems, measurements or estimations from
	https://smalltalkzoo.computerhistory.org/papers/EvolutionOfSmalltalk.pdf
										('The evolution of Smalltalk: from Smalltalk-72 through Squeak' by Dan Ingalls, p.98)
	http://wiki.c2.com/?GreenBook
	http://www.wirfs-brock.com/allen/things/smalltalk-things/tektronix-smalltalk-document-archive/1982-qtr4-magnolia-perf-graph.pdf
	http://www.wirfs-brock.com/allen/things/smalltalk-things/tektronix-smalltalk-document-archive/1983-Magnolia-st-perf.pdf

Xerox PARC systems
	Alto Smalltalk-72 5MHz µcode					694 bytecodes/sec						54 sends/sec					7200 µclocks/bytecode
	Alto Smalltalk-74 5MHz µcode					607 bytecodes/sec						46 sends/sec					8200 µclocks/bytecode
	Alto Smalltalk-76 5MHz µcode					16k bytecodes/sec						118 sends/sec				310 µclocks/bytecode
	NoteTaker Smalltalk-78 5MHz 8086				30k bytecodes/sec						250 sends/sec				166.67 clocks/bytecode
	Dorado Smalltalk-76 16.67MHz µcode				1M bytecodes/sec						50k sends/sec				16.67 µClocks/bytecode

Green Book systems
	DEC PDP-11/23									5k bytecodes/sec (Green Book, p.128)
	Apple 5MHz 68000								11k bytecodes/sec (Green Book, p.187, awb)							5000 clocks/bytecode
	VAX-11/780 5MHz C HP Smalltalk					10k bytecodes/sec (Green Book, p.235)								450 clocks/bytecode
	VAX-11/780 5MHz C Berkeley St					17k bytecodes/sec (Green Book, p.203, awb)							300 clocks/bytecode
	DEC VAX-11/780 5MHz assembly					20k bytecodes/sec (Green Book, p.149, awb)							250 clocks/bytecode
	TEK Magnolia 10MHz 68000						50k bytecodes/sec (awb)												200 clocks/bytecode

Squeak & Cuis
	110 MHz PowerPC Mac 8100						4.1M bytecodes/sec;						175k sends/sec				26.8 clocks/bytecode 
	292 MHz G3 Mac:								23M bytecodes/sec;						984k sends/sec				12.7 clocks/bytecode
	400 MHz PII/Win98:								18M bytecodes/sec;						1.08M sends/sec				22.2 clocks/bytecode

	1GHz RISCV Sipeed Lichee RV StackSpur64 		35.56M bytecodes/sec;  					1.56M sends/sec  			28.1 clocks/bytecode

	900MHz RasPi2 - StackVM:						38M bytecodes/sec;						2.41M sends/sec				23.7 clocks/bytecode
	900MHz RasPi2- CogSSpur:						157M bytecodes/sec;						10.95M sends/sec			5.7 clocks/bytecode
	
	1GHz C.H.I.P. (*1) - StackVM:					55M bytecodes/sec;						3.35M sends/sec				18.2 clocks/bytecode
	1GHz C.H.I.P. (*1) - CogSpur:						254M bytecodes/sec;						16.85M sends/sec			3.9 clocks/bytecode
	
	1.2GHz RasPi3B - StackSpur:						44M bytecodes/sec;						2.77M sends/sec				27.2 clocks/bytecode
	1.2GHz RasPi3B - CogSpur:						282M bytecodes/sec;						16.40M sends/sec			4.3 clocks/bytecode

	1.66GHz Atom N450 - Cog:						244M bytecodes/sec;						28.80M sends/sec			6.8 clocks/bytecode
	1.66GHz Atom N450 - CogSpur:					470M bytecodes/sec;						30.75M sends/sec			3.5 clocks/bytecode

	1.33GHz Atom 3735G - Cog:						326M bytecodes/sec;						34.99M sends/sec			4.1 clocks/bytecode
	1.33GHz Atom 3735G - CogSpur:					632M bytecodes/sec;						33.69M sends/sec			2.1 clocks/bytecode

	1.5GHz AMD A4-5000 - Cog:						390M bytecodes/sec;						47.51M sends/sec			3.8 clocks/bytecode
	1.5GHz AMD A4-5000 - CogSpur:					676M bytecodes/sec;						40.67M sends/sec			2.2 clocks/bytecode
	1.5GHz AMD A4-5000 - CogSpur64:				659M bytecodes/sec;						50.34M sends/sec			2.3 clocks/bytecode

	1.5GHz ARM64 RasberryPi4 - CogSpur64: 			732.5M bytecodes/sec;  					53.95M sends/sec			2.0 clocks/bytecode

	2.3GHz Tegra ARM64 (*2) - StackVM:				259M bytecodes/sec;						13.01M sends/sec			8.9 clocks/bytecode
	2.3GHz Tegra ARM64(*2) - CogSpur:				1.08G bytecodes/sec;						64.29M sends/sec			2.1 clocks/bytecode
	2.4GHz RasPi5 (ARM64) 							1.60G bytecodes/sec;  					140.00M sends/sec  			1.5 clocks/bytecode

	3.1GHz Core i3-2100 - Cog:						1.20G bytecodes/sec;						165.72M sends/sec			2.6 clocks/bytecode
	3.1GHz Core i3-2100 - CogSpur:					2.04G bytecodes/sec;						127.84M sends/sec			1.5 clocks/bytecode

	2.70GHz Core i5-6400 -CogSpur64				3.16G bytecodes/sec;						243.32M sends/sec			0.85 clocks/bytecode
	
	2.4GHz Core i9-9980HK 2019-15-MBP 			2.9G bytecodes/sec; 						260M sends/sec 				0.83 clocks/bytecode
	
	4.05GHz Apple M3 MacBook Air					6.17G bytecode/sec; 						405M sends/sec 				0.66 clocks / bytecode

	(*1) C.H.I.P. $9 Computer [Next Thing Co], Allwinner R8 Single-Core ARM Cortex-A8 - 1 GHz
	(*2) ACER Chromebook CB5-311 - NVIDIA Tegra K1 Quad-Core ARM Cortex-A15 'r3'  - 2.3 GHz

100 Dorados make Morphic comfortable and nice to use.
1000 Dorados make Vector Graphics Morphic 3 comfortable and nice to use.
	"
	| t1 t2 r n1 n2 |
	n1 := 1.
	[
		t1 := Time millisecondsToRun: [n1 benchmark].
		t1 < 1000] 
			whileTrue: [n1 := n1 * 2]. "Note: #benchmark's runtime is about O(n)"

	n2 := 28.
	[
		t2 := Time millisecondsToRun: [r := n2 benchFib].
		t2 < 1000] 
			whileTrue: [n2 := n2 + 1]. 
	"Note: #benchFib's runtime is about O(k^n),
		where k is the golden number = (1 + 5 sqrt) / 2 = 1.618...."

	^String streamContents: [ :strm |
		(n1 * 500000 * 1000) // t1 withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
			value printOn: strm fractionDigits: 2.
			strm
				space;
				nextPutAll: unitPrefixName;
				nextPutAll: 'Bytecodes/second; ' ].
		(r * 1000) // t2 withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
			value printOn: strm fractionDigits: 2.
			strm
				space;
				nextPutAll: unitPrefixName;
				nextPutAll: 'Sends/second' ]]! !

