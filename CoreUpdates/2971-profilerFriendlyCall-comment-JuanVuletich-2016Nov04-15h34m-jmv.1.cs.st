'From Cuis 4.2 of 25 July 2013 [latest update: #2970] on 4 November 2016 at 3:38:46 pm'!

!Object methodsFor: 'profiler support' stamp: 'jmv 11/4/2016 15:38:26'!
profilerFriendlyCall: aBlock
	"In AndreasSystemProfiler, usually primitives are reported as children of the wrong node.
	The reason is that while the primitive is recorded, it is only taken into account at next suspension point.
	
	For long running primitives, and maybe only while profiling, this method provides a workaround.
	Evaluate and compare

		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 10000]].

	Also see #runProfilerProcess, and this more complex example (you might want to make it use #profilerFriendlyCall:)
		AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]

	Keep in mind there is a performance penaly each time this method is called.
	Consider doing it only for long-running primitives, or only while profiling code.
	"

	"The apparently useless loop is to ensure we have a suspension point in this method, after block evaluation.
	Suspension points are activation of message sends and bytecode back jumps."
	| primResult primWasCalled |
	primWasCalled _ false.
	[ primWasCalled ] whileFalse: [
		primResult _ aBlock value.
		primWasCalled _ true ].
	^primResult! !

