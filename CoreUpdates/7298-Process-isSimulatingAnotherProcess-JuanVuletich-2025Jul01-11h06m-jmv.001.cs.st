'From Cuis7.5 [latest update: #7296] on 1 July 2025 at 11:10:00 am'!

!Process methodsFor: 'simulate execution' stamp: 'jmv 7/1/2025 11:09:51'!
isSimulatingAnotherProcess
	"Answer true if we are a debugger process, simulating the execution of another."

	^self effectiveProcess ~~ self! !

