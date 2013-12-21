'From Cuis 4.2 of 25 July 2013 [latest update: #1891] on 21 December 2013 at 3:18:42.875877 pm'!

!WorldState methodsFor: 'update cycle' stamp: 'jmv 12/21/2013 15:18'!
isUpdateCycleSlow
	"Answer true if Morphic is getting too slow, and FPS is dropping."

	^Time localMillisecondClock-lastCycleTime > (pause*5)! !

