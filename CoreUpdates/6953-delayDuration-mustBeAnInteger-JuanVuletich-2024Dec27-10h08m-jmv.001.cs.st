'From Cuis7.3 [latest update: #6952] on 27 December 2024 at 10:12:34 am'!

!Delay methodsFor: 'private' stamp: 'jmv 12/27/2024 10:11:02'!
setDelay: millisecondCount 
	"Private!! Initialize this delay to signal the given semaphore after the given number of milliseconds."

	delayDuration := millisecondCount asInteger! !

!Delay methodsFor: 'private' stamp: 'jmv 12/27/2024 10:11:39'!
setDelay: millisecondCount forSemaphore: aSemaphore
	"Private!! Initialize this delay to signal the given semaphore after the given number of milliseconds."

	delayDuration := millisecondCount asInteger.
	delaySemaphore := aSemaphore.
	beingWaitedOn := false.
! !

