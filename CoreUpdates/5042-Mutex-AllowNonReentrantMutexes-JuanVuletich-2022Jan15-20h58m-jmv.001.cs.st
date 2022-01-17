'From Cuis 6.0 [latest update: #5041] on 15 January 2022 at 9:08:32 pm'!

!Mutex methodsFor: 'mutual exclusion' stamp: 'jmv 1/15/2022 21:06:47'!
critical: aBlock onReentry: reentryBlock
	"Evaluate aBlock protected by the receiver.
	This method allows using the receiver either as a reentrant or non-reentrant Mutex.
	See https://en.wikipedia.org/wiki/Reentrant_mutex

	| m |
	m := Mutex new.
	m critical: [ 1 print. m critical: [ 2 print ] onReentry: ['Reentry attempt!!' print]. 3 print ]. 'Done'.
	"

	| activeProcess |
	activeProcess _ Processor activeProcess.
	activeProcess == owner ifTrue: [^reentryBlock value].
	^semaphore critical: [
		owner _ activeProcess.
		aBlock ensure: [owner _ nil]].! !

!Mutex methodsFor: 'mutual exclusion' stamp: 'jmv 1/15/2022 21:05:12'!
criticalNonReentrant: aBlock
	"Evaluate aBlock protected by the receiver.
	Provides non-reentrant mutex behavior: Raise an error if attempted to reenter from same process.
	See https://en.wikipedia.org/wiki/Reentrant_mutex

	| m |
	m := Mutex new.
	m criticalNonReentrant: [ 1 print. m criticalNonReentrant: [ 2 print ]. 3 print ]. 'Done'.
	"

	^self critical: aBlock onReentry: [ self error: 'Mutex reentry attempted' ]! !


!Mutex methodsFor: 'mutual exclusion' stamp: 'jmv 1/15/2022 21:04:14'!
critical: aBlock
	"Evaluate aBlock protected by the receiver.
	Provides reentrant mutex behavior: Allow reentry from the same process.
	See https://en.wikipedia.org/wiki/Reentrant_mutex

	| m |
	m := Mutex new.
	m critical: [ 1 print. m critical: [ 2 print ]. 3 print ]. 'Done'.
	"

	^self critical: aBlock onReentry: aBlock! !

