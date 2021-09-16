'From Cuis 5.0 [latest update: #4863] on 16 September 2021 at 2:36:00 pm'!

!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 9/16/2021 14:34:45'!
millisecondsToRunWithoutGC
	"Answer the number of milliseconds taken to execute this block without GC time."

	^(Smalltalk vmParameterAt: 8) + 
		(Smalltalk vmParameterAt: 10) +
		self millisecondsToRun -
		(Smalltalk vmParameterAt: 8) - 
		(Smalltalk vmParameterAt: 10)
! !


!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 9/16/2021 14:34:26'!
durationToRun
	"Answer the duration taken to execute this block."

	^ Duration milliSeconds: self millisecondsToRun.! !


!TestCase methodsFor: 'assertions' stamp: 'jmv 9/16/2021 14:35:21'!
should: aClosure notTakeMoreThan: aLimit 

	| millisecondsLimit |

	millisecondsLimit := aLimit totalMilliseconds.
	self assert: aClosure millisecondsToRun <= millisecondsLimit
		description: [ 'Took more than ', millisecondsLimit printString, ' milliseconds' ].! !

!methodRemoval: BlockClosure #timeToRunWithoutGC stamp: 'jmv 9/16/2021 14:35:30'!
BlockClosure removeSelector: #timeToRunWithoutGC!
