'From Cuis 5.0 [latest update: #4579] on 1 May 2021 at 11:38:01 pm'!

!ProgressInitiationException methodsFor: 'exceptionDescription' stamp: 'nice 5/1/2021 23:36:25'!
sendNotificationsTo: aNewBlock

	signalContext resumeEvaluating: [
		workBlock value: [ :barVal |
			aNewBlock value: minVal value: maxVal value: barVal
		]
	]
! !

