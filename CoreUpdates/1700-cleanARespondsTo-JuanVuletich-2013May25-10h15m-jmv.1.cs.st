'From Cuis 4.1 of 12 December 2012 [latest update: #1698] on 25 May 2013 at 10:17:17.755369 am'!

!StreamingMonoSound methodsFor: 'playing' stamp: 'jmv 5/15/2013 08:09'!
samplesRemaining
	"Answer the number of samples remaining to be played."

	| result |
	(stream isNil or: [stream closed]) ifTrue: [^ 0].
	self repeat ifTrue: [^ 1000000].
	result _ (totalSamples - self currentSampleIndex) max: 0.
	result <= 0 ifTrue: [
		stream close.
		mixer _ nil.
		codec _ nil ].
	^ result! !

!methodRemoval: StreamingMonoSound #closeFile!
StreamingMonoSound removeSelector: #closeFile!
