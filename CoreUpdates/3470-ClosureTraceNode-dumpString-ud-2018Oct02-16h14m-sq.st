'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 2 October 2018 at 4:54:47 pm'!

!ClosureTraceNode methodsFor: 'printing' stamp: 'sqr 10/2/2018 16:14:51'!
dumpOn: aStream

	self dumpOn: aStream depth: 0! !

!ClosureTraceNode methodsFor: 'printing' stamp: 'sqr 10/2/2018 16:54:39'!
dumpOn: aStream depth: anInteger

	anInteger timesRepeat: [aStream tab].
	self printOn: aStream.
	aStream newLine.
	self paths do: [:each | each dumpOn: aStream depth: anInteger + 1]! !

!ClosureTraceNode methodsFor: 'printing' stamp: 'sqr 10/2/2018 16:16:41'!
dumpString

	| answer |
	answer := String new writeStream.
	self dumpOn: answer.
	^answer contents! !

