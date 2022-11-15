'From Cuis 6.0 [latest update: #5552] on 15 November 2022 at 10:33:41 am'!

!Time class methodsFor: 'smalltalk-80' stamp: 'jmv 11/15/2022 10:33:12'!
readFrom: aStream
	"Read a Time from the stream in the form:
		<hour>:<minute>:<second> <am/pm>

	<minute>, <second> or <am/pm> may be omitted.  e.g. 1:59:30 pm; 8AM; 15:30"

	| hour minute second nanos ampm |
	hour _ Integer readFrom: aStream.
	minute _ 0.
	second _ 0.
	nanos _ 0.
	(aStream peekFor: $:) 
		ifTrue: [
			minute _ Integer readFrom: aStream.
			(aStream peekFor: $:) ifTrue: [
				second _ Integer readFrom: aStream.
				(aStream peekFor: $.)
					ifTrue: [ | nanosStart scale |
						nanosStart _ aStream position.
						nanos _ Integer readFrom: aStream.
						"As if there were trailing zeros to fill 9 positions"
						scale _ 10 raisedToInteger: nanosStart + 9 - aStream position.
						nanos _ nanos * scale ]]].
	aStream skipSeparators.
	(aStream atEnd not and: [aStream peek isLetter]) ifTrue: [
		ampm _ aStream next asLowercase.
	
	(ampm = $p and: [hour < 12]) ifTrue: [hour _ hour + 12].
		(ampm = $a and: [hour = 12]) ifTrue: [hour _ 0].
	
	(aStream peekFor: $m) ifFalse: [aStream peekFor: $M ]].
	^ self 
		hour: hour 
		minute: minute 
		second: second 
		nanoSecond: nanos.

	"
	Time readFrom: '2:23:09 pm' readStream
	Time readFrom: '2:23:09.0 pm' readStream
	Time readFrom: '2:23:09.123402 pm' readStream
	Time readFrom: '2:23:09.00123402 pm' readStream
	Time readFrom: '2:23:09.001234020 pm' readStream
	Time readFrom: '2:23:09.000123402 pm' readStream
	"! !

