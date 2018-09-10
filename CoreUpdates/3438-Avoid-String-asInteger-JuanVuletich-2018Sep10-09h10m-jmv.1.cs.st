'From Cuis 5.0 of 7 November 2016 [latest update: #3437] on 10 September 2018 at 9:12:16 am'!

!Time class methodsFor: 'smalltalk-80' stamp: 'jmv 9/10/2018 09:11:34'!
readFrom: aStream
	"Read a Time from the stream in the form:
		<hour>:<minute>:<second> <am/pm>

	<minute>, <second> or <am/pm> may be omitted.  e.g. 1:59:30 pm; 8AM; 15:30"

	| hour minute second ampm nanos nanosBuffer |
	hour := Integer readFrom: aStream.
	minute := 0.
	second := 0.
	nanosBuffer := '000000000' copy.
	nanos := WriteStream on: nanosBuffer.
	(aStream peekFor: $:) 
		ifTrue: [
			minute := Integer readFrom: aStream.
			(aStream peekFor: $:) 
				ifTrue: [
					second := Integer readFrom: aStream].
					(aStream peekFor: $.)
						ifTrue: [
							[aStream atEnd not and: [aStream peek isDigit]]
								whileTrue: [nanos nextPut: aStream next]]].
	aStream skipSeparators.
	(aStream atEnd not and: [aStream peek isLetter]) ifTrue: 
		[ampm := aStream next asLowercase.
	
	(ampm = $p and: [hour < 12]) ifTrue: [hour := hour + 12].
		(ampm = $a and: [hour = 12]) ifTrue: [hour := 0].
	
	(aStream peekFor: $m) ifFalse: [aStream peekFor: $M ]].
	^ self 
		hour: hour 
		minute: minute 
		second: second 
		nanoSecond: nanosBuffer asNumber

	"Time readFrom: (ReadStream on: '2:23:09 pm')"! !

