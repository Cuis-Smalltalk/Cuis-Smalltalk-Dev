'From Cuis 5.0 of 7 November 2016 [latest update: #3076] on 13 May 2017 at 12:54:11 pm'!

!Integer methodsFor: 'printing' stamp: 'jmv 5/9/2017 19:45:38'!
printOn: aStream length: minimum zeroPadded: zeroFlag
	"
	7 printOn: Transcript length: 4 padded: true. Transcript newLine.
	"
	self printOn: aStream base: 10 length: minimum padded: zeroFlag! !


!Character methodsFor: 'accessing' stamp: 'jmv 5/9/2017 19:49:32'!
digitValue
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 
	otherwise. This is used to parse literal numbers of radix 2-36.
	$0 numericValue = 48
	$9 numericValue = 57
	$A numericValue = 65
	$Z numericValue = 90 
	$7 digitValue = 7 
	"

	| nv |
	nv _ self numericValue.
	(nv between: 48 and: 57)
		ifTrue: [ ^ nv - 48 ].
	(nv between: 65 and: 90)
		ifTrue: [ ^ nv - 55 ].
	^ -1! !

