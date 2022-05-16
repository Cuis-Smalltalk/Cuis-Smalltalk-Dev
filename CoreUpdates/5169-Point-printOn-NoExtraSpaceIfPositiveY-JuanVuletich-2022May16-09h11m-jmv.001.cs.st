'From Cuis 6.0 [latest update: #5168] on 16 May 2022 at 9:11:51 am'!

!Point methodsFor: 'printing' stamp: 'jmv 5/16/2022 09:05:30'!
printOn: aStream 
	"The receiver prints on aStream in terms of infix notation."

	x printOn: aStream.
	aStream nextPut: $@.
	y negative ifTrue: [ aStream space ].
	y printOn: aStream! !

