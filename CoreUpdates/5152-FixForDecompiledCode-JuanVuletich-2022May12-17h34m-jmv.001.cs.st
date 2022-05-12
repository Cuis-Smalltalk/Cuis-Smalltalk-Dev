'From Cuis 6.0 [latest update: #5151] on 12 May 2022 at 5:34:51 pm'!

!Point methodsFor: 'printing' stamp: 'jmv 5/12/2022 17:24:12'!
printOn: aStream 
	"The receiver prints on aStream in terms of infix notation."

	x printOn: aStream.
	aStream nextPut: $@; space.
	y printOn: aStream! !

