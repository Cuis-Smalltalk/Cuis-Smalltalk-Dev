'From Haver 6.0 [latest update: #5496] on 20 October 2022 at 5:12:50 pm'!

!Collection methodsFor: 'printing' stamp: 'KLG 10/20/2022 17:11:30'!
printElementsOn: aStream
	"Print my elements on aStream."

	aStream nextPut: $(.
	self do: [ :element | aStream print: element ] separatedBy: [ aStream space ].
	aStream nextPut: $)! !

