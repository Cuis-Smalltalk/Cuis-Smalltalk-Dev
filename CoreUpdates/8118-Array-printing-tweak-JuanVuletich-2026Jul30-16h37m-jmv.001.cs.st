'From Cuis7.9 [latest update: #8117] on 30 July 2026 at 4:37:25 pm'!

!Array methodsFor: 'printing' stamp: 'eem 3/Apr/2017, 6:42:00 pm (UTC)'!
printAsBraceFormOn: aStream
	aStream nextPut: ${.
	self do: [:el | el printOn: aStream] separatedBy: [ aStream nextPutAll: '. '].
	aStream nextPut: $}! !

!Array methodsFor: 'printing' stamp: 'jmv 25/Jan/2025, 2:06:02 pm (UTC)'!
storeAsBraceFormOn: aStream
	"
	{(1/3)} storeString
	"
	aStream nextPut: ${.
	self do: [:el | el storeOn: aStream] separatedBy: [ aStream nextPutAll: '. '].
	aStream nextPut: $}! !

