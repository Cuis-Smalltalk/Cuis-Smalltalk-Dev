'From Cuis7.3 [latest update: #7006] on 25 January 2025 at 11:06:20 am'!

!Array methodsFor: 'printing' stamp: 'jmv 1/25/2025 11:06:02'!
storeAsBraceFormOn: aStream
	"
	{(1/3)} storeString
	"
	aStream nextPut: ${.
	self do: [:el | el storeOn: aStream] separatedBy: [ aStream nextPutAll: ' . '].
	aStream nextPut: $}! !


!Array methodsFor: 'printing' stamp: 'jmv 1/25/2025 11:05:19'!
storeOn: aStream
	"Use the literal form if possible."
	self shouldBePrintedAsLiteral
		ifTrue:  [self printAsLiteralOn: aStream]
		ifFalse: [self storeAsBraceFormOn: aStream]! !

