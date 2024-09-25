'From Cuis7.1 [latest update: #6742] on 25 September 2024 at 10:46:00 AM'!

!UnicodeString methodsFor: 'copying' stamp: 'jfr 9/25/2024 10:43:24'!
copyFrom: startIndex to: stopIndex
	"Return a new string, a copy of me from startIndex to stopIndex, both character indices.
	Signal SubscriptOutOfBounds or CollectionIsEmpty in case of illegal indices."
	
	| startByteIndex stopByteIndex |
	startIndex > self size ifTrue: [ ^ self species empty ].
	stopIndex <= 0 ifTrue: [ ^ self species empty ].
	self boundsCheck: startIndex. 
	self boundsCheck: stopIndex. 
	startByteIndex := self byteIndexAt: startIndex.
	stopByteIndex := self byteIndexAt: stopIndex.
	stopByteIndex := stopByteIndex + (self encodedByteCountAt: stopByteIndex) - 1.
	^ self species
		fromUtf8Bytes: (bytes copyFrom: startByteIndex to: stopByteIndex)
		codePointCount: stopIndex-startIndex+1.! !

