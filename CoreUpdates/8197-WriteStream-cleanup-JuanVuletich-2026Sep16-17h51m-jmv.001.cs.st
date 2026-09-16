'From Cuis7.9 [latest update: #8193] on 16 September 2026 at 6:09:08 pm'!

!WriteStream methodsFor: 'writing' stamp: 'jmv 16/Sep/2026, 9:06:56 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
nextPut: anObject
	"Insert the argument at the next position in the Stream represented by the receiver."

	(anObject isCharacter and: [anObject codePoint > 255 and: [collection canHoldAnyCodePoint not]]) ifTrue: [
		collection := collection thatCanHoldAnyCodePoint ].
	position := position + 1.
	position > collection size
		ifTrue: [ self growTo: position ].
	collection at: position put: anObject.! !


!LimitedWriteStream methodsFor: 'accessing' stamp: 'jmv 16/Sep/2026, 7:52:19 pm (UTC)'!
nextPut: anObject 
	"Ensure that the limit is not exceeded"

	position >= limit
		ifTrue: [limitBlock value]
		ifFalse: [super nextPut: anObject].! !

!methodRemoval: Utf8EncodedWriteStream #pastEndPut: stamp: 'jmv 16/Sep/2026, 8:54:29 pm (UTC)'!
Utf8EncodedWriteStream removeSelector: #pastEndPut:!
!methodRemoval: UniFileStream #pastEndPut: stamp: 'jmv 16/Sep/2026, 8:54:31 pm (UTC)'!
UniFileStream removeSelector: #pastEndPut:!
!methodRemoval: LimitedWriteStream #pastEndPut: stamp: 'jmv 16/Sep/2026, 8:56:05 pm (UTC)'!
LimitedWriteStream removeSelector: #pastEndPut:!
!methodRemoval: WriteStream #pastEndPut: stamp: 'jmv 16/Sep/2026, 8:56:05 pm (UTC)'!
WriteStream removeSelector: #pastEndPut:!
