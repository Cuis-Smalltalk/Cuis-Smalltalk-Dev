'From Cuis7.9 [latest update: #8192] on 16 September 2026 at 4:47:47 pm'!

!WriteStream methodsFor: 'writing' stamp: 'jmv 16/Sep/2026, 7:18:54 pm (UTC)'!
next: anInteger putAll: aCollection startingAt: startIndex
	"Store the next anInteger elements from the given collection."


	| newEnd |
	collection class == aCollection class ifFalse:
		[^ super next: anInteger putAll: aCollection startingAt: startIndex].

	newEnd := position + anInteger.
	newEnd > collection size ifTrue:
		[self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: startIndex.
	position := newEnd.

	^aCollection! !

!WriteStream methodsFor: 'writing' stamp: 'jmv 16/Sep/2026, 7:20:42 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
nextPut: anObject 
	"Insert the argument at the next position in the Stream represented by the receiver."

	(anObject isCharacter and: [anObject codePoint > 255 and: [collection canHoldAnyCodePoint not]]) ifTrue: [
		collection := collection thatCanHoldAnyCodePoint ].
	position >= collection size
		ifTrue: [
			self pastEndPut: anObject ]
		ifFalse: [
			position := position + 1.
			collection at: position put: anObject ].! !

!WriteStream methodsFor: 'writing' stamp: 'jmv 16/Sep/2026, 7:22:08 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
nextPutAll: aCollection

	| toBeAdded newEnd |
	toBeAdded := aCollection.
	(toBeAdded canHoldAnyCodePoint == collection canHoldAnyCodePoint) ifFalse: [
		toBeAdded canHoldAnyCodePoint
			ifTrue: [collection := collection thatCanHoldAnyCodePoint]
			ifFalse: [toBeAdded := toBeAdded asUnicodeStringOrText]].

	(self isCompatibleWithContents: toBeAdded)
		ifFalse: [^ super nextPutAll: toBeAdded].

	newEnd := position + toBeAdded size.
	newEnd > collection size ifTrue: [
		self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: toBeAdded startingAt: 1.
	position := newEnd.! !

!WriteStream methodsFor: 'writing' stamp: 'jmv 16/Sep/2026, 7:18:51 pm (UTC)'!
nextPutAllString: aString withAttributes: attributesArray

	| toBeAdded newEnd |
	toBeAdded := aString.
	(toBeAdded canHoldAnyCodePoint == collection canHoldAnyCodePoint) ifFalse: [
		toBeAdded canHoldAnyCodePoint
			ifTrue: [collection := collection thatCanHoldAnyCodePoint]
			ifFalse: [toBeAdded := toBeAdded asUnicodeStringOrText]].
	(self isCompatibleWithContents: toBeAdded)
		ifFalse: [ ^ self nextPutAll: toBeAdded ].

	newEnd := position + toBeAdded size.
	newEnd > collection size ifTrue: [
		self growTo: newEnd + 10].

	collection
		replaceFrom: position+1
		to: newEnd 
		withString: toBeAdded
		attributes: attributesArray
		startingAt: 1.
	position := newEnd! !


!LimitedWriteStream methodsFor: 'accessing' stamp: 'jmv 16/Sep/2026, 7:21:44 pm (UTC)'!
nextPutAll: aCollection

	position + aCollection size > limit ifTrue: [
		super nextPutAll: (aCollection copyFrom: 1 to: (limit - position max: 0)).
		limitBlock value.
		^self ].
	super nextPutAll: aCollection.! !


!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 16/Sep/2026, 7:18:12 pm (UTC)'!
nextPutByte: aByte 
	"Insert the argument at the next position in the Stream represented by the receiver."

	numberOfCodePointsWritten := nil.	"We can no longer know"
	position := position + 1.
	position > collection size
		ifTrue: [ self growTo: position ].
	collection at: position put: aByte .
	lastWrittenElement := aByte.! !

!methodRemoval: Utf8EncodedWriteStream #pastEndPutByte: stamp: 'jmv 16/Sep/2026, 7:45:39 pm (UTC)'!
Utf8EncodedWriteStream removeSelector: #pastEndPutByte:!

!UniFileStream reorganize!
('open/close' close closed ensureOpen open:forWrite: openReadOnly reopen)
('properties-setting' isReadOnly readOnly readWrite)
('access' directory fileEntry fileID fullName localName name peekFor: printOn: reset size)
('read, write, position' atEnd crc16 flush next next: next:putAll:startingAt: nextBytes:into:startingAt: nextPut: nextPutAll: nextPutByte: nextPutCodePoint: nextUtf8BytesAndCodePointInto:into: nextWordsInto: padSpacesTo: padToEndIfCantTruncate peek peekLast position position: readInto:startingAt:count: resetToStart setToEnd skip skip: skipBack truncate truncate:)
('testing' isFileStream)
('primitives' primAtEnd: primClose: primCloseNoError: primFlush: primGetPosition: primOpen:writable: primRead:into:startingAt:count: primSetPosition:to: primSize: primSizeNoError: primTruncate:to: primWrite:from:startingAt:count:)
('registry' register unregister)
('finalization' finalize)
('private' bytesUpToEnd disableReadBuffering enableReadBuffering flushReadBuffer nextByte nextBytes: nextCodePoint pastEndPut:)
('printing' longPrintOn: longPrintOn:limitedTo:indent: storeOn:)
('accessing' collectionSpecies contents contentsOfEntireFile upToEnd)
('reading modes' isBinary useByteCharacters useBytes useCodePoints writeOnly)
!

