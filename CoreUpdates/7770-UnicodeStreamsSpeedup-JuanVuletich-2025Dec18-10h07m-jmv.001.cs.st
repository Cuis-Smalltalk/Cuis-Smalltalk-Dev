'From Cuis7.5 [latest update: #7768] on 18 December 2025 at 10:09:47 am'!

!WriteStream methodsFor: 'writing' library: #MinimalKernel01 stamp: 'jmv 18/Dec/2025 09:38:24'!
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
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: toBeAdded startingAt: 1.
	position := newEnd.! !

!WriteStream methodsFor: 'writing' stamp: 'jmv 18/Dec/2025 09:40:29'!
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
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection
		replaceFrom: position+1
		to: newEnd 
		withString: toBeAdded
		attributes: attributesArray
		startingAt: 1.
	position := newEnd! !

