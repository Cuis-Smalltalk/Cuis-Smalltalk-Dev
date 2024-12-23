'From Cuis7.3 [latest update: #6945] on 23 December 2024 at 11:04:32 am'!

!PositionableStream methodsFor: 'accessing' stamp: 'jmv 5/22/2022 21:13:39'!
upToAny: aCollection 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of any objects in the given collection in the receiver. If 
	any of these is not in the collection, answer the entire rest of the receiver."
	| element prevPos |
	^self collectionSpecies streamContents: [ :strm |
		[self atEnd or: [
			prevPos := self position.
			element := self next.
			aCollection includes: element ]]
				whileFalse: [strm nextPut: element].
		(aCollection includes: element)
			ifTrue: [self position: prevPos]].! !

