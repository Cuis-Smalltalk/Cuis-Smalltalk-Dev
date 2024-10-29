'From Cuis7.1 [latest update: #6796] on 29 October 2024 at 7:32:29 am'!

!PositionableStream methodsFor: 'accessing' stamp: 'jmv 10/29/2024 07:30:06'!
upToAll: aCollection
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

	| startPos endMatch result |
	startPos := self position.
	(self matchStart: aCollection)
		ifNotNil: [ :matchStart |
			endMatch := self position.
			self position: startPos.
			result := self collectionSpecies streamContents: [ :ws | | n |
				[
					n := self next.
					self position < matchStart] whileTrue: [
						ws nextPut: n ]].
			self position: endMatch.
			^ result ]
		ifNil: [
			self position: startPos.
			^ self upToEnd ].! !

