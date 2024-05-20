'From Cuis7.1 [latest update: #6379] on 20 May 2024 at 9:54:15 am'!

!PositionableStream methodsFor: 'positioning' stamp: 'jmv 5/19/2024 19:52:39'!
matchStart: subCollection
	"Set the access position of the receiver to be past the next occurrence of the subCollection.
	If collection is found, answer match start position. If not, answer nil.
	Answer whether subCollection is found.
	No wildcards, and case does matter."

	| pattern startForNextMatch matchStart p |
	pattern := subCollection readStream.
	startForNextMatch := nil.
	[pattern atEnd] whileFalse: [
		self atEnd ifTrue: [^ nil].
		p := self position.
		(self next) = (pattern next) 
			ifTrue: [
				pattern position = 1 ifTrue: [
					startForNextMatch := self position.
					matchStart := p ]]
			ifFalse: [
				pattern position: 0.
				startForNextMatch ifNotNil: [
					self position: startForNextMatch.
					startForNextMatch := nil ]]].
	^matchStart! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 5/19/2024 19:58:28'!
upToAll: aCollection
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

	| startPos endMatch result |
	startPos := self position.
	(self matchStart: aCollection)
		ifNotNil: [ :matchStart |
			endMatch := self position.
			self position: startPos.
			result := self collectionSpecies streamContents: [ :ws |
				[self position < matchStart] whileTrue: [
					ws nextPut: self next ]].
			self position: endMatch.
			^ result ]
		ifNil: [
			self position: startPos.
			^ self upToEnd ]! !

!PositionableStream methodsFor: 'positioning' stamp: 'jmv 5/19/2024 19:52:59'!
match: subCollection
	"Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."

	^(self matchStart: subCollection) notNil
! !

