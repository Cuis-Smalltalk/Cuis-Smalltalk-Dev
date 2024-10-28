'From Cuis7.1 [latest update: #6793] on 28 October 2024 at 8:30:46 pm'!

!PositionableStream methodsFor: 'positioning' stamp: 'jmv 10/28/2024 20:30:10'!
matchStart: subCollection
	"Set the access position of the receiver to be past the next occurrence of the subCollection.
	If collection is found, answer match start position. If not, answer nil.
	No wildcards, and case does matter."

	| pattern startForNextMatch matchStart p |
	pattern := subCollection readStream.
	startForNextMatch := nil.
	[pattern atEnd] whileFalse: [
		self atEnd ifTrue: [^ nil].
		p := self position.
		self next = pattern next
			ifTrue: [
				pattern position = 1 ifTrue: [
					startForNextMatch := self position.
					matchStart := p+1 ]]
			ifFalse: [
				pattern position: 0.
				startForNextMatch ifNotNil: [
					self position: startForNextMatch.
					startForNextMatch := nil ]]].
	^matchStart! !

