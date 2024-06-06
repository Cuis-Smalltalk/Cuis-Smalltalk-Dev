'From Cuis7.1 [latest update: #6452] on 6 June 2024 at 5:42:56 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/6/2024 17:27:30'!
withDoubleColonsDoubled
	"Return a copy of the receiver with any double colons duplicated.
	Useful for turning text into Smalltalk comments."

	^self species streamContents: [ :stream |
		self do: [ :c |
			stream nextPut: c.
			c = $"
				ifTrue: [ stream nextPut: c ]]].! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/6/2024 17:36:44'!
withDoubleColonsUndoubled
	"Do the inverse of #withDoubleColonsDoubled"

	| lastWasDoubleColon |
	^self species streamContents: [ :stream |
		lastWasDoubleColon := false.
		self do: [ :c |
			c = $"
				ifTrue: [
					lastWasDoubleColon
						ifFalse: [
							stream nextPut: c.
							lastWasDoubleColon := true ]
						ifTrue: [
							lastWasDoubleColon := false]]
				ifFalse: [
					stream nextPut: c.
					lastWasDoubleColon := false ]]].! !


!SmalltalkEditor methodsFor: 'editing keys' stamp: 'jmv 6/6/2024 17:41:52'!
enclose: aKeyboardEvent
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| startIndex stopIndex oldSelection new enlargedBy |
	"This implementation is only for commenting and uncommenting code.
	Other cases are handled by the super implementation.
	This implementation only handles String and discards Text attributes."
	aKeyboardEvent keyCharacter = $" ifFalse: [
		^super enclose: aKeyboardEvent ].

	startIndex := self startIndex.
	stopIndex := self stopIndex.
	oldSelection := self selection string.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = $"
				and: [(model actualContents at: stopIndex) = $"]])
		ifTrue: [
			"already enclosed in double colons; make selection no longer a comment"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection withDoubleColonsUndoubled ]
		ifFalse: [
			"not enclosed in double colons; make selection a comment"
			new := oldSelection withDoubleColonsDoubled.
			enlargedBy := new size - oldSelection size.
			self replaceSelectionWith: $" asString, new, $" asString.
			self selectFrom: startIndex+1 to: stopIndex + enlargedBy ].
	^ true.! !

