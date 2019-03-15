'From Cuis 5.0 of 7 November 2016 [latest update: #3610] on 14 March 2019 at 4:34:14 pm'!

!Editor methodsFor: 'private' stamp: 'AY 3/14/2019 16:25:09'!
nextWordEndFrom: aPosition goingForwards: goingForwardsBoolean
	
	"Answer the position of the end of the next word on the current line going
	forwards (or backwards).
	If the given position is the end (or beginning) of the line then answer the
	beginning (or end) of the next (or previous) line."
	
	| string beginningOfLine endOfLine step offset index newPosition |
	
	string _ self privateCurrentString.
	beginningOfLine _ self beginningOfLine: aPosition.
	endOfLine _ self endOfLine: aPosition.
	step _ goingForwardsBoolean ifTrue: [1] ifFalse: [-1].
	offset _ goingForwardsBoolean ifTrue: [0] ifFalse: [-1].
	
	index _ aPosition.
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers not]]
			whileTrue: [index _ index + step].
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers]]
			whileTrue: [index _ index + step].
	
	newPosition _ index = aPosition ifTrue: [index + step] ifFalse: [index].
	^newPosition min: string size + 1 max: 1! !

!Editor methodsFor: 'private' stamp: 'AY 3/14/2019 16:29:31'!
nextWordStartFrom: aPosition goingForwards: goingForwardsBoolean
	
	"Answer the position of the start of the next word on the current line going
	forwards (or backwards).
	If the given position is the end (or beginning) of the line then answer the
	beginning (or end) of the next (or previous) line."
	
	| string beginningOfLine endOfLine step offset index newPosition |
	
	string _ self privateCurrentString.
	beginningOfLine _ self beginningOfLine: aPosition.
	endOfLine _ self endOfLine: aPosition.
	step _ goingForwardsBoolean ifTrue: [1] ifFalse: [-1].
	offset _ goingForwardsBoolean ifTrue: [0] ifFalse: [-1].
	
	index _ aPosition.
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers]]
			whileTrue: [index _ index + step].
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers not]]
			whileTrue: [index _ index + step].
	
	newPosition _ index = aPosition ifTrue: [index + step] ifFalse: [index].
	^newPosition min: string size + 1 max: 1! !


!Editor methodsFor: 'nonediting/nontyping keys' stamp: 'AY 3/14/2019 01:24:07'!
cursorLeft: aKeyboardEvent
	"Private - Move cursor left one character if nothing selected, otherwise move cursor to beginning of selection. If the shift key is down, start selecting or extending current selection.
	Don't allow cursor past beginning of text"

	self
		moveCursor: [ :position | | newPos |
			newPos _ position - 1.
			"Mac standard keystroke"
			(aKeyboardEvent rawMacOptionKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [ newPos _ self previousWordStart: position ].
			"Mac standard keystroke"
			aKeyboardEvent commandAltKeyPressed ifTrue: [
				newPos _ self beginningOfLine: position ].
			newPos ]
		forward: false
		event: aKeyboardEvent.
	^ true! !

!Editor methodsFor: 'nonediting/nontyping keys' stamp: 'AY 3/14/2019 16:26:36'!
cursorRight: aKeyboardEvent 
	"Private - Move cursor right one character if nothing selected, otherwise move cursor to end of selection. If the shift key is down, start selecting characters or extending already selected characters. 
	Don't allow cursor past end of text"

	self
		moveCursor: [ :position | | newPos |
			newPos _ position + 1.
			"Mac standard keystroke"
			(aKeyboardEvent rawMacOptionKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [ newPos _ self nextWordEnd: position ].
			"Mac standard keystroke"
			aKeyboardEvent commandAltKeyPressed ifTrue: [
				newPos _ self endOfLine: position ].
			newPos ]
		forward: true
		event: aKeyboardEvent.
	^ true! !

!Editor methodsFor: 'private' stamp: 'AY 3/14/2019 16:30:31'!
nextWordEnd: aPosition
	
	^self nextWordEndFrom: aPosition goingForwards: true! !

!Editor methodsFor: 'private' stamp: 'AY 3/14/2019 16:30:13'!
nextWordStart: aPosition
	
	^self nextWordStartFrom: aPosition goingForwards: true! !

!Editor methodsFor: 'private' stamp: 'AY 3/14/2019 16:31:46'!
previousWordStart: aPosition
	
	^self nextWordEndFrom: aPosition goingForwards: false! !

