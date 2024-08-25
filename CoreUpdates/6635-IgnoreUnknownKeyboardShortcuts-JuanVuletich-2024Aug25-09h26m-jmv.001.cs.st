'From Cuis7.1 [latest update: #6630] on 25 August 2024 at 9:34:38 am'!

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 8/25/2024 09:33:42'!
keyStroke: aKeyboardEvent

	super keyStroke: aKeyboardEvent.

	"Ignore any unknown keyboard shortcut"
	aKeyboardEvent anyModifierKeyPressed ifTrue: [
		aKeyboardEvent wasHandled: true ].

	aKeyboardEvent wasHandled ifTrue: [ 
		self selectionChanged.  "Note new selection"
		self updateFromTextComposition.
		^self ].

	autoCompleter 
		ifNil: [ self processKeystrokeEvent: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeystrokeEvent: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ].

	aKeyboardEvent wasHandled: true.
! !

