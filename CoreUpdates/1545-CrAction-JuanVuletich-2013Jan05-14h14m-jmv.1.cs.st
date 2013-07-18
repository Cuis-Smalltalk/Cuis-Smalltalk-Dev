'From Cuis 4.1 of 12 December 2012 [latest update: #1544] on 5 January 2013 at 2:29:21 pm'!
!classDefinition: #OneLineEditorMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #OneLineEditorMorph
	instanceVariableNames: 'font emphasis contents editor showCaret pauseBlinking caretRect keyboardFocusWatcher crAction '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!OneLineEditorMorph commentStamp: '<historical>' prior: 0!
A plain text editor for Morphic. Handles only one line. Does not handle fonts/styles, alignment, Smalltalk utilities and any other advanced stuff in TextModelMorph. Just a simple text editor.

Can optionally include a crAction: a zero argument closure, to be evaluated on Cr keystroke.!


!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 1/5/2013 14:23'!
crAction
	"Answer the optional Cr action"
	^crAction! !

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 1/5/2013 14:24'!
crAction: aBlock
	
	crAction := aBlock! !


!OneLineEditorMorph methodsFor: 'events' stamp: 'jmv 1/5/2013 14:29'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self closeWindowFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	"Return - check for special action
	Note: Code below assumes that this was some
	input field reacting on Return."
	aKeyboardEvent isReturnKey ifTrue: [
		crAction ifNotNil: [
			"Break the keyboard focus so that the receiver can be safely deleted."
			"Is this needed at all? (jmv)"
			"aKeyboardEvent hand newKeyboardFocus: nil."
			^crAction value ]].

	self pauseBlinking.
	self handleInteraction: [ self editor processKeyStroke: aKeyboardEvent ].
	self updateFromContents.
	super keyStroke: aKeyboardEvent  "sends to keyStroke event handler, if any"! !

!classDefinition: #OneLineEditorMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #OneLineEditorMorph
	instanceVariableNames: 'font emphasis contents editor showCaret pauseBlinking caretRect keyboardFocusWatcher crAction'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
