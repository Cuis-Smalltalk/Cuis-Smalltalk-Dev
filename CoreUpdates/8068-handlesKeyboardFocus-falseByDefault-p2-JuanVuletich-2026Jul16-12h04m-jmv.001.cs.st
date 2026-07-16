'From Cuis7.9 [latest update: #8065] on 16 July 2026 at 12:07:13 pm'!

!Morph methodsFor: 'event handling testing' stamp: 'jmv 16/Jul/2026, 2:48:41 pm (UTC)'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus.
	For instance, implementors of the following messages will usually answer true:
		#keyStroke:
		#keyDown:
		#keyUp:
		#keyboardFocusChange:
		#keyboardShortcut:
	There maybe others."

	(self hasProperty: #keyStroke:)
		ifTrue: [ ^true ].
	(self hasProperty: #keyDown:)
		ifTrue: [ ^true ].
	(self hasProperty: #keyUp:)
		ifTrue: [ ^true ].

	^false! !

