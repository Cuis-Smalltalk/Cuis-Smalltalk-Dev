'From Cuis7.9 [latest update: #8065] on 16 July 2026 at 12:04:46 pm'!

!TranscriptMorph methodsFor: 'event handling testing' stamp: 'jmv 16/Jul/2026, 2:21:25 pm (UTC)'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus"

	^true! !


!FindInSourceDialog methodsFor: 'event handling testing' stamp: 'jmv 16/Jul/2026, 2:32:32 pm (UTC)'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus"

	^true! !


!MenuMorph methodsFor: 'event handling testing' stamp: 'jmv 16/Jul/2026, 2:28:08 pm (UTC)'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus"

	^true! !


!PluggableMorph methodsFor: 'event handling testing' stamp: 'jmv 16/Jul/2026, 2:31:44 pm (UTC)'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus"

	^true! !


!AutoCompleterMorph methodsFor: 'event handling testing' stamp: 'jmv 16/Jul/2026, 2:16:26 pm (UTC)'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus."

	^true! !


!PasteUpMorph methodsFor: 'event handling testing' stamp: 'jmv 16/Jul/2026, 2:01:33 pm (UTC)'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus"

	^true! !


!InnerPluggableMorph methodsFor: 'event handling testing' stamp: 'jmv 16/Jul/2026, 2:02:21 pm (UTC)'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus"

	^true! !


!Morph methodsFor: 'event handling testing' stamp: 'jmv 16/Jul/2026, 2:48:24 pm (UTC)'!
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

	^true! !


!PluggableMorph reorganize!
('access' balloonText menuNshortcutsKey menuNshortcutsKey: model)
('caching' releaseCachedState)
('classification' hasModel)
('initialization' defaultBorderWidth defaultColor initialize model:)
('event handling testing' handlesKeyboardFocus)
('updating' update:)
('submorphs-add/remove' clearDependencyAndEvents)
!

