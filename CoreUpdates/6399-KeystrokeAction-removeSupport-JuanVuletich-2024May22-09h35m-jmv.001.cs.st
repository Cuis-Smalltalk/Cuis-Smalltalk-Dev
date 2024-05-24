'From Cuis7.1 [latest update: #6391] on 22 May 2024 at 9:45:23 am'!

!Morph methodsFor: 'events' stamp: 'jmv 5/22/2024 09:44:51'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].! !

!methodRemoval: TextModelMorph #keystrokeAction: stamp: 'jmv 5/22/2024 09:44:33'!
TextModelMorph removeSelector: #keystrokeAction:!
!methodRemoval: InnerTextMorph #keystrokeAction: stamp: 'jmv 5/22/2024 09:44:35'!
InnerTextMorph removeSelector: #keystrokeAction:!
