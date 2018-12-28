'From Cuis 5.0 of 7 November 2016 [latest update: #3559] on 28 December 2018 at 11:16:11 am'!

!SelectionMenu methodsFor: 'basic control sequence' stamp: 'KLG 12/28/2018 14:05:30'!
startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean
	"Overridden to return value returned by manageMarker.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

	| index |
	index _ super startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
	selections ifNil: [ ^index ].  "If there are no selections defined, show the super class' behavior."
	index between: 1 and: selections size :: ifFalse: [ ^nil ].
	^ selections at: index! !

