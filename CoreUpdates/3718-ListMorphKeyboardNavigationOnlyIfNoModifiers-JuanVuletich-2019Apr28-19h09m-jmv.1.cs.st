'From Cuis 5.0 of 7 November 2016 [latest update: #3717] on 28 April 2019 at 7:10:10 pm'!

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 4/28/2019 19:09:34'!
arrowKey: aKeyboardEvent

	aKeyboardEvent anyModifierKeyPressed ifFalse: [
		aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
		aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
		aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
		aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
		aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
		aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
		aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
		aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ]].
	^ nil! !

