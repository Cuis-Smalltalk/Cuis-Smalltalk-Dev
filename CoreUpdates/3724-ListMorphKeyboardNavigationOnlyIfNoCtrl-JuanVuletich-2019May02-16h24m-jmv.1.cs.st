'From Cuis 5.0 of 7 November 2016 [latest update: #3723] on 2 May 2019 at 4:27:24 pm'!

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/2/2019 16:25:15'!
arrowKey: aKeyboardEvent

	"Handle a keyboard navigation event. Answer nil if not handled."
	"Ctrl arrows is used to scroll without changing the selection"
	aKeyboardEvent controlKeyPressed ifFalse: [
		aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
		aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
		aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
		aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
		aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
		aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
		aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
		aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ]].
	^ nil! !


!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/2/2019 16:24:31'!
arrowKey: aKeyboardEvent

	"Handle a keyboard navigation event. Answer nil if not handled."
	"Ctrl arrows is used to scroll without changing the selection"
	aKeyboardEvent controlKeyPressed ifFalse: [
		aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
		aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
		aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
		aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
		aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
		aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
		aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
		aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ]].
	^ nil! !

