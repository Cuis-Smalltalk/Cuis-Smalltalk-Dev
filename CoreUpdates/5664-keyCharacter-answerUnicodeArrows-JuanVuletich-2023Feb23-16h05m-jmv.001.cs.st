'From Cuis 6.0 [latest update: #5663] on 23 February 2023 at 4:06:28 pm'!

!MenuMorph methodsFor: 'events' stamp: 'jmv 2/23/2023 14:09:40'!
keyStroke: aKeyboardEvent
	| matchString selectable |
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: [
					selectedItem hasSubMenu 
						ifTrue: [
							aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
							^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: [
							^selectedItem invokeWithEvent: aKeyboardEvent]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: aKeyboardEvent].
			^self].
	aKeyboardEvent isEsc
		ifTrue: [
			"escape key"
			self
				valueOfProperty: #matchString
				ifPresentDo: [ :str | 
					str isEmpty 
						ifFalse: [
							"If filtered, first ESC removes filter"
							self setProperty: #matchString toValue: String new.
							self selectItem: nil.
							^self displayFiltered: aKeyboardEvent]].
			"If a stand-alone menu, just delete it"
			self delete.
			aKeyboardEvent hand
				newKeyboardFocus: prevKbdFocus;
				newMouseFocus: prevMouseFocus.
			^self].
	aKeyboardEvent isArrowLeft | aKeyboardEvent isArrowRight
		ifTrue: [
			"left or right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: [
					aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
				ifFalse: [^ self]].
	aKeyboardEvent isArrowUp ifTrue: [^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	aKeyboardEvent isArrowDown ifTrue: [^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	aKeyboardEvent isPageUp ifTrue: [^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	aKeyboardEvent isPageDown ifTrue: [^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsent: [String new].
	matchString := aKeyboardEvent isBackspace
				ifTrue: [matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent! !


!KeyboardEvent methodsFor: 'keyboard' stamp: 'jmv 2/22/2023 15:17:57'!
keyCharacter
	"Answer the character corresponding this keystroke. This is defined only for keystroke events."
	| codePoint |
	codePoint _ keyValue.
	self isArrowLeft ifTrue: [ codePoint _ 16r2190 ].
	self isArrowUp ifTrue: [ codePoint _ 16r2191 ].
	self isArrowRight ifTrue: [ codePoint _ 16r2192 ].
	self isArrowDown ifTrue: [ codePoint _ 16r2193 ].
	^ UnicodeCodePoint codePoint: codePoint.! !

