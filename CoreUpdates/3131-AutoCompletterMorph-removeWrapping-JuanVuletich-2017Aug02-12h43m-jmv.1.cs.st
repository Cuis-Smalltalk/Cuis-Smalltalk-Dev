'From Cuis 5.0 of 7 November 2016 [latest update: #3130] on 2 August 2017 at 12:48:23 pm'!

!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 8/2/2017 12:45:12'!
goDown
	self selected: self selected + 1.
	(self selected > self lastVisible
			and: [self selected <= completer entryCount])
		ifTrue: [firstVisible := firstVisible + 1].
	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 8/2/2017 12:45:38'!
goHome
	firstVisible := 1.
	self selected: firstVisible. 
	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 8/2/2017 12:47:05'!
goPageDown
	(self gotoPage: self currentPage + 1)
		ifFalse: [ self goToEnd ].
	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 8/2/2017 12:45:54'!
goPageUp
	self gotoPage: self currentPage - 1.
	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 8/2/2017 12:45:32'!
goToEnd
	self selected: completer entryCount.
	firstVisible := selected - self class itemsPerPage + 1 max: 1.
	self redrawNeeded! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 8/2/2017 12:45:45'!
goUp
	(self selected = 0
			and: [self firstVisible = 1])
		ifTrue: [^ self].
	self selected: self selected - 1.
	self selected < self firstVisible
		ifTrue: [firstVisible := firstVisible - 1].
	self redrawNeeded! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 8/2/2017 12:46:45'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	(self morphContainsPoint: localEventPosition)
		ifTrue: [
			((self upButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goUp ].
			((self downButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goDown ].
			self selected: (localEventPosition y // self class itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !

!AutoCompleterMorph methodsFor: 'paging' stamp: 'jmv 8/2/2017 12:38:44'!
gotoPage: anInteger
	| item |
	item := ((anInteger - 1) * self class itemsPerPage) + 1. 
	item >= completer entryCount ifTrue: [^false].
	item < 1 ifTrue: [item := 1].
	firstVisible := item.
	self selected: firstVisible. 
	^ true! !


!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 8/2/2017 12:47:30'!
handleKeystrokeBefore: kbEvent
	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."
	| currentPos currentCharIsAlphaNumeric keyValue ctrl cmd tab colon alphanum backspace esc space return keyChar  |
	currentPos _ textMorph editor startIndex-1.
	currentCharIsAlphaNumeric _ currentPos > 0 and: [ model textSize >= currentPos and: [
			(model actualContents at: currentPos) isAlphaNumeric ]].
	keyValue _ kbEvent keyValue.
	keyChar _ kbEvent keyCharacter.
	ctrl _ kbEvent controlKeyPressed.
	cmd _ kbEvent commandAltKeyPressed.
	tab _ keyChar = Character tab.
	colon _ keyChar = $:.
	alphanum _ kbEvent keyCharacter isAlphaNumeric.
	backspace _ keyValue = 8.
	esc _ keyValue = 27.
	space _ #(0 32 160) includes: keyValue.
	return _ kbEvent isReturnKey.

	"Stuff to do if the menu is not open"
	menuMorph ifNil: [
		"Ctrl-Space or Tab for open"
		"Mac specific note: Using option-space (actually option+160) effectively disables the non-breaking space character 160"
		(space & (ctrl | kbEvent rawMacOptionKeyPressed) or: [
			(self opensWithTab and: [tab]) and: [ currentCharIsAlphaNumeric ]])
				ifTrue: [ self openCompletionMenu. ^ true].
		"Auto-open - currently deactivated"
"		(ctrl not & cmd not & alphanum) 
			ifTrue: [ self openCompletionMenu ]."
		^ false].

	"Starting here, stuff to do if the menu is open"
	menuMorph stillActive.
	"Escape"
	esc ifTrue: [ self closeMenu. ^ true].
	"Backspace"
	backspace ifTrue: [
		currentCharIsAlphaNumeric ifFalse: [ self closeMenu ].
		^ false].
	"Home"
	keyValue = 1 ifTrue: [ menuMorph goHome. ^ true ].
	"End"
	keyValue = 4 ifTrue: [ menuMorph goToEnd. ^ true].
	"?"
	keyChar = $? ifTrue: [ menuMorph help. ^true].
	"Arrow up"
	keyValue = 30 ifTrue: [ menuMorph goUp. ^ true].
	"Arrow down"
	keyValue = 31 ifTrue: [ menuMorph goDown. ^ true].
	"Page up"
	keyValue = 11 ifTrue: [ menuMorph goPageUp. ^ true].
	"Page down"
	keyValue = 12 ifTrue: [ menuMorph goPageDown. ^ true].
	"Return, Tab or Ctrl-Space"
	(return or: [ space & (ctrl | kbEvent rawMacOptionKeyPressed) or: [ tab]]) ifTrue: [
		self insertSelected
			ifTrue: [^ true]].
	"All keys but the alphanumeric chars (without command and control ) 
	and the backspace key do close the menu"
	(ctrl not & cmd not and: [ alphanum | colon])
		ifFalse: [ self closeMenu ].
	^false! !

!methodRemoval: AutoCompleterMorph #moveDown!
AutoCompleterMorph removeSelector: #moveDown!
!methodRemoval: AutoCompleterMorph #moveEnd!
AutoCompleterMorph removeSelector: #moveEnd!
!methodRemoval: AutoCompleterMorph #moveHome!
AutoCompleterMorph removeSelector: #moveHome!
!methodRemoval: AutoCompleterMorph #moveUp!
AutoCompleterMorph removeSelector: #moveUp!
!methodRemoval: AutoCompleterMorph #pageDown!
AutoCompleterMorph removeSelector: #pageDown!
!methodRemoval: AutoCompleterMorph #pageUp!
AutoCompleterMorph removeSelector: #pageUp!
