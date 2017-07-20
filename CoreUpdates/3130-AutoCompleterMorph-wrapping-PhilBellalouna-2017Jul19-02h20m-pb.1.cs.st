'From Cuis 5.0 of 7 November 2016 [latest update: #3128] on 19 July 2017 at 2:45:06 am'!

!AutoCompleterMorph methodsFor: 'actions' stamp: 'pb 7/19/2017 02:42:16'!
moveEnd
	self gotoPage: self pageCount.
	self selected: completer entryCount.
	self redrawNeeded.! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'pb 7/19/2017 02:42:22'!
moveHome
	self gotoPage: 1.
	self redrawNeeded.! !


!AutoCompleterMorph methodsFor: 'actions' stamp: 'pb 7/19/2017 02:43:17'!
moveDown
	self selected = completer entryCount
		ifTrue: [ self moveHome ]
		ifFalse: [
			self selected: self selected + 1.
			(self selected > self lastVisible and: [ self selected <= completer entryCount ]) ifTrue: [ firstVisible _ firstVisible + 1 ]].
	self redrawNeeded.! !

!AutoCompleterMorph methodsFor: 'actions' stamp: 'pb 7/19/2017 02:42:32'!
moveUp
	(self selected = 0 and: [ self firstVisible = 1 ]) ifTrue: [ ^ self ].
	self selected = 1
		ifTrue: [ 
			self moveEnd ]
		ifFalse: [
			self selected: self selected - 1.
			self selected < self firstVisible ifTrue: [ firstVisible _ firstVisible - 1 ]].
	self redrawNeeded.! !


!AutoCompleter methodsFor: 'keyboard' stamp: 'pb 7/19/2017 02:42:44'!
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
	keyValue = 1 ifTrue: [ menuMorph moveHome. ^ true ].
	"End"
	keyValue = 4 ifTrue: [ menuMorph moveEnd. ^ true].
	"?"
	keyChar = $? ifTrue: [ menuMorph help. ^true].
	"Arrow up"
	keyValue = 30 ifTrue: [ menuMorph moveUp. ^ true].
	"Arrow down"
	keyValue = 31 ifTrue: [ menuMorph moveDown. ^ true].
	"Page up"
	keyValue = 11 ifTrue: [ menuMorph pageUp. ^ true].
	"Page down"
	keyValue = 12 ifTrue: [ menuMorph pageDown. ^ true].
	"Return, Tab or Ctrl-Space"
	(return or: [ space & (ctrl | kbEvent rawMacOptionKeyPressed) or: [ tab]]) ifTrue: [
		self insertSelected
			ifTrue: [^ true]].
	"All keys but the alphanumeric chars (without command and control ) 
	and the backspace key do close the menu"
	(ctrl not & cmd not and: [ alphanum | colon])
		ifFalse: [ self closeMenu ].
	^false! !

!methodRemoval: AutoCompleterMorph #end!
AutoCompleterMorph removeSelector: #end!
!methodRemoval: AutoCompleterMorph #ensureVisible!
AutoCompleterMorph removeSelector: #ensureVisible!
!methodRemoval: AutoCompleterMorph #home!
AutoCompleterMorph removeSelector: #home!
