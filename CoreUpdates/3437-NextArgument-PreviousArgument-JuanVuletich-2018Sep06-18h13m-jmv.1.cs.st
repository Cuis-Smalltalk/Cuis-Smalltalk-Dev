'From Cuis 5.0 of 7 November 2016 [latest update: #3436] on 6 September 2018 at 6:13:28 pm'!

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 9/6/2018 17:51:29'!
argNext: aKeyboardEvent
	"Invoked by cmd-A.
	 Search forward from the end of the selection for a colon followed by
		a space.  Place the text cursor after the space.  If none are found, place the
		text cursor at the end of the text.."

	| start t |
	t _ model actualContents.
	start _ t findString: ': ' startingAt: self startIndex.
	start = 0 ifTrue: [ start _ t size + 1].
	self deselectAndPlaceCursorAt: start + 2.
	^true! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 9/6/2018 18:09:53'!
argPrev: aKeyboardEvent
	"Invoked by cmd-Q.
	 Search backwards from the start of the selection for a colon followed by
		a space.  Place the text cursor after the space.  If none are found, place the
		text cursor at the start of the text.."

	| t i |
	t _ model actualContents.
	i _ self stopIndex.
	i > 1 ifTrue: [
		i _ i -2.
		[i > 0 and: [ (t at: i) ~= $  or: [(t at: i-1) ~= $: ]]] whileTrue: [
			i _ i -1 ].
		self deselectAndPlaceCursorAt: i + 1.
	].
	^true! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 9/6/2018 17:56:10'!
quickFindLast: aBlock
	"Return the index of my last element for which aBlock evaluates as true.
	Assumes that receiver is sorted according with aBlock. Then, we are able to use faster binary search.
	Result is (in these cases) the same as #findLast:
	
	1 to: 1000 :: findLast: [:x | x squared < 123456]
	1 to: 1000 :: quickFindLast: [:x | x squared < 123456]
	
	1 to: 1000 :: findLast: [:x | x squared < -10]
	1 to: 1000 :: quickFindLast: [:x | x squared < -10]

	1 to: 1000 :: findLast: [:x | x squared < 1234560]
	1 to: 1000 :: quickFindLast: [:x | x squared < 1234560]
	"
	^self
		findBinaryIndex: [ :x | (aBlock value: x) ifTrue: [1] ifFalse: [-1]]
		do: [ :i | ]
		ifNone: [ :i1 :i2 | i1 ]! !


!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 9/6/2018 17:47:15'!
displayIfFalse: aKeyboardEvent
	"Replace the current text selection with the text 'ifFalse:'--initiated by 
	cmd-F."

	self addString: 'ifFalse:'.
	^false! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 9/6/2018 17:47:27'!
displayIfTrue: aKeyboardEvent
	"Replace the current text selection with the text 'ifTrue:'--initiated by 
	cmd-T."

	self addString: 'ifTrue:'.
	^false! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 9/6/2018 17:50:48'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$r	#acceptAndDebugTest:		'Saves method and debugs it')
		#(		$t	#acceptAndTest:				'Saves method, runs it as test and then all tests in class. Opens debugger if error')
		#(		$y	#acceptAndTestAll:				'Saves method, runs it as test and then all tests in class category. Opens debugger if error')
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:								'Print it (selection is a valid expression)')

		#(		$s	#save:								'Save (i.e. accept)')
		#(		$d	#doIt:								'Do it (selection is a valid expression)')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:						'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:				'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$E	#methodStringsContainingit:'Method strings containing it')
		#(		$T	#displayIfTrue:					'Insert #ifTrue:')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$Q	#argPrev:							'Previous argument')
		#(		$A	#argNext:							'Next argument')
		#(		$D	#debugIt:							'Debug it')
		#(		$F	#displayIfFalse:					'Insert #ifFalse:')
		#(		$G	#fileItIn:							'File in selection')

		#(		$V	#pasteInitials:					'Paste author initials')
		#(		$N	#referencesToIt:					'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !


!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 9/6/2018 17:40:31'!
handleKeystrokeBefore: kbEvent
	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."
	| currentPos currentChar currentCharIsAlphaNumericOrColon keyValue ctrl cmd tab colon alphanum backspace esc space return keyChar  |
	currentPos _ textMorph editor startIndex-1.
	currentCharIsAlphaNumericOrColon _ currentPos > 0 and: [ model textSize >= currentPos and: [
			currentChar _ model actualContents at: currentPos. currentChar isAlphaNumeric | (currentChar == $:) ]].
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
			(self opensWithTab and: [tab]) and: [ currentCharIsAlphaNumericOrColon ]])
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
		currentCharIsAlphaNumericOrColon ifFalse: [ self closeMenu ].
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

!methodRemoval: SmalltalkEditor #argAdvance:!
SmalltalkEditor removeSelector: #argAdvance:!
