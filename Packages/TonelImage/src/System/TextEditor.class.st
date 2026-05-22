"
See comment in Editor.

My instances edit Text, this is, they support multiple lines and TextAttributes.
They have no specific facilities for editing Smalltalk code. Those are found in SmalltalkEditor.
"
Class {
	#name : 'TextEditor',
	#superclass : 'Editor',
	#instVars : [
		'model',
		'textComposition',
		'markBlock',
		'pointBlock',
		'emphasisHere',
		'lastParenLocation',
		'initialSelectionStart',
		'initialSelectionStop',
		'doWordSelection',
		'doParagraphSelection',
		'defaultFont'
	],
	#classVars : [
		'ChangeText',
		'FindText'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'cached state access' }
TextEditor class >> changeText [
	ChangeText ifNil: [
		ChangeText := Text new ].
	^ChangeText
]

{ #category : 'cached state access' }
TextEditor class >> findText [
	FindText ifNil: [
		FindText := Text new ].
	^FindText
]

{ #category : 'class initialization' }
TextEditor class >> initializeKeyboardActions [
	
	super initializeKeyboardActions.
	keyboardActions at: 9 + 1 put: #tabKey:.
	keyboardActions at: 11 + 1 put: #cursorPageUp:.			"page up key"
	keyboardActions at: 12 + 1 put: #cursorPageDown:.		"page down key"
	keyboardActions at: 30 + 1 put: #cursorUp:.				"up arrow key"
	keyboardActions at: 31 + 1 put: #cursorDown:.			"down arrow key"
]

{ #category : 'ui' }
TextEditor class >> open [

	^ SystemWindow editText: TextModel new label: 'Text Editor' wrap: true
]

{ #category : 'misc' }
TextEditor class >> openHelp [

	self new openHelp
]

{ #category : 'cached state access' }
TextEditor class >> releaseClassCachedState [

	FindText := nil.
	ChangeText := nil.
	"We are not calling super to avoid cleansing class vars many times.
	So, repeat inherited class instVars!"
	keyboardActions := nil.
]

{ #category : 'menu-world' }
TextEditor class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		10.
			#label 			-> 		'Text Editor'.
			#object 		-> 		TextEditor.
			#selector 		-> 		#open.
			#icon 			-> 		#textEditorIcon.
			#balloonText 	-> 		'A window for composing text'.
		} asDictionary}`
]

{ #category : 'menu messages' }
TextEditor >> acceptContents [
	"Save the current text of the text being edited as the current acceptable version for purposes of canceling.  Allow my morph to take appropriate action"
	^morph acceptContents
]

{ #category : 'accessing' }
TextEditor >> actualContents [

	^model actualContents 
]

{ #category : 'accessing' }
TextEditor >> actualContents: aString [

	model actualContents: aString 
]

{ #category : 'private' }
TextEditor >> addAttributesForPasting: replacement [
	^ (replacement is: #Text)
		ifTrue: [ replacement ]
		ifFalse: [
			Text
				string: replacement
				attributes: emphasisHere ]
]

{ #category : 'typing support' }
TextEditor >> addString: aString [
	"Think of a better name"
	"This is a user command, and generates undo"

	self replaceSelectionWith: aString
]

{ #category : 'new selection' }
TextEditor >> afterSelectionInsertAndSelect: aString [
	"This is a user command, and generates undo"

	self insertAndSelect: aString at: self stopIndex 
]

{ #category : 'private' }
TextEditor >> applyAttribute: aTextAttribute [
	"The user selected aTextAttribute via shortcut, menu or other means.
	If there is a selection, apply the attribute to the selection.
	In any case use the attribute for the user input (emphasisHere)"
	"This generates undo"
	| anythingDone interval |

	morph disablesEditing ifTrue: [
		^ self ].

	anythingDone := false.
	emphasisHere := Text addAttribute: aTextAttribute toArray: emphasisHere.

	interval := self selectionInterval.
	(interval notEmpty or: [ aTextAttribute isParagraphAttribute ])
		ifTrue: [
			anythingDone := true.
			model logUndoAndAddAttribute: aTextAttribute from: interval first to: interval last.
			textComposition recomposeFrom: interval first to: interval last delta: 0 ].

	anythingDone ifTrue: [
		self recomputeSelection.	"Needed so visible selection is updated to reflect new visual extent of selection"
		self userHasEdited ].

	"Even if nothing done, emphasisHere might have changed"
	morph possiblyChanged.
]

{ #category : 'typing support' }
TextEditor >> backTo: startIndex [
	"During typing, backspace to startIndex."
	"This is a user command, and generates undo"

	self markIndex: startIndex.
	self replaceSelectionWith: self nullText.
	markBlock := pointBlock
]

{ #category : 'typing/selecting keys' }
TextEditor >> backWord: aKeyboardEvent [
	"If the selection is not empty, delete it and leave it in the backspace buffer.
	 Else, delete the word before the text cursor."
	"This is a user command, and generates undo"

	| startIndex |
	self hasSelection ifFalse: [ "No selection, delete at least one character"
		startIndex := 1 max: self markIndex - 1.
		[startIndex > 1 and:
			[(self privateCurrentString at: startIndex - 1) tokenish]]
				whileTrue: [
					startIndex := startIndex - 1]]
	ifTrue: [ "some selection, just delete it"
		startIndex := self markIndex].
	self backTo: startIndex.
	^false
]

{ #category : 'private' }
TextEditor >> beginningOfLine: position [
	"Redefined in subclasses using TextComposition"
	^ (textComposition lines at: (textComposition lineIndexFor: position)) first
]

{ #category : 'binding' }
TextEditor >> bindingOf: aString [
	^model bindingOf: aString
]

{ #category : 'parenblinking' }
TextEditor >> blinkParen [
	"Used if Shout"
	lastParenLocation ifNotNil: [
		model textSize >= lastParenLocation ifTrue: [
			model privateAddBoldAt: lastParenLocation ]]
]

{ #category : 'parenblinking' }
TextEditor >> blinkParenAt: parenLocation [

	model privateAddBoldAt: parenLocation.
	lastParenLocation := parenLocation
]

{ #category : 'parenblinking' }
TextEditor >> blinkPrevParen: aCharacter [
	"Used if not Shout"
	| closingDelimiter openingDelimiter level string here hereChar |
	string := self privateCurrentString.
	here := pointBlock stringIndex.
	closingDelimiter := aCharacter.
	openingDelimiter := '([{' at: (')]}' indexOf: closingDelimiter).
	level := 1.
	[ level > 0 and: [ here > 1 ]]
		whileTrue: [
			hereChar := string at: (here := here - 1).
			hereChar = openingDelimiter
				ifTrue: [
					level := level - 1.
					level = 0
						ifTrue: [^ self blinkParenAt: here]]
				ifFalse: [
					hereChar = closingDelimiter
						ifTrue: [ level := level + 1]]]
]

{ #category : 'editing keys' }
TextEditor >> changeEmphasisBold [
	^self changeEmphasisTo: 1
]

{ #category : 'editing keys' }
TextEditor >> changeEmphasisItalic [
	^self changeEmphasisTo: 2
]

{ #category : 'editing keys' }
TextEditor >> changeEmphasisNormal [
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"This is a user command, and generates undo"

	| attributesToRemove |

	"Remove all TextEmphasis"
	attributesToRemove := emphasisHere.
	attributesToRemove ifNotNil: [ self unapplyAttributes: attributesToRemove ].
	^ true
]

{ #category : 'editing keys' }
TextEditor >> changeEmphasisStruckThrough [
	^self changeEmphasisTo: 4
]

{ #category : 'editing keys' }
TextEditor >> changeEmphasisSubscript [
	^self changeEmphasisTo: 6
]

{ #category : 'editing keys' }
TextEditor >> changeEmphasisSuperscript [
	^self changeEmphasisTo: 5
]

{ #category : 'editing keys' }
TextEditor >> changeEmphasisTo: emphasisCode [
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"This is a user command, and generates undo"

	| attributeToAdd attributesToRemove oldAttributes |
	oldAttributes := emphasisHere.

	"Toggle the indicated TextEmphasis"
	(emphasisCode between: 1 and: 6) ifTrue: [
		attributeToAdd := TextEmphasis perform: (
			#(#bold #italic #underlined #struckThrough #superscript #subscript)
				at: emphasisCode).
		oldAttributes do: [ :oldAtt |
			oldAtt = attributeToAdd ifTrue: [
				attributeToAdd := nil.
				attributesToRemove := {oldAtt} ]]].

	attributeToAdd ifNotNil: [ self applyAttribute: attributeToAdd ].
	attributesToRemove ifNotNil: [ self unapplyAttributes: attributesToRemove ].
	^ true
]

{ #category : 'editing keys' }
TextEditor >> changeEmphasisUnderlined [
	^self changeEmphasisTo: 3
]

{ #category : 'editing keys' }
TextEditor >> changeLineEndsToLf [
	"Replace all CRs and CrLfs by LFs.
	Triggered by Cmd-U -- useful when getting code from FTP sites"
	"This is a user command, and generates undo"
	
	self replaceSelectionWith: self selectedString withCuisLineEndings.
]

{ #category : 'accessing-selection' }
TextEditor >> characterBlockForIndex: anIndex [

	^ textComposition characterBlockForIndex: anIndex 
]

{ #category : 'menu messages' }
TextEditor >> chooseAlignment [
	"This is a user command, and generates undo"

	| aList reply  |
	aList := #(leftFlush centered justified rightFlush).
	reply := (SelectionMenu labelList: aList selections: aList) startUpMenu.
	reply ifNil: [ ^self ].
	self applyAttribute: (TextAlignment perform: reply).
	morph updateFromTextComposition.
]

{ #category : 'editing keys' }
TextEditor >> chooseColor [
	"Make a new Text Color Attribute, let the user pick a color, and return the attribute"

	| |
	"(ColorPickerMorph new)
		choseModalityFromPreference;
		sourceHand: morph activeHand;
		target: (attribute := TextColor color: Color black);
		selector: #color:;
		originalColor: Color black;
		putUpFor: morph near: morph morphFullBoundsInWorld.
	^attribute"
]

{ #category : 'attributes' }
TextEditor >> clearFont [
	"Remove any Font attribute"
	"This is a user command, and generates undo"

	| attributesToRemove firstIndex lastIndex |
	attributesToRemove := Set new.
	firstIndex := self startIndex.
	lastIndex := self stopIndex - 1.
	model actualContents attributesFrom: firstIndex to: lastIndex do: [ :attribute |
		attribute isFont ifTrue: [ attributesToRemove add: attribute ]].

	self unapplyAttributes: attributesToRemove.
	morph resetTextComposition.
	self recomputeSelection.
	morph updateFromTextComposition.
]

{ #category : 'parenblinking' }
TextEditor >> clearParens [
	lastParenLocation ifNotNil: [
		model textSize >= lastParenLocation ifTrue: [
			model privateRemoveBoldAt: lastParenLocation ]].
	lastParenLocation := nil
]

{ #category : 'events' }
TextEditor >> clickAndHalfAt: localEventPosition [

	self selectWordOrDelimitedTextAt: localEventPosition.

	doWordSelection := true.
	doParagraphSelection := false.
	initialSelectionStart := self startBlock.
	initialSelectionStop := self stopBlock
]

{ #category : 'menu messages' }
TextEditor >> compareToClipboard [
	"Check to see if whether the receiver's text is the same as the text currently on the clipboard, and inform the user."
	| s1 s2 |
	s1 := self clipboardStringOrText string.
	s2 := self selection ifEmpty: [self privateCurrentString].
	s1 = s2 ifTrue: [^ self inform: 'Exact match'].

	(TextModel new contents:
		(DifferenceFinder displayPatchFrom: s1 to: s2 tryWords: true))
			openLabel: 'Comparison to Clipboard Text'
]

{ #category : 'editing keys' }
TextEditor >> copyHiddenInfo [
	"In TextLinks, TextDoits, TextColor, and TextURLs, there is hidden
info.  Copy that to the clipboard.  You can paste it and see what it is.
Usually enclosed in <>."

	^ self clipboardTextPut: self hiddenInfo
]

{ #category : 'menu messages' }
TextEditor >> copySelection [
	"Copy the current selection and store it in the Clipboard, unless empty."

	| multiSelection |

	"Copying whitespace to clipboard is OK."
	(self hasSelection and: [self selection isSeparators]) ifFalse: [
		self lineSelectAndEmptyCheck: [^ self]].

	multiSelection := self selection.
	self clipboardTextPut: multiSelection
]

{ #category : 'new selection' }
TextEditor >> correctFrom: start to: stop with: aString [
	"Make a correction in the model that the user has authorised from somewhere else in the system (such as from the compilier).  The user's selection is not changed, only corrected."
	"This is a user command, and generates undo"
	| userSelection delta loc str |
	aString = '#insert period' ifTrue: [
		loc := start.
		str := self privateCurrentString.
		[(loc := loc-1)>0 and: [(str at: loc) isSeparator]]
			whileTrue: [loc := loc-1].
		^ self correctFrom: loc+1 to: loc with: '.'].
	userSelection := self selectionInterval.

	self selectInvisiblyFrom: start to: stop.
	self replaceSelectionWith: aString.

	delta := aString size - (stop - start + 1).
	self
		selectInvisiblyFrom: userSelection first + (userSelection first > start ifFalse: [ 0 ] ifTrue: [ delta ])
		to: userSelection last + (userSelection last > start ifFalse: [ 0 ] ifTrue: [ delta ])

]

{ #category : 'accessing' }
TextEditor >> currentAttributes [
	| i |
	i := self startIndex.
	^i > model textSize
		ifTrue: [
			emphasisHere ]
		ifFalse: [
			"paragraph attributes at the current paragraph will later be applied.
			include them in the answer"
			(emphasisHere reject: [ :attr | attr isParagraphAttribute ]),
				((model actualContents attributesAt: i) select: [ :attr | attr isParagraphAttribute ]) ]
]

{ #category : 'accessing' }
TextEditor >> currentCharacterStyleOrNil [
	"Answer the CharacterStyle for the current selection or cursor location if any"

	^self hasSelection
		ifTrue: [ model actualContents characterStyleOrNilAt: self startIndex ]
		ifFalse: [ model actualContents characterStyleOrNilIfApplying: emphasisHere ]
]

{ #category : 'accessing' }
TextEditor >> currentParagraphStyle [
	"Answer the ParagraphStyle for the current selection or cursor location if any"
	
	| i |
	i := self startIndex.
	^i > model textSize
		ifTrue: [ model actualContents paragraphStyleOrNilIfApplying: emphasisHere ]
		ifFalse: [ model actualContents paragraphStyleOrNilAt: i ]
]

{ #category : 'nonediting/nontyping keys' }
TextEditor >> cursorDown: aKeyboardEvent [
	"Private - Move cursor from position in current line to same position in
	next line. If next line too short, put at end. If shift key down,
	select."

	self
		moveCursor: [ :position | | newPos |
			newPos := self sameColumn: position newLine: [ :line | line + 1 ] forward: true.
			"Mac standard keystroke"
			aKeyboardEvent rawMacOptionKeyPressed ifTrue: [
				newPos := self endOfParagraph: position+1 ].
			"Windows / Linux standard keystroke"
			aKeyboardEvent controlKeyPressed ifTrue: [
				newPos := self beginningOfNextParagraph: position ].
			"Mac standard keystroke"
			aKeyboardEvent commandAltKeyPressed ifTrue: [
				newPos := self endOfText ].
			newPos ]
		forward: true
		event: aKeyboardEvent.
	^ true
]

{ #category : 'nonediting/nontyping keys' }
TextEditor >> cursorEnd: aKeyboardEvent  [

	"Private - Move cursor end of current line."

	self
		moveCursor: [ :position |
			"Mac standard keystrole"		
			(aKeyboardEvent commandAltKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [ self endOfText ]
					ifFalse: [ self endOfLine: position ]]
		forward: true
		event: aKeyboardEvent.
	^true
]

{ #category : 'nonediting/nontyping keys' }
TextEditor >> cursorHome: aKeyboardEvent [

	"Private - Move cursor from position in current line to beginning of current line or end of indentation (see #firstOfBeginningOfLineOrEndOfIndentationLeftOf:).
	If control key is pressed put cursor at beginning of text"

	self
		moveCursor: [ :position |
			"Mac standard keystrole"		
			(aKeyboardEvent commandAltKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [ self beginningOfText ]
					ifFalse: [ self firstOfBeginningOfLineOrEndOfIndentationLeftOf: position ]]
		forward: false
		event: aKeyboardEvent.
	^true
]

{ #category : 'nonediting/nontyping keys' }
TextEditor >> cursorPageDown: aKeyboardEvent  [

	self 
		moveCursor: [ :position |
			self
				sameColumn: position
				newLine: [ :lineNo | lineNo + self pageHeight]
				forward: true]
		forward: true
		event: aKeyboardEvent.
	^true
]

{ #category : 'nonediting/nontyping keys' }
TextEditor >> cursorPageUp: aKeyboardEvent  [

	self 
		moveCursor: [ :position |
			self
				sameColumn: position
				newLine: [ :lineNo | lineNo - self pageHeight]
				forward: false]
		forward: false
		event: aKeyboardEvent.
	^true
]

{ #category : 'nonediting/nontyping keys' }
TextEditor >> cursorUp: aKeyboardEvent  [
	"Private - Move cursor from position in current line to same position in
	prior line. If prior line too short, put at end"

	self
		moveCursor: [ :position | | newPos |
			newPos := self sameColumn: position newLine: [ :line | line - 1] forward: false.
			"Mac standard keystroke"
			(aKeyboardEvent rawMacOptionKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [
						newPos := self beginningOfParagraph: newPos-1 ].
			"Mac standard keystroke"
			aKeyboardEvent commandAltKeyPressed ifTrue: [
				newPos := self beginningOfText ].
			newPos ]
		forward: false
		event: aKeyboardEvent.
	^true
]

{ #category : 'menu messages' }
TextEditor >> cut [
	"Cut out the current selection and redisplay if necessary."
	"This is a user command, and generates undo"

	| selection |
	self lineSelectAndEmptyCheck: [^ self].
	selection := self selection.
	self replaceSelectionWith: self nullText.
	self clipboardTextPut: selection.
]

{ #category : 'accessing' }
TextEditor >> defaultFont [
	^defaultFont ifNil: [FontFamily defaultFamilyAndPointSize].
]

{ #category : 'accessing' }
TextEditor >> defaultFont: aFont [
	defaultFont := aFont
]

{ #category : 'menu & kbd shortcuts' }
TextEditor >> defaultMenuSpec [
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	| acceptEntry |
	acceptEntry := self hasUnacceptedEdits
		ifTrue: [ #((110	'Save (s)' 		$s 	editor 	acceptContents 					acceptIcon 						'Save')) ]
		ifFalse: [ #((110 	'Save' 			nil 	editor 	nil 									acceptIcon 						'Save')) ].
	^#(
		#(0 		'Text Editor' 				true)
		#(10	'Help... (h)' 				$h 	editor 	openHelp 							helpIcon 						'Open Help')
		#(20 	'-----')
		#(30	'Find...(f)' 				$f 	editor 	find 								findIcon 						'Find')
		#(40	'Find Again (g)' 			$g 	editor 	findAgain 							systemIcon 					'Find again')
		#(50	'Use Selection for Find (j)'	$j 	editor 	setSearchString 					saveAsIcon 					'Set selection as search string for find again')
		#(60 	'-----')
		#(70	'Undo - multiple (z)' 		$z 	editor 	undo 								undoIcon 						'Undo (multiple levels)')
		#(80	'Redo - multiple (Z)' 		$Z 	editor 	redo 								redoIcon 						'Redo (multiple levels)')
		#(90	'Undo / Redo history' 		nil 	editor 	offerUndoHistory 					changesIcon)
		#(100 	'-----')
	),
	acceptEntry,
	#(
		#(120	'Copy (c)' 				$c 	editor 	copySelection 						copyIcon 						'Copy selection to the Clipboard')
		#(130	'Cut (x)' 					$x 	editor 	cut 									cutIcon 						'Cut selection and store it in the Clipboard')
		#(140 	'Compare to Clipboard (C)' $C 	editor 	compareToClipboard 				changesIcon 					'Compare selection to clipboard')
		#(150	'Paste (v)' 				$v 	editor 	paste 								pasteIcon 						'Paste Clipboard contents')
		#(160	'Paste without Format' 	nil 	editor 	pasteString 						pasteIcon)
		#(170	'Paste...' 				nil 	editor 	pasteRecent 						worldIcon)
		#(180 	'-----')
		#(190	'Toggle WordWrap' 		nil 	editor 	wrapOnOff 							genericTextIcon)
		#(200	'Clear Font' 				nil 	editor 	clearFont 							newIcon)
		#(210	'Set Font... (k)' 			$k 	editor 	offerFontMenu 						preferencesDesktopFontIcon 	'Set font')
		#(220	'Set Font Size' 			nil 	editor 	offerFontSizeMenu 				preferencesDesktopFontIcon)
		#(230	'Set Style' 				nil 	editor 	offerFontStyleMenu 				preferencesDesktopFontIcon)
		#(240	'Set Text Color (8)' 		$8 	editor 	offerColorMenu 					graphicsIcon)
		#(250	'Set Line Spacing... (l)'	$l 	editor 	offerLineSpacingMenu 				genericTextIcon 				'Change Interline spacing factor')
		#(260	'Set Alignment... (u)' 		$u 	editor 	chooseAlignment 					formatJustifyLeftIcon 			'Toggle alignment')

		"Keyboard shortcut only. No menu entry."
		#(500	nil 						$0 	editor 	changeEmphasisNormal 			nil 			'Normal')
		#(501	nil 						$1 	editor 	changeEmphasisBold 				nil 			'Bold')
		#(502	nil 						$2 	editor 	changeEmphasisItalic 				nil 			'Italic')
		#(503	nil 						$3 	editor 	changeEmphasisUnderlined 		nil 			'Underlined')
		#(504	nil 						$4 	editor 	changeEmphasisStruckThrough 	nil 			'Struck Through')
		#(505	nil 						$5 	editor 	changeEmphasisSuperscript 		nil 			'Superscript')
		#(506	nil 						$6 	editor 	changeEmphasisSubscript 			nil 			'Subscript')
		#(601 	nil 						$a 	editor 	selectAll 							nil 			'Select all')
		#(602 	nil						$U 	editor 	changeLineEndsToLf 				nil 			'Convert line endings to LF characters (Cuis convention) in selection')
		#(603 	nil 						$( 	editor 	encloseWithParenthesis 			nil 			'Enclose within ( and ), or remove enclosing ( and )')
		#(604 	nil 						$[ 	editor 	encloseWithSquareBrackets 		nil 			'Enclose within [ and ], or remove enclosing [ and ]')
		#(605 	nil 						${ 	editor 	encloseWithCurlyBrackets 			nil 			'Enclose within { and }, or remove enclosing { and }')
		#(606 	nil 						$' 	editor 	encloseWithSingleQuotes 			nil 			'Enclose within single quotes, or remove enclosing single quotes')
		#(607 	nil 						$" 	editor 	encloseWithDoubleQuotes 			nil 			'Enclose within double quotes, or remove enclosing double quotes')
		#(608 	nil 						$` 	editor 	encloseWithBackTicks 				nil 			'Enclose within backticks, or remove enclosing backticks')
		"#(609 	nil 						$< 	editor 	encloseWithAngleBrackets 			nil 			'Enclose within < and >, or remove enclosing ( and )')"
		#(610 	nil 						$H 	editor 	cursorTopHome 					nil 			'Move cursor to start of text')
	).

]

{ #category : 'typing support' }
TextEditor >> dispatchOn: aKeyboardEvent [
	"Carry out the action associated with this character, if any."

	| codePoint asciiValue c |
	self clearParens.
  	codePoint := aKeyboardEvent keyValue.
	
	codePoint > 255 ifTrue: [
		^self normalCharacter: aKeyboardEvent ].

	c := aKeyboardEvent keyCharacter.
	(')]}' includes: c)
		ifTrue: [ self blinkPrevParen: c ].

	asciiValue := codePoint.
	^ self perform: (self keyboardActions at: asciiValue + 1) with: aKeyboardEvent.
]

{ #category : 'events' }
TextEditor >> doubleClickAndHalf [

	| here interval |
	here := self pointIndex.
	interval := self privateCurrentString encompassParagraph: (here to: here).
	self selectFrom: interval first to: interval last.

	doWordSelection := false.
	doParagraphSelection := true.
	initialSelectionStart := self startBlock.
	initialSelectionStop := self stopBlock
]

{ #category : 'editing keys' }
TextEditor >> encloseWith: aCharacter [
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| left right startIndex stopIndex oldSelection which |
	startIndex := self startIndex.
	stopIndex := self stopIndex.
	oldSelection := self selection.
	which := '([<{"''`' indexOf: aCharacter ifAbsent: [ ^true ].
	left := '([<{"''`' at: which.
	right := ')]>}"''`' at: which.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = left
				and: [(model actualContents at: stopIndex) = right]])
		ifTrue: [
			"already enclosed; strip off brackets"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection]
		ifFalse: [
			"not enclosed; enclose by matching brackets"
			self replaceSelectionWith:
				(Text string: (UnicodeString with: left) attributes: emphasisHere),
				oldSelection,
				(Text string: (UnicodeString with: right) attributes: emphasisHere).
			self selectFrom: startIndex+1 to: stopIndex].
	^ true.
]

{ #category : 'editing keys' }
TextEditor >> encloseWithAngleBrackets [
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $<
]

{ #category : 'editing keys' }
TextEditor >> encloseWithBackTicks [
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $`
]

{ #category : 'editing keys' }
TextEditor >> encloseWithCurlyBrackets [
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: ${
]

{ #category : 'editing keys' }
TextEditor >> encloseWithDoubleQuotes [
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $"
]

{ #category : 'editing keys' }
TextEditor >> encloseWithParenthesis [
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $(
]

{ #category : 'editing keys' }
TextEditor >> encloseWithSingleQuotes [
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $'
]

{ #category : 'editing keys' }
TextEditor >> encloseWithSquareBrackets [
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	^self encloseWith: $[
]

{ #category : 'private' }
TextEditor >> endOfLine: position [
	"Redefined in subclasses using TextComposition"
	| targetLine |
	targetLine := textComposition lines at: (textComposition lineIndexFor: position).
	^ targetLine = textComposition lastLine
		ifFalse: [ targetLine last ]
		ifTrue: [ targetLine last + 1 ]
]

{ #category : 'menu messages' }
TextEditor >> find [
	"Prompt the user for a string to search for, and search the receiver from the current selection onward for it.  1/26/96 sw"

	self
		request: 'Find what?'
		initialAnswer: self class findText
		do: [ :aString | self find: aString ].

]

{ #category : 'menu messages' }
TextEditor >> find: aString [
	"Prompt the user for a string to search for, and search the receiver from the current selection onward for it.  1/26/96 sw"

	aString isEmpty ifFalse: [
		"Set focus on our text morph, so that cmd-g does the search again"
		morph world activeHand newKeyboardFocus: morph.
		self setSearch: aString.
		ChangeText := self class findText.  "Implies no replacement to againOnce: method"
		morph handleInteraction: [
			(self findAndReplaceMany: false)
				ifFalse: [ self flash ]].
		morph scrollSelectionIntoView ].

]

{ #category : 'menu messages' }
TextEditor >> findAgain [
	"Find the text-to-find again.  1/24/96 sw"

	morph handleInteraction: [
		(self findAndReplaceMany: false)
			ifFalse: [ self flash ]].
	morph scrollSelectionIntoView.

]

{ #category : 'private' }
TextEditor >> findAndReplaceMany: doMany [
	"Subroutine of search: and again.  Use same FindText and ChangeText as before.  If many is true, do it repeatedly.  Created 1/26/96 sw by adding the many argument to #againOrSame."
	"jmvnote: We currently have no command for search&replace, but it is really easy to add, and this method supports it. See ChangeText"
	"This is a user command, and generates undo"

	|  indexStream |

	"Find and Change, recording start indices in the array"
	indexStream := WriteStream on: (Array new: 20). "an array to store change locs"
	[(self findAndReplaceOnce: indexStream) & doMany] whileTrue. "<-- this does the work"
	"Answer whether something was found"
	^indexStream notEmpty
]

{ #category : 'private' }
TextEditor >> findAndReplaceOnce: indexStream [
	"Find the next occurrence of FindText.  If none, answer false.
	Append the start index of the occurrence to the stream indices, and, if
	ChangeText is not the same object as FindText, replace the occurrence by it.
	Note that the search is case-sensitive for replacements, otherwise not."
	"This is a user command, and generates undo"

	| where |
	where := model actualContents
		findString: self class findText
		startingAt: self stopIndex
		caseSensitive: ((self class changeText ~~ self class findText) or: [Preferences at: #caseSensitiveFinds]).
	where = 0 ifTrue: [^ false].

	self selectFrom: where to: where + self class findText size - 1.	"Repeat it here. Senders beware: only one of these should last"

	self class changeText ~~ self class findText ifTrue: [ self replaceSelectionWith: self class changeText ].
	indexStream nextPut: where.
	^ true
]

{ #category : 'private' }
TextEditor >> firstOfBeginningOfLineOrEndOfIndentationLeftOf: position [
	
	"Returns the first of beginning-of-line or end-of-indentation that appears to the left of the given position, wrapping around to the end of the line (i.e. the line is considered circular).
	This way, if the given position is beginning-of-line then end-of-indentation is returned."
	
	| currentLine beginningOfLine endOfIndentation stops |
	
	currentLine := textComposition lines at: (textComposition lineIndexFor: position).
	beginningOfLine := currentLine first.
	endOfIndentation := self privateCurrentString
		skipDelimiters: (UnicodeString with: Character tab)
		startingAt: beginningOfLine.
		
	stops := OrderedCollection with: endOfIndentation with: beginningOfLine.
	^ stops detect: [ :stop | stop < position ] ifNone: [endOfIndentation]
]

{ #category : 'displaying' }
TextEditor >> flash [
	^ morph flash
]

{ #category : 'undo & redo' }
TextEditor >> flushUndoRedoCommands [
	model flushUndoRedoCommands
]

{ #category : 'typing/selecting keys' }
TextEditor >> forwardDelete: aKeyboardEvent [
	"Delete forward over the next character."
	"This is a user command, and generates undo"

	| startIndex stopIndex |

	"If there was a selection"
	self hasSelection ifTrue: [
		self replaceSelectionWith: self nullText.
		^ false].

	"Exit if at end"
	startIndex := self markIndex.
	startIndex > model textSize ifTrue: [
		^ false].

	"Null selection - do the delete forward"
	stopIndex := startIndex.
	(self shouldDeleteAWordForward: aKeyboardEvent)
		ifTrue: [stopIndex := (self nextWordStart: stopIndex) - 1].
	self selectFrom: startIndex to: stopIndex.
	self replaceSelectionWith: self nullText.
	self deselectAndPlaceCursorAt: startIndex.
	^false
]

{ #category : 'accessing-selection' }
TextEditor >> hasSelection [
	^ markBlock ~= pointBlock
]

{ #category : 'testing' }
TextEditor >> hasUnacceptedEdits [

	^morph notNil and: [ morph hasUnacceptedEdits ].
]

{ #category : 'testing' }
TextEditor >> hasUnacceptedEdits: aBoolean [

	^morph hasUnacceptedEdits: aBoolean 
]

{ #category : 'editing keys' }
TextEditor >> hiddenInfo [
	"In TextLinks, TextDoits, TextColor, and TextURLs, there is hidden info.  Return the entire string that was used by Cmd-6 to create this text attribute.  Usually enclosed in < >."

	| attrList |
	attrList := model actualContents attributesAt: (self pointIndex + self markIndex)//2.
	attrList do: [:attr |
		attr forTextActionInfoDo: [ :info |
			^ self selectedString, '<', info, '>']].
	"If none of the above"
	attrList do: [:attr |
		attr forTextColorDo: [ :color |
			^ self selectedString, '<', color printString, '>']].
	^ self selectedString, '[No hidden info]'
]

{ #category : 'editing keys' }
TextEditor >> inOutdent: aKeyboardEvent delta: delta [
	"Add/remove a tab at the front of every line occupied by the selection.
	This is a user command, and generates undo"

	| realStart realStop lines startLine stopLine start stop adjustStart size inStream outStream |

	"Operate on entire lines, but remember the real selection for re-highlighting later"
	realStart := self startIndex.
	realStop := self stopIndex - 1.

	"Special case: the text cursor on a line of its own, including weird case at end of paragraph"
	(realStart > realStop and: [
				realStart < 2 or: [(self privateCurrentString at: realStart - 1) isLineSeparator ]])
		ifTrue: [
			delta < 0
				ifTrue: [
					morph flash]
				ifFalse: [
					self replaceSelectionWith: Character tab asSymbol.
					self deselectAndPlaceCursorAt: realStart + 1].
			^true].

	lines := textComposition lines.
	startLine := textComposition lineIndexFor: realStart.
	stopLine := textComposition lineIndexFor: (realStart max: realStop).
	start := (lines at: startLine) first.
	stop := (lines at: stopLine) last.
	
	"Pin the start of highlighting unless the selection starts a line"
	adjustStart := realStart > start.

	size :=  stop + 1 - start.
	inStream := self privateCurrentString readStreamFrom: start to: stop.
	outStream := UnicodeString writeStream.

	"This subroutine does the actual work"
	self indent: delta fromStream: inStream toStream: outStream.

	"Adjust the range that will be highlighted later"
	adjustStart ifTrue: [realStart := (realStart + delta) max: start].
	realStop := realStop + outStream position - size.

	"Prepare for another iteration"
	self selectInvisiblyFrom: start to: stop.
	self replaceSelectionWith: outStream contents.
	self selectFrom: realStart to: realStop. 	"highlight only the original range"
	^ true
]

{ #category : 'editing keys' }
TextEditor >> indent: aKeyboardEvent [
	"Add a tab at the front of every line occupied by the selection. Invoked from keyboard via cmd-shift-R.  2/29/96 sw"

	^ self inOutdent: aKeyboardEvent delta: 1
]

{ #category : 'private' }
TextEditor >> indent: delta fromStream: inStream toStream: outStream [
	"Append the contents of inStream to outStream, adding or deleting delta or -delta
	 tabs at the beginning, and after every NewLine except a final NewLine.  Do not add tabs
	 to totally empty lines, and be sure nothing but tabs are removed from lines."

	| ch skip tab prev atEnd |
	tab := Character tab.
	delta > 0
		ifTrue: [
			"shift right"
			prev := Character newLineCharacter.
			[
			ch := (atEnd := inStream atEnd)
				ifTrue: [ Character newLineCharacter ]
				ifFalse: [ inStream next ].
			(prev isLineSeparator and: [ ch isLineSeparator not ]) ifTrue: [ delta timesRepeat: [ outStream nextPut: tab ]].
			atEnd ] whileFalse: [
				outStream nextPut: ch.
				prev := ch ]]
		ifFalse: [
			"shift left"
			skip := delta.
			"a negative number"
			[ inStream atEnd ] whileFalse: [
				((ch := inStream next) = tab and: [ skip < 0 ]) ifFalse: [ outStream nextPut: ch ].
				skip := ch isLineSeparator
					ifTrue: [ delta ]
					ifFalse: [ skip + 1 ]]]
]

{ #category : 'initialization' }
TextEditor >> initialize [
	emphasisHere := #().
	initialSelectionStart := nil.
	initialSelectionStop := nil.
	doWordSelection := false.
	doParagraphSelection := false.
	defaultFont := nil.
]

{ #category : 'new selection' }
TextEditor >> insertAndSelect: aString at: anInteger [
	"This is a user command, and generates undo"

	| newText |
	newText := (aString is: #Text) ifTrue: [aString] ifFalse: [Text string: aString attributes: emphasisHere].
	self deselectAndPlaceCursorAt: anInteger.
	self replaceSelectionWith: newText.
	self selectFrom: anInteger to: anInteger + newText size - 1
]

{ #category : 'commands' }
TextEditor >> insertMorph: aMorph at: relPt [
	"This is a user command, and generates undo"
	| index newText |

	"Warning: As undo will only be done for text in the model, undoing this will neither
	kill the morph nor send it to another owner! See implementors of #removeMorph: in TextMorphs.
	A specific undo for inserting / removing morphs might be in order. But I doubt the
	TextModel should be responsible for that."
	self flag: #jmv.

	index := (textComposition characterBlockAtPoint: relPt) stringIndex.
	newText := ' ', (Text string: '*' attribute: (TextAnchor new anchoredFormOrMorph: aMorph)), ' '.
	model logUndoAndReplaceFrom: index to: index-1 with: newText.
	textComposition recomposeFrom: index to: index + newText size -1 delta: newText size.
]

{ #category : 'private' }
TextEditor >> isDisjointFrom: anInterval [
	"Answer true if anInterval is empty and not touching or within the current
	 interval, or if anInterval is a not empty but it does not overlap the current
	 selection."

	| fudge |
	fudge := anInterval size = 0 ifTrue: [1] ifFalse: [0].
	^(anInterval last + fudge < self startIndex or:
			[anInterval first - fudge >= self stopIndex])

]

{ #category : 'accessing' }
TextEditor >> lastFont [
	"Answer the Font for to be used if positioned at the end of the text"

	^self startIndex > model textSize
		ifTrue: [ model actualContents fontIfApplying: emphasisHere default: self defaultFont ]
		ifFalse: [ model actualContents fontAt: model textSize +1 default: self defaultFont ]
]

{ #category : 'accessing' }
TextEditor >> lastParagraphStyleOrNil [
	"Answer the ParagraphStyle for to be used if positioned at the end of the text"

	^self startIndex > model textSize
		ifTrue: [ model actualContents paragraphStyleOrNilIfApplying: emphasisHere ]
		ifFalse: [ model actualContents paragraphStyleOrNilAt: model textSize +1 ]
]

{ #category : 'editing keys' }
TextEditor >> makeCapitalized [
	"Force the current selection to uppercase.  Triggered by Cmd-X."
	"This is a user command, and generates undo"

	| prev |
	prev := $-.  "not a letter"
	self replaceSelectionWith: 
		(self selectedString collect:
			[:c | prev := prev isLetter ifTrue: [c asLowercase] ifFalse: [c asUppercase]]).
]

{ #category : 'editing keys' }
TextEditor >> makeLowercase [
	"Force the current selection to lowercase.  Triggered by Cmd-X."
	"This is a user command, and generates undo"

	self replaceSelectionWith: self selectedString asLowercase.
]

{ #category : 'editing keys' }
TextEditor >> makeUppercase [
	"Force the current selection to uppercase.  Triggered by Cmd-Y."
	"This is a user command, and generates undo"

	self replaceSelectionWith: self selectedString asUppercase.
]

{ #category : 'accessing-selection' }
TextEditor >> markIndex [
	markBlock ifNil: [^1].
	^ markBlock stringIndex
]

{ #category : 'accessing-selection' }
TextEditor >> markIndex: anIndex [
	"Called, for example, when selecting text with shift+arrow keys"
	markBlock := (textComposition characterBlockForIndex: anIndex)

]

{ #category : 'accessing-selection' }
TextEditor >> markIndex: anIndex pointIndex: anotherIndex [
	"Called, for example, when selecting text with shift+arrow keys"
	self
		markIndex: anIndex;
		pointIndex: anotherIndex
]

{ #category : 'model access' }
TextEditor >> model: aModel [
	model := aModel
]

{ #category : 'events' }
TextEditor >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	| b |

	initialSelectionStart := nil.
	initialSelectionStop := nil.
	doWordSelection := false.
	doParagraphSelection := false.

	b := textComposition characterBlockAtPoint: localEventPosition.
	(textComposition clickAt: localEventPosition) ifTrue: [
		markBlock := b.
		pointBlock := b.
		^ self ].
	
	aMouseButtonEvent shiftPressed
		ifFalse: [
			markBlock := b.
			pointBlock := b.	
			self setEmphasisHereFromText ]
]

{ #category : 'events' }
TextEditor >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [
	| cursorBlock cursorIndex startBlock startIndex stopBlock stopIndex |

	aMouseButtonEvent shiftPressed
		ifTrue: [
			"Squeak classic behavior for click, move, shift-click sequence "
			"pointBlock _(textComposition characterBlockAtPoint: (evt eventPosition))."

			"Mac behavior"
			cursorBlock := textComposition characterBlockAtPoint: localEventPosition.
			cursorIndex := cursorBlock stringIndex.
			startBlock := self startBlock min: cursorBlock.
			startIndex := startBlock stringIndex.
			stopBlock := self stopBlock max: cursorBlock.
			stopIndex := stopBlock stringIndex.
			(stopIndex - cursorIndex) < (cursorIndex - startIndex)
				ifTrue: [
					markBlock := startBlock.
					pointBlock := cursorBlock ]
				ifFalse: [
					markBlock := stopBlock.
					pointBlock := cursorBlock ]].
	self storeSelectionInComposition
]

{ #category : 'events' }
TextEditor >> mouseMove: aMouseMoveEvent localPosition: localEventPosition [
	"Change the selection in response to mouse-down drag"

	| newPointBlock goingBackwards newStartBlock newStopBlock interval i1 i2 |
	newPointBlock := textComposition characterBlockAtPoint: localEventPosition.
	goingBackwards := newPointBlock stringIndex < markBlock stringIndex.

	doWordSelection ifTrue: [
		pointBlock := newPointBlock.
		self selectWord.
		newStartBlock := self startBlock min: initialSelectionStart.
		newStopBlock := self stopBlock max: initialSelectionStop.
		markBlock := goingBackwards ifTrue: [newStopBlock] ifFalse: [newStartBlock].
		pointBlock := goingBackwards ifTrue: [newStartBlock] ifFalse: [newStopBlock].
		self storeSelectionInComposition.
		^self ].

	doParagraphSelection ifTrue: [
		i1 := newPointBlock stringIndex min: initialSelectionStart stringIndex.
		i2 := newPointBlock stringIndex max: initialSelectionStop stringIndex-1.
		interval := self privateCurrentString encompassParagraph: (i1 to: i2).
		self selectFrom: interval first to: interval last.
		newStartBlock := self startBlock min: initialSelectionStart.
		newStopBlock := self stopBlock max: initialSelectionStop.
		markBlock := goingBackwards ifTrue: [newStopBlock] ifFalse: [newStartBlock].
		pointBlock := goingBackwards ifTrue: [newStartBlock] ifFalse: [newStopBlock].
		self storeSelectionInComposition.
		^self ].

	pointBlock := newPointBlock.
	self storeSelectionInComposition
]

{ #category : 'private' }
TextEditor >> moveCursor: directionBlock forward: forward event: aKeyboardEvent [

	super moveCursor: directionBlock forward: forward event: aKeyboardEvent.
	self setEmphasisHereFromTextForward: forward
]

{ #category : 'private' }
TextEditor >> nullText [

	^Text string: '' attributes: emphasisHere
]

{ #category : 'attributes' }
TextEditor >> offerColorMenu [
	"Present a menu of available colors, and if one is chosen, apply it to the current selection."
	"This is a user command, and generates undo"

	| attribute colors index thisSel |
	thisSel := self selection.
	colors := #(#black #magenta #red #yellow #green #blue #cyan #white ).
	index := (PopUpMenu
		labelArray: colors ", #('choose color...' )
		lines: (Array with: colors size)") startUpMenu.
	index = 0 ifTrue: [ ^ true ].
	index <= colors size
		ifTrue: [ attribute := TextColor color: (Color perform: (colors at: index)) ]
		ifFalse: [
			index := index - colors size - 1.
			"Re-number!!!"
			index = 0 ifTrue: [ attribute := self chooseColor ].
			thisSel ifNil: [ ^ true ]].
	attribute ifNotNil: [ self applyAttribute: attribute ].
	morph updateFromTextComposition.
	^ true.
]

{ #category : 'attributes' }
TextEditor >> offerFontMenu [
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	"This is a user command, and generates undo"

	| currentFont attr startIndex |
	startIndex := self startIndex.
	currentFont := model actualContents fontAt: startIndex default: self defaultFont.
	(FontFamily promptUser: currentFont familyName) ifNotNil: [ :fontFamily |
		attr := TextFontFamily familyName: fontFamily familyName.
		self applyAttribute: attr.
		morph updateFromTextComposition ].
]

{ #category : 'attributes' }
TextEditor >> offerFontSizeMenu [
	| startIndex currentFont fontFamily attr |
	startIndex := self startIndex.
	currentFont := model actualContents fontAt: startIndex default: self defaultFont.
	fontFamily := currentFont family.
	(FontFamily
		promptUserForSize: currentFont pointSize
		fontFamily: fontFamily) ifNotNil: [ :ptSize |
			attr := TextFontSize pointSize: ptSize.
			self applyAttribute: attr.
			morph updateFromTextComposition ].
]

{ #category : 'attributes' }
TextEditor >> offerFontStyleMenu [
	"This is a user command, and generates undo"

	| emphases menuStrings entries reply code startIndex attribute |
	startIndex := self startIndex.
	code := model actualContents emphasisAt: startIndex.
	emphases := #(bold italic underlined struckThrough superscript subscript).
	menuStrings := Array streamContents: [ :strm |
		strm nextPut:(code isZero ifTrue: ['<on>'] ifFalse: ['<off>']), 'normal'.
		emphases do: [ :emph |
			strm nextPut:
				((code anyMask: (TextEmphasis perform: emph) emphasisCode)
					ifTrue: ['<on>'] ifFalse: ['<off>']),
				emph asPlainString ]].
	entries := `#(normal)`, emphases.
	reply := (SelectionMenu labelList: menuStrings lines: #(1) selections: entries) startUpMenu.
	reply ifNotNil: [
		attribute := TextEmphasis perform: reply.
		((menuStrings at: (entries indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttributes: {attribute} ]
			ifFalse: [ self applyAttribute:  attribute ]].
	morph updateFromTextComposition.
]

{ #category : 'attributes' }
TextEditor >> offerLineSpacingMenu [
	"This is a user command, and generates undo"

	| aList reply  |
	aList := `{ '0.9'. '1.0'. '1.15'. '1.3'. '1.5'. '1.7'. '2.0'}`.
	reply := (SelectionMenu labelList: aList selections: aList) startUpWithCaption: 'Line Spacing'.
	reply ifNil: [ ^self ].
	self applyAttribute: (TextLineSpacing factor: reply asNumber).
	morph updateFromTextComposition.
	^ true
]

{ #category : 'undo & redo' }
TextEditor >> offerUndoHistory [
	| index labels current |
	current := model undoRedoCommandsPosition.
	labels := model undoRedoCommands withIndexCollect: [ :each :i | 
		(i = current ifTrue: [ '<on>' ] ifFalse: [ '<off>' ]), each printString ].
	labels isEmpty ifFalse: [
		index := (PopUpMenu
			labelArray: labels
			lines: #()) startUpMenu.
		index = current ifTrue: [ ^self ].
		index = 0 ifTrue: [ ^self ].
		index < current
			ifTrue: [ current - index timesRepeat: [ self undo ]]
			ifFalse: [ index - current timesRepeat: [ self redo ]]]
]

{ #category : 'menu messages' }
TextEditor >> openHelp [
	"Show help screen"
	TextModel new contents: self help; openLabel: self name, ' Help'.
]

{ #category : 'editing keys' }
TextEditor >> outdent: aKeyboardEvent [
	"Remove a tab from the front of every line occupied by the selection.
	Invoked from keyboard via cmd-shift-L.  2/29/96 sw"

	^ self inOutdent: aKeyboardEvent delta: -1
]

{ #category : 'private' }
TextEditor >> pageHeight [
	| howManyLines visibleHeight totalHeight ratio |
	howManyLines := textComposition numberOfLines.
	visibleHeight := self visibleHeight.
	totalHeight := self totalTextHeight.
	ratio := visibleHeight / totalHeight.
	^(ratio * howManyLines) rounded - 2
]

{ #category : 'menu messages' }
TextEditor >> pasteRecent [
	"Paste an item chose from RecentClippings."

	| clipping |
	(clipping := Clipboard chooseRecentClipping) ifNil: [^ self].
	self clipboardTextPut: clipping.
	^ self paste
]

{ #category : 'accessing-selection' }
TextEditor >> pointBlock [
	^pointBlock
]

{ #category : 'accessing-selection' }
TextEditor >> pointIndex [
	pointBlock ifNil: [^1].
	^ pointBlock stringIndex
]

{ #category : 'accessing-selection' }
TextEditor >> pointIndex: anIndex [
	"Called, for example, when selecting text with shift+arrow keys"
	pointBlock := textComposition characterBlockForIndex: anIndex
]

{ #category : 'private' }
TextEditor >> privateCurrentString [
	"Answer the string I'm editing. Private. Just for internal Editor use."

	^model actualContents string
]

{ #category : 'typing support' }
TextEditor >> processKeystrokeEvent: aKeyboardEvent [
	"Key struck on the keyboard. Find out which one and, if special, carry 
	out the associated special action. Otherwise, add the character to the 
	stream of characters."

	(self dispatchOn: aKeyboardEvent) ifTrue: [
		self storeSelectionInComposition.
		^self].

	markBlock := pointBlock.
	self storeSelectionInComposition
]

{ #category : 'current selection' }
TextEditor >> recomputeSelection [
	"The same characters are selected but their coordinates may have changed."

	self markIndex: self markIndex pointIndex: self pointIndex.
]

{ #category : 'undo & redo' }
TextEditor >> redo [

	morph disablesEditing ifTrue: [
		^ self ].

	model redoAndEvaluate: [ :modelUpdated :newCursorPos |
		newCursorPos
			ifNil: [ self recomputeSelection ]
			ifNotNil: [ self markIndex: newCursorPos pointIndex: newCursorPos ].
		modelUpdated ifTrue: [
			self userHasEdited.
			textComposition composeAll.			"this could be made more specific..."
			morph possiblyChanged ]]
]

{ #category : 'commands' }
TextEditor >> removeMorph: aMorph [
	"This is a user command, and generates undo"

	| range |

	"Warning: As undo will only be done for text in the model, undoing this will neither
	recreate the morph nor add it to the TextMorph! (See senders of #insertMorph:at:
	A specific undo for inserting / removing morphs might be in order. But I doubt the
	TextModel should be responsible for that."
	self flag: #jmv.

	range := model actualContents find: (TextAnchor new anchoredFormOrMorph: aMorph).
	range ifNotNil: [
		model logUndoAndReplaceFrom: range first to: range last with: Text new.
		textComposition recomposeFrom: range first to: range first  -1 delta: range size negated ]
]

{ #category : 'accessing' }
TextEditor >> replaceSelectionWith: aTextOrString [
	"Deselect, and replace the selection text by aText."
	"This is a user command, and generates undo"

	| start stop replacement |

	morph disablesEditing ifTrue: [
		^ self ].

	morph aboutToEdit.
	start := self startIndex.
	stop := self stopIndex.
	(aTextOrString isEmpty and: [stop > start]) ifTrue: [
		"If deleting, then set emphasisHere from 1st character of the deletion"
		emphasisHere := (model actualContents attributesAt: start) select: [:att |
			att mayBeExtended]].

	(start = stop and: [ aTextOrString isEmpty ]) ifFalse: [
		replacement := self addAttributesForPasting: aTextOrString.
		model logUndoAndReplaceFrom: start to: stop - 1 with: replacement.
		textComposition
			recomposeFrom: start
			to:  start + replacement size - 1
			delta: replacement size - (stop-start).
		self deselectAndPlaceCursorAt: start + replacement size.
		self userHasEdited.  " -- note text now dirty"
		morph updateFromTextComposition ].

	morph possiblyChanged
]

{ #category : 'initialization' }
TextEditor >> resetState [
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character and set the emphasis to that of the first character"

	markBlock := textComposition defaultCharacterBlock.
	pointBlock := markBlock.
]

{ #category : 'private' }
TextEditor >> sameColumn: start newLine: lineBlock forward: isForward [
	"Private - Compute the index in my text
	with the line number derived from lineBlock,"
	" a one argument block accepting the old line number.
	The position inside the line will be preserved as good as possible"
	"The boolean isForward is used in the border case to determine if
	we should move to the beginning or the end of the line."
	| column currentLine offsetAtTargetLine targetEOL lines numberOfLines currentLineNumber targetLineNumber |
	lines := textComposition lines.
	numberOfLines := textComposition numberOfLines.
	currentLineNumber  := textComposition lineIndexFor: start.
	currentLine := lines at: currentLineNumber.
	column := start - currentLine first.
	targetLineNumber := ((lineBlock value: currentLineNumber) max: 1) min: numberOfLines.
	offsetAtTargetLine := (lines at: targetLineNumber) first.
	targetEOL := (lines at: targetLineNumber) last + (targetLineNumber = numberOfLines ifTrue:[1]ifFalse:[0]).
	targetLineNumber = currentLineNumber
	"No movement or movement failed. Move to beginning or end of line."
		ifTrue:[
			^isForward
				ifTrue:[targetEOL]
				ifFalse:[offsetAtTargetLine]].
	^offsetAtTargetLine + column min: targetEOL.
]

{ #category : 'new selection' }
TextEditor >> selectFrom: start to: stop [
	"Select the specified characters inclusive.
	I.e. if contents is 'hello' and we want to select the $e, do 'self selectFrom: 2 to: 2'
	If we want to put cursor after e, but with no selection, do 'self selectFrom: 3 to: 2', or better yet, call #selectAt:"
	self selectInvisiblyFrom: start to: stop.
	self storeSelectionInComposition.
	"Preserve current emphasis if selection is empty"
	start > stop ifTrue: [
		self setEmphasisHereFromText ]
]

{ #category : 'new selection' }
TextEditor >> selectLine [
	"Make the receiver's selection, encompass the current line."

	self selectInterval: (model actualContents encompassLine: self selectionInterval)
]

{ #category : 'new selection' }
TextEditor >> selectWordOrDelimitedTextAt: localEventPosition [
	"Select delimited text or word--the result of double-clicking."

	| hitCharacterBlock range p |
	p := localEventPosition x - 1 @ localEventPosition y.
	hitCharacterBlock := textComposition characterBlockIncludingPoint: p.
	range := self
		delimitedRangeAt: hitCharacterBlock stringIndex
		hitOnRightHalf: p x  > hitCharacterBlock center x
		leftDelimiters: '([{<'
		rightDelimiters: ')]}>'
		simpleDelimiters: '|''"`'.
	self selectFrom: range first to: range last.
]

{ #category : 'accessing-selection' }
TextEditor >> selectedString [

	^self selection string
]

{ #category : 'accessing-selection' }
TextEditor >> selection [
	"Answer the text that is currently selected."

	| t firstIndex lastIndex |
	t := model actualContents.
	firstIndex := self startIndex.
	lastIndex := self stopIndex - 1.
	(firstIndex = 1 and: [ lastIndex = t size ])
		ifTrue: [ ^t copy ].
	^t copyFrom: firstIndex to: lastIndex
]

{ #category : 'accessing' }
TextEditor >> selectionRectangle [
	"Answer a rectangle that encompasses selection.
	If no selection, answer a rectangle that includes cursor."
	^ markBlock quickMerge: pointBlock
]

{ #category : 'typing support' }
TextEditor >> setEmphasisHereFromText [

	self setEmphasisHereFromTextForward: true
]

{ #category : 'typing support' }
TextEditor >> setEmphasisHereFromTextForward: f [

	| i forward attributeIndex prevIsLineSeparator nextIsLineSeparator prevIsSeparator nextIsSeparator nextChar prevChar |
	i := self pointIndex.
	"Try to set emphasisHere correctly after whitespace.
	Most important after a cr, i.e. at the start of a new line"
	prevIsLineSeparator :=  i > 1 and: [ (prevChar := model actualContents at: i-1) isLineSeparator ].
	nextIsLineSeparator := i <= model textSize and: [ (nextChar := model actualContents at: i) isLineSeparator ].
	prevIsSeparator :=  i > 1 and: [ prevChar isSeparator ].
	nextIsSeparator := i <= model textSize and: [ nextChar isSeparator ].
	prevIsLineSeparator & nextIsLineSeparator
		ifTrue: [
			"Empty paragraph: take emphasis from the newLine character"
			forward := false ]
		ifFalse: [
			prevIsSeparator == nextIsSeparator
				ifTrue: [
					"Space at both sides, or non-space at both sides, take emphasis used where the cursor comes from"
					forward := f ]
				ifFalse: [
					"Space at one side and non-space at the other, take emphasis from the non-space character"
					forward := nextIsSeparator ]].
	attributeIndex := i - (forward ifTrue: [ 1 ] ifFalse: [ 0 ]).
	attributeIndex > 0 
		ifTrue: [
			emphasisHere := (model actualContents attributesAt: attributeIndex)
						select: [ :att | att mayBeExtended ].
			morph possiblyChanged ].
]

{ #category : 'accessing' }
TextEditor >> setSearch: aStringOrText [
	"Set the FindText and ChangeText to seek aString; except if already seeking aString, leave ChangeText alone so again will repeat last replacement."

	self class findText = aStringOrText
		ifFalse: [FindText := ChangeText := aStringOrText]
]

{ #category : 'menu messages' }
TextEditor >> setSearchString [
	"Make the current selection, if any, be the current search string."
	self hasSelection ifFalse: [morph flash. ^ self].
	self setSearch: self selectedString
]

{ #category : 'private' }
TextEditor >> shouldDeleteAWordForward: aKeyboardEvent [

	^ aKeyboardEvent isDelete and: [
		aKeyboardEvent rawMacOptionKeyPressed or: [
			aKeyboardEvent controlKeyPressed ] ]
]

{ #category : 'accessing-selection' }
TextEditor >> startBlock [
	^ pointBlock min: markBlock
]

{ #category : 'accessing-selection' }
TextEditor >> startIndex [
	pointBlock ifNil: [^1].
	^ self startBlock stringIndex
]

{ #category : 'accessing-selection' }
TextEditor >> stopBlock [
	^ pointBlock max: markBlock
]

{ #category : 'accessing-selection' }
TextEditor >> stopIndex [
	pointBlock ifNil: [^1].
	^ self stopBlock stringIndex
]

{ #category : 'private' }
TextEditor >> storeSelectionInComposition [
	"for proper display of selected text"

	pointBlock ifNil: [ ^self ].
	textComposition
		selectionStartBlock: self startBlock
		selectionStopBlock: self stopBlock.
]

{ #category : 'editing keys' }
TextEditor >> tabKey: aKeyboardEvent [
	"Add/remove a tab at the front of every line occupied by the selection if there is one; treat as a normal character otherwise."
	
	aKeyboardEvent shiftPressed 
		ifTrue: [ ^ self outdent: aKeyboardEvent ].
	
	^ self hasSelection 
		ifTrue: [ self indent: aKeyboardEvent ]
		ifFalse: [ self normalCharacter: aKeyboardEvent ]
]

{ #category : 'accessing' }
TextEditor >> text [
	"The returned object should be treated as read-only, and never modified"
	^ model actualContents
]

{ #category : 'initialization' }
TextEditor >> textComposition: aTextComposition  [
	"Install aTextComposition as the one to be edited by the receiver."

	textComposition := aTextComposition
]

{ #category : 'accessing' }
TextEditor >> totalTextHeight [

	^textComposition lines last bottom
]

{ #category : 'private' }
TextEditor >> unapplyAttributes: textAttributes [
	"The user selected textAttributes to be removed.
	If there is a selection, unapply the attributes to the selection.
	In any case do not use the attribute for the user input (emphasisHere)"
	"This generates undo"

	| interval |
	emphasisHere := emphasisHere copyWithoutAll: textAttributes.
	interval := self selectionInterval.
	(interval isEmpty and: [ textAttributes noneSatisfy: [ :each | each isParagraphAttribute ]])
		ifTrue: [ ^self ].
	model logUndoAndRemoveAttributes: textAttributes from: interval first to: interval last.
	textComposition recomposeFrom: interval first to: interval last delta: 0.
	self recomputeSelection.	"Needed so visible selection is updated to reflect new visual extent of selection"
	morph possiblyChanged
]

{ #category : 'undo & redo' }
TextEditor >> undo [

	morph disablesEditing ifTrue: [
		^ self ].

	model undoAndEvaluate: [ :modelUpdated :newCursorPos |
		newCursorPos
			ifNil: [ self recomputeSelection ]
			ifNotNil: [ self markIndex: newCursorPos pointIndex: newCursorPos ].
		modelUpdated ifTrue: [
			self userHasEdited.
			textComposition composeAll.				"this could be made more specific..."
			morph possiblyChanged ]]
]

{ #category : 'accessing' }
TextEditor >> userHasEdited [

	morph hasUnacceptedEdits: model isDirty
]

{ #category : 'accessing' }
TextEditor >> visibleHeight [

	^morph owner morphHeight
]

{ #category : 'model access' }
TextEditor >> wordAt: aPositionInText [
	
	| wordUnderCursorRange |
	wordUnderCursorRange := self wordRangeIncluding: aPositionInText.
	^(model actualContents copyFrom: wordUnderCursorRange first to: wordUnderCursorRange last) asPlainString.
]

{ #category : 'model access' }
TextEditor >> wordUnderCursor [
	
	^self wordAt: self pointIndex
]

{ #category : 'menu messages' }
TextEditor >> wrapOnOff [
	"Toggle WordWrap"

	morph wrapOnOff
]
