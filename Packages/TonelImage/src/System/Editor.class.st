"
New text editors.
TextEditor provides most of the functionality that used to be in TextMorphEditor.
SmalltalkEditor has Smalltalk code specific features.
SimpleEditor provides basic functionality for single line text editing. It does not handle fonts and styles, aligning and Smalltalk utilities. It handles one single line.
"
Class {
	#name : 'Editor',
	#superclass : 'Object',
	#instVars : [
		'morph'
	],
	#classInstVars : [
		'keyboardActions'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'class initialization' }
Editor class >> initialize [
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c | c initializeKeyboardActions ].
]

{ #category : 'class initialization' }
Editor class >> initializeKeyboardActions [
	"Initialize the table for regular (i.e. non-command) keystroke dispatch"
	"
	self initializeKeyboardActions
	"
	| actions |
	actions := Array new: 256 withAll: #normalCharacter:.
	0 to: 31 do: [ :i | actions at: i+1 put: #noop: ].
	actions at: 1 + 1 put: #cursorHome:.				"home key"
	actions at: 3 + 1 put: #enter:.					"enter / return key"
	actions at: 4 + 1 put: #cursorEnd:.				"end key"
	actions at: 5 + 1 put: #noop:.					"insert key"
	actions at: 8 + 1 put: #backspace:.				"macDelete winBackspace key"
	actions at: 9 + 1 put: #normalCharacter:.		"tab"
	actions
		at:  InputSensor returnKey + 1
		put: #returnKey:.							"return (sometimes labelled enter) key"
	actions at: 27 + 1 put: #offerMenuFromEsc:.		"escape key"
	actions at: 28 + 1 put: #cursorLeft:.				"left arrow key"
	actions at: 29 + 1 put: #cursorRight:.			"right arrow key"
	actions at: 127 + 1 put: #forwardDelete:.		"winDelete key"
	keyboardActions := actions
]

{ #category : 'keyboard shortcut tables' }
Editor class >> keyboardActions [
	"Same for all instances.
	A subclass could handle specific keyboard actions for each instance, though."
	
	keyboardActions ifNil: [ self initializeKeyboardActions ].
	^keyboardActions
]

{ #category : 'cached state access' }
Editor class >> releaseClassCachedState [

	keyboardActions := nil.
]

{ #category : 'typing/selecting keys' }
Editor >> backspace: aKeyboardEvent [
	"Backspace over the last character."
	"This is a user command, and generates undo"

	| startIndex |
	(aKeyboardEvent rawMacOptionKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [ ^ self backWord: aKeyboardEvent ].
	startIndex := self markIndex + (self hasSelection ifTrue: [1] ifFalse: [0]).
	startIndex := 1 max: startIndex - 1.
	self backTo: startIndex.
	^ false
]

{ #category : 'private' }
Editor >> beginningOfLine: position [
	"Redefined in subclasses using TextComposition"
	^ self beginningOfParagraph: position
]

{ #category : 'private' }
Editor >> beginningOfNextParagraph: position [
	| s |
	s := self privateCurrentString.
	^ (s
		indexOf: Character newLineCharacter
		startingAt: position
		ifAbsent: [ s size ])
			+ 1
]

{ #category : 'private' }
Editor >> beginningOfParagraph: position [
	| s |
	s := self privateCurrentString.
	^ (s
		lastIndexOf: Character newLineCharacter
		startingAt: (position-1 min: s size)
		ifAbsent: [ 0 ])
			+ 1.
]

{ #category : 'private' }
Editor >> beginningOfText [
	^1
]

{ #category : 'clipboard' }
Editor >> clipboardStringOrText [

	| clipContents |
	clipContents := Clipboard retrieveStringOrText.
	^ ((clipContents is: #Text) and: [ clipContents respondsTo: #asNonStyledText ])
		ifTrue: [ clipContents asNonStyledText ]
		ifFalse: [ clipContents ]
]

{ #category : 'clipboard' }
Editor >> clipboardTextPut: textOrString [

	^ Clipboard storeObject: textOrString
]

{ #category : 'accessing' }
Editor >> currentAttributes [
	"Redefined by subclasses that handle TextAttributes"
	^nil
]

{ #category : 'nonediting/nontyping keys' }
Editor >> cursorLeft: aKeyboardEvent [
	"Private - Move cursor left one character if nothing selected, otherwise move cursor to beginning of selection. If the shift key is down, start selecting or extending current selection.
	Don't allow cursor past beginning of text"

	self
		moveCursor: [ :position | | newPos |
			newPos := position - 1.
			"Mac standard keystroke"
			(aKeyboardEvent rawMacOptionKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [ newPos := self previousWordStart: position ].
			"Mac standard keystroke"
			aKeyboardEvent commandAltKeyPressed ifTrue: [
				newPos := self beginningOfLine: position ].
			newPos ]
		forward: false
		event: aKeyboardEvent.
	^ true
]

{ #category : 'nonediting/nontyping keys' }
Editor >> cursorRight: aKeyboardEvent  [
	"Private - Move cursor right one character if nothing selected, otherwise move cursor to end of selection. If the shift key is down, start selecting characters or extending already selected characters. 
	Don't allow cursor past end of text"

	self
		moveCursor: [ :position | | newPos |
			newPos := position + 1.
			"Mac standard keystroke"
			(aKeyboardEvent rawMacOptionKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [ newPos := self nextWordEnd: position ].
			"Mac standard keystroke"
			aKeyboardEvent commandAltKeyPressed ifTrue: [
				newPos := self endOfLine: position ].
			newPos ]
		forward: true
		event: aKeyboardEvent.
	^ true
]

{ #category : 'typing/selecting keys' }
Editor >> cursorTopHome [
	"Put cursor at beginning of text -- invoked from cmd-H shortcut, useful for keyboards that have no home key."

	self deselectAndPlaceCursorAt: 1.
]

{ #category : 'menu & kbd shortcuts' }
Editor >> defaultMenuSpec [
	^#()
]

{ #category : 'new selection' }
Editor >> delimitedRangeAt: index hitOnRightHalf: hitOnRightHalfOfDelimiter leftDelimiters: leftDelimiters rightDelimiters: rightDelimiters simpleDelimiters: simpleDelimiters [
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter finalDelimiter direction leftMatch rightMatch simpleMatch includeDelimiters level string hitOnRightHalf here hereChar start stop sSize |
	string := self privateCurrentString.
	sSize := string size.
	sSize < 2 ifTrue: [^1 to: 1].
	hitOnRightHalf := hitOnRightHalfOfDelimiter.
	here := index min: sSize max: 2.
	((string at: here) isLineSeparator and: [ here > 2 ]) ifTrue: [
		here := here - 1.
		hitOnRightHalf := true ].
	initialDelimiter := string at: here.
	leftMatch := leftDelimiters indexOf: initialDelimiter.
	rightMatch := rightDelimiters indexOf: initialDelimiter.
	simpleMatch := simpleDelimiters  indexOf: initialDelimiter.
	leftMatch + rightMatch + simpleMatch = 0 ifTrue: [
		"no delimiters, then just select word."
		^self wordRangeIncluding: index ].

	(simpleMatch > 0 & hitOnRightHalf or: [leftMatch > 0])
		ifTrue: [	"Hit on right half of simple delimiter or is a left delimiter -- match to the right"
			start := here.
			direction := 1 ]
		ifFalse: [ 	"Hit on left half of simple delimiter or is a right delimiter-- match to the left"
			stop := here.
			direction :=  -1 ].
	
	simpleMatch > 0
		ifTrue: [
			finalDelimiter := initialDelimiter.
			includeDelimiters := false ]
		ifFalse: [
			leftMatch > 0
				ifTrue: [
					finalDelimiter := rightDelimiters at: leftMatch.
					includeDelimiters := hitOnRightHalf not ]
				ifFalse: [
					finalDelimiter := leftDelimiters at: rightMatch.
					includeDelimiters := hitOnRightHalf ]].

	level := 1.
	[ level > 0 and: [ direction > 0
			ifTrue: [here < sSize]
			ifFalse: [here > 1]]]
		whileTrue: [
			here := here + direction.
			hereChar := string at: here.
			"delimiter match just counts nesting level"
			hereChar = finalDelimiter
				ifTrue: [level := level - 1"leaving nest"]
				ifFalse: [
					hereChar = initialDelimiter 
						ifTrue: [level := level + 1"entering deeper nest" ]]].
	level > 0 ifTrue: [
		"If we failed to find final delimiter, then just select word."
		^self wordRangeIncluding: index ].
	direction > 0
		ifTrue: [ stop := here ]
		ifFalse: [ start := here ].
	^includeDelimiters
		ifTrue: [ start to: stop ]
		ifFalse: [ start+1 to: stop-1 ]
]

{ #category : 'new selection' }
Editor >> deselectAndPlaceCursorAt: characterIndex  [
	"Deselect, then place the text cursor before the character at characterIndex.
	 Be sure it is in view."

	self selectFrom: characterIndex to: characterIndex - 1
]

{ #category : 'private' }
Editor >> endOfLine: position [
	"Redefined in subclasses using TextComposition"
	^self endOfParagraph: position
]

{ #category : 'private' }
Editor >> endOfParagraph: position [
	| s |
	s := self privateCurrentString.
	^ s
		indexOf: Character newLineCharacter
		startingAt: position
		ifAbsent: [ s size + 1 ]
]

{ #category : 'private' }
Editor >> endOfText [
	^self privateCurrentString size + 1
]

{ #category : 'typing/selecting keys' }
Editor >> enter: aKeyboardEvent [
	"Enter / return key was pressed"
	"Process the various Enter / Return keystrokes"
	"Not sure if this is ever called"
	
	^self returnKey: aKeyboardEvent
]

{ #category : 'accessing-selection' }
Editor >> hasSelection [

	self subclassResponsibility 
]

{ #category : 'accessing' }
Editor >> help [
	"
	TextEditor new help edit
	SmalltalkEditor new help edit
	"
	| commandKeyName tabs |
	commandKeyName := Smalltalk platformName = 'Mac OS' ifTrue: ['Command'] ifFalse: ['Ctrl'].
	tabs := String tab, String tab, String tab, String tab.
	^ String streamContents: [ :strm |
		self defaultMenuSpec do: [ :menuItemNshortcutSpec |
			menuItemNshortcutSpec size > 6 ifTrue: [
				menuItemNshortcutSpec third ifNotNil: [ :shortcutCharacter |
					strm nextPutAll: (commandKeyName, '-', shortcutCharacter asString, tabs, menuItemNshortcutSpec seventh).
				strm newLine ]]]].
]

{ #category : 'typing support' }
Editor >> keyboardActions [
	"Same for all instances.
	A subclass could handle specific keyboard actions for each instance, though."
	^self class keyboardActions
]

{ #category : 'private' }
Editor >> lineSelectAndEmptyCheck: returnBlock [
	"If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this.
	
	Do not push down this method. It is used in SimpleEditor of package Widgets-Extras
	"

	"if current selection is empty, then first select the entire line in which occurs before proceeding"
	self hasSelection ifFalse: [
		self selectLine ].
	self selectionIsEmptyOrSeparators ifTrue: [
		morph flash.
		^ returnBlock value ].
]

{ #category : 'accessing-selection' }
Editor >> markIndex [

	self subclassResponsibility 
]

{ #category : 'accessing' }
Editor >> morph [
	^ morph
]

{ #category : 'accessing' }
Editor >> morph: aMorph [
	"Install a link back to the morph being edited (esp for text links)"
	morph := aMorph 
]

{ #category : 'private' }
Editor >> moveCursor: directionBlock forward: forward event: aKeyboardEvent [
	"Private - Move cursor.
	directionBlock is a one argument Block that computes the new Position from a given one."
	| shift indices newPosition |
	shift := aKeyboardEvent notNil and: [aKeyboardEvent shiftPressed].
	indices := self
		setIndices: shift
		forward: forward.
	newPosition := directionBlock value: (indices at: #moving).
	shift
		ifTrue: [
			self
				selectMark: (indices at: #fixed)
				point: newPosition - 1 ]
		ifFalse: [ self deselectAndPlaceCursorAt: newPosition ]
]

{ #category : 'typing/selecting keys' }
Editor >> newLine: aKeyboardEvent [

	self addString: (UnicodeString with: Character newLineCharacter).
	^false
]

{ #category : 'private' }
Editor >> nextWordEnd: aPosition [
	
	^self nextWordEndFrom: aPosition goingForwards: true
]

{ #category : 'private' }
Editor >> nextWordEndFrom: aPosition goingForwards: goingForwardsBoolean [
	
	"Answer the position of the end of the next word on the current line going
	forwards (or backwards).
	If the given position is the end (or beginning) of the line then answer the
	beginning (or end) of the next (or previous) line."
	
	| string beginningOfLine endOfLine step offset index newPosition |
	
	string := self privateCurrentString.
	beginningOfLine := self beginningOfLine: aPosition.
	endOfLine := self endOfLine: aPosition.
	step := goingForwardsBoolean ifTrue: [1] ifFalse: [-1].
	offset := goingForwardsBoolean ifTrue: [0] ifFalse: [-1].
	
	index := aPosition.
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers not]]
			whileTrue: [index := index + step].
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers]]
			whileTrue: [index := index + step].
	
	newPosition := index = aPosition ifTrue: [index + step] ifFalse: [index].
	^newPosition min: string size + 1 max: 1
]

{ #category : 'private' }
Editor >> nextWordStart: aPosition [
	
	^self nextWordStartFrom: aPosition goingForwards: true
]

{ #category : 'private' }
Editor >> nextWordStartFrom: aPosition goingForwards: goingForwardsBoolean [
	
	"Answer the position of the start of the next word on the current line going
	forwards (or backwards).
	If the given position is the end (or beginning) of the line then answer the
	beginning (or end) of the next (or previous) line."
	
	| string beginningOfLine endOfLine step offset index newPosition |
	
	string := self privateCurrentString.
	beginningOfLine := self beginningOfLine: aPosition.
	endOfLine := self endOfLine: aPosition.
	step := goingForwardsBoolean ifTrue: [1] ifFalse: [-1].
	offset := goingForwardsBoolean ifTrue: [0] ifFalse: [-1].
	
	index := aPosition.
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers]]
			whileTrue: [index := index + step].
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers not]]
			whileTrue: [index := index + step].
	
	newPosition := index = aPosition ifTrue: [index + step] ifFalse: [index].
	^newPosition min: string size + 1 max: 1
]

{ #category : 'nonediting/nontyping keys' }
Editor >> noop: aKeyboardEvent  [
	"Unimplemented keyboard command; just ignore it."

	^ true
]

{ #category : 'typing/selecting keys' }
Editor >> normalCharacter: aKeyboardEvent  [
	"A nonspecial character is to be added to the stream of characters."

	| character i replacement |

	"Ignore keystrokes that could be keyboard shortcuts, even if not defined as such."
	((aKeyboardEvent keyValue between: 32 and: 126) and: [ aKeyboardEvent commandAltKeyPressed ])
		ifTrue: [ ^true ].

	"Convert strings like '\+161 ' (š) to unicode"
	aKeyboardEvent isSpace ifTrue: [ | string |
		string := self privateCurrentString.
		self pointIndex - 1 to: (self pointIndex - 7 max: 1) by: -1 do: [ :index |
		(string at: index) == $\ ifTrue: [ | key |
			key := string copyFrom: index + 1 to: self pointIndex - 1.
			(key size > 2 and: [ (key at: 1) == $+ ])
				ifTrue: [ | stream number value |
					stream := ReadStream on: key asUppercase from: 2 to: key size.
					[ number := Integer readFrom: stream  base: 16 ] on: Error do: [ ^ false ].
					value := Character codePoint: number.
					self
						selectFrom: index to: self pointIndex - 1;
						replaceSelectionWith: (UnicodeString with: value).
						^ false ] ] ] ].
	character := aKeyboardEvent keyCharacter.
	"Linux VMs don't automatically replace combining marks + space with ASCII,
	as Windows and Mac VMs do. It seems it is an app responsibility to do that in Linux."
	character isDiacriticalMark ifTrue: [
		i := self pointIndex-1.
		(i > 0 and: [ (self privateCurrentString at: i) == $  ]) ifTrue: [
			replacement := character caseOf: {
				[ `Character codePoint: 16r300` ] -> [ '`' ].
				[ `Character codePoint: 16r301` ] -> [ '''' ].
				[ `Character codePoint: 16r302` ] -> [ '^' ].
				[ `Character codePoint: 16r303` ] -> [ '~' ].
				[ `Character codePoint: 16r308` ] -> [ '"' ].
			} otherwise: [].
			replacement notNil ifTrue: [
				self selectFrom: i to: i; replaceSelectionWith: replacement.
				^ false ]
			]
		].
	self addString: (UnicodeString with: character).
	^false
]

{ #category : 'nonediting/nontyping keys' }
Editor >> offerMenuFromEsc: aKeyboardEvent [
	"The escape key was hit while the receiver has the keyboard focus; take action"

	^ aKeyboardEvent shiftPressed ifFalse: [
		morph mouseButton2Activity ]; not
]

{ #category : 'menu messages' }
Editor >> paste [
	"Paste the text from the shared buffer over the current selection and 
	redisplay if necessary."

	self replaceSelectionWith: self clipboardStringOrText
]

{ #category : 'menu messages' }
Editor >> pasteString [
	"Paste the text's string from the shared buffer over the current selection and 
	redisplay if necessary.
	Pasting a string means using destination current attributes if appropriate."

	self replaceSelectionWith: self clipboardStringOrText asPlainString
]

{ #category : 'accessing-selection' }
Editor >> pointIndex [

	self subclassResponsibility 
]

{ #category : 'private' }
Editor >> previousWordStart: aPosition [
	
	^self nextWordEndFrom: aPosition goingForwards: false
]

{ #category : 'private' }
Editor >> privateCurrentString [

	self subclassResponsibility
]

{ #category : 'typing/selecting keys' }
Editor >> returnKey: aKeyboardEvent [
	"Return / Enter / key was pressed"
	"Process the various Return / Enter keystrokes"
	
	morph acceptOnCR ifTrue: [
		^ true].
"Disabled. If someone needs this, we could add a preference."
false ifTrue: [
	aKeyboardEvent commandAltKeyPressed ifTrue: [
		(aKeyboardEvent controlKeyPressed | aKeyboardEvent rawMacOptionKeyPressed) ifTrue: [
			self addString: UnicodeString crString.
			^false ].
		self addString: UnicodeString crlfString.
		^false ].
].
	^ self newLine: aKeyboardEvent
]

{ #category : 'private' }
Editor >> sameColumn: start newLine: lineBlock forward: isForward [
	"See comment in other implementors."
	self subclassResponsibility
]

{ #category : 'typing/selecting keys' }
Editor >> selectAll [

	self selectFrom: 1 to: self privateCurrentString size
]

{ #category : 'new selection' }
Editor >> selectInterval: anInterval [
	"Deselect, then select the specified characters inclusive.
	 Be sure the selection is in view."

	self selectFrom: anInterval first to: anInterval last
]

{ #category : 'new selection' }
Editor >> selectInvisiblyFrom: start to: stop [
	"Select the designated characters, inclusive.  Make no visual changes."

	self markIndex: start pointIndex: stop + 1
]

{ #category : 'new selection' }
Editor >> selectInvisiblyMark: mark point: point [
	"Select the designated characters, inclusive.  Make no visual changes."

	self markIndex: mark pointIndex: point + 1
]

{ #category : 'new selection' }
Editor >> selectLine [

	self subclassResponsibility 
]

{ #category : 'new selection' }
Editor >> selectMark: mark point: point [
	"Deselect, then select the specified characters inclusive.
	 Be sure the selection is in view."

	(mark =  self markIndex and: [point + 1 = self pointIndex]) ifFalse: [
		self selectInvisiblyMark: mark point: point ]
]

{ #category : 'new selection' }
Editor >> selectWord [
	"Select exactly one word.
	See also #selectWordOrDelimitedText"

	| wordRange |
	wordRange := self wordRangeIncluding: self pointIndex.
	self selectFrom: wordRange first to: wordRange last
]

{ #category : 'accessing-selection' }
Editor >> selection [

	self subclassResponsibility 
]

{ #category : 'accessing-selection' }
Editor >> selectionInterval [
	"Answer the interval that is currently selected."

	^self startIndex to: self stopIndex - 1 
]

{ #category : 'accessing-selection' }
Editor >> selectionIsEmptyOrSeparators [
	"Answer true if no selection or selection is just separators (whitespace)"

	self hasSelection ifFalse: [ ^true ].
	^self selection isSeparators.

]

{ #category : 'private' }
Editor >> setIndices: shiftPressed forward: forward [
	"Little helper method that sets the moving and fixed indices according to some flags."
	| indices |
	indices := Dictionary new.
	(shiftPressed and:[Preferences at: #selectionsMayShrink])
		ifTrue: [
			indices at: #moving put: self pointIndex.
			indices at: #fixed put: self markIndex
		] ifFalse: [
			forward
				ifTrue:[
					indices at: #moving put: self stopIndex.
					indices at: #fixed put: self startIndex.
				] ifFalse: [
					indices at: #moving put: self startIndex.
					indices at: #fixed put: self stopIndex.
				]
		].
	^indices
]

{ #category : 'accessing-selection' }
Editor >> startIndex [

	self subclassResponsibility 
]

{ #category : 'accessing-selection' }
Editor >> stopIndex [

	self subclassResponsibility 
]

{ #category : 'accessing' }
Editor >> userHasEdited [
	"Note that my text is not free of user edits."

	morph hasUnacceptedEdits: true
]

{ #category : 'new selection' }
Editor >> wordRangeIncluding: aPositionInText [
	"Select delimited text or word--the result of double-clicking."

	| direction level string here hereChar start sSize |
	string := self privateCurrentString.
	sSize := string size.
	sSize < 2 ifTrue: [^1 to: 1].
	here := aPositionInText min: sSize max: 2.
	"no delimiters -- select a token"
	direction := -1.
	level := 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < sSize]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar := string at: (here := here + direction).
			"token scan goes left, then right"
			hereChar isValidInIdentifiers 	"Consider $: as a word separator"
				ifTrue: [
					here = 1
						ifTrue: [
							start := 1.
							"go right if hit string start"
							direction := 1]]
				ifFalse: [
					direction < 0
						ifTrue: [
							start := here + 1.
							"go right if hit non-token"
							direction := 1]
						ifFalse: [level := 0]]].
	level > 0 ifTrue: [
		here := here + direction ].
	"If a word ends with $: (a keyword), consider it part of the word"
	hereChar == $: ifTrue: [here := here + 1].
	^start to: here - 1
]

{ #category : 'private' }
Editor >> wordSelectAndEmptyCheck: returnBlock [
	"If the current selection is empty, expand it to be the entire current word; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this."

	"if current selection is empty, then first select the entire word in which occurs before proceeding"
	self hasSelection ifFalse: [
		self selectWord ].
	self selectionIsEmptyOrSeparators ifTrue: [
		^ returnBlock value ].
]
