"
See comment in Editor
"
Class {
	#name : 'SimpleEditor',
	#superclass : 'Editor',
	#instVars : [
		'string',
		'pointIndex',
		'markIndex'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'typing support' }
SimpleEditor >> addString: aString [
	"Think of a better name"

	self replaceSelectionWith: aString
]

{ #category : 'typing support' }
SimpleEditor >> backTo: startIndex [
	"During typing, backspace to startIndex."

	self markIndex: startIndex.
	self replaceSelectionWith: ''.
	self markIndex: self pointIndex
]

{ #category : 'typing/selecting keys' }
SimpleEditor >> backWord: aKeyboardEvent [
	^false
]

{ #category : 'initialization' }
SimpleEditor >> changeString: aString [
	"Install aString as the one to be edited by the receiver."

	string := aString.
	self resetState
]

{ #category : 'events' }
SimpleEditor >> clickAndHalf [

	| here |
	here := self pointIndex.
	(here between: 2 and: string size)
		ifTrue: [ self selectWord ]
		ifFalse: [
			"if at beginning or end, select entire string"
			self selectAll ].
]

{ #category : 'menu messages' }
SimpleEditor >> copySelection [
	"Copy the current selection and store it in the Clipboard, unless empty."

	self lineSelectAndEmptyCheck: [^ self].
	self clipboardTextPut: self selection
]

{ #category : 'nonediting/nontyping keys' }
SimpleEditor >> cursorEnd: aKeyboardEvent [
	"Private - Move cursor end of current line."

	self
		moveCursor: [ :dummy | self endOfText ]
		forward: true
		event: aKeyboardEvent.
	^ true.
]

{ #category : 'nonediting/nontyping keys' }
SimpleEditor >> cursorHome: aKeyboardEvent  [

	"Private - Move cursor from position in current line to beginning of
	current line. If control key is pressed put cursor at beginning of text"

	self
		moveCursor:  [ :dummy | self beginningOfText ]
		forward: false
		event: aKeyboardEvent.
	^true
]

{ #category : 'menu messages' }
SimpleEditor >> cut [
	"Cut out the current selection and redisplay the paragraph if necessary."

	| selection |
	self lineSelectAndEmptyCheck: [^ self].
	selection := self selection.
	self replaceSelectionWith: ''. 
	self clipboardTextPut: selection
]

{ #category : 'menu & kbd shortcuts' }
SimpleEditor >> defaultMenuSpec [
	"Spec for the menu and shortcut keys for the regular text editors.
	See coment at Morph >> #getMenu
	"
	^
	#(
		#(120	'Copy (c)' 	$c 	editor 	copySelection 	copyIcon 	'Copy selection to the Clipboard')
		#(130	'Cut (x)' 		$x 	editor 	cut 				cutIcon 	'Cut selection and store it in the Clipboard')
		#(140	'Paste (v)' 	$v 	editor 	paste 			pasteIcon 	'Paste Clipboard contents')
		"Keyboard shortcut only. No menu entry."
		#(999 	nil 			$a 	editor 	selectAll 		nil 			'Select all')
	).

]

{ #category : 'typing support' }
SimpleEditor >> dispatchOn: aKeyboardEvent [
	"Carry out the action associated with this character, if any."
	| codePoint asciiValue |
	codePoint := aKeyboardEvent keyValue.
	
	codePoint > 255 ifTrue: [
		^self normalCharacter: aKeyboardEvent ].

	"We don't support multiple lines. Therefore, we don't process return as a #normalCharacter:"
	aKeyboardEvent isReturnKey ifTrue: [
		^ true].

	asciiValue := codePoint.
	^ self perform: (self keyboardActions at: asciiValue + 1) with: aKeyboardEvent
]

{ #category : 'typing/selecting keys' }
SimpleEditor >> forwardDelete: aKeyboardEvent [
	"Delete forward over the next character."

	| startIndex stopIndex |
	startIndex := self markIndex.
	startIndex > string size ifTrue: [
		^ false].
	self hasSelection ifTrue: [
		"there was a selection"
		self replaceSelectionWith: ''.
		^ false].
	"Null selection - do the delete forward"
	stopIndex := startIndex.
	(aKeyboardEvent keyValue = 127 and: [ aKeyboardEvent shiftPressed ])
		ifTrue: [ stopIndex := (self nextWordStart: stopIndex) - 1 ].
	self selectFrom: startIndex to: stopIndex.
	self replaceSelectionWith: ''.
	self deselectAndPlaceCursorAt: startIndex.
	^false
]

{ #category : 'accessing-selection' }
SimpleEditor >> hasSelection [
	^markIndex ~= pointIndex
]

{ #category : 'initialization' }
SimpleEditor >> initialize [
	"Initialize the state of the receiver. Subclasses should include 'super 
	initialize' when redefining this message to insure proper initialization."

	super initialize.
	self changeString: UnicodeString new
]

{ #category : 'accessing-selection' }
SimpleEditor >> markIndex [
	^markIndex
]

{ #category : 'accessing-selection' }
SimpleEditor >> markIndex: anIndex [
	markIndex := anIndex min: string size + 1 max: 1
]

{ #category : 'accessing-selection' }
SimpleEditor >> markIndex: anIndex pointIndex: anotherIndex [
	markIndex := anIndex min: string size + 1 max: 1.
	pointIndex := anotherIndex min: string size + 1 max: 1
]

{ #category : 'events' }
SimpleEditor >> mouseDown: evt index: index [

	evt shiftPressed
		ifFalse: [
			self markIndex: index pointIndex: index ]
		ifTrue: [
			self mouseMove: evt index: index ]
]

{ #category : 'events' }
SimpleEditor >> mouseMove: evt index: index [
	"Change the selection in response to mouse-down drag"

	self pointIndex: index
]

{ #category : 'accessing-selection' }
SimpleEditor >> pointIndex [
	^ pointIndex
]

{ #category : 'accessing-selection' }
SimpleEditor >> pointIndex: anIndex [
	pointIndex := anIndex min: string size + 1 max: 1
]

{ #category : 'private' }
SimpleEditor >> privateCurrentString [
	"Answer the string I'm editing. Private. Just for internal Editor use."

	^string
]

{ #category : 'typing support' }
SimpleEditor >> processKeystrokeEvent: aKeyboardEvent [

	"Key struck on the keyboard. Find out which one and, if special, carry 
	out the associated special action. Otherwise, add the character to the 
	stream of characters."

	(self dispatchOn: aKeyboardEvent) ifTrue: [
		^self].
	self markIndex: self pointIndex
]

{ #category : 'accessing' }
SimpleEditor >> replaceSelectionWith: aString [
	"Deselect, and replace the selection text by aText."

	| start stop |
	morph disablesEditing ifTrue: [
		^ self ].

	morph aboutToEdit.
	start := self startIndex.
	stop := self stopIndex.
	(start = stop and: [aString size = 0]) ifFalse: [
		string := string copyReplaceFrom: start to: stop-1 with: aString.	
		self deselectAndPlaceCursorAt:  start + aString size.
		self userHasEdited  " -- note text now dirty" ]
]

{ #category : 'initialization' }
SimpleEditor >> resetState  [
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character, set the emphasis to that of the first character,
	and save the paragraph for purposes of canceling."

	pointIndex := 1.	"Like pointBlock in TextEditor"
	markIndex := 1.	"Like markBlock in TextEditor"
]

{ #category : 'private' }
SimpleEditor >> sameColumn: start newLine: lineBlock forward: isForward [
	"See comment in other implementors."
	^start
]

{ #category : 'new selection' }
SimpleEditor >> selectFrom: start to: stop [

	"Select the specified characters inclusive."
	self selectInvisiblyFrom: start to: stop
]

{ #category : 'new selection' }
SimpleEditor >> selectLine [
	"Make the receiver's selection, if currently empty, encompass the current line."
	self hasSelection ifTrue:[^self].
	self selectAll
]

{ #category : 'accessing-selection' }
SimpleEditor >> selection [

	| firstIndex lastIndex |
	firstIndex := self startIndex.
	lastIndex := self stopIndex - 1.

	(firstIndex = 1 and: [ lastIndex = string size ])
		ifTrue: [ ^string ].

	^string copyFrom: firstIndex to: lastIndex
]

{ #category : 'accessing-selection' }
SimpleEditor >> startIndex [
	^ pointIndex min: markIndex
]

{ #category : 'accessing-selection' }
SimpleEditor >> stopIndex [
	^ pointIndex max: markIndex
]

{ #category : 'accessing' }
SimpleEditor >> string [
	^string
]
