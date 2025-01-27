'From Cuis7.3 [latest update: #7004] on 23 January 2025 at 4:59:44 pm'!

!TextEditor methodsFor: 'nonediting/nontyping keys' stamp: 'jmv 1/23/2025 16:16:03'!
cursorDown: aKeyboardEvent
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
	^ true! !

!TextEditor methodsFor: 'nonediting/nontyping keys' stamp: 'jmv 1/23/2025 16:16:20'!
cursorPageDown: aKeyboardEvent 

	self 
		moveCursor: [ :position |
			self
				sameColumn: position
				newLine: [ :lineNo | lineNo + self pageHeight]
				forward: true]
		forward: true
		event: aKeyboardEvent.
	^true! !

!TextEditor methodsFor: 'nonediting/nontyping keys' stamp: 'jmv 1/23/2025 16:16:25'!
cursorPageUp: aKeyboardEvent 

	self 
		moveCursor: [ :position |
			self
				sameColumn: position
				newLine: [ :lineNo | lineNo - self pageHeight]
				forward: false]
		forward: false
		event: aKeyboardEvent.
	^true! !

!TextEditor methodsFor: 'nonediting/nontyping keys' stamp: 'jmv 1/23/2025 16:15:52'!
cursorUp: aKeyboardEvent 
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
	^true! !


!Editor class methodsFor: 'class initialization' stamp: 'jmv 1/23/2025 16:59:21'!
initialize
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c | c initializeKeyboardActions ].! !

!Editor class methodsFor: 'class initialization' stamp: 'jmv 1/23/2025 16:19:24'!
initializeKeyboardActions
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
	keyboardActions := actions! !


!TextEditor class methodsFor: 'class initialization' stamp: 'jmv 1/23/2025 16:19:30'!
initializeKeyboardActions
	
	super initializeKeyboardActions.
	keyboardActions at: 9 + 1 put: #tabKey:.
	keyboardActions at: 11 + 1 put: #cursorPageUp:.			"page up key"
	keyboardActions at: 12 + 1 put: #cursorPageDown:.		"page down key"
	keyboardActions at: 30 + 1 put: #cursorUp:.				"up arrow key"
	keyboardActions at: 31 + 1 put: #cursorDown:.			"down arrow key"! !

!methodRemoval: Editor #cursorDown: stamp: 'jmv 1/23/2025 16:58:26'!
Editor removeSelector: #cursorDown:!
!methodRemoval: Editor #cursorPageDown: stamp: 'jmv 1/23/2025 16:58:33'!
Editor removeSelector: #cursorPageDown:!
!methodRemoval: Editor #cursorUp: stamp: 'jmv 1/23/2025 16:58:47'!
Editor removeSelector: #cursorUp:!
!methodRemoval: Editor #cursorPageUp: stamp: 'jmv 1/23/2025 16:58:41'!
Editor removeSelector: #cursorPageUp:!
Editor initialize!
