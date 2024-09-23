'From Cuis7.1 [latest update: #6738] on 22 September 2024 at 10:22:27 pm'!

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 9/22/2024 22:18:22'!
explainDelimiter: string
	"Is string enclosed in delimiters?"

	| str |
	(string at: 1) isLetter ifTrue: [^nil].  "only special chars"
	(string first = string last) ifTrue:
			[^ self explainChar: (String with: string first)]
		ifFalse:
			[(string first = $( and: [string last = $)]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = $[ and: [string last = $]]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = ${ and: [string last = $}]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = $< and: [string last = $>]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = $# and: [string last = $)]) ifTrue:
				[^'An instance of class Array.  The Numbers, Characters, or Symbols between the parenthesis are the elements of the Array.'].
			string first = $# ifTrue:
				[^'An instance of class Symbol.'].
			(string first = $$ and: [string size = 2]) ifTrue:
				[^'An instance of class Character.  This one is the character ', (String with: string last), '.'].
			(string first = $:) ifTrue:
				[str := string allButFirst.
				(self explainTemp: str) ifNotNil: [
					^'An argument to this block will be bound to the temporary variable ',
						str, '.']]].
	^ nil! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 9/22/2024 22:15:36'!
fileIn
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimiters. The expressions are read and passed to the 
	Compiler."

	self fileInAnnouncing: 'Reading ' , self name.
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 9/22/2024 22:15:41'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimiters. The expressions are read and passed to the 
	Compiler. Put up a progress report with the given announcement as the title."

	Utilities logsUserChanges: false.

	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :barBlock | self fileInInformingTo: barBlock ].

	"Note:  The main purpose of this banner is to flush the changes file."
	Utilities logsUserChanges: true.
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	! !


!Scanner methodsFor: 'public access' stamp: 'jmv 9/22/2024 22:15:49'!
scanTokens: textOrString 
	"Answer an Array that has been tokenized as though the input text, 
	textOrString, had appeared between the array delimiters #( and ) in a 
	Smalltalk literal expression."

	self initScannerForTokenization.
	self scan: textOrString asPlainString readStream.
	self scanLitVec.
	^token

	"Scanner new scanTokens: 'identifier keyword: 8r31 ''string'' .'"! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 9/22/2024 22:18:55'!
explain
	"Try to shed some light on what kind of entity the current selection is. 
	The selection must be a single token or construct. Insert the answer after 
	the selection. Send private messages whose names begin with 'explain' 
	that return a string if they recognize the selection, else nil."

	| string tiVars cgVars selectors delimiters numbers sorry reply symbol provider |
	sorry := 'Sorry, I can''t explain that.  Please select a single
token, construct, or special character.'.
	(string := self selectedString) isEmpty
		ifTrue: [reply := '']
		ifFalse: [
			string := string withBlanksTrimmed.
			"Remove space, tab, cr"
			"Temps and Instance vars need only test strings that are all letters"
			(string detect: [:char | char isValidInIdentifiers not]
				ifNone: nil) ifNil: [
					tiVars := (self explainTemp: string)
						ifNil: [self explainInst: string]].
					
			provider := self codeProvider.
			(tiVars == nil and: [provider respondsTo: #explainSpecial:])
				ifTrue: [tiVars := provider explainSpecial: string].
			tiVars := tiVars
				ifNil: [ '']
				ifNotNil: [ tiVars , '\' withNewLines].
			"Context, Class, Pool, and Global vars, and Selectors need 
			only test symbols"
			(Symbol hasInterned: string ifTrue: [:s | symbol := s])
				ifTrue: [
					cgVars := (self explainCtxt: symbol) 
						ifNil: [ (self explainClass: symbol)
							ifNil: [ self explainGlobal: symbol]].
					"See if it is a Selector (sent here or not)"
					selectors := (self explainMySel: symbol)
						ifNil: [(self explainPartSel: string)
							ifNil: [ self explainAnySel: symbol]]]
				ifFalse: [selectors := self explainPartSel: string].
			cgVars := cgVars
				ifNil: [ '']
				ifNotNil: [cgVars , '\' withNewLines].
			selectors := selectors
				ifNil: [ '']
				ifNotNil: [ selectors , '\' withNewLines].
			delimiters := string size = 1
				ifTrue: ["single special characters"
					self explainChar: string]
				ifFalse: ["matched delimiters"
					self explainDelimiter: string].
			numbers := self explainNumber: string.
			numbers ifNil: [numbers := ''].
			delimiters ifNil: [delimiters := ''].
			reply := tiVars , cgVars , selectors , delimiters , numbers].
	reply isEmpty ifTrue: [reply := sorry].

	"morph showBalloon: reply.
	self runningWorld ifNotNil: [ :w | w findATranscript ].
	reply print."
	"This could be a preference."
	self afterSelectionInsertAndSelect: reply! !

!methodRemoval: SmalltalkEditor #explainDelimitor: stamp: 'jmv 9/22/2024 22:20:49'!
SmalltalkEditor removeSelector: #explainDelimitor:!
