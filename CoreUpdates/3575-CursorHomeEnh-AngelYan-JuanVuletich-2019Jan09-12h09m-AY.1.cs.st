'From Cuis 5.0 of 7 November 2016 [latest update: #3574] on 9 January 2019 at 12:29:29 pm'!

!TextEditor methodsFor: 'private' stamp: 'AY 1/2/2019 22:24:14'!
firstOfBeginningOfLineOrEndOfIndentationLeftOf: position
	
	"Returns the first of beginning-of-line or end-of-indentation that appears to the left of the given position, wrapping around to the end of the line (i.e. the line is considered circular).
	This way, if the given position is beginning-of-line then end-of-indentation is returned."
	
	| currentLine beginningOfLine endOfIndentation stops |
	
	currentLine _ textComposition lines at: (textComposition lineIndexFor: position).
	beginningOfLine _ currentLine first.
	endOfIndentation _ self privateCurrentString
		skipDelimiters: (String with: Character tab)
		startingAt: beginningOfLine.
		
	stops _ OrderedCollection with: endOfIndentation with: beginningOfLine.
	^ stops detect: [ :stop | stop < position ] ifNone: [endOfIndentation]! !


!Editor methodsFor: 'private' stamp: 'AY 1/9/2019 12:18:54'!
beginningOfParagraph: position
	| s |
	s _ self privateCurrentString.
	^ (s
		lastIndexOf: Character newLineCharacter
		startingAt: (position-1 min: s size)
		ifAbsent: [ 0 ])
			+ 1.! !


!TextEditor methodsFor: 'nonediting/nontyping keys' stamp: 'AY 1/9/2019 12:29:03'!
cursorEnd: aKeyboardEvent 

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
	^true! !

!TextEditor methodsFor: 'nonediting/nontyping keys' stamp: 'AY 1/9/2019 12:28:01'!
cursorHome: aKeyboardEvent

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
	^true! !

!TextEditor methodsFor: 'private' stamp: 'jmv 1/9/2019 12:27:48'!
sameColumn: start newLine: lineBlock forward: isForward
	"Private - Compute the index in my text
	with the line number derived from lineBlock,"
	" a one argument block accepting the old line number.
	The position inside the line will be preserved as good as possible"
	"The boolean isForward is used in the border case to determine if
	we should move to the beginning or the end of the line."
	| column currentLine offsetAtTargetLine targetEOL lines numberOfLines currentLineNumber targetLineNumber |
	lines _ textComposition lines.
	numberOfLines _ textComposition numberOfLines.
	currentLineNumber  _ textComposition lineIndexFor: start.
	currentLine _ lines at: currentLineNumber.
	column _ start - currentLine first.
	targetLineNumber _ ((lineBlock value: currentLineNumber) max: 1) min: numberOfLines.
	offsetAtTargetLine _ (lines at: targetLineNumber) first.
	targetEOL _ (lines at: targetLineNumber) last + (targetLineNumber = numberOfLines ifTrue:[1]ifFalse:[0]).
	targetLineNumber = currentLineNumber
	"No movement or movement failed. Move to beginning or end of line."
		ifTrue:[
			^isForward
				ifTrue:[targetEOL]
				ifFalse:[offsetAtTargetLine]].
	^offsetAtTargetLine + column min: targetEOL.! !

!methodRemoval: Preferences class #wordStyleCursorMovement!
Preferences class removeSelector: #wordStyleCursorMovement!
