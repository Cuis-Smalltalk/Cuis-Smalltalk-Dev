'From Cuis 6.0 [latest update: #5121] on 29 April 2022 at 2:45:16 pm'!

!Editor methodsFor: 'private' stamp: 'jmv 4/29/2022 11:43:42'!
stringClassToUse
	^String! !


!TextEditor methodsFor: 'private' stamp: 'jmv 4/29/2022 11:43:56'!
stringClassToUse
	^self privateCurrentString class! !


!Editor methodsFor: 'menu messages' stamp: 'jmv 4/29/2022 11:44:02'!
clipboardStringOrText

	| clipContents |
	clipContents _ Clipboard retrieveStringOrText: self stringClassToUse.
	^ ((clipContents is: #Text) and: [ clipContents respondsTo: #asNonStyledText ])
		ifTrue: [ clipContents asNonStyledText ]
		ifFalse: [ clipContents ]! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 4/29/2022 11:55:16'!
newLine: aKeyboardEvent

	self addString: (self stringClassToUse with: Character newLineCharacter).
	^false! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 4/29/2022 11:48:32'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	self addString: (self stringClassToUse with: aKeyboardEvent keyCharacter).
	^false! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 4/29/2022 12:00:04'!
returnKey: aKeyboardEvent
	"Return / Enter / key was pressed"
	"Process the various Return / Enter keystrokes"
	
	morph acceptOnCR ifTrue: [
		^ true].
	aKeyboardEvent commandAltKeyPressed ifTrue: [
		(aKeyboardEvent controlKeyPressed | aKeyboardEvent rawMacOptionKeyPressed) ifTrue: [
			self addString: self stringClassToUse crString.
			^false ].
		self addString: self stringClassToUse crlfString.
		^false ].
	^ self newLine: aKeyboardEvent! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 4/29/2022 12:00:22'!
enclose: aKeyboardEvent
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| left right startIndex stopIndex oldSelection which |
	startIndex _ self startIndex.
	stopIndex _ self stopIndex.
	oldSelection _ self selection.
	which _ '([<{"''`' indexOf: aKeyboardEvent keyCharacter ifAbsent: [ ^true ].
	left _ '([<{"''`' at: which.
	right _ ')]>}"''`' at: which.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = left and: [(model actualContents at: stopIndex) = right]])
		ifTrue: [
			"already enclosed; strip off brackets"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection]
		ifFalse: [
			"not enclosed; enclose by matching brackets"
			self replaceSelectionWith:
				(Text string: (self stringClassToUse with: left) attributes: emphasisHere),
				oldSelection,
				(Text string: (self stringClassToUse with: right) attributes: emphasisHere).
			self selectFrom: startIndex+1 to: stopIndex].
	^ true! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 4/29/2022 12:00:54'!
inOutdent: aKeyboardEvent delta: delta
	"Add/remove a tab at the front of every line occupied by the selection. 
	Derived from work by Larry Tesler back in December 1985.  Now triggered by Cmd-L and Cmd-R.  2/29/96 sw"
	"This is a user command, and generates undo"

	| realStart realStop lines startLine stopLine start stop adjustStart indentation size numLines inStream newString outStream |

	"Operate on entire lines, but remember the real selection for re-highlighting later"
	realStart _ self startIndex.
	realStop _ self stopIndex - 1.

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

	lines _ textComposition lines.
	startLine _ textComposition lineIndexFor: realStart.
	stopLine _ textComposition lineIndexFor: (realStart max: realStop).
	start _ (lines at: startLine) first.
	stop _ (lines at: stopLine) last.
	
	"Pin the start of highlighting unless the selection starts a line"
	adjustStart _ realStart > start.

	"Find the indentation of the least-indented non-blank line; never outdent more"
	indentation _ (startLine to: stopLine) inject: 1000 into: [ :previousValue :each |
		previousValue min: (textComposition indentationOfLineIndex: each ifBlank: [ :tabs | 1000 ])].			

	size _  stop + 1 - start.
	numLines _ stopLine + 1 - startLine.
	inStream _ ReadStream on: self privateCurrentString from: start to: stop.

	newString _ self stringClassToUse new: size + ((numLines * delta) max: 0).
	outStream _ ReadWriteStream on: newString.

	"This subroutine does the actual work"
	self indent: delta fromStream: inStream toStream: outStream.

	"Adjust the range that will be highlighted later"
	adjustStart ifTrue: [realStart _ (realStart + delta) max: start].
	realStop _ realStop + outStream position - size.

	"Prepare for another iteration"
	indentation _ indentation + delta.
	size _ outStream position.
	inStream _ outStream setFrom: 1 to: size.

	outStream
		ifNil: [ 	"tried to outdent but some line(s) were already left flush"
			morph flash]
		ifNotNil: [
			self selectInvisiblyFrom: start to: stop.
			size = newString size ifFalse: [newString _ outStream contents].
			self replaceSelectionWith: newString].
	self selectFrom: realStart to: realStop. 	"highlight only the original range"
	^ true! !

!TextEditor methodsFor: 'typing support' stamp: 'jmv 4/28/2022 17:10:31'!
setEmphasisHereFromTextForward: f

	| i forward delta prevIsLineSeparator nextIsLineSeparator prevIsSeparator nextIsSeparator nextChar prevChar |
	i _ self pointIndex.
	"Try to set emphasisHere correctly after whitespace.
	Most important after a cr, i.e. at the start of a new line"
	prevIsLineSeparator _  i > 1 and: [ (prevChar _ model actualContents at: i-1) isLineSeparator ].
	nextIsLineSeparator _ i <= model textSize and: [ (nextChar _ model actualContents at: i) isLineSeparator ].
	prevIsSeparator _  i > 1 and: [ prevChar isSeparator ].
	nextIsSeparator _ i <= model textSize and: [ nextChar isSeparator ].
	prevIsLineSeparator & nextIsLineSeparator
		ifTrue: [
			"Empty paragraph: take emphasis from the newLine character"
			forward _ false ]
		ifFalse: [
			prevIsSeparator == nextIsSeparator
				ifTrue: [
					"Space at both sides, or non-space at both sides, take emphasis used where the cursor comes from"
					forward _ f ]
				ifFalse: [
					"Space at one side and non-space at the other, take emphasis from the non-space character"
					forward _ nextIsSeparator ]].
	delta _ forward ifTrue: [ 1 ] ifFalse: [ 0 ].
	emphasisHere _ (model actualContents attributesAt: (i - delta max: 1))
					select: [:att | att mayBeExtended].
	morph possiblyChanged! !

!TextEditor methodsFor: 'private' stamp: 'jmv 4/29/2022 12:00:32'!
firstOfBeginningOfLineOrEndOfIndentationLeftOf: position
	
	"Returns the first of beginning-of-line or end-of-indentation that appears to the left of the given position, wrapping around to the end of the line (i.e. the line is considered circular).
	This way, if the given position is beginning-of-line then end-of-indentation is returned."
	
	| currentLine beginningOfLine endOfIndentation stops |
	
	currentLine _ textComposition lines at: (textComposition lineIndexFor: position).
	beginningOfLine _ currentLine first.
	endOfIndentation _ self privateCurrentString
		skipDelimiters: (self stringClassToUse with: Character tab)
		startingAt: beginningOfLine.
		
	stops _ OrderedCollection with: endOfIndentation with: beginningOfLine.
	^ stops detect: [ :stop | stop < position ] ifNone: [endOfIndentation]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/29/2022 12:01:03'!
doItSourceCodeFor: aSourceCode in: anEvaluationContext

	^self stringClassToUse streamContents: [ :stream |
		Scanner doItSourceCodeHeaderWithContext: anEvaluationContext notNil into: stream.
		stream nextPutAll: aSourceCode ]! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 4/29/2022 11:48:00'!
newLine: aKeyboardEvent
	"Replace the current text selection with a newLine (i.e. LF) followed by as many tabs
	as there are leading tabs on the current line (+/- bracket count)."

	| char s i tabCount stopIndex newLineString |
	s _ self privateCurrentString.
	stopIndex _ self stopIndex.
	i _ stopIndex.
	tabCount _ 0.
	[ (i _ i-1) > 0 and: [ (char _ s at: i) isLineSeparator not ] ] whileTrue: [
		"Count brackets"
		char = $[ ifTrue: [tabCount _ tabCount + 1].
		char = $] ifTrue: [tabCount _ tabCount - 1]].
	[ (i _ i + 1) < stopIndex and: [ (char _ s at: i) isSeparator ] ] whileTrue: [
		"Count leading tabs"
		char = Character tab ifTrue: [ tabCount _ tabCount + 1 ]].
	"Now inject newline with tabCount tabs, generating a new undoable command"
	newLineString _ self stringClassToUse streamContents: [ :strm | strm newLineTab: tabCount ].
	model startNewUndoRedoCommand.
	self replaceSelectionWith: newLineString.
	^ false! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 4/29/2022 12:01:36'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| stopIndex startIndex string key |
	aKeyboardEvent keyCharacter isLetter ifTrue: [^ super normalCharacter: aKeyboardEvent].
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	string _ self privateCurrentString.
	stopIndex _ self pointIndex - 1.
	(stopIndex between: 2 and: string size)
		ifFalse: [^ super normalCharacter: aKeyboardEvent].
	startIndex _ stopIndex.
	"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
	[startIndex > 0 and: [stopIndex - startIndex < 7 and: [(string at: startIndex) isLetter]]] whileTrue: [startIndex _ startIndex - 1].
	(startIndex > 0 and: [(string at: startIndex) = $\])
		ifFalse: [^ super normalCharacter: aKeyboardEvent].
	key _ string copyFrom: startIndex+1 to: stopIndex.
	(Character namedCharactersTable detect: [:one| key = one first] ifNone: [])
		ifNotNil: [:aPair| self selectFrom: startIndex to: stopIndex; replaceSelectionWith: (self stringClassToUse with: aPair second)].
	^ super normalCharacter: aKeyboardEvent! !

