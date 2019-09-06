'From Cuis 5.0 [latest update: #3858] on 5 September 2019 at 11:39:06 am'!

!TextModel methodsFor: 'undoable commands' stamp: 'jmv 9/5/2019 11:35:32'!
startNewUndoRedoCommand

	lastEditTimeStamp _ nil! !


!TextModel methodsFor: 'undoable commands' stamp: 'jmv 9/5/2019 11:32:02'!
logUndoAndReplaceFrom: start to: stop with: replacement
	"As requested."

	| command now |
	"Time millisecondClockValue rolls over and is generally not adviced.
	But here, we don't care. A user edit doing during rollover would be split  in two, as if the user did a pause.
	Not a problem."
	
	now _ Time millisecondClockValue.
	command _ self commandForReplaceFrom: start to: stop with: replacement.
	(stop+1 = start and: [ lastEditTimeStamp notNil and: [ now - lastEditTimeStamp < 1000 and: [start = undoRedoCommands last stopPosition] ]])
		ifTrue: [
			"Don't use the command we just built"
			undoRedoCommands last appendToNew: replacement
			]
		ifFalse: [
			undoRedoCommands
				nextPut: command;
				truncateAtPosition.	"To disable redo of previous commands, now invalidated."
			].
	command doOn: self.
	lastEditTimeStamp _ now! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 9/5/2019 11:33:21'!
replaceSelectionWith: aTextOrString
	"Deselect, and replace the selection text by aText."
	"This is a user command, and generates undo"

	| start stop replacement |

	morph disablesEditing ifTrue: [
		^ self ].

	start _ self startIndex.
	stop _ self stopIndex.
	(aTextOrString isEmpty and: [stop > start]) ifTrue: [
		"If deleting, then set emphasisHere from 1st character of the deletion"
		emphasisHere _ (model actualContents attributesAt: start) select: [:att |
			att mayBeExtended]].

	(start = stop and: [ aTextOrString isEmpty ]) ifFalse: [
		replacement _ self addAttributesForPasting: aTextOrString.
		model logUndoAndReplaceFrom: start to: stop - 1 with: replacement.
		textComposition
			recomposeFrom: start
			to:  start + replacement size - 1
			delta: replacement size - (stop-start).
		self deselectAndPlaceCursorAt: start + replacement size.
		selectionStartBlocks _ #().
		selectionStopBlocks _ #().
		self userHasEdited  " -- note text now dirty" ].

	morph possiblyChanged! !

!TextEditor methodsFor: 'new selection' stamp: 'jmv 9/5/2019 11:37:06'!
insertAndSelect: aString at: anInteger
	"This is a user command, and generates undo"

	| newText |
	newText _ (aString is: #Text) ifTrue: [aString] ifFalse: [Text string: aString attributes: emphasisHere].
	self deselectAndPlaceCursorAt: anInteger.
	self replaceSelectionWith: newText.
	self selectFrom: anInteger to: anInteger + newText size - 1! !


!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 9/5/2019 11:36:14'!
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
	newLineString _ String streamContents: [ :strm | strm newLineTab: tabCount ].
	model startNewUndoRedoCommand.
	self replaceSelectionWith: newLineString.
	^ false! !

!SmalltalkEditor methodsFor: 'new selection' stamp: 'jmv 9/5/2019 11:37:26'!
notify: aString at: anInteger in: aStream 
	"The compilation of text failed. The syntax error is noted as the argument, 
	aString. Insert it in the text at starting character position anInteger."
	"This is a user command, and generates undo"
	model startNewUndoRedoCommand.
	self insertAndSelect: aString at: (anInteger max: 1).! !

!methodRemoval: TextEditor #insertAndSelect:at:shouldMergeCommandsIfPossible:!
TextEditor removeSelector: #insertAndSelect:at:shouldMergeCommandsIfPossible:!
!methodRemoval: TextEditor #replaceSelectionWith:shouldMergeCommandsIfPossible:!
TextEditor removeSelector: #replaceSelectionWith:shouldMergeCommandsIfPossible:!
!methodRemoval: TextModel #logUndoAndReplaceFrom:to:with:shouldMergeCommandsIfPossible:!
TextModel removeSelector: #logUndoAndReplaceFrom:to:with:shouldMergeCommandsIfPossible:!
