'From Cuis 5.0 of 7 November 2016 [latest update: #3786] on 7 June 2019 at 5:12:13 pm'!

!TextModel methodsFor: 'undoable commands' stamp: 'EB 6/7/2019 17:02:49'!
logUndoAndReplaceFrom: start to: stop with: replacement shouldMergeCommandsIfPossible: shouldMergeCommands
	"As requested."

	| command now |
	"Time millisecondClockValue rolls over and is generally not adviced.
	But here, we don't care. A user edit doing during rollover would be split  in two, as if the user did a pause.
	Not a problem."
	
	now _ Time millisecondClockValue.
	command _ self commandForReplaceFrom: start to: stop with: replacement.
	(shouldMergeCommands and: [
		stop+1 = start and: [ lastEditTimeStamp notNil and: [ now - lastEditTimeStamp < 1000 and: [start = undoRedoCommands last stopPosition] ]]])
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


!TextEditor methodsFor: 'accessing' stamp: 'EB 6/7/2019 17:04:15'!
replaceSelectionWith: aTextOrString shouldMergeCommandsIfPossible: shouldMergeCommands
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
		model logUndoAndReplaceFrom: start to: stop - 1 with: replacement shouldMergeCommandsIfPossible: shouldMergeCommands.
		textComposition
			recomposeFrom: start
			to:  start + replacement size - 1
			delta: replacement size - (stop-start).
		self deselectAndPlaceCursorAt: start + replacement size.
		selectionStartBlocks _ #().
		selectionStopBlocks _ #().
		self userHasEdited  " -- note text now dirty" ].

	morph possiblyChanged! !

!TextEditor methodsFor: 'new selection' stamp: 'EB 6/7/2019 16:55:51'!
insertAndSelect: aString at: anInteger shouldMergeCommandsIfPossible: shouldMergeCommands
	"This is a user command, and generates undo"

	| newText |
	newText _ (aString is: #Text) ifTrue: [aString] ifFalse: [Text string: aString attributes: emphasisHere].
	self deselectAndPlaceCursorAt: anInteger.
	self replaceSelectionWith: newText shouldMergeCommandsIfPossible: shouldMergeCommands.
	self selectFrom: anInteger to: anInteger + newText size - 1! !


!TextModel methodsFor: 'undoable commands' stamp: 'EB 6/7/2019 17:03:26'!
logUndoAndReplaceFrom: start to: stop with: replacement
	"As requested."
	self logUndoAndReplaceFrom: start to: stop with: replacement shouldMergeCommandsIfPossible: true.! !


!TextEditor methodsFor: 'accessing' stamp: 'EB 6/7/2019 17:03:59'!
replaceSelectionWith: aTextOrString
	"Deselect, and replace the selection text by aText."
	"This is a user command, and generates undo"
	self replaceSelectionWith: aTextOrString shouldMergeCommandsIfPossible: true.! !

!TextEditor methodsFor: 'new selection' stamp: 'EB 6/7/2019 17:05:33'!
insertAndSelect: aString at: anInteger
	"This is a user command, and generates undo"
	self insertAndSelect: aString at: anInteger shouldMergeCommandsIfPossible: true.! !


!SmalltalkEditor methodsFor: 'new selection' stamp: 'EB 6/7/2019 17:07:30'!
notify: aString at: anInteger in: aStream 
	"The compilation of text failed. The syntax error is noted as the argument, 
	aString. Insert it in the text at starting character position anInteger."
	"This is a user command, and generates undo"
	self insertAndSelect: aString at: (anInteger max: 1) shouldMergeCommandsIfPossible: false.! !

