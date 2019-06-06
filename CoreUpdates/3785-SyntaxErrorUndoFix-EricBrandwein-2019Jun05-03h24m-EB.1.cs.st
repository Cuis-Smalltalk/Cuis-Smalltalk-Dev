'From Cuis 5.0 of 7 November 2016 [latest update: #3784] on 5 June 2019 at 2:42:46 pm'!

!TextReplaceCommand methodsFor: 'as yet unclassified' stamp: 'EB 6/5/2019 14:41:58'!
stopPosition
	^position + new size.! !


!TextModel methodsFor: 'undoable commands' stamp: 'EB 6/5/2019 14:41:58'!
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


!TextModel reorganize!
('accessing' actualContents actualContents: basicActualContents: contents: textSize undoRedoCommands undoRedoCommandsPosition)
('misc' getSelection refetch)
('testing' canBindVariables is: isTextEmpty wantsFrameAdornments)
('commands' acceptContentsFrom:)
('shout support' privateAddBoldAt: privateRemoveBoldAt:)
('undoable commands' basicReplaceAttributesFrom:to:with: basicReplaceFrom:to:with: commandForReplaceFrom:to:with: logUndoAndAddAttribute:from:to: logUndoAndRemoveAttributes:from:to: logUndoAndReplaceFrom:to:with: redoAndEvaluate: undoAndEvaluate:)
('object serialization' convertToCurrentVersion:refStream:)
('initialization' flushUndoRedoCommands initialize openLabel:)
('copying' postCopy)
('user interface support' autoCompleterClass editorClass formatAndStyleIfNeededWith: textStylerClass)
('shout styling' shouldStyle:with:)
('auto complete' classOfThisContext classOfWorkspaceVarNamed: computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('as yet unclassified' saveOn:as:)
!

