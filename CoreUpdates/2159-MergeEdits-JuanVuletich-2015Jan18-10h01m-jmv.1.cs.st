'From Cuis 4.2 of 25 July 2013 [latest update: #2156] on 18 January 2015 at 10:09:41.910778 am'!
!classDefinition: #TextModel category: #'System-Text'!
ActiveModel subclass: #TextModel
	instanceVariableNames: 'actualContents undoRedoCommands lastEditTimeStamp '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!TextModel methodsFor: 'undoable commands' stamp: 'jmv 1/16/2015 16:02'!
commandForReplaceFrom: start to: stop with: replacement

	^ actualContents commandForReplaceFrom: start to: stop with: replacement! !


!String methodsFor: 'converting' stamp: 'jmv 1/18/2015 10:05'!
withDescriptiveLineEndings
	^ self withLineEndings: '[enter]'! !


!TextReplaceCommand methodsFor: 'updating' stamp: 'jmv 1/18/2015 09:42'!
appendToNew: aStringOrText
	new _ new, aStringOrText! !


!TextModel methodsFor: 'accessing' stamp: 'jmv 1/18/2015 09:34'!
actualContents: aTextOrString
	actualContents _ aTextOrString ifNotNil: [ aTextOrString asText ].
	undoRedoCommands resetToStart.
	lastEditTimeStamp _ nil.
	self changed: #actualContents! !

!TextModel methodsFor: 'accessing' stamp: 'jmv 1/18/2015 09:32'!
basicActualContents: aTextOrString
	"Do not throw events... Not nice... Use with care... Clean some day..."
	actualContents _ aTextOrString ifNotNil: [ aTextOrString asText ].
	undoRedoCommands resetToStart.
	lastEditTimeStamp _ nil! !

!TextModel methodsFor: 'undoable commands' stamp: 'jmv 1/18/2015 09:33'!
logUndoAndAddAttribute: aTextAttribute from: requestedStart to: requestedStop
	"As requested."

	| command |
	command _ actualContents commandForAddAttribute: aTextAttribute from: requestedStart to: requestedStop.
	undoRedoCommands
		nextPut: command;
		truncateAtPosition.	"To disable redo of previous commands, now invalidated."
	command doOn: self.
	lastEditTimeStamp _ nil! !

!TextModel methodsFor: 'undoable commands' stamp: 'jmv 1/18/2015 09:35'!
logUndoAndRemoveAttribute: aTextAttribute from: requestedStart to: requestedStop
	"As requested."

	| command |
	command _ actualContents commandForRemoveAttributes: { aTextAttribute } from: requestedStart to: requestedStop.
	undoRedoCommands
		nextPut: command;
		truncateAtPosition.	"To disable redo of previous commands, now invalidated."
	command doOn: self.
	lastEditTimeStamp _ nil! !

!TextModel methodsFor: 'undoable commands' stamp: 'jmv 1/18/2015 10:04'!
logUndoAndReplaceFrom: start to: stop with: replacement
	"As requested."

	| command now |
	"Time millisecondClockValue rolls over and is generally not adviced.
	But here, we don't care. A user edit doing during rollover would be split  in two, as if the user did a pause.
	Not a problem."
	now _ Time millisecondClockValue.
	command _ self commandForReplaceFrom: start to: stop with: replacement.
	(stop+1 = start and: [ lastEditTimeStamp notNil and: [ now - lastEditTimeStamp < 1000 ]])
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

!TextModel methodsFor: 'undoable commands' stamp: 'jmv 1/18/2015 09:33'!
redoAndEvaluate: aTwoArgBlock
	| modelUpdated newCursorPos |
	modelUpdated _ false.
	undoRedoCommands atEnd ifFalse: [
		newCursorPos _ undoRedoCommands next doOn: self.
		modelUpdated _ true ].
	aTwoArgBlock value: modelUpdated value: newCursorPos.
	lastEditTimeStamp _ nil! !

!TextModel methodsFor: 'undoable commands' stamp: 'jmv 1/18/2015 09:33'!
undoAndEvaluate: aTwoArgBlock
	| modelUpdated newCursorPos |
	modelUpdated _ false.
	undoRedoCommands position > 0 ifTrue: [
		undoRedoCommands skip: -1.
		newCursorPos _ undoRedoCommands peek undoOn: self.
		modelUpdated _ true ].
	aTwoArgBlock value: modelUpdated value: newCursorPos.
	lastEditTimeStamp _ nil! !

!TextModel methodsFor: 'initialization' stamp: 'jmv 1/18/2015 09:33'!
flushUndoRedoCommands

	undoRedoCommands _ ReadWriteStream on: Array new.
	lastEditTimeStamp _ nil! !

!TextModel methodsFor: 'initialization' stamp: 'jmv 1/18/2015 09:34'!
initialize
	"Initialize the state of the receiver with its default contents."

	actualContents _ '' asText.
	undoRedoCommands _ ReadWriteStream on: Array new.
	lastEditTimeStamp _ nil! !


!PluggableTextModel methodsFor: 'initialization' stamp: 'jmv 1/18/2015 09:35'!
initWith: aTextProvider
	"aTextProvider can be a kind of TextProvider, or perhaps a more exotic object, like an ObjectExplorer or a TranscriptStream."

	textProvider _ aTextProvider.
	undoRedoCommands _ ReadWriteStream on: Array new.
	lastEditTimeStamp _ nil! !


!TextReplaceCommand methodsFor: 'printing' stamp: 'jmv 1/18/2015 10:07'!
printOn: aStream
	old isEmpty ifTrue: [
		^aStream nextPutAll: 'typed: '; nextPutAll: new asString withDescriptiveLineEndings surroundedBySingleQuotes].
	new isEmpty ifTrue: [
		^aStream nextPutAll: 'deleted: '; nextPutAll: old asString withDescriptiveLineEndings surroundedBySingleQuotes ].
	aStream
		nextPutAll: 'replaced: ';
		nextPutAll: old asString withDescriptiveLineEndings surroundedBySingleQuotes;
		nextPutAll: ' with: ';
		nextPutAll: new asString withDescriptiveLineEndings surroundedBySingleQuotes ! !

!classDefinition: #TextModel category: #'System-Text'!
ActiveModel subclass: #TextModel
	instanceVariableNames: 'actualContents undoRedoCommands lastEditTimeStamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!
