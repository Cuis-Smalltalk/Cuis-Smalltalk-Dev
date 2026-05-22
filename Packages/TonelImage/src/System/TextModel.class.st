"
I am a kind of Model that includes a piece of text.

Category 'pane menu' and 'message list menu' are messages that may be called by my menus.

See also CodeProvider.
"
Class {
	#name : 'TextModel',
	#superclass : 'ActiveModel',
	#instVars : [
		'actualContents',
		'undoRedoCommands',
		'lastEditTimeStamp'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'instance creation' }
TextModel class >> openLabel: aString [

	^self new openLabel: aString
]

{ #category : 'window color' }
TextModel class >> windowColor [
	^ Theme current textEditor
]

{ #category : 'instance creation' }
TextModel class >> withText: aTextOrString [
	^self new basicActualContents: aTextOrString
]

{ #category : 'shout styling' }
TextModel >> aboutToStyleWith: anSHTextStyler [
	"This is a notification that anSHTextStyler is about to re-style its text."
]

{ #category : 'commands' }
TextModel >> acceptContentsFrom: aTextModelMorph [
	"Tell the owning window to save the contents if it supports that action.
	
	Also clear the user edits."
	
	aTextModelMorph owningWindow ifNotNil: [ :ow |
		ow hasSaveAs ifTrue: [ 
			ow forceSaveContents.
			self changed: #clearUserEdits ] ]
]

{ #category : 'accessing' }
TextModel >> actualContents [
	^actualContents
]

{ #category : 'accessing' }
TextModel >> actualContents: aTextOrString [
	self basicActualContents: aTextOrString.
	self changed: #actualContents
]

{ #category : 'user interface support' }
TextModel >> allowStylingWithEmphasis [
	"Default for Smalltalk methods"

	^true
]

{ #category : 'user interface support' }
TextModel >> autoCompleterClass [
	^nil
]

{ #category : 'accessing' }
TextModel >> basicActualContents: aTextOrString [
	"Do not throw events."
	| prevContents |
	prevContents := actualContents.
	actualContents := aTextOrString asText asUnicodeStringOrText.
	actualContents = prevContents ifFalse: [	"Compares only characters, not attributes"
		undoRedoCommands resetToStart.
		lastEditTimeStamp := nil ]
]

{ #category : 'undoable commands' }
TextModel >> basicReplaceAttributesFrom: start to: stop with: replacement [
	"As requested. Basic service used by Undo / Redo. Does not genertate undo."

	actualContents basicReplaceAttributesFrom: start to: stop with: replacement
]

{ #category : 'undoable commands' }
TextModel >> basicReplaceFrom: start to: stop with: replacement [
	"As requested. Basic service used by Undo / Redo. Does not genertate undo."

	actualContents replaceFrom: start to: stop with: replacement
]

{ #category : 'testing' }
TextModel >> canBindVariables [
	^ false
]

{ #category : 'auto complete' }
TextModel >> classOfThisContext [

	^ MethodContext 
]

{ #category : 'auto complete' }
TextModel >> classOfWorkspaceVarNamed: aName [

	^ nil
]

{ #category : 'undoable commands' }
TextModel >> commandForReplaceFrom: start to: stop with: replacement [

	^ actualContents commandForReplaceFrom: start to: stop with: replacement
]

{ #category : 'auto complete' }
TextModel >> computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'auto complete' }
TextModel >> computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'auto complete' }
TextModel >> computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'auto complete' }
TextModel >> computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName   [

	anAutocompleter computeMessageEntriesForUnknowClass
]

{ #category : 'accessing' }
TextModel >> contents: aTextOrString [
	"Does not update any view...
	The kind of stuff that needs to be cleaned some day..."
	self basicActualContents: aTextOrString
]

{ #category : 'accessing' }
TextModel >> contentsForFileSaving [
	^self actualContents asPlainString
]

{ #category : 'user interface support' }
TextModel >> convertAndStyleAllWith: anSHTextStyler [
	"Convert and style all contents as a single method or script.
	Do it in a background process if too big."

	anSHTextStyler convertAndStyle: self actualContents allowBackgroundStyleProcess: true.
]

{ #category : 'user interface support' }
TextModel >> convertAndStyleIByParagraphsWith: anSHTextStyler [
	"Convert and style. Treat each paragraph (separated by an empty line) as a separate script.
	Useful for Workspaces, Inspectors, etc."
	| separator fragmentStart fragmentEnd done|

	actualContents := actualContents optimizedForMutationSpeed.	
	anSHTextStyler getReady.
	anSHTextStyler workspaceContents: actualContents.
	separator := String newLineString, String newLineString.
	fragmentStart := 1.
	done := false.
	[done] whileFalse: [
		fragmentEnd := actualContents
			indexOfSubCollection: separator
			startingAt: fragmentStart
			ifAbsent: [done := true. actualContents size].
		anSHTextStyler parseWorkspaceFrom: fragmentStart to: fragmentEnd.
		anSHTextStyler privateConvertAssignmentsOffset: fragmentStart-1.
		anSHTextStyler styleWorkspaceOffset: fragmentStart-1 allowEmphasis: self allowStylingWithEmphasis.
		fragmentStart := fragmentEnd+separator size ].
	self changed: #shoutStyled
]

{ #category : 'user interface support' }
TextModel >> convertAndStyleIfNeededWith: anSHTextStyler [

	self aboutToStyleWith: anSHTextStyler.
	self styleByParagraphs
		ifTrue: [ self convertAndStyleIByParagraphsWith: anSHTextStyler ]
		ifFalse: [ self convertAndStyleAllWith: anSHTextStyler ].
]

{ #category : 'object serialization' }
TextModel >> convertToCurrentVersion: varDict refStream: smartRefStrm [

	"Maybe old instances won't have this variable set."
	undoRedoCommands ifNil: [
		undoRedoCommands := ReadWriteStream on: #() ]
]

{ #category : 'accessing' }
TextModel >> currentCompiledMethod [
	"Answer the CompiledMethod installed in the system forthe currently selected message.
	Answer nil if no such concept exists."
	^ nil
]

{ #category : 'accessing' }
TextModel >> doItContext [
	^nil
]

{ #category : 'accessing' }
TextModel >> doItReceiver [
	^nil
]

{ #category : 'user interface support' }
TextModel >> editorClass [
	^TextEditor
]

{ #category : 'initialization' }
TextModel >> flushUndoRedoCommands [

	undoRedoCommands := ReadWriteStream on: #().
	lastEditTimeStamp := nil
]

{ #category : 'user interface support' }
TextModel >> fullPrintIt [
	^true
]

{ #category : 'misc' }
TextModel >> getSelection [
	"Answer the model's selection interval."

	^ nil	"null selection"
]

{ #category : 'initialization' }
TextModel >> initialize [
	"Initialize the state of the receiver with its default contents."

	undoRedoCommands := ReadWriteStream on: #().
	self basicActualContents: ''.
]

{ #category : 'testing' }
TextModel >> is: aSymbol [
	^ aSymbol == #canSaveContents or: [ super is: aSymbol ]
]

{ #category : 'testing' }
TextModel >> isDirty [

	^undoRedoCommands position isZero not.
]

{ #category : 'testing' }
TextModel >> isTextEmpty [
	^actualContents isEmpty
]

{ #category : 'undoable commands' }
TextModel >> logUndoAndAddAttribute: aTextAttribute from: requestedStart to: requestedStop [
	"As requested."

	| command |
	command := actualContents commandForAddAttribute: aTextAttribute from: requestedStart to: requestedStop.
	undoRedoCommands
		nextPut: command;
		truncateAtPosition.	"To disable redo of previous commands, now invalidated."
	command doOn: self.
	lastEditTimeStamp := nil
]

{ #category : 'undoable commands' }
TextModel >> logUndoAndRemoveAttributes: textAttributes from: requestedStart to: requestedStop [
	"As requested."

	| command |
	command := actualContents commandForRemoveAttributes: textAttributes from: requestedStart to: requestedStop.
	undoRedoCommands
		nextPut: command;
		truncateAtPosition.	"To disable redo of previous commands, now invalidated."
	command doOn: self.
	lastEditTimeStamp := nil
]

{ #category : 'undoable commands' }
TextModel >> logUndoAndReplaceFrom: start to: stop with: replacement [
	"As requested."

	| command now |
	"Time millisecondClockValue rolls over and is generally not adviced.
	But here, we don't care. A user edit doing during rollover would be split  in two, as if the user did a pause.
	Not a problem."
	
	now := Time millisecondClockValue.
	command := self commandForReplaceFrom: start to: stop with: replacement.
	(stop+1 = start and: [ lastEditTimeStamp notNil and: [ now - lastEditTimeStamp < 1000 and: [start = undoRedoCommands peekLast stopPosition] ]])
		ifTrue: [
			"Don't use the command we just built"
			undoRedoCommands peekLast appendToNew: replacement
			]
		ifFalse: [
			undoRedoCommands
				nextPut: command;
				truncateAtPosition.	"To disable redo of previous commands, now invalidated."
			].
	command doOn: self.
	lastEditTimeStamp := now
]

{ #category : 'initialization' }
TextModel >> openLabel: aString  [
	"Create a standard system view of the model, me, and open it."
	^SystemWindow editText: self label: aString wrap: true
]

{ #category : 'copying' }
TextModel >> postCopy [
	super postCopy.
	actualContents := actualContents copy.
	undoRedoCommands := ReadWriteStream on: #().
	lastEditTimeStamp := nil
]

{ #category : 'shout support' }
TextModel >> privateAddBoldAt: idx [
	"Just for shout. No undo."

	actualContents 
		addAttribute: ShoutTextEmphasis bold
		from: idx
		to: idx
]

{ #category : 'shout support' }
TextModel >> privateRemoveBoldAt: idx [
	"Just for shout. No undo."

	actualContents 
		removeAttributes: { ShoutTextEmphasis bold }
		from: idx
		to: idx
]

{ #category : 'undoable commands' }
TextModel >> redoAndEvaluate: aTwoArgBlock [
	| modelUpdated newCursorPos |
	modelUpdated := false.
	undoRedoCommands atEnd ifFalse: [
		newCursorPos := undoRedoCommands next doOn: self.
		modelUpdated := true ].
	aTwoArgBlock value: modelUpdated value: newCursorPos.
	lastEditTimeStamp := nil
]

{ #category : 'misc' }
TextModel >> refetch [
	"Nothing here. Answer true if actualContents was actually fetched."
	self actualContents: self actualContents.
	self changed: #refetched.
]

{ #category : 'accessing' }
TextModel >> selectedClass [
	"Answer the class that is currently selected.
	Answer nil if no concept of selectedClass exists."
	^ nil
]

{ #category : 'accessing' }
TextModel >> selectedClassOrMetaClass [
	"Answer the selected class or metaclass.
	Answer nil if no concept of selectedClassOrMetaClass exists."
	^ nil
]

{ #category : 'accessing' }
TextModel >> selectedMessageName [
	"Answer the message selector of the currently selected message.
	Answer nil if no concept of selectedMessageName exists."
	^ nil
]

{ #category : 'user interface support' }
TextModel >> shouldAutoHighlight [
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^false
]

{ #category : 'undoable commands' }
TextModel >> startNewUndoRedoCommand [

	lastEditTimeStamp := nil
]

{ #category : 'shout styling' }
TextModel >> styleByParagraphs [
	"Answer true if each paragraph should be styled independent of the others.
	This is useful in Workspaces and Inspectors, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^false
]

{ #category : 'shout styling' }
TextModel >> stylesText [
	"Answer true to allow styling to proceed, or false to veto the styling"

	^(Preferences at: #syntaxHighlightingAsYouType)
		and: [ self textStylerClass notNil ].
]

{ #category : 'accessing' }
TextModel >> textSize [
	actualContents ifNil: [ ^0 ].
	^actualContents size
]

{ #category : 'user interface support' }
TextModel >> textStylerClass [

	^nil
]

{ #category : 'undoable commands' }
TextModel >> undoAndEvaluate: aTwoArgBlock [
	| modelUpdated newCursorPos |
	modelUpdated := false.
	undoRedoCommands position > 0 ifTrue: [
		undoRedoCommands skipBack.
		newCursorPos := undoRedoCommands peek undoOn: self.
		modelUpdated := true ].
	aTwoArgBlock value: modelUpdated value: newCursorPos.
	lastEditTimeStamp := nil
]

{ #category : 'accessing' }
TextModel >> undoRedoCommands [
	^undoRedoCommands contents
]

{ #category : 'accessing' }
TextModel >> undoRedoCommandsPosition [
	^undoRedoCommands position
]

{ #category : 'testing' }
TextModel >> wantsFrameAdornments [
	^false
]
