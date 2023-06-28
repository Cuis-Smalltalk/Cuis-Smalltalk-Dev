'From Cuis 6.0 [latest update: #5880] on 28 June 2023 at 8:01:51 pm'!

!TextModel methodsFor: 'user interface support' stamp: 'jmv 6/28/2023 19:53:02'!
convertAndStyleAllWith: anSHTextStyler
	"Convert and style all contents as a single method or script.
	Do it in a background process if too big."

	| after before |
	before := self actualContents size.
	anSHTextStyler convertAndStyle: self actualContents allowBackgroundStyleProcess: true.
	after := anSHTextStyler convertedAndStyledText size.
	self basicActualContents: anSHTextStyler convertedAndStyledText.
	after = before ifFalse: [
		undoRedoCommands peekLast ifNotNil: [ :lastCommand |
			lastCommand newSizeShouted: after-before]].! !

!TextModel methodsFor: 'user interface support' stamp: 'jmv 6/28/2023 19:35:01'!
convertAndStyleIByParagraphsWith: anSHTextStyler
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
	self changed: #shoutStyled! !

!TextModel methodsFor: 'shout styling' stamp: 'jmv 6/28/2023 19:46:13'!
styleByParagraphs
	"Answer true if each paragraph should be styled independent of the others.
	This is useful in Workspaces and Inspectors, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^false! !


!PluggableTextModel methodsFor: 'shout styling' stamp: 'jmv 6/28/2023 19:46:24'!
styleByParagraphs
	"Answer true if each paragraph should be styled independent of the others.
	This is useful in Workspaces and Inspectors, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^textProvider styleByParagraphs! !


!TextProvider methodsFor: 'shout styling' stamp: 'jmv 6/28/2023 19:44:31'!
styleByParagraphs
	"Answer true if each paragraph should be styled independent of the others.
	False, for instance, in all code Browsers.
	True in Workspaces and Inspectors, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^true! !


!CodeProvider methodsFor: 'shout styling' stamp: 'jmv 6/28/2023 19:45:27'!
styleByParagraphs
	"Answer true if each paragraph should be styled independent of the others.
	False, for instance, in all code Browsers.
	True in Workspaces and Inspectors, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^false! !


!ObjectExplorer methodsFor: 'shout styling' stamp: 'jmv 6/28/2023 19:47:46'!
styleByParagraphs
	"Answer true if each paragraph should be styled independent of the others.
	False, for instance, in all code Browsers.
	True in Workspaces and Inspectors, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^true! !


!TextModel methodsFor: 'user interface support' stamp: 'jmv 6/28/2023 19:38:33'!
convertAndStyleIfNeededWith: anSHTextStyler

	anSHTextStyler ifNotNil: [
		(self shouldStyle: self actualContents with: anSHTextStyler) ifTrue: [
			self styleByParagraphs
				ifTrue: [ self convertAndStyleIByParagraphsWith: anSHTextStyler ]
				ifFalse: [ 	self convertAndStyleAllWith: anSHTextStyler ]	]]! !

!methodRemoval: Workspace #convertAndStyleIfNeededWith: stamp: 'jmv 6/28/2023 19:59:06'!
Workspace removeSelector: #convertAndStyleIfNeededWith:!

!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('variable declarations' mustDeclareVariableWording nameForObject: toggleVariableDeclarationMode)
('gui' openLabel:)
('shout styling' shouldStyle shouldStyle:with: styleByParagraphs toggleStyling toggleStylingLabel)
('initialization' initialize)
('user interface support' allowStylingWithEmphasis autoCompleterClass editorClass fullPrintIt textStylerClass toggleFullPrintIt toggleFullPrintItLabel)
('auto complete' classOfBindingOf: classOfWorkspaceVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('testing' canBindVariables is:)
('accessing' methodNodeOf:ifErrorsParsing:)
!

