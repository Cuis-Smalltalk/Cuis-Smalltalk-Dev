'From Cuis 4.2 of 25 July 2013 [latest update: #2377] on 21 June 2015 at 12:52:14.024827 pm'!

!TextModel methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 11:58'!
autoCompleterClass
	^nil! !

!TextModel methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 11:57'!
editorClass
	^TextEditor! !

!TextModel methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 11:58'!
textStylerClass
	^nil! !


!PluggableTextModel methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:04'!
autoCompleterClass
	^textProvider autoCompleterClass! !

!PluggableTextModel methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:04'!
textStylerClass
	^textProvider textStylerClass! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:08'!
autoCompleterClass
	^SmalltalkCompleter! !

!Workspace methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:07'!
textStylerClass
	^SHTextStylerST80! !


!TextProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:02'!
autoCompleterClass
	^nil! !

!TextProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:09'!
editorClass
	^TextEditor! !

!TextProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:05'!
textStylerClass
	^nil! !


!CodeProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:46'!
textStylerClass
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass textStylerClass ].
	^SHTextStylerST80! !


!Compiler class methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:18'!
autoCompleterClass
	^SmalltalkCompleter! !

!Compiler class methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:18'!
editorClass
	^SmalltalkEditor! !

!Compiler class methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:18'!
textStylerClass
	^SHTextStylerST80! !


!RemoteString methodsFor: 'accessing' stamp: 'jmv 6/21/2015 12:42'!
compilerClass
	"Answer a compiler class appropriate for source methods of this class."

	^Compiler! !


!Object methodsFor: 'user interface' stamp: 'jmv 6/21/2015 12:00'!
autoCompleterClass
"?????"
'' print.
thisContext printStack: 10.
'' print.
	^nil! !

!Object methodsFor: 'user interface' stamp: 'jmv 6/21/2015 12:12'!
editorClass
"?????
Se usa, por ejemplo en el FillInTheBlankMorph..."
'' print.
thisContext printStack: 10.
'' print.
	^TextEditor! !


!PluggableTextModel methodsFor: 'testing' stamp: 'jmv 6/21/2015 12:37'!
is: aSymbol
	^ aSymbol == #hasTextProvider or: [ super is: aSymbol ]! !

!PluggableTextModel methodsFor: 'user interface support' stamp: 'jmv 12/4/2010 00:31'!
editorClass
	^textProvider editorClass! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:08'!
editorClass
	^SmalltalkEditor! !


!CodeProvider methodsFor: 'testing' stamp: 'jmv 6/21/2015 12:38'!
is: aSymbol
	^ aSymbol == #CodeProvider or: [ super is: aSymbol ]! !

!CodeProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:47'!
autoCompleterClass
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass autoCompleterClass ].
	^SmalltalkCompleter! !

!CodeProvider methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 12:46'!
editorClass
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass editorClass ].
	^SmalltalkEditor! !


!TextModelMorph methodsFor: 'updating' stamp: 'jmv 6/21/2015 11:53'!
update: aSymbol
	super update: aSymbol.
	aSymbol ifNil: [^self].
	aSymbol == #flash ifTrue: [^self flash].
	aSymbol == #actualContents ifTrue: [
		"Some day, it would be nice to keep objects and update them
		instead of throwing them away all the time for no good reason..."
		self textMorph
			releaseEditorAndTextComposition;
			installEditorAndTextComposition;
			formatAndStyleIfNeeded.
		self setScrollDeltas.
		self redrawNeeded.
		^self ].
	aSymbol == #acceptedContents ifTrue: [
		self textMorph hasUnacceptedEdits ifTrue: [
			self textMorph hasEditingConflicts: true.
			^self redrawNeeded ].
		model refetch.
		"#actualContents also signalled in #refetch. No need to repeat what's done there."
		^self ].
	aSymbol == #refetched ifTrue: [
		"#actualContents also signalled when #refetched is signalled.
		No need to repeat what's done there."
		self setSelection: model getSelection.
		self hasUnacceptedEdits: false.
		^self ].
	aSymbol == #initialSelection ifTrue: [
		^self setSelection: model getSelection; redrawNeeded ].
	aSymbol == #autoSelect ifTrue: [
		TextEditor abandonChangeText.	"no replacement!!"
		self editor
			setSearch: model autoSelectString;
			findAndReplaceMany: true.
		self textMorph updateFromTextComposition.
		^self scrollSelectionIntoView ].
	"Quite ugly"
	aSymbol == #clearUserEdits ifTrue: [
		^self hasUnacceptedEdits: false].
	aSymbol == #shoutStyle ifTrue: [
		self textMorph stylerStyled.
		^self redrawNeeded ]! !


!TextModelMorph class methodsFor: 'instance creation' stamp: 'jmv 6/21/2015 12:34'!
textProvider: aTextProvider textGetter: textGetter textSetter: textSetter selectionGetter: selectionGetter allowStyler: aBoolean
"
El sllowStyler: esta al pedo. Debe ser en installEditorAndTextComposition que se evalua si hay styler o no....
"
	| newModel answer |
	answer _ self new.

	newModel _ PluggableTextModel on: aTextProvider.
	newModel textGetter: textGetter textSetter: textSetter selectionGetter: selectionGetter.
	aTextProvider addDependent: newModel.
	answer model: newModel.
	^answer! !

!TextModelMorph class methodsFor: 'instance creation' stamp: 'jmv 6/21/2015 12:32'!
withModel: aTextModel

	|  answer |
	answer _ self new.
	answer model: aTextModel.
	^answer! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 6/21/2015 12:50'!
installEditorAndTextComposition
	"Install an editor for my textComposition. Install also the textComposition."
	| e tc |
	
	"Editor and TextComposition are assigned here atomically."
	e _ model editorClass new morph: self.
	e model: model.
	tc _ TextComposition new.
	"Keep critical section short"
	self mutex critical: [
		editor _ e.
		textComposition _ tc ].
	tc
		setModel: model;
		extentForComposing: self extentForComposing.
	e textComposition: tc.
	tc editor: e.
	e setEmphasisHereFromText.
	tc composeAll.
	e resetState.
	self fit.
	self selectionChanged.

	"Add extras. Text Styler and Autocompleter"
	"'REVISAR' print."	"solo si distinto del anterior? o siempre?"
	self styler:
		(Preferences syntaxHighlightingAsYouType ifTrue: [
			model textStylerClass ifNotNil: [ :stylerClass |
				stylerClass new ]]).
	self autoCompleter:
		( model autoCompleterClass ifNotNil: [ :completerClass |
			completerClass withModel: model ])! !


!Compiler class reorganize!
('accessing' couldEvaluate: decompilerClass new parserClass)
('evaluating' evaluate: evaluate:for:logged: evaluate:for:notifying:logged: evaluate:logged: evaluate:notifying:logged: format:in:notifying: format:in:notifying:decorated:)
('class initialization')
('utilities' recompileAll recompileAllFrom:)
('user interface support' autoCompleterClass editorClass textStylerClass)
!


!CodeProvider reorganize!
('annotation' addPriorVersionsCountForSelector:ofClass:to: annotation annotationForClassCommentFor: annotationForClassDefinitionFor: annotationForHierarchyFor: annotationForSelector:ofClass: annotationRequests annotationSeparator)
('categories' categoryFromUserWithPrompt:for: categoryOfCurrentMethod changeCategory letUserReclassify:in: methodCategoryChanged selectedMessageCategoryName)
('contents' acceptedContentsChanged acceptedStringOrText commentContents contentsSymbol contentsSymbol:)
('commands' hierarchyBrowser removeClass)
('controls' contentsSymbolQuints sourceAndDiffsQuintsOnly)
('diffs' defaultDiffsSymbol diffFromPriorSourceFor: shouldDiffWords showDiffs: showLineDiffs: showPrettyLineDiffs: showPrettyWordDiffs: showWordDiffs: showingAnyKindOfDiffs showingAnyKindOfPrettyDiffs showingLineDiffs showingLineDiffsString showingPrettyLineDiffs showingPrettyLineDiffsString showingPrettyWordDiffs showingPrettyWordDiffsString showingWordDiffs showingWordDiffsString toggleDiffing toggleLineDiffing togglePlainSource togglePrettyLineDiffing togglePrettyPrint togglePrettyWordDiffing toggleWordDiffing)
('misc' isThereAnOverride isThisAnOverride okayToAccept priorSourceOrNil releaseCachedState setClassAndSelectorIn:)
('self-updating' didCodeChangeElsewhere stepAt:in: updateListsAndCodeIn: updatePaneIfNeeded wantsSteps)
('what to show' prettyPrintString showByteCodes: showComment showDecompile: showDocumentation: showingByteCodes showingByteCodesString showingDecompile showingDecompileString showingDocumentation showingDocumentationString showingPlainSource showingPlainSourceString showingPrettyPrint showingSource toggleDecompile toggleShowDocumentation toggleShowingByteCodes)
('message list' decompiledSource selectedBytecodes selectedMessage sourceStringPrettifiedAndDiffed validateMessageSource:forSelector:inClass:)
('message list menu' copySelector fileOutMessage revertToPreviousVersion timeStamp unusedMethods)
('message category functions' canShowMultipleMessageCategories)
('testing' is: isRefusingToAccept)
('code pane menu')
('accessing' currentCompiledMethod selectedClass selectedClassOrMetaClass selectedMessageName)
('evaluation' doItContext)
('shout styling' isModeStyleable shoutAboutToStyle:)
('user interface support' autoCompleterClass editorClass textStylerClass)
!

!methodRemoval: Workspace #autoCompleter!
Workspace removeSelector: #autoCompleter!
!methodRemoval: Workspace #is:!
Workspace removeSelector: #is:!

!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('variable declarations' mustDeclareVariableWording toggleVariableDeclarationMode)
('gui' openLabel:)
('shout styling' shouldStyle shoutAboutToStyle: toggleStyling toggleStylingLabel)
('testing')
('initialization' initialize)
('user interface support' autoCompleterClass editorClass textStylerClass)
!

!methodRemoval: PluggableTextModel #autoCompleter!
PluggableTextModel removeSelector: #autoCompleter!

!PluggableTextModel reorganize!
('accessing' actualContents textGetter:textSetter:selectionGetter: textProvider)
('testing' is: refusesToAccept wantsFrameAdornments)
('misc' autoSelectString getSelection refetch)
('updating' update:)
('commands' acceptContentsFrom:)
('shout styling' shoutAboutToStyle:)
('initialization' initWith:)
('user interface support' autoCompleterClass editorClass textStylerClass)
!

!methodRemoval: TextModel #autoCompleter!
TextModel removeSelector: #autoCompleter!

!TextModel reorganize!
('accessing' actualContents actualContents: basicActualContents: contents: textSize undoRedoCommands undoRedoCommandsPosition)
('misc' getSelection refetch)
('testing' isTextEmpty wantsFrameAdornments)
('commands' acceptContentsFrom:)
('shout support' privateAddBoldAt: privateRemoveBoldAt: privateStyleWith: stylerStyled:checkForChanges:)
('undoable commands' basicReplaceAttributesFrom:to:with: basicReplaceFrom:to:with: commandForReplaceFrom:to:with: logUndoAndAddAttribute:from:to: logUndoAndRemoveAttribute:from:to: logUndoAndReplaceFrom:to:with: redoAndEvaluate: undoAndEvaluate:)
('object serialization' convertToCurrentVersion:refStream:)
('initialization' flushUndoRedoCommands initialize openLabel:)
('copying' postCopy)
('user interface support' autoCompleterClass editorClass textStylerClass)
!

