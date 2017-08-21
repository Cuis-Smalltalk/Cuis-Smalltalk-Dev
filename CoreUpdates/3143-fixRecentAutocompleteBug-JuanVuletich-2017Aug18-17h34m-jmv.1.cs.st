'From Cuis 5.0 of 7 November 2016 [latest update: #3141] on 18 August 2017 at 5:43:59 pm'!

!Workspace methodsFor: 'testing' stamp: 'jmv 8/18/2017 17:42:35'!
is: aSymbol
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]! !


!Debugger methodsFor: 'testing' stamp: 'jmv 8/18/2017 17:41:49'!
is: aSymbol
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]! !


!Inspector methodsFor: 'testing' stamp: 'jmv 8/18/2017 17:42:07'!
is: aSymbol
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]! !


!ObjectExplorer methodsFor: 'testing' stamp: 'jmv 8/18/2017 17:42:19'!
is: aSymbol
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]! !


!Inspector methodsFor: 'accessing' stamp: 'jmv 9/21/2009 15:16'!
selectedClassOrMetaClass

	^ self selectedClass	"I don't know any better"! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'jmv 8/18/2017 17:43:14'!
computeEntries

	| allSource contextClass id specificModel last3Ranges range prevRange receiverClass prevPrevRange |
	allSource _ model actualContents string.
	specificModel _ (model is: #hasTextProvider)
		ifTrue: [ model textProvider ]
		ifFalse: [ model ].
	contextClass _ (specificModel is: #CodeProvider) ifTrue: [
		specificModel selectedClassOrMetaClass ].

	"Instead of creating a new string, maybe we could pass the last position to parse to Shout..."
	parser _ SHParserST80 new.
	parser
		workspace: ((specificModel is: #providesBindings) ifTrue: [specificModel]);
		classOrMetaClass: contextClass;
		source: (allSource copyFrom: 1 to: position).
	parser parse.
	last3Ranges _ parser last3Ranges.
	range _ last3Ranges third.
	range ifNil: [ ^entries _ #() ].

	"If parsing breaks before position, then we don't know what to suggest, therefore don't open Completion"
	range end = position ifFalse: [ ^entries _ #() ].

	prefix _ allSource copyFrom: range start to: range end.
	
	(parser isMessage: range rangeType) ifTrue: [
		"If previous range is a constant or a well known identifier, we might filter messages"
		prevRange _ last3Ranges second.
		prevPrevRange _ last3Ranges first.
		receiverClass _ nil.
		"3 if -> ifNil: but not ifTrue:
		3=4 -> ifNil: or ifTrue:"
		(prevRange notNil and: [ prevPrevRange isNil or: [ (#(binary keyword) includes: prevPrevRange rangeType) not]]) ifTrue: [
			id _ (allSource copyFrom: prevRange start to: prevRange end).
			receiverClass _ prevRange rangeType caseOf: {
				[ #globalVar ] -> [ (Smalltalk at: id asSymbol) class ].
				[ #self ] -> [ contextClass ].
				[ #super ] -> [ contextClass superclass ].
				[ #true ] -> [ True ].
				[ #false ] -> [ False ].
				[ #nil ] -> [ UndefinedObject ].
				[ #character ] -> [ id first class ].
				[ #number ] -> [ (Compiler evaluate: id) class ].
				[ #string ] -> [ (Compiler evaluate: id) class ].
				[ #symbol ] -> [ (Compiler evaluate: id) class ].
				[ #stringSymbol ] -> [ (Compiler evaluate: id) class ].
				"thisContext could mean ContextPart or BlockClosure..."
				"[ #thisContext ] -> [ ContextPart ]"
			} otherwise: [ nil ]
		].
		^self computeMessageEntries: receiverClass ].

	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [
		^self computeIdentifierEntries ].
	
	"If we don't know what to do, do nothing"
	entries _ #()! !


!Workspace reorganize!
('binding' bindingNamesDo: bindingOf: hasBindingOf: hasBindingThatBeginsWith: initializeBindings)
('variable declarations' mustDeclareVariableWording toggleVariableDeclarationMode)
('gui' openLabel:)
('shout styling' shouldStyle shoutAboutToStyle: toggleStyling toggleStylingLabel)
('initialization' initialize)
('user interface support' autoCompleterClass editorClass textStylerClass)
('testing' is:)
!


!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context:)
('notifier menu' storeLog)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack handleLabelUpdatesIn:whenExecuting: peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('class list' selectedClass)
('dependents access' updateInspectors wantsSteps)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
('breakpoints' toggleBreakOnEntry)
('shout styling' bindingNamesDo: hasBindingOf: shoutAboutToStyle:)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUndertand implement:inClass:context: shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnSubclassResponsibility)
('testing' is:)
!


!Inspector reorganize!
('accessing' baseFieldList context: fieldList i1 i2 object object: selectedClass selectedClassOrMetaClass update wantsSteps)
('selecting' accept: contentsIsString replaceSelectionValue: selectedSlotName selection selectionIndex selectionPrintString selectionUnmodifiable toggleIndex:)
('code' doItContext doItReceiver)
('private' printStringErrorText)
('stepping' stepAt:)
('contents' acceptedStringOrText)
('initialization' initialize inspect:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('user commands' inspectSelection)
('shout styling' bindingNamesDo: hasBindingOf: shoutAboutToStyle:)
('testing' is:)
!


!ObjectExplorer reorganize!
('accessing' doItContext doItReceiver getList object parentObject rootObject rootObject: selector)
('menus' selectedClass)
('monitoring' basicMonitorList monitorList stepAt: stopMonitoring)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('user commands' inspectSelection)
('shout styling' bindingNamesDo: hasBindingOf: shoutAboutToStyle:)
('testing' is:)
!

