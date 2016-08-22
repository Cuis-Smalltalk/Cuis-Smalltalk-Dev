'From Cuis 4.2 of 25 July 2013 [latest update: #2859] on 13 August 2016 at 1:59:41 pm'!

!Object methodsFor: 'printing' stamp: 'sqr 8/13/2016 13:25:24'!
printAs: streamType limitedTo: limit
	"Answer an instance of streamType whose characters describe the receiver.
	If you want to print without a character limit, use fullPrintString."

	| limitedString |
	limitedString _ streamType streamContents: [:s | self printOn: s] limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString].
	^ limitedString , '[..]'! !


!Object methodsFor: 'printing' stamp: 'sqr 8/13/2016 13:25:43'!
printStringLimitedTo: limit
	"Answer a String whose characters are a description of the receiver.
	If you want to print without a character limit, use fullPrintString."

	^self printAs: String limitedTo: limit! !

!Object methodsFor: 'printing' stamp: 'sqr 8/13/2016 13:26:00'!
printTextLimitedTo: limit
	"Answer a Text whose characters are a description of the receiver."

	^self printAs: Text limitedTo: limit! !


!Debugger class methodsFor: 'opening' stamp: 'sqr 8/13/2016 13:35:52'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	| w |
	Preferences logDebuggerStackToFile ifTrue: [
		Smalltalk logError: title inContext: context to: 'CuisDebug.log'].
	w _ ProjectX newProcessIfUIX: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	WorldState addDeferredUIMessage: [ 
		[	| debugger |
			debugger _ self new process: process context: context.
			debugger interruptedProcessUI: w.
			bool
				ifTrue: [debugger openFullMorphicLabel: title]
				ifFalse: [PreDebugWindow open: debugger label: title message: nil]
		] on: UnhandledError do: [ :exOuter |
			| errorDescription |
			errorDescription _ 'Error while trying to open Debugger', String newLineString,
				'Orginal error: ', title asString, '.', String newLineString,
				'	Debugger error: ', 
				([exOuter description]
					on: UnhandledError
					do: [:exInner | exInner return: 'a ', exInner class printString]), ':'.
			self primitiveError: errorDescription
		]
	].
	process suspend! !


!Inspector methodsFor: 'selecting' stamp: 'sqr 8/13/2016 13:48:38'!
selectionPrintString

	^[self selection printTextLimitedTo: 12000]
		on: UnhandledError
		do:
			[:ex |
				ex return:
					(self printStringErrorText
						addAttribute: TextColor red;
						yourself)
			]! !


!ContextPart class methodsFor: 'private' stamp: 'sqr 8/13/2016 13:32:32'!
carefullyPrint: anObject on: aStream

	| printString |
	printString _ [anObject printString]
		on: UnhandledError
		do: [:ex | ex return: 'unprintable ' , anObject class printString].
	aStream nextPutAll: printString! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'sqr 8/13/2016 13:55:11'!
logError: errMsg inContext: aContext to: localFileName
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	[
		DirectoryEntry smalltalkImageDirectory // localFileName forceWriteStreamDo: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: UnhandledError do: [:ex | ex return] "avoid recursive errors"! !

!SystemDictionary methodsFor: 'startup' stamp: 'sqr 8/13/2016 13:56:05'!
processCommandLineArgument: rawArgStream storeStartUpScriptArgsOn: startUpScriptArgs
	"
	Smalltalk processCommandLineArguments
	
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
	Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3''
	"
	| p data entry |
	p _ rawArgStream next.

	(p first = $- and: [ p size > 1 ]) ifTrue: [
		"If the command is not included in p, it is next argument"
		p size = 2
			ifTrue: [
				"as in 		-r RequiredFeature1"
				data _ rawArgStream next ]
			ifFalse: [
				"as in 		-rRequiredFeature2"
				data _ p copyFrom: 3 to: p size ].
		p second caseOf: {
			[ $r ] -> [		"as in 		-rRequiredFeature2"
				{ 'Feature require: '. data } print.
				[ Feature require: data ] on: UnhandledError do: [:ex | ex return] ].
			[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
				{ 'Compiler evaluate: '. data } print.
				[ Compiler evaluate: data ] on: UnhandledError do: [:ex | ex return] ].
			[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
				[ rawArgStream atEnd ] whileFalse: [
					startUpScriptArgs nextPut: rawArgStream next ].
				"Can use 'Smalltalk startUpScriptArguments' inside the startUp script
				{ 'Compiler evaluate contents of file named: '. data. ' arguments: '. Smalltalk startUpScriptArguments } print."
				entry _ data asFileEntry.
				entry exists ifTrue: [
					entry readStreamDo: [ :stream |
						[ Compiler evaluate: stream contentsOfEntireFile ] on: UnhandledError do: [:ex | ex return]]].
				"Maybe we decide to clear them after startup script execution
				startUpScriptArguments _ nil" ]
		}
		otherwise: []
	]! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'sqr 8/13/2016 13:50:13'!
buildMorphicWindow
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText label |
	"Build widgets. We'll assemble them below."
	list _ PluggableListMorph
			model: model 
			listGetter: #fieldList
			indexGetter: #selectionIndex
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #fieldListMenu
			keystrokeAction: #inspectorKey:from:.
	contentsText _ TextModelMorph
			textProvider: model
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.
	evaluatorText _ (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false.
	"Upper row has two widgets, side by side: the list of variables at the left side, and the variable contents pane at the right side."
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list proportionalWidth: 0.3;
		addAdjusterAndMorph: contentsText proportionalWidth: 0.7.
	"Inspector Window has two rows: At the top, the one we just bult. Below it, the evaluation pane."
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.7;
		addAdjusterAndMorph: evaluatorText proportionalHeight: 0.3.
	"Set label"
	label _ [model object printStringLimitedTo: 64]
		on: UnhandledError
		do: [:ex | ex return: model object class printString, ' (printing failed)'].
	self setLabel: label! !


!InnerTextMorph methodsFor: 'editing' stamp: 'sqr 8/13/2016 13:45:04'!
acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| accepted prevSelection prevScrollValue |
	prevSelection _ self editor selectionInterval copy.
	prevScrollValue _ owner verticalScrollBar value.
	(self canDiscardEdits and: [(self hasProperty: #alwaysAccept) not]) 
		ifTrue: [^self flash].
	self hasEditingConflicts 
		ifTrue: [
			(self confirm: 
'Caution!! Contents were saved
elsewhere since you started
editing them here.  Accept anyway?' ) 
					ifFalse: [^self flash]].
	accepted _ model acceptContentsFrom: owner.
	"During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the code-holding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
	accepted == true ifTrue:  "some implementors of acceptContentsFrom: answer self :("
		[
			model refetch.
			self editor selectFrom: prevSelection first to: prevSelection last.
			WorldState addDeferredUIMessage: [
				self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ].
				owner verticalScrollBar setValue: prevScrollValue]
		]! !


!ObjectExplorerWrapper methodsFor: 'converting' stamp: 'sqr 8/13/2016 13:51:02'!
asString
	| explorerString |
	explorerString _ [ item printString ]
		on: UnhandledError 
		do: [:ex | ex return: '<error in printString: evaluate "' , itemName , ' printString" to debug>'].
	^itemName , ': ' , explorerString :: withBlanksCondensed! !

!ObjectExplorerWrapper methodsFor: 'monitoring' stamp: 'sqr 8/13/2016 13:51:29'!
refresh
	"hack to refresh item given an object and a string that is either an index or an instance variable name."
	[ | index |
		(model class allInstVarNames includes: itemName)
			ifTrue: [ item _ model instVarNamed: itemName ]
			ifFalse: [ index _ itemName asNumber.
				(index between: 1 and: model basicSize) ifTrue: [ item _ model basicAt: index]]
	] on: UnhandledError do: [ :ex | item _ nil.  ex return ]! !

