'From Cuis 4.2 of 25 July 2013 [latest update: #2858] on 2 August 2016 at 4:46:06 pm'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/2/2016 16:32:15'!
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
		] on: UnhandledError do: [ :ex |
			self primitiveError:
				'Error while trying to open Debugger', String newLineString,
				'Orginal error: ', 
				title asString, '.', String newLineString,
				'	Debugger error: ', 
				([ex description] on: UnhandledError do: ['a ', ex class printString]), ':'
		]
	].
	process suspend! !


!Inspector methodsFor: 'selecting' stamp: 'jmv 8/2/2016 16:34:24'!
selectionPrintString
	| text |
	text _ [self selection printTextLimitedTo: 12000]
						on: UnhandledError
						do: [text := self printStringErrorText.
							text
								addAttribute: TextColor red
								from: 1
								to: text size.
							text].
	^ text! !


!OrderedCollectionInspector methodsFor: 'as yet unclassified' stamp: 'jmv 8/2/2016 16:35:09'!
fieldList

	| fieldsHere |
	object isNil ifTrue: [^OrderedCollection new].
	fieldsHere _
		[
			(object size <= (self i1 + self i2)
				ifTrue: [(1 to: object size) collect: [:i | i printString]]
				ifFalse: [(1 to: self i1) , (object size-(self i2-1) to: object size) collect: [:i | i printString]])
		] on: UnhandledError do: [:ex | ex return: OrderedCollection new].	
	^self baseFieldList , fieldsHere
"
OrderedCollection new inspect
(OrderedCollection newFrom: #(3 5 7 123)) inspect
(OrderedCollection newFrom: (1 to: 1000)) inspect
"! !


!ContextPart class methodsFor: 'private' stamp: 'jmv 8/2/2016 16:31:22'!
carefullyPrint: anObject on: aStream
	aStream nextPutAll: ([anObject printString]
		on: UnhandledError
		do: ['unprintable ' , anObject class name])! !


!WeakActionSequence methodsFor: 'evaluating' stamp: 'jmv 8/2/2016 16:38:32'!
valueStartingFrom: startIndex
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	| each answer |
	startIndex to: self size do: [:index |
		each := self at: index.
		[ answer := each value ]
			on: UnhandledError
			do: [:exc | 
				self valueStartingFrom: index + 1.
				exc pass]].
	^ answer! !

!WeakActionSequence methodsFor: 'evaluating' stamp: 'jmv 8/2/2016 16:38:23'!
valueWithArguments: anArray startingFrom: startIndex
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	| each answer |
	startIndex to: self size do: [:index |
		each := self at: index.
		[ answer := each valueWithArguments: anArray ]
			on: UnhandledError
			do: [:exc | 
				self valueWithArguments: anArray startingFrom: index + 1.
				exc pass]].
	^ answer! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/2/2016 16:41:07'!
logError: errMsg inContext: aContext to: localFileName
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	[
		DirectoryEntry smalltalkImageDirectory // localFileName forceWriteStreamDo: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: UnhandledError do: [] "avoid recursive errors"! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 8/2/2016 16:36:31'!
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
				[ Feature require: data ] on: UnhandledError do: [] ].
			[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
				{ 'Compiler evaluate: '. data } print.
				[ Compiler evaluate: data ] on: UnhandledError do: [] ].
			[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
				[ rawArgStream atEnd ] whileFalse: [
					startUpScriptArgs nextPut: rawArgStream next ].
				"Can use 'Smalltalk startUpScriptArguments' inside the startUp script
				{ 'Compiler evaluate contents of file named: '. data. ' arguments: '. Smalltalk startUpScriptArguments } print."
				entry _ data asFileEntry.
				entry exists ifTrue: [
					entry readStreamDo: [ :stream |
						[ Compiler evaluate: stream contentsOfEntireFile ] on: UnhandledError do: []]].
				"Maybe we decide to clear them after startup script execution
				startUpScriptArguments _ nil" ]
		}
		otherwise: []
	]! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 8/2/2016 16:35:25'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :bar | 
			[ self atEnd ] whileFalse: [
					bar value: self position.
					self skipSeparators.
					
					[
						val := (self peekFor: $!!) 
								ifTrue: [
									chunk := self nextChunk.
									"These are the ones that should do nothing, 
									because next line is a doit that does the stuff
									(or because it is handled elsewhere)"
									(chunk beginsWith: 'description:  ')
									| (chunk beginsWith: 'provides: ')
									| (chunk beginsWith: 'requires: ')
									| (chunk beginsWith: 'classDefinition: ')
									| (chunk beginsWith: 'classRemoval: ')
									| (chunk beginsWith: 'methodRemoval: ')
									| (chunk beginsWith: 'classMoveToSomePackage: ')
									| (chunk beginsWith: 'methodMoveToSomePackage: ')
										ifFalse: [(Compiler evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									[ Compiler evaluate: chunk logged: true ]
										on: UnhandledError
										do: [ :ex |
											ex print.
											('while evaluating: ', chunk) print.
											ex pass ]
										]] 
							on: InMidstOfFileinNotification
							do: [ :ex | ex resume: true ] ].
			].
	"Note:  The main purpose of this banner is to flush the changes file."
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 8/2/2016 16:39:54'!
selectedSelector
	"Try to make a selector out of the current text selection"

	^ self selection string findSelector! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 8/2/2016 16:34:29'!
buildMorphicWindow
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText |
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
	self setLabel: ([model object printStringLimitedTo: 64] on: UnhandledError do: [ model object class name, ' (printing failed)']).! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 8/2/2016 16:44:54'!
acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| ok prevSelection prevScrollValue |
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
	ok _ model acceptContentsFrom: owner.
	ok == true
		ifTrue: [ model refetch ].

	"During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the codeholding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
	ok
		ifTrue: [
			self editor selectFrom: prevSelection first to: prevSelection last.
			WorldState addDeferredUIMessage: [
				self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ].
				owner verticalScrollBar setValue: prevScrollValue ]]! !


!ObjectExplorerWrapper methodsFor: 'converting' stamp: 'jmv 8/2/2016 16:34:43'!
asString
	| explorerString string |
	explorerString _ [ item printString ]
			on: UnhandledError 
			do: ['<error in printString: evaluate "' , itemName , ' printString" to debug>'].
	string _ itemName , ': ' , explorerString.
	^ string withBlanksCondensed! !

!ObjectExplorerWrapper methodsFor: 'monitoring' stamp: 'jmv 8/2/2016 16:34:46'!
refresh
	"hack to refresh item given an object and a string that is either an index or an instance variable name."
	[ | index |
		(model class allInstVarNames includes: itemName)
			ifTrue: [ item _ model instVarNamed: itemName ]
			ifFalse: [ index _ itemName asNumber.
				(index between: 1 and: model basicSize) ifTrue: [ item _ model basicAt: index]]
	] on: UnhandledError do: [ :ex | item _ nil ]! !


!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 8/2/2016 16:45:19'!
displayPatchFrom: srcString to: dstString tryWords: aBoolean prettyPrintedIn: aClass
	| formattedSrcString formattedDstString |
	formattedSrcString _ aClass
		ifNil: [ srcString ]
		ifNotNil: [
			[
			aClass compilerClass new
				format: srcString
				in: aClass
				notifying: nil ]
				on: Error
				do: [ :ex |
					srcString ]].
	formattedDstString _ aClass
		ifNil: [ dstString ]
		ifNotNil: [
			[
			aClass compilerClass new
				format: dstString
				in: aClass
				notifying: nil ]
				on: Error
				do: [ :ex |
					dstString ]].
	^ self
		displayPatchFrom: formattedSrcString
		to: formattedDstString
		tryWords: aBoolean! !

