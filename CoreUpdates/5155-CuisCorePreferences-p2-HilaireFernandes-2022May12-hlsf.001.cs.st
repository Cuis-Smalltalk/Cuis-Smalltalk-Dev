'From Cuis 6.0 [latest update: #5146] on 12 May 2022 at 10:02:08 am'!

!Workspace methodsFor: 'shout styling' stamp: 'hlsf 5/12/2022 09:38:27'!
shouldStyle

	^shouldStyle ifNil: [ PreferenceNG at: #shoutInWorkspaces]! !

!Workspace methodsFor: 'user interface support' stamp: 'hlsf 5/12/2022 09:40:42'!
allowStylingWithEmphasis
	"Disabled by default for faster styling of large contents, as text metrics are not affected by styling."

	^ PreferenceNG at: #stylingWithEmphasisInWorkspaces! !

!Workspace methodsFor: 'user interface support' stamp: 'hlsf 5/12/2022 09:26:49'!
fullPrintIt

	^fullPrintIt ifNil: [ PreferenceNG at: #fullPrintItInWorkspaces]! !


!CodeProvider methodsFor: 'annotation' stamp: 'hlsf 5/12/2022 09:21:23'!
annotationForClassDefinitionFor: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Class definition for ';
			nextPutAll: aClass name.
		(PreferenceNG at: #classAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#instanceMethodsCount] -> [
					strm
						print: (aClass theNonMetaClass selectors size);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (aClass theMetaClass selectors size);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (aClass theNonMetaClass linesOfCode);
						nextPutAll: ' total lines of code' ]
			}]].! !

!CodeProvider methodsFor: 'annotation' stamp: 'hlsf 5/12/2022 09:35:20'!
annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"

	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].

	^ String streamContents: [ :strm |
		(PreferenceNG at: #methodAnnotations)
			do: [ :each |
				each caseOf: {
					[#firstComment] -> [
						strm nextPutAll: (aClass firstCommentAt: aSelector) ].
					[#masterComment] -> [
						strm nextPutAll: ((aClass supermostPrecodeCommentFor: aSelector) ifNil: ['']) ].
					[#documentation] -> [
						strm nextPutAll: ((aClass precodeCommentOrInheritedCommentFor: aSelector) ifNil: ['']) ].
					[#timeStamp] -> [ | stamp |
						stamp _ self timeStamp.
						strm nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp'])].
					[#linesOfCode] -> [
						strm
							print: ((aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm | cm linesOfCode]);
							nextPutAll: ' lines of code' ].
					[#messageCategory] -> [
						strm nextPutAll: (( aClass organization categoryOfElement: aSelector) ifNil: ['']) ].
					[#sendersCount] -> [ | sendersCount |
						sendersCount _ Smalltalk numberOfSendersOf: aSelector.
						sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
						strm nextPutAll: sendersCount ].
					[#implementorsCount] -> [ | implementorsCount |
						implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
						implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
						strm nextPutAll: implementorsCount ].
					[#priorVersionsCount] -> [
						self addPriorVersionsCountForSelector: aSelector ofClass: aClass to: strm].
					[#priorTimeStamp] -> [ | stamp |
						stamp _ VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
						strm nextPutAll: 'prior timestamp: '; nextPutAll: (stamp ifNil: ['None']) ].
					[#packages] -> [
						(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
							(CodePackage packageOfMethod: cm methodReference ifNone: nil)
								ifNil: [ strm nextPutAll: 'in no package' ]
								ifNotNil: [ :codePackage |
									strm nextPutAll: 'in package '; nextPutAll: codePackage packageName ]]].
					[#changeSets] -> [ | aList |
						aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allChangeSets] -> [ | aList |
						aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allBaseSystemChangeSets] -> [ | aList |
						aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in base system change set']
									ifFalse: [strm nextPutAll: 'in base system change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no base system change set']].
					[#closuresInfo] -> [
						strm nextPutAll: (aClass closuresInfoAt: aSelector)].
					
				}]
			separatedBy: [ strm nextPutAll: self annotationSeparator ] ].! !

!CodeProvider methodsFor: 'annotation' stamp: 'hlsf 5/12/2022 09:41:18'!
annotationForSystemCategory: aCategory
	"Provide a line of content for an annotation pane, given that the receiver is pointing at a System Category (i.e. a group of classes)."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'System Category: ';
			nextPutAll: aCategory.
		(PreferenceNG at: #systemCategoryAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#classCount] -> [
					strm
						print: (SystemOrganization listAtCategoryNamed: aCategory) size;
						nextPutAll: ' classes' ].
				[#instanceMethodsCount] -> [
					strm
						print: (SystemOrganization instanceMethodCountOf: aCategory);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (SystemOrganization classMethodCountOf: aCategory);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (SystemOrganization linesOfCodeOf: aCategory);
						nextPutAll: ' total lines of code' ]
			}]].! !

!CodeProvider methodsFor: 'contents' stamp: 'hlsf 5/12/2022 09:17:20'!
contentsSymbol
	"Answer a symbol indicating what kind of content should be shown for the method; for normal showing of source code, this symbol is #source.  A nil value in the contentsSymbol slot will be set to #source by this method"

	^ contentsSymbol ifNil: [
		contentsSymbol _ (PreferenceNG at: #browseWithPrettyPrint)
					ifTrue:
						[#prettyPrint]
					ifFalse:
						[#source]]! !

!CodeProvider methodsFor: 'diffs' stamp: 'hlsf 5/12/2022 09:26:00'!
defaultDiffsSymbol
	"Answer the code symbol to use when generically switching to diffing"

	^ (PreferenceNG at: #diffsWithPrettyPrint)
		ifTrue: [
			#prettyLineDiffs]
		ifFalse: [
			#lineDiffs]! !


!Browser methodsFor: 'class functions' stamp: 'hlsf 5/12/2022 09:29:13'!
createInstVarAccessors
	"Create getters and setters for all inst vars defined at the level of the current class selection, except do NOT clobber or override any selectors already understood by the instances of the selected class"
	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		aClass instVarNames do: [ :aName | | newMessage setter |
			(aClass canUnderstand: aName asSymbol) ifFalse: [
				newMessage _ aName , '
	"Answer the value of ' , aName , '"

	^ ' , aName.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ].
			(aClass canUnderstand: (setter _ aName , ':') asSymbol) ifFalse: [
				newMessage _ setter , ' anObject
	"Set the value of ' , aName , '"

	' , aName , ' ' ,
					((PreferenceNG at: #leftArrowAssignmentsInGeneratedCode)
						ifTrue: [ '_' ]
						ifFalse: [ ':=' ]) , ' anObject'.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ]]]! !


!Browser class methodsFor: 'class list' stamp: 'hlsf 5/12/2022 09:30:51'!
disableListClassesHierarchically
	
	^PreferenceNG at: #listClassesHierarchically put: false! !

!Browser class methodsFor: 'class list' stamp: 'hlsf 5/12/2022 09:31:05'!
enableListClassesHierarchically
	
	^PreferenceNG at: #listClassesHierarchically put: true! !

!Browser class methodsFor: 'class list' stamp: 'hlsf 5/12/2022 09:31:17'!
listClassesHierarchically
	
	^PreferenceNG at: #listClassesHierarchically! !


!CodeFileBrowser methodsFor: 'edit pane' stamp: 'hlsf 5/12/2022 09:17:27'!
selectedMessage
	"Answer a copy of the source code for the selected message selector."

	| class selector answer |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	answer _ class sourceCodeAt: selector.
	(self classOrMetaClassOrganizer isRemoved: selector) ifTrue: [
		^ Text
			string: answer
			attribute: TextColor red ].
	(PreferenceNG at: #browseWithPrettyPrint) ifTrue: [
		answer _ class compilerClass new
						format: answer in: class notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		answer _ self
			methodDiffFor: answer
			selector: self selectedMessageName ].
	^ answer! !


!ChangeList methodsFor: 'initialization-release' stamp: 'hlsf 5/12/2022 09:25:35'!
initialize
	"Initialize a blank ChangeList.  Set the contentsSymbol to reflect whether diffs will initally be shown or not"

	contentsSymbol _ (PreferenceNG at: #diffsInChangeList)
		ifTrue:
			[self defaultDiffsSymbol]
		ifFalse:
			[#source].
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	super initialize! !


!Debugger class methodsFor: 'class initialization' stamp: 'hlsf 5/12/2022 09:32:24'!
openContext: aContext label: aString contents: contentsStringOrNil
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	<primitive: 19> "Simulation guard"

	(self errorRecursion not and: [PreferenceNG at: #logDebuggerStackToFile]) ifTrue:
		[Smalltalk logError: aString inContext: aContext to: 'CuisDebug'].
	ErrorRecursion ifTrue: [
		ErrorRecursion _ false.
		contentsStringOrNil
			ifNil: [
				self primitiveError: 'Can not open debugger due to recursion error.', 
					String newLineString, aString]
			ifNotNil: [
				self primitiveError: 'Can not open debugger due to recursion error.', 
					String newLineString, aString, String newLineString, contentsStringOrNil ]].
	ErrorRecursion _ true.
	[self informExistingDebugger: aContext label: aString.
	(Debugger context: aContext)
		openNotifierContents: contentsStringOrNil
		label: aString.] ensure: [ ErrorRecursion _ false ].
	Processor activeProcess suspend.
! !

!Debugger class methodsFor: 'opening' stamp: 'hlsf 5/12/2022 09:32:58'!
openInterrupt: aString onProcess: interruptedProcess
	"Open a notifier in response to an interrupt. An interrupt occurs when the user types the interrupt key (cmd-. on Macs, ctrl-c or alt-. on other systems) or when the low-space watcher detects that memory is low."
	| debugger |
	<primitive: 19> "Simulation guard"
	debugger _ self new.
	debugger
		process: interruptedProcess
		context: interruptedProcess suspendedContext.
	debugger externalInterrupt: true.

	(PreferenceNG at: #logDebuggerStackToFile) ifTrue:
		[(aString includesSubString: 'Space') & 
			(aString includesSubString: 'low') ifTrue: [
				Smalltalk logError: aString inContext: debugger interruptedContext to: 'LowSpaceDebug']].

	^ debugger
		openNotifierContents: nil
		label: aString
! !

!Debugger class methodsFor: 'opening' stamp: 'hlsf 5/12/2022 09:33:28'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	| w |
	(PreferenceNG at: #logDebuggerStackToFile) ifTrue: [
		Smalltalk
			logError: title
			inContext: context
			to: 'CuisDebug' ].
	w := UISupervisor newProcessIfUI: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	UISupervisor whenUIinSafeState: [
		[ | debugger |
		"In case an error in Morphic code got us here, ensure mouse focus has been released"
		true runningWorld ifNotNil: [ :rWorld |
			rWorld activeHand ifNotNil: [ :aHand |
				aHand releaseMouseFocus ]].
		debugger := self new
			process: process
			context: context.
		debugger interruptedProcessUI: w.
		bool
			ifTrue: [ debugger openFullMorphicLabel: title ]
			ifFalse: [
				PreDebugWindow
					open: debugger
					label: title
					message: nil ]]
			on: UnhandledError
			do: [ :exOuter | | errorDescription |
				errorDescription := 'Error while trying to open Debugger' , String newLineString , 'Orginal error: ' , title asPlainString , '.' , String newLineString , '	Debugger error: ' ,
					([ exOuter description ]
						on: UnhandledError
						do: [ :exInner |
							exInner return: 'a ' , exInner class printString ]) , ':'.
				self primitiveError: errorDescription ]].
	process suspend.! !


!Behavior methodsFor: 'testing method dictionary' stamp: 'hlsf 5/12/2022 10:01:09'!
whichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a literal."

	| who |

	(PreferenceNG at: #thoroughSenders)
		ifTrue: [ who _ self thoroughWhichSelectorsReferTo: literal special: specialFlag byte: specialByte ]
		ifFalse: [ 
			who _ Set new.
			self selectorsAndMethodsDo: [:sel :method |
				((method hasLiteral: literal) or: [specialFlag and: [method scanFor: specialByte]]) ifTrue: [
					((literal isVariableBinding) not or: [method sendsToSuper not
					"N.B. (method indexOfLiteral: literal) < method numLiterals copes with looking for
					Float bindingOf: #NaN, since (Float bindingOf: #NaN) ~= (Float bindingOf: #NaN)."
						or: [(method indexOfLiteral: literal) ~= 0]]) ifTrue: [who add: sel]]]].
		
	^self rejectSelectorsFrom: who thatReferenceTo: literal byte: specialByte ! !


!Message methodsFor: 'stub creation' stamp: 'hlsf 5/12/2022 09:29:21'!
addSetterCodeOn: stream 
						
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: ((PreferenceNG at: #leftArrowAssignmentsInGeneratedCode)
			ifTrue: [ ' _ ' ]
			ifFalse: [ ' := ' ]);
		nextPutAll: self arguments first argumentName ! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'hlsf 5/12/2022 09:23:05'!
logError: errMsg inContext: aContext to: baseFilename
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	| localFilename file |
	localFilename _ (PreferenceNG at: #debugLogTimestamp)
		ifTrue: [ baseFilename, '-', Utilities dateTimeSuffix, '.log' ]
		ifFalse: [ baseFilename, '.log' ].
	file _ DirectoryEntry smalltalkImageDirectory // localFilename.
	[
		file forceWriteStreamDo: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"
	[
 	 	StdIOWriteStream stdout newLine; nextPutAll: errMsg.
		StdIOWriteStream stdout newLine; nextPutAll: 'See '; nextPutAll: file pathName.
		StdIOWriteStream stdout newLine.
		aContext shortErrorReportOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout flush
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"! !


!BlockNode methodsFor: 'printing' stamp: 'hlsf 5/12/2022 09:35:47'!
printOn: aStream indent: level
	| separateLines |
	aStream nextPut: $[.
	self
		printArgumentsOn: aStream
		indent: level.
	separateLines _ (self
		printTemporaries: temporaries
		on: aStream
		doPrior: [ aStream space ]) or: [arguments notNil and: [arguments notEmpty] ].
	(PreferenceNG at: #prettyPrintRectangularBlocks)
		ifTrue: [
			"If args+temps > 0 and statements > 1 (or just one complex statement),
			put all statements on separate lines"
			separateLines
				ifTrue: [
					(statements size > 1 or: [
						statements size = 1 and: [ statements first isComplex ]])
							ifTrue: [ aStream newLineTab: (1 max: level) ]
							ifFalse: [ aStream space ] ]
				ifFalse: [
					(statements size = 1 and: [ statements first isComplex not ])
						ifTrue: [ aStream space ]]]
		ifFalse: [
			self isComplex
				ifTrue: [ aStream newLineTab: (1 max: level) ]
				ifFalse: [ aStream space ] ].
	((self printStatementsOn: aStream indent: level) > 0 and: [ aStream peekLast ~= $] ])
		ifTrue: [ aStream space ].
	aStream nextPut: $]! !

!BlockNode methodsFor: 'testing' stamp: 'hlsf 5/12/2022 09:35:57'!
printsInNewLine
	"Used for pretty printing to determine whether to start a new line"

	(PreferenceNG at: #prettyPrintRectangularBlocks) ifFalse: [ ^false ].
	^super printsInNewLine! !


!TempVariableNode methodsFor: 'testing' stamp: 'hlsf 5/12/2022 09:13:14'!
assignmentCheck: encoder at: location
	^((self isBlockArg and: [(PreferenceNG at: #allowBlockArgumentAssignment) not])
	    or: [self isMethodArg])
			ifTrue: [location]
			ifFalse: [-1]! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'hlsf 5/12/2022 09:14:00'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry |
	(PreferenceNG at: #alternativeBrowseIt) ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [^ self].
	aSymbol _ self selectedSymbol ifNil: [
		self
			evaluateSelectionAndDo: [ :result | result class name ]
			ifFail: [ ^morph flash ]
			profiled: false].

	aSymbol first isUppercase
		ifTrue: [
			anEntry _ (Smalltalk
				at: aSymbol
				ifAbsent: [
					Smalltalk browseAllImplementorsOf: aSymbol.
					^ nil]).
			anEntry ifNil: [^ morph flash].
			(anEntry isKindOf: Class)
				ifFalse: [anEntry _ anEntry class].
			BrowserWindow fullOnClass: anEntry selector: nil]
		ifFalse:
			[Smalltalk browseAllImplementorsOf: aSymbol]! !


!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'hlsf 5/12/2022 09:14:51'!
syntheticST80Glyphs
	"Build and answer a derivative that includes ST-80 glyphs:
	a left arrow instead of the underscore, and an up arrow instead of the caret."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'ST80';
		perform: (PreferenceNG at: #assignmentGlyphSelector).
	^ derivative! !


!MorphicScanner methodsFor: 'scanning' stamp: 'hlsf 5/12/2022 09:16:43'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos x1 |

	topLeft _ textTopLeft.
	line _ textLine.
	lineY _ line top + textTopLeft y.
	rightMargin _ line rightMargin + textTopLeft x.
	lastIndex _ line first.
	tabCount _ 0.
	leftInRun <= 0 ifTrue: [
		self setFont.
		self setStopConditions ].
	leftMargin _ (line leftMarginForAlignment: alignment) + textTopLeft x.
	destX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				foregroundColor _ defaultColor.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				spaceWidth _ font widthOf: Character space.
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]]].
		self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.

	leftInRun <= 0
		ifTrue: [nowLeftInRun _ text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun _ leftInRun].
	runStopIndex _ lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount _ 0.
	string _ text string.

	self placeEmbeddedObject.
	[
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions.
		backgroundColor ifNotNil: [
			x1 _ destX.
			((PreferenceNG at: #backgroundColorFillsAllBackground) and: [startIndex > line last]) ifTrue: [
				x1 _ rightMargin ].
			canvas
				fillRectangle: (lastPos corner: x1 @ (line bottom + textTopLeft y))
				color: backgroundColor.
			((PreferenceNG at: #backgroundColorFillsAllBackground) and: [stopCondition = #tab]) ifTrue: [
				canvas
					fillRectangle: (destX @ lastPos y corner: self tabDestX @ (line bottom + textTopLeft y))
					color: backgroundColor ]].
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat! !


!HaloMorph methodsFor: 'handles' stamp: 'hlsf 5/12/2022 09:22:10'!
addDebugHandle: handleSpec

	(PreferenceNG at: #debugHaloHandle) ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doDebug:with: ]
! !

!HaloMorph methodsFor: 'handles' stamp: 'hlsf 5/12/2022 09:22:18'!
addExploreHandle: handleSpec

	(PreferenceNG at: #debugHaloHandle) ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doExplore:with: ]
! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'hlsf 5/12/2022 09:40:03'!
drawOn: aCanvas

	(PreferenceNG at: #showLinesInHierarchyViews) ifTrue:[
		self drawLinesOn: aCanvas ]! !


!InnerTextMorph methodsFor: 'private' stamp: 'hlsf 5/12/2022 09:41:56'!
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
		textComposition _ tc.
		tc
			setModel: model;
			extentForComposing: self extentForComposing.
		e textComposition: tc.
		tc editor: e ].
	e setEmphasisHereFromText.
	tc composeAll.
	e resetState.
	self fit.
	self selectionChanged.

	"Add extras. Text Styler and Autocompleter"
	self stylerClass:
		((PreferenceNG at: #syntaxHighlightingAsYouType) ifTrue: [
			model textStylerClass ]).
	self autoCompleterClass:
		model autoCompleterClass! !


!CodeWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 09:39:05'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	(PreferenceNG at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(PreferenceNG at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!CodeWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 09:38:07'!
buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
	
	| codePane |
	
	codePane := self createCodePaneMorph.	
	(PreferenceNG at: #shiftClickShowsImplementors)
		ifTrue: [ self addShiftClickEventHandlerFor: codePane ].
		
	^codePane! !

!CodeWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 09:24:33'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList textConstructor |
	textConstructor _ [ :string :backgroundColor | 
		string asText addAttribute: (TextBackgroundColor color: backgroundColor) ].

	aList _ #(
		(10	'browse'			browseMethodFull							'view this method in a browser')
		(11	'senders' 			browseSendersOfMessages							'browse senders of...' 						browseSendersOfMethod)
		(16	'implementors'			browseMessages							'browse implementors of...' 						browseImplementors)
		(12	'versions'			browseVersions							'browse versions')), 

		((PreferenceNG at: #decorateBrowserButtons)
			ifTrue: [
				{{13	. 'inheritance'.	 #methodInheritance. 	'Browse Method Inheritance

', (textConstructor value: 'green' value: `Color green muchLighter`),': sends to super
', (textConstructor value: 'tan' value: `Color tan`), ': has override(s)
', (textConstructor value: 'mauve' value: `Color blue muchLighter`), ': both of the above
', (textConstructor value: 'pink' value: `Color red muchLighter`), ': is an override but doesn''t call super
', (textConstructor value: 'pinkish tan' value: `Color r: 0.94 g: 0.823 b: 0.673`), ': has override(s), also is an override but doesn''t call super
'}}]
			ifFalse: [
				{#(13	'inheritance'		 methodInheritance	 'browse method inheritance')}]),

		#(
		(12	'hierarchy'			browseHierarchy							'browse class hierarchy')
		(10	'inst vars'			browseInstVarRefs							'inst var refs...')
		(11	'class vars'			browseClassVarRefs							'class var refs...')
		(10	'show...'			offerWhatToShowMenu							'menu of what to show in lower pane')).

	^ aList! !

!CodeWindow methodsFor: 'updating' stamp: 'hlsf 5/12/2022 09:24:21'!
decorateForInheritance
	"Check to see if the currently-viewed method has a super send or an override, and if so, change screen feedback, unless the #decorateBrowserButtons says not to."

	| cm aColor aButton flags buttonColor |
	(aButton _ self inheritanceButton) ifNil: [^ self].
	buttonColor _ self buttonColor.

	(PreferenceNG at: #decorateBrowserButtons)
		ifFalse: [ ^aButton color: buttonColor ].
	cm _ model currentCompiledMethod.
	(cm is: #CompiledMethod)
		ifFalse: [ ^aButton color: buttonColor ].

	flags _ 0.
	model isThisAnOverride ifTrue: [ flags _ flags bitOr: 4 ].
	cm sendsToSuper ifTrue: [ flags _ flags bitOr: 2 ].
	model isThereAnOverride ifTrue: [ flags _ flags bitOr: 1 ].
	aColor _ {

		"This is NOTan override. There is no super implementation."
		buttonColor.							"no sends to super. there is not override in any subclass"
		`Color tan`.							"no sends to super. there is an override in some subclass"
		`Color red`.							"sends to super. there is no override in any subclass. Error: no super to call (or calls super with a different message)"
		`Color red`.							"sends to super. there is  an override in some subclass. Error: no super to call (or calls super with a different message)"

		"This is an override. There is some super implementation"
		`Color red muchLighter`.			"doesn't have sub; has super but doesn't call it"
		`Color r: 0.94 g: 0.823 b: 0.673`.		"has sub; has super but doesn't call it"
		`Color green muchLighter`.			"doesn't have sub; has super and callsl it"
		`Color blue muchLighter`.			"has sub; has super and callsl it"

	} at: flags + 1.
	Theme current useUniformColors
		ifTrue: [
			aButton color: (self buttonColor mixed: 0.8 with: aColor) ]
		ifFalse: [
			aButton color: aColor ]! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 09:39:16'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations code comment separator |
	code _ self buildMorphicCodePane.
	comment _ self buildMorphicCommentPane.
	separator _ LayoutAdjustingMorph new.
	comment separator: separator code: code.
	codeAndButtons _ LayoutMorph newColumn.
	(PreferenceNG at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: code proportionalHeight: 0.5;
		addMorph: separator fixedHeight: Theme current layoutAdjusterThickness;
		addMorph: comment proportionalHeight: 0.5.
	(PreferenceNG at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 09:39:24'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	(PreferenceNG at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(PreferenceNG at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'hlsf 5/12/2022 09:39:33'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations twoRowsOfButtons h |
	twoRowsOfButtons _ LayoutMorph newColumn.
	h _ self defaultButtonPaneHeight.
	(PreferenceNG at: #optionalButtons) ifTrue: [
		h _ self defaultButtonPaneHeight * 2.
		twoRowsOfButtons
			addMorph: self optionalButtonRow proportionalHeight: 1.0;
			addAdjusterMorph ].
	twoRowsOfButtons
		addMorph: self customButtonRow proportionalHeight: 1.0.
	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: twoRowsOfButtons fixedHeight: h;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(PreferenceNG at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!Theme methodsFor: 'private - shout mappings' stamp: 'hlsf 5/12/2022 09:27:37'!
italic
	^(PreferenceNG at: #italicsInShout)
		ifTrue: [ #italic ]
		ifFalse: [ #normal ]! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'hlsf 5/12/2022 09:57:15'!
privateFormatAndConvert
	"Perform any formatting of formattedText necessary and store or a formatted copy in formattedText"
	(PreferenceNG at: #syntaxHighlightingAsYouTypeAnsiAssignment) ifTrue: [
		self convertAssignmentsToAnsi ].
	(PreferenceNG at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) ifTrue: [
		self convertAssignmentsToLeftArrow ]! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'hlsf 5/12/2022 09:27:08'!
privateStyle

	| alpha end start count startIndexes c hue |
	self parseSetWorkspace: true.
	parser ranges ifNotNil: [ :ranges |
		self setAttributesFromRanges: ranges ].

	(PreferenceNG at: #highlightBlockNesting) ifTrue: [
		startIndexes _ parser blockDepthsStartIndexes.
		count _ startIndexes size.
		parser blockDepths withIndexDo: [ :depth :idx |
			start _ startIndexes at: idx.
			end _ idx = count ifTrue: [formattedText size] ifFalse: [ (startIndexes at: idx+1)-1].
			alpha _ depth / 10.0 min: 1.0.
			hue _ depth * 60.
			c _ Color h: hue s: 0.2 v: 0.5 alpha: alpha.
			formattedText 
				addAttribute: (ShoutTextBackgroundColor color: c ) 
				from: start 
				to: end ]]! !


!SHTextStylerST80 class methodsFor: 'preferences' stamp: 'hlsf 5/12/2022 09:54:06'!
ansiAssignmentPreferenceChanged
	"the user has changed the syntaxHighlightingAsYouTypeAnsiAssignment setting.
	If they have turned it on then force syntaxHighlightingAsYouTypeLeftArrowAssignment
	to be turned off"
	(PreferenceNG at: #syntaxHighlightingAsYouTypeAnsiAssignment) 		ifTrue: [
		PreferenceNG at: #syntaxHighlightingAsYouTypeLeftArrowAssignment put: false]! !

!SHTextStylerST80 class methodsFor: 'preferences' stamp: 'hlsf 5/12/2022 09:56:26'!
leftArrowAssignmentPreferenceChanged
	"the user has changed the syntaxHighlightingAsYouTypeLeftArrowAssignment setting.
	If they have turned it on then force syntaxHighlightingAsYouTypeAnsiAssignment
	to be turned off"
	(PreferenceNG at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) 		ifTrue:[
		PreferenceNG at: #syntaxHighlightingAsYouTypeAnsiAssignment put: false]! !

!SHTextStylerST80 class methodsFor: 'class initialization' stamp: 'hlsf 5/12/2022 09:58:59'!
initialize  
	"Clear styleTable and textAttributesByPixelSize cache so that they will 
	reinitialize.	 

		SHTextStylerST80 initialize
	" 
	
	styleTable := nil.
	textAttributes := nil.	
	"We want to be  informed at preference changes"
	(PreferenceNG instanceAt: #syntaxHighlightingAsYouTypeAnsiAssignment)
		when: #preferenceChanged
		send: #ansiAssignmentPreferenceChanged
		to: self.
	(PreferenceNG instanceAt: #syntaxHighlightingAsYouTypeLeftArrowAssignment)
		when: #preferenceChanged
		send: #leftArrowAssignmentPreferenceChanged
		to: self! !


!ExtractToTemporary methodsFor: 'private - applying steps' stamp: 'hlsf 5/12/2022 09:29:32'!
preferredAssignmentOperator

	^ (PreferenceNG at: #leftArrowAssignmentsInGeneratedCode)
		ifTrue: [ '_' ]
		ifFalse: [ ':=' ]! !

!methodRemoval: SHTextStylerST80 class #initializeSyntaxHighlightingAsYouTypeLeftArrowAssignment stamp: 'hlsf 5/12/2022 09:59:24'!
SHTextStylerST80 class removeSelector: #initializeSyntaxHighlightingAsYouTypeLeftArrowAssignment!
!methodRemoval: SHTextStylerST80 class #initializeSyntaxHighlightingAsYouType stamp: 'hlsf 5/12/2022 09:19:13'!
SHTextStylerST80 class removeSelector: #initializeSyntaxHighlightingAsYouType!
!methodRemoval: SHTextStylerST80 class #initializePreferences stamp: 'hlsf 5/12/2022 09:59:06'!
SHTextStylerST80 class removeSelector: #initializePreferences!
!methodRemoval: SHTextStylerST80 class #initializeSyntaxHighlightingAsYouTypeAnsiAssignment stamp: 'hlsf 5/12/2022 09:59:20'!
SHTextStylerST80 class removeSelector: #initializeSyntaxHighlightingAsYouTypeAnsiAssignment!
!methodRemoval: Parser class #initialize stamp: 'hlsf 5/12/2022 09:12:49'!
Parser class removeSelector: #initialize!
Parser initialize!
SHTextStylerST80 initialize!
