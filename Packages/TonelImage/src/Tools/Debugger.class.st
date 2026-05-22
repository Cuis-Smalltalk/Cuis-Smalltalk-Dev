"
I represent the machine state at the time of an interrupted process. I also represent a query path into the state of the process. The debugger is typically viewed through a window that views the stack of suspended contexts, the code for, and execution point in, the currently selected message, and inspectors on both the receiver of the currently selected message, and the variables in the current context.
"
Class {
	#name : 'Debugger',
	#superclass : 'CodeProvider',
	#instVars : [
		'interruptedProcess',
		'contextStack',
		'contextStackTop',
		'contextStackIndex',
		'contextStackList',
		'receiverInspector',
		'contextVariablesInspector',
		'savedCursor',
		'labelString',
		'interruptedProcessUI'
	],
	#category : 'Tools-Debugger',
	#package : 'Tools',
	#tag : 'Debugger'
}

{ #category : 'gui support' }
Debugger class >> debuggerWindowsSatisfying: qualifyingBlock do: anotherBlock [
	"For all open debugger windows whose model (the Debugger itself) satisfies aBlock,
	evaluate anotherBlock with the window as argument.
	GUI support."

	DebuggerWindow debuggerWindowsSatisfying: qualifyingBlock do: anotherBlock
]

{ #category : 'constants' }
Debugger class >> debuggingTerminatedProcessName [
	^'debugging terminated'
]

{ #category : 'constants' }
Debugger class >> defaultDebugStackSize [
	^50
]

{ #category : 'instance creation' }
Debugger class >> informExistingDebugger: aContext label: aString [
	"Walking the context chain, we try to find out if we're in a debugger stepping situation.
	If we find the relevant contexts, we must rearrange them so they look just like they would
	if the methods were excuted outside of the debugger."
	| ctx quickStepMethod oldSender baseContext |
	ctx := thisContext.
	quickStepMethod := ContextPart compiledMethodAt: #quickSend:to:with:lookupIn:.
	[ctx sender == nil or: [ctx sender method == quickStepMethod]] whileFalse: [ctx := ctx sender].
	ctx sender ifNil: [^self].
	baseContext := ctx.
	"baseContext is now the context created by the #quickSend... method."
	oldSender := ctx := ctx sender home sender.
	"oldSender is the context which originally sent the #quickSend... method"
	[ctx == nil or: [ctx receiver isKindOf: self]] whileFalse: [ctx := ctx sender].
	ctx == nil ifTrue: [^self].
	"ctx is the context of the Debugger method #doStep"
	ctx receiver labelString: aString.
	baseContext swapSender: baseContext sender sender sender.	"remove intervening contexts"
	thisContext swapSender: oldSender.	"make myself return to debugger"
	^aContext
]

{ #category : 'constants' }
Debugger class >> lowSpaceMessage [
	"Return a notifier message string to be presented when space is running low."

	^ 'Warning! Cuis is almost out of memory!

Low space detection is now disabled. It will be restored when you close or proceed from this error notifier. Don''t panic, but do proceed with caution.

Here are some suggestions:

- If you suspect an infinite recursion (the same methods calling each other again and again), then close this debugger, and fix the problem.

- If you want this computation to finish, then make more space available (read on) and choose "proceed" in this debugger. Here are some ways to make more space available...
   > Close any windows that are not needed.
   > Get rid of some large objects (e.g., images).
   > Leave this window on the screen, choose "save as..." from the screen menu, quit, restart the Squeak VM with a larger memory allocation, then restart the image you just saved, and choose "proceed" in this window.

- If you want to investigate further, choose "debug" in this window.  Do not use the debugger "fullStack" command unless you are certain that the stack is not very deep. (Trying to show the full stack will definitely use up all remaining memory if the low-space problem is caused by an infinite recursion!).

'

]

{ #category : 'instance creation' }
Debugger class >> newDebugging: aBlock [
	"Create a new process to debug aBlock"

	| processToDebug debugger |
	debugger := self new.
	processToDebug := aBlock newProcessForDebugging name: 'debugIt'.
	debugger process: processToDebug context: processToDebug suspendedContext.
		
	^ debugger
]

{ #category : 'opening' }
Debugger class >> openContext: aContext label: label preDebugMessage: contentsStringOrNil [
	"Open a notifier (a PreDebugWindow) in response to an error, halt, or notify.
	A PreDebugWindow just shows a short view of the sender stack and provides a menu that lets the user open a full debugger.
	The context argument is in the active process stack."

	| debugger |
	<primitive: 19> "Simulation guard"

	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		Smalltalk logError: label inContext: aContext to: 'CuisDebug'].
	self informExistingDebugger: aContext label: label.

	debugger := self new.
	debugger 		process: Processor activeProcess context: aContext.
	debugger openWindowLabel: label usePreDebugWindow: true preDebugMessage: contentsStringOrNil.
]

{ #category : 'opening' }
Debugger class >> openDebugging: aBlock label: aString [

	| debugger |
	debugger := self newDebugging: aBlock.
	debugger openFullMorphicLabel: aString.
		
	^ debugger
]

{ #category : 'opening' }
Debugger class >> openDebugging: aBlock to: aCompiledMethod label: aString [
	"Create a new process to debug aBlock, and step into it."

	| debugger |
	debugger := self openDebugging: aBlock label: aString.
	
	[debugger interruptedContext method == aCompiledMethod]
		whileFalse: [debugger send]
]

{ #category : 'opening' }
Debugger class >> openProcess: process context: context label: labelString [
	"Open a Debugger on an existing process."

	| debugger logFile label |
	<primitive: 19> "Simulation guard"
	label := labelString,
					' - Process: ', process name,
					' - Priority: ', process priority printString,
					' (', process hash printString, ')'.
	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		logFile := 'CuisDebug'.
		(labelString beginsWith: 'Space is low')
			ifTrue: [logFile := 'LowSpaceDebug'].
		Smalltalk logError: label inContext: context to: logFile ].

	debugger := self new.
	debugger process: process context: context.
	debugger openWindowLabel: label usePreDebugWindow: false preDebugMessage: nil.
	"
[1/0] fork.
5 seconds asDelay wait.
self error: 'error'.
	"
]

{ #category : 'gui support' }
Debugger class >> updateDebuggersLeadingTo: aContext for: anException [
	"Update debugger titles to reflect that anException occured.
	For instance, when stepping over some code, if a new halt is found, reflect it on the window title."

	self debuggerWindowsSatisfying: [ :debugger |
		aContext hasSender: debugger interruptedContext ]
		do: [ :debuggerWindow |
			debuggerWindow setLabel: anException description, ' - ', debuggerWindow label ].
]

{ #category : 'shout styling' }
Debugger >> aboutToStyleWith: anSHTextStyler [
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly."

	anSHTextStyler 
		classOrMetaClass: 
			(self selectedContext selector = Scanner doItSelector ifFalse: [
				self selectedClassOrMetaClass]); "nil for DoIts"
		workspace: self.
]

{ #category : 'accessing' }
Debugger >> acceptedStringOrText  [
	"Depending on the current selection, different information is retrieved.
	Answer a string description of that information.  This information is the
	method in the currently selected context."

	^ self selectedContext
			ifNotNil: [self selectedMessage]
			ifNil: [String new]
]

{ #category : 'shout styling' }
Debugger >> allowsStylingFor: selector isTextDirty: hasUnsavedEdits [
	"Answer true to allow styling to proceed, or false to veto the styling"

	^self isModeStyleable
]

{ #category : 'context stack menu' }
Debugger >> askForCategoryIn: aClass default: aDefaultCategory [

	^(MethodCategoriesPrompter 
		staringFrom: aClass 
		rejectingFirst: false) prompt: 'Select category for the new method' ifNone: [ aDefaultCategory ]
]

{ #category : 'method creation' }
Debugger >> askForSuperclassOf: aClass toImplement: aSelector ifCancel: cancelBlock [

	^self askForSuperclassOf: aClass upTo: ProtoObject toImplement: aSelector ifCancel: cancelBlock

]

{ #category : 'method creation' }
Debugger >> askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector ifCancel: cancelBlock [

	^ self askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector withCaption: 'Define #', aSelector, ' in which class?' ifCancel: cancelBlock
]

{ #category : 'method creation' }
Debugger >> askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector withCaption: aCaptionText ifCancel: cancelBlock [

	| classes chosenClassIndex |

	classes := aClass = aSuperclass ifTrue: [ { aClass } ] ifFalse: [ aClass withAllSuperclassesUpTo: aSuperclass ].
	chosenClassIndex := PopUpMenu
		withCaption: aCaptionText
		chooseFrom: (classes collect: [:c | c name]).
	chosenClassIndex = 0 ifTrue: [^ cancelBlock value].

	^ classes at: chosenClassIndex
]

{ #category : 'context stack menu' }
Debugger >> assert: runToCursorContext mapsPcTo: aPosition [

	(runToCursorContext mapsPcTo: aPosition) ifFalse: [ self error: self invalidPositionErrorDescription. ]
]

{ #category : 'shout styling' }
Debugger >> bindingNamesDo: aBlock [

	self selectedContext tempNames do: aBlock
]

{ #category : 'private' }
Debugger >> checkContextSelection [

	contextStackIndex = 0 ifTrue: [self contextStackIndex: 1 oldContextWas: nil].

]

{ #category : 'auto complete' }
Debugger >> classOfThisContext [
	
	^ self selectedContext class
]

{ #category : 'auto complete' }
Debugger >> computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName   [

	self computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName 
]

{ #category : 'auto complete' }
Debugger >> computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName   [

	^self computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName 
]

{ #category : 'auto complete' }
Debugger >> computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName   [

	contextStackIndex = 0 
		ifTrue: [ anAutocompleter computeMessageEntriesForUnknowClass ]
		ifFalse: [ anAutocompleter computeMessageEntriesForClass: (self receiver instVarNamed: aName) class ]
]

{ #category : 'auto complete' }
Debugger >> computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName   [

	| context tempIndex |
							
	context := self selectedContext.
	tempIndex := context tempNames indexOf: aName ifAbsent: [ ^ anAutocompleter computeMessageEntriesForUnknowClass ].
	
	anAutocompleter computeMessageEntriesForClass: 
		(self debuggerMap namedTempAt: tempIndex in: context) class 
]

{ #category : 'accessing' }
Debugger >> contents: input notifying: aController [
	"The retrieved information has changed and its source must now be updated.
	 In this case, the retrieved information is the method of the selected context."

	| codeString result selector classOfMethod category h newMethod |
	codeString := input string.

	contextStackIndex = 0 ifTrue: [^false].

	classOfMethod := self selectedClass.
	category := self selectedMessageCategoryName.
	selector :=self selectedClass parserClass selectorFrom: codeString.

	selector ~= self selectedMessageName ifTrue: [
		self inform: 'Can not change the selector in the debugger'.
		^false].	
 	(classOfMethod = UndefinedObject and: [ selector = Scanner doItSelector or: [ selector = Scanner doItInSelector ]]) ifTrue: [
		self inform: 'DoIt and DoItIn: methods can not be changed'.
	 	^false].

	self selectedContext isExecutingBlock ifTrue: [
		h := self selectedContext activeHome.
		h ifNil: [
			self inform: 'Method for block not found on stack, can''t edit and continue'.
			^false].
		(self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withNewLines) ifFalse: [^false].
		self resetContext: h.
		(result := self contents: codeString notifying: aController) ifTrue: [self acceptedContentsChanged].
		^result].
		
	selector := classOfMethod
				compile: codeString
				classified: category
				notifying: aController.
	selector ifNil: [^false]. "compile cancelled"
	newMethod := classOfMethod compiledMethodAt: selector.

	newMethod isQuick ifTrue: [
		contextStackIndex + 1 > contextStack size ifTrue: [
			self inform: 'Can not compile a quick method in the stack base context'.
			^false].
		self down.
		self selectedContext jump: (self selectedContext previousPc - self selectedContext pc)].

	self resetToSelectedContextWith: newMethod.
	
	^true
]

{ #category : 'code pane' }
Debugger >> contentsSelection [

	"If we are just creating the method in the debugger, help the user by selecting the method body."
	| sendInterval |
	currentCompiledMethod ifNotNil: [
		((currentCompiledMethod hasLiteral: #shouldBeImplemented) and: [
			(currentCompiledMethod abstractPCForConcretePC: self selectedContext pc) = 1]) ifTrue: [
				sendInterval := (self selectedContext debuggerMap abstractSourceMap at: 2 ifAbsent: nil).
				sendInterval ifNotNil: [ ^ sendInterval first - 5 to: sendInterval last + 1 ]]].

	^self pcRange
]

{ #category : 'context stack (message list)' }
Debugger >> contextStackIndex [
	"Answer the index of the selected context."

	^contextStackIndex
]

{ #category : 'private' }
Debugger >> contextStackIndex: anInteger oldContextWas: oldContext [
	"Change the context stack index to anInteger, perhaps in response to user selection."

	| isNewMethod selectedContextSlotName index |
	contextStackIndex := anInteger.
	anInteger = 0 ifTrue: [
		currentCompiledMethod := nil.
		self changed: #contextStackIndex.
		self acceptedContentsChanged.
		contextVariablesInspector object: nil.
		self fixReceiverInspector.
		^ self ].
	selectedContextSlotName := contextVariablesInspector selectedSlotName.
	isNewMethod := oldContext == nil
					or: [ oldContext method ~~ (currentCompiledMethod := self selectedContext method) ].
	self acceptedContentsChanged.
	isNewMethod ifTrue: [
		self pcRange ].
	self changed: #contextStackIndex.
	self triggerEvent: #decorateButtons.
	contextVariablesInspector object: self selectedContext.
	((index := contextVariablesInspector fieldList indexOf: selectedContextSlotName) ~= 0 and: [
		index ~= contextVariablesInspector selectionIndex ]) ifTrue: [
			contextVariablesInspector toggleIndex: index ].
	self fixReceiverInspector.
	isNewMethod ifFalse: [ self changed: #contentsSelection ].
]

{ #category : 'context stack (message list)' }
Debugger >> contextStackList [
	"Answer the array of contexts."

	^contextStackList
]

{ #category : 'accessing' }
Debugger >> contextVariablesInspector [
	"Answer the instance of Inspector that is providing a view of the 
	variables of the selected context."

	^contextVariablesInspector
]

{ #category : 'method creation' }
Debugger >> createMethod [

	self wasInterrupedOnDoesNotUnderstand ifTrue: [ ^self createMethodWhenDoesNotUnderstand ].
	self wasInterruptedOnSubclassResponsibility ifTrue: [ ^self createMethodOnSubclassResponsibility ].
	self wasInterruptedOnOverridableMethod ifTrue: [ ^self overrideMethodOnSubclass ].
	
	self inform: 'Only available to override methods or for #doesNotUnderstand: and #subclassResponsibility' 
]

{ #category : 'method creation' }
Debugger >> createMethodOnSubclassResponsibility [

	| message chosenClass subclassResponsibilityContext |
	
	subclassResponsibilityContext := self interruptedContext sender sender.
	message := subclassResponsibilityContext messageForYourself.
	
	chosenClass := self
		askForSuperclassOf: subclassResponsibilityContext receiver class
		upTo: subclassResponsibilityContext method methodClass
		toImplement: message selector
		ifCancel: [^self].
		
	self implement: message inClass: chosenClass context: subclassResponsibilityContext 


]

{ #category : 'method creation' }
Debugger >> createMethodWhenDoesNotUnderstand [

	| message chosenClass interruptedContext |
	
	"The doesNotUndertand context must be selected - Hernan"
	contextStackIndex = 1 ifFalse: [ self contextStackIndex: 1 oldContextWas: self selectedContext ].
		
	interruptedContext := self interruptedContext.
	message := interruptedContext tempAt: 1.
	
	chosenClass := self
		askForSuperclassOf: interruptedContext receiver class
		toImplement: message selector
		ifCancel: [^self].
	
	self implement: message inClass: chosenClass context: self selectedContext


]

{ #category : 'contents' }
Debugger >> currentMethodRefactored [

	| newMethod |
	
	newMethod := self selectedClass compiledMethodAt: self selectedContext selector.
	self resetToSelectedContextWith: newMethod 
]

{ #category : 'accessing' }
Debugger >> debuggerMap [
	^self selectedContext debuggerMap
]

{ #category : 'code pane' }
Debugger >> doItContext [
	"Answer the context in which a text selection can be evaluated."

	contextStackIndex = 0
		ifTrue: [^super doItContext]
		ifFalse: [^self selectedContext]
]

{ #category : 'code pane' }
Debugger >> doItReceiver [
	"Answer the object that should be informed of the result of evaluating a
	text selection."

	^self receiver
]

{ #category : 'context stack menu' }
Debugger >> doStep [
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns.
	Button [Over] in the Debugger."
	
	| currentContext newContext |

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].

	currentContext := self selectedContext.
	newContext := self handleLabelUpdatesIn: [interruptedProcess completeStep: currentContext]
						whenExecuting: currentContext.
	newContext == currentContext ifTrue:
		[newContext := interruptedProcess stepToSendOrReturn].
	self contextStackIndex > 1
		ifTrue: [self resetContext: newContext]
		ifFalse:
			[newContext == currentContext
				ifTrue: [self changed: #contentsSelection.
						self updateInspectors]
				ifFalse: [self resetContext: newContext]].
]

{ #category : 'context stack menu' }
Debugger >> down [
	"move down the context stack to the previous (enclosing) context"

	self toggleContextStackIndex: contextStackIndex+1
]

{ #category : 'context stack (message list)' }
Debugger >> expandStack [
	"A Notifier is being turned into a full debugger.  Show a substantial amount of stack in the context pane."

	self newStack: (contextStackTop stackOfSize: Debugger defaultDebugStackSize).
	contextStackIndex := 0.
	receiverInspector := Inspector inspect: nil.
	contextVariablesInspector := ContextVariablesInspector inspect: nil.
]

{ #category : 'private' }
Debugger >> fixReceiverInspector [
	"Make receiver inspector work on current context receiver.
	Create a new inspector if needed"

	| currentReceiver requiredInspectorClass oldInspectorClass |
	currentReceiver := self receiver.
	requiredInspectorClass := currentReceiver isObject
		ifTrue: [ currentReceiver inspectorClass ]
		ifFalse: [ Inspector ].
	oldInspectorClass := receiverInspector class.
	
	oldInspectorClass ~= requiredInspectorClass ifTrue: [
		oldInspectorClass format = requiredInspectorClass format
			ifTrue: [receiverInspector primitiveChangeClassTo: requiredInspectorClass basicNew]
			ifFalse: [receiverInspector becomeForward: (requiredInspectorClass basicNew copyFrom: receiverInspector)]].
	
	receiverInspector object: currentReceiver
]

{ #category : 'context stack menu' }
Debugger >> fullStack [
	"Change from displaying the minimal stack to a full one."

	self contextStackList size > self class defaultDebugStackSize "Already expanded"
		ifTrue:
			[self changed: #flash]
		ifFalse:
			[self contextStackIndex = 0 ifFalse: [
				self toggleContextStackIndex: self contextStackIndex].
			self fullyExpandStack]
]

{ #category : 'context stack (message list)' }
Debugger >> fullyExpandStack [
	"Expand the stack to include all of it, rather than the first four or five
	contexts."

	self newStack: contextStackTop contextStack.
	self changed: #contextStackList
]

{ #category : 'context stack menu' }
Debugger >> handleLabelUpdatesIn: aBlock whenExecuting: aContext [
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns."
	
	^aBlock
		on: Notification
		do: [:aNotification|
			aNotification 
				withNewDebuggerLabelOn: aContext
				do: [ :aNewLabel | 
					self labelString: aNewLabel.
					aNotification resume]
				ifNone: [aNotification pass]]
]

{ #category : 'shout styling' }
Debugger >> hasBindingOf: aString [
	^self selectedContext tempNames includes: aString
]

{ #category : 'method creation' }
Debugger >> implement: aMessage inCategory: aCategory fromClass: aClass context: aContext [

	aClass
		compile: (aMessage createStubMethodFor: aClass)
		classified: aCategory.
		
	aContext privRefreshWith: (aClass lookupSelector: aMessage selector).
	aMessage arguments withIndexDo: [ :arg :index | aContext tempAt: index put: arg ].
	self resetContext: aContext
]

{ #category : 'method creation' }
Debugger >> implement: aMessage inClass: aClass context: aContext  [
	
	self implement: aMessage inCategory: (self askForCategoryIn: aClass default: ClassOrganizer defaultMethodCategory) fromClass: aClass context: aContext
]

{ #category : 'accessing' }
Debugger >> interruptedContext [
	"Answer the suspended context of the interrupted process."

	^contextStackTop
]

{ #category : 'accessing' }
Debugger >> interruptedProcess [
	"Answer the interrupted process."

	^interruptedProcess
]

{ #category : 'context stack menu' }
Debugger >> invalidPositionErrorDescription [

	^'Invalid position'
]

{ #category : 'testing' }
Debugger >> is: aSymbol [
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]
]

{ #category : 'testing' }
Debugger >> isEditingMethod [

	^true
]

{ #category : 'accessing' }
Debugger >> isNotifier [
	"Return true if this debugger has not been expanded into a full sized window"

	^ receiverInspector == nil
]

{ #category : 'testing' }
Debugger >> isOnBreakingMethodWrapper [

	^ self selectedContext method == (BreakingMethodWrapper>>#run:with:in:)
]

{ #category : 'testing' }
Debugger >> isOnHaltMethod [
	"Tells whether the selected method is a halt method, like #halt or #haltOnce"

	^ self selectedContext method selector beginsWith: 'halt'
]

{ #category : 'context stack menu' }
Debugger >> isPcAt: aPosition in: aContext [

	^ aContext isDead or: [ self pcRange includes: aPosition ]
]

{ #category : 'accessing' }
Debugger >> labelString [
	^labelString
]

{ #category : 'accessing' }
Debugger >> labelString: aString [
	labelString := aString.
	self changed: #relabel
]

{ #category : 'context stack (message list)' }
Debugger >> messageListIndex [
	"Answer the index of the currently selected context."

	^contextStackIndex
]

{ #category : 'private' }
Debugger >> newStack: stack [
	| oldStack diff |
	oldStack := contextStack.
	contextStack := stack.
	(oldStack == nil or: [oldStack last ~~ stack last])
		ifTrue: [contextStackList := contextStack collect: [:ctx | ctx printString].
				^ self].
	"May be able to re-use some of previous list"
	diff := stack size - oldStack size.
	contextStackList := diff <= 0
		ifTrue: [contextStackList copyFrom: 1-diff to: oldStack size]
		ifFalse: [diff > 1
				ifTrue: [contextStack collect: [:ctx | ctx printString]]
				ifFalse: [(Array with: stack first printString) , contextStackList]]
]

{ #category : 'initialization' }
Debugger >> openFullAt: index [
	self triggerEvent: #closeViews.
	self toggleContextStackIndex: index.
	self openFullMorphicLabel: 'Debugger'
]

{ #category : 'initialization' }
Debugger >> openFullMorphicLabel: aLabelString [
	"Open a full morphic debugger with the given label"

	| oldContextStackIndex |
	oldContextStackIndex := contextStackIndex.
	self expandStack. "Sets contextStackIndex to zero."

	DebuggerWindow open: self label: aLabelString.
	self toggleContextStackIndex: oldContextStackIndex.

	self stepOutBreakpointMethods.
]

{ #category : 'initialization' }
Debugger >> openWindow2Label: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil [

	[
		bool
			ifTrue: [
				PreDebugWindow
					open: self
					label: label
					message: preDebugMessageOrNil ]
			ifFalse: [ self openFullMorphicLabel: label ]
	]
		on: UnhandledError
		do: [ :exOuter | | errorDescription |
			errorDescription := 'Error while trying to open Debugger' , String newLineString , 
				'Orginal error: ' , label asPlainString , '.' , String newLineString , '	Debugger error: ' ,
					([ exOuter description ]
						on: UnhandledError
						do: [ :exInner |
							exInner return: 'a ' , exInner class printString ]) , ':'.
			errorDescription print.
			self primitiveError: errorDescription signalContext: exOuter signalerContext ]
]

{ #category : 'initialization' }
Debugger >> openWindowLabel: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil [
	"Create and schedule a full debugger, or just a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."

	| safeBlockAlreadyRan |
	Sensor flushKeyboard.
	savedCursor := Cursor currentCursor.
	Cursor defaultCursor activateCursor.
	"In case an error in Morphic code got us here, ensure mouse focus has been released"
	self runningWorld ifNotNil: [ :rWorld |
		rWorld activeHand ifNotNil: [ :aHand |
			aHand releaseMouseFocus ]].

	safeBlockAlreadyRan := false.
	UISupervisor whenUIinSafeState: [
		safeBlockAlreadyRan ifFalse: [
			safeBlockAlreadyRan := true.
			"Suspend it as early as possible"
			interruptedProcess suspend ].
		self openWindow2Label: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil ].
	"Ensure the Debugger is opened in a new UI process.
	This avoid unresponsive UI if the interruptedProcess is the UI current UI process, but also if the UI is busy, blocked or suspended."
	UISupervisor newUIProcess.
	"Suspend it as early as possible"
	safeBlockAlreadyRan ifFalse: [
		safeBlockAlreadyRan := true.
		interruptedProcess suspend ].
]

{ #category : 'method creation' }
Debugger >> overrideMethodOnSubclass [

	| chosenClass lastPossibleClass message methodCategory methodClass receiverClass |

	methodCategory := self interruptedContext method category.
	methodClass := self interruptedContext method methodClass.
	receiverClass := self interruptedContext receiver class.
	lastPossibleClass := (receiverClass withAllSuperclassesUpTo: methodClass) penultimate.
	message := self interruptedContext messageForYourself.

	chosenClass := self
		askForSuperclassOf: receiverClass
		upTo: lastPossibleClass
		toImplement: message selector
		withCaption: 'Override #', message selector, ' in which class?'
		ifCancel: [^self].
	
	^ self implement: message inCategory: methodCategory fromClass: chosenClass context: self interruptedContext
]

{ #category : 'code pane' }
Debugger >> pc [

	^ self pcRange
]

{ #category : 'code pane' }
Debugger >> pcRange [
	"Answer the indices in the source code for the method corresponding to 
	the selected context's program counter value."

	contextStackIndex = 0 ifTrue:
		[^1 to: 0].
	self selectedContext isDead ifTrue:
		[^1 to: 0].
	^self selectedContext debuggerMap
		rangeForPC: self selectedContext pc
		contextIsActiveContext: contextStackIndex = 1
]

{ #category : 'context stack menu' }
Debugger >> peelToFirst [
	"Peel the stack back to the second occurance of the currently selected message.  Very useful for an infinite recursion.  Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning.  Also frees a lot of space!"

	| ctxt |
	contextStackIndex = 0 ifTrue: [^ Smalltalk beep].
	"self okToChange ifFalse: [^ self]."
	ctxt := interruptedProcess popTo: self selectedContext findSecondToOldestSimilarSender.
	self resetContext: ctxt.

]

{ #category : 'context stack menu' }
Debugger >> proceed [
	"Proceed execution of the receiver's model, starting after the expression at 
	which an interruption occurred."

	Smalltalk okayToProceedEvenIfSpaceIsLow ifTrue: [
		"So we don't send #windowIsClosing, which acts like 'terminate'"
		self triggerEvent: #closeViews.
		"Resume instead"
		self resumeProcess ]
]

{ #category : 'private' }
Debugger >> process: aProcess context: aContext [

	interruptedProcess := aProcess.
	interruptedProcessUI := (UISupervisor animatedUIOf: interruptedProcess) ifNotNil: [ :world | world mainCanvas ].
	contextStackTop := aContext.
	self newStack: (contextStackTop stackOfSize: 1).
	contextStackIndex := 1.
]

{ #category : 'accessing' }
Debugger >> receiver [
	"Answer the receiver of the selected context, if any. Answer nil 
	otherwise."

	contextStackIndex = 0
		ifTrue: [^nil]
		ifFalse: [^self selectedContext receiver]
]

{ #category : 'accessing' }
Debugger >> receiverInspector [
	"Answer the instance of Inspector that is providing a view of the 
	variables of the selected context's receiver."

	^receiverInspector
]

{ #category : 'private' }
Debugger >> resetContext: aContext  [
	"Used when a new context becomes top-of-stack, for instance when the
	method of the selected context is re-compiled, or the simulator steps or
	returns to a new method. There is room for much optimization here, first
	to save recomputing the whole stack list (and text), and secondly to avoid
	recomposing all that text (by editing the TextComposition instead of recreating it)."

	| oldContext |
	oldContext := self selectedContext.
	contextStackTop := aContext.
	self newStack: contextStackTop contextStack.
	self changed: #contextStackList.
	self contextStackIndex: 1 oldContextWas: oldContext.
	self acceptedContentsChanged.

]

{ #category : 'accessing' }
Debugger >> resetToSelectedContextWith: newMethod [

	| ctxt |

	ctxt := interruptedProcess popTo: self selectedContext.
	ctxt == self selectedContext
		ifFalse:
			[self inform: 'Method saved, but current context unchanged\because of unwind error. Click OK to see error' withNewLines]
		ifTrue:
			[newMethod isQuick ifFalse:
				[interruptedProcess
					restartTopWith: newMethod;
				 	stepToSendOrReturn].
			contextVariablesInspector object: nil].
	self resetContext: ctxt.
	
]

{ #category : 'context stack menu' }
Debugger >> restart [
	"Proceed from the initial state of the currently selected context."

	| ctxt |
	self checkContextSelection.
	ctxt := interruptedProcess popTo: self selectedContext.
	ctxt == self selectedContext ifTrue: [
		interruptedProcess restartTop; stepToSendOrReturn].
	self resetContext: ctxt
]

{ #category : 'private' }
Debugger >> resumeProcess [
	| mustTerminateActive worldIfMustRedisplay |
	worldIfMustRedisplay := self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive := false.

	interruptedProcess isTerminated ifFalse: [
		interruptedProcess resolvePendingQuickReturns.
		interruptedProcessUI isNil
			ifTrue: [
				interruptedProcess resume ]
			ifFalse: [
				interruptedProcessUI isConsistentWithTarget
					ifTrue: [
						interruptedProcessUI beMainCanvasOfWorld.
						UISupervisor isUIProcessRunning
							ifTrue: [ mustTerminateActive := true]
							ifFalse: [ UISupervisor stopUIProcess].
						UISupervisor uiProcess: interruptedProcess ]
					ifFalse: [
						"The Display has changed size while running the debugger.
						The interupted process would use an invalid canvas, corrupting the Display, likely
						crashing the system. Don't resume the process. Just keep the current UI process running."
						interruptedProcess terminate ]]].

	"Cleanup"
	interruptedProcess := nil.
	interruptedProcessUI := nil.
	contextStackIndex := 0.
	contextStack := nil.
	contextStackTop := nil.
	receiverInspector := nil.
	contextVariablesInspector := nil.
	worldIfMustRedisplay ifNotNil: [ :w |
		[UISupervisor whenUIinSafeState: [ w invalidateBounds ]]
			forkAt: Processor userSchedulingPriority-1 ].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, and we just resumed the old one,
	then terminate us and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ].
]

{ #category : 'context stack menu' }
Debugger >> returnValue: expression [
	"Force a return of a given value to the previous context!"

	| previous selectedContext value |
	contextStackIndex = 0 ifTrue: [^Smalltalk beep].
	selectedContext := self selectedContext.
	value := Compiler new 
				evaluate: expression
				in: selectedContext
				to: selectedContext receiver.
	previous := selectedContext sender.
	self resetContext: previous.
	interruptedProcess popTo: previous value: value
]

{ #category : 'context stack menu' }
Debugger >> runToCursorAt: aPositionToStop [

	| runToCursorContext |
	
	runToCursorContext := self selectedContext home.
	self assert: runToCursorContext mapsPcTo: aPositionToStop.
	
	[ (self pcRange includes: aPositionToStop) 
		ifTrue: [ self stepIntoBlock ]
		ifFalse: [ self doStep ].
	self isPcAt: aPositionToStop in: runToCursorContext ] whileFalse
]

{ #category : 'code pane menu' }
Debugger >> runToSelection: selectionInterval [
	| currentContext |
	self pc first >= selectionInterval first ifTrue: [ ^self ].
	currentContext := self selectedContext.
	[ currentContext == self selectedContext and: [ self pc first < selectionInterval first ] ] whileTrue: [ self doStep ].
]

{ #category : 'class list' }
Debugger >> selectedClass [
	"Answer the class in which the currently selected context's method was found."

	^self selectedContext ifNotNil: [ :ctx |
		(#(doesNotUnderstand: halt halt:) statePointsTo: ctx selector)
			ifTrue: [ctx receiver class]
			ifFalse: [ctx methodClass ]]
]

{ #category : 'private' }
Debugger >> selectedContext [
	contextStackIndex = 0
		ifTrue: [^contextStackTop]
		ifFalse: [^contextStack ifNotNil: [ :aContextStack | aContextStack at: contextStackIndex]]
]

{ #category : 'context stack (message list)' }
Debugger >> selectedMessage [
	"Answer the source code of the currently selected context."
	^self selectedContext debuggerMap sourceText
]

{ #category : 'context stack (message list)' }
Debugger >> selectedMessageName [
	"Answer the message selector of the currently selected context.
	 If the method is unbound we can still usefully answer its old selector."

	| selector |
	selector := (self selectedContext ifNil: [ ^nil ]) selector.
	^(selector ~~ self selectedContext method selector
	    and: [selector beginsWith: 'DoIt'])
		ifTrue: [self selectedContext method selector]
		ifFalse: [selector]
]

{ #category : 'context stack (message list)' }
Debugger >> selector: oldSelector refactoredTo: newSelector [

	self selectedMessageName = oldSelector ifTrue: [ self down ].
	self currentMethodRefactored 
]

{ #category : 'context stack menu' }
Debugger >> send [
	"Send the selected message in the accessed method, and take control in 
	the method invoked to allow further step or send.
	Button [Into] in the Debugger."

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].

	self checkContextSelection.
	interruptedProcess step: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.

]

{ #category : 'method creation' }
Debugger >> shouldBeAbleToCreateMethod [

	^self wasInterrupedOnDoesNotUnderstand or: [ self wasInterruptedOnSubclassResponsibility]
]

{ #category : 'context stack menu' }
Debugger >> stepIntoBlock [
	"Send messages until you return to the present method context.
	 Used to step into a block in the method.
	Button [Through] in the Debugger."

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].
	"It is actually suspended, not terminated!"
	interruptedProcess name = Debugger debuggingTerminatedProcessName ifTrue: [
		^self inform: 'Debugging terminated'].

	self selectedContext method isQuick ifTrue: [ ^self doStep ].

	(self selectedContext = interruptedProcess suspendedContext and: [
			interruptedProcess suspendedContext willReturn and: [
				interruptedProcess suspendedContext closure isNil]])
		ifTrue: [
			^self doStep ].

	self 
		handleLabelUpdatesIn: [
			interruptedProcess stepToHome: self selectedContext ]
		whenExecuting: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.
]

{ #category : 'initialization' }
Debugger >> stepOutBreakpointMethods [

	[ self isOnBreakingMethodWrapper ]
		whileTrue: [ self send ].

	[self isOnHaltMethod ]
		whileTrue: [ self send. self down ].

]

{ #category : 'breakpoints' }
Debugger >> toggleBreakOnEntry [
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self ].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[ selectedMethod uninstall ]
		ifFalse:
			[ (BreakingMethodWrapper on: selectedMethod) install ].
	self changed: #annotation
]

{ #category : 'context stack (message list)' }
Debugger >> toggleContextStackIndex: anInteger  [
	"If anInteger is the same as the index of the selected context, deselect it, and select the top context. 
	Otherwise, the context whose index is anInteger becomes the selected context."

	self contextStackIndex: 
		(contextStackIndex = anInteger
			ifTrue: [1]
			ifFalse: [anInteger max: 1])
		oldContextWas:
		(contextStackIndex = 0
			ifTrue: [nil]
			ifFalse: [contextStack at: contextStackIndex])
]

{ #category : 'context stack menu' }
Debugger >> up [
	"move up the context stack to the next (enclosed) context"

	contextStackIndex > 1 ifTrue: [self toggleContextStackIndex: contextStackIndex-1]
]

{ #category : 'dependents access' }
Debugger >> updateInspectors  [
	"Update the inspectors on the receiver's variables."

	receiverInspector
		ifNotNil: [receiverInspector update].
	contextVariablesInspector
		ifNotNil: [contextVariablesInspector update]
]

{ #category : 'dependents access' }
Debugger >> wantsSteps [
 
	^ true
]

{ #category : 'method creation' }
Debugger >> wasInterrupedOnDoesNotUnderstand [

	^self interruptedContext selector == #doesNotUnderstand:
]

{ #category : 'method creation' }
Debugger >> wasInterruptedOnOverridableMethod [
	| methodClass receiverClass |

	self interruptedContext sender ifNotNil:
		[ :senderContext |
			senderContext selector == #shouldBeImplemented ifTrue: 
				[ ^false ]].

	methodClass := self interruptedContext method methodClass.
	receiverClass := self interruptedContext receiver class.

	^ methodClass ~= receiverClass
]

{ #category : 'method creation' }
Debugger >> wasInterruptedOnSubclassResponsibility [

	^self interruptedContext sender ifNil: [ false ] ifNotNil: [ :senderContext | senderContext selector == #subclassResponsibility ]
]

{ #category : 'context stack menu' }
Debugger >> where [
	"Select the expression whose evaluation was interrupted."

	self contextStackIndex: contextStackIndex oldContextWas: self selectedContext

]

{ #category : 'initialization' }
Debugger >> windowIsClosing [
	"My window is being closed.
	Terminate the process being debugged.
	Clean up. Restart the low space watcher."

	interruptedProcess
		ifNil: [^ self].
	interruptedProcess terminate.
	interruptedProcess := nil.
	interruptedProcessUI := nil.
	contextStackIndex := 0.
	contextStack := nil.
	contextStackTop := nil.
	receiverInspector := nil.
	contextVariablesInspector := nil.
	Smalltalk installLowSpaceWatcher.  "restart low space handler"

]
