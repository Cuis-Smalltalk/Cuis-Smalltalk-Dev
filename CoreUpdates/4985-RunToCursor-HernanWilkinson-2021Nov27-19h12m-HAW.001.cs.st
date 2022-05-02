'From Cuis 5.0 [latest update: #4972] on 27 November 2021 at 8:57:55 pm'!

!Debugger methodsFor: 'context stack menu' stamp: 'HAW 11/27/2021 20:47:57'!
assert: runToCursorContext mapsPcTo: aPosition

	(runToCursorContext mapsPcTo: aPosition) ifFalse: [ self error: self invalidPositionErrorDescription. ]! !

!Debugger methodsFor: 'context stack menu' stamp: 'HAW 11/27/2021 20:38:24'!
invalidPositionErrorDescription

	^'Invalid position'! !

!Debugger methodsFor: 'context stack menu' stamp: 'HAW 11/27/2021 20:56:06'!
isPcAt: aPosition in: aContext

	^ aContext isDead or: [ self pcRange includes: aPosition ]! !

!Debugger methodsFor: 'context stack menu' stamp: 'HAW 11/27/2021 20:56:06'!
runToCursorAt: aPositionToStop

	| runToCursorContext |
	
	runToCursorContext := self selectedContext home.
	self assert: runToCursorContext mapsPcTo: aPositionToStop.
	
	[ self stepIntoBlock.
	self isPcAt: aPositionToStop in: runToCursorContext ] whileFalse! !


!InstructionStream methodsFor: 'testing' stamp: 'HAW 11/27/2021 20:37:08'!
mapsPcTo: aPosition

	^self debuggerMap mapsPcTo: aPosition! !


!TextModelMorph methodsFor: 'editor access' stamp: 'HAW 11/27/2021 20:44:23'!
startIndex
	
	^self editor startIndex ! !


!DebuggerMethodMap methodsFor: 'testing' stamp: 'HAW 11/27/2021 20:37:08'!
mapsPcTo: aPosition

	^self abstractSourceMap anySatisfy: [:aRange | aRange includes: aPosition ] ! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'HAW 11/27/2021 19:13:00'!
customButtonSpecs
	"Answer an array of elements of the form wording, selector, help-message, that characterize the custom button row of a debugger."

	^#(
		('Restart'		restart				'reset this context to its start.')
		('Into'			send					'step Into message sends')
		('Over'			doStep				'step Over message sends')
		('Through'		stepIntoBlock		'step into a block')
		('Run to Cursor'		runToCursor		'run to cursor')
		('Full Stack'	fullStack			'show full stack')
		('Where'		where				'select current pc range')
		('Create'		createMethod		'create method'))! !

!DebuggerWindow methodsFor: 'context stack menu' stamp: 'HAW 11/27/2021 20:43:14'!
runToCursor

	^ self ifOkToChangeCodePaneDo: [ 
		[model runToCursorAt: codePane startIndex ]
			on: [ :anError | anError messageText = model invalidPositionErrorDescription ]
			do: [ :anError | self inform:  'Invalid cursor position. Put the cursor in a message send to run until it' ]]! !


!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context: resetToSelectedContextWith:)
('auto complete' classOfThisContext computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('breakpoints' toggleBreakOnEntry)
('class list' selectedClass)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('contents' currentMethodRefactored)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: assert:mapsPcTo: doStep down fullStack handleLabelUpdatesIn:whenExecuting: invalidPositionErrorDescription isPcAt:in: peelToFirst proceed restart returnValue: runToCursorAt: selectPC send stepIntoBlock up where)
('dependents access' updateInspectors wantsSteps)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:withCaption:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUnderstand implement:inClass:context: overrideMethodOnSubclass shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnOverridableMethod wasInterruptedOnSubclassResponsibility)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('testing' is: isEditingMethod)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: fixReceiverInspector lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
!


!InstructionStream reorganize!
('testing' mapsPcTo: willBlockReturn willJump willJumpIfFalse willJumpIfTrue willJustPop willReallyStore willReturn willReturnTopFromMethod willSend willStore willStorePop)
('decoding' atEnd interpret interpretJump interpretJumpIfCond interpretNextInstructionFor: skipCallPrimitive)
('scanning' addSelectorTo: firstByte followingByte followingBytecode followingPc fourthByte method nextByte nextInstruction pc peekInstruction previousPc scanFor: secondByte selectorToSendOrSelf skipBackBeforeJump thirdByte)
('private' interpretExtension:in:for: method:pc: nextPc: pc: unusedBytecode:at:)
('debugger access' abstractPC debuggerMap)
('decoding - private - v3 plus closures' interpretNextV3ClosuresInstructionFor: interpretV3ClosuresExtension:in:for: interpretV3Extension:in:for: interpretV3Jump interpretV3JumpIfCond)
('as yet unclassified' movePcForward)
!

