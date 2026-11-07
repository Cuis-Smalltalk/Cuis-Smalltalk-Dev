'From Cuis7.9 [latest update: #8339] on 7 November 2026 at 10:36:13 am'!

!Debugger methodsFor: 'controls' stamp: 'jmv 7/Nov/2026, 1:35:35 pm (UTC)'!
whatToShowSubmenuSpec
	" 'What to Show' SubMenu"
	^{
		{3010. 	'-----'}.
		{3020.	 self showingSourceString. 		nil. 	#codeProvider. 	#showSource. 				nil. 'the textual source code as written'}.
	}.
! !


!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess isNotifier labelString labelString: receiver receiverInspector resetToSelectedContextWith:)
('auto complete' classOfThisContext computeMessageEntriesIn:ofBlockArgNamed: computeMessageEntriesIn:ofBlockTempVarNamed: computeMessageEntriesIn:ofInstVarNamed: computeMessageEntriesIn:ofTempVarNamed:)
('breakpoints' toggleBreakOnEntry)
('class list' selectedClass)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('contents' currentMethodRefactored)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName selector:refactoredTo: toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: assert:mapsPcTo: doStep down fullStack handleLabelUpdatesIn:whileExecuting: invalidPositionErrorDescription isPcAt:in: peelToFirst proceed restart returnValue: runToCursorAt: send stepIntoBlock up where)
('controls' whatToShowSubmenuSpec)
('dependents access' updateInspectors wantsSteps)
('initialization' openFullAt: openFullMorphicLabel: openWindow2Label:usePreDebugWindow:preDebugMessage: openWindowLabel:usePreDebugWindow:preDebugMessage: stepOutBreakpointMethods windowIsClosing)
('method creation' askForSuperclassOf:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:withCaption:ifCancel: createMethod createMethodOnDoesNotUnderstand createMethodOnSubclassResponsibility implement:inCategory:fromClass:context: implement:inClass:context: overrideMethodOnSubclass shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnOverridableMethod wasInterruptedOnSubclassResponsibility)
('shout styling' aboutToStyleWith: allowsStylingFor:isTextDirty: bindingNamesDo: hasBindingOf:)
('testing' is: isEditingMethod isOnBreakingMethodWrapper isOnHaltMethod)
('private' checkContextSelection contextStackIndex:oldContextWas: fixReceiverInspector newStack: process:context: resetContext: resumeProcess selectedContext)
!

