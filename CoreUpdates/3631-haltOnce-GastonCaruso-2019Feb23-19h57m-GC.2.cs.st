'From Cuis 5.0 of 7 November 2016 [latest update: #3610] on 23 February 2019 at 8:07:26 pm'!

!Object methodsFor: 'debugging-haltOnce' stamp: 'GC 2/23/2019 20:05:35'!
clearHaltOnce
	"Turn on the halt once flag."
	
	Smalltalk at: #HaltOnce put: false! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'GC 2/23/2019 20:05:43'!
haltOnce
	"Halt unless we have already done it once."
	
	self haltOnceEnabled ifTrue: [
		self clearHaltOnce.
		^ self halt
	]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'GC 2/23/2019 20:05:48'!
haltOnceEnabled

	^ Smalltalk
		at: #HaltOnce
		ifAbsent: [false]! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'GC 2/23/2019 20:05:52'!
setHaltOnce
	"Turn on the halt once flag."
	
	Smalltalk at: #HaltOnce put: true! !

!Object methodsFor: 'debugging-haltOnce' stamp: 'GC 2/23/2019 20:00:03'!
toggleHaltOnce
	self haltOnceEnabled
		ifTrue: [self clearHaltOnce]
		ifFalse: [self setHaltOnce]! !


!Object reorganize!
('Breakpoint' break)
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize customizeExplorerContents rawBasicAt: rawBasicAt:put: size yourself)
('as yet unclassified' revisar)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = ~= hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('events-old protocol' addDependent: breakDependents changed changed: removeDependent: update:)
('error handling' assert: assert:description: caseError confirm: confirm:orCancel: doesNotUnderstand: error: halt halt: handles: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isClosure isCollection isComplex isFloat isFraction isInteger isInterval isMethodProperties isNumber isPoint isPseudoContext isString isSymbol isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy explore hasContentsInExplorer inform: notYetImplemented notify:)
('private' errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('debugging-haltOnce' clearHaltOnce haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
!

