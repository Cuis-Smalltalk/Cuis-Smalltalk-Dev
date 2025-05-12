'From Cuis7.3 [latest update: #7115] on 29 April 2025 at 4:19:23 pm'!

!Object methodsFor: 'system primitives' stamp: 'jmv 4/29/2025 16:16:23'!
become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				| contextReceiver |
				contextReceiver := context receiver.
				(self == contextReceiver or: [ otherObject == contextReceiver ]) ifTrue: [
					| selector selfMethod otherObjectMethod |
					selector := context method selector.
					selfMethod := self class lookupSelector: selector.
					otherObjectMethod := otherObject class lookupSelector: selector.
					selfMethod = otherObjectMethod ifFalse: [
						| errorMessage |
						errorMessage := contextReceiver class name, ' has some instance running #', selector, 
							'. This CompiledMethod would become invalid.'.
						errorMessage print.
						MethodInCallStackToBecomeInvalid signal ifFalse: [
							'#become: aborted.' print.
							Error signal: errorMessage, ' #become: aborted. Please see Transcript.'.
							^nil ]]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsExchangeIdentityWith: { otherObject }! !


!ProtoObject methodsFor: 'minimalKernel - Bootstrap' stamp: 'jmv 4/29/2025 16:17:57'!
become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger.
	This implementation is appropriate for minimal bootstrapped images.
	Regular images will automatically use the implementation in Object (if present)."

	{ self } elementsExchangeIdentityWith: { otherObject }! !


!ProtoObject reorganize!
('minimalKernel - Bootstrap' == ~~ become: identityHash initialize isNil notNil)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isObject)
('comparing' scaledIdentityHash)
('system primitives' cannotInterpret: doesNotUnderstand: nextInstance nextObject statePointsTo:)
('objects from disk' rehash)
('debugging' flag: withArgs:executeMethod:)
('apply primitives' tryPrimitive:withArgs:)
('tracing' pointsTo:)
!


!Object reorganize!
('minimalKernel - Bootstrap' = ~= at: at:put: basicAt: basicSize class hash is: isBehavior isByteString isCharacter isInteger isInterval isMethodProperties isNumber isString isSymbol isUnicodeString perform: perform:with: value)
('accessing' addInstanceVarNamed:withValue: basicAt:put: rawBasicAt: rawBasicAt:put: size yourself)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' isKindOf: isMemberOf: respondsTo:)
('comparing' literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('error handling' assert: assert:description: caseError deny: doesNotUnderstand: error: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage shouldNotImplement subclassResponsibility)
('evaluating' valueWithArguments: valueWithPossibleArgument:)
('dependency mechanism' addDependent: breakDependents changed: removeDependent:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor finalizationRegistry finalize mourn retryWithGC:until: toFinalizeSend:to: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard customizeExplorerContents explore hasContentsInExplorer inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform:with:with: perform:with:with:with: perform:with:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: longPrintText longPrintTextLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' become: becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: run:with:in: someObject)
('stepping' stepAt: wantsSteps)
('testing' isArray isBlock isClosure isCollection isCompiledCode isComplex isContext isFloat isFloatOrFloatComplex isFraction isObject isPoint isPseudoContext isVariableBinding name renameTo: shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel: request:initialAnswer:verifying:orCancel:)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('updating' changed update:)
('debugging-halt' clearHaltOnce halt halt: haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
('literals' allLiteralsDo: allLiteralsDo:scanForSpecial: hasLiteral: hasLiteralSuchThat: hasLiteralSuchThat:scanForSpecial:)
!

