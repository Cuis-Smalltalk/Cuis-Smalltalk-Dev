'From Cuis7.3 [latest update: #7115] on 29 April 2025 at 4:51:16 pm'!

!ProtoObject methodsFor: 'minimalKernel - Bootstrap' stamp: 'jmv 4/29/2025 16:46:57'!
error: aString 
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"

	| context count |
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Error: ';
		nextPutAll: aString; newLine;
		nextPutAll: '------ Call Stack: '; newLine;
		flush.

	context := thisContext sender.
	count := 0.
	[context notNil and: [ count < 10 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit Cuis now.';
		newLine;
		flush.
	Smalltalk quitPrimitive: 128.! !

!ProtoObject methodsFor: 'minimalKernel - Bootstrap' stamp: 'jmv 4/29/2025 16:50:28'!
warning: aString
	"May be redefined."
	"
	7  warning: 'El totón!!'
	"
	StdIOWriteStream stdout
		nextPutAll: 'Warning: ';
		nextPutAll: aString; newLine;
		flush.! !


!MessageCatcher methodsFor: 'error handling' stamp: 'jmv 4/29/2025 16:42:59'!
error: aString 
	"Throw a generic Error exception."

	^Error new signal: aString! !


!BreakingMethodWrapper methodsFor: 'error handling' stamp: 'jmv 4/29/2025 16:44:05'!
error: aString 
	"Throw a generic Error exception."

	^Error new signal: aString! !


!ProtoObject methodsFor: 'minimalKernel - Bootstrap' stamp: 'jmv 4/29/2025 16:40:58'!
doesNotUnderstand: aMessage
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"
	| context count |
	StdIOWriteStream stdout flush.
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Message not understood.'; newLine;
		nextPutAll: 'Receiver: '.

	self class isMeta
		ifFalse: [ StdIOWriteStream stdout nextPutAll: 'a ' ]
		ifTrue: [ StdIOWriteStream stdout nextPutAll: 'class ' ].
	self class theNonMetaClass printNameOn: StdIOWriteStream stdout.

	StdIOWriteStream stdout
		nextPutAll: ' selector: #';
		nextPutAll: aMessage selector; newLine;
		nextPutAll: '------ Call Stack: '; newLine;
		flush.

	context := thisContext sender.
	count := 0.
	[context notNil and: [ count < 10 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit now.';
		newLine;
		flush.

	Smalltalk quitPrimitive: 127.! !


!ProtoObject reorganize!
('minimalKernel - Bootstrap' == ~~ become: doesNotUnderstand: error: identityHash initialize isNil notNil warning:)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isObject)
('comparing' scaledIdentityHash)
('system primitives' cannotInterpret: nextInstance nextObject statePointsTo:)
('objects from disk' rehash)
('debugging' flag: withArgs:executeMethod:)
('apply primitives' tryPrimitive:withArgs:)
('tracing' pointsTo:)
!


!MessageCatcher reorganize!
('accessing' privAccumulator privAccumulator:)
('error handling' doesNotUnderstand: error:)
!


!Object reorganize!
('minimalKernel - Bootstrap' = ~= at: at:put: basicAt: basicSize class hash is: isBehavior isByteString isCharacter isInteger isInterval isMethodProperties isNumber isString isSymbol isUnicodeString perform: perform:with: printOn: value)
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
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform:with:with: perform:with:with:with: perform:with:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: longPrintText longPrintTextLimitedTo: nominallyUnsent: print printAs:limitedTo: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' become: becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: run:with:in: someObject)
('stepping' stepAt: wantsSteps)
('testing' isArray isBlock isClosure isCollection isCompiledCode isComplex isContext isFloat isFloatOrFloatComplex isFraction isObject isPoint isPseudoContext isVariableBinding name renameTo: shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel: request:initialAnswer:verifying:orCancel:)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand flash runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('updating' changed update:)
('debugging-halt' clearHaltOnce halt halt: haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
('literals' allLiteralsDo: allLiteralsDo:scanForSpecial: hasLiteral: hasLiteralSuchThat: hasLiteralSuchThat:scanForSpecial:)
!


!InnerTextMorph reorganize!
('accessing' askBeforeDiscardingEdits: codeProvider contents: contentsAsIs: crAction crAction: disableEditing editor enableEditing escAction escAction: hasEditingConflicts hasEditingConflicts: isWrapped keystrokeAction: model: model:wrappedTo: positionInTextOf: textColor textColor: textProviderOrModel wrapFlag:)
('anchors' anchorMorph:at:)
('caching' releaseCachedState)
('drawing' debugDrawLineRectsOn: drawOn:)
('editing' acceptContents acceptOnCR confirmAcceptAnyway enterClickableRegion:localPosition: handleInteraction: hasUnacceptedEdits: model)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: keyStroke: keyboardShortcut: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('event handling testing' disablesEditing handlesKeyboard handlesMouseDown: tabToFocus tabToFocus:)
('event handling' keyboardFocusChange: processKeystrokeEvent:)
('events-processing' processMouseMove:localPosition:)
('geometry' adjustExtent extentChanged: minimumExtent privateExtent:)
('initialization' defaultColor initialize)
('menu' addCustomMenuItems:hand: menuNshortcutsSpecsDo: wrapOnOff wrapString)
('submorphs-add/remove' addMorphFrontFromWorldPosition:)
('private' autoCompleterClass: extentForComposing fit installEditorAndTextComposition mutex releaseEditorAndTextComposition removedMorph: resetTextComposition selectionChanged stylerClass: textComposition updateFromTextComposition)
('notifications' fontPreferenceChanged possiblyChanged someSubmorphPositionOrExtentChanged)
('testing' canDiscardEdits hasUnacceptedEdits)
('blinking cursor' onBlinkCursor pauseBlinking showsBlinkingCursor startBlinking stopBlinking)
('accept/cancel' acceptOnAny: acceptOnCR:)
('miscellaneous' disregardUnacceptedEdits flash selectAll)
('shout' convertAndStyleIfNeeded stylerStyled)
('selection' scrollSelectionIntoView setSelection:)
('classification' is:)
('copying' okayToDuplicate)
!


!BreakingMethodWrapper reorganize!
('system primitives' doesNotUnderstand: run:with:in:)
('printing' printAs:limitedTo: printOn: printStringLimitedTo: printTextLimitedTo:)
('install/uninstall' install uninstall)
('breakpoints' hasBreakpoint)
('initialization' initializeOn:)
('class membership' class)
('error handling' error:)
!

