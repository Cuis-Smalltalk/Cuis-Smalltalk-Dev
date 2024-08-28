'From Cuis7.1 [latest update: #6648] on 28 August 2024 at 11:18:45 am'!

!PluggableMorph methodsFor: 'updating' stamp: 'jmv 8/28/2024 09:38:43'!
update: aspectSymbol
	"Receive a change notice from an object we are observing. 
	The default behavior is to do nothing.
	Subclasses should act appropriately on the aspects they are interested in."

	^self! !


!Object methodsFor: 'dependency mechanism' stamp: 'jmv 8/28/2024 10:02:19'!
addDependent: anObserver
	"Make the given object one of the receiver's observers (dependents).

	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.

	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."

	self
		when: #changed:
		send: #update:
		to: anObserver.
	^anObserver.! !

!Object methodsFor: 'dependency mechanism' stamp: 'jmv 8/28/2024 09:46:00'!
breakDependents
	"Remove all of the receiver's observers (dependents)."

	self removeActionsForEvent: #changed:! !

!Object methodsFor: 'dependency mechanism' stamp: 'jmv 8/28/2024 09:46:33'!
changed: aspectSymbol
	"Receiver changed. The change is denoted by the argument. 
	Usually the argument is a Symbol that is part of the observer's change 
	protocol. Inform all of the observers.
	Note: In Smalltalk-80 observers were called 'dependents'."

	self 
		triggerEvent: #changed:
		with: aspectSymbol! !

!Object methodsFor: 'dependency mechanism' stamp: 'jmv 8/28/2024 09:48:00'!
removeDependent: anObject
	"Remove the given object as one of the receiver's observers (dependents)."

	self 
		removeActionsWithReceiver: anObject
		forEvent: #changed:.
	^ anObject! !

!Object methodsFor: 'events-registering' stamp: 'rww 12/30/2002 10:37'!
when: anEventNameSymbol
send: aMessageSelector
to: anObserver
	"Make the given object one of the receiver's observers (dependents).
	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.
	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."
 
    self
        when: anEventNameSymbol
        evaluate: (WeakMessageSend
            receiver: anObserver
            selector: aMessageSelector)! !

!Object methodsFor: 'events-registering' stamp: 'rww 12/30/2002 10:37'!
when: anEventNameSymbol
send: aMessageSelector
to: anObserver
with: messageArgument
	"Make the given object one of the receiver's observers (dependents).
	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.
	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."
 
    self
        when: anEventNameSymbol
        evaluate: (WeakMessageSend
            receiver: anObserver
            selector: aMessageSelector
		arguments: (Array with: messageArgument))! !

!Object methodsFor: 'events-registering' stamp: 'rww 12/30/2002 10:37'!
when: anEventNameSymbol
send: aMessageSelector
to: anObserver
withArguments: messageArgumentsArray
	"Make the given object one of the receiver's observers (dependents).
	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.
	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."
 
    self
        when: anEventNameSymbol
        evaluate: (WeakMessageSend
            receiver: anObserver
            selector: aMessageSelector
		arguments: messageArgumentsArray)! !

!Object methodsFor: 'object serialization' stamp: 'jmv 8/28/2024 09:43:35'!
storeDataOn: aDataStream
	"Store myself on a DataStream.  Answer self.  This is a low-level DataStream/ReferenceStream method. See also objectToStoreOnDataStream.  NOTE: This method must send 'aDataStream beginInstance:size:' and then (nextPut:/nextPutWeak:) its subobjects.  readDataFrom:size: reads back what we write here."
	| cntInstVars cntIndexedVars |

	cntInstVars := self class instSize.
	cntIndexedVars := self basicSize.
	aDataStream
		beginInstance: self class
		size: cntInstVars + cntIndexedVars.
	1 to: cntInstVars do:
		[:i | aDataStream nextPut: (self instVarAt: i)].

	"Write fields of a variable length object.  When writing to a dummy 
		stream, don't bother to write the bytes"
	((aDataStream byteStream class == DummyStream) and: [self class isBits]) ifFalse: [
		self class isWeak
			ifTrue: [
				"For weak classes (for example ActiveModel's ActionMaps) write the referenced object only
				if referenced from elsewhere in the dumped object graph.
				This means, for instance that if we only dump a model, no observers are stored, 
				but if we store a view (i.e. a Morph), it is properly handled as an observer after the object graph is revived."
				1 to: cntIndexedVars do: [ :i |
					aDataStream nextPutWeak: (self basicAt: i)]]
			ifFalse: [
				1 to: cntIndexedVars do: [ :i |
					aDataStream nextPut: (self basicAt: i)]]]! !

!Object methodsFor: 'updating' stamp: 'ST-80 5/31/1983 9:10:35'!
changed
	"Receiver changed in a general way. Inform all of the observers by sending
	them an #update: message.
	Note: In Smalltalk-80 observers were called 'dependents'."

	self changed: self! !

!Object methodsFor: 'updating' stamp: 'ST-80 5/31/1983 9:10:35'!
update: aspectSymbol
	"Receive a change notice from an object we are observing. 
	The default behavior is to do nothing.
	Subclasses should act appropriately on the aspects they are interested in."

	^self! !


!Object reorganize!
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize rawBasicAt: rawBasicAt:put: size yourself)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = ~= hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('error handling' assert: assert:description: caseError deny: doesNotUnderstand: error: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('dependency mechanism' addDependent: breakDependents changed: removeDependent:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard customizeExplorerContents explore hasContentsInExplorer inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: longPrintText longPrintTextLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isByteString isCharacter isClosure isCollection isComplex isContext isFloat isFloatOrFloatComplex isFraction isInteger isInterval isMethodProperties isNumber isObject isPoint isPseudoContext isString isSymbol isUnicodeString isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel:)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('updating' changed update:)
('debugging-halt' clearHaltOnce halt halt: haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
('as yet unclassified' run:with:in:)
!

