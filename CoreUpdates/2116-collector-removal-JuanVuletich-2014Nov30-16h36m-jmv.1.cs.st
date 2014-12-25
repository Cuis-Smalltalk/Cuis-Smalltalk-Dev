'From Cuis 4.2 of 25 July 2013 [latest update: #2111] on 30 November 2014 at 4:42:58.83223 pm'!

!Object methodsFor: 'tracing' stamp: 'jmv 11/30/2014 16:42'!
inboundPointersExcluding: objectsToExclude
"Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself, especially method and block contexts. Adapted from PointerFinder class >> #pointersTo:except:"

	| object lastObject pointers objectsToAlwaysExclude |
	Smalltalk garbageCollect.
	"Do this to get rid of just created MethodContext instance."
	Smalltalk primitiveGarbageCollect.
	lastObject _ Object new.
	"big collection shouldn't grow, so it's contents array is always the same"
	pointers := OrderedCollection new: 1000.
	"#allObjectsDo: and #pointsTo: are expanded inline to keep spurious
	 method and block contexts out of the results"
	object := self someObject.
	[lastObject == object] whileFalse: [
		object isInMemory
			ifTrue: [((object statePointsTo: self)
				or: [object class == self])
					ifTrue: [pointers add: object]].
		object := object nextObject].

	objectsToAlwaysExclude := {
		pointers instVarNamed: 'array'.
		thisContext.
		thisContext sender.
		thisContext sender sender.
		objectsToExclude.
	}.

	^ pointers removeAllSuchThat: [ :ea |
		(objectsToAlwaysExclude identityIncludes: ea)
			or: [ objectsToExclude identityIncludes: ea ]]! !

!methodRemoval: OrderedCollection #collector!
OrderedCollection removeSelector: #collector!

!Object reorganize!
('Breakpoint' break)
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize customizeExplorerContents size yourself)
('as yet unclassified' revisar)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = closeTo: hash literalEqual: ~=)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' clone copy copyForClipboard copyFrom: copySameFrom: copyTwoLevel postCopy shallowCopy)
('events-old protocol' addDependent: breakDependents changed changed: removeDependent: update:)
('error handling' assert: caseError confirm: confirm:orCancel: doesNotUnderstand: error: halt halt: handles: notify:at: notifyWithLabel: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: nominallyUnsent: print printOn: printString printStringLimitedTo: printWithClosureAnalysisOn: storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('stepping' stepAt:in: wantsSteps)
('testing' is: isArray isBehavior isBlock isClosure isCollection isComplex isFloat isFraction isHeap isInteger isInterval isMethodProperties isNumber isPoint isPseudoContext isSequenceableCollection isString isSymbol isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' autoCompleterClass browseClassHierarchy editorClass explore hasContentsInExplorer inform: notYetImplemented notify:)
('private' errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
!

