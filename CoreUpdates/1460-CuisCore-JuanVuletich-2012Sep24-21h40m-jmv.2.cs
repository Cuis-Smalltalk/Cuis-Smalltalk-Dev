'From Cuis 4.0 of 21 April 2012 [latest update: #1459] on 24 September 2012 at 9:52:33 pm'!

!Object methodsFor: 'morphic' stamp: 'jmv 9/24/2012 21:43'!
currentWorld
	"Answer a morphic world that is the current UI focus."

	"Should query all instances of world, looking for the one animated by the currently running process!! (and answer nil if this is not a Morphic process!!)"
	self flag: #jmvVer2.

	^World! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 9/24/2012 21:43'!
install
	owner := nil.	"since we may have been inside another world previously"
	ActiveHand := self hands first.	"default"
	submorphs do: [:ss | ss owner ifNil: [ss privateOwner: self]].
	"Transcript that was in outPointers and then got deleted."
	self viewBox: Display boundingBox.
	Sensor flushAllButDandDEvents.
	worldState handsDo: [:h | h initForEvents].
	self borderWidth: 0.	"default"
	SystemWindow noteTopWindowIn: self.
	self displayWorldSafely! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 9/24/2012 21:43'!
interruptNameX: labelString
	"Create a Notifier on the active scheduling process with the given label."
	| preemptedProcess projectProcess |
	
	ActiveHand ifNotNil:[ActiveHand interrupted].
	ActiveHand _ World activeHand.
	ActiveHand interrupted. "make sure this one's interrupted too"

	projectProcess _ ProjectX uiProcessX.	"we still need the accessor for a while"
	preemptedProcess _ Processor preemptedProcess.
	"Only debug preempted process if its priority is >= projectProcess' priority"
	preemptedProcess priority < projectProcess priority ifTrue:[
		projectProcess suspend.
		preemptedProcess _ projectProcess.
	] ifFalse:[
		preemptedProcess suspend.
	].
	Debugger openInterrupt: labelString onProcess: preemptedProcess
! !


!TheWorldMenu methodsFor: 'action' stamp: 'jmv 9/24/2012 21:47'!
doMenuItem: aCollection with: event
	| realTarget selector nArgs |
	selector _ aCollection second.
	nArgs _ selector numArgs.
	realTarget _ aCollection first.
	realTarget == #myWorld ifTrue: [realTarget _ myWorld].
	^nArgs = 0 
		ifTrue:[realTarget perform: selector]
		ifFalse:[realTarget perform: selector with: event].
! !


!WorldState methodsFor: 'stepping' stamp: 'jmv 9/24/2012 21:43'!
runLocalStepMethodsIn: aWorld 
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world."

	| now morphToStep stepTime |
	now _ Time millisecondClockValue.
	self triggerAlarmsBefore: now.
	stepList isEmpty 
		ifTrue: [
			^self].
	"jmv: I say it is only needed on clock rollover. See commented code below."
	"(now < lastStepTime or: [now - lastStepTime > 5000]) "
	now < lastStepTime		"clock slipped"
		ifTrue: [ self adjustWakeupTimes: now ].
	[ stepList isEmpty not and: [ stepList first scheduledTime < now ]] 
		whileTrue: [
			lastStepMessage _ stepList removeFirst.
			morphToStep _ lastStepMessage receiver.
			(morphToStep shouldGetStepsFrom: aWorld) 
				ifTrue: [
					lastStepMessage value: now.
					lastStepMessage ifNotNil: [
							stepTime _ lastStepMessage stepTime ifNil: [ morphToStep stepTime ].
							lastStepMessage scheduledTime: now + (stepTime max: 1).
							stepList add: lastStepMessage]].
			lastStepMessage _ nil].
	lastStepTime _ now.! !

!methodRemoval: PasteUpMorph #shouldGetStepsFrom:!
PasteUpMorph removeSelector: #shouldGetStepsFrom:!
!methodRemoval: ObjectExplorer #shouldGetStepsFrom:!
ObjectExplorer removeSelector: #shouldGetStepsFrom:!

!Object reorganize!
('Breakpoint' break)
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize customizeExplorerContents size yourself)
('as yet unclassified' revisar)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = closeTo: hash literalEqual: ~=)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asOrderedCollection asString asStringOrText complexContents mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' clone copy copyForClipboard copyFrom: copySameFrom: copyTwoLevel postCopy shallowCopy)
('events-old protocol' addDependent: breakDependents changed: removeDependent: update:)
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
('printing' fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: nominallyUnsent: print printOn: printString printStringLimitedTo: printWithClosureAnalysisOn: storeOn: storeString stringRepresentation)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('testing' is: isArray isBehavior isBlock isClosure isCollection isComplex isFloat isFraction isHeap isInteger isInterval isMethodProperties isNumber isPoint isPseudoContext isString isSymbol isVariableBinding name renameTo: stepAt:in: stepIn: wantsSteps wantsStepsIn:)
('translation support' inline: success: var:declareC:)
('user interface' autoCompleterClass browseClassHierarchy editorClass explore hasContentsInExplorer inform: notYetImplemented notify:)
('private' errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: species storeAt:inTempFrame:)
('tracing' exploreAllPointers explorePointers inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' currentWorld)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk removeKey: #ActiveWorld!

