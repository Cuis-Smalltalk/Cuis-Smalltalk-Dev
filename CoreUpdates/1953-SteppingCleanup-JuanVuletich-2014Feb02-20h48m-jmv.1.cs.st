'From Cuis 4.2 of 25 July 2013 [latest update: #1952] on 2 February 2014 at 9:13:32.527635 pm'!

!Object methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:12'!
stepAt: millisecondClockValue in: aWindow

	^ self step! !

!Object methodsFor: 'stepping' stamp: 'jmv 2/2/2014 20:57'!
wantsSteps
	"Overridden by morphic classes whose instances want to be stepped,
	or by model classes who want their morphic views to be stepped."

	^ false! !


!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:01'!
shouldGetStepsFrom: aWorld

	^self world == aWorld! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:01'!
startStepping
	"Start getting sent the 'step' message."
	"que feo. le digo stepAt: que lleva un argumento... Cuando hay un argumento, se le pasa automaticamente el tiempo.
	mhhhhh"

	self startStepping: #stepAt: at: Time localMillisecondClock stepTime: nil! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:01'!
startStepping: aSelector at: scheduledTime stepTime: stepTime
	"Start stepping the receiver"

	| w |
	w _ self world.
	w ifNotNil: [
		w startStepping: self at: scheduledTime selector: aSelector stepTime: stepTime.
		self redrawNeeded ]! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:01'!
startSteppingSelector: aSelector
	"Start getting sent the 'step' message.
	| m |
	m _ RectangleLikeMorph new..
	m openInWorld.
	m startSteppingSelector: [self print]
	Esto va a requerir que pueda pasar un bloque....
	"

	self startStepping: aSelector at: Time localMillisecondClock stepTime: nil! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:00'!
step
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The time between steps is specified by this morph's answer to the stepTime message.  The generic version dispatches control to the player, if any.  The nasty circumlocation about owner's transformation is necessitated by the flexing problem that the player remains in the properties dictionary both of the flex and the real morph.  In the current architecture, only the top renderer's pointer to the player should actually be honored for the purpose of firing."
! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:01'!
stepAt: millisecondClockValue
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The time between steps is specified by this morph's answer to the stepTime message.
	The millisecondClockValue parameter gives the value of the millisecond clock at the moment of dispatch.
	Default is to dispatch to the parameterless step method for the morph, but this protocol makes it possible for some morphs to do differing things depending on the clock value"
	
	self step! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:00'!
stopStepping
	"Stop getting sent the 'step' message."

	| w |
	w _ self world.
	w ifNotNil: [ w stopStepping: self ]! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:00'!
stopSteppingSelector: aSelector
	"Stop getting sent the given message."
	| w |
	w _ self world.
	w ifNotNil: [ w stopStepping: self selector: aSelector ]! !


!SystemWindow methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:11'!
wantsSteps
	"Return true if the model wants its view to be stepped.  For an open system window, we give the model to offer an opinion"

	^ model wantsSteps! !


!UpdatingMenuItemMorph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:02'!
step

	super step.
	self updateContents! !

!UpdatingMenuItemMorph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 21:03'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message"

	^true! !


!WorldState methodsFor: 'initialization' stamp: 'jmv 2/2/2014 21:05'!
stepListSortBlock

	^ [ :stepMsg1 :stepMsg2 |
		stepMsg1 scheduledTime <= stepMsg2 scheduledTime ]! !


!UpdatingMenuItemMorph reorganize!
('enablement' enablement enablementSelector:)
('stepping' step wantsSteps)
('testing' is: stepTime)
('wording' wordingArgument: wordingProvider:wordingSelector:)
('world' updateContents)
!


!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location lock resistsRemoval toggleStickiness unlock unlockContents)
('accessing - properties' hasProperty: isLocked isSticky lock: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits dragEvent:localPosition: rotationDegrees:)
('caching' fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: clipsSubmorphs drawErrorOn: drawOn: drawSubmorphsOn: drawingFails drawingFailsNot fullDrawOn: hide imageForm: imageForm:forRectangle: isKnownFailing ownShadowForm refreshWorld show visible visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseLeave: mouseMove:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' closeWindowFor: containsPoint:event: dispatchEvent:localPosition: focusKeyboardFor: handleFocusEvent: processDropMorph:localPosition: processKeyDown:localPosition: processKeyUp:localPosition: processKeystroke:localPosition: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent:localPosition: processWindowEvent:localPosition: rejectsEvent:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' clippingRect externalize: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: internalize: internalizeDistance: internalizeFromWorld: layoutBounds minimumExtent morphBoundsInWorld morphBoundsInWorld: morphExtent morphExtent: morphExtentInWorld morphFullBoundsInWorld morphHeight morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphWidth worldBoundsForHalo)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' fullContainsPoint: isOrthoRectangularMorph morphContainsPoint:)
('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' acceptDroppingMorph:event: layoutSubmorphs layoutSubmorphsIfNeeded someSubmorphPositionOrExtentChanged submorphBounds)
('layout-properties' layoutSpec layoutSpec: layoutSpecOrNil)
('macpal' flash)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor collapse expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: changeColorTarget:selector:originalColor:hand: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets resizeFromMenu resizeMorph)
('naming' label)
('object serialization' objectForDataStream:)
('player' okayToDuplicate)
('printing' printOn:)
('rotate scale and flex' rotationDegrees)
('stepping' shouldGetStepsFrom: startStepping startStepping:at:stepTime: startSteppingSelector: step stepAt: stopStepping stopSteppingSelector: wantsSteps)
('structure' allOwnersDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root veryLastLeaf withAllOwnersDo: world)
('submorphs-accessing' allMorphsDo: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph morphsAt: morphsAt:behind:unlocked: morphsAt:unlocked: morphsAt:unlocked:do: noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorphBack: addMorphFront: addMorphFrontFromWorldPosition: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isOpaqueMorph isReallyVisible stepTime)
('updating' redrawNeeded update:)
('user interface' activateWindow activateWindowAndSendTopToBack:)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateOwner: privateRemove:)
!

!methodRemoval: Object #wantsStepsIn:!
Object removeSelector: #wantsStepsIn:!

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
('morphic' runningWorld)
('*FFI-Kernel' externalCallFailed)
('*JSON' asJsonString)
!

