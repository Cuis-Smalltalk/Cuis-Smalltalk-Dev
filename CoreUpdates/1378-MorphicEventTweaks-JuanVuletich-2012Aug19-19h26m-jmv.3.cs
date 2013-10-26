'From Cuis 4.0 of 21 April 2012 [latest update: #1377] on 19 August 2012 at 9:32:33 pm'!

!MorphicEvent commentStamp: '<historical>' prior: 0!
This class represents the base for all Morphic events.

Instance variables:
	stamp	<Integer>	The millisecond clock time stamp (based on Time millisecondClock)
	source	<Hand | nil>	If non-nil the hand that generated the event.!


!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:44'!
handleDropMorph: anEvent
	"Handle a dropping morph."
	| aMorph localPt |
	aMorph _ anEvent contents.
	"Do a symmetric check if both morphs like each other"
	((self wantsDroppedMorph: aMorph event: anEvent)	"I want her"
		and: [aMorph wantsToBeDroppedInto: self])		"she wants me"
			ifFalse: [
				^ self].
	anEvent wasHandled: true.
	localPt _ aMorph referencePosition.
	aMorph referencePosition: localPt.
	self acceptDroppingMorph: aMorph event: anEvent.
	aMorph justDroppedInto: self event: anEvent.
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:44'!
handleFocusEvent: aMorphicEvent
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."

	^aMorphicEvent sentTo: self! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:44'!
handleKeyDown: anEvent
	"System level event handling."

	anEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	anEvent wasHandled: true.
	^self keyDown: anEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:45'!
handleKeyUp: anEvent
	"System level event handling."

	anEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	anEvent wasHandled: true.
	^self keyUp: anEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:45'!
handleKeystroke: aKeyboardEvent
	"System level event handling."

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyStroke: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:45'!
handleMouseDown: anEvent
	"System level event handling."
	anEvent wasHandled ifTrue: [ ^self ]. "not interested"
	anEvent hand removePendingBalloonFor: self.
	anEvent wasHandled: true.
	self activateWindow.

	"Make me modal during mouse transitions"
	anEvent hand newMouseFocus: self event: anEvent.
	anEvent mouseButton3Changed ifTrue: [^self mouseButton3Down: anEvent].

	self mouseDown: anEvent.
	anEvent hand removeHaloFromClick: anEvent on: self.

	(self handlesMouseStillDown: anEvent) ifTrue:[
		self startStepping: #handleMouseStillDown: 
			at: Time millisecondClockValue + self mouseStillDownThreshold
			arguments: {anEvent copy resetHandlerFields}
			stepTime: self mouseStillDownStepRate ].
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:45'!
handleMouseEnter: anEvent
	"System level event handling."
	anEvent isDraggingEvent ifTrue: [
		^self].
	self wantsBalloon ifTrue: [
		anEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: anEvent) ifTrue: [
		anEvent wasHandled: true.
		self mouseEnter: anEvent ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:46'!
handleMouseMove: anEvent
	"System level event handling."
	anEvent wasHandled ifTrue: [^self]. "not interested"
	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	(anEvent hand hasSubmorphs) ifTrue: [^self].
	(anEvent anyButtonPressed and:[anEvent hand mouseFocus == self]) ifFalse: [^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(self handlesMouseStillDown: anEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #handleMouseStillDown: 
			at: Time millisecondClockValue
			arguments: {anEvent copy resetHandlerFields}
			stepTime: 1].
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:46'!
handleMouseOver: aMorphicEvent
	"System level event handling."
	aMorphicEvent hand mouseFocus == self ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: aMorphicEvent eventPosition event: aMorphicEvent) ifFalse: [
			^self ]].
	aMorphicEvent hand noticeMouseOver: self event: aMorphicEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:46'!
handleMouseStillDown: anEvent
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	(anEvent hand mouseFocus == self) 
		ifFalse: [ ^self stopSteppingSelector: #handleMouseStillDown: ].
	self mouseStillDown: anEvent.
! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:47'!
handleMouseUp: anEvent
	"System level event handling."
	anEvent wasHandled ifTrue: [^self]. "not interested"
	anEvent hand mouseFocus == self ifFalse: [^self]. "Not interested in other parties"
	anEvent hand releaseMouseFocus: self.
	anEvent wasHandled: true.
	anEvent mouseButton3Changed
		ifTrue: [ self mouseButton3Up: anEvent ]
		ifFalse: [ self mouseUp: anEvent.
				self stopSteppingSelector: #handleMouseStillDown: ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:47'!
handleUnknownEvent: anEvent
	"An event of an unknown type was sent to the receiver. What shall we do?!!"

	Beeper beep. 
	anEvent printString displayAt: 0@0.
	anEvent wasHandled: true! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 8/19/2012 19:47'!
handleWindowEvent: anEvent
	"Handle an event concerning our host window"

	anEvent wasHandled ifTrue: [^self]. "not interested"
	(self wantsWindowEvent: anEvent) ifFalse: [^self].
	anEvent wasHandled: true.
	self windowEvent: anEvent.
! !


!Morph reorganize!
('accessing' adoptWidgetsColor: balloonText beSticky color color: isLocked isSticky lock lock: resistsRemoval sticky: toggleResistsRemoval toggleStickiness unlock unlockContents)
('accessing - extension' assureExtension hasExtension initializeExtension privateExtension: resetExtension)
('accessing - properties' hasProperty: removeProperty: setProperty:toValue: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits rotationDegrees:)
('caching' fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidRect: invalidRect:from: privateInvalidateMorph:)
('classification' isPlayfieldLike isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: altSpecialCursor0 altSpecialCursor1 altSpecialCursor2 altSpecialCursor3 altSpecialCursor3: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: changeClipSubmorphs clipSubmorphs: clippingBounds clipsSubmorphs drawDropHighlightOn: drawErrorOn: drawMouseDownHighlightOn: drawOn: drawSubmorphsOn: drawingFails drawingFailsNot fullDrawOn: hasClipSubmorphsString hide highlightForMouseDown highlightForMouseDown: highlightedForMouseDown imageForm imageForm: imageForm:forRectangle: isKnownFailing ownShadowForm refreshWorld shadowForm show visible visible:)
('dropping/grabbing' aboutToBeGrabbedBy: dragEnabled dragEnabled: dragNDropEnabled dropEnabled dropEnabled: dropHighlightColor enableDrag: enableDragNDrop enableDragNDrop: enableDrop: highlightForDrop highlightForDrop: highlightedForDrop justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: repelsMorph:event: resetHighlightForDrop separateDragAndDrop wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('event handling' click: doubleClick: handlesKeyboard handlesMouseDown: handlesMouseOver: handlesMouseStillDown: keyDown: keyStroke: keyUp: mouseDown: mouseEnter: mouseLeave: mouseMove: mouseStillDown: mouseStillDownStepRate mouseStillDownThreshold mouseUp: windowEvent:)
('events-alarms' addAlarm:after: addAlarm:at: addAlarm:with:after: addAlarm:with:at: addAlarm:with:with:after: addAlarm:with:with:at: addAlarm:withArguments:after: addAlarm:withArguments:at: alarmScheduler removeAlarm:)
('events-processing' closeWindowFor: containsPoint:event: dispatchEvent: focusKeyboardFor: handleDropMorph: handleFocusEvent: handleKeyDown: handleKeyUp: handleKeystroke: handleMouseDown: handleMouseEnter: handleMouseLeave: handleMouseMove: handleMouseOver: handleMouseStillDown: handleMouseUp: handleUnknownEvent: handleWindowEvent: rejectDropEvent: rejectsEvent:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus keyboardFocusChange:)
('geometry' basicExtent: externalizeDistanceToWorld: externalizeToWorld: innerBounds internalizeFromWorld: minimumExtent morphBoundsInWorld morphBoundsInWorld: morphExtent morphExtent: morphExtentInWorld morphFullBoundsInWorld morphHeight morphHeight: morphPosition morphPosition: morphPositionInOwner morphPositionInOwner: morphPositionInWorld morphPositionInWorld: morphWidth morphWidth: validateNotSent validateOwnerNotNil worldBoundsForHalo)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' containsPoint: fullContainsPoint: isOrthoRectangularMorph)
('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: addWorldHandlesTo:box: balloonHelpDelayTime comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloClass mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: setBalloonText:maxLineLength: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHaloHandleWithSelector:inHalo:)
('initialization' defaultBounds inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' acceptDroppingMorph:event: computeFullBounds layoutBounds layoutSubmorphs layoutSubmorphsAndComputeFullBounds someSubmorphPositionOrExtentChanged submorphBounds)
('layout-properties' layoutSpec layoutSpec:)
('macpal' flash)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor changeDragAndDrop collapse expand exportAsBMP exportAsJPEG exportAsPNG hasDragAndDropEnabledString lockUnlockMorph lockedString maybeAddCollapseItemTo: resistsRemovalString stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: changeColorTarget:selector:originalColor:hand: copyToClipboard: dismissMorph duplicateMorph: grabMorph: maybeDuplicateMorph mouseButton3Down: mouseButton3Up: potentialEmbeddingTargets resizeFromMenu resizeMorph)
('naming' nameForFindWindowFeature)
('object serialization' objectForDataStream:)
('player' okayToDuplicate)
('printing' printOn:)
('property extension' extension)
('rotate scale and flex' rotationDegrees)
('stepping' wantsSteps)
('stepping and presenter' arrangeToStartStepping arrangeToStartSteppingIn: shouldGetStepsFrom: startStepping startStepping:at:arguments:stepTime: startSteppingSelector: step stepAt: stopStepping stopSteppingSelector:)
('structure' activeHand allOwners allOwnersDo: firstOwnerSuchThat: hasOwner: isInWorld outermostWorldMorph owner ownerThatIsA: pasteUpMorph root veryLastLeave withAllOwnersDo: world)
('submorphs-accessing' allMorphs allMorphsDo: findA: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph morphsAt: morphsAt:behind:unlocked: morphsAt:unlocked: morphsAt:unlocked:do: noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorphBack: addMorphFront: addMorphFrontFromWorldPosition: comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('submorphs-add/remove-layers' addMorphInFrontOfLayer: addMorphInLayer: morphicLayerNumber morphicLayerNumberWithin:)
('testing' hasModel isOpaqueMorph isReallyVisible shouldDropOnMouseUp stepTime)
('updating' redrawNeeded update:)
('user interface' activateWindow activateWindowAndSendTopToBack:)
('visual properties' defaultColor)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateOwner: privateRemove:)
!

