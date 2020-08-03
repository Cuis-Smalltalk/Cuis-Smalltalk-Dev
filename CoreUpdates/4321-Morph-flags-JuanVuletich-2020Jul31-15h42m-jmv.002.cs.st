'From Cuis 5.0 [latest update: #4319] on 31 July 2020 at 4:13:32 pm'!

!Morph commentStamp: 'jmv 7/31/2020 16:05:32' prior: 0!
A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://wiki.squeak.org/squeak/morph. 

Morphs exist in a tree, rooted at a World (generally a PasteUpMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets drawn. To hide a Morph and its submorphs, send the #visible: message. 

Structure:
instance var 				Type 						Description 
owner 				Morph		 				My parent Morph, or nil for the top-level Morph, which is a
 				or nil						world, typically a PasteUpMorph.
submorphs 				Array 						My child Morphs. 
location 				GeometryTransformation 						Specifies position (and possibly, angle of rotation and scale change) inside owner
												See comment at GeometryTransformation
extension 				MorphExtension 						Allows extra properties to be stored without adding a
				or nil  						storage burden to all morphs. 
!


!Morph methodsFor: 'accessing - flags' stamp: 'jmv 7/31/2020 16:08:29'!
isLayoutNeeded

	^ self privateFlagAt: 4! !

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 7/31/2020 16:08:33'!
isRedrawNeeded

	^ self privateFlagAt: 1! !

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 7/31/2020 16:12:00'!
isSubmorphRedrawNeeded

	^ self privateFlagAt: 2! !

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 7/31/2020 16:08:56'!
layoutNeeded: aBoolean

	^self privateFlagAt: 4 put: aBoolean! !

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 7/31/2020 16:08:43'!
needsRedraw: aBoolean

	^self privateFlagAt: 1 put: aBoolean! !

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 7/31/2020 16:11:49'!
submorphNeedsRedraw: aBoolean

	^self privateFlagAt: 2 put: aBoolean! !

!Morph methodsFor: 'private' stamp: 'jmv 7/31/2020 15:56:13'!
privateFlagAt: bitIndex
	"We can hold up to 8 1-bit flags.
	Initial value of all flags is false."

	id ifNil: [ self morphId ].
	^(id bitAt: bitIndex) = 1! !

!Morph methodsFor: 'private' stamp: 'jmv 7/31/2020 15:56:19'!
privateFlagAt: bitIndex put: aBoolean
	"We can hold up to 8 1-bit flags.
	Initial value of all flags is false."

	id ifNil: [ self morphId ].
	id _ id bitAt: bitIndex put: (aBoolean ifTrue: [1] ifFalse: [0])! !


!Morph class methodsFor: 'initialize-release' stamp: 'jmv 7/31/2020 15:46:22'!
clearIds

	LastMorphId _ nil.
	self allSubInstancesDo: [ :each |
		each clearId ]! !


!Morph methodsFor: 'accessing' stamp: 'jmv 7/31/2020 15:52:47'!
morphId
	"Non zero. Zero id means no Morph.
	reserve lower 8 bits of numeric ivar for boolean flags."
	"
	Morph clearIds
	"
	id ifNil: [
		LastMorphId isNil ifTrue: [ LastMorphId _ 0 ].
		LastMorphId _ LastMorphId + 1.
		id _ LastMorphId << 8 ].
	^id >> 8! !

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 7/31/2020 16:09:44'!
visible
	"Answer if I am visible -- default is true.
	Store value of 'hidden', because flags default to false."

	^ (self privateFlagAt: 3) not! !

!Morph methodsFor: 'caching' stamp: 'jmv 7/31/2020 15:56:42'!
clearId
	"Also clear flags (i.e. sets all flags to false)"

	id _ nil.! !

!Morph methodsFor: 'drawing' stamp: 'jmv 7/31/2020 16:05:51'!
visible: aBoolean
	"set the 'visible' attribute of the receiver to aBoolean
	Store value of 'hidden', because flags default to false."

	self visible == aBoolean
		ifTrue: [ ^ self ].
	self privateFlagAt: 3 put: aBoolean not.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded! !

!Morph methodsFor: 'initialization' stamp: 'jmv 7/31/2020 16:00:26'!
initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	location _ MorphicTranslation new.! !

!Morph methodsFor: 'layout' stamp: 'jmv 7/31/2020 16:00:40'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."
	"Only specific subclasses do layout. They redefine this method.
	Remember to call super, or set layoutNeeded ivar to false!!"

	self layoutNeeded: false! !

!Morph methodsFor: 'layout' stamp: 'jmv 7/31/2020 15:59:49'!
layoutSubmorphsIfNeeded
	"Return self. Recompute the layout if necessary."

	"Check senders. Many many not be needed. Others might be just to compute fullBounds, that we hope to elliminate!! Keep those that really need layout. of submorphs"
	self flag: #jmvVer2.

	self isLayoutNeeded ifTrue: [
		self layoutSubmorphs ].! !

!Morph methodsFor: 'layout' stamp: 'jmv 7/31/2020 15:59:02'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	self layoutNeeded: true.! !

!Morph methodsFor: 'updating' stamp: 'jmv 7/31/2020 16:13:02'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self invalidateDisplayRect: self displayBoundsOrBogus from: nil.
	self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]! !


!SystemWindow methodsFor: 'layout' stamp: 'jmv 7/31/2020 15:59:34'!
layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww pos |
	thickness _ 4.
	cornerExtent _ 20.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) morphPosition: cornerExtent@0 extent: w@thickness.
	(adjusters at: #bottomAdjuster) morphPosition: cornerExtent@(wh-thickness) extent: w@thickness.
	(adjusters at: #leftAdjuster) morphPosition: 0@cornerExtent extent: thickness@h.
	(adjusters at: #rightAdjuster) morphPosition: ww-thickness@cornerExtent extent: thickness@h.
	(adjusters at: #topLeftAdjuster) morphPosition: `0@0` extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster) morphPosition: 0@(wh-cornerExtent) extent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster) morphPosition: ww-cornerExtent@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster) morphPosition: ww@wh-cornerExtent extent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		pos _ borderWidth @ (borderWidth + self labelHeight).
		layoutMorph
			morphPosition: pos
			extent: extent - pos - borderWidth ].

	self layoutNeeded: false.! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 7/31/2020 16:00:14'!
layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		self layoutNeeded: false.
		^self].

	"Invariant: morphExtent >=  minimumLayoutExtent"
	self refreshExtent.
	
	direction == #horizontal ifTrue: [
		self layoutSubmorphsHorizontallyIn: self layoutBounds ].

	direction == #vertical ifTrue: [
		self layoutSubmorphsVerticallyIn: self layoutBounds ].

	self layoutNeeded: false.! !


!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 7/31/2020 15:59:11'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self updatePositionAndExtent.
	labelMorph fitContents.
	subLabelMorph fitContents.
	self layoutNeeded: true.! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/31/2020 16:13:11'!
fullAddCurrentRect: addCurrentBounds submorphs: trySubmorphs to: aDamageRecorder
	| currentMorphBounds |
	addCurrentBounds ifTrue: [
		currentMorph needsRedraw: false.
		currentMorph drawOn: self.
		currentMorphBounds _ self boundingRectOfCurrentMorphAfterDraw.
		currentMorph displayBounds: currentMorphBounds.
		aDamageRecorder recordInvalidRect: currentMorphBounds.
		currentMorph submorphsDo: [ :m | self fullUpdateBounds: m ].
	].
	trySubmorphs ifTrue: [
		currentMorph submorphNeedsRedraw: false.
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self clippingByCurrentMorphDo: [ self fullAddRedrawRect: clipped to: aDamageRecorder ]].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullAddRedrawRect: m to: aDamageRecorder ]
	].! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/31/2020 16:12:51'!
fullAddRedrawRect: aMorph to: aDamageRecorder
	| addCurrentBounds trySubmorphs |
	aMorph visible ifTrue: [
		addCurrentBounds _ aMorph isRedrawNeeded.
		trySubmorphs _ aMorph isSubmorphRedrawNeeded.
		(trySubmorphs or: [addCurrentBounds]) ifTrue: [
			self into: aMorph.
			self canvasToUse
				fullAddCurrentRect: addCurrentBounds
				submorphs: trySubmorphs
				to: aDamageRecorder.
			self outOfMorph
		]]! !

!methodRemoval: Morph #privateFlag: stamp: 'jmv 7/31/2020 15:50:27'!
Morph removeSelector: #privateFlag:!

!Morph reorganize!
('accessing' adoptWidgetsColor: beSticky color location lock morphId resistsRemoval taskbar toggleStickiness unlock unlockContents)
('accessing - flags' isLayoutNeeded isRedrawNeeded isSubmorphRedrawNeeded layoutNeeded: needsRedraw: submorphNeedsRedraw: visible)
('accessing - properties' hasProperty: isLocked isSticky lock: name name: removeProperty: setProperty:toValue: sticky: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits rotationDegrees: whenUIinSafeState:)
('caching' clearId fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateDisplayRect:from: invalidateLocalRect:)
('classification' isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' drawOn: drawingFails drawingFailsNot hide icon imageForm: imageForm:depth: isKnownFailing refreshWorld show visible:)
('dropping/grabbing' aboutToBeGrabbedBy: aboutToGrab: dropFiles: justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: dragEvent:localPosition: keyDown: keyStroke: keyUp: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Down:localPosition: mouseButton2Up:localPosition: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseEnter: mouseHover:localPosition: mouseLeave: mouseMove:localPosition: mouseScroll:localPosition: mouseStillDown windowEvent:)
('event handling testing' allowsFilesDrop allowsMorphDrop allowsSubmorphDrag handlesKeyboard handlesMouseDown: handlesMouseHover handlesMouseOver: handlesMouseScroll: handlesMouseStillDown:)
('event handling' mouseButton2Activity mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:with:after: addAlarm:withArguments:after: alarmScheduler removeAlarm:)
('events-processing' dispatchEvent:localPosition: focusKeyboardFor: handleFocusEvent: processDropFiles:localPosition: processDropMorph:localPosition: processKeyDown:localPosition: processKeyUp:localPosition: processKeystroke:localPosition: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseScroll:localPosition: processMouseStillDown processMouseUp:localPosition: processUnknownEvent:localPosition: processWindowEvent:localPosition: rejectsEvent: rejectsEventFully:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus hasMouseFocus keyboardFocusChange:)
('geometry' allocHeightForFactor: allocWidthForFactor: displayBounds displayBounds: displayBoundsForHalo displayBoundsOrBogus displayFullBounds extentBorder externalize: externalizeDisplayBounds: externalizeDistance: externalizeDistanceToWorld: externalizeToWorld: fontPreferenceChanged internalize: internalizeDistance: internalizeDistanceFromWorld: internalizeFromWorld: minimumExtent minimumLayoutExtent morphAlign:with: morphBounds morphExtent morphExtent: morphExtentInWorld morphExtentInWorld: morphHeight morphLocalBounds morphPosition morphPosition: morphPositionInWorld morphPositionInWorld: morphTopLeft morphWidth rotateBy: rotation:scale: scaleBy:)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' clipsLastSubmorph fullContainsGlobalPoint: fullContainsPoint: isOrthoRectangularMorph requiresVectorCanvas submorphsMightProtrude)
('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: balloonHelpDelayTime balloonText comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHalo wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' acceptDroppingMorph:event: layoutSubmorphs layoutSubmorphsIfNeeded minItemWidth minimumLayoutHeight minimumLayoutWidth minimumShrinkExtent minimumShrinkHeight minimumShrinkWidth someSubmorphPositionOrExtentChanged)
('layout-properties' layoutSpec layoutSpec: layoutSpecOrNil)
('macpal' flash flashWith:)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor expand exportAsBMP exportAsJPEG lockUnlockMorph lockedString maybeAddCollapseItemTo: stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: copyToClipboard: dismissMorph duplicateMorph: maybeDuplicateMorph potentialEmbeddingTargets resizeFromMenu resizeMorph)
('naming' label)
('object serialization' objectForDataStream:)
('player' okayToDuplicate)
('printing' printOn:)
('rotate scale and flex' rotationDegrees)
('stepping' shouldGetStepsFrom: startStepping startStepping: startStepping:in:stepTime: startStepping:stepTime: startSteppingStepTime: step stepAt: stopStepping stopStepping: wantsSteps)
('structure' allOwnersDo: allOwnersReverseDo: firstOwnerSuchThat: hasOwner: isInWorld owner owningWindow root veryLastLeaf withAllOwnersDo: withAllOwnersReverseDo: world)
('submorphs-accessing' allMorphsDo: clippedSubmorph findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph noteNewOwner: submorphBehind: submorphCount submorphInFrontOf: submorphs submorphsBehind:do: submorphsDo: submorphsDrawingOutsideReverseDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying: unclippedSubmorphsReverseDo:)
('submorphs-add/remove' addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:inFrontOf: addMorph:position: addMorphBack: addMorphBack:position: addMorphFront: addMorphFront:position: addMorphFrontFromWorldPosition: atFront comeToFront delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)
('testing' hasModel is: isCollapsed isOwnedByHand isOwnedByWorld isProportionalHeight isProportionalWidth isReallyVisible stepTime)
('updating' redrawNeeded)
('user interface' activateWindow activateWindowAndSendTopToBack: collapse showAndComeToFront toggleCollapseOrShow)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateAddMorph:atIndex:position: privateAnyOwnerHandlesMouseScroll: privateFlagAt: privateFlagAt:put: privateMoveBackMorph: privateMoveFrontMorph: privateOwner: privatePosition: privateRemove: privateSubmorphs)
('previewing' beginPreview endPreview endPreviewAndToggleCollapseOrShow morphBehindBeforePreview morphBehindBeforePreview: previewing previewing: visibleBeforePreview visibleBeforePreview:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Morph allSubInstancesDo: [ :m | (m valueOfProperty: #visible ifAbsent: [true]) ifFalse: [m privateFlagAt: 3 put: true]]!


