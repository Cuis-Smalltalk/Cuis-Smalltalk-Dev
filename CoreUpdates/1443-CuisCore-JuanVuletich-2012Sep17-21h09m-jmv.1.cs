'From Cuis 4.0 of 21 April 2012 [latest update: #1442] on 17 September 2012 at 9:15:51 pm'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 9/17/2012 21:05'!
invalidateRect: aRectangle

	"warning. Senders are using global coordinates. Redesign!!"
	self flag: #jmvVer2.	"ok?"
	"local now!!!!!!!!!!"
	owner ifNotNil: [
		owner invalidateRect: (location displayBoundsOfTransformOf: aRectangle) ]! !


!BorderedRectMorph methodsFor: 'drawing' stamp: 'jmv 9/17/2012 20:54'!
clipsSubmorphs
	"Drawing specific. If this property is set, clip the receiver's  
	submorphs to the receiver's clipping bounds.
	Maybe move up to RectangleLikeMorph"
	^ self
		valueOfProperty: #clipSubmorphs
		ifAbsent: [true]! !

!BorderedRectMorph methodsFor: 'geometry' stamp: 'jmv 9/17/2012 20:57'!
zzclippingBounds
	"Return the bounds to which any submorphs should be clipped if the property is set"
	"Maybe shouldn't exist"
	self flag: #jmvVer2.

	^ self morphBoundsInWorld insetBy: borderWidth! !


!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 9/17/2012 21:05'!
invalidateRect: damageRect
        "Clip damage reports to my bounds, since drawing is clipped to my bounds."

        self == self outermostWorldMorph 
                ifTrue: [ worldState recordDamagedRect: (damageRect intersect: (0@0 extent: self morphExtent) ) ]
                ifFalse: [ super invalidateRect: damageRect ]
! !


!RectangleIndicatorMorph methodsFor: 'resizong' stamp: 'jmv 9/17/2012 11:01'!
windowBottom: aNumber
	"aNumber is an Y coordinate in the owner's coordinate system"
	self morphBoundsInWorld: (self morphBoundsInWorld bottom: aNumber)! !

!RectangleIndicatorMorph methodsFor: 'resizong' stamp: 'jmv 9/17/2012 11:01'!
windowBottomLeft: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self morphBoundsInWorld: (aPoint x @ self morphBoundsInWorld top corner: self morphBoundsInWorld right @ aPoint y)! !

!RectangleIndicatorMorph methodsFor: 'resizong' stamp: 'jmv 9/17/2012 11:02'!
windowBottomRight: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self morphBoundsInWorld: (self morphPositionInWorld corner: aPoint)! !

!RectangleIndicatorMorph methodsFor: 'resizong' stamp: 'jmv 9/17/2012 11:02'!
windowLeft: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"
	self morphBoundsInWorld: (self morphBoundsInWorld left: aNumber)! !

!RectangleIndicatorMorph methodsFor: 'resizong' stamp: 'jmv 9/17/2012 11:02'!
windowRight: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"
	self morphBoundsInWorld: (self morphBoundsInWorld right: aNumber)! !

!RectangleIndicatorMorph methodsFor: 'resizong' stamp: 'jmv 9/17/2012 11:02'!
windowTop: aNumber
	"aNumber is an Y coordinate in the owner's coordinate system"
	self morphBoundsInWorld: (self morphBoundsInWorld top: aNumber)! !

!RectangleIndicatorMorph methodsFor: 'resizong' stamp: 'jmv 9/17/2012 11:02'!
windowTopLeft: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self morphBoundsInWorld: (aPoint corner: self morphBoundsInWorld corner)! !

!RectangleIndicatorMorph methodsFor: 'resizong' stamp: 'jmv 9/17/2012 11:02'!
windowTopRight: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"
	self morphBoundsInWorld: (self morphBoundsInWorld left @ aPoint y corner: aPoint x @ self morphBoundsInWorld bottom)! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 9/17/2012 21:00'!
zzlayoutBounds
	"Return the bounds for laying out children of the receiver"
	"Exclude the label area"

	^ self morphBoundsInWorld insetBy: (borderWidth @ (self labelHeight+borderWidth) corner: borderWidth @ borderWidth)! !


!BitBlt class methodsFor: 'examples' stamp: 'jmv 9/17/2012 10:46'!
alphaBlendDemo
	"To run this demo, use...
		Display restoreAfter: [BitBlt alphaBlendDemo]	
	Displays 10 alphas, then lets you paint.  Option-Click to stop painting."

	"This code exhibits alpha blending in any display depth by performing
	the blend in an off-screen buffer with 32-bit pixels, and then copying
	the result back onto the screen with an appropriate color map. - tk 3/10/97"
	
	"This version uses a sliding buffer for painting that keeps pixels in 32 bits
	as long as they are in the buffer, so as not to lose info by converting down
	to display resolution and back up to 32 bits at each operation. - di 3/15/97"

	| brush buff dispToBuff buffToDisplay mapDto32 map32toD prevP p brushToBuff theta buffRect buffSize buffToBuff brushRect delta newBuffRect updateRect |  

	"compute color maps if needed"
	Display depth <= 8 ifTrue: [
		mapDto32 _ Color cachedColormapFrom: Display depth to: 32.
		map32toD _ Color cachedColormapFrom: 32 to: Display depth].

	"display 10 different alphas, across top of screen"
	buff _ Form extent: 500@50 depth: 32.
	dispToBuff _ BitBlt toForm: buff.
	dispToBuff colorMap: mapDto32.
	dispToBuff copyFrom: (50@10 extent: 500@50) in: Display to: 0@0.
	1 to: 10 do: [:i | dispToBuff fill: (50*(i-1)@0 extent: 50@50)
						fillColor: (Color red alpha: i/10)
						rule: Form blend].
	buffToDisplay _ BitBlt toForm: Display.
	buffToDisplay colorMap: map32toD.
	buffToDisplay copyFrom: buff boundingBox in: buff to: 50@10.

	"Create a brush with radially varying alpha"
	brush _ Form extent: 30@30 depth: 32.
	1 to: 5 do: 
		[:i | brush fillShape: (Form dotOfSize: brush width*(6-i)//5)
				fillColor: (Color red alpha: 0.02 * i - 0.01)
				at: brush extent // 2].

	"Now paint with the brush using alpha blending."
	buffSize _ 100.
	buff _ Form extent: brush extent + buffSize depth: 32.  "Travelling 32-bit buffer"
	dispToBuff _ BitBlt toForm: buff.  "This is from Display to buff"
	dispToBuff colorMap: mapDto32.
	brushToBuff _ BitBlt toForm: buff.  "This is from brush to buff"
	brushToBuff sourceForm: brush; sourceOrigin: 0@0.
	brushToBuff combinationRule: Form blend.
	buffToBuff _ BitBlt toForm: buff.  "This is for slewing the buffer"

	[Sensor mouseButton2Pressed] whileFalse:
		[prevP _ nil.
		buffRect _ Sensor mousePoint - (buffSize // 2) extent: buff extent.
		dispToBuff copyFrom: buffRect in: Display to: 0@0.
		[Sensor mouseButton1Pressed] whileTrue:
			["Here is the painting loop"
			p _ Sensor mousePoint - (brush extent // 2).
			(prevP == nil or: [prevP ~= p]) ifTrue:
				[prevP == nil ifTrue: [prevP _ p].
				(p dist: prevP) > buffSize ifTrue:
					["Stroke too long to fit in buffer -- clip to buffer,
						and next time through will do more of it"
					theta _ (p-prevP) theta.
					p _ ((theta cos@theta sin) * buffSize asFloat + prevP) truncated].
				brushRect _ p extent: brush extent.
				(buffRect containsRect: brushRect) ifFalse:
					["Brush is out of buffer region.  Scroll the buffer,
						and fill vacated regions from the display"
					delta _ brushRect amountToTranslateWithin: buffRect.
					buffToBuff copyFrom: buff boundingBox in: buff to: delta.
					newBuffRect _ buffRect translatedBy: delta negated.
					newBuffRect
						areasOutside: buffRect
						do: [ :r | dispToBuff copyFrom: r in: Display to: r origin - newBuffRect origin ].
					buffRect _ newBuffRect].

				"Interpolate from prevP to p..."
				brushToBuff drawFrom: prevP - buffRect origin
									to: p - buffRect origin
									withFirstPoint: false.

				"Update (only) the altered pixels of the destination"
				updateRect _ (p min: prevP) corner: (p max: prevP) + brush extent.
				buffToDisplay copy: updateRect from: updateRect origin - buffRect origin in: buff.
				prevP _ p]]]! !


!DisplayMedium methodsFor: 'bordering' stamp: 'jmv 9/17/2012 10:44'!
border: aRectangle widthRectangle: insets rule: combinationRule fillColor: aHalfTone
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of each edge of the border is determined by the four coordinates 
	of insets. Uses aHalfTone and combinationRule for drawing the border."

	aRectangle
		areasOutside: (aRectangle insetBy: insets)
		do: [ :edgeStrip |
			self fill: edgeStrip rule: combinationRule fillColor: aHalfTone ]! !


!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 9/17/2012 10:44'!
forceDamageToScreen: allDamage
	"Force all the damage rects to the screen."
	| rectList excluded remaining regions |
	rectList _ allDamage.
	"Note: Reset extra regions at the beginning to prevent repeated errors"
	regions _ extraRegions.
	extraRegions _ nil.
	regions ifNotNil: [
		"exclude extra regions"
		regions do: [ :drawerAndRect |
			excluded _ drawerAndRect at: 2.
			remaining _ WriteStream on: #().
			rectList do: [ :r |
				r areasOutside: excluded do: [ :each |
					remaining nextPut: each ]].
			rectList _ remaining contents].
	].
	rectList do: [ :r | self forceToScreen: r ].
	regions ifNotNil:[
		"Have the drawers paint what is needed"
		regions do: [ :drawerAndRect | (drawerAndRect at: 1) forceToScreen ].
	]! !


!Morph methodsFor: 'geometry' stamp: 'jmv 9/17/2012 20:57'!
zzclippingBounds
	"Return the bounds to which any submorphs should be clipped if the property is set"
	"Maybe shouldn't exist"
	self flag: #jmvVer2.
	^ self morphBoundsInWorld! !

!Morph methodsFor: 'geometry' stamp: 'jmv 9/17/2012 21:01'!
zzlayoutBounds
	"Return the bounds for laying out children of the receiver"

	self flag: #jmvVer2.
	^ self zzclippingBounds! !

!Morph methodsFor: 'updating' stamp: 'jmv 9/17/2012 21:05'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Include submorphs if we don't clip!!
	Think about it. We don't to know about a specific display rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"
	
	self layoutSubmorphsIfNeeded.
	self invalidateRect: (0@0 extent: self morphExtent).
	self clipsSubmorphs ifFalse: [
		self submorphsDo: [ :m | m redrawNeeded ]]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/17/2012 21:04'!
restoreSavedPatchOn: aCanvas 
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas image: savedPatch at: savedPatch offset.
		submorphs notEmpty ifTrue: [ ^self ].

		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		owner invalidateRect: (savedPatch offset extent: savedPatch extent + self shadowOffset).
		Sensor currentCursor == Cursor normal ifFalse: [ Cursor normal show ].	"show hardware cursor"
		savedPatch _ nil ]! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/17/2012 10:46'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| ownBnds fullBnds bw |
	ownBnds _ self morphBoundsInWorld.
	fullBnds _ self morphFullBoundsInWorld.
	(savedPatch isNil or: [savedPatch extent ~= fullBnds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBnds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: (fullBnds translatedBy: aCanvas canvasOrigin)
		into: savedPatch.
	savedPatch offset: fullBnds topLeft.
	Preferences fastDragWindowForMorphic ifTrue: [
		bw _ HandMorph fastDragBorderWidth.
		aStream nextPut: ownBnds.
		prevBounds ifNotNil: [ aStream nextPut: prevBounds ].
		fullBnds
			areasOutside: (fullBnds insetBy: bw)
			do: [ :r | aStream nextPut: r ].
		prevFullBounds ifNotNil: [
			prevFullBounds areasOutside: (prevFullBounds insetBy: bw) do: [ :r |
				aStream nextPut: r ]]]
	ifFalse: [
		prevFullBounds ifNil: [
			aStream nextPut: fullBnds ]
		ifNotNil: [
			aStream nextPut: (fullBnds merge: prevFullBounds)]].
	prevBounds _ ownBnds.
	prevFullBounds _ fullBnds! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 9/17/2012 21:04'!
selectionChanged

	self paragraph selectionRects do: [:r | self invalidateRect: r ]! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 9/17/2012 21:04'!
onBlinkCursor
	"Blink the cursor"
	paragraph ifNil: [ ^nil ].
	paragraph showCaret: paragraph showCaret not | pauseBlinking.
	pauseBlinking _ false.
	paragraph lastCaretRect ifNotNil: [ :r | self invalidateRect: r].! !


!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 9/17/2012 21:05'!
onBlinkCursor
	"Blink the cursor"
	showCaret _ showCaret not | pauseBlinking.
	pauseBlinking _ false.
	caretRect ifNotNil: [ :r | self invalidateRect: r]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 9/17/2012 21:05'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	"Show cursor right now if needed"
	showCaret ifFalse: [
		showCaret _ true.
		caretRect ifNotNil: [ :r | self invalidateRect: r ]]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 9/17/2012 21:05'!
stopBlinking
	"And do not show cursor anymore."
	self stopSteppingSelector: #onBlinkCursor.
	"Hide cursor right now if needed"
	showCaret ifTrue: [
		showCaret _ false.
		caretRect ifNotNil: [ :r | self invalidateRect: r ]]! !


!Preferences class methodsFor: 'standard queries' stamp: 'jmv 3/28/2011 19:14'!
fastDragWindowForMorphic
	^ self
		valueOfFlag: #fastDragWindowForMorphic
		ifAbsent: [ false ]! !


!RectangleIndicatorMorph methodsFor: 'drawing' stamp: 'jmv 9/17/2012 11:07'!
drawOn: aCanvas
	| bw b |

	"We are using a regular #drawOn:, and invalidating in the usual way when resizing.
	This effectively defeats the purpose of this class: optimize redraws durin resize.
	It is true that the window is not resized until finished, and no layout is done while resizing,
	but the invalidate / redraw is slow too. Think of a better implementation. But without asking
	too much to the canvas form (like xoring a pattern... is that ok? Then, we would not be a
	standard morph, and we need to replace the usual invalidate / redraw behavior with this
	xoring strategy...)
	"
	self flag: #jmvVer2.

	bw _ self defaultBorderWidth.
	b _ 0@0 extent: extent.
	aCanvas frameRectangle: b borderWidth: bw color: Color black.
	aCanvas frameRectangle: (b insetBy: bw) borderWidth: bw color: Color white! !


!Sonogram methodsFor: 'all' stamp: 'jmv 9/17/2012 21:05'!
plotColumn: dataArray

	| chm1 i normVal r |
	columnForm unhibernate.
	chm1 _ columnForm height - 1.
	0 to: chm1 do: [ :y | 
		i _ y*(dataArray size-1)//chm1 + 1.
		normVal _ ((dataArray at: i) - minVal) / (maxVal - minVal).
		normVal < 0.0 ifTrue: [normVal _ 0.0].
		normVal > 1.0 ifTrue: [normVal _ 1.0].
		columnForm bits at: chm1-y+1 put: (pixValMap at: (normVal * 255.0) truncated + 1)].
	(lastX _ lastX + 1) > (image width - 1) ifTrue:
		[self scroll].
	image copy: (r _ (lastX@0 extent: 1@image height))
			from: (32//image depth-1)@0
			in: columnForm rule: Form over.
	"self changed."
	self invalidateRect: r! !


!SystemWindow methodsFor: 'change reporting' stamp: 'jmv 9/17/2012 21:05'!
invalidateTitleArea

	"not really pretty... also invalidating the top border, regardless of it being above or below the title area
	(Different themes use various looks, this covers them all)"
	self invalidateRect: (0@0 extent: extent x @ (self labelHeight + borderWidth))! !

!methodRemoval: SystemWindow #zzinnerBounds!
SystemWindow removeSelector: #zzinnerBounds!
!methodRemoval: PluggableScrollPane #clipsSubmorphs!
PluggableScrollPane removeSelector: #clipsSubmorphs!
!methodRemoval: PluggableButtonMorph #clipsSubmorphs!
PluggableButtonMorph removeSelector: #clipsSubmorphs!
!methodRemoval: PasteUpMorph #invalidRect:!
PasteUpMorph removeSelector: #invalidRect:!
!methodRemoval: PasteUpMorph #zzinvalidRect:!
PasteUpMorph removeSelector: #zzinvalidRect:!
!methodRemoval: LayoutMorph #clipsSubmorphs!
LayoutMorph removeSelector: #clipsSubmorphs!
!methodRemoval: HaloMorph #redrawNeeded!
HaloMorph removeSelector: #redrawNeeded!
!methodRemoval: BorderedRectMorph #zzinnerBounds!
BorderedRectMorph removeSelector: #zzinnerBounds!
!methodRemoval: Morph #invalidRect:!
Morph removeSelector: #invalidRect:!
!methodRemoval: Morph #zzinnerBounds!
Morph removeSelector: #zzinnerBounds!
!methodRemoval: Morph #zzinvalidRect:!
Morph removeSelector: #zzinvalidRect:!

!Morph reorganize!
('accessing' adoptWidgetsColor: balloonText beSticky color isLocked isSticky location lock lock: resistsRemoval sticky: toggleResistsRemoval toggleStickiness unlock unlockContents)
('accessing - extension' assureExtension hasExtension initializeExtension privateExtension: resetExtension)
('accessing - properties' hasProperty: removeProperty: setProperty:toValue: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo:)
('as yet unclassified' canDiscardEdits disregardUnacceptedEdits rotationDegrees:)
('caching' fullReleaseCachedState releaseCachedState)
('change reporting' addedMorph: invalidateRect:)
('classification' isPlayfieldLike isWorldMorph)
('copying' copy copyForClipboard duplicate)
('debug and other' addDebuggingItemsTo:hand: altSpecialCursor0 altSpecialCursor1 altSpecialCursor2 altSpecialCursor3 altSpecialCursor3: buildDebugMenu: inspectOwnerChain ownerChain resumeAfterDrawError resumeAfterStepError)
('drawing' addPossiblyUncoveredAreasIn:to: changeClipSubmorphs clipSubmorphs: clipsSubmorphs drawDropHighlightOn: drawErrorOn: drawMouseDownHighlightOn: drawOn: drawSubmorphsOn: drawingFails drawingFailsNot fullDrawOn: hasClipSubmorphsString hide highlightForMouseDown highlightForMouseDown: highlightedForMouseDown imageForm imageForm: imageForm:forRectangle: isKnownFailing ownShadowForm refreshWorld show visible visible:)
('dropping/grabbing' aboutToBeGrabbedBy: dragEnabled dragEnabled: dragNDropEnabled dropEnabled dropEnabled: dropHighlightColor enableDrag: enableDragNDrop enableDragNDrop: enableDrop: highlightForDrop highlightForDrop: highlightedForDrop justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: repelsMorph:event: resetHighlightForDrop separateDragAndDrop wantsDroppedMorph:event: wantsToBeDroppedInto:)
('e-toy support' embeddedInMorphicWindowLabeled: unlockOneSubpart wantsRecolorHandle)
('events' click:localPosition: doubleClick:localPosition: keyDown: keyStroke: keyUp: mouseButton3Down:localPosition: mouseButton3Up:localPosition: mouseDown:localPosition: mouseEnter: mouseLeave: mouseMove:localPosition: mouseStillDown: mouseUp:localPosition: windowEvent:)
('event handling testing' handlesKeyboard handlesMouseDown: handlesMouseOver: handlesMouseStillDown:)
('event handling' mouseStillDownStepRate mouseStillDownThreshold)
('events-alarms' addAlarm:after: addAlarm:at: addAlarm:with:after: addAlarm:with:at: addAlarm:with:with:after: addAlarm:with:with:at: addAlarm:withArguments:after: addAlarm:withArguments:at: alarmScheduler removeAlarm:)
('events-processing' closeWindowFor: containsPoint:event: dispatchEvent:localPosition: focusKeyboardFor: handleFocusEvent: processDropMorph:localPosition: processKeyDown:localPosition: processKeyUp:localPosition: processKeystroke:localPosition: processMouseDown:localPosition: processMouseEnter:localPosition: processMouseLeave:localPosition: processMouseMove:localPosition: processMouseOver:localPosition: processMouseStillDown:localPosition: processMouseUp:localPosition: processUnknownEvent:localPosition: processWindowEvent:localPosition: rejectDropEvent: rejectsEvent:)
('fileIn/out' prepareToBeSaved storeDataOn:)
('focus handling' hasKeyboardFocus keyboardFocusChange:)
('geometry' externalize: externalizeDistanceToWorld: externalizeToWorld: internalize: internalizeFromWorld: minimumExtent morphBoundsInWorld morphBoundsInWorld: morphExtent morphExtentInWorld morphFullBoundsInWorld morphHeight morphPosition morphPosition: morphPositionInOwner morphPositionInOwner: morphPositionInWorld morphPositionInWorld: morphWidth validateNotSent validateOwnerNotNil worldBoundsForHalo zzclippingBounds zzlayoutBounds)
('geometry eToy' referencePosition referencePosition:)
('geometry testing' containsPoint: fullContainsPoint: isOrthoRectangularMorph)
('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: addWorldHandlesTo:box: balloonHelpDelayTime comeToFrontAndAddHalo deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloClass mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: setBalloonText:maxLineLength: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHaloHandleWithSelector:inHalo:)
('initialization' inATwoWayScrollPane initialize intoWorld: openInHand openInWorld openInWorld:)
('iteration of all morphs' nextMorph nextMorphPart2 nextMorphThat: previousMorph previousMorphThat:)
('layout' acceptDroppingMorph:event: layoutSubmorphs layoutSubmorphsIfNeeded someSubmorphPositionOrExtentChanged submorphBounds)
('layout-properties' layoutSpec layoutSpec:)
('macpal' flash)
('menus' addAddHandMenuItemsForHalo:hand: addColorMenuItems:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addHaloActionsTo: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: changeColor changeDragAndDrop collapse expand exportAsBMP exportAsJPEG exportAsPNG hasDragAndDropEnabledString lockUnlockMorph lockedString maybeAddCollapseItemTo: resistsRemovalString stickinessString)
('meta-actions' addEmbeddingMenuItemsTo:hand: buildHandleMenu: changeColorTarget:selector:originalColor:hand: copyToClipboard: dismissMorph duplicateMorph: grabMorph: maybeDuplicateMorph potentialEmbeddingTargets resizeFromMenu resizeMorph)
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
('visual properties')
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateOwner: privateRemove:)
!

