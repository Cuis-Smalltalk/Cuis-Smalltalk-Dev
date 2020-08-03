'From Cuis 5.0 [latest update: #4266] on 20 July 2020 at 5:20:13 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 7/20/2020 17:06:19'!
displayBounds
	"At time of last draw. nil if unknown."

	self visible ifFalse: [ ^nil ].
	^ self valueOfProperty: #displayBounds! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/20/2020 17:12:49'!
displayBounds: aRectangle
	"If we update know bounds, chances are last draw operation used incorrect value. Draw again then."

	self displayBounds ~= aRectangle ifTrue: [
		self setProperty: #displayBounds toValue: aRectangle ].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/20/2020 17:11:35'!
displayBoundsOrBogus
	"Never answer nil. A bogus value will trigger a draw operation, and bounds will then be corrected."
"Ver si esto aun tiene algun sentido"
	^self displayBounds ifNil: [
		Rectangle center: (self externalizeToWorld: 0@0) extent: 2@2 ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/20/2020 17:10:46'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world)."

	| answer |
	answer _ self displayBounds.
	answer isNil ifTrue: [ ^nil ].
	self submorphsDrawingOutsideReverseDo: [ :m |
		answer _ answer quickMerge: m displayFullBounds ].
	^answer! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2020 17:14:34'!
displayBounds
	^self isWorldMorph ifTrue: [0@0 extent: extent] ifFalse: [super displayBounds]! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/20/2020 17:16:36'!
displayFullBounds
"ver lo que habia hecho. #lastPos, reusar cuando se mueve."
	^super displayFullBounds! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/20/2020 17:18:36'!
displayBoundsInWorldOf: aMorph
	"Answer a rectangle that completely bounds aMorph when drawn on our form.
	Might be larger than strictly required.
		- In Morphic 2, this could cause clipping artifacts. This doesn't usually happen because:
			a) Morphic 2 doesn't use scaling and rotation
			b) Most Morphic 2 morphs have rectangular shape.
		- In Morphic 3, clipping also considers the real shape of the owner morph. This avoids those artifacts."
"borrar?"
	"Think about doing a BoundsFinderCanvas even for Morphic 2"
	self flag: #jmvVer2.

	^self externalizeDisplayBounds: aMorph morphLocalBounds from: aMorph! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/20/2020 17:18:44'!
displayFullBoundsInWorldOf: aMorph
	"Answer a rectangle that completely bounds aMorph and submorphs when drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Might be larger than strictly required. See comment at #displayBoundsInWorldOf:"

	| r |
	"Think about doing a BoundsFinderCanvas even for Morphic 2"
"borrar?"
	self flag: #jmvVer2.

	r _ self displayBoundsInWorldOf: aMorph.
	aMorph submorphsDrawingOutsideReverseDo: [ :m |
		m visible ifTrue: [
			r _ r quickMerge: (self displayFullBoundsInWorldOf: m) ]].

	^r! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/20/2020 17:19:44'!
drawCurrentAndSubmorphs

	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self.
		world notNil ifTrue: [
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ].
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		currentMorph hasSubmorphs ifTrue: [
			currentMorph clippedSubmorph ifNotNil: [ :clipped |
				self clippingByCurrentMorphDo: [ self fullDraw: clipped ]].
			currentMorph unclippedSubmorphsReverseDo: [ :m |
				self fullDraw: m ]]].! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 7/11/2020 09:41:46'!
boundingRectOfCurrentMorphAfterDraw
	"This rectangle is used for clipping submorphs in BitBltCanvas.
	In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used as an optimization of the area to be redrawn."

	self subclassResponsibility! !


!HandMorph reorganize!
('accessing' lastMouseEvent lastMouseEventTime mouseOverHandler)
('balloon help' balloonHelp balloonHelp: deleteBalloonTarget: removePendingBalloonFor: spawnBalloonFor: triggerBalloonFor:after:)
('caching' releaseCachedState)
('classification' is:)
('double click support' dontWaitForMoreClicks waitForClicksOrDrag:event:clkSel:dblClkSel: waitForClicksOrDrag:event:dragSel:clkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel:dragSel:)
('drawing' drawOn: fullDrawHandOn: hasChanged needsToBeDrawn restoreSavedPatchOn: savePatchFrom:appendDamageTo:)
('event handling' createEventFrom:ofType: flushEvents noticeMouseOver:event: processEventQueue)
('events-processing' startDropEventDispatch: startDropFilesEventDispatch: startEventDispatch: startKeyboardDispatch: startMouseDispatch: startWindowEventDispatch:)
('focus handling' activateNextWindow activatePreviousWindow keyboardFocus keyboardFocusNext keyboardFocusPrevious mouseFocus newKeyboardFocus: newMouseFocus: nextFocusMorph nextFocusWindow previousFocusMorph previousFocusWindow releaseAllFoci releaseKeyboardFocus releaseKeyboardFocus: releaseMouseFocus releaseMouseFocus:)
('geometry' displayFullBounds)
('grabbing/dropping' attachMorph: attachMorphBeside: dropMorph:event: dropMorphs: grabMorph: grabMorph:delta: grabMorph:moveUnderHand:)
('halo handling' halo: obtainHalo: releaseHalo: removeHaloFromClick:on:)
('halos and balloon help' halo)
('initialization' initForEvents initialize)
('objects from disk' objectForDataStream:)
('paste buffer' pasteMorph)
('updating' redrawNeeded)
('private' forgetGrabMorphDataFor: grabMorphDataFor: rememberGrabMorphDataFor:)
('private events' generateDropFilesEvent: generateKeyboardEvent: generateMouseEvent: generateWindowEvent: mouseTrailFrom: shouldControlEmulateAltFor:)
!


!MorphicCanvas reorganize!
('accessing' clipRect currentTransformation depth extent form newClipRect:)
('drawing' line:to:width:color:)
('drawing-icons' drawButtonIconFromCurrentMorph)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol: reverseRectangleBorder:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:at:font:color: drawString:at:font:color:embossed: drawString:from:to:at:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('initialization' initializeWith:origin: world:)
('morphic' clippingMorph: displayBoundsInWorldOf: displayFullBoundsInWorldOf: drawCurrentAndSubmorphs drawCurrentAsError drawCurrentAsOutline drawWorld:submorphs:repair: fullDraw: fullDrawHand: into: outOfMorph)
('other' contentsOfArea:into: showAt:invalidRects:)
('printing' printOn:)
('testing' drawsOnDisplay isCurrentMorphVisible)
('private' boundingRectOfCurrentMorphAfterDraw canvasOrigin canvasToUse clippingByCurrentMorphDo: externalizeDisplayBounds:from: setClipRect: setForm:)
!

