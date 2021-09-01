'From Cuis 5.0 [latest update: #4828] on 1 September 2021 at 4:09:49 pm'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/1/2021 15:44:52'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	DisplayScreen isDisplayExtentOk ifFalse: [
		self clearCanvas.
		DisplayScreen startUp.
		self setMainCanvas.
		self restoreDisplay.
		self whenUIinSafeState: [ Cursor defaultCursor activateCursor ]].! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 9/1/2021 15:46:02'!
mainLoop

	
	self clearWaitDelay.
	canvas isNil ifTrue: [
		self setMainCanvas ].
	self redrawNeeded.
	[
		self doOneCycle.
		true ]
			whileTrue: []! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/1/2021 15:39:30'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAny |
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	self displayWorldSafely.
	"Run steps, alarms and deferred UI messages"
	self runStepMethods.
	"Process user input events. Run all event triggered code."
	hadAny _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAny _ hadAny | h processEventQueue.
		activeHand _ nil ].
	"The default is the primary hand"
	activeHand _ self hands first.
	^ hadAny.! !


!WorldMorph class methodsFor: 'instance creation' stamp: 'jmv 9/1/2021 15:46:58'!
newWorld
	"
[
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: WorldMorph newWorld
] fork.
	"
	| w |
	w _ self new.
	w morphPosition: `0@0` extent: Display extent.
	w handsDo: [ :h |
		h privateOwner: w ].
	^w! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/1/2021 15:33:22'!
fullScreenOff

	Display fullScreenMode: false.! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/1/2021 15:33:17'!
fullScreenOn

	Display fullScreenMode: true.! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 9/1/2021 15:55:46'!
activeSubclass: aMorphicCanvasSubclass
	ActiveSubclass _ aMorphicCanvasSubclass.
	UISupervisor ui ifNotNil: [ :world |
		world whenUIinSafeState: [
			world setMainCanvas ]].! !

!methodRemoval: WorldMorph #extentChanged: stamp: 'jmv 9/1/2021 16:07:02'!
WorldMorph removeSelector: #extentChanged:!
!methodRemoval: DisplayScreen class #checkForNewScreenSize stamp: 'jmv 9/1/2021 16:06:53'!
DisplayScreen class removeSelector: #checkForNewScreenSize!
!methodRemoval: UISupervisor class #restoreDisplay stamp: 'jmv 9/1/2021 16:08:06'!
UISupervisor class removeSelector: #restoreDisplay!

!MorphicCanvas reorganize!
('accessing' boundsFinderCanvas clipRect currentTransformation extent form geometryTransformation: newClipRect:)
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor: ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol:)
('drawing-roundedRectangles' roundRect:color:radius:)
('drawing-text' drawString:at:font:color: drawString:at:font:color:embossed: drawString:atBaseline:font:color: drawString:atCenterX:font:color: drawString:atCenterXBaselineY:font:color: drawString:atWaist:font:color: drawString:atWaistRight:font:color: drawString:from:to:at:font:color: drawString:from:to:atBaseline:font:color: drawString:from:to:atWaist:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing-icons' drawCloseIcon drawCollapseIcon drawDownIcon drawExpandIcon drawExpandedAt: drawLeftIcon drawMenuIcon drawNotExpandedAt: drawPushPinIcon drawRightIcon drawUpIcon)
('drawing-widgets' drawButtonIconFromCurrentMorph)
('drawing-windows' drawRoundedFrame:border:color:insideColor:labelHeight:)
('drawing - Global Coordinates' frameAndFillGlobalRect:fillColor:borderWidth:color: frameGlobalRect:borderWidth:color: frameReverseGlobalRect:borderWidth:)
('initialization' initializeWithTranslation: resetCanvas world:)
('morphic' canDraw: clipCurrentMorph: contourOfCurrentMorphAfterDrawInto:into: currentMorphDrawingFails drawCurrentAndSubmorphs drawCurrentAsError drawCurrentAsOutline drawHand drawHighlight: fullDraw: fullDrawHand: into: outOfMorph restorePatch:bounds: savePatch:bounds:)
('other' showAt:invalidRects:)
('printing' printOn:)
('testing' drawsOnDisplay isBoundsFinderCanvas isCurrentMorphVisible usesVectorEnginePlugin)
('private' canvasOrigin canvasToUse clippingByCurrentMorphDo: fontToUse: setClipRect:)
('morphic world' boundingRectOfCurrentMorphAfterDraw computeDamage:repair:rootMorphs:rootMorphsDamage: drawWorld:repair: drawWorld:rootMorphs:rootMorphsDamage:allDamage: drawWorldBackground:rects:)
!

