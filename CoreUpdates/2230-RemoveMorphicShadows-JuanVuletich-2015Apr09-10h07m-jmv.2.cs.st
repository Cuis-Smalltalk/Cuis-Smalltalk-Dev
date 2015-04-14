'From Cuis 4.2 of 25 July 2013 [latest update: #2230] on 9 April 2015 at 10:17:41.373932 am'!

!MorphicScanner methodsFor: 'private' stamp: 'jmv 4/9/2015 09:44'!
text: t foreground: foreColor
	text _ t.
	paragraphStyle _ nil.
	foregroundColor _ defaultColor _ foreColor! !


!MorphicScanner methodsFor: 'private' stamp: 'jmv 4/9/2015 09:44'!
textColor: textColor
	foregroundColor _ textColor! !


!Morph methodsFor: 'geometry testing' stamp: 'jmv 4/9/2015 09:52'!
fullContainsPoint: aPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	(self morphContainsPoint: aPoint) ifTrue: [ ^ true ].  "quick acceptance"
	self unclippedSubmorphsReverseDo: [ :m |
		(m fullContainsPoint: (m internalize: aPoint)) ifTrue: [ ^ true ]].
	^ false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 4/9/2015 09:40'!
morphContainsPoint: aLocalPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ self morphLocalBounds containsPoint: aLocalPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	"Can't do better. Please redefine in subclasses as appropriate!! (or finish Morphic 3!!)"
	"Mhhh. Podria usar el #imageForm: y ver si es transparente... deberia andar"
	^ self morphLocalBounds containsPoint: aLocalPoint! !


!EllipseMorph methodsFor: 'drawing' stamp: 'jmv 4/9/2015 09:43'!
drawOn: aCanvas 

	| rx ry |
	self revisar. "The morph should be specified better!!"
	rx _ extent x //2.
	ry _ extent y // 2.
	aCanvas ellipseCenterX: rx y: ry rx: rx ry: ry borderWidth: borderWidth borderColor: borderColor fillColor: color! !


!MagnifierMorph methodsFor: 'drawing' stamp: 'jmv 4/9/2015 09:43'!
drawOn: aCanvas
	RecursionLock == self ifFalse: [
		super drawOn: aCanvas.		"border and fill"
		aCanvas image: self magnifiedForm at: borderWidth@borderWidth]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/9/2015 09:48'!
fullDrawHandOn: aCanvas
	"A HandMorph has an unusual drawing requirement:
		The hand itself (i.e., the cursor) appears in front of its submorphs
	The illusion is that the hand plucks up morphs and carries them above the world."

	submorphs reverseDo: [ :m | aCanvas fullDraw: m ].
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/9/2015 09:49'!
needsToBeDrawn
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor. This is the case if it (a) it is a remote hand, (b) it is showing a temporary cursor, or (c) it is not empty and there are any visible submorphs. If using the software cursor, ensure that the hardware cursor is hidden."
	"Details:  Return true if this hand has a saved patch to ensure that is is processed by the world. This saved patch will be deleted after one final display pass when it becomes possible to start using the hardware cursor again. This trick gives us one last display cycle to allow us to remove the software cursor from the display."
	
	"Note. We draw the hand as a regular morph (using #drawOn:), disabling the hardware cursor, when we carry submorphs. The reason is to lock the mouse pointer and the carried morph together. Otherwhise the carried morph would lag behind the mouse pointer.
	This method answers whether the regular #drawOn: drawing mechanism is used for us.
	
	Check senders. Hand drawing is handled explicitly by the world, because the Hand is not a submorph of the world!!"

	(savedPatch notNil
		or: [ submorphs anySatisfy: [ :ea | ea visible ] ] )
		ifTrue: [
			"using the software cursor; hide the hardware one"
			Sensor currentCursor == Cursor blank ifFalse: [Cursor blank show].
			^ true].

	^ false! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/9/2015 09:49'!
restoreSavedPatchOn: aCanvas 
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas image: savedPatch at: savedPatch offset.
		submorphs notEmpty ifTrue: [ ^self ].

		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self invalidateDisplayRect: (self morphPosition extent: extent) from: nil.
		Sensor currentCursor == Cursor normal ifFalse: [ Cursor normal show ].	"show hardware cursor"
		savedPatch _ nil ]! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/9/2015 09:47'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| fullBounds |
	fullBounds _ self morphFullBoundsInWorld.
	fullBounds ifNil: [ ^self ].

	fullBounds _ fullBounds intersect: (0@0 extent: aCanvas extent).
	(savedPatch isNil or: [savedPatch extent ~= fullBounds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBounds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: fullBounds
		into: savedPatch.
	savedPatch offset: fullBounds topLeft.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBounds ]
		ifNotNil: [ aStream nextPut: (fullBounds merge: prevFullBounds)].
	prevFullBounds _ fullBounds! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 4/9/2015 09:50'!
drawOn: aCanvas
	"
	Transcript
		showOnDisplay: false;
		bounds: self morphBoundsInWorld;
		displayOn: aCanvas form.
	"
	Transcript
		showOnDisplay: true;
		bounds: (0@0 extent: extent);
		displayOn: form;
		bounds: self morphBoundsInWorld.
	aCanvas image: form at: 0@0
	! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/9/2015 09:46'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line boundsInWorld tl |

	tl _ boundsRect topLeft.
	boundsInWorld _ currentTransformation displayBoundsOfTransformOf: boundsRect.

	displayScanner _ MorphicScanner new 
		text: aTextComposition textComposed
		foreground: c.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aTextComposition lineIndexForPoint: (0@0 max: (clipLeft@clipTop)- boundsInWorld origin))
		to: (aTextComposition lineIndexForPoint: (boundsInWorld extent min: (clipRight@clipBottom+1) - boundsInWorld origin))
		do: [ :i |
			line _ aTextComposition lines at: i.
			aTextComposition
				displaySelectionInLine: line
				on: self
				textTopLeft: tl
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line textTopLeft: tl leftInRun: leftInRun  ]! !


!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 4/9/2015 09:42'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |
	p _ (currentTransformation transform: aPoint) rounded.
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: p sourceRect: sourceRect rule: r! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 4/9/2015 09:42'!
fillRectangle: aRectangle color: aColor
	"Fill the given rectangle."

	^self 
		frameAndFillRectangle: aRectangle
		fillColor: aColor
		borderWidth: 0
		borderColor: Color transparent! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/9/2015 09:46'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	| p1 font |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^self ].
	p1 _ (currentTransformation transform: aPoint) rounded.
	port colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	font on:	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 4/9/2015 09:41'!
setForm: aForm

	form _ aForm.
	self resetGrafPort.
	self newClipRect: nil! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 4/9/2015 09:46'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ aColor ifNil: [ Color transparent ].
	(paintColor is: #Color) ifFalse: [
		^self error: 'Cannot install color' ].

	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isOpaque or: [ self depth < 32]) ifTrue: [
		port fillColor: paintColor.
		port combinationRule: Form paint.
		^self ].

	"BitBlt setup for alpha mapped transfer"
	port fillColor: paintColor.
	port combinationRule: Form blend! !


!WorldState methodsFor: 'hands' stamp: 'jmv 4/9/2015 09:47'!
selectHandsToDrawForDamage: damageList
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	| result hBnds |
	result _ OrderedCollection new.
	hands do: [:h |
		h needsToBeDrawn ifTrue: [
			h hasChanged
				ifTrue: [result add: h]
				ifFalse: [
					hBnds _ h morphFullBoundsInWorld.
					hBnds _ hBnds origin corner: hBnds corner.
					(damageList detect: [:r | r intersects: hBnds] ifNone: nil)
						ifNotNil: [result add: h]]]].
	^ result
! !

!methodRemoval: MorphicCanvas class #forShadowOver:!
MorphicCanvas class removeSelector: #forShadowOver:!
!methodRemoval: MorphicCanvas #isShadowDrawing!
MorphicCanvas removeSelector: #isShadowDrawing!
!methodRemoval: MorphicCanvas #shadowColor:!
MorphicCanvas removeSelector: #shadowColor:!
!methodRemoval: HandMorph #shadowOffset!
HandMorph removeSelector: #shadowOffset!
!methodRemoval: HandMorph #submorphsShadowForm!
HandMorph removeSelector: #submorphsShadowForm!

!HandMorph reorganize!
('accessing' lastMouseEvent lastMouseEventTime mouseOverHandler)
('balloon help' balloonHelp balloonHelp: deleteBalloonTarget: removePendingBalloonFor: spawnBalloonFor: triggerBalloonFor:after:)
('caching' releaseCachedState)
('double click support' dontWaitForMoreClicks waitForClicksOrDrag:event:clkSel:dblClkSel: waitForClicksOrDrag:event:dragSel:clkSel: waitForClicksOrDragOrSimulatedMouseButton2:event:clkSel:clkNHalf:dblClkSel:dblClkNHalfSel:tripleClkSel:)
('drawing' drawOn: fullDrawHandOn: hasChanged needsToBeDrawn restoreSavedPatchOn: savePatchFrom:appendDamageTo:)
('event handling' flushEvents noticeMouseOver:event: processEventQueue)
('events-processing' startDropEventDispatch: startEventDispatch: startKeyboardDispatch: startMouseDispatch: startWindowEventDispatch:)
('focus handling' activateNextWindow activatePreviousWindow keyboardFocus keyboardFocusNext keyboardFocusPrevious mouseFocus newKeyboardFocus: newMouseFocus: nextFocusMorph nextFocusWindow previousFocusMorph previousFocusWindow releaseAllFoci releaseKeyboardFocus releaseKeyboardFocus: releaseMouseFocus releaseMouseFocus:)
('grabbing/dropping' attachMorph: dropMorph:event: dropMorphs: grabMorph:from:at:)
('halo handling' halo: obtainHalo: releaseHalo: removeHaloFromClick:on:)
('halos and balloon help' halo)
('initialization' initForEvents initialize)
('meta-actions' grabMorph: grabMorph:positionInWorld:)
('objects from disk' objectForDataStream:)
('paste buffer' pasteMorph)
('updating' redrawNeeded)
('private events' generateKeyboardEvent: generateMouseEvent: generateWindowEvent: mouseTrailFrom: shouldControlEmulateAltFor:)
('classification' is:)
!

!methodRemoval: Morph #ownShadowForm!
Morph removeSelector: #ownShadowForm!
!methodRemoval: MorphicScanner #text:foreground:ignoreColorChanges:!
MorphicScanner removeSelector: #text:foreground:ignoreColorChanges:!
