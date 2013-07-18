'From Cuis 4.1 of 12 December 2012 [latest update: #1665] on 9 April 2013 at 7:17:24 am'!

!FormCanvas methodsFor: 'clipping rectangle' stamp: 'jmv 4/9/2013 07:15'!
additionalClipRect: aRectangle while: aBlock
	"aRectangle is in canvas coordinates.
	But ivar clipRect is relative to the form,
	For example, if we had previously been built like
		aCanvas on: someForm over: (100@100 extent 200@100)
	then our origin would be -100 @ -100.
	Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting
	only (20@20 extent: 40@30) in our form"
	| oldClipRect |
	oldClipRect _ clipRect.
	self setClipRect: (clipRect intersect: (aRectangle translatedBy: self canvasOrigin)).
	aBlock value.
	self setClipRect: oldClipRect.! !

!FormCanvas methodsFor: 'clipping rectangle' stamp: 'jmv 4/8/2013 21:21'!
newClipRect: aRectangleOrNil
	"aRectangle is in canvas coordinates.
	But ivar clipRect is relative to the form,
	For example, if we had previously been built like
		aCanvas on: someForm over: (100@100 extent 200@100)
	then our origin would be -100 @ -100.
	Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting
	only (20@20 extent: 40@30) in our form"

	^ self setClipRect: (aRectangleOrNil
		ifNil: [0@0 corner: form extent]
		ifNotNil: [aRectangleOrNil translatedBy: self canvasOrigin])! !


!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/8/2013 21:36'!
drawString: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: aColor kern: kernOrNil
	| font portRect bounds kern insideColor |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.	"horible!!"
	port clipByX1: bounds left
		y1: bounds top
		x2: bounds right
		y2: bounds bottom.
	font _ fontOrNil ifNil: [ StrikeFont default ].
	kern _ kernOrNil ifNil: [ font baseKern negated ].
	insideColor _ shadowColor ifNil: [ aColor ].
	port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: bounds topLeft
		strikeFont: font
		color: insideColor
		kern: kern.
	port clipRect: portRect! !

!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 4/8/2013 21:36'!
drawStringEmbossed: aString from: firstIndex to: lastIndex in: aRectangle font: fontOrNil color: aColor
	| font portRect insideColor bounds |
	bounds _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	port colorMap: nil.
	portRect _ port clipRect.	"horible!!"
	port clipByX1: bounds left
		y1: bounds top
		x2: bounds right
		y2: bounds bottom.
	font _ fontOrNil ifNil: [StrikeFont default].
	insideColor _ shadowColor ifNil: [ aColor ].
	insideColor = Color black ifFalse: [ | topColor |
		topColor _ insideColor alphaMixed: 0.25 with: Color black.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: bounds topLeft
			strikeFont: font
			color: topColor
			kern: font baseKern negated ].
	insideColor = Color white ifFalse: [ | bottomColor |
		bottomColor _ insideColor alphaMixed: 0.22 with: Color white.
		port
			displayString: aString asString
			from: firstIndex
			to: lastIndex
			at: bounds topLeft + (0@2)
			strikeFont: font
			color: bottomColor
			kern: font baseKern negated ].
	port
		displayString: aString asString
		from: firstIndex
		to: lastIndex
		at: bounds topLeft + (0@1)
		strikeFont: font
		color: insideColor
		kern: font baseKern negated.
	port clipRect: portRect! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 4/8/2013 21:20'!
setForm: aForm

	form _ aForm.
	self resetGrafPort.

	"origin of the top-left corner of this cavas"
	self setOrigin: 0@0.
	self newClipRect: nil.
	shadowColor _ nil! !


!Morph methodsFor: 'drawing' stamp: 'jmv 4/9/2013 07:16'!
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front"

	submorphs isEmpty ifTrue: [ ^ self ].

	"No Clipping? Just draw and we're done."
	self clipsSubmorphs ifFalse: [
		submorphs reverseDo: [ :m |
			aCanvas fullDraw: m ].
		^self].

	"Draw with additional clipping done by us."
	aCanvas additionalClipRect: self clippingBoundsInWorld while: [
		submorphs reverseDo: [ :m |
			aCanvas fullDraw: m ]]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 4/8/2013 22:58'!
fullDrawOn: aCanvas
	| shadowForm |
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."
	"Note: This version does not cache an image of the morphs being held by the hand.
	 Thus, it is slower for complex morphs, but consumes less space."

	submorphs isEmpty ifTrue: [^ self drawOn: aCanvas].  "just draw the hand itself"

	"Note: We use a shadow form here to prevent drawing
	overlapping morphs multiple times using the transparent
	shadow color."
	shadowForm _ self submorphsShadowForm.

	"draw shadows"
	aCanvas stencil: shadowForm at: shadowForm offset  + self shadowOffset color: (Color black alpha: 0.5).
	
	"draw morphs in front of shadows"
	self drawSubmorphsOn: aCanvas.
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 4/8/2013 22:59'!
displayWorld: aWorld submorphs: submorphs
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvasFor: aWorld.

	"repair world's damage on canvas"
	worldDamageRects _ self drawInvalidAreasWorld: aWorld submorphs: submorphs.

	"Check which hands need to be drawn (they are not the hardware mouse pointer)"
	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"Draw hands (usually carying morphs) onto world canvas"
	canvas newClipRect: nil.
	handsToDraw reverseDo: [ :h | canvas fullDraw: h ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode
		ifTrue: [
			"Drawing was done on Display object. Just show it"
			Display forceDamageToScreen: allDamage.
			]
		ifFalse: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: aWorld viewBox origin invalidRects: allDamage.
			].

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].
	Display deferUpdates: false; forceDisplayUpdate! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 4/8/2013 23:19'!
drawInvalidAreasWorld: aWorld submorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |

	"The simple implementation is slowers as it draws morph areas that will later be covered by other morphs.
	But it works, and it is easier to understand."
	true not ifTrue: [ ^self simpleDrawInvalidAreasWorld: aWorld submorphs: submorphs ].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ (damageRecorder invalidRectsFullBounds: aWorld viewBox) reject: [ :r | r isNil ].
	damageRecorder reset.
	currentRectsToRepair _ OrderedCollection new.
	newRectsToRepair _ OrderedCollection withAll: initialRectsToRepair.
	morphsToDraw _ OrderedCollection new.
	rectsForEachMorph _ OrderedCollection new.
	thisMorphRects _ OrderedCollection new.
	n _ submorphs size.
	i _ 1.

	"The idea here is to iterate morphs front to back, i.e. first the ones on top.
	For each morph, record the rectangles it affects. And if a morph is opaque, remove the area behind it from the list of rectangles to be repaired.
	This means, for example, that morphs completely covered might even not be redrawn.
	this is a huge performance gain when there are many morphs on screen, especially if there are more than ten open windows, and Theme uses opaque colors."
	[ i <= n and: [ newRectsToRepair notEmpty ]] whileTrue: [
		morph _ submorphs at: i.
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			reuse _ currentRectsToRepair.
			currentRectsToRepair _ newRectsToRepair.
			newRectsToRepair _ reuse removeAll.
			currentRectsToRepair do: [ :r |
				(morphBounds intersects: r)
					ifTrue: [
						morphClipRect _ morphBounds intersect: r.
						thisMorphRects add: morphClipRect. "We could perhaps try and join adjacent rectangles in this collection..."
						morph addPossiblyUncoveredAreasIn: r to: newRectsToRepair ]
					ifFalse: [
						newRectsToRepair add: r ]].
			thisMorphRects ifNotEmpty: [
				morphsToDraw add: morph.
				rectsForEachMorph add: thisMorphRects.
				thisMorphRects _ OrderedCollection new.
			]].
		i _ i + 1 ].

	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [ aWorld flashRects: initialRectsToRepair color: nil ].

	"Draw world background on those areas that were not completely covered by opaque morphs"
	i > n  ifTrue: [
		newRectsToRepair do: [ :r |
			aWorld drawOn: (canvas newClipRect: r) ]].

	"Now, draw the recorded areas of selected morphs, back to front."
	morphsToDraw with: rectsForEachMorph reverseDo: [ :m :xrects |
		"Here we could think about merging all xrects into just one call...
		This would mean drawing each morph just once.
		But that would mean drawing pixels we were not told to. There could be other morphs in that area that are not even being drawn!!
		See WorldState class >> #experiment1"
"		rr _ nil."
		xrects do: [ :r |
"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
			(canvas newClipRect: r) fullDraw: m.
		].
		"*make this true to flash damaged areas for testing*"
		Preferences debugShowDamage ifTrue: [
			aWorld flashRects: xrects color: Color random ].
"		(canvas newClipRect: rr) fullDraw: m"
	].

	"Answer a list of rectangles to be updated on the Display.
	This usually is not performance critical, drawing morphs is slower than just exposing the Display."
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 4/8/2013 21:14'!
simpleDrawInvalidAreasWorld: aWorld submorphs: submorphs

	"mover todo esto al canvas, no?
	Y ver que hacer con los argumentos, etc.... Toda esta bananarama!!"

	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	This simple implementation just does as requested: No optimizations are done."

	| rectsToRepair morphBounds |
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ (damageRecorder invalidRectsFullBounds: aWorld viewBox)
		reject: [ :r | r isNil ].
	damageRecorder reset.

	"Draw World"
	rectsToRepair do: [ :r |
		aWorld drawOn: (canvas newClipRect: r) ].

	"Draw morphs"
	submorphs reverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(canvas newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !

!methodRemoval: HandMorph #nonCachingFullDrawOn:!
HandMorph removeSelector: #nonCachingFullDrawOn:!
!methodRemoval: FormCanvas #copyClipRect:!
FormCanvas removeSelector: #copyClipRect:!

!FormCanvas reorganize!
('accessing' canvasOrigin clipRect contentsOfArea:into: currentTransformation depth extent form formWithOffset grafPort shadowColor:)
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:at:font:color: drawString:from:to:at:font:color:kern: drawString:from:to:in:font:color:kern: drawString:in:font:color: drawStringEmbossed:from:to:in:font:color: drawStringEmbossed:in:font:color: paragraph:bounds:color:selectionColor:)
('initialization' initialize)
('other' showAt:invalidRects:)
('printing' printOn:)
('testing' drawsOnDisplay isFullyVisible: isShadowDrawing isVisible:)
('private' buildAuxWith:multipliedWith: fillRectangle:tilingWith:sourceRect:rule: frameRectangle:borderWidth:topLeftColor:bottomRightColor: resetGrafPort setClipRect: setForm: setOrigin: setPaintColor:)
('morphic' fullDraw: into: outOf:)
('clipping rectangle' additionalClipRect:while: newClipRect:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
DamageRecorder allInstancesDo: [ :dr | dr reset; doFullRepaint ]!

