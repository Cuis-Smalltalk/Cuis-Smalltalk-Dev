'From Cuis 4.1 of 12 December 2012 [latest update: #1663] on 8 April 2013 at 9:46:20 am'!

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 4/8/2013 09:38'!
flashRects: rectangleList color: aColorOrNil
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."
	"Details: Uses two reverses so that the display is restored to its original state. This is necessary when in deferred update mode."

	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: 0@0;
		clipRect: self viewBox;
		fillColor: (aColorOrNil ifNil: [Color white]);
		combinationRule: Form reverse.
	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect; forceDisplayUpdate.
		(Delay forMilliseconds: 150) wait.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect; forceDisplayUpdate
		].
! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 4/8/2013 08:50'!
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
		aWorld drawOn: (canvas copyClipRect: r) ].

	"Draw morphs"
	submorphs reverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(canvas copyClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !


!WorldState class methodsFor: 'sample code' stamp: 'jmv 4/8/2013 09:15'!
experiment1
	"To play with and learn about morphic Display update logic.
	Remove the morphs when done!!"
	"
	WorldState experiment1
	"
	| dr morph1 morph2 s w |
	morph1 _ RectangleLikeMorph new openInWorld.
	morph2 _ RectangleLikeMorph new openInWorld.
	morph2 color: Color green.
	morph2 morphPosition: 200@800; morphExtent: 50@40.
	morph1 morphPosition: 120@720; morphExtent: 220@100.

	w _ self runningWorld.
	s _ w instVarNamed: 'worldState'.
	dr _ s instVarNamed: 'damageRecorder'.

	dr doFullRepaint.
	dr reset;
	"	recordInvalidRect: (100@700 corner: 400@900);"
		recordInvalidRect: (100@700 corner: 150@900);
		recordInvalidRect: (300@700 corner: 400@900).
	Display fillColor: Color gray.
	"Do not draw the area used by the small rectangle, unless it is also drawn!!"
	s drawInvalidAreasWorld: w submorphs: {morph1. morph2 }! !


!Preferences class methodsFor: 'standard queries' stamp: 'jmv 4/8/2013 09:45'!
debugShowDamage
	"Useful for debugging, but also for simulating vnc over a slow network"
	^ (self
		valueOfFlag: #debugShowDamage
		ifAbsent: [ false ])! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 4/8/2013 09:37'!
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

!WorldState methodsFor: 'drawing' stamp: 'jmv 4/8/2013 09:38'!
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
			aWorld drawOn: (canvas copyClipRect: r) ]].

	"Now, draw the recorded areas of selected morphs, back to front."
	morphsToDraw with: rectsForEachMorph reverseDo: [ :m :xrects |
		"Here we could think about merging all xrects into just one call...
		This would mean drawing each morph just once.
		But that would mean drawing pixels we were not told to. There could be other morphs in that area that are not even being drawn!!
		See WorldState class >> #experiment1"
"		rr _ nil."
		xrects do: [ :r |
"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
			(canvas copyClipRect: r) fullDraw: m.
		].
		"*make this true to flash damaged areas for testing*"
		Preferences debugShowDamage ifTrue: [
			aWorld flashRects: xrects color: Color random ].
"		(canvas copyClipRect: rr) fullDraw: m"
	].

	"Answer a list of rectangles to be updated on the Display.
	This usually is not performance critical, drawing morphs is slower than just exposing the Display."
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !


!WorldState class reorganize!
('class initialization' addDeferredUIMessage: deferredUIMessages initialize)
('sample code' experiment1)
!

!methodRemoval: PasteUpMorph #flashRects:!
PasteUpMorph removeSelector: #flashRects:!
