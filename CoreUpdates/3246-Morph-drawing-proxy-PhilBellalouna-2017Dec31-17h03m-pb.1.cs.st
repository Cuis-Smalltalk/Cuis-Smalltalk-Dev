'From Cuis 5.0 of 7 November 2016 [latest update: #3236] on 6 January 2018 at 5:56:59 pm'!
!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'owner submorphs location layoutNeeded layoutSpec properties drawingProxy '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Morph methodsFor: 'caching' stamp: 'pb 1/6/2018 17:13:14'!
drawingProxy: aProxy
	"Inject on a per-Morph (or Morph subclass) basis as needed"
    drawingProxy _ aProxy.
    (drawingProxy respondsTo: #morph:) ifTrue: [ drawingProxy morph: self ].
    self redrawNeeded! !

!Morph methodsFor: 'caching' stamp: 'pb 1/6/2018 17:18:01'!
removeDrawingProxy
	self drawingProxy: nil! !

!Morph methodsFor: 'private' stamp: 'pb 1/6/2018 17:43:31'!
privateDrawOn: aCanvas
	drawingProxy ifNil: [self drawOn: aCanvas] ifNotNil: [drawingProxy draw: self on: aCanvas]! !

!Morph methodsFor: 'private' stamp: 'pb 12/31/2017 23:42:02'!
privateDrawingProxy
	^ drawingProxy! !


!Morph class methodsFor: 'caching' stamp: 'pb 1/6/2018 17:18:26'!
removeAllDrawingProxies
	self allSubInstancesDo: [:ea| ea removeDrawingProxy ]! !

!Morph methodsFor: 'caching' stamp: 'pb 12/31/2017 23:39:10'!
releaseCachedState
	"Release any state that can be recomputed on demand, such as the pixel values for a color gradient or the editor state for a TextMorph. This method may be called to save space when a morph becomes inaccessible. Implementations of this method should do 'super releaseCachedState'."
	drawingProxy ifNotNil: [:dp| dp releaseCachedState ]! !

!Morph methodsFor: 'updating' stamp: 'pb 1/6/2018 17:49:55'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."
	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Include submorphs if we don't clip!!
	Think about it. We don't to know about a specific display rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"
	drawingProxy ifNotNil: [ :dc |
		dc markDirty: self ].
	self morphBoundsInWorld ifNotNil: [ :r |
		self
			invalidateDisplayRect: r
			from: nil.
		"Expensive in many cases..."
		self submorphsDrawingOutsideReverseDo: [ :m |
			m redrawNeeded ]].! !

!WorldState methodsFor: 'drawing' stamp: 'pb 12/31/2017 17:05:37'!
drawInvalidAreasSubmorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |

	"The simple implementation is slowers as it draws morph areas that will later be covered by other morphs.
	But it works, and it is easier to understand.
	See en.wikipedia.org/wiki/Painter's_algorithm"
	true not ifTrue: [ ^self simpleDrawInvalidAreasSubmorphs: submorphs ].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ damageRecorder invalidRectsFullBounds: world viewBox.
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
	this is a huge performance gain when there are many morphs on screen, especially if there are more than ten open windows, and Theme uses opaque colors.
	See 'A reverse painter's algorithm' at en.wikipedia.org/wiki/Painter's_algorithm"
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
	Preferences debugShowDamage ifTrue: [ world fillRects: initialRectsToRepair color: Color gray ].

	"Draw world background on those areas that were not completely covered by opaque morphs"
	i > n  ifTrue: [
		newRectsToRepair do: [ :r |
			world privateDrawOn: (canvas newClipRect: r) ]].

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
"		(canvas newClipRect: rr) fullDraw: m"
		"*make this true to flash damaged areas for testing*"
		Preferences debugShowDamage2 ifTrue: [
			world flashRects: xrects color: Color random ].
	].

	"Answer a list of rectangles to be updated on the Display.
	This usually is not performance critical, drawing morphs is slower than just exposing the Display."
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !

!WorldState methodsFor: 'drawing' stamp: 'pb 12/31/2017 17:06:30'!
simpleDrawInvalidAreasSubmorphs: submorphs

	"mover todo esto al canvas, no?
	Y ver que hacer con los argumentos, etc.... Toda esta bananarama!!"

	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	This simple implementation just does as requested: No optimizations are done."

	| rectsToRepair morphBounds |
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ damageRecorder invalidRectsFullBounds: world viewBox.
	damageRecorder reset.

	"Draw World"
	rectsToRepair do: [ :r |
		world privateDrawOn: (canvas newClipRect: r) ].

	"Draw morphs"
	submorphs reverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(canvas newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'pb 12/31/2017 17:06:08'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	| oldClipRect |
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [ Preferences cheapWindowReframe ]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].
	
	" --------------------------------------------------" 
	"Aca es donde hay que dibujar primero el morph, y despues los submorphs, sabiendo los bounds hallados al dibujar el morph!!!!!!!!!! (en M3)
	este es el mismo metodo que hace into: y outOfMorph    :D  "
	" --------------------------------------------------"
	"preferiria ni preguntar... para M3 al menos"
	self isCurrentMorphVisible ifTrue: [
		currentMorph privateDrawOn: self ].

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	currentMorph hasSubmorphs ifTrue: [
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self currentOwnerIfClips: currentMorph.
			oldClipRect _ self clipRect.
			self setClipRect: (oldClipRect intersect: self clippingRectForCurrentMorph).
			self fullDraw: clipped.
			self setClipRect: oldClipRect.
			self currentOwnerIfClips: nil ].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ].
		].
	self outOfMorph! !

!classDefinition: #Morph category: #'Morphic-Kernel'!
Object subclass: #Morph
	instanceVariableNames: 'owner submorphs location layoutNeeded layoutSpec properties drawingProxy'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!




