'From Cuis 4.2 of 25 July 2013 [latest update: #2596] on 29 November 2015 at 9:35:33.8317 am'!

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 11/26/2015 10:47'!
invalidRectsFullBounds: aRectangle
	"Return a collection of damaged rectangles for the given canvas. If a total repaint has been requested, return the given rectangle."
	"The collection answered should not be modified outside this method. In addition, it could contain nil objects, that should be ignored."
	| answer |
	answer _ totalRepaint
		ifTrue: [ Array with: aRectangle ]
		ifFalse: [ invalidRects reject: [ :r | r isNil ]].
	^answer! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 11/26/2015 10:47'!
drawInvalidAreasWorld: aWorld submorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |

	"The simple implementation is slowers as it draws morph areas that will later be covered by other morphs.
	But it works, and it is easier to understand.
	See en.wikipedia.org/wiki/Painter's_algorithm"
	true not ifTrue: [ ^self simpleDrawInvalidAreasWorld: aWorld submorphs: submorphs ].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ damageRecorder invalidRectsFullBounds: aWorld viewBox.
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
	Preferences debugShowDamage ifTrue: [ aWorld fillRects: initialRectsToRepair color: Color gray ].

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
"		(canvas newClipRect: rr) fullDraw: m"
		"*make this true to flash damaged areas for testing*"
		Preferences debugShowDamage2 ifTrue: [
			aWorld flashRects: xrects color: Color random ].
	].

	"Answer a list of rectangles to be updated on the Display.
	This usually is not performance critical, drawing morphs is slower than just exposing the Display."
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 11/26/2015 10:47'!
simpleDrawInvalidAreasWorld: aWorld submorphs: submorphs

	"mover todo esto al canvas, no?
	Y ver que hacer con los argumentos, etc.... Toda esta bananarama!!"

	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	This simple implementation just does as requested: No optimizations are done."

	| rectsToRepair morphBounds |
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ damageRecorder invalidRectsFullBounds: aWorld viewBox.
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

