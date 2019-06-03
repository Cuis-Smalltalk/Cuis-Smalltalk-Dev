'From Cuis 5.0 of 7 November 2016 [latest update: #3777] on 1 June 2019 at 6:21:58 am'!
!classDefinition: #DamageRecorder category: #'Morphic-Support'!
Object subclass: #DamageRecorder
	instanceVariableNames: 'invalidRects totalRepaint drSemaphore '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!DamageRecorder methodsFor: 'initialization' stamp: 'pb 6/1/2019 06:02:18'!
initialize
	super initialize .
	invalidRects _ OrderedCollection new: 15.
	totalRepaint _ false! !

!DamageRecorder methodsFor: 'private' stamp: 'pb 6/1/2019 05:21:24'!
pvtAccessProtect
	^ drSemaphore ifNil: [drSemaphore := Semaphore forMutualExclusion]! !

!DamageRecorder methodsFor: 'private' stamp: 'pb 6/1/2019 06:00:14'!
pvtInnerRecordInvalidRect: requestedRect
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle.
	Rectangles are specified in world coordinates (might be not the same as Display coordinates if there is some origin set on some canvas. Usually there will be none, but we can't be sure)
	"
	"Details: Damaged rectangles are often identical or overlap significantly. In these cases, we merge them to reduce the number of damage rectangles that must be processed when the display is updated. Moreover, above a certain threshold, we ignore the individual rectangles completely, and simply do a complete repaint on the next cycle.
	jmv: Important. There should be no overlapping rectangles in the list. If there are, translucent morphs might be drawn several times, with visible defects.
	WRT performance, the different paths for various intersections seem a bit complicated. I could not find strong evidence of the best way.
	Leave it as it is right now."
	| newRect mergeRect indexToReuse |
	totalRepaint ifTrue: [ ^ self ].
	"planning full repaint; don't bother collecting damage"
	indexToReuse _ nil.
	newRect _ requestedRect truncated.
	invalidRects withIndexDo: [ :oldRect :index |
		oldRect
			ifNil: [ indexToReuse ifNil: [ indexToReuse _ index ]]
			ifNotNil: [
				"No two rectangles should intersect"
				(oldRect intersects: newRect) ifTrue: [
					"newRect already in. Nothing to do then."
					(oldRect containsRect: newRect) ifTrue: [ ^ self ].
					"Some oldRect included in newRect. Remove it and continue, as newRect could still intersect others."
					(newRect containsRect: oldRect) ifTrue: [
						invalidRects
							at: index
							put: nil.
						"Effectively like 'invalidRects remove: rect', but without performance penalty."
						indexToReuse ifNil: [ indexToReuse _ index ]].
					"Merge rectangles if they overlap significantly, i.e. if the merge is not much larger than separated rectangles."
					mergeRect _ (oldRect origin min: newRect origin) corner: (oldRect corner max: newRect corner).
					mergeRect area > (newRect area + oldRect area * 2)
						ifTrue: [
							"Avoid intersections!!"
							newRect
								areasOutside: oldRect
								do: [ :nonIntersection |
									"We have brand new newRects. Start all over with each of them."
									self pvtInnerRecordInvalidRect: nonIntersection ].
							"newRect no longer needed, then."
							^ self ]
						ifFalse: [
							"Merge into newRect, as any overlap with forecoming rectangles in the iteration is detected and treated."
							newRect _ mergeRect.
							invalidRects
								at: index
								put: nil.
							"Effectively like 'invalidRects remove: rect', but without performance penalty."
							indexToReuse ifNil: [ indexToReuse _ index ]]]]].
	invalidRects size >= 10 ifTrue: [
		"if there are too many separate areas, merge them all"
		mergeRect _ Rectangle merging: invalidRects.
		self pvtReset.
		mergeRect ifNotNil: [ invalidRects addLast: mergeRect ].
		indexToReuse _ nil ].
	"Add the given rectangle to the damage list"
	indexToReuse
		ifNil: [ invalidRects addLast: newRect ]
		ifNotNil: [
			invalidRects
				at: indexToReuse
				put: newRect ].! !

!DamageRecorder methodsFor: 'private' stamp: 'pb 6/1/2019 05:59:38'!
pvtReset
	"Clear the damage list."
	invalidRects removeAll.
	totalRepaint _ false.! !


!WorldState methodsFor: 'drawing' stamp: 'pb 6/1/2019 06:06:26'!
drawInvalidAreasSubmorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |

	"The simple implementation is slowers as it draws morph areas that will later be covered by other morphs.
	But it works, and it is easier to understand.
	See en.wikipedia.org/wiki/Painter's_algorithm"
	true not ifTrue: [ ^self simpleDrawInvalidAreasSubmorphs: submorphs ].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ damageRecorder invalidRectsFullBounds: world viewBox.
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
	Preferences debugShowDamage ifTrue: [ world fillRects: initialRectsToRepair ].

	"Draw world background on those areas that were not completely covered by opaque morphs"
	i > n  ifTrue: [
		newRectsToRepair do: [ :r |
			world drawOn: (canvas newClipRect: r) ]].

	"Now, draw the recorded areas of selected morphs, back to front."
	morphsToDraw with: rectsForEachMorph reverseDo: [ :m :xrects |
		"Here we could think about merging all xrects into just one call...
		This would mean drawing each morph just once.
		But that would mean drawing pixels we were not told to. There could be other morphs in that area that are not even being drawn!!
		See WorldState class >> #experiment1"
		"*make this true to flash damaged areas for testing*"
		Preferences debugShowDamage2 ifTrue: [
			world fillRects: xrects ].
"		rr _ nil."
		xrects do: [ :r |
"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
			(canvas newClipRect: r) fullDraw: m.
		].
"		(canvas newClipRect: rr) fullDraw: m"
	].

	"Answer a list of rectangles to be updated on the Display.
	This usually is not performance critical, drawing morphs is slower than just exposing the Display."
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !

!WorldState methodsFor: 'drawing' stamp: 'pb 6/1/2019 06:06:40'!
simpleDrawInvalidAreasSubmorphs: submorphs

	"mover todo esto al canvas, no?
	Y ver que hacer con los argumentos, etc.... Toda esta bananarama!!"

	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	This simple implementation just does as requested: No optimizations are done."

	| rectsToRepair morphBounds |
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ damageRecorder invalidRectsFullBounds: world viewBox.

	"Draw World"
	rectsToRepair do: [ :r |
		world drawOn: (canvas newClipRect: r) ].

	"Draw morphs"
	submorphs reverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(canvas newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !


!DamageRecorder methodsFor: 'recording' stamp: 'pb 6/1/2019 06:05:16'!
invalidRectsFullBounds: aRectangle
	"Return a collection of damaged rectangles for the given canvas. If a total repaint has been requested, return the given rectangle."
	"The collection answered should not be modified outside this method. In addition, it could contain nil objects, that should be ignored."
	| answer |
	answer _ totalRepaint
		ifTrue: [ Array with: aRectangle ].
		self pvtAccessProtect critical: [
			answer ifNil: [answer := invalidRects reject: [ :r |
					r isNil ]].
			self pvtReset].
	^ answer.! !

!DamageRecorder methodsFor: 'recording' stamp: 'pb 6/1/2019 05:39:59'!
recordInvalidRect: requestedRect
	^ self pvtAccessProtect critical: [ self pvtInnerRecordInvalidRect: requestedRect ]! !

!DamageRecorder methodsFor: 'testing' stamp: 'pb 6/1/2019 05:34:58'!
updateIsNeeded
	"Return true if the display needs to be updated.
	Note: This could give a false positive (i.e. answer true) if invalidRects is not empty but it only contains nils.
	Senders should be aware of this."
	^ totalRepaint or: [ self pvtAccessProtect critical: [invalidRects notEmpty] ].! !

!methodRemoval: DamageRecorder class #new!
DamageRecorder class removeSelector: #new!
!methodRemoval: DamageRecorder #reset!
DamageRecorder removeSelector: #reset!
!classDefinition: #DamageRecorder category: #'Morphic-Support'!
Object subclass: #DamageRecorder
	instanceVariableNames: 'invalidRects totalRepaint drSemaphore'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!DamageRecorder reorganize!
('initialization' initialize)
('recording' doFullRepaint invalidRectsFullBounds: recordInvalidRect:)
('testing' updateIsNeeded)
('private' pvtAccessProtect pvtInnerRecordInvalidRect: pvtReset)
!


!DamageRecorder class reorganize!
('as yet unclassified')
!

