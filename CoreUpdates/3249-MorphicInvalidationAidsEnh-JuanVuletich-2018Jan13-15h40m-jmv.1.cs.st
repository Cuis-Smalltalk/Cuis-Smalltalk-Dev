'From Cuis 5.0 of 7 November 2016 [latest update: #3248] on 13 January 2018 at 3:55:27 pm'!

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 1/13/2018 15:49:22'!
fillRects: rectangleList
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."


	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: `0@0`;
		clipRect: self viewBox;
		combinationRule: Form over.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt fillColor: Color random.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect ].

	(Delay forMilliseconds: 50) wait! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 1/13/2018 15:47:59'!
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

!methodRemoval: PasteUpMorph #fillRects:color:!
PasteUpMorph removeSelector: #fillRects:color:!
!methodRemoval: PasteUpMorph #flashRects:color:!
PasteUpMorph removeSelector: #flashRects:color:!
