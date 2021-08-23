'From Cuis 5.0 [latest update: #4797] on 23 August 2021 at 7:19:56 pm'!

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 8/23/2021 19:16:24'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included."

	 | bounds r e |
	self isOrAnyOwnerIsRotated ifTrue: [
		aCollection add: aRectangle.
		^self ].

	color mightBeTranslucent ifTrue: [
		aCollection add: aRectangle.
		^self ].

	bounds _ self displayBounds.
	bounds ifNil: [
		aCollection add: aRectangle.
		^self ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	aRectangle areasOutside: bounds do: [ :rect |  aCollection add: rect ].

	"Add area around rounded corners if needed."
	Theme current roundWindowCorners ifTrue: [
		r _ Theme current roundedWindowRadius.
		e _ r@r.
		aCollection add: (aRectangle intersect: (bounds topLeft extent: e)).
		aCollection add: (aRectangle intersect: (bounds topRight - (r@0) extent: e)).
		aCollection add: (aRectangle intersect: (bounds bottomLeft - (0@r)extent: e)).
		aCollection add: (aRectangle intersect: (bounds bottomRight - (r@r) extent: e)).
		].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/23/2021 19:19:51'!
drawWorld: aWorldMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| visibleRootMorphs visibleRootsDamage worldDamage |
	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	self boundsFinderCanvas updateBoundsIn: aWorldMorph addDamageTo: aDamageRecorder.

	visibleRootMorphs _ aWorldMorph privateSubmorphs select: [ :m | m visible ].
	visibleRootsDamage _ Array new: visibleRootMorphs size.

	worldDamage _ self computeDamage: aWorldMorph repair: aDamageRecorder
		rootMorphs: visibleRootMorphs rootMorphsDamage: visibleRootsDamage.

	self drawWorldBackground: aWorldMorph rects: worldDamage.
	"Debugging aids."
	"
	worldDamage do: [ :r | Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.005) wait].
	""
	visibleRootsDamage do: [ :r | r ifNotNil: [ Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.05) wait]].
	"
	self drawWorld: aWorldMorph
		rootMorphs: visibleRootMorphs rootMorphsDamage: visibleRootsDamage
		allDamage: worldDamage.

	aDamageRecorder reset.
	^ worldDamage! !

