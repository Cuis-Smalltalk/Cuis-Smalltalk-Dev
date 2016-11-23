'From Cuis 4.2 of 25 July 2013 [latest update: #2940] on 30 October 2016 at 5:36:56 pm'!
!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent activeHand world newRectsToRepair rectsForEachMorph currentRectsToRepair morphsToDraw '
	classVariableNames: 'DeferredUIMessages '
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!WorldState methodsFor: 'private' stamp: 'pb 10/30/2016 17:36:00'!
recreateCollections
	currentRectsToRepair := OrderedCollection new.
	newRectsToRepair := OrderedCollection new.
	morphsToDraw := OrderedCollection new.
	rectsForEachMorph := OrderedCollection new.! !

!WorldState methodsFor: 'drawing' stamp: 'pb 10/30/2016 17:36:34'!
drawInvalidAreasWorld: aWorld submorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."
	"The simple implementation is slowers as it draws morph areas that will later be covered by other morphs.
	But it works, and it is easier to understand.
	See en.wikipedia.org/wiki/Painter's_algorithm"
	| initialRectsToRepair reuse i n morph morphBounds morphClipRect thisMorphRects |
	true not ifTrue: [ ^ self
			simpleDrawInvalidAreasWorld: aWorld
			submorphs: submorphs ].
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair := damageRecorder invalidRectsFullBounds: aWorld viewBox.
	damageRecorder reset.
	currentRectsToRepair ifNil: [ self recreateCollections ].
	currentRectsToRepair removeAll.
	newRectsToRepair removeAll.
	morphsToDraw removeAll.
	rectsForEachMorph removeAll.
	thisMorphRects := OrderedCollection new.
	newRectsToRepair addAll: initialRectsToRepair.
	n := submorphs size.
	i := 1.
	"The idea here is to iterate morphs front to back, i.e. first the ones on top.
	For each morph, record the rectangles it affects. And if a morph is opaque, remove the area behind it from the list of rectangles to be repaired.
	This means, for example, that morphs completely covered might even not be redrawn.
	this is a huge performance gain when there are many morphs on screen, especially if there are more than ten open windows, and Theme uses opaque colors.
	See 'A reverse painter's algorithm' at en.wikipedia.org/wiki/Painter's_algorithm"
	[
	i <= n and: [ newRectsToRepair notEmpty ]] whileTrue: [
		morph := submorphs at: i.
		morph visible ifTrue: [
			morphBounds := morph morphFullBoundsInWorld.
			reuse := currentRectsToRepair.
			currentRectsToRepair := newRectsToRepair.
			newRectsToRepair := reuse removeAll.
			currentRectsToRepair do: [ :r |
				(morphBounds intersects: r)
					ifTrue: [
						morphClipRect := morphBounds intersect: r.
						thisMorphRects add: morphClipRect.
						"We could perhaps try and join adjacent rectangles in this collection..."
						morph
							addPossiblyUncoveredAreasIn: r
							to: newRectsToRepair ]
					ifFalse: [ newRectsToRepair add: r ]].
			thisMorphRects ifNotEmpty: [
				morphsToDraw add: morph.
				rectsForEachMorph add: thisMorphRects.
				thisMorphRects := OrderedCollection new. ]].
		i := i + 1 ].
	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [
		aWorld
			fillRects: initialRectsToRepair
			color: Color gray ].
	"Draw world background on those areas that were not completely covered by opaque morphs"
	i > n ifTrue: [
		newRectsToRepair do: [ :r |
			aWorld drawOn: (canvas newClipRect: r) ]].
	"Now, draw the recorded areas of selected morphs, back to front."
	morphsToDraw
		with: rectsForEachMorph
		reverseDo: [ :m :xrects |
			"Here we could think about merging all xrects into just one call...
		This would mean drawing each morph just once.
		But that would mean drawing pixels we were not told to. There could be other morphs in that area that are not even being drawn!!
		See WorldState class >> #experiment1"
			"		rr _ nil."
			xrects do: [ :r |
				"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
				(canvas newClipRect: r) fullDraw: m ].
			"		(canvas newClipRect: rr) fullDraw: m"
			"*make this true to flash damaged areas for testing*"
			Preferences debugShowDamage2 ifTrue: [
				aWorld
					flashRects: xrects
					color: Color random ]].
	"Answer a list of rectangles to be updated on the Display.
	This usually is not performance critical, drawing morphs is slower than just exposing the Display."
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair.! !

!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent activeHand world currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects'
	classVariableNames: 'DeferredUIMessages'
	poolDictionaries: ''
	category: 'Morphic-Worlds'!
