'From Cuis 4.0 of 21 April 2012 [latest update: #1435] on 8 September 2012 at 11:01:28 pm'!

!FormCanvas methodsFor: 'initialization' stamp: 'jmv 9/8/2012 22:02'!
initialize
	super initialize.

	currentTransformation _ MatrixTransform2x3 identity.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ]! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 22:47'!
imageForm: depth forRectangle: rect
	| canvas |
	canvas _ Display defaultCanvasClass depth: depth over: rect.
	canvas fullDraw: self.
	^ canvas formWithOffset! !

!Morph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 22:44'!
ownShadowForm
	"Return a form representing the 'shadow' of the receiver, without including submorphs 
	regardless of clipping"
	| canvas |
	canvas _ Display defaultCanvasClass forShadowOver: self morphBoundsInWorld.
	canvas into: self.
	canvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ].
	^ canvas formWithOffset! !

!Morph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 22:45'!
shadowForm
	"Return a form representing the 'shadow' of the receiver - e.g., all pixels that are occupied by the receiver are one, all others are zero."
	| bnds canvas |
	bnds _ self morphFullBoundsInWorld.
	canvas _ Display defaultCanvasClass forShadowOver: bnds.
	canvas into: self.
	canvas fullDraw: self.
	^ canvas formWithOffset! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 22:58'!
restoreSavedPatchOn: aCanvas 
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas zzimage: savedPatch at: savedPatch offset.
		submorphs notEmpty ifTrue: [ ^self ].

		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		self invalidRect: (savedPatch offset extent: savedPatch extent + self shadowOffset).
		Sensor currentCursor == Cursor normal ifFalse: [ Cursor normal show ].	"show hardware cursor"
		savedPatch _ nil ]! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/8/2012 22:45'!
shadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	canvas _ Display defaultCanvasClass forShadowOver: bnds.
	canvas into: self.
	self drawSubmorphsOn: canvas.
	^ canvas formWithOffset! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 9/8/2012 22:54'!
canvas: aFormCanvas
	canvas _ aFormCanvas.
	self flag: #jmvVer2.
	"should be our world..."
	aFormCanvas ifNotNil: [
		aFormCanvas into: World ].
	damageRecorder
		ifNil: [ damageRecorder _ DamageRecorder new]
		ifNotNil: [ damageRecorder doFullRepaint]! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 9/8/2012 22:55'!
drawInvalidAreasWorld: aWorld submorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ OrderedCollection new.
	(damageRecorder invalidRectsFullBounds: aWorld viewBox) do: [ :r |
		r ifNotNil: [ initialRectsToRepair addLast: r ]].
	damageRecorder reset.
	currentRectsToRepair _ OrderedCollection new.
	newRectsToRepair _ OrderedCollection withAll: initialRectsToRepair.
	morphsToDraw _ OrderedCollection new.
	rectsForEachMorph _ OrderedCollection new.
	thisMorphRects _ OrderedCollection new.
	n _ submorphs size.
	i _ 1.
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

	i > n  ifTrue: [
		newRectsToRepair do: [ :r |
			(canvas copyClipRect: r) clipBy: aWorld morphBoundsInWorld during: [ :c | aWorld drawOn: c ]]].
	morphsToDraw with: rectsForEachMorph reverseDo: [ :m :xrects |
		"Here we could merge all xrects into just one call... Most likely, that would be slower, though."
"		rr _ nil."
		xrects do: [ :r |
"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
			(canvas copyClipRect: r) fullDraw: m
		].
"		(canvas copyClipRect: rr) fullDrawMorph: m "
	].
	
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !

!methodRemoval: FormCanvas #image:at:!
FormCanvas removeSelector: #image:at:!
!methodRemoval: FormCanvas #initTransformationsFor:!
FormCanvas removeSelector: #initTransformationsFor:!
