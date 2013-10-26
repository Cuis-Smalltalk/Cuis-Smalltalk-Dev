'From Cuis 4.0 of 21 April 2012 [latest update: #1421] on 30 August 2012 at 10:39:38 pm'!
!classDefinition: #FormCanvas category: #'Morphic-Support'!
Object subclass: #FormCanvas
	instanceVariableNames: 'origin clipRect form port shadowColor cti currentTransformation transformations '
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms '
	poolDictionaries: ''
	category: 'Morphic-Support'!

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/30/2012 08:38'!
zzfillRectangle: aRectangle colorOrInfiniteForm: aColorOrInfiniteForm
	"Fill the given rectangle."

	| displayRectangle |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	^self fillRectangle: displayRectangle colorOrInfiniteForm: aColorOrInfiniteForm! !

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 8/30/2012 08:39'!
zzfillRectangle: aRectangle colorOrInfiniteForm: aColorOrInfiniteForm borderWidth: bw borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

	| displayRectangle |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	^self fillRectangle: displayRectangle colorOrInfiniteForm: aColorOrInfiniteForm borderWidth: bw borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 8/30/2012 22:21'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	"
	This is starting to work:
		| c |
		c _ Display getCanvas initTransformationsFor: World.
		World submorphsDo: [ :m | c fullDraw: m ].
	"

	"To replace #fullDrawOn:"
	self flag: #jmvVer3.

	aMorph visible ifFalse: [^ self].
	(self isVisible: aMorph morphFullBoundsInWorld) ifFalse: [^ self].		"Needs fullBounds 'in owner' if inside a scroller"
	aMorph isKnownFailing ifTrue: [ ^ aMorph drawErrorOn: self ].

	self into: aMorph.

	aMorph fullDrawOn: self.

	self outOf: aMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 8/30/2012 20:23'!
initTransformationsFor: aWorld

	currentTransformation _ aWorld location.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ]! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 8/30/2012 08:55'!
into: aMorph

	| last |
	cti _ cti + 1.
	transformations size < cti
		ifTrue: [	
			currentTransformation _ currentTransformation composedWith: aMorph location.
			transformations add: currentTransformation ]
		ifFalse: [
			"reuse the instance"
			last _ currentTransformation.
			currentTransformation _ transformations at: cti.
			last composedWith: aMorph location into: currentTransformation ]! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 8/30/2012 08:55'!
outOf: aMorph

	cti _ cti - 1.
	currentTransformation _ transformations at: cti! !


!Morph methodsFor: 'accessing' stamp: 'jmv 8/29/2012 22:16'!
location
	^location! !


!Morph methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:30'!
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front"
	submorphs isEmpty ifTrue: [ ^ self ].
	self clipsSubmorphs
		ifTrue: [
			aCanvas
				clipBy: self clippingBounds
				during: [ :clippedCanvas | 
					submorphs reverseDo:
						[ :m |  clippedCanvas fullDraw: m ] ] ]
		ifFalse: [
			submorphs reverseDo:
				[ :m |  aCanvas fullDraw: m ] ]! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:31'!
fullDrawOn: aCanvas
	"Draw the full Morphic structure on the given Canvas"

	"Draw receiver itself"
	(aCanvas isVisible: self morphBoundsInWorld) ifTrue: [
		aCanvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ]].	"Needs bounds 'in owner' if inside a scroller"
	self drawSubmorphsOn: aCanvas.
	self drawDropHighlightOn: aCanvas.
	self drawMouseDownHighlightOn: aCanvas! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:35'!
imageForm: depth forRectangle: rect
	| canvas |
	canvas _ Display defaultCanvasClass depth: depth over: rect.
	canvas initTransformationsFor: self world.
	canvas fullDraw: self.
	^ canvas formWithOffset! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:39'!
ownShadowForm
	"Return a form representing the 'shadow' of the receiver, without including submorphs 
	regardless of clipping"
	| canvas |
	canvas _ Display defaultCanvasClass forShadowOver: self morphBoundsInWorld.
	canvas initTransformationsFor: self world.
	canvas clipBy: self morphBoundsInWorld during: [ :c | self drawOn: c ].
	^ canvas formWithOffset! !

!Morph methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:35'!
shadowForm
	"Return a form representing the 'shadow' of the receiver - e.g., all pixels that are occupied by the receiver are one, all others are zero."
	| bnds canvas |
	bnds _ self morphFullBoundsInWorld.
	canvas _ Display defaultCanvasClass forShadowOver: bnds.
	canvas initTransformationsFor: self world.
	canvas fullDraw: self.
	^ canvas formWithOffset! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:31'!
fullDrawOn: aCanvas 
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."

	self nonCachingFullDrawOn: aCanvas.
! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:38'!
shadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	canvas _ Display defaultCanvasClass forShadowOver: bnds.
	canvas initTransformationsFor: self world.
	self drawSubmorphsOn: canvas.
	^ canvas formWithOffset! !


!WorldState methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:31'!
drawHand: aHandMorph

	| bw r |
	Preferences fastDragWindowForMorphic ifTrue: [
		bw _ HandMorph fastDragBorderWidth // 2.
		r _ aHandMorph morphFullBoundsInWorld.
		canvas frameRectangle: r borderWidth: bw color: Color black.
		canvas frameRectangle: (r insetBy: bw) borderWidth: bw color: Color white.
		canvas clipBy: aHandMorph morphBoundsInWorld during: [ :c | aHandMorph drawOn: c ]]
	ifFalse: [
		 canvas fullDraw: aHandMorph]! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 8/30/2012 22:29'!
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

	self flag: #jmvVer3.
	"Not sure if needed on every cycle... most likely not!! anyway, harmless"
	canvas initTransformationsFor: aWorld.

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

!classDefinition: #FormCanvas category: #'Morphic-Support'!
Object subclass: #FormCanvas
	instanceVariableNames: 'origin clipRect form port shadowColor transformations currentTransformation cti'
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms'
	poolDictionaries: ''
	category: 'Morphic-Support'!
