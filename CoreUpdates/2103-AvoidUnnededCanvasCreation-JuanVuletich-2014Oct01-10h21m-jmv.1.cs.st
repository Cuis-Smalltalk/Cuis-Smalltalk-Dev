'From Cuis 4.2 of 25 July 2013 [latest update: #2100] on 1 October 2014 at 11:06:26.487148 am'!

!FormCanvas methodsFor: 'testing' stamp: 'jmv 10/1/2014 10:23'!
isFullMorphVisible: aMorph

	| o worldBounds |
	worldBounds _ aMorph morphFullBoundsInWorld.
	worldBounds ifNil: [ ^false ].

	o _ self canvasOrigin.
	"Optimization"
	(worldBounds right + o x) < clipRect left		ifTrue: [^ false].
	(worldBounds left + o x) > clipRect right		ifTrue: [^ false].
	(worldBounds bottom + o y) < clipRect top		ifTrue: [^ false].
	(worldBounds top + o y) > clipRect bottom		ifTrue: [^ false].
	^ true
! !

!FormCanvas methodsFor: 'testing' stamp: 'jmv 10/1/2014 10:23'!
isMorphVisible: aMorph

	| o worldBounds |
	worldBounds _ aMorph morphBoundsInWorld.
	worldBounds ifNil: [ ^false ].

	o _ self canvasOrigin.
	"Optimization"
	(worldBounds right + o x) < clipRect left		ifTrue: [^ false].
	(worldBounds left + o x) > clipRect right		ifTrue: [^ false].
	(worldBounds bottom + o y) < clipRect top		ifTrue: [^ false].
	(worldBounds top + o y) > clipRect bottom		ifTrue: [^ false].
	^ true
! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 10/1/2014 10:39'!
invalidateDisplayRect: damageRect from: aMorph

	| clippedRect b |

	"warning. Senders are using global coordinates. Redesign!!"
	"local now!!!!!!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

	clippedRect _ damageRect.
	aMorph ifNotNil: [
	 	aMorph == self clippedSubmorph
			ifTrue: [
				b _ self morphBoundsInWorld.
				b ifNil: [ ^self ].
				clippedRect _ damageRect intersect: b ]].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect from: self ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/1/2014 11:02'!
morphBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."

	self flag: #jmvVer2.
	self visible ifFalse: [ ^nil ].
	^self world ifNotNil: [ :w | w canvas displayBoundsOf: self ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/1/2014 10:34'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."

	self flag: #jmvVer2.
	self visible ifFalse: [ ^nil ].
	^self world ifNotNil: [ :w | w canvas displayFullBoundsOf: self ]! !

!Morph methodsFor: 'macpal' stamp: 'jmv 10/1/2014 10:37'!
flash

	self morphBoundsInWorld ifNotNil: [ :r |
		Display flash: r ]! !

!Morph methodsFor: 'updating' stamp: 'jmv 10/1/2014 10:44'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Include submorphs if we don't clip!!
	Think about it. We don't to know about a specific display rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"

	self morphBoundsInWorld ifNotNil: [ :r |
		self invalidateDisplayRect: r from: nil.
		"caro al pedo en casi todoso los casos..."
		self unclippedSubmorphsReverseDo: [ :m | m redrawNeeded ]]! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 10/1/2014 10:43'!
morphExtent: newExtent
	"assume it is always in our coordinates!!"
	| oldBoundsInWorld |
	self flag: #jmvVer2.
	extent = newExtent ifFalse: [
		"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
		oldBoundsInWorld _ self morphBoundsInWorld.
		(self privateExtent: newExtent) ifTrue: [
			"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
			oldBoundsInWorld ifNotNil: [
				self invalidateDisplayRect: oldBoundsInWorld from: nil ].
			self someSubmorphPositionOrExtentChanged.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 10/1/2014 10:45'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| oldBoundsInWorld someChange |

	"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
	oldBoundsInWorld _ self morphBoundsInWorld.
	someChange _ false.
	location position = newPos ifFalse: [
		location setPosition: newPos.
		someChange _ true ].

	extent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			someChange _ true ]].

	someChange ifTrue: [
		"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
		oldBoundsInWorld ifNotNil: [
			self invalidateDisplayRect: oldBoundsInWorld from: nil ].
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !


!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 10/1/2014 10:51'!
privateExtent: newExtent

	| r |
	^ (super privateExtent: newExtent)
		ifTrue: [
			self buildMagnifiedBackgroundImage.

			worldState ifNotNil: [
				worldState viewBox ifNotNil: [
					r _ self morphBoundsInWorld.
					worldState viewBox = r ifFalse: [
						worldState clearCanvas.
						worldState viewBox: r ]]]]; yourself! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 10/1/2014 11:05'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph morphExtent.
	delta _ trialRect amountToTranslateWithin: self morphBoundsInWorld.
	self addMorph: aMorph.
	aMorph morphPositionInWorld: trialRect origin + delta.! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 10/1/2014 10:58'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included."
	 | bounds radious |
	color mightBeTranslucent ifTrue: [
		aCollection add: aRectangle.
		^self ].

	bounds _ self morphBoundsInWorld.
	bounds ifNil: [
		aCollection add: aRectangle.
		^self ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!!"
	Theme current roundWindowCorners ifFalse: [
		aRectangle areasOutside: bounds do: [ :rect |  aCollection add: rect ].
		^self ].

	"The solid rectangle does not include the corners.
	Report a couple of rows (top and bottom) or columns (left and right) as uncovered areas.
	We could also try to be more careful and answer each rounded corner...
	Right now, report top and bottom rows as uncovered areas"
	radious _ Theme current roundedWindowRadius.
	aRectangle areasOutside: (bounds insetBy: 0@radious) do: [ :rect |  aCollection add: rect ]! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/1/2014 10:27'!
fullDrawHandOn: aCanvas
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."

	submorphs isEmpty ifTrue: [^ self drawOn: aCanvas].  "just draw the hand itself"

	Preferences cheapWindowReframe ifFalse: [
		"Note: We use a shadow form here to prevent drawing
		overlapping morphs multiple times using the transparent
		shadow color."
		self submorphsShadowForm ifNotNil: [ :shadowForm |
			"draw shadows"
			aCanvas stencil: shadowForm at: shadowForm offset  + self shadowOffset color: (Color black alpha: 0.5) ]].
	
	"draw morphs in front of shadows"
	submorphs reverseDo: [ :m | aCanvas fullDraw: m ].
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/1/2014 10:24'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| fullBounds |
	fullBounds _ self morphFullBoundsInWorld.
	fullBounds ifNil: [ ^self ].

	fullBounds _ fullBounds origin corner: fullBounds corner + self shadowOffset.
	fullBounds _ fullBounds intersect: (0@0 extent: aCanvas extent).
	(savedPatch isNil or: [savedPatch extent ~= fullBounds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBounds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: fullBounds
		into: savedPatch.
	savedPatch offset: fullBounds topLeft.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBounds ]
		ifNotNil: [ aStream nextPut: (fullBounds merge: prevFullBounds)].
	prevFullBounds _ fullBounds! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/1/2014 10:27'!
submorphsShadowForm
	"Return a 1-bit shadow of my submorphs.  Assumes submorphs is not empty"
	| bnds canvas |
	bnds _ Rectangle merging: (submorphs collect: [:m | m morphFullBoundsInWorld]).
	bnds ifNil: [ ^nil ].

	canvas _ FormCanvas forShadowOver: bnds.
	canvas into: self.
	submorphs reverseDo: [ :m | canvas fullDraw: m ].
	^ canvas form offset: bnds topLeft - self morphPositionInWorld! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 10/1/2014 10:52'!
initialFrameFor: aView initialExtent: initialExtent world: aWorld 
	"Find a plausible initial screen area for the supplied view, which should be a StandardSystemView, taking into account the 'reverseWindowStagger' Preference, the size needed, and other windows currently on the screen."

	| allOrigins screenRight screenBottom putativeOrigin putativeFrame allowedArea staggerOrigin otherFrames |
	Preferences reverseWindowStagger 
		ifTrue: 
			[^self 
				strictlyStaggeredInitialFrameFor: aView
				initialExtent: initialExtent
				world: aWorld].
	allowedArea := self maximumUsableAreaInWorld: aWorld.
	screenRight := allowedArea right.
	screenBottom := allowedArea bottom.
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [ :w | w visible and: [w isCollapsed not]]) 
						collect: [:w | w morphBoundsInWorld].
	allOrigins := otherFrames collect: [:f | f origin].
	(self standardPositionsInWorld: aWorld) do: [ :aPosition | 
			"First see if one of the standard positions is free"
			(allOrigins includes: aPosition) 
				ifFalse: [
					^(aPosition extent: initialExtent) translatedAndSquishedToBeWithin: allowedArea]].
	staggerOrigin := (self standardPositionsInWorld: aWorld) first.	"Fallback: try offsetting from top left"
	putativeOrigin := staggerOrigin.
	
	[putativeOrigin := putativeOrigin + StaggerOffset.
	putativeFrame := putativeOrigin extent: initialExtent.
	putativeFrame bottom < screenBottom 
		and: [putativeFrame right < screenRight]] 
			whileTrue: [
				(allOrigins includes: putativeOrigin) 
					ifFalse: [
						^(putativeOrigin extent: initialExtent) 
							translatedAndSquishedToBeWithin: allowedArea]].
	^(self scrollBarSetback @ self screenTopSetback extent: initialExtent) 
		translatedAndSquishedToBeWithin: allowedArea! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 10/1/2014 10:53'!
strictlyStaggeredInitialFrameFor: aStandardSystemView initialExtent: initialExtent world: aWorld 
	"This method implements a staggered window placement policy that I (di) like.
	Basically it provides for up to 4 windows, staggered from each of the 4 corners.
	The windows are staggered so that there will always be a corner visible."

	| allowedArea grid initialFrame otherFrames cornerSel corner delta putativeCorner free maxLevel |
	allowedArea := (self maximumUsableAreaInWorld: aWorld) 
				insetBy: (self scrollBarSetback @ self screenTopSetback extent: 0 @ 0).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel := allowedArea area > 300000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid := allowedArea area > 500000 ifTrue: [40] ifFalse: [20].
	initialFrame := 0 @ 0 extent: initialExtent.
	"min: (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))))
							min: 600@400"
	otherFrames := (SystemWindow windowsIn: aWorld satisfying: [ :w | w visible and: [w isCollapsed not]]) 
						collect: [:w | w morphBoundsInWorld].
	0 to: maxLevel do: [ :level | 
			1 to: 4 do: [:ci | 
					cornerSel := #(#topLeft #topRight #bottomRight #bottomLeft) at: ci.
					corner := allowedArea perform: cornerSel.
					"The extra grid//2 in delta helps to keep title tabs distinct"
					delta := ((maxLevel - level) * grid + (grid // 2)) @ (level * grid).
					1 to: ci - 1 do: [ :i | delta _ delta y negated @ delta x ].	"slow way"
					putativeCorner := corner + delta.
					free := true.
					otherFrames do: [ :w |
						free := free & ((w perform: cornerSel) ~= putativeCorner)].
					free 
						ifTrue: [
							^(initialFrame aligned: (initialFrame perform: cornerSel)
								with: putativeCorner) translatedAndSquishedToBeWithin: allowedArea]]].
	"If all else fails..."
	^(self scrollBarSetback @ self screenTopSetback 
		extent: initialFrame extent) translatedAndSquishedToBeWithin: allowedArea! !


!TextComposition methodsFor: 'editing' stamp: 'jmv 10/1/2014 10:59'!
clickAt: clickPoint
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	| startBlock action target range boxes box t |
	action _ false.
	startBlock _ self characterBlockAtPoint: clickPoint.
	t _ model actualContents.
	(t attributesAt: startBlock stringIndex) do: [ :att | 
		att mayActOnClick ifTrue: [
				(target _ model) ifNil: [ target _ editor morph].
				range _ t rangeOf: att startingAt: startBlock stringIndex.
				boxes _ self selectionRectsFrom: (self characterBlockForIndex: range first) 
							to: (self characterBlockForIndex: range last+1).
				box _ boxes detect: [ :each | each containsPoint: clickPoint] ifNone: nil.
				box ifNotNil: [
					box _ editor morph morphBoundsInWorld.
					editor morph allOwnersDo: [ :m | box _ box intersect: (m morphBoundsInWorld) ].
					Utilities
						awaitMouseUpIn: box
						repeating: nil
						ifSucceed: [(att actOnClickFor: target in: self at: clickPoint editor: editor) ifTrue: [action _ true]].
				]]].
	^ action! !

