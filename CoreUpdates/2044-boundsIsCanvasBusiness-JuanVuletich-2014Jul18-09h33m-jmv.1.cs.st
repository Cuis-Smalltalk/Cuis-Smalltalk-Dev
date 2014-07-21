'From Cuis 4.2 of 25 July 2013 [latest update: #2042] on 18 July 2014 at 11:42:39.603342 am'!

!FormCanvas methodsFor: 'private' stamp: 'jmv 7/18/2014 11:08'!
externalizeDisplayBounds: r from: aMorph
	"r is a Rectangle, expressed in aMorph's coordinate system.
	Answer another rectangle, that bounds r when translated to World coordinates"

	"Should translate only to whatever world or PasteUp we are displaying.
	Fix when implementing multiple Canvases (Displays) showing different
	(potentially nested Worlds)"
	| inOwners owner |
	self flag: #jmvVer2.

	inOwners _ aMorph location displayBoundsOfTransformOf: r.
	owner _ aMorph owner.
	^owner
		ifNotNil: [ self externalizeDisplayBounds: inOwners from: owner]
		ifNil: [ inOwners ]! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/18/2014 11:07'!
displayBoundsOf: aMorph
	"Answer a rectangle that completely bounds aMorph when drawn on our form.
	Might be larger than strictly required.
		- In Morphic 2, this could cause clipping artifacts. This doesn't usually happen because:
			a) Morphic 2 doesn't use scaling and rotation
			b) Most Morphic 2 morphs have rectangular shape.
		- In Morphic 3, clipping also considers the real shape of the owner morph. This avoids those artifacts."

	"Think about doing a BoundsFinderCanvas even for Morphic 2"
	self flag: #jmvVer2.

	^self externalizeDisplayBounds: aMorph clippingRect from: aMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/18/2014 11:41'!
displayFullBoundsOf: aMorph
	"Answer a rectangle that completely bounds aMorph and submorphs when drawn on our form.
	Might be larger than strictly required. See comment at #displayBoundsOf:"

	| r |
	"Think about doing a BoundsFinderCanvas even for Morphic 2"

	self flag: #jmvVer2.

	r _ self displayBoundsOf: aMorph.
	self unclippedSubmorphsReverseDo: [ :m |
		m visible ifTrue: [
			r _ r merge: (self displayFullBoundsOf: m) ]].

	^r! !


!FormCanvas methodsFor: 'accessing' stamp: 'jmv 7/18/2014 10:51'!
newClipRect: aRectangleOrNil
	"aRectangle is in canvas coordinates.
	But ivar clipRect is relative to the form,
	For example, if we had previously been built like
		aCanvas on: someForm over: (100@100 extent 200@100)
	then our origin would be -100 @ -100.
	Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting
	only (20@20 extent: 40@30) in our form"

	self setClipRect: (aRectangleOrNil
		ifNil: [ 0@0 corner: form extent ]
		ifNotNil: [ aRectangleOrNil translatedBy: self canvasOrigin ])! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/18/2014 11:40'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	| oldClipRect |
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	"preferiria ni preguntar... para M3 al menos"
	(self isVisible: currentMorph morphFullBoundsInWorld) ifFalse: [
		self outOfMorph.
		^ self].

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
	(self isVisible: currentMorph morphBoundsInWorld) ifTrue: [
		currentMorph drawOn: self ].

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	currentMorph hasSubmorphs ifTrue: [
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			currentOwnerIfClips _ currentMorph.
			oldClipRect _ clipRect.
			self setClipRect: (clipRect intersect: self clippingRectForCurrentMorph).
			self fullDraw: clipped.
			self setClipRect: oldClipRect.
			currentOwnerIfClips _ nil ].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ].
		].
	self outOfMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 7/18/2014 11:35'!
fullDrawHand: aHandMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	aHandMorph visible ifFalse: [^ self].
	self into: aHandMorph.
	aHandMorph fullDrawHandOn: self.
	self outOfMorph! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/18/2014 11:40'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."
	"Warning:
		Not #visible morphs will not be considered in recursion, but will answer if asked directly"

	| r |
	self flag: #jmvVer2.
	r _ self morphBoundsInWorld.
	self unclippedSubmorphsReverseDo: [ :m |
		m visible ifTrue: [
			r _ r merge: m morphFullBoundsInWorld ]].
	^r! !

!Morph methodsFor: 'layout' stamp: 'jmv 7/18/2014 11:28'!
submorphBounds
	"Private. Compute the actual full bounds of the receiver"

	| b inOwns |
	b _ nil.
	self unclippedSubmorphsReverseDo: [ :m |
		m visible ifTrue: [
			inOwns _ m location displayBoundsOfTransformOf: m  clippingRect.
			b _ b ifNil: [ inOwns ] ifNotNil: [ b quickMerge: inOwns ]]].
	^b! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/18/2014 11:32'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| fullBounds |
	fullBounds _ self morphFullBoundsInWorld.
	fullBounds _ fullBounds origin corner: fullBounds corner + self shadowOffset.
	fullBounds _ fullBounds intersect: (0@0 extent: aCanvas extent).
	(savedPatch isNil or: [savedPatch extent ~= fullBounds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBounds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: (fullBounds translatedBy: aCanvas canvasOrigin)
		into: savedPatch.
	savedPatch offset: fullBounds topLeft.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBounds ]
		ifNotNil: [ aStream nextPut: (fullBounds merge: prevFullBounds)].
	prevFullBounds _ fullBounds! !


!InnerPluggableMorph methodsFor: 'geometry' stamp: 'jmv 7/18/2014 11:26'!
adjustExtent
	self flag: #jmvVer2.	"Do it just adding submorph extents!!"
	self submorphBounds ifNotNil: [ :r |
		self morphExtent: r bottomRight ]! !


!WorldState methodsFor: 'hands' stamp: 'jmv 7/18/2014 11:31'!
selectHandsToDrawForDamage: damageList
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	| result hBnds |
	result _ OrderedCollection new.
	hands do: [:h |
		h needsToBeDrawn ifTrue: [
			h hasChanged
				ifTrue: [result add: h]
				ifFalse: [
					hBnds _ h morphFullBoundsInWorld.
					hBnds _ hBnds origin corner: hBnds corner + h shadowOffset.
					(damageList detect: [:r | r intersects: hBnds] ifNone: nil)
						ifNotNil: [result add: h]]]].
	^ result
! !

!methodRemoval: HandMorph #morphFullBoundsInWorld!
HandMorph removeSelector: #morphFullBoundsInWorld!

!FormCanvas reorganize!
('accessing' canvasOrigin clipRect contentsOfArea:into: currentTransformation depth extent form formWithOffset newClipRect: shadowColor:)
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol: reverseRectangleBorder:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:at:font:color: drawString:from:to:at:font:color: drawStringEmbossed:at:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('initialization' initialize)
('other' showAt:invalidRects:)
('printing' printOn:)
('testing' drawsOnDisplay isFullyVisible: isShadowDrawing isVisible:)
('private' buildAuxWith:multipliedWith: externalizeDisplayBounds:from: fillRectangle:tilingWith:sourceRect:rule: frameRectangle:borderWidth:topLeftColor:bottomRightColor: resetGrafPort setClipRect: setForm: setOrigin: setPaintColor:)
('morphic' clippingRectForCurrentMorph displayBoundsOf: displayFullBoundsOf: drawCurrentAsError drawCurrentAsOutline fullDraw: fullDrawHand: into: intoLocation: outOfLocation outOfMorph)
!

