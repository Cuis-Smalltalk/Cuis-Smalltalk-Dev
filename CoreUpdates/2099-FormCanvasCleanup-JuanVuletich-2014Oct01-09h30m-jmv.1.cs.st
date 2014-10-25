'From Cuis 4.2 of 25 July 2013 [latest update: #2098] on 1 October 2014 at 9:31:12.473132 am'!

!FormCanvas methodsFor: 'testing' stamp: 'jmv 9/30/2014 22:22'!
isFullMorphVisible: aMorph

	| o aRectangle |
	aRectangle _ aMorph morphFullBoundsInWorld.
	o _ self canvasOrigin.
	"Optimization"
	(aRectangle right + o x) < clipRect left		ifTrue: [^ false].
	(aRectangle left + o x) > clipRect right		ifTrue: [^ false].
	(aRectangle bottom + o y) < clipRect top	ifTrue: [^ false].
	(aRectangle top + o y) > clipRect bottom	ifTrue: [^ false].
	^ true
! !

!FormCanvas methodsFor: 'testing' stamp: 'jmv 9/30/2014 22:23'!
isMorphVisible: aMorph

	| o aRectangle |
	aRectangle _ aMorph morphBoundsInWorld.
	o _ self canvasOrigin.
	"Optimization"
	(aRectangle right + o x) < clipRect left		ifTrue: [^ false].
	(aRectangle left + o x) > clipRect right		ifTrue: [^ false].
	(aRectangle bottom + o y) < clipRect top	ifTrue: [^ false].
	(aRectangle top + o y) > clipRect bottom	ifTrue: [^ false].
	^ true
! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 9/30/2014 21:53'!
retrieveStringOrText
	"Answer a Text or a String. Appropriate for a Text Editor."
	| answer |
	
	answer _ self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^answer ].
	
	"If clipboard contains a Morph, answer a Text incuding its drawing..."
	(answer is: #Morph) ifTrue: [
		answer _ answer imageForm: 32 ].
	
	"If clipboard contains a Form, prepare a Text including it."
	(answer is: #Form) ifTrue: [
		^Text withForm: answer ].
	
	"Answer some string..."
	^answer ifNil: [ '' ] ifNotNil: [ answer printString ]! !


!DamageRecorder methodsFor: 'recording' stamp: 'jmv 9/30/2014 22:54'!
recordInvalidRect: requestedRect
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle.
	Rectangles are specified in world coordinates (might be not the same as Display coordinates if there is some origin set on some canvas. Usually there will be none, but we cant be sure)
	"
	"Details: Damaged rectangles are often identical or overlap significantly. In these cases, we merge them to reduce the number of damage rectangles that must be processed when the display is updated. Moreover, above a certain threshold, we ignore the individual rectangles completely, and simply do a complete repaint on the next cycle.
	jmv: Important. There should be no overlapping rectangles in the list. If there are, translucent morphs might be drawn several times, with visible defects.
	WRT performance, the different paths for various intersections seem a bit complicated. I could not find strong evidence of the best way.
	Leave it as it is right now."

	| newRect mergeRect a indexToReuse |
	totalRepaint ifTrue: [^ self].  "planning full repaint; don't bother collecting damage"

	indexToReuse _ nil.
	newRect _ requestedRect truncated.
	invalidRects doWithIndex: [ :rect :index |
		rect ifNil: [
			indexToReuse ifNil: [ indexToReuse _ index ]]
		ifNotNil: [
			"No two rectangles should intersect"
			(rect intersects: newRect) ifTrue: [
				"requestedRect (possibly smaller than newRect) already in. Nothing to do then."
				(rect containsRect: requestedRect) ifTrue: [ 
					^self ].
				"Some rect included in newRect. Remove it and continue, as newRect could still intersect others."
				(newRect containsRect: rect) ifTrue: [
					invalidRects at: index put: nil.	"Effectively like 'invalidRects remove: rect', but without performance penalty."
					indexToReuse ifNil: [ indexToReuse _ index ]].			
				"Merge rectangles if they overlap significantly"
				a _ (rect intersect: newRect) area.
				(a > 40
					and: ["Avoid combining a vertical and horizontal rects.
						  Can make a big diff and we only test when likely."
						  a > (newRect area // 4) or: [ a > (rect area // 4)]])
				ifTrue: [
					"Merge into newRect, as any overlap with forecoming rectangles in the iteration is detected and treated."
					"Before modifying newRect, ensure making a copy: we don't want to break rectangles that belong to someone else (i.e. some morph bounds)"
					newRect == requestedRect ifTrue: [
						newRect _ (rect origin min: newRect origin) corner: (rect corner max: newRect corner) ]
					ifFalse: [
						newRect
							setOrigin: (rect origin min: newRect origin)
							corner: (rect corner max: newRect corner) ].
					invalidRects at: index put: nil.	"Effectively like 'invalidRects remove: rect', but without performance penalty."
					indexToReuse ifNil: [ indexToReuse _ index ]]
				ifFalse: [
					"Avoid intersections!!"
					newRect areasOutside: rect do: [ :nonIntersection |
						"We have brand new newRects. Start all over with each of them."
						self recordInvalidRect: nonIntersection ].
					"newRect no longer needed, then."
					^self ]]]].

	invalidRects size >= 15 ifTrue: [
		"if there are too many separate areas, merge them all"
		mergeRect _ Rectangle merging: invalidRects.
		self reset.
		mergeRect ifNotNil: [
			invalidRects addLast: mergeRect ].
		indexToReuse _ nil ].

	"Ensure making a copy: we don't want to break rectangles that belong to someone else (i.e. some morph bounds)
	(note: no need to send #truncated, because if that was needed, we'd already have a truncated copy!!)"
	newRect == requestedRect ifTrue: [
		newRect _ requestedRect origin corner: requestedRect corner ].
	"add the given rectangle to the damage list"
	indexToReuse
		ifNil: [
			invalidRects addLast: newRect ]
		ifNotNil: [
			invalidRects at: indexToReuse put: newRect ]! !


!FormCanvas methodsFor: 'accessing' stamp: 'jmv 9/30/2014 22:57'!
clipRect
	"Return the currently active clipping rectangle"
	"In form coordinates"

	^ clipRect! !

!FormCanvas methodsFor: 'accessing' stamp: 'jmv 9/30/2014 22:06'!
contentsOfArea: aRectangle into: aForm
	| bb |
	bb _ BitBlt toForm: aForm.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	^aForm! !

!FormCanvas methodsFor: 'accessing' stamp: 'jmv 9/30/2014 22:49'!
newClipRect: aRectangleOrNil
	"aRectangle is in world coordinates.
	But ivar clipRect is relative to the form,
	For example, if we had previously been built like
		aCanvas on: someForm over: (100@100 extent 200@100)
	then our origin would be -100 @ -100.
	Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting
	only (20@20 extent: 40@30) in our form"

	self setClipRect: (aRectangleOrNil
		ifNil: [ 0@0 corner: form extent ]
		ifNotNil: [ aRectangleOrNil translatedBy: self canvasOrigin ])! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 22:24'!
canvasOrigin
	"Return the current origin for drawing operations"
	^ transformations first translation! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 22:55'!
setClipRect: aRectangle
	"In form coordinates"

	clipRect _ aRectangle.
	port clipRect: aRectangle! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 9/30/2014 22:26'!
setForm: aForm

	form _ aForm.
	self resetGrafPort.
	self newClipRect: nil.
	shadowColor _ nil! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 9/30/2014 22:57'!
clippingRectForCurrentMorph
	"In form coordinates"

"En M3, es el obtenido al dibujarlo, recien.
Dejar eso claro en el nombre. Eliminar 'clipping'"
	^self displayBoundsOf: currentMorph! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 9/30/2014 22:19'!
drawCurrentAsOutline

	(self isMorphVisible: currentMorph) ifTrue: [
		self
			reverseRectangleBorder: (0 @ 0 extent: currentMorph morphExtent)
			borderWidth: 2 ]! !

!FormCanvas methodsFor: 'morphic' stamp: 'jmv 9/30/2014 22:21'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	| oldClipRect |
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	"preferiria ni preguntar... para M3 al menos"
	(self isFullMorphVisible: currentMorph) ifFalse: [
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
	(self isMorphVisible: currentMorph) ifTrue: [
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


!FormCanvas class methodsFor: 'instance creation' stamp: 'jmv 9/30/2014 22:36'!
depth: depth over: aRectangle

	^self on: (Form extent: aRectangle extent depth: depth) over: aRectangle! !

!FormCanvas class methodsFor: 'instance creation' stamp: 'jmv 9/30/2014 22:35'!
forShadowOver: aRectangle
	"Color black represents one for 1bpp"

	^(self on: (Form extent: aRectangle extent depth: 1) over: aRectangle) 
		shadowColor: Color black! !


!Morph methodsFor: 'drawing' stamp: 'jmv 9/30/2014 21:54'!
imageForm: depth
	| canvas |
	canvas _ FormCanvas depth: depth over: self morphFullBoundsInWorld.
	canvas fullDraw: self.
	^ canvas form! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/30/2014 22:06'!
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
		contentsOfArea: fullBounds
		into: savedPatch.
	savedPatch offset: fullBounds topLeft.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBounds ]
		ifNotNil: [ aStream nextPut: (fullBounds merge: prevFullBounds)].
	prevFullBounds _ fullBounds! !


!Utilities class methodsFor: 'common requests' stamp: 'jmv 9/30/2014 21:52'!
saveScreenshot
	"Make a screenshot of the world and save it to a file"

	"SampledSound playSoundNamed: 'camera'."
	(self runningWorld imageForm: 32) writeBMPfileNamed: 'CuisWorld.bmp'! !

!methodRemoval: MagnifierMorph #hasTranslucentColor!
MagnifierMorph removeSelector: #hasTranslucentColor!
!methodRemoval: Morph #imageForm:forRectangle:!
Morph removeSelector: #imageForm:forRectangle:!
!methodRemoval: FormCanvas #formWithOffset!
FormCanvas removeSelector: #formWithOffset!
!methodRemoval: FormCanvas #isVisible:!
FormCanvas removeSelector: #isVisible:!

!FormCanvas reorganize!
('accessing' clipRect contentsOfArea:into: currentTransformation depth extent form newClipRect: shadowColor:)
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol: reverseRectangleBorder:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:at:font:color: drawString:from:to:at:font:color: drawStringEmbossed:at:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('initialization' initialize)
('other' showAt:invalidRects:)
('printing' printOn:)
('testing' drawsOnDisplay isFullMorphVisible: isFullyVisible: isMorphVisible: isShadowDrawing)
('private' buildAuxWith:multipliedWith: canvasOrigin externalizeDisplayBounds:from: fillRectangle:tilingWith:sourceRect:rule: frameRectangle:borderWidth:topLeftColor:bottomRightColor: resetGrafPort setClipRect: setForm: setOrigin: setPaintColor:)
('morphic' clippingRectForCurrentMorph displayBoundsOf: displayFullBoundsOf: drawCurrentAsError drawCurrentAsOutline fullDraw: fullDrawHand: into: intoLocation: outOfLocation outOfMorph)
('drawing-icons' drawButtonIconFromCurrentMorph)
!

