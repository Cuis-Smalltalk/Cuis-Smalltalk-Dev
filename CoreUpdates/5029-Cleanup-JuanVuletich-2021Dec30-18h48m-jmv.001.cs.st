'From Cuis 5.0 [latest update: #5027] on 30 December 2021 at 6:50:11 pm'!

!PluggableScrollPane commentStamp: 'jmv 12/30/2021 18:49:33' prior: 0!
Allows viewing just part of a larger Morph. The scroll values vary from 0.0 to 1.0.

| p |
p := PluggableScrollPane new.
p scroller: BoxedMorph new.
p openInWorld.

| p |
p := PluggableScrollPane new.
p scroller: Sample01Star new.
p openInWorld.!


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 12/30/2021 18:49:17'!
allInstVarNamesEverywhere
	"Answer the set of inst var names used by the receiver, all superclasses, and all subclasses"

	| aList |
	aList _ OrderedCollection new.
	(self allSuperclasses , self withAllSubclasses asOrderedCollection) do:
		[:cls | aList addAll: cls instVarNames].
	^ aList asSet

	"BoxedMorph allInstVarNamesEverywhere"! !


!Morph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/30/2021 18:49:00'!
aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^nil  		"PlacedMorphs can be grabbed and moved around with the hand"! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 18:49:07'!
morphPosition
	"Answer our position inside our owner, in owner's coordinates.
	Only subclasses of PlacedMorph actually have a position."

	^ 0@0! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 12/30/2021 18:49:20'!
collides: aMorph
	"Answer whether the pixels used by morphs touch at least at one place.
	Morphs not in the BoxedMorph hierarchy should answer #true to wantsContour."

	privateDisplayBounds ifNil: [
		^false ].

	"Quick check with display bounds"
	(aMorph displayBoundsIntersects: privateDisplayBounds) ifFalse: [
		^false ].

	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | contourTop contourBottom |
		contourTop _ self valueOfProperty: #contourY0.
		contourBottom _ self valueOfProperty: #contourY1.
		^ aMorph contourIntersects: contour top: contourTop bottom: contourBottom ].

	"If contour is not available, and both displayBounds overlap, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 12/30/2021 18:49:22'!
coversPixel: worldPoint
	"Answer true if pixel worldPoint is covered by us, even if we are not visible a it because of some
	other morph above us also covers it.

	WARNING: Might answer nil when we simply don't know. It is generally advisable to call this method only for
	morphs where the answer is not nil:
		- BoxedMorphs. No sepecial care needed. (Senders in the base Cuis image are of this kind).
		- Morphs drawn by VectorCanvas (i.e. they #requiresVectorCanvas). See notes below.
	
	Note: If your morph #requiresVectorCanvas, and depends on this general implementation, remember to 
	`removeProperty: #bitMask` whenever it becomes invalid due to changes in your morphs. You may consider
	using #ownsPixel: if appropriate, that doesn't require any maintenance and is cheaper (in cpu and memory).
	See #bitMask.

	See #ownsPixel:
	See #includesPixel:"

	self visible ifTrue: [
		"Expensive way for morphs with arbitrary shape in VectorGraphics.
		The base Cuis System doesn't use this.
		Use this in applications if the expense of maintaining #bitMask is worth it."
		self canvas ifNotNil: [ :canvas |
			canvas canDoVectorGraphics ifTrue: [
				^(self bitMask pixelValueAt: worldPoint - self displayFullBounds topLeft) = 1 ]].

		"This is cheap and OK for unrotated Morphs with a rectangular shape.
		In general, the answer is not strictly correct.
		BoxedMorph redefines this method with an equally cheap implementation that can also handle
		rotated morphs (but assuming they are of rectangular shape)."
		self basicDisplayBounds ifNotNil: [ :r |
			(r containsPoint: worldPoint) ifFalse: [ ^false ] ]].

	"If we don't know."
	^ nil.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 12/30/2021 18:49:24'!
includesPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.

	Some implementations (BoxedMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	For other morphs, not BoxedMOrph, (i.e. they run this implementation), if VectorGraphics is not active,
	answer true for any point that lies within our bounds. If this is the case, consider using VectorGraphics: In addition to
	many other advantages, this method will always answer a strictly correct answer.

	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph
	covers us. A common case is to find the topmost morph at some position. In that case, iterating front to back ensures
	that if any other morph covers us, it is found first.

	Note: This method is redefined by some subclasses. This implementation is only used for morphs drawn by VectorCanvas,
	either because the main canvas is a VectorCanvas, or because it is a HybridCanvas and we answer true to #requiresVectorCanvas.
	(See other implementors)

	Note: Also see #ownsPixel: and #coversPixel:"

	"Check if #ownsPixel:, if possible."
	self canvas ifNotNil: [ :canvas |
		canvas canDoVectorGraphics ifTrue: [
			^ (canvas morphIdAt: worldPoint) = self morphId ]].

	"This is cheap and OK for unrotated Morphs with a rectangular shape.
	In general, the answer is not strictly correct. (There are false positives).
	BoxedMorph redefines this method with an equally cheap implementation that can also handle
	rotated morphs (but assuming they are of rectangular shape)."
	self visible ifTrue: [
		self basicDisplayBounds ifNotNil: [ :r |
			^r containsPoint: worldPoint ]].

	^ false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 12/30/2021 18:49:26'!
isCloserThan: maxDistance toPixel: worldPoint
	"Answer true if our closest point to aPoint is less than aNumber pixels away.
	In target surface (i.e. Display) coordinates.
	Uses precise testing of the morph contour if available:
	Morphs not in the BoxedMorph hierarchy should answer #true to wantsContour.
	Note: Considers only the external border. Any inner pixel is considered 'inside' regardless of us being transparent there.
	Note: Cheaper than #coversAnyPixelCloserThan:to: . Doesn't use #bitMask. Doesn't require maintenance."

	| center contourTop contourBottom |
	privateDisplayBounds ifNil: [
		^false ].
	center _ privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - worldPoint y) abs < (privateDisplayBounds height  // 2 + maxDistance) ifFalse: [
		^false ].
	(center x - worldPoint x) abs < (privateDisplayBounds width // 2 + maxDistance) ifFalse: [
		^false ].
	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | y0 y1 x0 x1 |
		contourTop _ self valueOfProperty: #contourY0.
		contourBottom _ self valueOfProperty: #contourY1.
		"Contour rows to consider are those within requested distance."
		y0 _ worldPoint y - maxDistance max: contourTop.
		y1 _ worldPoint y + maxDistance min: contourBottom.
		y0 to: y1 do: [ :y |
			x0 _ (contour at: (y - contourTop) * 2 + 1) - maxDistance.
			x1 _ (contour at: (y - contourTop) * 2 + 2) + maxDistance.
			"If a vertical line of 2*aNumber height centered on aPoint is inside the contour, quick exit"
			(worldPoint x between: x0 and: x1) ifTrue: [ ^true ].
			"Check if aPoint is close enough to contour"
			(x0@y - worldPoint) r < maxDistance ifTrue: [ ^true ].
			(x1@y - worldPoint) r < maxDistance ifTrue: [ ^true ]].
		"Not inside, not close enough to contour"
		^ false ].
	"If contour is not available, and aPoint is close enough to displayBounds, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'stepping' stamp: 'jmv 12/30/2021 18:49:29'!
stepAt: millisecondSinceLast
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. 
	The desired time between steps is specified by this morph's answer to the stepTime message.
	The millisecondSinceLast parameter gives the time elapsed since the previous step."
	"
	m _ BoxedMorph new.
	m color: Color random.
	m openInWorld.
	m morphPosition: 10@10.
	t _ 0.
	m when: #morphicStep evaluate: [ :delta |
		t _ t + delta.
		t < 10000
			ifTrue: [
				(m owner is: #HandMorph) ifFalse: [
					m morphPosition: 3@2 * t // 100 ]]
			ifFalse: [ m stopStepping ]].
	m startSteppingStepTime: 20.
	"
	self step.
	self triggerEvent: #morphicStep with: millisecondSinceLast! !

