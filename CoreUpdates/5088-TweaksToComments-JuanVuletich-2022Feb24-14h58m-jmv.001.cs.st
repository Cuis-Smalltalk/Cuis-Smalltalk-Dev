'From Cuis 6.0 [latest update: #5087] on 24 February 2022 at 3:22:30 pm'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 2/24/2022 15:10:48'!
coversPixel: worldPoint
	"Answer true if pixel worldPoint is covered by us (or a submorph of ours), even if we are not visible a it
	because of some other morph above us also covers it.

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

!Morph methodsFor: 'geometry services' stamp: 'jmv 2/24/2022 15:20:12'!
includesPixel: worldPoint
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:

	- Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.

	- Some implementations (BoxedMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	- For other morphs, not BoxedMorph, (i.e. they run this implementation), if VectorGraphics is not active,
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
	In general, the answer is not strictly correct. (There are false positives, for submorphs or sibling morphs covering us).
	BoxedMorph redefines this method with an equally cheap implementation that can also handle
	rotated morphs (but assuming they are of rectangular shape)."
	self visible ifTrue: [
		self basicDisplayBounds ifNotNil: [ :r |
			^r containsPoint: worldPoint ]].

	^ false.! !


!BoxedMorph methodsFor: 'geometry services' stamp: 'jmv 2/24/2022 15:21:55'!
includesPixel: worldPoint
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:
	(See comment and implementation at Morph).
	Note: This implementation also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.
	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph covers us.
	Note: This implementation is only used for morphs with a cheap #coversPixel:.
	(See other implementors)
	Also see #ownsPixel: and #coversPixel:"

	^ self visible and: [self coversPixel: worldPoint].! !

