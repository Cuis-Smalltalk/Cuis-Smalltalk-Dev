'From Cuis 5.0 [latest update: #4952] on 28 October 2021 at 11:51:57 am'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/28/2021 11:46:45'!
coversPixel: worldPoint
	"Answer true if pixel worldPoint is covered by us, even if we are not visible a it because of some
	other morph above us also covers it.

	See #bitMask.
	Remember to do
	self removeProperty: #bitMask.
	when appropriate!! (i.e. when we change in such a way to make the bitMask invalid).

	Note: Subclasses such as KernelMorph and WidgetMorph redefine this method with an optimized
	implementation that doesn't require computing and invalidating the #bitMask. Senders in the base image
	and framework actually only use this optimized implementation. That's why general morphs don't care about
	invalidting #bitMask.
	
	Note: If your morph #requiresVectorCanvas, and depends on this general implementation, remember to 
	`removeProperty: #bitMask` whenever it becomes invalid due to changes in your morphs. You may consider
	using #ownsPixel: if appropriate, that doesn't require any maintenance and is cheaper (in cpu and memory).

	Note: This implementation requires VectorGraphics.

	See #ownsPixel:
	See #ownsOrCoversPixel:"

	self visible ifTrue: [
		"Expensive way for morphs with arbitrary shape in VectorGraphics.
		The base Cuis System doesn't use this.
		Use this in applications if the expense of maintaining #bitMask is worth it."
		self canvas ifNotNil: [ :canvas |
			canvas canDoVectorGraphics ifTrue: [
				^(self bitMask pixelValueAt: worldPoint - self displayFullBounds topLeft) = 1 ]].

		"This is cheap and OK for unrotated Morphs with a rectangular shape.
		In general, the answer is not strictly correct.
		WidgetMorph redefines this method with an equally cheap implementation that can also handle
		rotated morphs (but assuming they are of rectangular shape)."
		self basicDisplayBounds ifNotNil: [ :r |
			^ r containsPoint: worldPoint ]].

	^ false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/28/2021 11:50:48'!
ownsOrCoversPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.

	Some implementations (KernelMorph and WidgetMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	For other morphs, not KernelMorph or WidgetMorph, (i.e. they run this implementation), if VectorGraphics is not active,
	answer true for any point that lies within our bounds. If this is the case, consider using VectorGraphics: In addition to
	many other advantages, this method will always answer a strictly correct answer.

	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph
	covers us. A common case is to find the topmost morph at some position. In that case, iterating front to back ensures
	that if any other morph covers us, it is found first.

	Note: This implementation is only used for morphs drawn by VectorCanvas, either because the main canvas is a
	VectorCanvas, or because it is a HybridCanvas and we answer true to #requiresVectorCanvas.
	(See other implementors)

	Note: Also see #ownsPixel: and #coversPixel:"

	"Check if #ownsPixel:, if possible."
	self canvas ifNotNil: [ :canvas |
		canvas canDoVectorGraphics ifTrue: [
			^ (canvas morphIdAt: worldPoint) = self morphId ]].

	"This is cheap and OK for unrotated Morphs with a rectangular shape.
	In general, the answer is not strictly correct.
	WidgetMorph redefines this method with an equally cheap implementation that can also handle
	rotated morphs (but assuming they are of rectangular shape)."
	self visible ifTrue: [
		self basicDisplayBounds ifNotNil: [ :r |
			^r containsPoint: worldPoint ]].

	^ false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/28/2021 11:41:15'!
ownsPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	Requires VectorGraphics, as we need VectorCanvas' morphIdAt: service.

	Valid if running on VectorCanvas.

	In case of running on HybridCanvas, this method is only valid for morphs that
	are drawn by VectorCanvas (i.e. they answer true to #requiresVectorCanvas).

	See #coversPixel:
	See #ownsOrCoversPixel:"

	self canvas ifNotNil: [ :canvas |
		canvas canDoVectorGraphics ifTrue: [
			^ (canvas morphIdAt: worldPoint) = self morphId ]].
	^ false.! !

