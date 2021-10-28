'From Cuis 5.0 [latest update: #4950] on 28 October 2021 at 11:22:39 am'!

!Morph methodsFor: 'accessing' stamp: 'jmv 10/28/2021 11:18:04'!
boundsFinderCanvas
	"Might answer nil if not in a world!!"

	^ self canvas ifNotNil: [ :c | c boundsFinderCanvas ]! !

!Morph methodsFor: 'accessing' stamp: 'jmv 10/27/2021 16:06:26'!
canvas
	"Might answer nil if not in a world!!"

	^ self topmostWorld ifNotNil: [ :w | w canvas ].! !


!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 10/27/2021 16:10:26'!
canDoVectorGraphics
	^false! !


!Morph methodsFor: 'geometry' stamp: 'jmv 10/27/2021 16:07:00'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Clipped appropiately if some owner clips us.
	See also #displayBoundsUpdateFrom:"

	self canvas ifNotNil: [ :c |
		(aCanvas isBoundsFinderCanvas or: [c canvasToUse == aCanvas canvasToUse]) ifTrue: [
			privateDisplayBounds _ aCanvas canvasToUse boundingRectOfCurrentMorphAfterDraw.
			self wantsContour
				ifTrue: [
					 (aCanvas canvasToUse
						contourOfCurrentMorphAfterDrawInto: (self valueOfProperty: #contour) 
						into: [ :contourArray :contourY0 :contourY1 |
							self setProperty: #contour toValue: contourArray.
							self setProperty: #contourY0 toValue: contourY0.
							self setProperty: #contourY1 toValue: contourY1 ])]
				ifFalse: [
					self setProperty: #contour toValue: nil.
					self setProperty: #contourY0 toValue: nil.
					self setProperty: #contourY1 toValue: nil ]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/27/2021 16:07:14'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us.
	To be called after #postDrawOn:. See also #displayBoundsSetFrom:
	This more complicated update is needed because the contour information from #drawOn: was possibly
	when drawing submorphs. So, the controur from #drawOn: needs to be retrieved, and updated with the
	new contour information from #postDrawOn:"

	self canvas ifNotNil: [ :c |
		(aCanvas isBoundsFinderCanvas or: [c canvasToUse == aCanvas canvasToUse]) ifTrue: [
			privateDisplayBounds _ privateDisplayBounds
				quickMerge: aCanvas canvasToUse boundingRectOfCurrentMorphAfterDraw.
			self wantsContour ifTrue: [
				(aCanvas canvasToUse
					contourOfCurrentMorphAfterDrawUpdate: (self valueOfProperty: #contour)
					oldTop: (self valueOfProperty: #contourY0)
					oldBottom: (self valueOfProperty: #contourY1)
					into: [ :contourArray :contourY0 :contourY1 |
						self setProperty: #contour toValue: contourArray.
						self setProperty: #contourY0 toValue: contourY0.
						self setProperty: #contourY1 toValue: contourY1 ])]]].! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/27/2021 16:21:16'!
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

	See #ownsOrCoversPixel:"

	self visible ifTrue: [
		^(self bitMask pixelValueAt: worldPoint - self displayFullBounds topLeft) = 1 ].
	^ false! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/27/2021 16:04:46'!
ownsOrCoversPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	Some implementations (KernelMorph and WidgetMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph
	covers us. A common case is to find the topmost morph at some position. In that case, iterating front to back ensures
	that if any other morph covers us, it is found first.

	Note: This implementation is only used for morphs drawn by VectorCanvas, either because the main canvas is a
	VectorCanvas, or because it is a HybridCanvas and we answer true to #requiresVectorCanvas.
	(See other implementors)
	Note: Also see #ownsPixel: and #coversPixel:"

	^ self ownsPixel: worldPoint.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/27/2021 16:07:29'!
ownsPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	Requires VectorGraphics.
	In case of running on HybridCanvas, this method is only valid for morphs that
	are drawon by VectorCanvas (i.e. they answer true to #requiresVectorCanvas)."

	self canvas ifNotNil: [ :canvas |
		^ (canvas morphIdAt: worldPoint) = self morphId ].
	^ false.! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/28/2021 11:18:30'!
basicDisplayBounds

	^ self boundsFinderCanvas displayBoundsForHand: self! !


!WidgetMorph methodsFor: 'halos and balloon help' stamp: 'jmv 10/27/2021 16:07:44'!
okayToRotateEasily
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver."

	^ self canvas usesVectorEnginePlugin.! !

!WidgetMorph methodsFor: 'halos and balloon help' stamp: 'jmv 10/27/2021 16:07:55'!
okayToScaleEasily
	"Answer whether it is appropriate for a scale handle to be shown for the receiver."

	^ self canvas usesVectorEnginePlugin.! !

