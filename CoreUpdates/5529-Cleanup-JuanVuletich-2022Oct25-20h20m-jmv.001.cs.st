'From Cuis 6.0 [latest update: #5515] on 25 October 2022 at 8:29:01 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 10/1/2022 19:47:29'!
findFullBoundsInOwner
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!!
	Note: Answers an integer rectangle"

	| prevNotVisibleFlag w c answer prevOwner |
	(owner notNil and: [owner isWorldMorph]) ifTrue: [
		w _ owner ]
	ifFalse: [
		w _ UISupervisor ui ].
	c _ w boundsFinderCanvas.
	"Hack owner so #displayBoundsSetFrom: will update privateDisplayBounds"
	prevOwner _ owner.
	owner _ w.
	prevNotVisibleFlag _ self privateFlagAt: 3.
	self privateFlagAt: 3 put: false.
	c fullUpdateProtrudingBounds: self.
	answer _ self displayFullBounds.
	"Reset owner and privateDisplayBounds (if needed) so no one finds out what we've just done!!"
	prevOwner == owner ifFalse: [
		owner _ prevOwner.
		self world = w ifTrue: [
			self privateFlagAt: 3 put: false.
			self allOwnersReverseDo: [ :m | c into: m ].
			c fullUpdateProtrudingBounds: self.
			self allOwnersDo: [ :m | c outOfMorph ]]].
	self privateFlagAt: 3 put: prevNotVisibleFlag.
	^answer! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/25/2022 20:26:41'!
coversPixel: worldPoint
	"Answer true if pixel worldPoint is covered by us (or a submorph of ours), even if we are not visible a it
	because of some other morph above us also covers it.

	WARNING: Might answer nil when we simply don't know. It is generally advisable to call this method only for
	morphs where the answer is not nil:
		- BoxedMorphs. No special care needed. (Senders in the base Cuis image are of this kind).
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
				^(self bitMask pixelValueAt: worldPoint - self displayFullBounds topLeft) = 1 ]]].

	"If we don't know."
	^ nil.! !


!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 10/1/2022 19:59:27'!
knowsOwnLocalBounds
	"HaloMorph is a bit special because of the drawing of the coordinate systems."

	^false! !

!methodRemoval: MorphicCanvas class #onForm:over: stamp: 'jmv 10/25/2022 20:24:19'!
MorphicCanvas class removeSelector: #onForm:over:!
