'From Cuis7.5 [latest update: #7483] on 2 September 2025 at 2:19:34 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 08:56:28'!
rejectsEventFully: aMorphicEvent
	"Return true to reject the given event, for the receiver and all submorphs."

	^ self isLocked or: [ self visible not ]! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/2/2025 09:13:45'!
coversAnyPixelCloserThan: maxDistance to: worldPoint
	"Answer true if our closest point to worldPoint is less than aNumber pixels away.
	In target surface (i.e. Display) coordinates.
	See #bitMask.
	Remember to do
	self removeProperty: #bitMask.
	when appropriate!! (i.e. when we change in such a way to make the bitMask invalid).

	Note: Requires VectorGraphics (#requiresVectorCanvas == true). Meant to be used only when really needed.
	Note: Prefer #isCloserThan:toPixel:, that doesn't use #bitMask, and doesn't require maintenance."

	| center coversIt maxDistanceSquared |
	self visible ifFalse: [
		^false ].
	privateDisplayBounds ifNil: [
		^false ].
	center := privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - worldPoint y) abs < (privateDisplayBounds height  // 2 + maxDistance) ifFalse: [
		^false ].
	(center x - worldPoint x) abs < (privateDisplayBounds width // 2 + maxDistance) ifFalse: [
		^false ].
	"Precise check with bitMask. If unavailable, just answer false. See #coversPixel:"
	coversIt := (self coversPixel: worldPoint) ifNil: [ ^false ].
	coversIt ifTrue: [ ^true ].
	maxDistanceSquared := maxDistance squared.
	maxDistance negated to: maxDistance do: [ :dy |
		maxDistance negated to: maxDistance do: [ :dx |
			dx squared + dy squared <= maxDistanceSquared ifTrue: [
				(self coversPixel: worldPoint + (dx@dy)) ifTrue: [ ^true ]]]].
	^false.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 9/2/2025 09:14:31'!
ownsPixel: worldPoint
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	Requires VectorGraphics (#requiresVectorCanvas == true), as we need VectorCanvas' morphIdAt: service.

	Valid if running on VectorCanvas.

	In case of running on HybridCanvas, this method is only valid for morphs that
	are drawn by VectorCanvas (i.e. they answer true to #requiresVectorCanvas).

	See #coversPixel:
	See #includesPixel:"

	self mainCanvas ifNotNil: [ :mainCanvas |
		mainCanvas canDoVectorGraphics ifTrue: [
			^ (mainCanvas morphIdAt: worldPoint) = self morphId ]].
	^ false.! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/2/2025 09:29:39'!
allSubmorphsDo: aBlock
	"Evaluate the given block for all morphs in this composite morph (not including the receiver)."

	submorphs do: [ :m |
		aBlock value: m.
		m allSubmorphsDo: aBlock].! !


!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 08:56:33'!
rejectsEventFully: anEvent
	"Return true to reject the given event, for the receiver and all submorphs."

	(super rejectsEventFully: anEvent) ifTrue: [^true].
	anEvent isDropEvent ifTrue: [^true]. "never attempt to drop on halos"
	^false! !

