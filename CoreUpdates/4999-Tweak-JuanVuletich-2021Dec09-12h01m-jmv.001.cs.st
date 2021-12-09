'From Cuis 5.0 [latest update: #4998] on 9 December 2021 at 12:01:58 pm'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 12/9/2021 11:59:49'!
coversAnyPixelCloserThan: maxDistance to: worldPoint
	"Answer true if our closest point to worldPoint is less than aNumber pixels away.
	In target surface (i.e. Display) coordinates.
	See #bitMask.
	Remember to do
	self removeProperty: #bitMask.
	when appropriate!! (i.e. when we change in such a way to make the bitMask invalid).

	Note: Requires VectorGraphics. Meant to be used only when needed.
	Note: Prefer #isCloserThan:toPixel:, that doesn't use #bitMask, and doesn't require maintenance."

	| center coversIt maxDistanceSquared |
	self visible ifFalse: [
		^false ].
	privateDisplayBounds ifNil: [
		^false ].
	center _ privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - worldPoint y) abs < (privateDisplayBounds height  // 2 + maxDistance) ifFalse: [
		^false ].
	(center x - worldPoint x) abs < (privateDisplayBounds width // 2 + maxDistance) ifFalse: [
		^false ].
	"Precise check with bitMask. If unavailable, just answer false. See #coversPixel:"
	coversIt _ (self coversPixel: worldPoint) ifNil: [ ^false ].
	coversIt ifTrue: [ ^true ].
	maxDistanceSquared _ maxDistance squared.
	maxDistance negated to: maxDistance do: [ :dy |
		maxDistance negated to: maxDistance do: [ :dx |
			dx squared + dy squared <= maxDistanceSquared ifTrue: [
				(self coversPixel: worldPoint + (dx@dy)) ifTrue: [ ^true ]]]].
	^false.! !

