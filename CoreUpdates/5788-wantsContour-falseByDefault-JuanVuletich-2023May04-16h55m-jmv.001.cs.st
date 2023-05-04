'From Cuis 6.0 [latest update: #5787] on 4 May 2023 at 4:57:11 pm'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 5/4/2023 11:16:27'!
bitMask
	"Answer an 1bpp Form with our bitMask.
	Include all pixels drawn by any morph in our submorphs tree.
	Query with #pixelValueAt:
	Pixels drawn by us (or any submorph) are 0. Pixels not drawn by us (or any submorph) are 1.
	Remember to do `self removeProperty: #bitMask1  when appropriate!!
	(i.e. when we change in such a way to make the bitMask invalid)
	
	Also see senders of #contour"

	| prevNotVisibleFlag bounds answer auxCanvas |

	(self valueOfProperty: #bitMask) ifNotNil: [ :bitMask |
		^bitMask ].

	"Position and scale us in order to fill required extent, but disregard any rotation.
	Do it without triggering any invalidation at all."
	prevNotVisibleFlag := self privateFlagAt: 3.
	bounds := self findFullBoundsInOwner.
	[
		answer := Form extent: bounds extent depth: 32.
		"Ask for a Canvas with subpixels so it can also handle translucent target"
		auxCanvas := VectorCanvas onFormWithSubPixelAntiAliasing: answer translation: bounds origin negated.
		"But disable subpixel anti aliasing, as we are answering a Form, and therefore can't assume a Display geometry."
		auxCanvas engine disableSubPixelSampling.
		self privateFlagAt: 3 put: false.
		auxCanvas fullDraw: self.
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^self setProperty: #bitMask toValue:
		"Reverse it so we get bit=1 for affected pixels."
		((answer asGrayForm: 1) asFormOfDepth: 1) reverse.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 5/4/2023 11:13:28'!
collides: aMorph
	"Answer whether the pixels used by morphs touch at least at one place.
	Morphs not in the BoxedMorph hierarchy should answer #true to #wantsContour."

	privateDisplayBounds ifNil: [
		^false ].

	"Quick check with display bounds"
	aMorph displayFullBounds
		ifNil: [ ^false ]
		ifNotNil: [ :r |
			(self displayFullBounds
				ifNil: [ ^ false ]
				ifNotNil: [ :myFullBounds | myFullBounds intersects: r ]) ifFalse: [ ^false ]].

	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | contourTop contourBottom |
		contourTop := self valueOfProperty: #contourY0.
		contourBottom := self valueOfProperty: #contourY1.
		^ aMorph contourIntersects: contour top: contourTop bottom: contourBottom ].

	"If contour is not available, and both displayBounds overlap, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 5/4/2023 11:13:49'!
isCloserThan: maxDistance toPixel: worldPoint
	"Answer true if our closest point to aPoint is less than aNumber pixels away.
	In target surface (i.e. Display) coordinates.
	Uses precise testing of the morph contour if available:
	Morphs not in the BoxedMorph hierarchy should answer #true to #wantsContour.
	Note: Considers only the external border. Any inner pixel is considered 'inside' regardless of us being transparent there.
	Note: Cheaper than #coversAnyPixelCloserThan:to: . Doesn't use #bitMask. Doesn't require maintenance."

	| center contourTop contourBottom |
	privateDisplayBounds ifNil: [
		^false ].
	center := privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - worldPoint y) abs < (privateDisplayBounds height  // 2 + maxDistance) ifFalse: [
		^false ].
	(center x - worldPoint x) abs < (privateDisplayBounds width // 2 + maxDistance) ifFalse: [
		^false ].
	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | y0 y1 x0 x1 |
		contourTop := self valueOfProperty: #contourY0.
		contourBottom := self valueOfProperty: #contourY1.
		"Contour rows to consider are those within requested distance."
		y0 := worldPoint y - maxDistance max: contourTop.
		y1 := worldPoint y + maxDistance min: contourBottom.
		y0 to: y1 do: [ :y |
			x0 := (contour at: (y - contourTop) * 2 + 1) - maxDistance.
			x1 := (contour at: (y - contourTop) * 2 + 2) + maxDistance.
			"If a vertical line of 2*aNumber height centered on aPoint is inside the contour, quick exit"
			(worldPoint x between: x0 and: x1) ifTrue: [ ^true ].
			"Check if aPoint is close enough to contour"
			(x0@y - worldPoint) r < maxDistance ifTrue: [ ^true ].
			(x1@y - worldPoint) r < maxDistance ifTrue: [ ^true ]].
		"Not inside, not close enough to contour"
		^ false ].
	"If contour is not available, and aPoint is close enough to displayBounds, answer true, as it is the best we can know."
	^ true! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 5/4/2023 11:18:27'!
wantsContour
	"True if a morph requires the Canvas to update a #morphContour property on us.
	Any subclass might answer true, if it #requiresVectorCanvas.
	But if #requiresVectorCanvas is false, #contour can not be computed.
	Not a problem, as #contour is useless for BoxedMorphs.
	
	Also see #bitMask"

	"^self requiresVectorCanvas and: [ self isOwnedByWorldOrHand ]"
	^false! !

