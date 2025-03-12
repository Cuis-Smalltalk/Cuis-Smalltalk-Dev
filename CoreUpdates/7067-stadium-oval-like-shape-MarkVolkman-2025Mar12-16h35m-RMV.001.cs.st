'From Cuis7.3 [latest update: #7066] on 12 March 2025 at 4:37:11 pm'!

!AbstractVectorCanvas methodsFor: 'paths' stamp: 'mv 3/5/2025 18:09:04'!
stadium: aRectangle
	"
| r |
r := 500@150 extent: 100@250.
(VectorCanvas onForm: Display)
	strokeWidth: 2.0 color: Color red do: [ :canvas |
			canvas stadium: r ].
(VectorCanvas onForm: Display)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :canvas |
		canvas
			stadium: r ].
Display forceToScreen
"
	| height p1 p2 p3 p4 radius width x1 x2 y1 y2 |
	width := aRectangle width.
	height := aRectangle height.
	radius := (width min: height) / 2.
	height < width
		ifTrue: [
			x1 := aRectangle left + radius.
			x2 := aRectangle right - radius.
			y1 := aRectangle top.
			y2 := aRectangle bottom.
			p1 := x1 @ y1.
			p2 := x2 @ y1.
			p3 := x2 @ y2.
			p4 := x1 @ y2 ]
		ifFalse: [
			x1 := aRectangle left.
			x2 := aRectangle right.
			y1 := aRectangle top + radius.
			y2 := aRectangle bottom - radius.
			p1 := x1 @ y2.
			p2 := x1 @ y1.
			p3 := x2 @ y1.
			p4 := x2 @ y2].
	self
		moveTo: p1;
		lineTo: p2;
		arcTo: p3 radius: radius angleOfXAxis: 0 largeFlag: false sweepFlag: true;
		lineTo: p4;
		arcTo: p1 radius: radius angleOfXAxis: 0 largeFlag: false sweepFlag: true.! !


!Morph methodsFor: 'geometry services' stamp: 'mv 3/12/2025 16:35:24'!
bitMask
	"Answer an 1bpp Form with our bitMask.
	Include all pixels drawn by any morph in our submorphs tree.
	Query with #pixelValueAt:
	Pixels drawn by us (or any submorph) are 0. Pixels not drawn by us (or any submorph) are 1.
	Remember to do `self removeProperty: #bitMask when appropriate!!
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

