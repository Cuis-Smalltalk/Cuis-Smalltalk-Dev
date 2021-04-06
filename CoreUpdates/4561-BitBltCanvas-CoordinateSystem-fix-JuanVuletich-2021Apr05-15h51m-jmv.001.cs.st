'From Cuis 5.0 [latest update: #4558] on 5 April 2021 at 4:14:24 pm'!

!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 4/5/2021 16:14:13'!
drawCoordinateSystem: aRectangle
	| c stepX stepY x0 x1 y0 y1 prevClipRect |
	stepX _ 100.
	stepY _ 50.

	prevClipRect _ self clipRect.
	self setClipRect: (prevClipRect intersect:currentMorph displayBounds).

	self frameRectangle: aRectangle borderWidth: 4 color: `Color lightGray alpha: 0.4`.

	x0 _ aRectangle left.
	x1 _ aRectangle right-1.
	y0 _ aRectangle top.
	y1 _ aRectangle bottom-1.

	c _ `Color lightGray alpha: 0.6`.
	self line: x0@0 to: x1@0 width: 2 color: c.
	self line: 0@y0 to: 0@y1 width: 2 color: c.

	(x0 truncateTo: stepX) to: (x1 - stepX truncateTo: stepX) by: stepX do: [ :x |
		self line: x @ -5 to: x @ 5 width: 2 color: c.
	   	self drawString: x printString atCenterX: x @ 5 font: nil color: c ].
	  self drawString: 'x' atCenterX: x1 - 15 @ 0 font: nil color: c.

	(y0 truncateTo: stepY) to: (y1 - stepY truncateTo: stepY) by: stepY do: [ :y |
		self line: -5 @ y to: 5 @ y width: 2 color: c.
	   	self drawString: y printString, ' ' atWaist: 5 @ y font: nil color: c ].
	  self drawString: 'y' atWaist: 0 @ (y1 - 20) font: nil color: c.

	self setClipRect: prevClipRect.! !

