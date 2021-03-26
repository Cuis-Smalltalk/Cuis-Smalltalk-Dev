'From Cuis 5.0 [latest update: #4542] on 25 March 2021 at 7:24:40 pm'!

!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 3/25/2021 19:23:57'!
drawCoordinateSystem: aRectangle
	| c stepX stepY x0 x1 y0 y1 |
	stepX _ 100.
	stepY _ 50.

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
	  self drawString: 'y' atWaist: 0 @ (y1 - 20) font: nil color: c.! !

