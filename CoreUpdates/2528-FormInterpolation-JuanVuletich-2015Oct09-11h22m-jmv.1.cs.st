'From Cuis 4.2 of 25 July 2013 [latest update: #2525] on 9 October 2015 at 4:06:23.365358 pm'!

!Form methodsFor: 'pixel access' stamp: 'jmv 10/9/2015 16:00'!
colorInterpolatedAt: aPoint
	"Evaluate a bilinear (i.e. cheap) interpolation
	Like OpenCV's CV_INTER_LINEAR
	Answer Color transparent if outside image bounds.
	Copied almost verbatim from FloatImage.
	Answer Colors with float components. Will be rounded to be stored in, for example, a 32-bit Form."

	| x y w interpolated xWeight1 xWeight0 yWeight1 yWeight0 xIndex0 xIndex1 yIndex0 yIndex1 |
	x _ aPoint x.
	y _ aPoint y.
	xIndex0 _ x truncated. 		"Could be #floor. But as we only care for values >=1, it is the same. But faster."
	xIndex0 < 0 ifTrue: [ ^Color transparent ].
	xIndex0 > (width-1) ifTrue: [ ^Color transparent ].
	(xIndex0 = (width-1) and: [ x > (width-1) ]) ifTrue: [ ^Color transparent ].
	xIndex1 _ xIndex0 = (width-1) 	"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ xIndex0 + 1 ]
		ifTrue: [ xIndex0 ].

	yIndex0 _ y truncated.			"Could be #floor. But as we only care for values >=1, it is the same. But faster."
	yIndex0 < 0 ifTrue: [ ^Color transparent ].
	yIndex0 > (height-1) ifTrue: [ ^Color transparent ].
	(yIndex0 = (height-1) and: [ y > (height-1) ]) ifTrue: [ ^Color transparent ].
	yIndex1 _ yIndex0 = (height-1) 	"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ yIndex0 + 1 ]
		ifTrue: [ yIndex0 ].

	xWeight1 _ x - xIndex0.
	xWeight0 _ 1.0 - xWeight1.

	yWeight1 _ y - yIndex0.
	yWeight0 _ 1.0 - yWeight1.

	"/* perform interpolation */"
	w _ ((self colorAt: xIndex0 @ yIndex0) * xWeight0) +
		((self colorAt: xIndex1 @ yIndex0) * xWeight1).
	interpolated _ w * yWeight0.

	w _ ((self colorAt: xIndex0 @ yIndex1) * xWeight0) +
		((self colorAt: xIndex1 @ yIndex1) * xWeight1).
	interpolated _ w * yWeight1 + interpolated.

	^interpolated! !

