'From Cuis 4.2 of 25 July 2013 [latest update: #2148] on 8 January 2015 at 6:30:28.000056 pm'!

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 1/7/2015 08:13'!
rotatedBy: deg smoothing: cellSize
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."
	| side rot warp r1 pts p center |
	side _ 1 + ((width*width) + (height*height)) asFloat sqrt asInteger.
	rot _ Form extent: side@side depth: self depth.
	center _ rot extent // 2.

	"Now compute the sin and cos constants for the rotation angle." 
	warp _ (WarpBlt toForm: rot)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: rot);
		cellSize: cellSize;  "installs a new colormap if cellSize > 1"
		combinationRule: Form over.
	r1 _ rot boundingBox aligned: center with: self boundingBox center.

	pts _ r1 innerCorners collect: [ :pt |
		p _ pt - r1 center.
		(r1 center x asFloat + (p x asFloat*deg degreeCos) + (p y asFloat*deg degreeSin)) @
		(r1 center y asFloat - (p x asFloat*deg degreeSin) + (p y asFloat*deg degreeCos))].
	warp copyQuad: pts toRect: rot boundingBox.
	^ rot
"
 | a f |  f _ Form fromDisplay: (0@0 extent: 200@200).  a _ 0.
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotatedBy: (a _ a+0.1) smoothing: 2) display].
f display
"! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 1/7/2015 08:13'!
rotatedByDegrees: deg
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."

	^ self rotatedBy: deg smoothing: 1
"
 | a f |  f _ Form fromDisplay: (0@0 extent: 200@200).  a _ 0.
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotatedByDegrees: (a _ a+0.1)) display].
f display
"! !


!Point methodsFor: 'transforming' stamp: 'jmv 1/7/2015 08:12'!
rotatedBy: radians

	| r theta |
	r _ self r.
	theta _ self theta +  radians asFloat.
	^ (r * theta cos) @ (r * theta sin)! !


!BitBltCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 1/7/2015 08:12'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	 (self buildArrowOfDirection: #up size: 120) display
	"

	| resizeFactor resizedForm f e c bottomMargin topMargin |
	resizeFactor _ 4.
	e _ finalSizeInteger@finalSizeInteger*resizeFactor.
	f _ Form extent: e depth: 32.
	c _ Color gray: 0.5.
	topMargin _ finalSizeInteger * 3//4.
	bottomMargin _ finalSizeInteger * 4//4.
	0 to: e y-1-bottomMargin do: [ :y |
		0 to: e x -1 do: [ :x |
			(e x / 2 - 1 - x) abs * 2 + topMargin < y ifTrue: [
				f colorAt: x@y put: c
			]
		]
	].
	resizedForm _ f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.

	aSymbolDirection == #right ifTrue: [
		resizedForm _ resizedForm rotatedByDegrees: 90 ].
	aSymbolDirection == #down ifTrue: [
		resizedForm _ resizedForm rotatedByDegrees: 180 ].
	aSymbolDirection == #left ifTrue: [
		resizedForm _ resizedForm rotatedByDegrees:  270 ].
		
	aSymbolDirection == #up ifFalse: [
		resizedForm _ resizedForm
			copy: (resizedForm boundingBox insetBy: (resizedForm width - finalSizeInteger/ 2.0) rounded) ].
		
	^resizedForm! !

!methodRemoval: Point #rotateBy:!
Point removeSelector: #rotateBy:!
!methodRemoval: Form #rotateBy:!
Form removeSelector: #rotateBy:!
!methodRemoval: Form #rotateBy:smoothing:!
Form removeSelector: #rotateBy:smoothing:!
