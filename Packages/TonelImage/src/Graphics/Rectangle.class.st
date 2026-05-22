"
My instances represent a rectangular area of the screen. Arithmetic functions take points as arguments and carry out scaling and translating operations to create new instances of me. Rectangle functions create new instances by determining intersections of rectangles with rectangles.

Instance variables:
	origin	<Point> the coordinates of corner having smallest coordinates (top left in Screen coordinates)
	corner	<Point> the coordinates of corner having largest coordinates (bottom right in Screen coordinates)

Important notes:

 1: only rectangles parallel to reference frame (Screen) can be represented by this class.

 2: the Rectangle is represented by two extremities of one diagonal. By convention, it must be the diagonal:
	from rectangle origin (the point having smallest coordinates in reference frame),
	to rectangle corner (the point having largest coordinates in reference frame).

 3: Screen coordinates conventions are:
	x is horizontal axis, zero at left border, oriented toward right;
	y is vertical axis, zero at top border, oriented toward bottom.

 4: the Rectangle extent is obtained by subtracting rectangle origin to rectangle corner coordinates.
If this leads to a negative width (extent x coordinate) and/or a negative height (extent y coordinate), then the Rectangle is degenerated and considered empty.

 5: Instances of Rectangle contain the top left 'origin', but do not include the bottom right 'corner'. See for instance #containsPoint: . This has been like this since Smalltalk-80, and most likely since earlier. This convention is useful for working with pixel areas on the Display. The Rectangle `0@0 corner: 100@100` will have width = 100 and cover exactly 100 pixels wide. It will include point 0@0, but will not include point 100@100. Rectangles `0@0 corner: 100@100` and `100@0 corner: 200@100` do not overlap!

"
Class {
	#name : 'Rectangle',
	#superclass : 'Object',
	#instVars : [
		'origin',
		'corner'
	],
	#category : 'Graphics-Primitives',
	#package : 'Graphics',
	#tag : 'Primitives'
}

{ #category : 'instance creation' }
Rectangle class >> center: centerPoint extent: extentPoint [
	"Answer an instance of me whose center is centerPoint and width 
	by height is extentPoint.  "

	^self origin: centerPoint - (extentPoint/2) extent: extentPoint
]

{ #category : 'instance creation' }
Rectangle class >> encompassing: listOfPoints [
	"A number of callers of encompass: should use this method."
	| topLeft bottomRight |
	topLeft := bottomRight := nil.
	listOfPoints do: [ :p |
		topLeft
			ifNil: [ topLeft := bottomRight := p]
			ifNotNil: [
				topLeft := topLeft min: p.
				bottomRight := bottomRight max: p]].
	^ topLeft corner: bottomRight
]

{ #category : 'instance creation' }
Rectangle class >> encompassingInteger: listOfPoints [
	"Like #encompassing:, but with integer coordinates."
	| topLeft bottomRight |
	topLeft := bottomRight := nil.
	listOfPoints do: [ :p |
		topLeft
			ifNil: [
				topLeft := p floor.
				bottomRight := p ceiling]
			ifNotNil: [
				topLeft := topLeft min: p floor.
				bottomRight := bottomRight max: p ceiling]].
	^ topLeft corner: bottomRight
]

{ #category : 'instance creation' }
Rectangle class >> fromUser [
	"Answer a Rectangle that is determined by having the user 
	designate the top left and bottom right corners."
	| originRect |
	originRect := (Cursor cursorAt: #originCursor) showWhile: [
		(Sensor mousePoint extent: `0 @ 0`) newRectFrom: [ :f |
			Sensor mousePoint extent: `0 @ 0` ]].
	^ (Cursor cursorAt: #cornerCursor) showWhile: [
		originRect newRectFrom: [ :f |
			f origin corner: Sensor mousePoint ]].
]

{ #category : 'instance creation' }
Rectangle class >> left: leftNumber right: rightNumber top: topNumber bottom: bottomNumber  [
	"Answer an instance of me whose left, right, top, and bottom coordinates 
	are determined by the arguments."

	^ self basicNew setOrigin: leftNumber @ topNumber corner: rightNumber @ bottomNumber
]

{ #category : 'instance creation' }
Rectangle class >> merging: listOfRects  [
	"A number of callers of merge: should use this method.
	Note: ignore possible nils in the argument."
	| minX minY maxX maxY |
	listOfRects
		do: [ :r | 
			r ifNotNil: [
				minX
					ifNil: [
						minX := r topLeft x. minY := r topLeft y.
						maxX := r bottomRight x. maxY := r bottomRight y]
					ifNotNil: [
						minX := minX min: r topLeft x. minY := minY min: r topLeft y.
						maxX := maxX max: r bottomRight x. maxY := maxY max: r bottomRight y]]].
	^ minX ifNotNil: [ minX@minY corner: maxX@maxY ]
]

{ #category : 'instance creation' }
Rectangle class >> origin: originPoint corner: cornerPoint  [
	"Answer an instance of me whose corners (top left and bottom right) are 
	determined by the arguments."

	^self basicNew setOrigin: originPoint corner: cornerPoint
]

{ #category : 'instance creation' }
Rectangle class >> origin: originPoint extent: extentPoint  [
	"Answer an instance of me whose top left corner is originPoint and width 
	by height is extentPoint."

	^self basicNew setOrigin: originPoint corner: originPoint + extentPoint
]

{ #category : 'instance creation' }
Rectangle class >> originFromUser: extentPoint [
	"Answer an instance of me that is determined by having the user 
	designate the top left corner. The width and height are determined by 
	extentPoint."
	^ (Cursor cursorAt: #originCursor) showWhile: [
		(Sensor mousePoint extent: extentPoint) newRectFrom: [ :f |
			Sensor mousePoint extent: extentPoint ]].
]

{ #category : 'comparing' }
Rectangle >> = aRectangle  [

	self == aRectangle ifTrue: [ ^ true ].

	"Answer true if the receiver's species, origin and corner match aRectangle's."
	self species == aRectangle species
		ifFalse: [ ^false ].

	^ origin = aRectangle origin and: [ corner = aRectangle corner ]
]

{ #category : 'transforming' }
Rectangle >> aligned: aPoint1 with: aPoint2 [
	"Answer a Rectangle that is a translated by aPoint2 - aPoint1."

	^self translatedBy: aPoint2 - aPoint1
]

{ #category : 'rectangle functions' }
Rectangle >> amountToTranslateWithin: aRectangle [
	"Answer a Point, delta, such that self + delta is forced within aRectangle."
	"Altered so as to prefer to keep self topLeft inside when all of self
	cannot be made to fit 7/27/96 di"
	| dx dy |
	dx := 0.  dy := 0.
	self right > aRectangle right ifTrue: [dx := aRectangle right - self right].
	self bottom > aRectangle bottom ifTrue: [dy := aRectangle bottom - self bottom].
	(self left + dx) < aRectangle left ifTrue: [dx := aRectangle left - self left].
	(self top + dy) < aRectangle top ifTrue: [dy := aRectangle top - self top].
	^ dx@dy
]

{ #category : 'accessing' }
Rectangle >> area [
	"Answer the receiver's area, the product of width and height."
	| w |
	(w := self width) <= 0 ifTrue: [^ 0].
	^ w * self height max: 0
]

{ #category : 'rectangle functions' }
Rectangle >> areasOutside: aRectangle [
	"Answer an Array of Rectangles comprising the parts of the receiver not 
	intersecting aRectangle."

	| areas |
	"Make sure the intersection is non-empty"
     (self intersects: aRectangle)
		ifFalse: [^Array with: self].
	areas := OrderedCollection new.
	self areasOutside: aRectangle do: [ :r |  areas add: r ].
	^areas
]

{ #category : 'rectangle functions' }
Rectangle >> areasOutside: aRectangle do: aBlock [
	"Answer an Array of Rectangles comprising the parts of the receiver not 
	intersecting aRectangle.
	(100@100 extent: 100@100) areasOutside: (100@100 extent: 100@100).
	(110@100 extent: 100@100) areasOutside: (100@100 extent: 100@100).
	(90@90 extent: 100@100) areasOutside: (100@100 extent: 100@100).
	Note: The rectangles passed to aBlock are new instances. It is OK to modify them as needed.
	"

	| yOrigin yCorner |
	"Make sure the intersection is non-empty"
     (self intersects: aRectangle)
		ifFalse: [
			aBlock value: self copy.
			^self ].
	aRectangle origin y > origin y
		ifTrue: [ aBlock value: (origin corner: corner x @ (yOrigin := aRectangle origin y))] 
		ifFalse: [ yOrigin := origin y ].
	aRectangle corner y < corner y
		ifTrue: [ aBlock value: (origin x @ (yCorner := aRectangle corner y) corner: corner) ]
		ifFalse: [ yCorner := corner y ].
	aRectangle origin x > origin x 
		ifTrue: [ aBlock value: (origin x @ yOrigin corner: aRectangle origin x @ yCorner) ].
	aRectangle corner x < corner x 
		ifTrue: [ aBlock value: (aRectangle corner x @ yOrigin corner: corner x @ yCorner) ]
]

{ #category : 'accessing' }
Rectangle >> bottom [
	"Answer the position of the receiver's bottom horizontal line."
	^corner y
]

{ #category : 'accessing' }
Rectangle >> bottom: aNumber [
	^origin corner: corner x @ aNumber
]

{ #category : 'accessing' }
Rectangle >> bottomCenter [
	"Answer the point at the center of the bottom horizontal line of the receiver."
	^self center x @ self bottom
]

{ #category : 'accessing' }
Rectangle >> bottomLeft [
	"Answer the point at the left edge of the bottom horizontal line of the receiver."
	^origin x @ corner y
]

{ #category : 'accessing' }
Rectangle >> bottomRight [
	"Answer the point at the right edge of the bottom horizontal line of the receiver."
	^corner
]

{ #category : 'accessing' }
Rectangle >> boundingBox [
	^ self
]

{ #category : 'accessing' }
Rectangle >> center [
	"Answer the point at the center of the receiver."
	^self topLeft + self bottomRight // 2
]

{ #category : 'testing' }
Rectangle >> containsPoint: aPoint  [
	"Answer whether aPoint is within the receiver."

	^origin <= aPoint and: [aPoint < corner]
]

{ #category : 'testing' }
Rectangle >> containsRect: aRect [
	"Answer whether aRect is within the receiver (OK to coincide)."

	^ aRect origin >= origin and: [aRect corner <= corner]

]

{ #category : 'accessing' }
Rectangle >> corner [
	"Answer the point at the bottom right corner of the receiver."
	^corner
]

{ #category : 'accessing' }
Rectangle >> corners [
	"Return an array of corner points in the order of a quadrilateral spec for WarpBlt"
	^ Array with: self topLeft with: self bottomLeft with: self bottomRight with: self topRight
]

{ #category : 'rectangle functions' }
Rectangle >> encompass: aPoint  [
	"Answer a Rectangle that contains both the receiver and aPoint.  5/30/96 sw"

	^ Rectangle 
		origin: (origin min: aPoint)
		corner: (corner max:  aPoint)
]

{ #category : 'truncation and round off' }
Rectangle >> encompassingIntegerRectangle [
	"Answer a Rectangle whose origin and corner are integer, and that completely includes the receiver."

	(origin isIntegerPoint and: [ corner isIntegerPoint ]) ifTrue: [ ^self ].
	^Rectangle origin: origin floor corner: self corner ceiling
]

{ #category : 'rectangle functions' }
Rectangle >> expandBy: delta  [
	"Answer a Rectangle that is outset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin - delta origin 
				corner: corner + delta corner ]
		ifFalse: [
			Rectangle
				origin: origin - delta 
				corner: corner + delta ]
]

{ #category : 'rectangle functions' }
Rectangle >> extendBy: delta  [
	"Answer a Rectangle with the same origin as the receiver, but whose corner is offset by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin
				corner: corner + delta corner ]
		ifFalse: [
			Rectangle
				origin: origin
				corner: corner + delta ]
]

{ #category : 'accessing' }
Rectangle >> extent [
	"Answer with a rectangle with origin 0@0 and corner the receiver's 
	width @ the receiver's height."
	^corner - origin
]

{ #category : 'transforming' }
Rectangle >> flippedBy: direction centerAt: aPoint  [
	"Return a copy flipped #vertical or #horizontal, about aPoint."
	| futureOrigin futureCorner |
	direction == #horizontal
		ifTrue:
			[futureOrigin := self topRight.
			futureCorner := self bottomLeft]
		ifFalse: [direction == #vertical
			ifTrue:
				[futureOrigin := self bottomLeft.
				futureCorner := self topRight]
			ifFalse: [self error: 'unrecognizable direction']].
	^ (futureOrigin flipBy: direction centerAt: aPoint)
		corner: (futureCorner flipBy: direction centerAt: aPoint)
]

{ #category : 'testing' }
Rectangle >> hasPositiveExtent [
	^ (corner x > origin x) and: [corner y > origin y]
]

{ #category : 'comparing' }
Rectangle >> hash [
	"Hash is reimplemented because = is implemented."

	^origin hash bitXor: corner hash
]

{ #category : 'accessing' }
Rectangle >> height [
	"Answer the height of the receiver."
	^corner y - origin y
]

{ #category : 'accessing' }
Rectangle >> innerCorners [
	"Return an array of inner corner points,
	ie, the most extreme pixels included,
	in the order of a quadrilateral spec for WarpBlt"
	| r1 |
	r1 := self topLeft corner: self bottomRight - `1@1`.
	^ Array with: r1 topLeft with: r1 bottomLeft with: r1 bottomRight with: r1 topRight
]

{ #category : 'rectangle functions' }
Rectangle >> insetBy: delta  [
	"Answer a Rectangle that is inset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin + delta origin 
				corner: corner - delta corner ]
		ifFalse: [
			Rectangle
				origin: origin + delta 
				corner: corner - delta ]
]

{ #category : 'rectangle functions' }
Rectangle >> insetOriginBy: originDeltaPoint cornerBy: cornerDeltaPoint  [
	"Answer a Rectangle that is inset from the receiver by a given amount in the 
	origin and corner."

	^Rectangle
		origin: origin + originDeltaPoint
		corner: corner - cornerDeltaPoint
]

{ #category : 'mathematical functions' }
Rectangle >> interpolateAt: amountDone [
	"Interpolate between the instance and end after the specified amount has been done (0 - 1)."

	^ self extent * amountDone + origin
]

{ #category : 'rectangle functions' }
Rectangle >> intersect: aRectangle [
	"Answer a Rectangle that is the area in which the receiver overlaps with 
	aRectangle. Optimized for speed; old code read:
		^Rectangle 
			origin: (origin max: aRectangle origin)
			corner: (corner min: aRectangle corner)
	Note: Answer is always a new instance. It is OK to modify it as needed.
	Warning! If receiver and argument don't intersect at all, answer will be
		a rectangle that answers false to #hasPositiveExtent and 0 to #area.
		Usually not what you want!
		You may check using #intersects: before calling this method.
	"
	| aPoint left right top bottom |
	aPoint := aRectangle origin.
	aPoint x > origin x ifTrue:[left := aPoint x] ifFalse:[left := origin x].
	aPoint y > origin y ifTrue:[top := aPoint y] ifFalse:[top := origin y].
	aPoint := aRectangle corner.
	aPoint x < corner x ifTrue:[right := aPoint x] ifFalse:[right := corner x].
	aPoint y < corner y ifTrue:[bottom := aPoint y] ifFalse:[bottom := corner y].
	^Rectangle
		origin: (left@top)
		corner: (right@bottom)
]

{ #category : 'testing' }
Rectangle >> intersects: aRectangle  [
	"Answer whether aRectangle intersects the receiver anywhere."
	"Optimized; old code answered:
		(origin max: aRectangle origin) < (corner min: aRectangle corner)"

	| rOrigin rCorner |
	rOrigin := aRectangle origin.
	rCorner := aRectangle corner.
	rCorner x <= origin x	ifTrue: [^ false].
	rCorner y <= origin y	ifTrue: [^ false].
	rOrigin x >= corner x	ifTrue: [^ false].
	rOrigin y >= corner y	ifTrue: [^ false].
"None of the two rectangle shall be empty"
	corner x <= origin x	ifTrue: [^ false].
	corner y <= origin y	ifTrue: [^ false].
	rCorner x <= rOrigin x	ifTrue: [^ false].
	rCorner y <= rOrigin y	ifTrue: [^ false].
	^ true

]

{ #category : 'testing' }
Rectangle >> is: aSymbol [
	^ aSymbol == #Rectangle or: [ super is: aSymbol ]
]

{ #category : 'testing' }
Rectangle >> isTall [
	^ self height > self width
]

{ #category : 'testing' }
Rectangle >> isWide [
	^ self width > self height
]

{ #category : 'accessing' }
Rectangle >> left [
	"Answer the position of the receiver's left vertical line."
	^origin x
]

{ #category : 'accessing' }
Rectangle >> left: aNumber [
	^aNumber @ origin y corner: corner
]

{ #category : 'accessing' }
Rectangle >> leftCenter [
	"Answer the point at the center of the receiver's left vertical line."
	^self left @ self center y
]

{ #category : 'rectangle functions' }
Rectangle >> merge: aRectangle [
	"Answer a Rectangle that contains both the receiver and aRectangle.
	Note: Ignore empty rectangles!
	See #quickMerge:"

	aRectangle hasPositiveExtent ifFalse: [ ^self ].
	self hasPositiveExtent ifFalse: [ ^aRectangle ].
	^Rectangle 
		origin: (origin min: aRectangle origin)
		corner: (corner max: aRectangle corner)
]

{ #category : 'transforming' }
Rectangle >> newRect2From: newRectBlock [
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect aHand delay |
	delay := Delay forMilliseconds: 10.
	rect := self.
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	[Sensor isAnyButtonPressed] whileTrue: 
		[delay wait.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen deferredUpdatingDisplay border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen forceDeferredToScreen: (rect merge: newRect).
			rect := newRect]].
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		releaseMouseFocus;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect
]

{ #category : 'transforming' }
Rectangle >> newRectFrom: newRectBlock [
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow aHand delay |
	delay := Delay forMilliseconds: 10.
	buttonStart := buttonNow := Sensor isAnyButtonPressed.
	rect := self.
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	[buttonNow == buttonStart] whileTrue: 
		[delay wait.
		buttonNow := Sensor isAnyButtonPressed.
		newRect := newRectBlock value: rect.
		newRect = rect ifFalse:
			[DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen deferredUpdatingDisplay border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			DisplayScreen forceDeferredToScreen: (rect merge: newRect).
			rect := newRect]].
	DisplayScreen deferredUpdatingDisplay border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	DisplayScreen forceDeferredToScreen: rect.
	" pay the price for reading the sensor directly ; get this party started "
	aHand := self runningWorld activeHand.
	aHand
		releaseMouseFocus;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect
]

{ #category : 'accessing' }
Rectangle >> origin [
	"Answer the point at the top left corner of the receiver."
	^origin
]

{ #category : 'rectangle functions' }
Rectangle >> outsetBy: delta  [
	"Answer a Rectangle that is outset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin - delta origin 
				corner: corner + delta corner ]
		ifFalse: [
			Rectangle
				origin: origin - delta 
				corner: corner + delta ]
]

{ #category : 'printing' }
Rectangle >> printOn: aStream  [
	origin printOn: aStream.
	aStream nextPutAll: ' corner: '.
	corner printOn: aStream
]

{ #category : 'rectangle functions' }
Rectangle >> quickMerge: aRectangle [
	"Answer the receiver if it encloses the given rectangle or the merge of the two rectangles if it doesn't.
	This method is an optimization to reduce extra rectangle creations.
	Accept nil as argument.
	Note: Ignore empty rectangles!"

	| useRcvr rOrigin rCorner minX maxX minY maxY |
	aRectangle ifNil: [ ^self ].
	aRectangle hasPositiveExtent ifFalse: [ ^self ].
	self hasPositiveExtent ifFalse: [ ^aRectangle ].

	useRcvr := true.
	rOrigin := aRectangle origin.
	rCorner := aRectangle corner.
	minX := rOrigin x < origin x ifTrue: [ useRcvr := false. rOrigin x ] ifFalse: [ origin x ].
	maxX := rCorner x > corner x ifTrue: [ useRcvr := false. rCorner x ] ifFalse: [ corner x ].
	minY := rOrigin y < origin y ifTrue: [ useRcvr := false. rOrigin y ] ifFalse: [ origin y ].
	maxY := rCorner y > corner y ifTrue:  [useRcvr := false. rCorner y ] ifFalse: [ corner y ].

	^useRcvr
		ifTrue: [ self ]
		ifFalse: [ Rectangle origin: minX@minY corner: maxX@maxY ]
]

{ #category : 'rectangle functions' }
Rectangle >> quickMerge: aRectangle intersectionWith: anotherRectangle [
	"Take the intersection of aRectangle and anotherRectangle. Merge that with the receiver.
	Answer the receiver if possible, or a new Rectangle if needed, an optimization to reduce rectangle creations.
	Accept nil as the first argument.
	Note: Ignore empty rectangles!"
	
	"Quick response in the usual case."
	(anotherRectangle containsRect: aRectangle) ifTrue: [
		^self quickMerge: aRectangle ].
	
	"Intersection between arguments is empty: nothing to do."
	(aRectangle intersects: anotherRectangle) ifFalse: [
		^self ].

	"Assume this is an unusual case, an extra Rectangle creation may be done."
	^self quickMerge: (aRectangle intersect: anotherRectangle)
]

{ #category : 'accessing' }
Rectangle >> right [
	"Answer the position of the receiver's right vertical line."
	^corner x
]

{ #category : 'accessing' }
Rectangle >> right: aNumber [
	^origin corner: aNumber @ corner y
]

{ #category : 'accessing' }
Rectangle >> rightCenter [
	"Answer the point at the center of the receiver's right vertical line."
	^self right @ self center y
]

{ #category : 'truncation and round off' }
Rectangle >> rounded [
	"Answer a Rectangle whose origin and corner are rounded."

	^Rectangle origin: origin rounded corner: self corner rounded
]

{ #category : 'truncation and round off' }
Rectangle >> roundedHAFZ [
	"Answer a Rectangle whose origin and corner are rounded."

	^Rectangle origin: origin roundedHAFZ corner: self corner roundedHAFZ
]

{ #category : 'transforming' }
Rectangle >> scaledBy: scale  [
	"Answer a Rectangle scaled by scale, a Point or a scalar."

	^Rectangle origin: origin * scale corner: corner * scale
]

{ #category : 'private' }
Rectangle >> setOrigin: topLeft corner: bottomRight [
	origin := topLeft.
	corner := bottomRight
]

{ #category : 'transforming' }
Rectangle >> squishedWithin: aRectangle [
	"Return an adjustment of the receiver that fits within aRectangle by reducing its size, not by changing its origin.  "

	^ origin corner: (corner min: aRectangle bottomRight)

"(50 @ 50 corner: 160 @ 100) squishedWithin:  (20 @ 10 corner: 90 @ 85)"

]

{ #category : 'printing' }
Rectangle >> storeOn: aStream  [
	"printed form is good for storing too"
	
	aStream nextPut: $(.
	self printOn: aStream.
	aStream nextPut: $).
]

{ #category : 'accessing' }
Rectangle >> top [
	"Answer the position of the receiver's top horizontal line."
	^origin y
]

{ #category : 'accessing' }
Rectangle >> top: aNumber [
	^origin x @ aNumber corner: corner
]

{ #category : 'accessing' }
Rectangle >> topCenter [
	"Answer the point at the center of the receiver's top horizontal line."
	^self center x @ self top
]

{ #category : 'accessing' }
Rectangle >> topLeft [
	"Answer the point at the top left corner of the receiver's top horizontal line."
	^origin
]

{ #category : 'accessing' }
Rectangle >> topRight [
	"Answer the point at the top right corner of the receiver's top horizontal line."
	^corner x @ origin y
]

{ #category : 'transforming' }
Rectangle >> translatedAndSquishedToBeWithin: aRectangle [
	"Return an adjustment of the receiver that fits within aRectangle by
		- translating it to be within aRectangle if necessary, then
		- reducing its size, if necessary"

	^ (self translatedToBeWithin: aRectangle) squishedWithin: aRectangle
]

{ #category : 'transforming' }
Rectangle >> translatedBy: delta  [
	"Answer a Rectangle translated by delta, a Point or a scalar."

	^Rectangle origin: origin + delta corner: corner + delta
]

{ #category : 'rectangle functions' }
Rectangle >> translatedToBeWithin: aRectangle [
	"Answer a copy of the receiver that does not extend beyond aRectangle.  7/8/96 sw"

	^ self translatedBy: (self amountToTranslateWithin: aRectangle)
]

{ #category : 'truncation and round off' }
Rectangle >> truncateTo: grid [
	"Answer a Rectangle whose origin and corner are truncated to grid x and grid y."

	^Rectangle origin: (origin truncateTo: grid)
				corner: (corner truncateTo: grid)
]

{ #category : 'truncation and round off' }
Rectangle >> truncated [
	"Answer a Rectangle whose origin and corner have any fractional parts removed. Answer the receiver if its coordinates are already integral."

	(origin x isInteger and:
	[origin y isInteger and:
	[corner x isInteger and:
	[corner y isInteger]]])
		ifTrue: [^ self].

	^ Rectangle origin: origin truncated corner: corner truncated

]

{ #category : 'rectangle functions' }
Rectangle >> updateMerging: aRectangle [
	"Modify receiver"

	origin := origin min: aRectangle origin.
	corner := corner max: aRectangle corner.
]

{ #category : 'accessing' }
Rectangle >> width [
	"Answer the width of the receiver."
	^corner x - origin x
]

{ #category : 'rectangle functions' }
Rectangle >> withHeight: height  [
	"Return a copy of me with a different height"
	^ origin corner: corner x @ (origin y + height)
]

{ #category : 'rectangle functions' }
Rectangle >> withLeft: x  [
	"Return a copy of me with a different left x"
	^ x @ origin y corner: corner x @ corner y
]

{ #category : 'rectangle functions' }
Rectangle >> withRight: x  [
	"Return a copy of me with a different right x"
	^ origin x @ origin y corner: x @ corner y
]
