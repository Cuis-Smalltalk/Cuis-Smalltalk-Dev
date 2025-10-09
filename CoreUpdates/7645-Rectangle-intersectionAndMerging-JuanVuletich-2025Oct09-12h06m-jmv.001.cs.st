'From Cuis7.5 [latest update: #7644] on 9 October 2025 at 12:06:33 pm'!

!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 10/9/2025 11:52:40'!
intersect: aRectangle
	"Answer a Rectangle that is the area in which the receiver overlaps with 
	aRectangle. Optimized for speed; old code read:
		^Rectangle 
			origin: (origin max: aRectangle origin)
			corner: (corner min: aRectangle corner)
	Note: Answer is always a new instance. It is OK to modify it as needed.
	Warning!! If receiver and argument don't intersect at all, answer will be
		a rectangle that answers false to #hasPositiveExtent and 0 to #area.
		Usually not what you want!!
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
		corner: (right@bottom)! !


!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 10/9/2025 10:11:48'!
quickMerge: aRectangle intersectionWith: anotherRectangle
	"Take the intersection of aRectangle and anotherRectangle. Merge that with the receiver.
	Answer the receiver if possible, or a new Rectangle if needed, an optimization to reduce rectangle creations.
	Accept nil as the first argument.
	Note: Ignore empty rectangles!!"
	
	"Quick response in the usual case."
	(anotherRectangle containsRect: aRectangle) ifTrue: [
		^self quickMerge: aRectangle ].
	
	"Intersection between arguments is empty: nothing to do."
	(aRectangle intersects: anotherRectangle) ifFalse: [
		^self ].

	"Assume this is an unusual case, an extra Rectangle creation may be done."
	^self quickMerge: (aRectangle intersect: anotherRectangle)! !


!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 10/9/2025 12:03:44'!
merge: aRectangle
	"Answer a Rectangle that contains both the receiver and aRectangle.
	Note: Ignore empty rectangles!!
	See #quickMerge:"

	aRectangle hasPositiveExtent ifFalse: [ ^self ].
	self hasPositiveExtent ifFalse: [ ^aRectangle ].
	^Rectangle 
		origin: (origin min: aRectangle origin)
		corner: (corner max: aRectangle corner)! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 10/9/2025 12:03:55'!
quickMerge: aRectangle
	"Answer the receiver if it encloses the given rectangle or the merge of the two rectangles if it doesn't.
	This method is an optimization to reduce extra rectangle creations.
	Accept nil as argument.
	Note: Ignore empty rectangles!!"

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
		ifFalse: [ Rectangle origin: minX@minY corner: maxX@maxY ]! !

