'From Cuis 6.0 [latest update: #5699] on 17 March 2023 at 6:19:21 pm'!

!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 3/17/2023 17:40:18'!
areasOutside: aRectangle do: aBlock
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
		ifTrue: [ aBlock value: (aRectangle corner x @ yOrigin corner: corner x @ yCorner) ]! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'jmv 3/17/2023 17:39:22'!
intersect: aRectangle 
	"Answer a Rectangle that is the area in which the receiver overlaps with 
	aRectangle. Optimized for speed; old code read:
		^Rectangle 
			origin: (origin max: aRectangle origin)
			corner: (corner min: aRectangle corner)
	Note: Answer is always a new instance. It is OK to modify it as needed.
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
! !

