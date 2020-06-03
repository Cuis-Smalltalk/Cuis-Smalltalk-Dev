'From Cuis 5.0 [latest update: #4203] on 3 June 2020 at 2:26:53 pm'!

!Point methodsFor: 'testing' stamp: 'jmv 6/3/2020 11:55:20'!
isIntegerPoint
	^x isInteger and:[y isInteger]! !


!Rectangle methodsFor: 'truncation and round off' stamp: 'jmv 6/3/2020 11:56:00'!
encompassingIntegerRectangle
	"Answer a Rectangle whose origin and corner are integer, and that completely includes the receiver."

	(origin isIntegerPoint and: [ corner isIntegerPoint ]) ifTrue: [ ^self ].
	^Rectangle origin: origin floor corner: self corner ceiling! !


!Rectangle class methodsFor: 'instance creation' stamp: 'jmv 6/3/2020 14:23:25'!
encompassingInteger: listOfPoints
	"Like #encompassing:, but with integer coordinates."
	| topLeft bottomRight |
	topLeft _ bottomRight _ nil.
	listOfPoints do: [ :p |
		topLeft
			ifNil: [
				topLeft _ p floor.
				bottomRight _ p ceiling]
			ifNotNil: [
				topLeft _ topLeft min: p floor.
				bottomRight _ bottomRight max: p ceiling]].
	^ topLeft corner: bottomRight! !


!AffineTransformation methodsFor: 'transforming rects' stamp: 'jmv 6/3/2020 11:45:09'!
displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
		and vertical bounds and integer coordinates (i.e. adisplayBounds)."

	^Rectangle encompassingInteger: (aRectangle corners collect: [ :pt |
		self transform: pt ])! !


!MorphicTranslation methodsFor: 'transforming rects' stamp: 'jmv 6/3/2020 11:45:54'!
displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
	and vertical bounds and integer coordinates (i.e. adisplayBounds)."

	^(aRectangle translatedBy: self translation) encompassingIntegerRectangle! !


!DamageRecorder methodsFor: 'private' stamp: 'jmv 6/3/2020 11:56:09'!
pvtInnerRecordInvalidRect: requestedRect
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle.
	Rectangles are specified in world coordinates (might be not the same as Display coordinates if there is some origin set on some canvas. Usually there will be none, but we can't be sure)
	"
	"Details: Damaged rectangles are often identical or overlap significantly. In these cases, we merge them to reduce the number of damage rectangles that must be processed when the display is updated. Moreover, above a certain threshold, we ignore the individual rectangles completely, and simply do a complete repaint on the next cycle.
	jmv: Important. There should be no overlapping rectangles in the list. If there are, translucent morphs might be drawn several times, with visible defects.
	WRT performance, the different paths for various intersections seem a bit complicated. I could not find strong evidence of the best way.
	Leave it as it is right now."
	| newRect mergeRect indexToReuse |
	totalRepaint ifTrue: [ ^ self ].
	"planning full repaint; don't bother collecting damage"
	indexToReuse _ nil.
	newRect _ requestedRect encompassingIntegerRectangle.
	invalidRects withIndexDo: [ :oldRect :index |
		oldRect
			ifNil: [ indexToReuse ifNil: [ indexToReuse _ index ]]
			ifNotNil: [
				"No two rectangles should intersect"
				(oldRect intersects: newRect) ifTrue: [
					"newRect already in. Nothing to do then."
					(oldRect containsRect: newRect) ifTrue: [ ^ self ].
					"Some oldRect included in newRect. Remove it and continue, as newRect could still intersect others."
					(newRect containsRect: oldRect) ifTrue: [
						invalidRects
							at: index
							put: nil.
						"Effectively like 'invalidRects remove: rect', but without performance penalty."
						indexToReuse ifNil: [ indexToReuse _ index ]].
					"Merge rectangles if they overlap significantly, i.e. if the merge is not much larger than separated rectangles."
					mergeRect _ (oldRect origin min: newRect origin) corner: (oldRect corner max: newRect corner).
					mergeRect area > (newRect area + oldRect area * 2)
						ifTrue: [
							"Avoid intersections!!"
							newRect
								areasOutside: oldRect
								do: [ :nonIntersection |
									"We have brand new newRects. Start all over with each of them."
									self pvtInnerRecordInvalidRect: nonIntersection ].
							"newRect no longer needed, then."
							^ self ]
						ifFalse: [
							"Merge into newRect, as any overlap with forecoming rectangles in the iteration is detected and treated."
							newRect _ mergeRect.
							invalidRects
								at: index
								put: nil.
							"Effectively like 'invalidRects remove: rect', but without performance penalty."
							indexToReuse ifNil: [ indexToReuse _ index ]]]]].
	invalidRects size >= 10 ifTrue: [
		"if there are too many separate areas, merge them all"
		mergeRect _ Rectangle merging: invalidRects.
		self pvtReset.
		mergeRect ifNotNil: [ invalidRects addLast: mergeRect ].
		indexToReuse _ nil ].
	"Add the given rectangle to the damage list"
	indexToReuse
		ifNil: [ invalidRects addLast: newRect ]
		ifNotNil: [
			invalidRects
				at: indexToReuse
				put: newRect ].! !

!methodRemoval: AffineTransformation #primDisplayBoundsOfTransformOf:into: stamp: 'jmv 6/3/2020 14:22:10'!
AffineTransformation removeSelector: #primDisplayBoundsOfTransformOf:into:!
