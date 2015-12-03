'From Cuis 4.2 of 25 July 2013 [latest update: #2596] on 29 November 2015 at 9:42:06.6167 am'!

!DamageRecorder methodsFor: 'recording' stamp: 'jmv 11/29/2015 09:41'!
recordInvalidRect: requestedRect
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle.
	Rectangles are specified in world coordinates (might be not the same as Display coordinates if there is some origin set on some canvas. Usually there will be none, but we can't be sure)
	"
	"Details: Damaged rectangles are often identical or overlap significantly. In these cases, we merge them to reduce the number of damage rectangles that must be processed when the display is updated. Moreover, above a certain threshold, we ignore the individual rectangles completely, and simply do a complete repaint on the next cycle.
	jmv: Important. There should be no overlapping rectangles in the list. If there are, translucent morphs might be drawn several times, with visible defects.
	WRT performance, the different paths for various intersections seem a bit complicated. I could not find strong evidence of the best way.
	Leave it as it is right now."

	| newRect mergeRect a indexToReuse |
	totalRepaint ifTrue: [^ self].  "planning full repaint; don't bother collecting damage"

	indexToReuse _ nil.
	newRect _ requestedRect truncated.
	invalidRects doWithIndex: [ :rect :index |
		rect ifNil: [
			indexToReuse ifNil: [ indexToReuse _ index ]]
		ifNotNil: [
			"No two rectangles should intersect"
			(rect intersects: newRect) ifTrue: [
				"requestedRect (possibly smaller than newRect) already in. Nothing to do then."
				(rect containsRect: requestedRect) ifTrue: [ 
					^self ].
				"Some rect included in newRect. Remove it and continue, as newRect could still intersect others."
				(newRect containsRect: rect) ifTrue: [
					invalidRects at: index put: nil.	"Effectively like 'invalidRects remove: rect', but without performance penalty."
					indexToReuse ifNil: [ indexToReuse _ index ]].			
				"Merge rectangles if they overlap significantly"
				a _ (rect intersect: newRect) area.
				(a > 40
					and: ["Avoid combining a vertical and horizontal rects.
						  Can make a big diff and we only test when likely."
						  a > (newRect area // 4) or: [ a > (rect area // 4)]])
				ifTrue: [
					"Merge into newRect, as any overlap with forecoming rectangles in the iteration is detected and treated."
					newRect
						setOrigin: (rect origin min: newRect origin)
						corner: (rect corner max: newRect corner).
					invalidRects at: index put: nil.	"Effectively like 'invalidRects remove: rect', but without performance penalty."
					indexToReuse ifNil: [ indexToReuse _ index ]]
				ifFalse: [
					"Avoid intersections!!"
					newRect areasOutside: rect do: [ :nonIntersection |
						"We have brand new newRects. Start all over with each of them."
						self recordInvalidRect: nonIntersection ].
					"newRect no longer needed, then."
					^self ]]]].

	invalidRects size >= 15 ifTrue: [
		"if there are too many separate areas, merge them all"
		mergeRect _ Rectangle merging: invalidRects.
		self reset.
		mergeRect ifNotNil: [
			invalidRects addLast: mergeRect ].
		indexToReuse _ nil ].

	"Add the given rectangle to the damage list"
	indexToReuse
		ifNil: [
			invalidRects addLast: newRect ]
		ifNotNil: [
			invalidRects at: indexToReuse put: newRect ]! !

