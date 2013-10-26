'From Cuis 4.0 of 21 April 2012 [latest update: #1449] on 20 September 2012 at 10:37:17 pm'!

!InnerHierarchicalListMorph methodsFor: 'geometry' stamp: 'jmv 9/20/2012 22:11'!
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last |
	self hasSubmorphs ifFalse: [ ^nil ].
	(aPoint > (0@0) and: [ aPoint < extent ]) ifFalse: [ ^nil ].
	ptY _ aPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	self firstSubmorph morphPositionInOwner y > ptY ifTrue: [ ^nil ].
	last _ self lastSubmorph.
	last morphPositionInOwner y + last morphHeight < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^self 
		findSubmorphBinary: [ :m |
			(m morphPositionInOwner y <= ptY and: [ m morphPositionInOwner y + m morphHeight >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPositionInOwner y + (m morphHeight // 2) > ptY ifTrue: [-1] ifFalse: [1]]]! !


!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 9/20/2012 22:12'!
itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	^scroller itemFromPoint: (scroller internalize: aPoint)! !


!InnerPluggableMorph methodsFor: 'geometry' stamp: 'jmv 9/20/2012 22:05'!
adjustExtent
	self flag: #jmvVer2.	"Do it just adding submorph extents!!"
	self submorphBounds ifNotNil: [ :r |
		self morphExtent: r bottomRight - self morphPositionInWorld ]! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/20/2012 21:55'!
drawLineToggleToTextFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
	"If I am not the only item in my container, draw the line between:
		- my toggle (if any) or my left edge (if no toggle)
		- and my text left edge"

	| aMorphCenter hLineY hLineLeft rect right |
	anIndentingListItemMorph isSoleItem ifTrue: [ ^ self ].
	rect _ anIndentingListItemMorph toggleRectangle.
	aMorphCenter _ anIndentingListItemMorph externalize: rect center.
	right _ (anIndentingListItemMorph externalize: rect rightCenter) x.
	hLineY _ aMorphCenter y.
	hasToggle
		ifTrue: [ hLineLeft _ right - 3 ]
		ifFalse: [ hLineLeft _ aMorphCenter x - 1 ].
	"Draw line from toggle to text"
	aCanvas
		line: hLineLeft @ hLineY
		to: right + 0 @ hLineY
		width: 1
		color: lineColor! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/20/2012 22:36'!
drawLinesOn: aCanvas 
	| lColor bottomY topY tx |
	lColor _ Theme current line.
	tx _ aCanvas currentTransformation.
	topY _ (tx internalizePosition: aCanvas clipRect topLeft) y min: (tx internalizePosition: aCanvas clipRect topRight) y.
	bottomY _ (tx internalizePosition: aCanvas clipRect bottomLeft) y max: (tx internalizePosition: aCanvas clipRect bottomRight) y.
	self submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(submorph morphPositionInOwner y between: topY and: bottomY) or: [
				submorph nextSibling notNil and: [
					submorph nextSibling morphPositionInOwner y between: topY and: bottomY ] ] ])
		ifTrue: [
			self
				drawLinesFor: submorph
				on: aCanvas
				lineColor: lColor ]]
	! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/20/2012 21:49'!
drawLinesToFirstChildFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor
	"Draw line from me to next sibling"

	| vLineX vLineTop vLineBottom childCenter firstChild |
	"child in the drawn tree. it is acually our submorph"
	firstChild _ anIndentingListItemMorph firstChild.
	childCenter _ firstChild externalize: firstChild toggleRectangle center.
	vLineX _ childCenter x - 1.
	vLineTop _ (anIndentingListItemMorph
		externalize: anIndentingListItemMorph morphExtent) y.
	firstChild hasToggle
		ifTrue: [ vLineBottom _ childCenter y - 7 ]
		ifFalse: [ vLineBottom _ childCenter y ].
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @vLineBottom
		width: 1
		color: lineColor! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 9/20/2012 21:47'!
drawLinesToNextSiblingFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
	| vLineX aMorphCenter vLineTop vLineBottom nextSibCenter nextSibling |

	nextSibling _ anIndentingListItemMorph nextSibling.
	nextSibCenter _ nextSibling externalize: nextSibling toggleRectangle center.

	aMorphCenter _ anIndentingListItemMorph externalize:
		 anIndentingListItemMorph toggleRectangle center.
	vLineX _ aMorphCenter x - 1.
	hasToggle
		ifTrue: [ vLineTop _ aMorphCenter y + 5 ]
		ifFalse: [ vLineTop _ aMorphCenter y ].
	nextSibling hasToggle
		ifTrue: [ vLineBottom _ nextSibCenter y - 7 ]
		ifFalse: [ vLineBottom _ nextSibCenter y ].
	"Draw line from me to next sibling"
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @ vLineBottom
		width: 1
		color: lineColor! !

!methodRemoval: InnerHierarchicalListMorph #bottomVisibleItemForCanvas:!
InnerHierarchicalListMorph removeSelector: #bottomVisibleItemForCanvas:!
!methodRemoval: InnerHierarchicalListMorph #topVisibleItemForCanvas:!
InnerHierarchicalListMorph removeSelector: #topVisibleItemForCanvas:!
