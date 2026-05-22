"
The morph that displays the list in a HierarchicalListMorph.
"
Class {
	#name : 'InnerHierarchicalListMorph',
	#superclass : 'InnerPluggableMorph',
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'geometry' }
InnerHierarchicalListMorph >> adjustExtent [
	"And reposition submorphs"
	| w y |
	"make all items wide, so selection indicator is wide too"
	w := self desiredWidth.
	y := 0.
	self submorphsDo: [ :m | | h |
		h := m morphHeight.
		m position: 0@y extent: w@h.
		y := y + h ].
	self morphExtent: w@y
]

{ #category : 'geometry' }
InnerHierarchicalListMorph >> desiredWidth [
	^submorphs inject: owner viewableWidth into: [ :previousValue :each |
		previousValue max: each desiredWidth ]
]

{ #category : 'drawing' }
InnerHierarchicalListMorph >> drawLineToggleToTextFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle [
	"If I am not the only item in my container, draw the line between:
		- my left edge
		- and my text left edge"

	| aMorphCenter hLineY hLineLeft rect right |
	anIndentingListItemMorph isSoleItem ifTrue: [ ^ self ].
	hasToggle ifFalse: [
		rect := anIndentingListItemMorph toggleRectangle.
		aMorphCenter := anIndentingListItemMorph externalize: rect center.
		right := (anIndentingListItemMorph externalize: rect rightCenter) x.
		hLineY := aMorphCenter y.
		hLineLeft := aMorphCenter x.
		aCanvas
			line: hLineLeft @ hLineY
			to: right @ hLineY
			width: self lineWidth
			color: lineColor ]
]

{ #category : 'drawing' }
InnerHierarchicalListMorph >> drawLinesFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor  [
	anIndentingListItemMorph indentLevel > 0 ifTrue: [
		| hasToggle |
		hasToggle := anIndentingListItemMorph hasToggle.
	
		"Draw line from toggle to text"
		self
			drawLineToggleToTextFor: anIndentingListItemMorph
			on: aCanvas
			lineColor: lineColor
			hasToggle: hasToggle.

		"Draw the line from toggle to the nextSibling's toggle"
		anIndentingListItemMorph nextSibling ifNotNil: [
			self
				drawLinesToNextSiblingFor: anIndentingListItemMorph
				on: aCanvas
				lineColor: lineColor
				hasToggle: hasToggle ]].

	"If it have children and am expanded, draw a line to its first child"
	(anIndentingListItemMorph firstChild notNil and: [
			anIndentingListItemMorph isExpanded ]) ifTrue: [
		self
			drawLinesToFirstChildFor: anIndentingListItemMorph
			on: aCanvas
			lineColor: lineColor]
]

{ #category : 'drawing' }
InnerHierarchicalListMorph >> drawLinesOn: aCanvas  [
	| lColor bottomY topY tx clipRect |
	lColor := Theme current line.
	tx := aCanvas currentTransformation.
	clipRect := aCanvas clipRect.
	topY := (tx internalizePosition: clipRect topLeft) y min: (tx internalizePosition: clipRect topRight) y.
	bottomY := (tx internalizePosition: clipRect bottomLeft) y max: (tx internalizePosition: clipRect bottomRight) y.
	self submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(submorph morphPosition y between: topY and: bottomY) or: [
				submorph nextSibling notNil and: [
					submorph nextSibling morphPosition y between: topY and: bottomY ] ] ])
		ifTrue: [
			self
				drawLinesFor: submorph
				on: aCanvas
				lineColor: lColor ]]
	
]

{ #category : 'drawing' }
InnerHierarchicalListMorph >> drawLinesToFirstChildFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor [
	"Draw line from me to next sibling"

	| vLineX vLineTop vLineBottom childCenter firstChild |
	"child in the drawn tree. it is acually our submorph"
	firstChild := anIndentingListItemMorph firstChild.
	childCenter := firstChild externalize: firstChild toggleRectangle center.
	vLineX := childCenter x.
	vLineTop := (anIndentingListItemMorph
		externalize: anIndentingListItemMorph morphExtent) y.
	firstChild hasToggle
		ifTrue: [ vLineBottom := childCenter y - (self lineSlack*4//3) ]
		ifFalse: [ vLineBottom := childCenter y ].
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @vLineBottom
		width: self lineWidth
		color: lineColor
]

{ #category : 'drawing' }
InnerHierarchicalListMorph >> drawLinesToNextSiblingFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle [
	| vLineX aMorphCenter vLineTop vLineBottom nextSibCenter nextSibling |

	nextSibling := anIndentingListItemMorph nextSibling.
	nextSibCenter := nextSibling externalize: nextSibling toggleRectangle center.

	aMorphCenter := anIndentingListItemMorph externalize:
		 anIndentingListItemMorph toggleRectangle center.
	vLineX := aMorphCenter x.
	hasToggle
		ifTrue: [
			anIndentingListItemMorph isExpanded
				ifTrue: [ vLineTop := self lineSlack *3//2 + aMorphCenter y  ]
				ifFalse: [ vLineTop := self lineSlack + aMorphCenter y  ]]
		ifFalse: [ vLineTop := aMorphCenter y ].
	nextSibling hasToggle
		ifTrue: [ vLineBottom := nextSibCenter y - (self lineSlack*4//3) ]
		ifFalse: [ vLineBottom := nextSibCenter y ].
	"Draw line from me to next sibling"
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @ vLineBottom
		width: self lineWidth
		color: lineColor
]

{ #category : 'drawing' }
InnerHierarchicalListMorph >> drawOn: aCanvas [

	(Preferences at: #showLinesInHierarchyViews) ifTrue:[
		self drawLinesOn: aCanvas ]
]

{ #category : 'event handling testing' }
InnerHierarchicalListMorph >> handlesKeyboardFocus [
	"Return true if the receiver wishes to handle keyboard focus.
	Answer false because it is handled by our owner, a HierarchicalListMorph"

	^false
]

{ #category : 'geometry' }
InnerHierarchicalListMorph >> itemFromPoint: aPoint [
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last |
	self hasSubmorphs ifFalse: [ ^nil ].
	(aPoint > `0@0` and: [ aPoint < extent ]) ifFalse: [ ^nil ].
	ptY := aPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	self firstSubmorph morphPosition y > ptY ifTrue: [ ^nil ].
	last := self lastSubmorph.
	last morphPosition y + last morphHeight < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^self 
		findSubmorphBinary: [ :m |
			(m morphPosition y <= ptY and: [ m morphPosition y + m morphHeight >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPosition y + (m morphHeight // 2) > ptY ifTrue: [-1] ifFalse: [1]]]
]

{ #category : 'drawing' }
InnerHierarchicalListMorph >> lineSlack [
	^FontFamily defaultPointSize //2
]

{ #category : 'drawing' }
InnerHierarchicalListMorph >> lineWidth [
	^FontFamily defaultPointSize//6
]
