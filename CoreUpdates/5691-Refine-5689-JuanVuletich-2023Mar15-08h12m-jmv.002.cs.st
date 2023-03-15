'From Cuis 6.0 [latest update: #5688] on 15 March 2023 at 10:15:20 am'!

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 3/15/2023 09:21:09'!
lineSlack
	^FontFamily defaultPointSize //2! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 3/14/2023 16:47:57'!
lineWidth
	^FontFamily defaultPointSize//6! !


!IndentingListItemMorph methodsFor: 'geometry' stamp: 'jmv 3/14/2023 16:36:48'!
indentWidth
	^FontFamily defaultPointSize! !


!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 3/15/2023 08:12:39'!
drawExpandedAt: aPoint height: h
	"For Tree View (IndentingListItemMorph)"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 3/15/2023 08:13:00'!
drawNotExpandedAt: aPoint height: h
	"For Tree View (IndentingListItemMorph)"

	self subclassResponsibility.! !


!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/15/2023 10:12:34'!
drawExpandedAt: aPoint height: h
	"For Tree View (IndentingListItemMorph)"

	| halfWidth width |
	width := h * 3/4.
	halfWidth := width / 2.
	self fillColor: `Color gray` do: [
		self moveTo: aPoint.
		self rel_MoveTo: (h/8) @ (h/4) negated;
			rel_LineTo: halfWidth @ width;
			rel_LineTo: halfWidth @ width negated;
			rel_LineTo: width negated @ 0 ].
		! !

!AbstractVectorCanvas methodsFor: 'drawing - icons' stamp: 'jmv 3/15/2023 10:12:27'!
drawNotExpandedAt: aPoint height: h
	"For Tree View (IndentingListItemMorph)"

	| halfWidth width |
	width := h * 3/4.
	halfWidth := width / 2.
	self fillColor: `Color gray` do: [
		self moveTo: aPoint.
		self rel_MoveTo: (h/4) @ halfWidth negated;
			rel_LineTo: width @ halfWidth;
			rel_LineTo: width negated @ halfWidth;
			rel_LineTo: 0 @ width negated.
		].! !


!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 3/15/2023 10:10:56'!
drawExpandedAt: aPoint height: h
	"For Tree View (IndentingListItemMorph)"

	| f |
	f := BitBltCanvas arrowOfDirection: #down size: h.
	self 
		image: f 
		at: aPoint x+(h//12) @ (aPoint y - (f height // 2)).! !

!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 3/15/2023 10:10:47'!
drawNotExpandedAt: aPoint height: h
	"For Tree View (IndentingListItemMorph)"

	| f |
	f := BitBltCanvas arrowOfDirection: #right size: h.
	self 
		image: f 
		at: aPoint x+(h//12) @ (aPoint y - (f height // 2)).! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 3/14/2023 16:49:12'!
drawLineToggleToTextFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
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
			color: lineColor ]! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 3/14/2023 17:20:30'!
drawLinesToFirstChildFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor
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
		color: lineColor! !

!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 3/15/2023 09:25:57'!
drawLinesToNextSiblingFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
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
		color: lineColor! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 3/15/2023 09:03:05'!
drawOn: aCanvas

	| x colorToUse centeringOffset |
	isHighlighted ifTrue: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: ((Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) alpha: 0.3) ].

	isSelected ifTrue: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].

	x := indentLevel * self indentWidth.

	complexContents hasContents ifTrue: [
		isExpanded
			ifTrue: [ aCanvas drawExpandedAt: x@(extent y//2) height: self indentWidth]
			ifFalse: [ aCanvas drawNotExpandedAt: x@(extent y//2) height: self indentWidth ]].
	x := self indentWidth * 3 // 2 + x.

	icon isNil ifFalse: [
		centeringOffset := ((extent y - icon height) / 2.0) roundedHAFZ.
		 aCanvas 
			image: icon
			at:  (x @ centeringOffset).
		x := self indentWidth // 2 + icon width + x. ].

	colorToUse := complexContents preferredColor ifNil: [ color ].
	aCanvas
		drawString: contents asString
		at: x@0
		font: self fontToUse
		color: colorToUse! !

!IndentingListItemMorph methodsFor: 'geometry' stamp: 'jmv 3/14/2023 16:25:58'!
desiredWidth

	^(indentLevel + 2) * self indentWidth + (self font widthOfStringOrText: self contents)! !

!IndentingListItemMorph methodsFor: 'private' stamp: 'jmv 3/14/2023 17:15:28'!
toggleRectangle

	^(indentLevel * self indentWidth @ 0) extent: self indentWidth@extent y! !


!BitBltCanvas class methodsFor: 'cached button forms' stamp: 'jmv 3/15/2023 10:12:03'!
arrowOfDirection: aSymbol size: finalSizeInteger
	^self cachedForms
		at: { aSymbol . finalSizeInteger asInteger}
		ifAbsentPut: [
			self buildArrowOfDirection: aSymbol size: finalSizeInteger ]! !

!BitBltCanvas class methodsFor: 'cached button forms' stamp: 'jmv 3/15/2023 10:11:54'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	 (self buildArrowOfDirection: #up size: 120) display
	"

	| resizeFactor resizedForm f e c topLeftMargin bottomRightMargin |
	resizeFactor := 4.
	e := finalSizeInteger@finalSizeInteger*resizeFactor.
	f := Form extent: e depth: 32.
	c := `Color gray: 0.5`.
	topLeftMargin := 0.
	bottomRightMargin := finalSizeInteger*resizeFactor//4.
	topLeftMargin := finalSizeInteger*resizeFactor//4.
	bottomRightMargin := 0.
	(#(up down) includes: aSymbolDirection) 
		ifTrue: [
			topLeftMargin to: e y-1-bottomRightMargin do: [ :y |
				0 to: e x -1 do: [ :x |
					(e x / 2 - 1 - x) abs * 2 + bottomRightMargin< (e y -1- y) ifTrue: [
						f colorAt: x@y put: c ]]]]
		ifFalse: [
			topLeftMargin to: e x-1-bottomRightMargin do: [ :x |
				0 to: e y -1 do: [ :y |
					(e y / 2 - 1 - y) abs * 2 + bottomRightMargin < (e x -1-x) ifTrue: [
						f colorAt: x@y put: c ]]]].
	resizedForm := f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.
	aSymbolDirection = #up ifTrue: [
		resizedForm := resizedForm flippedBy: #vertical ].
	aSymbolDirection = #left ifTrue: [
		resizedForm := resizedForm flippedBy: #horizontal ].
	^resizedForm! !

!methodRemoval: BitBltCanvas #drawExpandedAt: stamp: 'jmv 3/15/2023 08:13:59'!
BitBltCanvas removeSelector: #drawExpandedAt:!
!methodRemoval: BitBltCanvas #drawNotExpandedAt: stamp: 'jmv 3/15/2023 08:14:00'!
BitBltCanvas removeSelector: #drawNotExpandedAt:!
!methodRemoval: AbstractVectorCanvas #drawExpandedAt: stamp: 'jmv 3/15/2023 08:14:00'!
AbstractVectorCanvas removeSelector: #drawExpandedAt:!
!methodRemoval: AbstractVectorCanvas #drawNotExpandedAt: stamp: 'jmv 3/15/2023 08:14:01'!
AbstractVectorCanvas removeSelector: #drawNotExpandedAt:!
!methodRemoval: MorphicCanvas #drawExpandedAt: stamp: 'jmv 3/15/2023 08:14:01'!
MorphicCanvas removeSelector: #drawExpandedAt:!
!methodRemoval: MorphicCanvas #drawNotExpandedAt: stamp: 'jmv 3/15/2023 08:14:02'!
MorphicCanvas removeSelector: #drawNotExpandedAt:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
BitBltCanvas releaseClassState!

