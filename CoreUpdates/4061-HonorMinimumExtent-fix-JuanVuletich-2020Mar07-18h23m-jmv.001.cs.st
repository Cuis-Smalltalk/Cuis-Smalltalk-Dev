'From Cuis 5.0 [latest update: #4058] on 7 March 2020 at 6:23:55 pm'!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
RectangleLikeMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation padding cachedMinExtent doAdoptWidgetsColor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!Morph methodsFor: 'geometry' stamp: 'jmv 3/7/2020 18:19:05'!
minimumLayoutExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	| minExtent |
	minExtent _ self minimumExtent.
	^ layoutSpec
		ifNil: [ minExtent ]
		ifNotNil: [ minExtent max: layoutSpec minimumLayoutWidth @ layoutSpec minimumLayoutHeight ]! !


!LayoutSpec methodsFor: 'layout' stamp: 'jmv 3/6/2020 18:01:08'!
fixedOrMinimumLayoutHeight
	^fixedHeight ifNil: [0]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 3/6/2020 18:01:13'!
fixedOrMinimumLayoutWidth
	^fixedWidth ifNil: [0]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 3/6/2020 17:59:40'!
minimumLayoutHeight
	"Generally prefer asking the morph itself!!"

	proportionalHeight ifNil: [ ^0 ].
	^fixedHeight ifNil: [0]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 3/6/2020 17:59:48'!
minimumLayoutWidth
	"Generally prefer asking the morph itself!!"

	proportionalWidth ifNil: [ ^0 ].
	^fixedWidth ifNil: [0]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 3/6/2020 18:02:32'!
proportionaLayoutlHeight

	^ proportionalHeight ifNil: [ 0 ]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 3/6/2020 18:03:02'!
proportionalLayoutWidth

	^ proportionalWidth ifNil: [ 0 ]! !


!Morph methodsFor: 'geometry' stamp: 'jmv 3/7/2020 18:19:15'!
minimumExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ `1@1`! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 3/7/2020 18:16:34'!
minimumExtent
	| minW minH |
	"Figure out the minimum extent for this pane so that either content, or at least required scrollbars, will fit"
	minW _ self xtraBorder * 2 + scroller morphWidth min: self scrollBarClass scrollbarThickness * 2.
	self vIsScrollbarShowing
		ifTrue: [
			minW _ minW + self scrollBarClass scrollbarThickness].
	minH _ self xtraBorder * 2 + scroller morphHeight.
	self hIsScrollbarShowing
		ifTrue: [
			minH _ minH + self scrollBarClass scrollbarThickness].
	minH _ minH min: self scrollBarClass scrollbarThickness * 2.
	^ (minW + (borderWidth * 2)) @ (minH + (borderWidth * 2))! !


!BrowserCommentTextMorph methodsFor: 'displaying' stamp: 'jmv 3/6/2020 18:37:46'!
hidePane

	self hide.
	separator visible ifTrue: [
		proportionalHeight _ self layoutSpec proportionaLayoutlHeight.
		separatorHeight _ separator layoutSpec fixedOrMinimumLayoutHeight ].
	separator layoutSpec fixedHeight: 0.
	self layoutSpec proportionalHeight: 0.
	separator ifNotNil: [ separator hide ]! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 3/7/2020 18:16:44'!
minimumExtent

	^layoutMorph minimumExtent + (borderWidth * 2) + (0@self labelHeight)! !


!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 3/7/2020 18:19:33'!
adjustHorizontallyBy: aLayoutAdjustMorph at: aPoint
	| delta l ls r rs lNewWidth rNewWidth i lCurrentWidth rCurrentWidth |
	i _ submorphs indexOf: aLayoutAdjustMorph.
	l _ self submorphs at: i +1.
	ls _ l layoutSpec.
	lCurrentWidth _ l morphWidth max: 1.	"avoid division by zero"
	r _ self submorphs at: i - 1.
	rs _ r layoutSpec.
	rCurrentWidth _ r morphWidth max: 1.	"avoid division by zero"
	delta _ aPoint x - aLayoutAdjustMorph referencePosition x.
	delta _ delta max: l minimumLayoutExtent x - lCurrentWidth.
	delta _ delta min: rCurrentWidth - r minimumLayoutExtent x.
	delta = 0 ifTrue: [ ^self ].
	rNewWidth _ rCurrentWidth - delta.
	lNewWidth _ lCurrentWidth + delta.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [ | leftNewProportion rightNewProportion toDistribute |	"If both proportional, update them"
			leftNewProportion _ lNewWidth / (lNewWidth + rNewWidth).
			rightNewProportion _ 1.0 - leftNewProportion.
			toDistribute _ ls proportionalLayoutWidth + rs proportionalLayoutWidth.
			ls setProportionalWidth: leftNewProportion * toDistribute.
			rs setProportionalWidth: rightNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 3/7/2020 18:19:39'!
adjustVerticallyBy: aLayoutAdjustMorph at: aPoint
	| delta t ts b bs tNewHeight bNewHeight i tCurrentHeight bCurrentHeight |
	i _ submorphs indexOf: aLayoutAdjustMorph.
	t _ self submorphs at: i +1.
	ts _ t layoutSpec.
	tCurrentHeight _ t morphHeight max: 1.	"avoid division by zero"
	b _ self submorphs at: i - 1.
	bs _ b layoutSpec.
	bCurrentHeight _ b morphHeight max: 1.	"avoid division by zero"
	delta _ aPoint y - aLayoutAdjustMorph referencePosition y.
	delta _ delta max: t minimumLayoutExtent y - tCurrentHeight.
	delta _ delta min: bCurrentHeight - b minimumLayoutExtent y.
	delta = 0 ifTrue: [ ^self ].
	tNewHeight _ tCurrentHeight + delta.
	bNewHeight _ bCurrentHeight - delta.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [ | bottomNewProportion toDistribute topNewProportion |	"If both proportional, update them"
			topNewProportion _ tNewHeight / (tNewHeight + bNewHeight).
			bottomNewProportion _ 1.0 - topNewProportion.
			toDistribute _ ts proportionaLayoutlHeight + bs proportionaLayoutlHeight.
			ts setProportionalHeight: topNewProportion * toDistribute.
			bs setProportionalHeight: bottomNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	self layoutSubmorphs! !

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 3/7/2020 18:20:11'!
minimumExtent
	"Answer size sufficient to frame my submorphs."
	
	| width height |
	width := self ySeparation.
	height := self xSeparation.
	(self direction =  #vertical)
		ifTrue: [
			self submorphsDo: [ :sm | | smMinExtent |
				smMinExtent := sm minimumLayoutExtent.
				"use maximum width across submorphs"
				width := width max: (smMinExtent x max: sm layoutSpec fixedOrMinimumLayoutWidth).
				"sum up submorph heights"
				height := height + (smMinExtent y max: sm layoutSpec fixedOrMinimumLayoutHeight) + self ySeparation.
			].
		     width := width + self xSeparation.
		]
		ifFalse: [
			self submorphsDo: [ :sm | | smMinExtent |
				smMinExtent := sm minimumLayoutExtent.
				"sum up submorphs width"
				width := width + (smMinExtent x max: sm layoutSpec fixedOrMinimumLayoutWidth) + self xSeparation.
				"use maximum height across submorph"
				height := height max: (smMinExtent y max: sm layoutSpec fixedOrMinimumLayoutHeight).
			].
			height := height + self xSeparation.
		].

	^ (width @ height) + self extentBorder! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 3/7/2020 18:19:43'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixedOrMinimum normalizationFactor availableForPropWidth 
		fractionalWidths integerWidths theLeft usableHeight boundsTop boundsRight theTop minWidth submorphsToLayout 
			nextMorph ht wd ls theRight boundsBottom theBottom alternativeWidths count diff i |
	
	boundsForLayout extent > `2@2` "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableWidth := boundsForLayout width - ((submorphsToLayout size + 1) * xSep).
	sumOfFixedOrMinimum := submorphsToLayout sum: [ :m | m minimumLayoutExtent x max: m layoutSpec fixedOrMinimumLayoutWidth ].
	availableForPropWidth := usableWidth - sumOfFixedOrMinimum max: 0.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	availableForPropWidth := availableForPropWidth * normalizationFactor.
	
	fractionalWidths := submorphsToLayout collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	"Compute integer widths, mostly rounding but with occasional #floor or #ceiling as needed to keep sum"
	integerWidths _ fractionalWidths collect: [ :w | w rounded ].
	diff _ integerWidths sum - fractionalWidths sum rounded.
	alternativeWidths _ diff > 0 ifTrue: [ fractionalWidths collect: [ :w | w floor ]] ifFalse: [ fractionalWidths collect: [ :w | w ceiling ]].
	count _ diff abs.
	i _ 1.
	[ count > 0] whileTrue: [
		(integerWidths at: i) = (alternativeWidths at: i) ifFalse: [
			integerWidths at: i put: (alternativeWidths at: i).
			count _ count -1 ].
		i _ i + 1 ].
	minWidth := integerWidths sum.
	theLeft := ((usableWidth - minWidth) * (padding ifNil: [0]) max: 0) + boundsForLayout left + xSep.
	usableHeight := boundsForLayout height - (ySep * 2) max: 0.
	boundsTop    := boundsForLayout top + ySep.
	boundsRight  := boundsForLayout right - xSep.
	boundsBottom := boundsForLayout bottom - ySep.

	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		wd := integerWidths at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		ht := (ls heightFor: usableHeight) min: usableHeight.
		theTop := ((usableHeight - ht) * ls minorDirectionPadding) floor + boundsTop.
		theBottom := (theTop + ht) ceiling min: boundsBottom.
		theRight := (theLeft + (wd min: minWidth)) "ceiling "min: boundsRight.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		ls usesMorphExtent
			ifTrue: [
				nextMorph morphPosition: theLeft floor @ theTop floor ]
			ifFalse: [
				nextMorph morphPosition: theLeft floor @ theTop floor extent: theRight - theLeft @ (theBottom - theTop) ].
		theLeft := theRight + xSep
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 3/7/2020 18:19:47'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixedOrMinimum normalizationFactor availableForPropHeight 
		fractionalHeights integerHeights theTop usableWidth boundsLeft boundsBottom theLeft minHeight submorphsToLayout 
			nextMorph ht wd ls theBottom boundsRight theRight alternativeHeights count diff i |

	boundsForLayout extent > `2@2` "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableHeight := boundsForLayout height - ((submorphsToLayout size + 1) * ySep).
	sumOfFixedOrMinimum := submorphsToLayout sum: [ :m | m minimumLayoutExtent y max: m layoutSpec fixedOrMinimumLayoutHeight ].
	availableForPropHeight := usableHeight - sumOfFixedOrMinimum max: 0.
	normalizationFactor := self proportionalHeightNormalizationFactor.
	availableForPropHeight := availableForPropHeight * normalizationFactor.
	
	fractionalHeights := submorphsToLayout collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	"Compute integer widths, mostly rounding but with occasional #floor or #ceiling as needed to keep sum"
	integerHeights _ fractionalHeights collect: [ :w | w rounded ].
	diff _ integerHeights sum - fractionalHeights sum rounded.
	alternativeHeights _ diff > 0 ifTrue: [ fractionalHeights collect: [ :w | w floor ]] ifFalse: [ fractionalHeights collect: [ :w | w ceiling ]].
	count _ diff abs.
	i _ 1.
	[ count > 0] whileTrue: [
		(integerHeights at: i) = (alternativeHeights at: i) ifFalse: [
			integerHeights at: i put: (alternativeHeights at: i).
			count _ count -1 ].
		i _ i + 1 ].
	minHeight := integerHeights sum.
	theTop := ((usableHeight - minHeight) * (padding ifNil: [0]) max: 0) + boundsForLayout top + ySep.
	usableWidth     := boundsForLayout width - (xSep * 2) max: 0.
	boundsLeft      := boundsForLayout left + xSep.
	boundsBottom := boundsForLayout bottom - ySep.
	boundsRight := boundsForLayout right - xSep.
	
	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		ht := integerHeights at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		wd := (ls widthFor: usableWidth) min: usableWidth.
		theLeft := ((usableWidth - wd) * ls minorDirectionPadding) floor + boundsLeft.
		theRight := (theLeft + wd) ceiling min: boundsRight.
		theBottom := (theTop + (ht min: minHeight)) "ceiling" min: boundsBottom.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		ls usesMorphExtent
			ifTrue: [
				nextMorph morphPosition: theLeft floor @ theTop floor ]
			ifFalse: [
				nextMorph morphPosition: theLeft floor @ theTop floor extent: theRight - theLeft @ (theBottom - theTop) ].
		theTop := theBottom + ySep
	]! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 3/6/2020 18:37:57'!
proportionalHeightNormalizationFactor

	| sumOfProportional |
	sumOfProportional _ self submorphsToLayout sum: [ :m | m layoutSpec proportionaLayoutlHeight ].
	^1.0 / (sumOfProportional max: 1.0).! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 3/6/2020 18:38:18'!
proportionalWidthNormalizationFactor

	| sumOfProportional |
	sumOfProportional _ self submorphsToLayout sum: [ :m | m layoutSpec proportionalLayoutWidth ].
	^1.0 / (sumOfProportional max: 1.0).! !


!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 3/6/2020 16:06:23'!
example1
"
	self example1
"
| pane row |
pane _ (LayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row _ LayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #A)
		layoutSpec: (LayoutSpec fixedWidth: 10); 
	addMorph: (BorderedRectMorph new color: (Color blue); name: #B)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #C)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSpec proportionalWidth: 0.15);
	addMorph: (BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
		layoutSpec: (LayoutSpec fixedWidth: 200 fixedHeight: 200).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color blue); name: #F)
		layoutSpec: (LayoutSpec proportionalWidth: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #G)
		layoutSpec: (LayoutSpec proportionalWidth: 0.4);
	addMorph: (BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #H)
		layoutSpec: (LayoutSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSpec useAll.

row _ (LayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 60).
pane morphPosition: 150@130 extent: 400@300.
pane openInWorld! !


!LayoutSpec methodsFor: 'layout' stamp: 'jmv 3/7/2020 18:20:17'!
heightFor: availableSpace
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined.
	Otherwise, we do proportional layout, and the stored extent is a minimum extent.
	If there is no minimum extent, it should be set to zero."

	^proportionalHeight isNil
		ifTrue: [fixedHeight ifNil: [morph morphHeight]]
		ifFalse: [proportionalHeight * availableSpace + morph minimumLayoutExtent y]! !

!LayoutSpec methodsFor: 'layout' stamp: 'jmv 3/7/2020 18:20:21'!
widthFor: availableSpace
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined.
	Otherwise, we do proportional layout, and the stored extent is a minimum extent.
	If there is no minimum extent, it should be set to zero."

	^proportionalWidth isNil
		ifTrue: [fixedWidth ifNil: [morph morphWidth]]
		ifFalse: [proportionalWidth * availableSpace + morph minimumLayoutExtent x]! !

!methodRemoval: LayoutSpec #fixedWidth stamp: 'jmv 3/7/2020 18:23:32'!
LayoutSpec removeSelector: #fixedWidth!
!methodRemoval: LayoutSpec #proportionalHeight stamp: 'jmv 3/7/2020 18:23:32'!
LayoutSpec removeSelector: #proportionalHeight!
!methodRemoval: LayoutSpec #fixedHeight stamp: 'jmv 3/7/2020 18:23:32'!
LayoutSpec removeSelector: #fixedHeight!
!methodRemoval: LayoutSpec #proportionalWidth stamp: 'jmv 3/7/2020 18:23:32'!
LayoutSpec removeSelector: #proportionalWidth!
!methodRemoval: LayoutMorph #fontPreferenceChanged stamp: 'jmv 3/7/2020 18:23:32'!
LayoutMorph removeSelector: #fontPreferenceChanged!
!methodRemoval: LayoutMorph #calculateMinimumExtent stamp: 'jmv 3/7/2020 18:23:32'!
LayoutMorph removeSelector: #calculateMinimumExtent!
!methodRemoval: LayoutMorph #minPaneHeightForReframe stamp: 'jmv 3/7/2020 18:23:32'!
LayoutMorph removeSelector: #minPaneHeightForReframe!
!methodRemoval: LayoutMorph #minPaneWidthForReframe stamp: 'jmv 3/7/2020 18:23:32'!
LayoutMorph removeSelector: #minPaneWidthForReframe!
!methodRemoval: SystemWindow #minPaneWidthForReframe stamp: 'jmv 3/7/2020 18:23:32'!
SystemWindow removeSelector: #minPaneWidthForReframe!
!methodRemoval: SystemWindow #minPaneHeightForReframe stamp: 'jmv 3/7/2020 18:23:32'!
SystemWindow removeSelector: #minPaneHeightForReframe!
!methodRemoval: Morph #minPaneWidthForReframe stamp: 'jmv 3/7/2020 18:23:32'!
Morph removeSelector: #minPaneWidthForReframe!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
RectangleLikeMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation padding doAdoptWidgetsColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
