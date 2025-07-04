'From Cuis7.5 [latest update: #7313] on 3 July 2025 at 3:29:45 pm'!

!ColumnLayout methodsFor: 'adjust' stamp: 'jmv 7/3/2025 15:23:49'!
adjustMorphBefore: t morphAfter: b adjusterRefPos: adjusterRefPos at: localPoint
	| tCurrentHeight bCurrentHeight delta tNewHeight bNewHeight ts bs |

	tCurrentHeight := t extentInOwner y max: 1.	"avoid division by zero"
	bCurrentHeight := b extentInOwner y max: 1.	"avoid division by zero"
	delta := localPoint y - adjusterRefPos y.
	delta := delta max: t minimumShrinkHeight - tCurrentHeight.
	delta := delta min: bCurrentHeight - b minimumShrinkHeight.
	delta = 0 ifTrue: [ ^false ].
	tNewHeight := tCurrentHeight + delta.
	bNewHeight := bCurrentHeight - delta.
	ts := t layoutSpec.
	bs := b layoutSpec.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [ | bottomNewProportion toDistribute topNewProportion |	"If both proportional, update them"
			topNewProportion := tNewHeight / (tNewHeight + bNewHeight).
			bottomNewProportion := 1.0 - topNewProportion.
			toDistribute := ts proportionalLayoutHeight + bs proportionalLayoutHeight.
			ts setProportionalHeight: topNewProportion * toDistribute.
			bs setProportionalHeight: bottomNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	^true! !


!RowLayout methodsFor: 'adjust' stamp: 'jmv 7/3/2025 15:25:01'!
adjustMorphBefore: l morphAfter: r adjusterRefPos: adjusterRefPos at: localPoint
	| lCurrentWidth rCurrentWidth delta lNewWidth rNewWidth ls rs |

	lCurrentWidth := l extentInOwner x max: 1.	"avoid division by zero"
	rCurrentWidth := r extentInOwner x max: 1.	"avoid division by zero"
	delta := localPoint x - adjusterRefPos x.
	delta := delta max: l minimumShrinkWidth - lCurrentWidth.
	delta := delta min: rCurrentWidth - r minimumShrinkWidth.
	delta = 0 ifTrue: [ ^false ].
	rNewWidth := rCurrentWidth - delta.
	lNewWidth := lCurrentWidth + delta.
	ls := l layoutSpec.
	rs := r layoutSpec.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [ | leftNewProportion rightNewProportion toDistribute |	"If both proportional, update them"
			leftNewProportion := lNewWidth / (lNewWidth + rNewWidth).
			rightNewProportion := 1.0 - leftNewProportion.
			toDistribute := ls proportionalLayoutWidth + rs proportionalLayoutWidth.
			ls setProportionalWidth: leftNewProportion * toDistribute.
			rs setProportionalWidth: rightNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	^true! !


!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 7/3/2025 15:28:46'!
adjustBy: aLayoutAdjustMorph at: aGlobalPoint
	"See Class Comment of LayoutAdjustingMorph"

	| localPoint  i before after adjusterRefPos |
	localPoint := self internalizeFromWorld: aGlobalPoint.
	i := submorphs indexOf: aLayoutAdjustMorph.
	((i = 1) or: [i = self submorphs size]) ifTrue: [
		self inform: 'LayoutAdjustingMorphs require morphs before and after'.
		^self ].
	before := self submorphs at: i +1.
	after := self submorphs at: i - 1.
	adjusterRefPos := aLayoutAdjustMorph referencePositionInOwner.
	(layout adjustMorphBefore: before morphAfter: after adjusterRefPos: adjusterRefPos at: localPoint)
		ifTrue: [
			self layoutSubmorphs ].! !

!methodRemoval: LayoutMorph #adjustHorizontallyBy:at: stamp: 'jmv 7/3/2025 15:28:54'!
LayoutMorph removeSelector: #adjustHorizontallyBy:at:!
!methodRemoval: LayoutMorph #adjustHorizontallyMorphAtLeft:morphAtRight:adjusterRefPos:at: stamp: 'jmv 7/3/2025 15:25:37'!
LayoutMorph removeSelector: #adjustHorizontallyMorphAtLeft:morphAtRight:adjusterRefPos:at:!
!methodRemoval: LayoutMorph #adjustVerticallyBy:at: stamp: 'jmv 7/3/2025 15:28:52'!
LayoutMorph removeSelector: #adjustVerticallyBy:at:!
!methodRemoval: LayoutMorph #adjustVerticallyMorphAbove:morphBelow:adjusterRefPos:at: stamp: 'jmv 7/3/2025 15:25:52'!
LayoutMorph removeSelector: #adjustVerticallyMorphAbove:morphBelow:adjusterRefPos:at:!

!ColumnLayout reorganize!
('accessing' axisEdgeWeight:)
('initialization' initialize)
('layout' heightsFor:within:minLayoutExtentCache: layoutMorphs:in: offAxisWidthFor:within:minLayoutExtentCache: offAxisXOffsetFor:within:)
('adjust' adjustMorphBefore:morphAfter:adjusterRefPos:at:)
('testing' isColumn)
!


!RowLayout reorganize!
('accessing' axisEdgeWeight:)
('initialization' initialize)
('layout' layoutMorphs:in: offAxisHeightFor:within:minLayoutExtentCache: offAxisYOffsetFor:within: widthsFor:within:minLayoutExtentCache:)
('adjust' adjustMorphBefore:morphAfter:adjusterRefPos:at:)
('testing' isRow)
!

