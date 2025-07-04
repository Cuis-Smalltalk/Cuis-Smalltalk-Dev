'From Cuis7.5 [latest update: #7313] on 3 July 2025 at 3:22:20 pm'!

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 7/3/2025 15:21:04'!
adjustHorizontallyMorphAtLeft: l morphAtRight: r adjusterRefPos: adjusterRefPos at: localPoint
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

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 7/3/2025 15:20:58'!
adjustVerticallyMorphAbove: t morphBelow: b adjusterRefPos: adjusterRefPos at: localPoint
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


!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 7/3/2025 15:20:03'!
adjustHorizontallyBy: aLayoutAdjustMorph at: localPoint
	| i l r adjusterRefPos |

	i := submorphs indexOf: aLayoutAdjustMorph.
	((i = 1) or: [i = self submorphs size]) ifTrue: [
		 self inform: 'LayoutAdjustingMorphs require morphs before and after'.
		^self  ].
	l := self submorphs at: i +1.
	r := self submorphs at: i - 1.
	adjusterRefPos := aLayoutAdjustMorph referencePositionInOwner.
	(self adjustHorizontallyMorphAtLeft: l morphAtRight: r adjusterRefPos: adjusterRefPos at: localPoint)
		ifTrue: [
			self layoutSubmorphs ].! !

!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 7/3/2025 15:19:54'!
adjustVerticallyBy: aLayoutAdjustMorph at: localPoint

	| i t b adjusterRefPos |
	i := submorphs indexOf: aLayoutAdjustMorph.
	((i = 1) or: [i = self submorphs size]) ifTrue: [
			 self inform: 'LayoutAdjustingMorphs require morphs above and below'.
			^self ].
	t := self submorphs at: i +1.
	b := self submorphs at: i - 1.
	adjusterRefPos := aLayoutAdjustMorph referencePositionInOwner.
	(self adjustVerticallyMorphAbove: t morphBelow: b adjusterRefPos: adjusterRefPos at: localPoint)
		ifTrue: [
			self layoutSubmorphs ].! !

!methodRemoval: LayoutMorph #adjustHorizontallyBy2:at: stamp: 'jmv 7/3/2025 15:16:28'!
LayoutMorph removeSelector: #adjustHorizontallyBy2:at:!
!methodRemoval: LayoutMorph #adjustVerticallyBy2:at: stamp: 'jmv 7/3/2025 15:16:30'!
LayoutMorph removeSelector: #adjustVerticallyBy2:at:!
