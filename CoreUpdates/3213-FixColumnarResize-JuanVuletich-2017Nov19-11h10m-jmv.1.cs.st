'From Cuis 5.0 of 7 November 2016 [latest update: #3212] on 19 November 2017 at 11:38:44 am'!

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 11/19/2017 11:33:31'!
initialExtent

	^`540@400` * Preferences standardCodeFont height // 14! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 11/19/2017 11:34:12'!
initialExtent
	^`540@300` * Preferences standardCodeFont height // 14! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 11/19/2017 11:36:45'!
initialExtent
	^ `640 @ 320` * Preferences standardCodeFont height // 14! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 11/19/2017 11:36:58'!
initialExtent

	^`600@325` * Preferences standardCodeFont height // 14! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 11/19/2017 11:37:06'!
initialExtent

	^`300@500` * Preferences standardCodeFont height // 14! !


!LayoutMorph methodsFor: 'adjust' stamp: 'jmv 11/19/2017 11:25:32'!
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
	delta _ delta max: l minPaneWidthForReframe - lCurrentWidth.
	delta _ delta min: rCurrentWidth - r minPaneWidthForReframe.
	delta = 0 ifTrue: [ ^self ].
	rNewWidth _ rCurrentWidth - delta.
	lNewWidth _ lCurrentWidth + delta.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [	"If both proportional, update them"
			ls setProportionalWidth: (1.0 * lNewWidth / lCurrentWidth * ls proportionalWidth).
			rs setProportionalWidth: (1.0 * rNewWidth / rCurrentWidth * rs proportionalWidth) ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 11/19/2017 11:27:34'!
minPaneWidthForReframe

	^(self submorphs collect: [ :m | m minimumExtent x ]) max! !

