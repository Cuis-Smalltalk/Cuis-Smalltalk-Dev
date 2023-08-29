'From Cuis 6.0 [latest update: #5984] on 28 August 2023 at 4:47:45 pm'!

!MenuItemMorph methodsFor: 'layout' stamp: 'jmv 8/27/2023 20:39:39'!
minItemWidth
	| fontToUse iconWidth subMenuWidth markerWidth |
	fontToUse := self fontToUse.
	subMenuWidth := self hasSubMenu
				ifFalse: [0]
				ifTrue: [self class subMenuMarker width].
	iconWidth := icon
				ifNotNil: [self magnifiedIcon width * 12//10]
				ifNil: [0].
	markerWidth := self hasMarker
		ifTrue: [ submorphs first morphWidth + 15 ]
		ifFalse: [ 0 ].
	^ (fontToUse widthOfString: contents)
		+ subMenuWidth + iconWidth + markerWidth.! !

