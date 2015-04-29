'From Cuis 4.2 of 25 July 2013 [latest update: #2257] on 19 April 2015 at 9:16:14.840912 am'!

!MenuItemMorph class methodsFor: 'cached state access' stamp: 'jmv 4/19/2015 09:15'!
releaseClassCachedState
	SubMenuMarker _ nil! !

!MenuItemMorph class methodsFor: 'cached state access' stamp: 'jmv 4/19/2015 09:15'!
subMenuMarker

	| f |
	SubMenuMarker ifNotNil: [ ^SubMenuMarker ].
	f _ Form
		extent: 5@9
		fromArray: #(2147483648 3221225472 3758096384 4026531840 4160749568 4026531840 3758096384 3221225472 2147483648)
		offset: 0@0.
	SubMenuMarker _ ColorForm mappingWhiteToTransparentFrom: f.
	^SubMenuMarker! !


!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 4/19/2015 09:14'!
drawOn: aCanvas 
	| stringColor leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: (0@0 extent: extent) color: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	self hasIcon
		ifTrue: [| iconForm | 
			iconForm _ isEnabled ifTrue: [ self icon ] ifFalse: [ self icon asGrayScale ].
			aCanvas image: iconForm at: leftEdge+1 @ (extent y - iconForm height // 2).
			leftEdge _ leftEdge + iconForm width + self iconSeparation].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: self class subMenuMarker
			at: extent x - 8 @ (extent y - self class subMenuMarker height // 2) ]! !

!methodRemoval: MenuItemMorph class #initialize!
MenuItemMorph class removeSelector: #initialize!
MenuItemMorph initialize!

!MenuItemMorph class reorganize!
('cached state access' releaseClassCachedState subMenuMarker)
!

