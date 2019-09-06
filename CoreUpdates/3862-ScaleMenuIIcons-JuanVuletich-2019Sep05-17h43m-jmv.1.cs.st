'From Cuis 5.0 [latest update: #3861] on 5 September 2019 at 5:43:43 pm'!

!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 5/1/2018 14:17:12'!
setIcon: symbolOrFormOrNil
	"Argument can be a Form, a Symbol (to be sent to Theme current) or nil."

	icon _ symbolOrFormOrNil isSymbol
		ifTrue: [Theme current perform: symbolOrFormOrNil]
		ifFalse: [ symbolOrFormOrNil ]! !

!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 9/5/2019 17:31:30'!
drawOn: aCanvas 
	| stringColor leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: (`0@0` extent: extent) color: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	icon ifNotNil: [
		| iconForm w h factor magnifiedExtent magnifiedIcon |
		iconForm _ isEnabled ifTrue: [ icon ] ifFalse: [ icon asGrayScaleAndTransparent ].
		magnifiedIcon _ iconForm.
		w _ iconForm width.
		h _ iconForm height.
		w*h = 0 ifFalse: [
			factor _ extent y * 0.8 / h.
			factor = 1.0 ifFalse: [
				magnifiedExtent _ (iconForm extent * factor) rounded.
				magnifiedIcon _ iconForm magnifyTo: magnifiedExtent ]].
		aCanvas image: magnifiedIcon at: leftEdge+1 @ (magnifiedIcon height *1//10).
		leftEdge _ magnifiedIcon width *12//10 + leftEdge].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: self class subMenuMarker
			at: extent x - 8 @ (extent y - self class subMenuMarker height // 2) ]! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jmv 9/5/2019 17:28:38'!
minItemWidth
	| fontToUse iconWidth subMenuWidth markerWidth |
	fontToUse _ self fontToUse.
	subMenuWidth _ self hasSubMenu
				ifFalse: [0]
				ifTrue: [10].
	iconWidth _ icon
				ifNotNil: [icon width * 12//10]
				ifNil: [0].
	markerWidth _ self hasMarker
		ifTrue: [ submorphs first morphWidth + 8 ]
		ifFalse: [ 0 ].
	^ (fontToUse widthOfString: contents)
		+ subMenuWidth + iconWidth + markerWidth.! !

