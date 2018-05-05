'From Cuis 5.0 of 7 November 2016 [latest update: #3300] on 1 May 2018 at 10:29:54 am'!

!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 4/30/2018 21:22:56'!
setBlankIcon
	"change the the receiver's icon"
	icon := Theme current blankIcon! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 4/30/2018 21:26:57'!
target: anObject selector: aSymbol arguments: aCollection

	target _ anObject.
	selector _ aSymbol.
	arguments _ aCollection! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 5/1/2018 10:27:02'!
addBlankIconsIfNecessary
	"If any of my items have an icon, ensure that all do by using anIcon for those that don't"

	| withIcons withoutIcons |
	withIcons _ Set new.
	withoutIcons _ Set new.
	self items do: [ :item |
		item hasIcon | item hasMarker
			ifTrue: [ withIcons add: item ]
			ifFalse: [ withoutIcons add: item ].
		item hasSubMenu ifTrue: [ item subMenu addBlankIconsIfNecessary ]].
	(withIcons isEmpty or: [ withoutIcons isEmpty ]) ifTrue: [ ^self ].
	withoutIcons do: [ :item | item setBlankIcon ]! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:15:20'!
add: aString target: target selector: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  Answer the appended menu item."

	| item |
	item _ MenuItemMorph new
		contents: aString;
		target: target selector: aSymbol arguments: argList asArray.
	self addMorphBack: item.
	^ item! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 5/1/2018 10:15:48'!
addUpdating: wordingSelector target: target selector: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  In this variant, the wording of the menu item is obtained by sending the wordingSelector to the target,  Answer the item added."

	| item |
	item _ UpdatingMenuItemMorph new
		target: target selector: aSymbol arguments: argList asArray;
		wordingProvider: target wordingSelector: wordingSelector.
	self addMorphBack: item.
	^ item! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 5/1/2018 10:23:05'!
contentsWithMarkers: aString inverse: inverse 
	"Set the menu item entry. Parse aString for embedded markers."

	| markerIndex marker |
	contentString _ nil.	"get rid of old"
	self removeAllMorphs.	"get rid of old markers if updating"
	icon _ nil.
	(aString notEmpty and: [aString first = $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	contentString _ aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph new image: marker.
	self addMorphFront: marker position: `0@2`! !

!MenuItemMorph methodsFor: 'drawing' stamp: 'jmv 5/1/2018 10:25:29'!
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
		| iconForm | 
		iconForm _ isEnabled ifTrue: [ icon ] ifFalse: [ icon asGrayScaleAndTransparent ].
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

!MenuItemMorph methodsFor: 'layout' stamp: 'jmv 5/1/2018 10:25:03'!
minItemWidth
	| fontToUse iconWidth subMenuWidth markerWidth |
	fontToUse _ self fontToUse.
	subMenuWidth _ self hasSubMenu
				ifFalse: [0]
				ifTrue: [10].
	iconWidth _ icon
				ifNotNil: [icon width + self iconSeparation]
				ifNil: [0].
	markerWidth _ self hasMarker
		ifTrue: [ submorphs first morphWidth + 8 ]
		ifFalse: [ 0 ].
	^ (fontToUse widthOfString: contents)
		+ subMenuWidth + iconWidth + markerWidth.! !

!methodRemoval: MenuItemMorph #arguments!
MenuItemMorph removeSelector: #arguments!
!methodRemoval: MenuItemMorph #arguments:!
MenuItemMorph removeSelector: #arguments:!
!methodRemoval: MenuItemMorph #icon!
MenuItemMorph removeSelector: #icon!
!methodRemoval: MenuItemMorph #selector!
MenuItemMorph removeSelector: #selector!
!methodRemoval: MenuItemMorph #selector:!
MenuItemMorph removeSelector: #selector:!
!methodRemoval: MenuItemMorph #target!
MenuItemMorph removeSelector: #target!
!methodRemoval: MenuItemMorph #target:!
MenuItemMorph removeSelector: #target:!
!methodRemoval: MenuMorph #target:!
MenuMorph removeSelector: #target:!
