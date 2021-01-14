'From Cuis 5.0 [latest update: #4520] on 13 January 2021 at 12:03:37 pm'!
!classDefinition: #HighlightingMenuItemMorph category: #'Morphic-Menus'!
MenuItemMorph subclass: #HighlightingMenuItemMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!HighlightingMenuItemMorph commentStamp: '<historical>' prior: 0!
Highlights the target when hovered.!


!Morph methodsFor: 'accessing - flags' stamp: 'jmv 1/13/2021 11:03:38'!
highlighted: aBoolean

	self privateFlagAt: 5 put: aBoolean.
	self redrawNeeded! !

!Morph methodsFor: 'accessing - flags' stamp: 'jmv 1/13/2021 09:54:34'!
isHighlighted

	^ self privateFlagAt: 5! !


!HighlightingMenuItemMorph methodsFor: 'selecting' stamp: 'jmv 1/13/2021 10:41:50'!
isSelected: aBoolean

	super isSelected: aBoolean.
	target highlighted: aBoolean.! !


!MenuMorph methodsFor: 'accessing' stamp: 'jmv 1/13/2021 10:57:10'!
itemsDo: aBlock

	submorphs do: [ :m |
		(m is: #MenuItemMorph) ifTrue: [
			aBlock value: m ]].! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 1/13/2021 10:44:39'!
add: aString targetHighlight: target action: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.
	Highlight target when hovering over item.
	Answer the appended menu item."

	| item |
	item _ HighlightingMenuItemMorph new
		contents: aString;
		target: target selector: aSymbol arguments: argList asArray.
	self addMorphBack: item.
	^ item! !


!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 1/13/2021 11:57:47'!
drawCurrentMorphHighlight

	currentMorph displayBounds ifNotNil: [ :r |
		engine
			sourceForm: nil;
			colorMap: nil;
			combinationRule: Form blend;
			fillColor: `Color black`;
			frameRect: r borderWidth: 4;
			fillColor: `Color pink alpha: 0.2`;
			fillRect: (r insetBy: 4).
	]! !


!Morph methodsFor: 'meta-actions' stamp: 'jmv 1/13/2021 10:43:33'!
addEmbeddingMenuItemsTo: aMenu hand: aHandMorph
	| menu |
	menu _ MenuMorph new defaultTarget: self.
	self potentialEmbeddingTargets reverseDo: [:m | 
		menu 
			add: m class name asString 
			targetHighlight: m 
			action: #addMorphFrontFromWorldPosition: 
			argumentList: {self}].
	aMenu ifNotNil:[
		menu submorphCount > 0 
			ifTrue:[aMenu add:'embed into' subMenu: menu].
	].
	^menu! !


!MenuMorph methodsFor: 'initialization' stamp: 'jmv 1/13/2021 10:58:22'!
delete
	activeSubMenu ifNotNil: [activeSubMenu delete].
	self itemsDo: [ :i | i deselect ].
	^super delete! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 1/13/2021 11:12:00'!
drawCurrentAndSubmorphs
	| b |
	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self.
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph halo ifNotNil: [ b _ currentMorph morphLocalBounds ].
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ].
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].
		currentMorph postDrawOn: self.
		b ifNotNil: [
			self drawCoordinateSystem: b ].
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds:
				(currentMorph displayBounds quickMerge: self boundingRectOfCurrentMorphAfterDraw) ].

		currentMorph isHighlighted ifTrue: [
			self drawCurrentMorphHighlight ].
	].! !

