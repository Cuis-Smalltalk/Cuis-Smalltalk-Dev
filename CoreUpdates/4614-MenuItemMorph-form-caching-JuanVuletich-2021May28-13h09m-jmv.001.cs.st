'From Cuis 5.0 [latest update: #4613] on 28 May 2021 at 1:10:02 pm'!
!classDefinition: #MenuItemMorph category: #'Morphic-Menus'!
LabelMorph subclass: #MenuItemMorph
	instanceVariableNames: 'isEnabled subMenu isSelected target selector arguments icon contentString '
	classVariableNames: 'SubMenuMarker OnImage OffImage '
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 5/28/2021 10:21:25'!
offImage
	"Return the form to be used for indicating an '<off>' marker"
	| e |
	e _ (self fontToUse ascent-2) rounded asPoint.
	(OffImage isNil or: [ OffImage extent ~= e ]) ifTrue: [
		OffImage _ Form extent: e depth: 32.
		OffImage getCanvas
			frameAndFillRectangle: OffImage boundingBox fillColor: `(Color gray: 0.9)`
				borderWidth: 1 borderColor: `Color black` ].
	^OffImage! !

!MenuItemMorph methodsFor: 'private' stamp: 'jmv 5/28/2021 10:21:32'!
onImage
	"Return the form to be used for indicating an '<off>' marker"
	| e |
	e _ (self fontToUse ascent-2) rounded asPoint.
	(OnImage isNil or: [ OnImage extent ~= e ]) ifTrue: [
		OnImage _ Form extent: e depth: 32.
		OnImage getCanvas
			frameAndFillRectangle: OnImage boundingBox fillColor: `Color gray: 0.8`
				borderWidth: 1 borderColor: `Color black`;
			fillRectangle: (OnImage boundingBox insetBy: 2) color: `Color black` ].
	^OnImage! !

!classDefinition: #MenuItemMorph category: #'Morphic-Menus'!
LabelMorph subclass: #MenuItemMorph
	instanceVariableNames: 'isEnabled subMenu isSelected target selector arguments icon contentString'
	classVariableNames: 'OffImage OnImage SubMenuMarker'
	poolDictionaries: ''
	category: 'Morphic-Menus'!
