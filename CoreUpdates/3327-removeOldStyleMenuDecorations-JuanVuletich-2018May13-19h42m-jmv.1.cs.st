'From Cuis 5.0 of 7 November 2016 [latest update: #3325] on 13 May 2018 at 7:42:58 pm'!
!classDefinition: #Theme category: #'Theme-Core'!
Object subclass: #Theme
	instanceVariableNames: 'menuItemIcons '
	classVariableNames: 'Content CurrentTheme '
	poolDictionaries: ''
	category: 'Theme-Core'!

!MenuMorph methodsFor: 'control' stamp: 'jmv 5/13/2018 19:12:18'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self runningWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 5/13/2018 19:12:24'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !

!methodRemoval: Theme class #addBasicIconsTo:!
Theme class removeSelector: #addBasicIconsTo:!
!methodRemoval: Theme class #addMiscellaneousIconsTo:!
Theme class removeSelector: #addMiscellaneousIconsTo:!
!methodRemoval: Theme #allIcons!
Theme removeSelector: #allIcons!
!methodRemoval: Theme #basicIcons!
Theme removeSelector: #basicIcons!
!methodRemoval: Theme #decorateMenu:!
Theme removeSelector: #decorateMenu:!
!methodRemoval: Theme #iconDefinersFor:!
Theme removeSelector: #iconDefinersFor:!
!methodRemoval: Theme #iconsDefinitionFor:!
Theme removeSelector: #iconsDefinitionFor:!
!methodRemoval: Theme #initialize!
Theme removeSelector: #initialize!
!methodRemoval: Theme #menuDecorations!
Theme removeSelector: #menuDecorations!
!methodRemoval: Theme #miscellaneousIcons!
Theme removeSelector: #miscellaneousIcons!
!methodRemoval: MenuItemMorph #set_icon:!
MenuItemMorph removeSelector: #set_icon:!
!methodRemoval: MenuMorph #addList:!
MenuMorph removeSelector: #addList:!
!classDefinition: #Theme category: #'Theme-Core'!
Object subclass: #Theme
	instanceVariableNames: ''
	classVariableNames: 'Content CurrentTheme'
	poolDictionaries: ''
	category: 'Theme-Core'!
