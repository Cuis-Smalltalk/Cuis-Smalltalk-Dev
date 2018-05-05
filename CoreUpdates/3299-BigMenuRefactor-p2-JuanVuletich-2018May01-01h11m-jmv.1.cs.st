'From Cuis 5.0 of 7 November 2016 [latest update: #3296] on 1 May 2018 at 1:15:46 am'!
!classDefinition: #MenuItemMorph category: #'Morphic-Menus'!
StringMorph subclass: #MenuItemMorph
	instanceVariableNames: 'isEnabled subMenu isSelected target selector arguments icon contentString '
	classVariableNames: 'SubMenuMarker '
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 5/1/2018 01:15:02'!
contents: aString withMarkers: aBool inverse: inverse 
	"Set the menu item entry. If aBool is true, parse aString for embedded markers."

	| markerIndex marker |
	contentString _ nil.	"get rid of old"
	aBool ifFalse: [^super contents: aString].
	self removeAllMorphs.	"get rid of old markers if updating"
	self hasIcon ifTrue: [ self icon: nil ].
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

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 5/1/2018 01:14:28'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	owner hasMouseFocus ifFalse: [ ^self ].
	"This will happen if the menu has toggles in it. (for instance, the 'show...' button)
	Update the look, refresh the world and wait a bit,
	to give the user some visual feedback"
	contentString ifNotNil: [
		self contents: contentString withMarkers: true inverse: true.
		self refreshWorld.
		(Delay forMilliseconds: 200) wait].
	self deselect.
	self invokeWithEvent: aMouseButtonEvent! !


!UpdatingMenuItemMorph methodsFor: 'world' stamp: 'jmv 5/1/2018 01:14:34'!
updateContents
	"Update the receiver's contents"

	| newString enablement nArgs |
	((wordingProvider isNil) or: [wordingSelector isNil]) ifFalse: [
		nArgs _ wordingSelector numArgs.
		newString _ nArgs = 0
			ifTrue: [
				wordingProvider perform: wordingSelector]
			ifFalse: [
				(nArgs = 1 and: [wordingArgument notNil])
					ifTrue: [
						wordingProvider perform: wordingSelector with: wordingArgument]
					ifFalse: [
						nArgs = arguments size ifTrue: [
							wordingProvider perform: wordingSelector withArguments: arguments]]].
		newString = (contentString ifNil: [ contents ])
			ifFalse: [
				self contents: newString.
				Theme current decorateMenu: owner ]].
	enablementSelector ifNotNil: [
		(enablement _ self enablement) == isEnabled 
			ifFalse:	[self isEnabled: enablement]]! !

!methodRemoval: MenuItemMorph #contentString!
MenuItemMorph removeSelector: #contentString!
!methodRemoval: MenuItemMorph #contentString:!
MenuItemMorph removeSelector: #contentString:!
!classDefinition: #MenuItemMorph category: #'Morphic-Menus'!
StringMorph subclass: #MenuItemMorph
	instanceVariableNames: 'isEnabled subMenu isSelected target selector arguments icon contentString'
	classVariableNames: 'SubMenuMarker'
	poolDictionaries: ''
	category: 'Morphic-Menus'!
