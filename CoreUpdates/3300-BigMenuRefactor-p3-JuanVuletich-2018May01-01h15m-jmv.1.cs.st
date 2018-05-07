'From Cuis 5.0 of 7 November 2016 [latest update: #3296] on 1 May 2018 at 1:26:50 am'!

!MenuMorph methodsFor: 'accessing' stamp: 'jmv 5/1/2018 01:22:28'!
addBlankIconsIfNecessary
	"If any of my items have an icon, ensure that all do by using anIcon for those that don't"

	| withIcons withoutIcons blankIcon |
	withIcons _ Set new.
	withoutIcons _ Set new.
	self items do: [ :item |
		item hasIcon | item hasMarker
			ifTrue: [ withIcons add: item ]
			ifFalse: [ withoutIcons add: item ].
		item hasSubMenu ifTrue: [ item subMenu addBlankIconsIfNecessary ]].
	(withIcons isEmpty or: [ withoutIcons isEmpty ]) ifTrue: [ ^self ].
	blankIcon _ Theme current blankIcon.
	withoutIcons do: [ :item | item set_icon: blankIcon ].! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 5/1/2018 01:20:01'!
contentsWithMarkers: aString inverse: inverse 
	"Set the menu item entry. If aBool is true, parse aString for embedded markers."

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

!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 4/30/2018 20:54:05'!
set_icon: aForm 
	"change the the receiver's icon"
	icon := aForm! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/30/2018 21:04:03'!
add: aString subMenu: aMenuMorph
	"Append the given submenu with the given label."

	| item |
	item _ MenuItemMorph new.
	item
		contents: aString;
		subMenu: aMenuMorph.
	self addMorphBack: item! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/30/2018 21:04:08'!
add: aString target: target selector: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  Answer the appended menu item."

	| item |
	item _ MenuItemMorph new
		contents: aString;
		target: target;
		selector: aSymbol;
		arguments: argList asArray.
	self addMorphBack: item.
	^ item! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 4/30/2018 20:51:33'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
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

!MenuMorph methodsFor: 'control' stamp: 'jmv 4/30/2018 20:51:37'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	Theme current decorateMenu: self.
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !


!MenuItemMorph methodsFor: 'accessing' stamp: 'jmv 5/1/2018 01:19:11'!
contents: aString
	^self contentsWithMarkers: aString inverse: false! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 5/1/2018 01:19:31'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	owner hasMouseFocus ifFalse: [ ^self ].
	"This will happen if the menu has toggles in it. (for instance, the 'show...' button)
	Update the look, refresh the world and wait a bit,
	to give the user some visual feedback"
	contentString ifNotNil: [
		self contentsWithMarkers: contentString inverse: true.
		self refreshWorld.
		(Delay forMilliseconds: 200) wait].
	self deselect.
	self invokeWithEvent: aMouseButtonEvent! !


!UpdatingMenuItemMorph methodsFor: 'world' stamp: 'jmv 5/1/2018 01:24:02'!
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
				self contents: newString ]].
	enablementSelector ifNotNil: [
		(enablement _ self enablement) == isEnabled 
			ifFalse:	[self isEnabled: enablement]]! !


!Theme methodsFor: 'accessing' stamp: 'jmv 4/30/2018 20:56:48'!
decorateMenu: aMenu

	"decorate aMenu with icons"

	self flag: #todo. "Do I really belong on Theme, and not some menu class somewhere?"

	menuItemIcons ifEmpty: [
		^ self ].

	aMenu items do: [ :item | 
		| decoration |
		decoration _ menuItemIcons 
			at: item contents asString asLowercase 
			ifAbsent: nil.

		decoration ifNotNil: [
			item set_icon: decoration ]]! !

!methodRemoval: MenuItemMorph #contents:withMarkers:!
MenuItemMorph removeSelector: #contents:withMarkers:!
!methodRemoval: MenuItemMorph #contents:withMarkers:inverse:!
MenuItemMorph removeSelector: #contents:withMarkers:inverse:!
!methodRemoval: MenuItemMorph #icon:!
MenuItemMorph removeSelector: #icon:!
!methodRemoval: MenuMorph #addBlankIconsIfNecessary:!
MenuMorph removeSelector: #addBlankIconsIfNecessary:!
!methodRemoval: MenuMorph #addWithLabel:enablement:action:!
MenuMorph removeSelector: #addWithLabel:enablement:action:!
!methodRemoval: MenuMorph #addWithLabel:enablementSelector:target:selector:argumentList:!
MenuMorph removeSelector: #addWithLabel:enablementSelector:target:selector:argumentList:!
!methodRemoval: MenuMorph #defaultTarget!
MenuMorph removeSelector: #defaultTarget!
