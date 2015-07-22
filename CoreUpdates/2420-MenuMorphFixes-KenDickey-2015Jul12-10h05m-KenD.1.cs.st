'From Cuis 4.2 of 25 July 2013 [latest update: #2419] on 12 July 2015 at 10:50:02.32181 am'!

!MenuMorph methodsFor: 'initialization' stamp: 'KenD 7/12/2015 10:49'!
intoWorld: aWorld
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	super intoWorld: aWorld.
	self adjustSubmorphsLayout! !


!MenuMorph methodsFor: 'control' stamp: 'KenD 7/12/2015 10:48'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	sourceItem world addMorphFront: self.
	self world startSteppingSubmorphsOf: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld
			amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = (0@0) ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPointInWorld first value: false;
		value: rightOrLeftPointInWorld last - (extent x @ 0) value: false;
		value: rightOrLeftPointInWorld first value: true! !

!MenuMorph methodsFor: 'control' stamp: 'KenD 7/12/2015 10:49'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self runningWorld addMorphFront: self position: aPoint - (2 @ 8).
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]! !

!MenuMorph methodsFor: 'control' stamp: 'KenD 7/12/2015 10:49'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	Theme current decorateMenu: self.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self position: aPoint - (2 @ 8).
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	oldKeyboardFocus _ hand keyboardFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]! !


!MenuMorph class methodsFor: 'example' stamp: 'KenD 7/12/2015 10:05'!
example
	"
	MenuMorph example openInWorld
	"

	| menu |
	menu _ MenuMorph new.
	menu addStayUpIcons.
	menu add: 'apples' action: #apples.
	menu add: 'oranges' action: #oranges.
	menu addLine.
	menu addLine.  "extra lines ignored"
	menu add: 'peaches' action: #peaches.
	menu addLine.
	menu add: 'pears' action: #pears.
	menu addLine.
	^ menu
! !

!MenuMorph class methodsFor: 'example' stamp: 'KenD 7/12/2015 10:06'!
example2
	"
	MenuMorph example2 openInWorld
	"

	| menu |
	menu _ MenuMorph entitled: 'sdfasdfa sfda sdf as df as fd'.
	menu addStayUpIcons.
	menu add: 'apples' action: #apples.
	menu add: 'oranges' action: #oranges.
	menu addLine.
	menu addLine.  "extra lines ignored"
	menu add: 'peaches' action: #peaches.
	menu addLine.
	menu add: 'pears' action: #pears.
	menu addLine.
	^ menu
! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 4/17/2013 12:04'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ self world ifNil: [ self runningWorld ].
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString morphWidth + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w ifNotNil: [
			w displayWorld	].	 "show myself"
	]. 
	self delete.
	w ifNotNil: [
		w displayWorld ]! !


!MenuItemMorph methodsFor: 'grabbing' stamp: 'KenD 7/12/2015 10:44'!
aboutToBeGrabbedBy: aHand
	"Don't allow the receiver to act outside a Menu"
	| menu |
	self isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: self.
	menu stayUp.
	^ menu! !

!MenuItemMorph methodsFor: 'grabbing' stamp: 'KenD 7/12/2015 10:45'!
duplicateMorph: evt
	"Don't allow the receiver to act outside a Menu"
	| dup menu |
	dup _ self duplicate isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: dup.
	menu stayUp.
	^ menu! !

!methodRemoval: MenuItemMorph #justGrabbedFrom:!
MenuItemMorph removeSelector: #justGrabbedFrom:!

!MenuMorph reorganize!
('accessing' addBlankIconsIfNecessary: defaultTarget items lastItem popUpOwner stayUp stayUp:)
('construction' add:action: add:action:enabled: add:selector:argument: add:subMenu: add:target:action: add:target:selector: add:target:selector:argument: add:target:selector:argumentList: addLine addList: addServices:for:extraLines: addStayUpIcons addTitle: addUpdating:action: addUpdating:target:action: addUpdating:target:selector:argumentList: addWithLabel:enablement:action: addWithLabel:enablementSelector:target:selector:argumentList: balloonTextForLastItem: defaultTarget: labels:lines:selections: title:)
('control' activeSubmenu: deleteIfPopUp: popUpAdjacentTo:from: popUpAt:forHand:allowKeyboard: popUpAt:forHand:in: popUpAt:forHand:in:allowKeyboard: popUpForHand:in: popUpInWorld popUpInWorld: selectItem: wantsToBeDroppedInto:)
('dropping/grabbing' justDroppedInto:event:)
('events' keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseButton2Up:localPosition:)
('event handling testing' handlesKeyboard handlesMouseDown:)
('events-processing' handleFocusEvent:)
('initialization' defaultBorderWidth defaultColor delete initialize intoWorld:)
('keyboard control' displayFiltered: keyboardFocusChange: moveSelectionDown:event:)
('menu' addCustomMenuItems:hand: addItem addTitle removeStayUpBox sightTarget: target:)
('modal control' invokeModal invokeModal: isModalInvokationDone isModalInvokationDone: modalSelection modalSelection:)
('private' adjustSubmorphsLayout fitInWorld selectedItem)
('drawing' drawOn:)
('misc' activate:)
!

