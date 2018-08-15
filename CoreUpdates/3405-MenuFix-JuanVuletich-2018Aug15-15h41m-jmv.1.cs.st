'From Cuis 5.0 of 7 November 2016 [latest update: #3404] on 15 August 2018 at 3:42:44 pm'!

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 8/15/2018 15:42:06'!
activateSubmenu: evt
	"Activate our submenu; e.g., pass control to it"
	subMenu ifNil: [ ^false ]. "not applicable"
	(subMenu morphContainsPoint: (subMenu internalizeFromWorld: evt eventPosition)) ifFalse:[^false].
	subMenu activate: evt.
	^true! !

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 8/15/2018 15:41:44'!
deselect

	self isSelected: false.
	subMenu ifNotNil: [
		owner ifNotNil: [ owner activeSubmenu: nil ]]! !

!methodRemoval: MenuItemMorph #deselectTimeOut!
MenuItemMorph removeSelector: #deselectTimeOut!
!methodRemoval: MenuItemMorph #mouseLeave:!
MenuItemMorph removeSelector: #mouseLeave:!
