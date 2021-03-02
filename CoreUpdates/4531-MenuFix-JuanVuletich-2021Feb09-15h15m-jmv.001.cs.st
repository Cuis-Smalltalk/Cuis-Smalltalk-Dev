'From Cuis 5.0 [latest update: #4530] on 9 February 2021 at 3:15:43 pm'!

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/30/2021 15:59:33'!
activateSubmenu: event
	"Activate our submenu; e.g., pass control to it"

	subMenu ifNil: [ ^false ]. "not applicable"
	subMenu isInWorld ifFalse: [ ^false ].	
	(subMenu containsGlobalPoint: event eventPosition) ifFalse: [^false].
	subMenu activate: event.
	^true! !

