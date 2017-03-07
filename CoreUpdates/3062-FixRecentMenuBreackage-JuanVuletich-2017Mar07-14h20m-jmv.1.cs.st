'From Cuis 5.0 of 7 November 2016 [latest update: #3061] on 7 March 2017 at 2:55:27 pm'!
!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedRectMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp titleMorph activeSubMenu popUpOwner '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!MenuMorph methodsFor: 'control' stamp: 'ar 9/17/2000 20:38'!
activeSubmenu: aSubmenu
	activeSubMenu ifNotNil:[activeSubMenu delete].
	activeSubMenu _ aSubmenu.! !

!MenuMorph methodsFor: 'initialization' stamp: 'ar 10/10/2000 01:57'!
delete
	activeSubMenu ifNotNil:[activeSubMenu delete].
	^super delete! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 9/20/2012 00:14'!
activateOwnerMenu: evt
	"Activate our owner menu; e.g., pass control to it"
	owner ifNil: [ ^false ]. "not applicable"
	(owner morphContainsPoint: (owner internalizeFromWorld: evt eventPosition))
		ifFalse: [ ^false ].
	owner activate: evt.
	^true! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 3/7/2017 14:37:43'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	stayUp ifFalse: [ self delete ].
	popUpOwner ifNotNil: [
		popUpOwner isSelected: false.
		popUpOwner deleteIfPopUp: evt ].
	evt ifNotNil: [ evt hand ifNotNil: [ :h | h releaseMouseFocus: self ]]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 5/9/2016 20:40'!
popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	sourceItem world addMorphFront: self.

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

!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 8/20/2012 17:50'!
handleFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."
	| eventPositionInOurCoordinates |
	eventPositionInOurCoordinates _ self internalizeFromWorld: aMorphicEvent eventPosition.

	self dispatchEvent: aMorphicEvent localPosition: eventPositionInOurCoordinates.

	"Need to handle keyboard input if we have the focus."
	aMorphicEvent isKeyboard ifTrue: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(aMorphicEvent isMouseOver or: [aMorphicEvent isMouse not]) ifTrue: [ ^self ].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [^self]].
	"Note: The following does not traverse upwards but it's the best I can do for now"
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue: [^self]].! !

!MenuMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2012 20:15'!
initialize
	super initialize.
	extent _ 40@10.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil! !


!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 1/29/2014 23:46'!
deselect

	self isSelected: false.
	subMenu ifNotNil: [
		owner ifNotNil:[ owner activeSubmenu: nil ].
		self removeAlarm: #deselectTimeOut ]! !

!MenuItemMorph methodsFor: 'selecting' stamp: 'jmv 1/29/2014 23:44'!
select
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: (Array with: self morphBoundsInWorld topRight + (10@0)
									with: self morphBoundsInWorld topLeft)
			from: self.
		subMenu selectItem: nil ]! !

!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedRectMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp titleMorph activeSubMenu popUpOwner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
