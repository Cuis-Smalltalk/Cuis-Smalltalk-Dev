'From Cuis 4.0 of 21 April 2012 [latest update: #1491] on 3 December 2012 at 12:34:31 am'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 12/3/2012 00:33'!
delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld _ self world ifNil: [ self runningWorld ].
	"Terminate genie recognition focus"
	"I encountered a case where the hand was nil, so I put in a little 
	protection - raa "
	" This happens when we are in an MVC project and open
	  a morphic window. - BG "
	aWorld ifNotNil: [
		aWorld activeHand ifNotNil: [ :h | h
			releaseKeyboardFocus: self;
			releaseMouseFocus: self ]].
	owner ifNotNil:[ self privateDelete].! !


!MenuItemMorph methodsFor: 'events-processing' stamp: 'jmv 12/3/2012 00:32'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"The handling of control between menu item requires them to act on mouse up even if not the current focus. This is different from the default behavior which really only wants to handle mouse ups when they got mouse downs before"

	aMouseButtonEvent wasHandled ifTrue:[^self]. "not interested"
	aMouseButtonEvent hand ifNotNil: [ :h | h releaseMouseFocus: self ].
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed
		ifTrue: [ self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition ]
		ifFalse: [ self mouseUp: aMouseButtonEvent localPosition: localEventPosition ]! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 12/3/2012 00:32'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	stayUp ifFalse: [ self delete ].
	popUpOwner ifNotNil: [
		popUpOwner isSelected: false.
		popUpOwner deleteIfPopUp: evt ].
	evt ifNotNil: [ evt hand ifNotNil: [ :h | h releaseMouseFocus: self ]]! !

!MenuMorph methodsFor: 'events' stamp: 'jmv 12/3/2012 00:31'!
mouseUp: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsPoint: localEventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		aMouseButtonEvent hand ifNotNil: [ :h | h releaseMouseFocus: self ].
		^ self deleteIfPopUp: aMouseButtonEvent ].
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ]! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 12/3/2012 00:29'!
displayAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	self runningWorld ifNotNil: [ :w |
		w addMorph: self centeredNear: aPoint.
		self world ifNotNil: [ w displayWorld ].  "show myself"
		].
	aBlock value.
	self delete! !

!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 12/3/2012 00:30'!
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
		titleMorph morphWidth: titleString width + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w ifNotNil: [
			w displayWorld	].	 "show myself"
	]. 
	self delete.
	w ifNotNil: [
		w displayWorld ]! !

