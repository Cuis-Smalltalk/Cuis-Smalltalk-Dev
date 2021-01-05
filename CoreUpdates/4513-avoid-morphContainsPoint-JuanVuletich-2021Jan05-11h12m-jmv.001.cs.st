'From Cuis 5.0 [latest update: #4512] on 5 January 2021 at 11:26:28 am'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 1/5/2021 11:20:58'!
containsGlobalPoint: worldPoint

	"If not visible, won't contain any point at all."
	| canvas |
	self visible ifFalse: [ ^false ].

	canvas _ self world canvas.
	canvas isNil ifTrue: [ ^false ].
	(canvas morph: self isAtPoint: worldPoint) ifTrue: [ ^ true ]. 
	^ false! !


!Morph methodsFor: 'geometry testing' stamp: 'jmv 1/5/2021 11:21:34'!
fullContainsGlobalPoint: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape."

	"If not visible, won't contain any point at all."
	| canvas |
	self visible ifFalse: [ ^false ].

	canvas _ self world canvas.
	canvas isNil ifTrue: [ ^false ].
	(canvas morph: self isAtPoint: worldPoint) ifTrue: [ ^ true ]. 
	self submorphsDo: [ :m |
		(m fullContainsGlobalPoint: worldPoint) ifTrue: [ ^ true ]].
	^ false! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 1/5/2021 11:22:11'!
doRecolor: event with: aHandle
	"The mouse went down in the 'recolor' halo handle.  Allow the user to change the color of the innerTarget"

	event hand obtainHalo: self.
	(aHandle containsGlobalPoint: event eventPosition)
		ifFalse: [  "only do it if mouse still in handle on mouse up"
			self delete.
			target addHalo: event]
		ifTrue: [
			target changeColor]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 1/5/2021 11:22:14'!
maybeCollapse: event with: aHandle 
	"Ask hand to collapse my target if mouse comes up in it."

	event hand obtainHalo: self.
	self delete.
	(aHandle containsGlobalPoint: event eventPosition)
		ifFalse: [
			target addHalo: event ]
		ifTrue: [
			target collapse ]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 1/5/2021 11:22:19'!
maybeDismiss: event with: aHandle
	"Ask hand to dismiss my target if mouse comes up in it."

	event hand obtainHalo: self.
	(aHandle containsGlobalPoint: event eventPosition)
		ifFalse: [
			self delete.
			target addHalo: event]
		ifTrue: [
			target resistsRemoval ifTrue: [
				(PopUpMenu
					confirm: 'Really throw this away'
					trueChoice: 'Yes'
					falseChoice: 'Um, no, let me reconsider') ifFalse: [^ self]].

			self delete.
			target dismissViaHalo]! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 1/5/2021 11:22:25'!
setDismissColor: event with: aHandle
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	event hand obtainHalo: self.
	colorToUse _  (aHandle containsGlobalPoint: event eventPosition)
		ifFalse: [ `Color red muchLighter` ]
		ifTrue: [ `Color lightGray` ].
	aHandle color: colorToUse! !


!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 1/5/2021 11:24:34'!
stepAt: millisecondSinceLast
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [ Cursor defaultCursor activateCursor ].
		^ self stopStepping ].
	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p.
			(Preferences cheapWindowReframe or: [ millisecondSinceLast > 200]) ifTrue: [
				owner displayBounds newRectFrom: [ :f |
					self adjustOwnerAt: Sensor mousePoint.
					owner morphPosition extent: owner morphExtent ]]]
		ifFalse: [
			self stopStepping.
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self containsGlobalPoint: p) ifFalse: [
				hand _ nil.
				Cursor defaultCursor activateCursor ]].! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/5/2021 11:24:48'!
activateOwnerMenu: evt
	"Activate our owner menu; e.g., pass control to it"
	owner ifNil: [ ^false ]. "not applicable"
	(owner containsGlobalPoint: evt eventPosition)
		ifFalse: [ ^false ].
	owner activate: evt.
	^true! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 1/5/2021 11:25:19'!
activateSubmenu: event
	"Activate our submenu; e.g., pass control to it"

	subMenu ifNil: [ ^false ]. "not applicable"
	(subMenu containsGlobalPoint: event eventPosition) ifFalse: [^false].
	subMenu activate: event.
	^true! !


!PluggableButtonMorph methodsFor: 'events' stamp: 'jmv 1/5/2021 11:22:49'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [ 
		self containsGlobalPoint: aMouseButtonEvent eventPosition ])
		ifTrue: [ self performAction ].
	self redrawNeeded! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 1/5/2021 11:24:01'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	(self containsGlobalPoint: aMouseButtonEvent eventPosition)
		ifTrue: [
			((self upButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goUp ].
			((self downButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goDown ].
			self selected: (localEventPosition y // self itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !


!KeyboardEvent methodsFor: 'actions' stamp: 'jmv 1/5/2021 11:23:35'!
closeCurrentWindowOf: aMorph

	aMorph owningWindow ifNotNil: [ :w |
		(w containsGlobalPoint: self eventPosition)
			ifTrue: [ w delete ] ].! !

