'From Cuis7.5 [latest update: #7486] on 2 September 2025 at 4:53:50 pm'!

!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 16:52:00'!
handleKeyboardFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."

	aMorphicEvent dispatchIn: self.

	"Need to handle keyboard input if we have the focus."
	^ aMorphicEvent sendEventTo: self.! !

!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/2/2025 16:53:08'!
handleMouseFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."

	aMorphicEvent dispatchIn: self.

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	aMorphicEvent isMouseOver ifTrue: [
		^self ].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [ 
		aMorphicEvent sendEventTo: self.
		^self ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil: [(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [
		^self]].
	"Note: The following does not traverse upwards but it's the best I can do for now"
	popUpOwner ifNotNil: [(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue: [
		^self]].! !

