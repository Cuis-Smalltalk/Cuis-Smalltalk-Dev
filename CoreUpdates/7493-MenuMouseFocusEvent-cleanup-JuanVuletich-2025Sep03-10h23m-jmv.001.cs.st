'From Cuis7.5 [latest update: #7492] on 3 September 2025 at 10:31:37 am'!

!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 10:30:54'!
handleMouseFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."

	"#dispatchIn: and not #sendEventTo: so it is handled by the appropriate menu item."
	aMorphicEvent dispatchIn: self.
	
	"Handle clicks outside us, so we are closed. This will only be done if the event wasn't handled above,
	meaning that it is a click outside of us."
	aMorphicEvent isMouseButton ifTrue: [
		aMorphicEvent sendEventTo: self.
		^self ].
	
	"On MouseMove maybe activate submenu, or owner."
	aMorphicEvent isMove ifTrue: [
		"On #mouseMove we might transfer control to *either* the currently active submenu
		or the owner menu, if any.
		Since the active sub menu is always displayed upfront check it first."
		selectedItem ifNotNil: [
			(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [
				^self]].
		"Note: The following does not traverse upwards but it's the best I can do for now"
		popUpOwner ifNotNil: [
			(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue: [
				^self]]].! !

