'From Cuis7.5 [latest update: #7515] on 4 September 2025 at 1:49:00 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:31:26'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue:[
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:31:43'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:37:23'!
processMouseOver: aMouseOverEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
	 self hasMouseFocus ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		((self rejectsEvent: aMouseOverEvent) not and: [self fullIncludesPixel: aMouseOverEvent eventPosition]) ifFalse: [
			^self ]].
	aMouseOverEvent hand noticeMouseOver: self event: aMouseOverEvent.
	"Open question: should any unhandled mouse move events be filtered out? (i.e. should mouseHover:localPosition: be called when a mouse button is pressed but the morph doesn't have mouse button handlers?  Essentially, what are the limits of what is considered 'hovering'?"
	self handlesMouseHover ifTrue: [
		self
			mouseHover: aMouseOverEvent
			localPosition: localEventPosition ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:33:22'!
processMouseScroll: aMouseScrollEvent localPosition: localEventPosition

	(self handlesMouseScroll: aMouseScrollEvent) ifTrue: [
		self
			mouseScroll: aMouseScrollEvent
			localPosition: localEventPosition.
		aMouseScrollEvent wasHandled: true ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:32:22'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].
	
	self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self stopStepping: #processMouseStillDown! !


!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:48:51'!
handleMouseFocusEvent: aMouseEvent
	"Handle the given event.
		This message is sent if the receiver currently has the mouse focus
	and is therefore receiving events directly from some hand,
	without going through #dispatchIn:.
		Valid menu transitions are determined based on the menu currently holding the focus 
	after the mouse went down on one of its children."

	"#dispatchIn: and not #sendEventTo: so it is handled by the appropriate menu item."
	aMouseEvent dispatchIn: self.
	
	"If it is a mouse event and it wasn't handled, still deliver it to us: it may have been a click outside us,
	and then, we need to be closed. See MenuMorph >> #mouseButton1Down:localPosition:"
	aMouseEvent isMouseButton ifTrue: [
		aMouseEvent wasHandled ifFalse: [
			aMouseEvent sendEventTo: self.
			^self ]].
	
	"On MouseMove maybe activate submenu, or owner."
	aMouseEvent isMouseMove ifTrue: [
		"On #mouseMove we might transfer control to *either* the currently active submenu
		or the owner menu, if any.
		Since the active sub menu is always displayed upfront check it first."
		selectedItem ifNotNil: [
			(selectedItem activateSubmenu: aMouseEvent) ifTrue: [
				^self]].
		"Note: The following does not traverse upwards but it's the best I can do for now"
		popUpOwner ifNotNil: [
			(popUpOwner activateOwnerMenu: aMouseEvent) ifTrue: [
				^self]]].! !


!HierarchicalListMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:31:52'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !


!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:31:57'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	scroller highlightedRow: (
		(self viewableArea containsPoint: localEventPosition) ifTrue: [
			self rowAtLocation: localEventPosition ifNone: nil]).
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !


!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 13:32:02'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Re-implemented to allow for mouse-up move events"

	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1]! !

