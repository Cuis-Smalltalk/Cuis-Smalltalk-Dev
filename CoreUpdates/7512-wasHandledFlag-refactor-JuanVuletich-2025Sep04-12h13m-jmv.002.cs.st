'From Cuis7.5 [latest update: #7507] on 4 September 2025 at 1:03:30 pm'!

!WindowEvent methodsFor: 'dispatching' stamp: 'jmv 9/4/2025 12:18:57'!
dispatchIn: aWorldMorph
	"Host window events do not have a position and are only dispatched to the World"

	self wasHandled not ifTrue: [
		self sendEventTo: aWorldMorph ].! !


!Morph methodsFor: 'events' stamp: 'jmv 9/4/2025 12:31:43'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event.
	Subclasses should call super, and check if the event was handled."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [
			aKeyboardEvent wasHandled: true.
			^self ].

	aKeyboardEvent anyModifierKeyPressed ifTrue: [
		(self keyboardShortcut: aKeyboardEvent)
			ifTrue: [ aKeyboardEvent wasHandled: true ]].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 12:27:44'!
processDropFiles: aDropFilesEvent
	"Handle a dropping file."
	
	aDropFilesEvent wasHandled: true.
	self dropFiles: aDropFilesEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 12:27:30'!
processDropMorph: aDropEvent
	"Handle a dropping morph."

	| aMorph |
	aMorph := aDropEvent contents.
	aDropEvent wasHandled: true.
	aMorph justDroppedInto: self event: aDropEvent.
	self acceptDroppingMorph: aMorph event: aDropEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 12:28:59'!
processKeyDown: aKeyboardEvent
	"System level event handling."

	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	self keyDown: aKeyboardEvent.! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 12:29:23'!
processKeyUp: aKeyboardEvent
	"System level event handling."

	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	self keyUp: aKeyboardEvent! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 12:28:37'!
processKeystroke: aKeyboardEvent
	"System level event handling."

	self handlesKeyboard ifTrue: [
		self keyStroke: aKeyboardEvent.
		aKeyboardEvent wasHandled: true.
		self 
			valueOfProperty: #'keyStroke:'
			ifPresentDo: [ :handler | handler value: aKeyboardEvent ]].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 12:39:42'!
processWindowEvent: aWindowEvent
	"Handle an event concerning our host window"

	(self wantsWindowEvent: aWindowEvent) ifFalse: [^self].
	aWindowEvent wasHandled: true.
	self windowEvent: aWindowEvent.
! !


!FindInSourceDialog methodsFor: 'events' stamp: 'jmv 9/4/2025 12:50:47'!
keyStroke: aKeyboardEvent
	"Avoid the need to give focus to subwidgets"

	(aKeyboardEvent isArrowUp or: [  aKeyboardEvent isArrowDown ]) ifTrue: [
		scopeList keyStroke: aKeyboardEvent.
		aKeyboardEvent wasHandled: true.
		^self ].
	aKeyboardEvent isReturnKey ifTrue: [
		self ok.
		aKeyboardEvent wasHandled: true.
		^self ].
	aKeyboardEvent isEsc ifTrue: [
		self cancel.
		aKeyboardEvent wasHandled: true.
		^self ].
	searchField keyStroke: aKeyboardEvent.! !


!MenuMorph methodsFor: 'events' stamp: 'jmv 9/4/2025 12:53:35'!
keyStroke: aKeyboardEvent
	| matchString selectable |
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: [
				selectedItem hasSubMenu 
					ifTrue: [
						aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
						aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
					ifFalse: [
						selectedItem invokeWithEvent: aKeyboardEvent].
				aKeyboardEvent wasHandled: true.
				^self ].
			(selectable := self items) size = 1 
				ifTrue: [
					selectable first invokeWithEvent: aKeyboardEvent ].
			"Consume the event in any case"
			aKeyboardEvent wasHandled: true.
			^self].
	aKeyboardEvent isEsc
		ifTrue: [
			"escape key"
			self
				valueOfProperty: #matchString
				ifPresentDo: [ :str | 
					str isEmpty 
						ifFalse: [
							"If filtered, first ESC removes filter"
							self setProperty: #matchString toValue: String new.
							self selectItem: nil.
							self displayFiltered: aKeyboardEvent.
							aKeyboardEvent wasHandled: true.
							^self]].
			"If a stand-alone menu, just delete it"
			self delete.
			aKeyboardEvent hand
				newKeyboardFocus: prevKbdFocus;
				newMouseFocus: prevMouseFocus.
			aKeyboardEvent wasHandled: true.
			^self].
	aKeyboardEvent isArrowLeft | aKeyboardEvent isArrowRight
		ifTrue: [
			"left or right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: [
					aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu ].
			"Consume the event in any case"
			aKeyboardEvent wasHandled: true.
			^self].
	aKeyboardEvent isArrowUp ifTrue: [aKeyboardEvent wasHandled: true.^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	aKeyboardEvent isArrowDown ifTrue: [aKeyboardEvent wasHandled: true.^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	aKeyboardEvent isPageUp ifTrue: [aKeyboardEvent wasHandled: true.^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	aKeyboardEvent isPageDown ifTrue: [aKeyboardEvent wasHandled: true.^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsent: [String new].
	matchString := aKeyboardEvent isBackspace
				ifTrue: [matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent.
	aKeyboardEvent wasHandled: true.! !

!MenuMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 12:22:21'!
handleMouseFocusEvent: aMouseEvent
	"Handle the given event.
		This message is sent if the receiver currently has the mouse focus
	and is therefore receiving events directly from some hand,
	without going through #dispatchIn:.
		Valid menu transitions are determined based on the menu currently holding the focus 
	after the mouse went down on one of its children."

	"#dispatchIn: and not #sendEventTo: so it is handled by the appropriate menu item."
	aMouseEvent dispatchIn: self.
	
	"Handle clicks outside us, so we are closed. This will only be done if the event wasn't handled above,
	meaning that it is a click outside of us."
	aMouseEvent isMouseButton ifTrue: [
		aMouseEvent sendEventTo: self.
		^self ].
	
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


!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 9/4/2025 12:58:32'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."
	
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [
		contents := self editor string.
		self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
		self redrawNeeded.
		^self ].
	
	self pauseBlinking.

	"All the cases below consume the event."
	aKeyboardEvent wasHandled: true.

	"Return - check for special action"
	aKeyboardEvent isReturnKey ifTrue: [
		self crAction ifNotNil: [ :action |
			action value.
			^self ]].

	"Esc - check for special action"
	aKeyboardEvent isEsc ifTrue: [
		self escAction ifNotNil: [ :action |
			action value.
			^self  ]].

	self editor processKeystrokeEvent: aKeyboardEvent.
	contents := self editor string.
	self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
	self redrawNeeded.! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 9/4/2025 12:57:14'!
keyStroke: aKeyboardEvent 
	"Process potential command keys"

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	(self arrowKey: aKeyboardEvent) ifNotNil: [
		aKeyboardEvent wasHandled: true.
		^self ].

	aKeyboardEvent isEsc ifTrue: [
		" escape key"
		self mouseButton2Activity.
		aKeyboardEvent wasHandled: true ].! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/4/2025 12:58:51'!
keyStroke: aKeyboardEvent 
	"Process keys"

	| aCharacter |
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].
	
	(self arrowKey: aKeyboardEvent) ifNotNil: [
		aKeyboardEvent wasHandled: true.
		^self ].

	"All the cases below consume the event."
	aKeyboardEvent wasHandled: true.
	aKeyboardEvent isReturnKey ifTrue: [
		doubleClickSelector notNil ifTrue: [(self window ifNil: [model]) perform: doubleClickSelector].
		^self].
	aKeyboardEvent isEsc ifTrue: [
		self mouseButton2Activity.
		^self ].
	aKeyboardEvent isDelete ifTrue: [
		self deleteAction.
		^self ].
	aKeyboardEvent isBackspace ifTrue: [
		self deleteAction.
		^self ].
	
	aCharacter := aKeyboardEvent keyCharacter.
	self keyboardSearch: aCharacter! !

!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 9/4/2025 13:02:18'!
keyboardSearch: aChar
	| oldSelection nextSelection max milliSeconds nextSelectionList nextSelectionText |
	nextSelection := oldSelection := self getCurrentSelectionIndex.
	max := self maximumSelection.
	milliSeconds := Time localMillisecondClock.
	milliSeconds - lastKeystrokeTime > 300 ifTrue: ["just use the one current character for selecting"
		lastKeystrokes := ''].
	lastKeystrokes := lastKeystrokes copyWith: aChar asLowercase.
	lastKeystrokeTime := milliSeconds.
	nextSelectionList := OrderedCollection newFrom: (list copyFrom: oldSelection + 1 to: max).
	nextSelectionList addAll: (list copyFrom: 1 to: oldSelection).
	"Get rid of blanks and style used in some lists"
	nextSelectionText := nextSelectionList detect: [:a | a asString withBlanksTrimmed asLowercase beginsWith: lastKeystrokes]
				ifNone: [^ self flash"match not found"].
	self owningWindow ifNotNil: [ :w |
		(w okToChangeDueTo: self) ifFalse: [^ self]].
	nextSelection := list findFirst: [:a | a == nextSelectionText].
	"No change if model is locked"
	oldSelection == nextSelection ifTrue: [^ self flash].
	self setSelectionIndex: nextSelection! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/4/2025 12:21:11'!
startKeyboardDispatch: aKeyboardEvent

	| focusedElement |
	
	focusedElement := self keyboardFocus ifNil: [ self world ].
	aKeyboardEvent wasHandled not ifTrue: [
		aKeyboardEvent sendEventTo: focusedElement ].
	self mouseOverHandler processMouseOver: lastMouseEvent! !

