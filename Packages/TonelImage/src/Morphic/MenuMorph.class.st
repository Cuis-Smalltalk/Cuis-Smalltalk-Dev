"
Instance variables:
	defaultTarget 		<Object>					The default target for creating menu items
	selectedItem			<MenuItemMorph> 		The currently selected item in the receiver
	stayUp 				<Boolean>				True if the receiver should stay up after clicks
"
Class {
	#name : 'MenuMorph',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'defaultTarget',
		'selectedItem',
		'stayUp',
		'titleMorph',
		'activeSubMenu',
		'popUpOwner',
		'prevKeyboardFocus',
		'prevMouseFocus'
	],
	#category : 'Morphic-Menus',
	#package : 'Morphic',
	#tag : 'Menus'
}

{ #category : 'instance creation' }
MenuMorph class >> entitled: aString [
	"Answer a new instance of me with the given title."

	^ self new addTitle: aString

]

{ #category : 'example' }
MenuMorph class >> example [
	"
	MenuMorph example openInWorld
	"

	| menu |
	menu := MenuMorph new.
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

]

{ #category : 'example' }
MenuMorph class >> example2 [
	"
	MenuMorph example2 openInWorld
	"

	| menu |
	menu := MenuMorph entitled: 'sdfasdfa sfda sdf as df as fd'.
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

]

{ #category : 'new-morph participation' }
MenuMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"

	^ false
]

{ #category : 'misc' }
MenuMorph >> activate: evt [
	"Receiver should be activated; e.g., so that control passes correctly."
	evt hand newMouseFocus: self.
]

{ #category : 'accessing' }
MenuMorph >> activeSubMenu [
	^activeSubMenu 
]

{ #category : 'control' }
MenuMorph >> activeSubmenu: aSubmenu [
	activeSubMenu ifNotNil:[activeSubMenu delete].
	activeSubMenu := aSubmenu.
]

{ #category : 'construction' }
MenuMorph >> add: aString action: aSymbol [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #()
]

{ #category : 'construction' }
MenuMorph >> add: aString action: aSymbol argument: arg [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: { arg }
]

{ #category : 'construction' }
MenuMorph >> add: aString action: aSymbol balloonText: stringOrText [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^(self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #())
			setBalloonText: stringOrText
]

{ #category : 'construction' }
MenuMorph >> add: aString action: aSymbol icon: symbolOrFormOrNil [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^(self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #())
			setIcon: symbolOrFormOrNil
]

{ #category : 'construction' }
MenuMorph >> add: aString action: aSymbol icon: symbolOrFormOrNil enabled: aBoolean [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^(self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #())
			setIcon: symbolOrFormOrNil;
			isEnabled: aBoolean
]

{ #category : 'construction' }
MenuMorph >> add: aString subMenu: aMenuMorph [
	"Append the given submenu with the given label."

	| item |
	item := MenuItemMorph new.
	item
		contents: aString;
		subMenu: aMenuMorph.
	self addMorphBack: item.
	^item
]

{ #category : 'construction' }
MenuMorph >> add: aString target: aTarget action: aSymbol [
	^self add: aString
		target: aTarget
		action: aSymbol
		argumentList: #()
]

{ #category : 'construction' }
MenuMorph >> add: aString target: target action: aSymbol argument: arg [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given argument."

	^self add: aString
		target: target
		action: aSymbol
		argumentList: { arg }
]

{ #category : 'construction' }
MenuMorph >> add: aString target: target action: aSymbol argument: arg icon: symbolOrFormOrNil [
	
	^(self add: aString
		target: target
		action: aSymbol
		argumentList: { arg }) 
			setIcon: symbolOrFormOrNil;
			yourself

]

{ #category : 'construction' }
MenuMorph >> add: aString target: target action: aSymbol argumentList: argList [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  Answer the appended menu item."

	| item |
	item := MenuItemMorph new
		contents: aString;
		target: target selector: aSymbol arguments: argList asArray.
	self addMorphBack: item.
	^ item
]

{ #category : 'construction' }
MenuMorph >> add: aString target: aTarget action: aSymbol icon: symbolOrFormOrNil [
	^(self add: aString
		target: aTarget
		action: aSymbol
		argumentList: #())
			setIcon: symbolOrFormOrNil
]

{ #category : 'construction' }
MenuMorph >> add: aString targetHighlight: target action: aSymbol argumentList: argList [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.
	Highlight target when hovering over item.
	Answer the appended menu item."

	| item |
	item := HighlightingMenuItemMorph new
		contents: aString;
		target: target selector: aSymbol arguments: argList asArray.
	self addMorphBack: item.
	^ item
]

{ #category : 'accessing' }
MenuMorph >> addBlankIconsIfNecessary [
	"If any of my items have an icon, ensure that all do by using anIcon for those that don't"

	| withIcons withoutIcons |
	withIcons := Set new.
	withoutIcons := Set new.
	self items do: [ :item |
		item hasIcon | item hasMarker
			ifTrue: [ withIcons add: item ]
			ifFalse: [ withoutIcons add: item ].
		item hasSubMenu ifTrue: [ item subMenu addBlankIconsIfNecessary ]].
"	(withIcons isEmpty or: [ withoutIcons isEmpty ]) ifTrue: [ ^self ]."
	withoutIcons do: [ :item | item setBlankIcon ]
]

{ #category : 'halos and balloon help' }
MenuMorph >> addHalo: evt [
	self stayUp.
	self world ifNotNil: [ :w | w activeHand ifNotNil: [ :h | h releaseMouseFocus: self ]].
	super addHalo: evt.
]

{ #category : 'construction' }
MenuMorph >> addItemFromDictionary: aMenuItemDefinition [
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object. If nil, use defaultTarget. If a Symbol, send it as message to defaultTarget to get real target.
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text
		#icon-	optional icon selector or Form
		#submenuOf - optional label of the parent menu this item should belong to"
		
	| realTarget  item |
	
	realTarget := aMenuItemDefinition at: #object ifAbsent: [defaultTarget].
	realTarget isSymbol ifTrue: [ realTarget := defaultTarget perform: realTarget ].
	
	item := self addItemFromDictionary: aMenuItemDefinition targeting: realTarget.
	
	(Preferences at: #wantsMenuIcons) ifTrue: [ aMenuItemDefinition at: #icon ifPresent: [ :symbolOrFormOrNil | item setIcon: symbolOrFormOrNil ]].
	aMenuItemDefinition at: #balloonText ifPresent: [ :balloonText | item setBalloonText: balloonText ].
	
	^item
]

{ #category : 'construction' }
MenuMorph >> addItemFromDictionary: aMenuItemDefinition targeting: realTarget [

	| label selector argumentList |
	
	label := aMenuItemDefinition at: #label.
	selector := aMenuItemDefinition at: #selector.
	argumentList := aMenuItemDefinition at: #arguments ifAbsent: [ #() ].
	
	^ label isSymbol
		ifTrue: [
			self
				addUpdating: label
				target: realTarget
				action: selector
				argumentList: argumentList ]
		ifFalse: [
			self
				add: label
				target: realTarget
				action: selector
				argumentList: argumentList ]
]

{ #category : 'construction' }
MenuMorph >> addItemFromDictionaryOrNil: aMenuItemDefinitionOrNil  [
	"See addItemFromDictionary: for definition of the expected Dictionary.
	Note: nil elements will add a line."

	^aMenuItemDefinitionOrNil
		ifNil: [ self addLine ]
		ifNotNil: [ self addItemFromDictionary: aMenuItemDefinitionOrNil ].
	
	
]

{ #category : 'construction' }
MenuMorph >> addItemsFromDictionaries: aMenuItemsDefinition [
	
	"see addItemFromDictionary: for expected dictionary definition"
	
	aMenuItemsDefinition do: [ :menuItemDefinition | self addItemFromDictionaryOrNil: menuItemDefinition ]
]

{ #category : 'construction' }
MenuMorph >> addLabel: aString [

	^(self add: aString italic
		target: defaultTarget
		action: nil
		argumentList: #())
			setIcon: nil;
			isEnabled: false;
			color: Color black
]

{ #category : 'construction' }
MenuMorph >> addLine [
	"Append a divider line to this menu. Suppress duplicate lines."

	| lastMorph |
	
	submorphs isEmpty ifTrue: [^ self].
	
	lastMorph := self lastSubmorph.
	lastMorph class == MenuLineMorph
		ifFalse: [
			lastMorph := MenuLineMorph new.
			self addMorphBack: lastMorph ].
		
	^lastMorph 

]

{ #category : 'construction' }
MenuMorph >> addStayUpIcons [
	| closeButton pinButton w buttonHW |
	(Preferences at: #optionalButtons) ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	buttonHW := Theme current titleBarButtonsExtent x.
	closeButton := PluggableButtonMorph model: self action: #delete.
	closeButton iconDrawSelector: #drawCloseIcon; noButtonShape.
	pinButton := PluggableButtonMorph model: self action: #stayUp.
	pinButton iconDrawSelector: #drawPushPinIcon; noButtonShape.
	w := (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + (buttonHW*2).
	self addMorphFront: 
		(LinearLayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW * 12 // 10);
			color: `Color transparent`;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) lock fixedWidth: buttonHW//10;
			addMorph: (closeButton layoutSpec: (LayoutSizeSpec fixedWidth: buttonHW fixedHeight: buttonHW));
			addMorph: (ColoredBoxMorph new color: `Color transparent`) lock fixedWidth: buttonHW//7;
			addMorph: titleMorph lock proportionalWidth: 1;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) lock fixedWidth: buttonHW//7;
			addMorph: pinButton fixedWidth: buttonHW;
			addMorph: (ColoredBoxMorph new color: `Color transparent`) lock fixedWidth: buttonHW//3).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons
]

{ #category : 'construction' }
MenuMorph >> addTitle: aString [
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w newMorph |
	
	newMorph := ColoredBoxMorph new lock.
	newMorph color: Theme current menuTitleBar.
	pp := `8@2`.
	aString asPlainString linesDo: [ :line | | font |
		font := Preferences at: #standardMenuFont.
		s := LabelMorph new
			contents: line;
			font: (titleMorph
				ifNil: [ font bold ]
				ifNotNil: [ font italic ])..
		newMorph addMorphBack: s position: pp.
		pp := pp + (0@(s morphHeight+2)) ].
	w := newMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	newMorph morphExtent: (w + 16) @ (pp y).
	titleMorph
		 ifNil: [
			titleMorph := newMorph.
			self addMorphFront: titleMorph ]
		ifNotNil: [ self addMorphBack: newMorph ].
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]
]

{ #category : 'construction' }
MenuMorph >> addUpdating: aWordingSelector action: aSymbol [

	^self addUpdating: aWordingSelector target: defaultTarget action: aSymbol argumentList: #()
]

{ #category : 'construction' }
MenuMorph >> addUpdating: aWordingSelector target: aTarget action: aSymbol [

	^self addUpdating: aWordingSelector target: aTarget action: aSymbol argumentList: #()
]

{ #category : 'construction' }
MenuMorph >> addUpdating: wordingSelector target: target action: aSymbol argumentList: argList [
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  In this variant, the wording of the menu item is obtained by sending the wordingSelector to the target,  Answer the item added."

	| item |
	item := UpdatingMenuItemMorph new
		target: target selector: aSymbol arguments: argList asArray;
		wordingProvider: target wordingSelector: wordingSelector.
	self addMorphBack: item.
	^ item
]

{ #category : 'private' }
MenuMorph >> adjustSubmorphsLayout [
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p h |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w := submorphs inject: 0 into: [ :prev :each |
		prev max: each minItemWidth].

	w := w + 1.
	p := `5 @ 5`.
	submorphs do: [ :m |
		h := m morphHeight.
		m position: p extent: w@h.
		p := p + (0@(h + 1)) ].

	self morphExtent: w+4 @ p y + 5
]

{ #category : 'initialization' }
MenuMorph >> defaultBorderWidth [
	^ Theme current menuBorderWidth
]

{ #category : 'initialization' }
MenuMorph >> defaultColor [
	^ Theme current menu
]

{ #category : 'accessing' }
MenuMorph >> defaultLayerNumber [
	"Morphs with smaller layer number will always cover those with larger ones.
	We want to be rather on top."

	^20
]

{ #category : 'construction' }
MenuMorph >> defaultTarget: anObject [
	"Set the default target for adding menu items."

	defaultTarget := anObject.

]

{ #category : 'control' }
MenuMorph >> delete [
	activeSubMenu ifNotNil: [activeSubMenu delete].
	self itemsDo: [ :i | i deselect ].
	super delete.
]

{ #category : 'control' }
MenuMorph >> deleteIfPopUp: evt [
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu.
	Answer wether some action was actually taken."

	| h |
	h := evt ifNotNil: [ evt hand ].
	h ifNil: [
		self world ifNotNil: [ :w | h := h activeHand ]].
	h ifNil: [ h := self activeHand ].
	h ifNotNil: [
		h keyboardFocus == self ifTrue: [
			h newKeyboardFocus: prevKeyboardFocus ].
		h mouseFocus == self ifTrue: [
			"h newMouseFocus: prevMouseFocus."
			h releaseMouseFocus ]].
	stayUp ifFalse: [
		self delete.
		popUpOwner ifNotNil: [
			popUpOwner isSelected: false.
			popUpOwner deleteIfPopUp: evt ].
		^true ].
	^false
]

{ #category : 'modal progress dialog' }
MenuMorph >> displayAt: aPoint during: aBlock [
	"Add this menu to the Morphic world during the execution of the given block."

	self runningWorld ifNotNil: [ :w |
		w addMorph: self centeredNear: aPoint.
		w displayWorldOn: w mainCanvas.  "show myself"
		].
	aBlock ensure: [ self delete ].
]

{ #category : 'keyboard control' }
MenuMorph >> displayFiltered: evt [
	| matchStr allItems isMatch feedbackMorph |
	matchStr := self valueOfProperty: #matchString.
	allItems := self submorphs select: [ :m |
		m is: #MenuItemMorph ].
	allItems do: [ :m |
		isMatch := matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch ].
	feedbackMorph := self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph := LabelMorph new color: `Color darkGray`.
		self addMorphBack: feedbackMorph lock position: `0@ -20`.
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	self selectItem: nil.
	self adjustSubmorphsLayout.
]

{ #category : 'drawing' }
MenuMorph >> drawOn: aCanvas [
	
	| roundCorners |
	roundCorners := Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: self localBounds color: color radius: Theme current roundedWindowRadius * 5//6 ]
		ifFalse: [
			aCanvas fillRectangle: self localBounds color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]
]

{ #category : 'geometry' }
MenuMorph >> fontPreferenceChanged [
	super fontPreferenceChanged.
	self adjustSubmorphsLayout.
]

{ #category : 'events-processing' }
MenuMorph >> handleMouseFocusEvent: aMouseEvent [
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
				^self]]].
]

{ #category : 'event handling testing' }
MenuMorph >> handlesMouseDown: aMouseButtonEvent [
	^true
]

{ #category : 'modal progress dialog' }
MenuMorph >> informUserAt: aPoint during: aBlock [
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString := titleMorph submorphs first.
	self visible: false.
	w := self world ifNil: [ self runningWorld ].
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
			w displayWorldOn: w mainCanvas	].	 "show myself"
	]. 
	self delete.
	w ifNotNil: [
		w displayWorldOn: w mainCanvas ]
]

{ #category : 'initialization' }
MenuMorph >> initialize [
	super initialize.
	extent := `40@10`.
	defaultTarget := nil.
	selectedItem := nil.
	stayUp := false.
	popUpOwner := nil
]

{ #category : 'initialization' }
MenuMorph >> intoWorld: aWorld [
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	super intoWorld: aWorld.
	self adjustSubmorphsLayout
]

{ #category : 'modal control' }
MenuMorph >> invokeModal [
	"Invoke this menu and don't return until the user has chosen a value.
	See example below on how to use modal menu morphs."
	^ self invokeModal: (Preferences at: #menuKeyboardControl)

	"
	| menu sub entry |
	menu := MenuMorph new.
	1 to: 3 do: [:i |
		entry := 'Line', i printString.
		sub := MenuMorph new.
		menu add: entry subMenu: sub.
		#('Item A' 'Item B' 'Item C')  do:[:subEntry|
			sub add: subEntry target: menu 
				action: #modalSelection: argument: {entry. subEntry}]].
	menu invokeModal.
	"
]

{ #category : 'modal control' }
MenuMorph >> invokeModal: allowKeyboardControl [
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w actHand delay |
	w := self runningWorld.
	w ifNil: [
		| answer |
		answer := self.	"Just a marker object, can not use nil, because it is a possible answer (if user cancels)"
		UISupervisor whenUIinSafeState: [ answer := self invokeModal: allowKeyboardControl ].
		 [ answer == self ] whileTrue: [ Processor yield ].
		^answer ].
	actHand := w activeHand.
	w doOneMinimalCycleNow.
	self popUpAt: actHand morphPosition allowKeyboard: allowKeyboardControl.
	self isModalInvokationDone: false.
	delay := Delay forMilliseconds: 10.
	[ self isInWorld and: [self isModalInvokationDone not] ] whileTrue: [
		self comeToFront.
		w doOneMinimalCycleNow. delay wait ].
	self delete.
	^ self modalSelection
]

{ #category : 'testing' }
MenuMorph >> is: aSymbol [
	^ aSymbol == #MenuMorph or: [ super is: aSymbol ]
]

{ #category : 'testing' }
MenuMorph >> isIncludedInTaskbar [
	"Answer true a button for us should be added to any TaskbarMorph."

	^ stayUp
]

{ #category : 'modal control' }
MenuMorph >> isModalInvokationDone [
	^self valueOfProperty: #isModalInvokationDone ifAbsent:[false]
]

{ #category : 'modal control' }
MenuMorph >> isModalInvokationDone: aBool [
	self setProperty: #isModalInvokationDone toValue: aBool

]

{ #category : 'accessing' }
MenuMorph >> items [

	^ submorphs select: [:m | m is: #MenuItemMorph]

]

{ #category : 'accessing' }
MenuMorph >> itemsDo: aBlock [

	submorphs do: [ :m |
		(m is: #MenuItemMorph) ifTrue: [
			aBlock value: m ]].
]

{ #category : 'events' }
MenuMorph >> keyStroke: aKeyboardEvent [
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
				newKeyboardFocus: prevKeyboardFocus;
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
	aKeyboardEvent wasHandled: true.
]

{ #category : 'keyboard control' }
MenuMorph >> keyboardFocusChange: aBoolean [
	"Notify change due to green border for keyboard focus"

	self redrawNeeded
]

{ #category : 'keyboard control' }
MenuMorph >> keyboardUnfocused [
	"Just lost keyboard focus via a keyboard event.
	See also #keyboardFocused"
	
	self deleteIfPopUp: nil.
]

{ #category : 'accessing' }
MenuMorph >> label [
	"Answer a nice label.
	
	Pinned menus in the taskbar are easier to identify on big screens."

	titleMorph ifNotNil: [
		titleMorph submorphsDo: [ :labelMorph |
			^ labelMorph contents ].
		^super label ].
	"One item menu, copied by shift right click, use the item's content:"
	submorphs size = 1 ifTrue: [
		^ submorphs first label ].
	^ super label
]

{ #category : 'construction' }
MenuMorph >> labels: labelList lines: linesArray selections: selectionsArray [
	"This method allows the receiver to accept old-style SelectionMenu creation messages. It should be used only for backward compatibility during the MVC-to-Morphic transition. New code should be written using the other menu construction protocol such as addList:."
	"Labels can be either a sting with embedded crs, or a collection of strings."

	| labelArray |
	labelArray := (labelList isMemberOf: String)
		ifTrue: [ labelList lines ]
		ifFalse: [ labelList ].
	1 to: labelArray size do: [ :i |
		self add: (labelArray at: i) action: (selectionsArray at: i).
		(linesArray includes: i) ifTrue: [ self addLine ]]
]

{ #category : 'accessing' }
MenuMorph >> lastItem [
	^ submorphs reverse
		detect: [ :m | m is: #MenuItemMorph ]
		ifNone: [ submorphs last ]
]

{ #category : 'modal control' }
MenuMorph >> modalSelection [
	^self valueOfProperty: #modalSelection ifAbsent: nil
]

{ #category : 'modal control' }
MenuMorph >> modalSelection: anObject [
	self setProperty: #modalSelection toValue: anObject.
	self isModalInvokationDone: true
]

{ #category : 'events' }
MenuMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse down event."

	"Grab the menu and drag it to some other place
	This is reimplemented here because we handle the event, and if the following lines are commented,
	a menu can't be grabbed with the hand. This is not nice and shouldn't be needed"
	self isSticky ifTrue: [ ^self ].
	aMouseButtonEvent hand grabMorph: self.
]

{ #category : 'events' }
MenuMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse up event."

	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ].
]

{ #category : 'events' }
MenuMorph >> mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		"mejor en un lugar que no dependa de esto... es una porqueria"
		aMouseButtonEvent hand newMouseFocus: self ]
]

{ #category : 'keyboard control' }
MenuMorph >> moveSelectionDown: direction event: evt [
	"Move the current selection up or down by one, presumably under keyboard control.
	direction = +/-1"

	| index m |
	index := (submorphs indexOf: selectedItem ifAbsent: [1-direction]) + direction.
	submorphs do: "Ensure finite"
		[:unused | m := submorphs atWrap: index.
		((m is: #MenuItemMorph) and: [m isEnabled]) ifTrue: [
			^ self selectItem: m ].
		"Keep looking for an enabled item"
		index := index + direction sign].
	^ self selectItem: nil
]

{ #category : 'control' }
MenuMorph >> popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem [
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| trialRect |
	popUpOwner := sourceItem.
	sourceItem world addMorphFront: self position: rightOrLeftPointInWorld first.
	trialRect := rightOrLeftPointInWorld first extent: extent.
	trialRect right > sourceItem world morphWidth ifTrue: [
		self morphPosition: rightOrLeftPointInWorld second - (extent x@0)].
	self fitInOwner.
]

{ #category : 'control' }
MenuMorph >> popUpAt: aPoint allowKeyboard: aBoolean  [
	"Present this menu at the given point under control of the given hand."

	| evt hand world |
	self items isEmpty ifTrue: [^self].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	world := self runningWorld.
	world ifNil: [ ^ self ].
	hand := world activeHand.
	world addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInOwner.
	"Acquire focus for valid pop up behavior"
	prevKeyboardFocus := hand keyboardFocus.
	prevMouseFocus := hand mouseFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt := hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]
]

{ #category : 'control' }
MenuMorph >> popUpAt: aPoint forHand: hand in: aWorld [
	"Present this menu at the given point under control of the given hand.  Allow keyboard input into the menu."

	^ self 
		popUpAt: aPoint 
		forHand: hand 
		in: aWorld 
		allowKeyboard: (Preferences at: #menuKeyboardControl)
]

{ #category : 'control' }
MenuMorph >> popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean  [
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInOwner.
	"Acquire focus for valid pop up behavior"
	prevKeyboardFocus := hand keyboardFocus.
	prevMouseFocus := hand mouseFocus.
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]
]

{ #category : 'control' }
MenuMorph >> popUpForHand: hand in: aWorld [
	| p |
	"Present this menu under control of the given hand."

	p := hand morphPosition truncated.
	^self popUpAt: p forHand: hand in: aWorld

]

{ #category : 'control' }
MenuMorph >> popUpInWorld [
	"Present this menu in the current World"

	^ self popUpInWorld: self runningWorld
]

{ #category : 'control' }
MenuMorph >> popUpInWorld: aWorld [
	"Present this menu under control of the given hand."
	"Needed if not the real world but an inner PasteUpMorph"
	| positionInWorld |
	positionInWorld := aWorld internalizeFromWorld: aWorld activeHand morphPosition.
	^self
		popUpAt: positionInWorld
		forHand: aWorld activeHand
		in: aWorld

]

{ #category : 'menu' }
MenuMorph >> removeStayUpBox [
	| box |
	submorphs isEmpty ifTrue: [^self].
	(submorphs first is: #LayoutMorph) ifFalse: [^self].
	box := submorphs first submorphs second.
	(box is: #PluggableButtonMorph) 
		ifTrue: [ box hide ]
]

{ #category : 'control' }
MenuMorph >> selectItem: aMenuItem [
	selectedItem ifNotNil: [ selectedItem deselect ].
	selectedItem := aMenuItem.
	selectedItem ifNotNil: [ selectedItem select ]
]

{ #category : 'private' }
MenuMorph >> selectedItem [
	^selectedItem
]

{ #category : 'accessing' }
MenuMorph >> stayUp [

	^ self stayUp: true

]

{ #category : 'accessing' }
MenuMorph >> stayUp: aBoolean [

	stayUp := aBoolean.
	aBoolean ifTrue: [ self removeStayUpBox ].
	self taskbar ifNotNil: [ :tb | tb refreshTaskbarFor: self ].
]

{ #category : 'control' }
MenuMorph >> wantsToBeDroppedInto: aMorph [
	"Return true if it's okay to drop the receiver into aMorph.  A single-item MenuMorph is in effect a button rather than a menu, and as such should not be reluctant to be dropped into another object."

	^ (aMorph isWorldMorph or: [submorphs size = 1]) or:
		[Preferences at: #systemWindowEmbedOK]
]
