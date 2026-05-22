"
I represent an item in a menu.

Instance variables:
	isEnabled 	<Boolean>			True if the menu item can be executed.
	subMenu 	<MenuMorph | nil>	The submenu to activate automatically when the user mouses over the item.
	isSelected 	<Boolean>			True if the item is currently selected.
	target 		<Object>			The target of the associated action.
	selector 	<Symbol>			The associated action.
	arguments 	<Array>			The arguments for the associated action.
	icon		<Form | nil>		An optional icon form to be displayed to my left.

If I have a dynamic marker, created by strings like <yes> or <no> in my contents, it will be installed as a submorph.
"
Class {
	#name : 'MenuItemMorph',
	#superclass : 'LabelMorph',
	#instVars : [
		'isEnabled',
		'subMenu',
		'isSelected',
		'target',
		'selector',
		'arguments',
		'icon',
		'contentString'
	],
	#classVars : [
		'OffImage',
		'OnImage',
		'SubMenuMarker'
	],
	#category : 'Morphic-Menus',
	#package : 'Morphic',
	#tag : 'Menus'
}

{ #category : 'new-morph participation' }
MenuItemMorph class >> includeInNewMorphMenu [
	"Not to be instantiated from the menu"
	^ false
]

{ #category : 'cached state access' }
MenuItemMorph class >> initializeSubMenuMarker [

	SubMenuMarker := BitBltCanvas buildArrowOfDirection: #right size: FontFamily defaultPointSize.
]

{ #category : 'cached state access' }
MenuItemMorph class >> releaseClassCachedState [
	SubMenuMarker := nil
]

{ #category : 'cached state access' }
MenuItemMorph class >> subMenuMarker [

	SubMenuMarker ifNotNil: [ ^SubMenuMarker ].
	
	self initializeSubMenuMarker.
	
	^SubMenuMarker
]

{ #category : 'grabbing' }
MenuItemMorph >> aboutToBeGrabbedBy: aHand [
	"Don't allow the receiver to act outside a Menu"
	| menu |
	self isSelected: false.
	menu := MenuMorph new defaultTarget: nil.
	menu addMorphFront: self.
	menu stayUp.
	^ menu
]

{ #category : 'private' }
MenuItemMorph >> actionBlockForEvent: anEvent [

	| selArgCount |
	(selArgCount := selector numArgs) = 0
		ifTrue: [
			^ [ target perform: selector ]]
		ifFalse: [ | messageArguments |
			messageArguments := selArgCount = arguments size
				ifTrue: [ arguments ] ifFalse: [ arguments copyWith: anEvent ].
				
			^ [ target perform: selector withArguments: messageArguments ]]
]

{ #category : 'events' }
MenuItemMorph >> activateOwnerMenu: evt [
	"Activate our owner menu; e.g., pass control to it"
	owner ifNil: [ ^false ]. "not applicable"
	(owner includesPixel: evt eventPosition)
		ifFalse: [ ^false ].
	owner activate: evt.
	^true
]

{ #category : 'events' }
MenuItemMorph >> activateSubmenu: event [
	"Activate our submenu; e.g., pass control to it"

	subMenu ifNil: [ ^false ]. "not applicable"
	subMenu isInWorld ifFalse: [ ^false ].	
	(subMenu includesPixel: event eventPosition) ifFalse: [^false].
	subMenu activate: event.
	^true
]

{ #category : 'debug and other' }
MenuItemMorph >> buildDebugMenu: aHand  [
	
	| aMenu |
	aMenu := super buildDebugMenu: aHand.
	
	aMenu addLine.
	aMenu add: 'debug action' action: #debugAction.
	
	^aMenu
]

{ #category : 'accessing' }
MenuItemMorph >> contents: aString [
	^self contentsWithMarkers: aString inverse: false
]

{ #category : 'accessing' }
MenuItemMorph >> contentsWithMarkers: aString inverse: invertMarkers [
	"Set the menu item entry. Parse aString for embedded markers."

	| markerIndex marker |
	contentString := nil.	"get rid of old"
	self removeAllMorphs.	"get rid of old markers if updating"
	(aString notEmpty and: [aString first == $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	contentString := aString.	"remember actual string"
	marker := ((marker = '<on>' or: [marker = '<yes>']) xor: invertMarkers)
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents: (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph newWith: marker.
	self addMorphFront: marker position: 1@(self morphHeight +1- marker morphHeight // 2).
	marker lock.
]

{ #category : 'debug and other' }
MenuItemMorph >> debugAction [
	| debugger actionBlock |
	(self isEnabled or: [ PopUpMenu confirm: 'The menu item is disabled, do you want to continue?' ])
		ifFalse: [ ^ self ].
	
	actionBlock := self actionBlockForEvent: nil. " maybe we should generate a synthetic/placeholder event instead of using nil "
	debugger := Debugger
		openDebugging: actionBlock
		label: 'Debugging action #', selector.
	
	[debugger interruptedContext receiver == target]
		whileFalse: [debugger send].
]

{ #category : 'initialization' }
MenuItemMorph >> deleteIfPopUp: evt [
	"Recurse up for nested pop ups"
	owner ifNotNil: [^owner deleteIfPopUp: evt].
	^false
]

{ #category : 'selecting' }
MenuItemMorph >> deselect [

	self isSelected: false.
	subMenu ifNotNil: [
		owner ifNotNil: [ owner activeSubmenu: nil ]]
]

{ #category : 'drawing' }
MenuItemMorph >> drawOn: aCanvas  [
	| stringColor leftEdge magnifiedIcon |

	stringColor := color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: self localBounds color: Theme current menuHighlight].
	leftEdge := 0.

	self hasMarker ifTrue: [
		leftEdge := leftEdge + submorphs first morphWidth + 8 ].

	icon ifNotNil: [
		magnifiedIcon := self magnifiedIcon.
		aCanvas image: magnifiedIcon at: leftEdge+1 @ (magnifiedIcon height *1//10).
		leftEdge := magnifiedIcon width *12//10 + leftEdge].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: self class subMenuMarker
			at: extent x - self class subMenuMarker width @ (extent y - self class subMenuMarker height // 2) ]
]

{ #category : 'grabbing' }
MenuItemMorph >> duplicateMorph: evt [
	"Don't allow the receiver to act outside a Menu"
	| dup menu |
	dup := self duplicate isSelected: false.
	menu := MenuMorph new defaultTarget: nil.
	menu addMorphFront: dup.
	menu stayUp.
	^ menu
]

{ #category : 'font' }
MenuItemMorph >> font: aFont emphasis: emphasisCode [

	super font: aFont emphasis: emphasisCode.
	contentString ifNotNil: [
		self contentsWithMarkers: contentString inverse: false ].
]

{ #category : 'event handling testing' }
MenuItemMorph >> handlesKeyboardFocus [
	"It is handled by owner?"
	
	^false
]

{ #category : 'event handling testing' }
MenuItemMorph >> handlesMouseDown: evt [

	^ true

]

{ #category : 'event handling testing' }
MenuItemMorph >> handlesMouseOver: anEvent [
	"implements #mouseEnter: and/or #mouseLeave:"
	^true
]

{ #category : 'accessing' }
MenuItemMorph >> hasIcon [
	"Answer whether the receiver has an icon."
	^ icon notNil
]

{ #category : 'accessing' }
MenuItemMorph >> hasMarker [
	"Answer whether the receiver has a marker morph."
	^ submorphs isEmpty not
]

{ #category : 'accessing' }
MenuItemMorph >> hasSubMenu [
	"Return true if the receiver has a submenu"
	^subMenu notNil
]

{ #category : 'initialization' }
MenuItemMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	isEnabled := true.
	subMenu := nil.
	isSelected := false.
	target := nil.
	selector := nil.
	arguments := nil.
	font := Preferences at: #standardMenuFont.
	self contents: ''.
]

{ #category : 'events' }
MenuItemMorph >> invokeWithEvent: evt [
	"Perform the action associated with the given menu item."

	self isEnabled ifFalse: [^ self ].
	owner ifNotNil: [ owner deleteIfPopUp: evt ].
	selector ifNil: [ ^self ].
	
	(self actionBlockForEvent: evt) value.
]

{ #category : 'testing' }
MenuItemMorph >> is: aSymbol [
	^ aSymbol == #MenuItemMorph or: [ super is: aSymbol ]
]

{ #category : 'accessing' }
MenuItemMorph >> isEnabled [

	^ isEnabled

]

{ #category : 'accessing' }
MenuItemMorph >> isEnabled: aBoolean [

	isEnabled = aBoolean ifTrue: [^ self].
	isEnabled := aBoolean.
	self color: (aBoolean ifTrue: [`Color black`] ifFalse: [`Color lightGray`]).
]

{ #category : 'selecting' }
MenuItemMorph >> isSelected: aBoolean [

	isSelected := aBoolean.
	self redrawNeeded
]

{ #category : 'layout' }
MenuItemMorph >> magnifiedIcon [

	| iconForm w h factor magnifiedExtent magnifiedIcon |
	icon ifNil: [ ^nil ].
	iconForm := isEnabled ifTrue: [ icon ] ifFalse: [ icon asGrayScaleAndTransparent ].
	magnifiedIcon := iconForm.
	w := iconForm width.
	h := iconForm height.
	w*h = 0 ifFalse: [
		factor := extent y * 0.8 / h.
		factor = 1.0 ifFalse: [
			magnifiedExtent := (iconForm extent * factor) rounded.
			magnifiedIcon := iconForm magnifyTo: magnifiedExtent ]].
	^magnifiedIcon
]

{ #category : 'layout' }
MenuItemMorph >> measureContents [
	| e |
	e := super measureContents.
	^e y > 12
		ifTrue: [e+`2@2`]
		ifFalse: [e+`2@1`]
]

{ #category : 'layout' }
MenuItemMorph >> minItemWidth [
	| fontToUse iconWidth subMenuWidth markerWidth |
	fontToUse := self fontToUse.
	subMenuWidth := self hasSubMenu
				ifFalse: [0]
				ifTrue: [self class subMenuMarker width].
	iconWidth := icon
				ifNotNil: [self magnifiedIcon width * 12//10]
				ifNil: [0].
	markerWidth := self hasMarker
		ifTrue: [ submorphs first morphWidth + 15 ]
		ifFalse: [ 0 ].
	^ (fontToUse widthOfString: contents)
		+ subMenuWidth + iconWidth + markerWidth.
]

{ #category : 'events' }
MenuItemMorph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	aMouseButtonEvent shiftPressed ifTrue: [
		super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].  "enable label editing" 
	aMouseButtonEvent hand newMouseFocus: owner. "Redirect to menu for valid transitions"
	owner selectItem: self
]

{ #category : 'events' }
MenuItemMorph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."

	owner hasMouseFocus ifFalse: [ ^self ].
	owner selectedItem == self ifFalse: [ ^self ].
	"This will happen if the menu has toggles in it. (for instance, the 'show...' button)
	Update the look, refresh the world and wait a bit,
	to give the user some visual feedback"
	contentString ifNotNil: [
		self contentsWithMarkers: contentString inverse: true.
		self world ifNotNil: [ :w | w displayWorldOn: w mainCanvas ].  "show myself"
		(Delay forMilliseconds: 200) wait].
	self deselect.
	self invokeWithEvent: aMouseButtonEvent
]

{ #category : 'events' }
MenuItemMorph >> mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition [
	"Create a copy of me or my submenu."

	aMouseButtonEvent shiftPressed ifTrue: [
		(subMenu
			ifNil: [ self duplicate ]
			ifNotNil: [
				(subMenu duplicate)
					addTitle: contents;
					addStayUpIcons;
					stayUp;
					adjustSubmorphsLayout;
					yourself ])
						openInHand ].
	super mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
]

{ #category : 'events' }
MenuItemMorph >> mouseEnter: aMouseEnterEvent [
	"The mouse entered the receiver"
	super mouseEnter: aMouseEnterEvent.
	owner ifNotNil: [
		owner selectItem: self ]
]

{ #category : 'private' }
MenuItemMorph >> offImage [
	"Return the form to be used for indicating an '<off>' marker"
	| e ps1 ps2 |
	ps1 := self fontToUse pointSize.
	ps2 := FontFamily defaultPointSize.
	e := (ps1 < ps2 ifTrue: [ ps1+ ps2 // 2 ] ifFalse: [ ps1 * 2 + (ps2 * 6) // 8 ]) asPoint.
	(OffImage isNil or: [ OffImage extent ~= e ]) ifTrue: [
		OffImage := Form extent: e depth: 32.
		OffImage getCanvas
			frameAndFillRectangle: OffImage boundingBox fillColor: `(Color gray: 0.9)`
				borderWidth: 2 borderColor: `Color black` ].
	^OffImage
]

{ #category : 'private' }
MenuItemMorph >> onImage [
	"Return the form to be used for indicating an '<on>' marker"
	| e ps1 ps2 |
	ps1 := self fontToUse pointSize.
	ps2 := FontFamily defaultPointSize.
	e := (ps1 < ps2 ifTrue: [ ps1+ ps2 // 2 ] ifFalse: [ ps1 * 2 + (ps2 * 6) // 8 ]) asPoint.
	(OnImage isNil or: [ OnImage extent ~= e ]) ifTrue: [
		OnImage := Form extent: e depth: 32.
		OnImage getCanvas
			frameAndFillRectangle: OnImage boundingBox fillColor: `Color gray: 0.9`
				borderWidth: 2 borderColor: `Color black`;
			fillRectangle: (OnImage boundingBox insetBy: 3) color: `Color black` ].
	^OnImage
]

{ #category : 'selecting' }
MenuItemMorph >> select [
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		self displayBounds ifNotNil: [ :r |
			subMenu delete.
			subMenu
				popUpAdjacentTo: (Array with: r topRight + `10@0` with: r topLeft)
				from: self ].
		subMenu selectItem: nil ]
]

{ #category : 'accessing' }
MenuItemMorph >> setBlankIcon [
	"change the the receiver's icon"
	icon := Theme current blankIcon
]

{ #category : 'accessing' }
MenuItemMorph >> setIcon: symbolOrFormOrNil [
	"Argument can be a Form, a Symbol (to be sent to Theme current) or nil."

	icon := symbolOrFormOrNil isSymbol
		ifTrue: [Theme current perform: symbolOrFormOrNil]
		ifFalse: [ symbolOrFormOrNil ]
]

{ #category : 'accessing' }
MenuItemMorph >> subMenu [

	^ subMenu

]

{ #category : 'accessing' }
MenuItemMorph >> subMenu: aMenuMorph [
	subMenu := aMenuMorph.
	self redrawNeeded
]

{ #category : 'accessing' }
MenuItemMorph >> target: anObject selector: aSymbol arguments: aCollection [

	target := anObject.
	selector := aSymbol.
	arguments := aCollection
]
