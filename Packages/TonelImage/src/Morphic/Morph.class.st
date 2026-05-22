"
A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://wiki.squeak.org/squeak/morph. 

Morphs exist in a tree, rooted at a World (a WorldMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets 
drawn. To hide a Morph and its submorphs, send the #visible: message. 

Subclass PlacedMorph defines a local coordinate system (see its class comment). Morphs NOT in the PlacedMorph hierarchy live in the space defined by their owner. Their local coordinate system is the same as the owner's. They ignore #morphPosition:.

Local coordinates are used in the #drawOn: method (the Canvas understands positions in the local coordinate system), for the positions of submorphs (for example #morphPosition and #morphPosition:) and for positions carried by mouse events.

Events are delivered to morphs in Z-order, i.e. if a morph occludes another the event is only delivered to the foremost (just like physical objects). Events received by a morph carry positions in the local coordinate system.

Structure:
instance var 				Type 						Description 
owner 				Morph or nil		 				My parent Morph, or nil for the top-level Morph, which is a WorldMorph
submorphs 				Array 						My child Morphs.
"
Class {
	#name : 'Morph',
	#superclass : 'Object',
	#instVars : [
		'owner',
		'submorphs',
		'properties',
		'id',
		'privateDisplayBounds'
	],
	#classVars : [
		'LastMorphId'
	],
	#category : 'Morphic-Kernel',
	#package : 'Morphic',
	#tag : 'Kernel'
}

{ #category : 'new-morph participation' }
Morph class >> categoryInNewMorphMenu [
	"Unknown"
	^ nil
]

{ #category : 'initialize-release' }
Morph class >> clearIds [

	LastMorphId := nil.
	self allSubInstancesDo: [ :each |
		each clearId ]
]

{ #category : 'new-morph participation' }
Morph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"
	^ true
]

{ #category : 'instance creation' }
Morph class >> initializedInstance [
	"Answer an instance of the receiver which in some sense is initialized.  In the case of Morphs, this will yield an instance that can be attached to the Hand after having received the same kind of basic initialization that would be obtained from an instance chosen from the 'new morph' menu.
	Return nil if the receiver is reluctant for some reason to return such a thing"

	^ self new
]

{ #category : 'initialize-release' }
Morph class >> releaseClassState [

	self allInstancesDo: [ :each |
		each releaseCachedState.
		each clearId ].
	LastMorphId := nil.
]

{ #category : 'dropping/grabbing' }
Morph >> aboutToBeGrabbedBy: aHand [
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^nil  		"PlacedMorphs can be grabbed and moved around with the hand"
]

{ #category : 'dropping/grabbing' }
Morph >> aboutToGrab: submorph [
	"submorph is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^submorph "Grab it"
]

{ #category : 'dropping/grabbing' }
Morph >> acceptDroppingMorph: aMorph event: aDropEvent [
	"This message is sent when a morph is dropped onto a morph that has agreed to accept the dropped morph by responding 'true' to the wantsDroppedMorph:event: message. This default implementation just adds the given morph to the receiver."
	
	self addMorph: aMorph
]

{ #category : 'active model behavior' }
Morph >> actionMap [

	^self valueOfProperty: #actionMap ifAbsent: nil
]

{ #category : 'user interface' }
Morph >> activateWindow [

	self activateWindowAndSendTopToBack: false
]

{ #category : 'user interface' }
Morph >> activateWindowAndSendTopToBack: aBoolean [

	self owningWindow ifNotNil: [ :w |
		w activateAndSendTopToBack: aBoolean]
]

{ #category : 'menus' }
Morph >> addAddHandMenuItemsForHalo: aMenu hand: aHandMorph [
	"The former charter of this method was to add halo menu items that pertained specifically to the hand.  Over time this charter has withered, and most morphs reimplement this method simply to add their morph-specific menu items.  So in the latest round, all other implementors in the standard image have been removed.  However, this is left here as a hook for the benefit of existing code in client uses."


]

{ #category : 'events-alarms' }
Morph >> addAlarm: aSelector after: delayTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: nil after: delayTime
]

{ #category : 'events-alarms' }
Morph >> addAlarm: aSelector with: arg1 after: delayTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: (Array with: arg1) after: delayTime
]

{ #category : 'events-alarms' }
Morph >> addAlarm: aSelector withArguments: args after: delayTime [
	"Add an alarm (that is an action to be executed once) with the given set of parameters"

	self alarmScheduler ifNotNil: [ :scheduler |
		scheduler
			addAlarm: aSelector
			withArguments: args
			for: self
			at: Time localMillisecondClock + delayTime ]
]

{ #category : 'submorphs-add/remove' }
Morph >> addAllMorphs: aCollection [
	^ (aCollection size > 0) 
		ifTrue: [ self privateAddAllMorphs: aCollection atIndex: submorphs size ]
		ifFalse: [ self ]
]

{ #category : 'submorphs-add/remove' }
Morph >> addAllMorphs: aCollection after: anotherMorph [
	^self privateAddAllMorphs: aCollection 
			atIndex: (submorphs indexOf: anotherMorph ifAbsent: [submorphs size])
]

{ #category : 'menus' }
Morph >> addColorMenuItems: aMenu hand: aHand  [
	"Add the items for changing the current color of the Morph"

	aMenu add: 'change color...' action: #changeColor
]

{ #category : 'menus' }
Morph >> addCopyItemsTo: aMenu  [
	"Add copy-like items to the halo menu"

	self flag: #assumesKeyboardShortcuts.
	aMenu add: 'copy to clipboard (c)' action: #copyToClipboard:
]

{ #category : 'menus' }
Morph >> addCustomHaloMenuItems: aMenu hand: aHandMorph [
	"Add morph-specific items to the given menu which was invoked by the given hand from the halo.  To get started, we defer to the counterpart method used with the option-menu, but in time we can have separate menu choices for halo-menus and for option-menus"

	self addCustomMenuItems: aMenu hand: aHandMorph
]

{ #category : 'menus' }
Morph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [
	"Add morph-specific items to the given menu which was invoked by the given hand.  This method provides is invoked both from the halo-menu and from the control-menu regimes."

]

{ #category : 'debug and other' }
Morph >> addDebuggingItemsTo: aMenu hand: aHandMorph [
	aMenu add: 'debug...' subMenu:  (self buildDebugMenu: aHandMorph)
]

{ #category : 'meta-actions' }
Morph >> addEmbeddingMenuItemsTo: aMenu hand: aHandMorph [
	| menu |
	menu := MenuMorph new defaultTarget: self.
	self potentialEmbeddingTargets reverseDo: [:m | 
		menu 
			add: m class name asPlainString 
			targetHighlight: m 
			action: #addMorphFrontFromWorldPosition: 
			argumentList: {self}].
	aMenu ifNotNil:[
		menu submorphCount > 0 
			ifTrue:[aMenu add:'embed into' subMenu: menu].
	].
	^menu
]

{ #category : 'menus' }
Morph >> addExportMenuItems: aMenu hand: aHandMorph [
	"Add export items to the menu"

	aMenu ifNotNil: [
		| aSubMenu |
		aSubMenu := MenuMorph new defaultTarget: self.
		(self respondsTo: #exportAsPNG) ifTrue: [
			aSubMenu add: 'PNG file' action: #exportAsPNG.].
		aSubMenu add: 'BMP file' action: #exportAsBMP.
		aSubMenu add: 'JPEG file' action: #exportAsJPEG.
		aMenu add: 'export...' subMenu: aSubMenu]

]

{ #category : 'halos and balloon help' }
Morph >> addHalo [
	"Invoke a halo programatically (e.g., not from a meta gesture)"
	self addHalo: nil
]

{ #category : 'halos and balloon help' }
Morph >> addHalo: aMorphicEventOrNil [

	| hand position |
	aMorphicEventOrNil
		ifNil: [
			hand := self world activeHand.
			position := hand lastMouseEvent eventPosition ]
		ifNotNil: [
			hand := aMorphicEventOrNil hand.
			position := aMorphicEventOrNil eventPosition ].

	HaloMorph new popUpFor: self handPosition: position hand: hand.
]

{ #category : 'menus' }
Morph >> addHaloActionsTo: aMenu  [
	"Add items to aMenu representing actions requestable via halo"

	| subMenu |
	subMenu := MenuMorph new defaultTarget: self.
	subMenu addTitle: self shortPrintString.
	subMenu addStayUpIcons.
	subMenu addLine.
	(subMenu add: 'delete' action: #dismissViaHalo)
		setBalloonText: 'Delete this object -- warning -- can be destructive!' .
	self maybeAddCollapseItemTo: subMenu.
	(subMenu add: 'grab' action: #openInHand)
		setBalloonText: 'Pick this object up -- warning, since this removes it from its container, it can have adverse effects.' .
	subMenu addLine.
	(subMenu add: 'duplicate' action: #maybeDuplicateMorph)
		setBalloonText: 'Hand me a copy of this object'.
	(subMenu
		add: 'set color'
		target: self
		action: #changeColor)
			setBalloonText: 'Change the color of this object'.
	(subMenu
		add: 'inspect'
		target: self
		action: #inspect)
			setBalloonText: 'Open an Inspector on this object'.
	aMenu add: 'halo actions...' subMenu: subMenu
]

{ #category : 'halos and balloon help' }
Morph >> addHandlesTo: aHaloMorph box: box [
	"Add halo handles to the halo.  Apply the halo filter if appropriate"

	self haloSpecifications do: [ :aSpec |
		(self
			wantsHaloHandleWithSelector: aSpec addHandleSelector
			inHalo: aHaloMorph) ifTrue: [
		aHaloMorph
			perform: aSpec addHandleSelector
			with: aSpec ]].
	aHaloMorph target
		addOptionalHandlesTo: aHaloMorph
		box: box
]

{ #category : 'submorphs-menus' }
Morph >> addItemTo: topMenu fromSpec: itemSpec submenus: submenusDictionary [
	"
	See coment at Morph >> #getMenu
	"
	| submenuId menu label target selector arguments iconSymbol helpText menuItem |
	label := itemSpec second.
	label ifNil: [ ^self ]. "Only a keyboard shortcut, not a real menu item."
	submenuId := itemSpec first // 1000.
	menu := submenuId = 0 ifTrue: [ topMenu ] ifFalse: [ submenusDictionary at: submenuId ifAbsent: [^self]].
	target := itemSpec fourth
		ifNotNil: [ :targetOrGetter |
			targetOrGetter isSymbol
				ifTrue: [ 	self perform: targetOrGetter ]
				ifFalse: [ targetOrGetter ]]
		ifNil: [ self ].
	selector := itemSpec fifth.
	arguments := (selector isNil or: [ selector numArgs = 0 ])
		ifTrue: [#()]
		ifFalse: [itemSpec size >= 8 ifTrue: [ {itemSpec eighth} ] ifFalse: [ {self} ]].
	menuItem := label isSymbol
		ifTrue: [ menu addUpdating: label target: target action: selector argumentList: arguments ]
		ifFalse: [ menu add: label target: target action: selector argumentList: arguments ].
	iconSymbol := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
	iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
	helpText := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
	helpText ifNotNil: [ menuItem setBalloonText: helpText ].
	
	selector
		 ifNil: [
			menuItem isEnabled: false ]
		ifNotNil: [
			| enabledSelector |
			enabledSelector := itemSpec size >= 9 ifTrue: [ itemSpec ninth ].
			enabledSelector ifNotNil: [ 
				menuItem isEnabled: (target perform: enabledSelector) ]].

]

{ #category : 'submorphs-add/remove' }
Morph >> addLabel: string font: font centeredAt: position rotationDegrees: angle [
	"Add a label top-centered at position and rotated of angle degrees"
	| p label |
	p := position - (position normal * (font widthOfString: string asPlainString) / 2).
	label := LabelMorph
		contents: string
		font: font.
	label rotation: (90 - angle) degreesToRadians.
	self addMorph: label.
	label morphPosition: p.
	^ label.
]

{ #category : 'submorphs-menus' }
Morph >> addLineTo: topMenu fromSpec: itemSpec submenus: submenusDictionary [
	| submenuId menu |
	submenuId := itemSpec first // 1000.
	menu := submenuId = 0 ifTrue: [ topMenu ] ifFalse: [ submenusDictionary at: submenuId ifAbsent: [^self]].
	menu addLine.
]

{ #category : 'submorphs-add/remove' }
Morph >> addMorph: aMorph [
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	self addMorphFront: aMorph
]

{ #category : 'submorphs-add/remove' }
Morph >> addMorph: newMorph behind: aMorph [
	"Add a morph to the list of submorphs behind the specified morph
		Play with this in a Workspace:
		
	r := ColoredBoxMorph new color: Color red. g := ColoredBoxMorph new color: Color green. b := ColoredBoxMorph new color: Color blue.
	y := ColoredBoxMorph new color: Color yellow. o := ColoredBoxMorph new color: Color orange. p := ColoredBoxMorph new color: Color pink.

	l := LinearLayoutMorph newColumn.
	l morphExtent: 200@200.
	l addMorph: r. l addMorph: g. l addMorph: b. l addMorph: y. l addMorph: o. l addMorph: p.
	l openInWorld.

	'Original positions'.
	l addMorph: b behind: g. l addMorph: b inFrontOf: g.

	'Try these once and repeated times, in random order. Repeated times should have no effect.'.
	l addMorph: b behind: r.
	l addMorph: b behind: g.
	l addMorph: b behind: y.
	l addMorph: b behind: o.
	l addMorph: b behind: p.
	"
	| aMorphIndex index |
	newMorph == aMorph ifTrue: [
		^ self error: 'both arguments are the same morph' ].
	aMorphIndex := submorphs indexOf: aMorph.
	
	aMorphIndex = 0 ifTrue: [ ^ self error: 'cannot add a morph behind a non submorph' ].
	
	index := (newMorph owner == self and: [
		(submorphs indexOf: newMorph) < aMorphIndex])
			ifTrue: [aMorphIndex]
			ifFalse: [aMorphIndex + 1].
	self privateAddMorph: newMorph atIndex: index
]

{ #category : 'submorphs-add/remove' }
Morph >> addMorph: newMorph inFrontOf: aMorph [
	"Add a morph to the list of submorphs in front of the specified morph
		Play with this in a Workspace:
		
	r := ColoredBoxMorph new color: Color red. g := ColoredBoxMorph new color: Color green. b := ColoredBoxMorph new color: Color blue.
	y := ColoredBoxMorph new color: Color yellow. o := ColoredBoxMorph new color: Color orange. p := ColoredBoxMorph new color: Color pink.

	l := LinearLayoutMorph newColumn.
	l morphExtent: 200@200.
	l addMorph: r. l addMorph: g. l addMorph: b. l addMorph: y. l addMorph: o. l addMorph: p.
	l openInWorld.

	'Original positions'.
	l addMorph: b behind: g. l addMorph: b inFrontOf: g.

	'Try these once and repeated times, in random order. Repeated times should have no effect.'.
	l addMorph: b inFrontOf: r.
	l addMorph: b inFrontOf: g.
	l addMorph: b inFrontOf: y.
	l addMorph: b inFrontOf: o.
	l addMorph: b inFrontOf: p.
	"
	| aMorphIndex index |
	newMorph == aMorph ifTrue: [
		^ self error: 'both arguments are the same morph' ].
	aMorphIndex := submorphs indexOf: aMorph.
	index := (newMorph owner == self and: [
		(submorphs indexOf: newMorph) < aMorphIndex])
			ifTrue: [aMorphIndex - 1]
			ifFalse: [aMorphIndex].
	self privateAddMorph: newMorph atIndex: index.
]

{ #category : 'submorphs-add/remove' }
Morph >> addMorph: aMorph position: aPoint [
	^self privateAddMorph: aMorph atIndex: 1 position: aPoint
]

{ #category : 'submorphs-add/remove' }
Morph >> addMorphBack: aMorph [
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	self privateAddMorph: aMorph
		atIndex: (aMorph owner == self
			ifTrue: [submorphs size]
			ifFalse: [submorphs size+1])
]

{ #category : 'submorphs-add/remove' }
Morph >> addMorphBack: aMorph position: aPoint [
	self
		privateAddMorph: aMorph
		atIndex: (aMorph owner == self
			ifTrue: [submorphs size]
			ifFalse: [submorphs size+1])
		position: aPoint
]

{ #category : 'submorphs-add/remove' }
Morph >> addMorphFront: aMorph [
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	^self privateAddMorph: aMorph atIndex: 1
]

{ #category : 'submorphs-add/remove' }
Morph >> addMorphFront: aMorph position: aPoint [
	^self privateAddMorph: aMorph atIndex: 1 position: aPoint
]

{ #category : 'submorphs-add/remove' }
Morph >> addMorphFrontFromWorldPosition: aMorph [

	| tx |
	tx := GeometryTransformation identity.
	aMorph withAllOwnersDo: [ :o |
		tx := o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx := o location inverseTransformation composedWith: tx ].
	self addMorphFront: aMorph.
	aMorph location: tx
]

{ #category : 'halos and balloon help' }
Morph >> addOptionalHandlesTo: aHalo box: box [
	
]

{ #category : 'menus' }
Morph >> addStandardHaloMenuItemsTo: aMenu hand: aHandMorph [
	"Add standard halo items to the menu"

	| unlockables |

	self isWorldMorph ifTrue:
		[^ self addWorldHaloMenuItemsTo: aMenu hand: aHandMorph].

	aMenu add: 'send to back' action: #goBehind.
	aMenu add: 'bring to front' action: #comeToFront.
	self addEmbeddingMenuItemsTo: aMenu hand: aHandMorph.
	aMenu addLine.

	self addColorMenuItems: aMenu hand: aHandMorph.
	self addHaloActionsTo: aMenu.
	aMenu addLine.
	self addToggleItemsToHaloMenu: aMenu.
	aMenu addLine.
	self addCopyItemsTo: aMenu.
	self addExportMenuItems: aMenu hand: aHandMorph.
	self addDebuggingItemsTo: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu defaultTarget: self.

	aMenu addLine.

	unlockables := self submorphs select:
		[:m | m isLocked].
	unlockables size = 1 ifTrue:
		[aMenu
			add: ('unlock "{1}"' format: unlockables first shortPrintString)
			action: #unlockContents].
	unlockables size > 1 ifTrue:
		[aMenu add: 'unlock all contents' action: #unlockContents.
		aMenu add: 'unlock...' action: #unlockOneSubpart].

	aMenu defaultTarget: aHandMorph.

]

{ #category : 'submorphs-menus' }
Morph >> addSubmenuTo: menu fromSpec: itemSpec submenus: submenusDictionary [
	| submenu label submenuId iconSymbol helpText menuItem |
	submenu := MenuMorph new.
	label := itemSpec second.
	submenuId := itemSpec fifth // 1000.
	menuItem := menu add: label subMenu: submenu.
	iconSymbol := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
	iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
	helpText := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
	helpText ifNotNil: [ menuItem setBalloonText: helpText ].
	submenusDictionary at: submenuId put: submenu
]

{ #category : 'menus' }
Morph >> addTitleForHaloMenu: aMenu [
	aMenu addTitle: self shortPrintString.
]

{ #category : 'submorphs-menus' }
Morph >> addTitleTo: menu fromSpec: itemSpec [
	menu addTitle: itemSpec second.
	itemSpec third ifTrue: [ menu addStayUpIcons ]
]

{ #category : 'menus' }
Morph >> addToggleItemsToHaloMenu: aMenu [
	"Add standard true/false-checkbox items to the memu"

	#(
		(stickinessString toggleStickiness 'whether I should be resistant to a drag done by mousing down on me')
		(lockedString lockUnlockMorph 'when "locked", I am inert to all user interactions')
	) do: [ :trip | 
			(aMenu addUpdating: trip first action: trip second)
				setBalloonText: trip third ]
]

{ #category : 'change reporting' }
Morph >> addedMorph: aMorph [
	"Notify the receiver that the given morph was just added."

]

{ #category : 'geometry' }
Morph >> adjustDisplayBoundsBy: delta [
	"Private for framework use.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us."

	privateDisplayBounds ifNotNil: [
		privateDisplayBounds := delta ifNil: [ nil ] ifNotNil: [privateDisplayBounds translatedBy: delta]].
	self allSubmorphsDo: [ :m | m adjustDisplayBoundsBy: delta ].
]

{ #category : 'accessing' }
Morph >> adoptWidgetsColor: paneColor [
	self submorphsDo: [ :m| m adoptWidgetsColor: paneColor]
]

{ #category : 'events-alarms' }
Morph >> alarmScheduler [
	"Return the scheduler being responsible for triggering alarms"
	^self world
]

{ #category : 'submorphs-menus' }
Morph >> allMenuNshortcutsSpecs [
	"Spec for the menu and shortcut keys.
	This is the main method to get the specs, either to open a Menu or to start a Keyboard Shortcut action.
	See senders: #getMenu and #keyboardShortcut:. See #getMenu for details of the spec format.
	This default implementation allows any morph to define their own #menuNshortcutsSpecsDo: although few do.
	It also traverses owners, as an owner may specify menu-and-shortcut specs for their submorphs.
	Main users of this are SystemWindow. See comment at SystemWindow >> #menuNshortcutsSpecsFor:do:
	Any morph redefining this method will take full control of its own menu and keyboard shortcuts."

	| menuNshortcutsSpecs |
	menuNshortcutsSpecs := Dictionary new.
	"First, take menu and shortcut specs declared by us (i.e. the widget)"
	self menuNshortcutsSpecsDo: [ :menuItemSpec |
		menuNshortcutsSpecs at: menuItemSpec first put: menuItemSpec ].
	
	"Then allow all owners to override or remove them.
	This is done outwards, each morph can override choices made by submorphs.
	This helps building reusable composites, that can be tweaked by user morphs / windows."
	self allOwnersDo: [ :o |
		o menuNshortcutsSpecsFor: self do: [ :menuItemSpec |
			menuNshortcutsSpecs at: menuItemSpec first put: menuItemSpec ]].
	
	^ menuNshortcutsSpecs.
]

{ #category : 'submorphs-accessing' }
Morph >> allMorphsBut: aMorph do: aBlock  [
	"Evaluate the given block for all morphs in this composite morph (including the receiver),
	but avoid aMorph and all its submorphs.
	Also avoid halos (might happen when called on the World) and Hands"

	self == aMorph ifTrue: [ ^self ].
	(self is: #HaloMorph) ifTrue: [ ^self ].
	submorphs do: [:m | m allMorphsBut: aMorph do: aBlock ].
	aBlock value: self.
]

{ #category : 'submorphs-accessing' }
Morph >> allMorphsDo: aBlock [
	"Evaluate the given block for all morphs in this composite morph (including the receiver).
	Note: Redefined in WorldMorph to also include the Hand and any carried morphs."

	aBlock value: self.
	self allSubmorphsDo: aBlock
]

{ #category : 'submorphs-accessing' }
Morph >> allMorphsFrontToBackDo: aBlock [
	"Evaluate the given block for all morphs in this composite morph (including the receiver).
	Do it front to back. Any morph partially covering another is drawn first."

	self allSubmorphsFrontToBackDo: aBlock.
	aBlock value: self.
]

{ #category : 'structure' }
Morph >> allOwnersDo: aBlock [
	"Evaluate aBlock with all owners of the receiver.
	Start from our owner, go up to top owner."
	
	owner ifNotNil: [ owner withAllOwnersDo: aBlock ]
]

{ #category : 'structure' }
Morph >> allOwnersReverseDo: aBlock [
	"Evaluate aBlock with all owners of the receiver.
	Start from top owner, go down to our owner."

	owner ifNotNil: [ owner withAllOwnersReverseDo: aBlock ]
]

{ #category : 'submorphs-accessing' }
Morph >> allSubmorphsDo: aBlock [
	"Evaluate the given block for all morphs in this composite morph (not including the receiver)."

	submorphs do: [ :m |
		aBlock value: m.
		m allSubmorphsDo: aBlock].
]

{ #category : 'submorphs-accessing' }
Morph >> allSubmorphsFrontToBackDo: aBlock [
	"Evaluate the given block for all morphs in this composite morph (not including the receiver).
	Do it front to back. Any morph partially covering another is drawn first."

	submorphs do: [ :m |
		m allSubmorphsFrontToBackDo: aBlock.
		aBlock value: m ].
]

{ #category : 'event handling testing' }
Morph >> allowsFilesDrop [
	"Answer whether we accept dropping files. By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'allowsFilesDrop'
]

{ #category : 'event handling testing' }
Morph >> allowsMorphDrop [
	"Answer whether we accept dropping morphs. By default answer false."

	^ (self valueOfProperty: #allowsMorphDrop) = true
]

{ #category : 'event handling testing' }
Morph >> allowsSubmorphDrag [
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'allowsSubmorphDrag'
]

{ #category : 'submorphs-add/remove' }
Morph >> atFront [
	
	^owner firstSubmorph == self
]

{ #category : 'halos and balloon help' }
Morph >> balloonHelpDelayTime [
	"Return the number of milliseconds before a balloon help should be put up on the receiver. The balloon help will only be put up if the receiver responds to #wantsBalloon by returning true."
	^300
]

{ #category : 'halos and balloon help' }
Morph >> balloonText [
	"Answer balloon help text or nil, if no help is available.
	NB: subclasses may override such that they programatically
	construct the text, for economy's sake, such as model phrases in
	a Viewer.
	Allow for dynamic balloon text."

	^ (self valueOfProperty: #balloonText ifAbsent: nil) value.
]

{ #category : 'geometry' }
Morph >> basicDisplayBounds [
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us.
	Integer pixel coordinates!"

	self visible ifFalse: [ ^nil ].
	^ privateDisplayBounds
]

{ #category : 'accessing' }
Morph >> beSticky [
	"make the receiver sticky"
	self sticky: true
]

{ #category : 'previewing' }
Morph >> beginPreview [

	owner ifNil: [ ^self ].
	self visibleBeforePreview: self visible.
	self morphBehindBeforePreview: (self owner submorphBehind: self).
	self previewing: true.
	self show; comeToFront.
]

{ #category : 'geometry services' }
Morph >> bitMask [
	"Answer an 1bpp Form with our bitMask.
	Include all pixels drawn by any morph in our submorphs tree.
	Query with #pixelValueAt:
	Pixels drawn by us (or any submorph) are 0. Pixels not drawn by us (or any submorph) are 1.
	Remember to do `self removeProperty: #bitMask when appropriate!
	(i.e. when we change in such a way to make the bitMask invalid)
	
	Also see senders of #contour"

	| prevNotVisibleFlag bounds answer auxCanvas |

	(self valueOfProperty: #bitMask) ifNotNil: [ :bitMask |
		^bitMask ].

	"Position and scale us in order to fill required extent, but disregard any rotation.
	Do it without triggering any invalidation at all."
	prevNotVisibleFlag := self privateFlagAt: 3.
	bounds := self findFullBoundsInOwner.
	[
		answer := Form extent: bounds extent depth: 32.
		auxCanvas := VectorCanvas onForm: answer translation: bounds origin negated.
		auxCanvas engine enableTargetTranslucency.
		self privateFlagAt: 3 put: false.
		auxCanvas fullDraw: self.
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^self setProperty: #bitMask toValue:
		"Reverse it so we get bit=1 for affected pixels."
		((answer asGrayForm: 1) asFormOfDepth: 1) reverse.
]

{ #category : 'debug and other' }
Morph >> buildDebugMenu: aHand  [
	"Answer a debugging menu for the receiver.  The hand argument is seemingly historical and plays no role presently"

	| aMenu |
	self flag: #assumesKeyboardShortcuts.
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addStayUpIcons.
	self isKnownFailing
		ifTrue: [
			aMenu add: 'start drawing again' action: #resumeAfterDrawError.
			aMenu addLine].
	(self hasProperty: #errorOnStep) 
		ifTrue: [
			aMenu add: 'start stepping again' action: #resumeAfterStepError.
			aMenu addLine].
	aMenu add: 'inspect morph' action: #inspect.
	aMenu add: 'inspect owner chain' action: #inspectOwnerChain.
	self hasModel 
		ifTrue: [
			aMenu 
				add: 'inspect model'
				target: self model
				action: #inspect].
	aMenu 
		add: 'explore morph'
		target: self
		action: #explore.
	aMenu 
		add: 'copy to clipboard (c)'
		target: self
		action: #copyToClipboard.
	aMenu addLine.
	aMenu 
		add: 'browse morph class'
		target: self
		action: #browseClassHierarchy.
	self hasModel 
		ifTrue: [
			aMenu 
				add: 'browse model class'
				target: self model
				action: #browseClassHierarchy].
	aMenu addLine.
	aMenu
		add: 'edit balloon help' action: #editBalloonHelpText.
	^aMenu
]

{ #category : 'meta-actions' }
Morph >> buildHandleMenu: aHand [
	"Build the morph menu for the given morph's halo's menu handle. This menu has two sections. The first section contains commands that are interpreted by the hand; the second contains commands provided by the target morph. This method allows the morph to decide which items should be included in the hand's section of the menu."

	| menu |
	menu := MenuMorph new defaultTarget: self.
	menu addStayUpIcons.
	menu addLine.
	self addStandardHaloMenuItemsTo: menu hand: aHand.
	menu defaultTarget: aHand.
	self addAddHandMenuItemsForHalo: menu  hand: aHand.
	menu defaultTarget: self.
	self addCustomHaloMenuItems: menu hand: aHand.
	menu defaultTarget: aHand.
	^ menu

]

{ #category : 'testing' }
Morph >> canDiscardEdits [
	"Return true if this view either has no text changes or does not care."

	submorphs do: [ :m | m canDiscardEdits ifFalse: [ ^false ]].
	^ true
]

{ #category : 'menus' }
Morph >> changeColor [
	"Change the color of the receiver -- triggered, e.g. from a menu"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self world activeHand;
		target: self;
		selector: #color:;
		originalColor: self color;
		putUpFor: self near: self morphFullBoundsInWorld"
	self flag: #jmvVer2.
	self showBalloon: 'Interactive color change is currently disabled. Please use #color:'
]

{ #category : 'submorphs-add/remove' }
Morph >> clearDependencyAndEvents [
"
Morph allSubInstancesDo: [ :m | m clearDependencyAndEvents ].
"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	self removeAllActions.
]

{ #category : 'caching' }
Morph >> clearId [
	"But keep flags.
	Morph clearIds
	"

	id := id bitAnd: 255.
]

{ #category : 'events' }
Morph >> click: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a single-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."
	
	self 
		valueOfProperty: #'click:localPosition:'
		ifPresentDo: [ :handler | handler value:  aMouseButtonEvent value: localEventPosition ]
]

{ #category : 'event handling testing' }
Morph >> clickToFocusConsumesEvent [
	"Only relevant if #focusFollowsMouse == false.
	We react immediately to clicks, even if they are used to set keyboard focus on us.
	Some subclasses do not. See inheritance."
	
	^false
]

{ #category : 'geometry testing' }
Morph >> clipsSubmorphs [
	"Answer true if we clip the shape of our submorphs to our own.
	Answer true only when clipping by the canvas is needed.
	Morphs that can guarantee that submorphs don't protrude and answer false to 
	#submorphsMightProtrude should answer false for better performance.
	Note: Currently we are limited to only one clipping morph in an owner / submorph chain.
	See #clipsSubmorphsReally"

	^ false
]

{ #category : 'user interface' }
Morph >> collapse [
	"If taskbar not visible, just hide."

	self taskbar
		ifNotNil: [ :tb | tb aboutToCollapse: self ].
	self hide
]

{ #category : 'geometry services' }
Morph >> collides: aMorph [
	"Answer whether the pixels used by morphs touch at least at one place.
	Morphs not in the BoxMorph hierarchy should answer #true to #wantsContour,
	in order to use this service."

	privateDisplayBounds ifNil: [
		^false ].

	"Quick check with display bounds"
	aMorph displayFullBounds
		ifNil: [ ^false ]
		ifNotNil: [ :r |
			(self displayFullBounds
				ifNil: [ ^ false ]
				ifNotNil: [ :myFullBounds | myFullBounds intersects: r ]) ifFalse: [ ^false ]].

	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | contourTop contourBottom |
		contourTop := self valueOfProperty: #contourY0.
		contourBottom := self valueOfProperty: #contourY1.
		^ aMorph contourIntersects: contour top: contourTop bottom: contourBottom ].

	"If contour is not available, and both displayBounds overlap, answer true, as it is the best we can know."
	^ true
]

{ #category : 'accessing' }
Morph >> color [

	^ `Color blue`
]

{ #category : 'submorphs-add/remove' }
Morph >> comeToFront [

	"Restore default #layerNumber for us.
	We may still end behind other 'more topMost' morphs."
	self removeProperty: #layerNumber.
	owner privateMoveFrontMorph: self
]

{ #category : 'halos and balloon help' }
Morph >> comeToFrontAndAddHalo [
	self show.
	self comeToFront.
	self addHalo
]

{ #category : 'geometry services' }
Morph >> contourIntersects: aContourArray top: aContourTop bottom: aContourBottom [
	"Check if contours intersect.
	If contour is not available, use displayBounds.
	Not to be called directly. Pefer a higher level service. See senders."

	| contour contourTop contourBottom x0Own x1Own x0Arg x1Arg |
	contour := self valueOfProperty: #contour.	
	contourTop := (self valueOfProperty: #contourY0) ifNil: [aContourTop].
	contourBottom := (self valueOfProperty: #contourY1) ifNil: [aContourBottom].

	(contourTop max: aContourTop) to: (contourBottom min: aContourBottom) do: [ :y |
		x0Own := contour ifNil: [privateDisplayBounds left] ifNotNil: [ contour at: (y - contourTop) * 2 + 1 ].
		x1Own := contour ifNil: [privateDisplayBounds right-1] ifNotNil: [ contour at: (y - contourTop) * 2 + 2 ].
		x0Arg := aContourArray at: (y - aContourTop) * 2 + 1.
		x1Arg := aContourArray at: (y - aContourTop) * 2 + 2.
		(x0Own <= x1Arg and: [ x0Arg <= x1Own ])
			ifTrue: [ ^true ]].

	^false
]

{ #category : 'copying' }
Morph >> copy [
	"Answer a copy of the Morph.
	Copy is not in the World or any other owner."
	^ Object unStream: (ReferenceStream streamedRepresentationOf: self)
]

{ #category : 'copying' }
Morph >> copyForClipboard [
	"The answer is used both for internal (Smalltalk) and external (Host OS) clipboard.
	Some subclasses might need specific behavior."

	self okayToDuplicate ifFalse: [ ^ nil ].
	^self copy
		removeProperty: #contour;
		removeProperty: #contourY0;
		removeProperty: #contourY1;
		yourself
]

{ #category : 'meta-actions' }
Morph >> copyToClipboard: evt [
	self copyToClipboard
]

{ #category : 'geometry services' }
Morph >> coversAnyPixelCloserThan: maxDistance to: worldPoint [
	"Answer true if our closest point to worldPoint is less than aNumber pixels away.
	In target surface (i.e. Display) coordinates.
	See #bitMask.
	Remember to do
	self removeProperty: #bitMask.
	when appropriate! (i.e. when we change in such a way to make the bitMask invalid).

	Note: Requires VectorGraphics (#requiresVectorCanvas == true). Meant to be used only when really needed.
	Note: Prefer #isCloserThan:toPixel:, that doesn't use #bitMask, and doesn't require maintenance."

	| center coversIt maxDistanceSquared |
	self visible ifFalse: [
		^false ].
	privateDisplayBounds ifNil: [
		^false ].
	center := privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - worldPoint y) abs < (privateDisplayBounds height  // 2 + maxDistance) ifFalse: [
		^false ].
	(center x - worldPoint x) abs < (privateDisplayBounds width // 2 + maxDistance) ifFalse: [
		^false ].
	"Precise check with bitMask. If unavailable, just answer false. See #coversPixel:"
	coversIt := (self coversPixel: worldPoint) ifNil: [ ^false ].
	coversIt ifTrue: [ ^true ].
	maxDistanceSquared := maxDistance squared.
	maxDistance negated to: maxDistance do: [ :dy |
		maxDistance negated to: maxDistance do: [ :dx |
			dx squared + dy squared <= maxDistanceSquared ifTrue: [
				(self coversPixel: worldPoint + (dx@dy)) ifTrue: [ ^true ]]]].
	^false.
]

{ #category : 'geometry services' }
Morph >> coversPixel: worldPoint [
	"Answer true if pixel worldPoint is covered by us (or a submorph of ours), even if we are not visible a it
	because of some other morph above us also covers it.

	WARNING: Might answer nil when we simply don't know. It is generally advisable to call this method only for
	morphs where the answer is not nil:
		- BoxMorphs. No special care needed. (Senders in the base Cuis image are of this kind).
		- Morphs drawn by VectorCanvas (i.e. they #requiresVectorCanvas). See notes below.
	
	Note: If your morph #requiresVectorCanvas, and depends on this general implementation, remember to 
	`removeProperty: #bitMask` whenever it becomes invalid due to changes in your morphs. You may consider
	using #ownsPixel: if appropriate, that doesn't require any maintenance and is cheaper (in cpu and memory).
	See #bitMask.

	See #ownsPixel:
	See #includesPixel:"

	self visible ifTrue: [
		"Expensive way for morphs with arbitrary shape in VectorGraphics.
		The base Cuis System doesn't use this.
		Use this in applications if the expense of maintaining #bitMask is worth it."
		self mainCanvas ifNotNil: [ :mainCanvas |
			mainCanvas canDoVectorGraphics ifTrue: [
				^(self bitMask pixelValueAt: worldPoint - self displayFullBounds topLeft) = 1 ]]].

	"If we don't know."
	^ nil.
]

{ #category : 'accessing' }
Morph >> defaultLayerNumber [
	"Morphs with smaller layer number will always cover those with larger ones.
	Please make them natural numbers.
	See implementors."

	^100
]

{ #category : 'submorphs-add/remove' }
Morph >> delete [
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld := self world ifNil: [ self runningWorld ].
	aWorld ifNotNil: [
		aWorld activeHand ifNotNil: [ :h | h
			releaseKeyboardFocusIfOrOwnedBy: self;
			releaseMouseFocus: self ]].
	self clearDependencyAndEvents.
	self allSubmorphsDo: [ :each |
		each clearDependencyAndEvents ].
	self privateDelete.
]

{ #category : 'halos and balloon help' }
Morph >> deleteBalloon [
	"If I am showing a balloon, delete it."
	| w |
	w := self world ifNil:[^self].
	w deleteBalloonTarget: self.
]

{ #category : 'events' }
Morph >> deliverKeyboardShortcut: aKeyboardEvent [
	"aKeyboardEvent is keyboard shortcut command.
	Just handle it if it is appropriate for us.
	Otherwise, let a willing owner handle it."

	(self keyboardShortcut: aKeyboardEvent) ifTrue: [
		^true].
	owner ifNotNil: [
		^owner deliverKeyboardShortcut: aKeyboardEvent ].
	^false.
]

{ #category : 'meta-actions' }
Morph >> dismissMorph [
	| w |
	w := self world ifNil: [ ^self ].
	w deleteAllHalos; stopSteppingMorph: self.
	self delete
]

{ #category : 'submorphs-add/remove' }
Morph >> dismissViaHalo [
	"The user has clicked in the delete halo-handle.  This provides a hook in case some concomitant action should be taken, or if the particular morph is not one which should be put in the trash can, for example."

	^ self dismissMorph
]

{ #category : 'geometry' }
Morph >> displayBounds [
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us.
	Integer pixel coordinates!"

	| answer |	
	answer := self basicDisplayBounds.
	answer ifNil: [ ^nil ].
	answer hasPositiveExtent ifTrue: [ ^answer ].
	^self displayFullBounds.
]

{ #category : 'geometry' }
Morph >> displayBoundsForHalo [
	"Answer the rectangle to be used as the inner dimension of my halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	^ (Preferences at: #haloEnclosesFullBounds)
		ifFalse: [ self displayBounds ]
		ifTrue: [ self displayFullBounds ]
]

{ #category : 'geometry' }
Morph >> displayFullBounds [
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Integer pixel coordinates!"

	| answer |
	answer := self basicDisplayBounds.
	(self submorphsMightProtrude and: [self clipsSubmorphs not]) ifTrue: [
		self submorphsDo: [ :m |
			answer := answer
				ifNil: [m displayFullBounds]
				ifNotNil: [answer quickMerge: m displayFullBounds]]].
	^answer
]

{ #category : 'testing' }
Morph >> disregardUnacceptedEdits [
	"Return true if this view either has no text changes or does not care."

	submorphs do: [ :m | m disregardUnacceptedEdits ].
]

{ #category : 'events' }
Morph >> doubleClick: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a double-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'doubleClick:localPosition:'
		ifPresentDo: [ :handler | handler value:  aMouseButtonEvent value: localEventPosition ]
]

{ #category : 'dropping/grabbing' }
Morph >> dragCancel [
	"This message is sent if the user presses ESC while the hand is dragging this morph, to give this morph an opportunity to cancel the dragging. By default do nothing.
	For example, a morph might want to handle this by deleting itself from the hand, or by returning to its original owner and position."
	^ self
]

{ #category : 'events' }
Morph >> dragEnter: aMouseEnterEvent [
	"Handle a mouse enter event, meaning the mouse just entered the bounds of this morph while dragging something. See also #mouseEnter: and #handlesDragOver:."
	self 
		valueOfProperty: #dragEnter: 
		ifPresentDo: [ :handler | handler value: aMouseEnterEvent ]
]

{ #category : 'events' }
Morph >> dragEvent: aMouseEvent localPosition: aPoint [

	aMouseEvent hand halo: nil.
	aMouseEvent hand grabMorph: self
]

{ #category : 'events' }
Morph >> dragHover: aMouseOverEvent localPosition: localEventPosition [
	"Handle a mouse move event while the hand is dragging something. See also #mouseHover:localPosition: and #handlesDragHover."

	self
		valueOfProperty: #dragHover:localPosition:
		ifPresentDo: [ :handler |
			handler
				value: aMouseOverEvent
				value: localEventPosition ].
]

{ #category : 'events' }
Morph >> dragLeave: aMouseLeaveEvent [
	"Handle a mouse leave event, meaning the mouse just left the bounds of this morph while dragging something. See also #mouseLeave: and #handleDragOver:."
	self 
		valueOfProperty: #dragLeave: 
		ifPresentDo: [ :handler | handler value: aMouseLeaveEvent ].
]

{ #category : 'drawing' }
Morph >> drawOn: aCanvas [
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: `-75 @ -70 corner: 75 @ 70`
		color: `Color blue`
]

{ #category : 'drawing' }
Morph >> drawingFails [
	self world ifNotNil: [ :w | w addKnownFailing: self ].
	self redrawNeeded.
]

{ #category : 'drawing' }
Morph >> drawingFailsNot [
	self world removeKnownFailing: self.
	self redrawNeeded.

]

{ #category : 'testing' }
Morph >> drawsKeyboardFocusIndicator [

	^false
]

{ #category : 'dropping/grabbing' }
Morph >> dropFiles: aDropFilesEvent [

	"I do nothing, subclasses should redefine if they handle this event - Hernan"
]

{ #category : 'copying' }
Morph >> duplicate [
	"Make and return a duplicate of the receiver"

	| newMorph |
	self okayToDuplicate ifFalse: [^ self].
	newMorph := self copy.

	newMorph wantsSteps ifTrue: [ newMorph startStepping ].

	^ newMorph
]

{ #category : 'meta-actions' }
Morph >> duplicateMorph: evt [
	"Make and return a duplicate of the receiver's argument"

	^self duplicate
]

{ #category : 'halos and balloon help' }
Morph >> editBalloonHelpContent: aString [
	self
		request: 'Edit the balloon help text for ' , self shortPrintString
		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString])
		do: [:reply|
			(reply isEmpty or: [reply asPlainString = self noHelpString])
				ifTrue: [self setBalloonText: nil]
				ifFalse: [self setBalloonText: reply]]
]

{ #category : 'halos and balloon help' }
Morph >> editBalloonHelpText [
	"Modify the receiver's balloon help text."

	self editBalloonHelpContent: self balloonText
]

{ #category : 'user interface' }
Morph >> embeddedInMorphicWindowLabeled: labelString  [
	| window |
	window := SystemWindow new.
	window setLabel: labelString.
	window layoutMorph addMorph: self proportionalHeight: 1.
	^window
]

{ #category : 'previewing' }
Morph >> endPreview [

	self previewing ifTrue: [
		owner notNil ifTrue: [
			self visible: self visibleBeforePreview.
			self morphBehindBeforePreview ifNotNil: [ :m |
				owner addMorph: self inFrontOf: m ].
			self morphBehindBeforePreview: nil ].
		self previewing: false ]
]

{ #category : 'geometry' }
Morph >> ensureMinimumExtent [
	"We can't change extent. NOP here.
	See inheritance."
]

{ #category : 'menus' }
Morph >> expand [
	
	self show.
	self comeToFront
]

{ #category : 'menus' }
Morph >> exportAsBMP [
	"Export the receiver's image as a BMP."
	self request: 'Enter file name'
		initialAnswer: (self printStringLimitedTo: 20),'.bmp'
		do: [:aString| (self imageForm: 32) writeBMPfileNamed: aString]
]

{ #category : 'menus' }
Morph >> exportAsJPEG [
	"Export the receiver's image as a JPEG."
	self request: 'Enter file name'
		initialAnswer: (self printStringLimitedTo: 20),'.jpeg'
		do: [:aString| (self imageForm: 32) writeJPEGfileNamed: aString]
]

{ #category : 'geometry' }
Morph >> extentInOwner [

	self morphExtent ifNotNil: [ :e |
		^ self externalizeDistance: e ].
	^self fullBoundsInOwner extent
]

{ #category : 'geometry' }
Morph >> externalize: aPoint [
	"aPoint is in own coordinates. Answer is in owner's coordinates."

	^ aPoint.
]

{ #category : 'geometry' }
Morph >> externalizeBoundingRectOf: aRectangle [

	^aRectangle
]

{ #category : 'geometry' }
Morph >> externalizeBoundsToWorld: aRectangle [
	"aRectangle is in own coordinates. Answer is in world coordinates."

	^owner
		ifNotNil: [ owner externalizeBoundsToWorld: aRectangle ]
		ifNil: [ aRectangle ]
]

{ #category : 'geometry' }
Morph >> externalizeDistance: aPoint [
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	^ aPoint
]

{ #category : 'geometry' }
Morph >> externalizeDistanceToWorld: aPoint [
	"aPoint is a delta in own coordinates. Answer is in world coordinates."

	| inOwners |
	inOwners := self externalizeDistance: aPoint.
	^owner
		ifNotNil: [ owner externalizeDistanceToWorld: inOwners ]
		ifNil: [ inOwners ]
]

{ #category : 'geometry' }
Morph >> externalizeToWorld: aPoint [
	"aPoint is in own coordinates. Answer is in world coordinates."

	| inOwners |
	inOwners := self externalize: aPoint.
	^owner
		ifNotNil: [ owner externalizeToWorld: inOwners ]
		ifNil: [ inOwners ]
]

{ #category : 'submorphs-accessing' }
Morph >> findDeepSubmorphThat: block1 ifAbsent: block2  [
	self
		allMorphsDo: [:m | (block1 value: m)
				== true ifTrue: [^ m]].
	^ block2 value
]

{ #category : 'geometry' }
Morph >> findFullBoundsInOwner [
	"Find and answer full bounds in whatever owner.
	These might not equal #displayFullBounds at all!
	Note: Answers an integer rectangle"

	| prevNotVisibleFlag w c answer prevOwner |
	(owner notNil and: [owner isWorldMorph]) ifTrue: [
		w := owner ]
	ifFalse: [
		w := UISupervisor ui ].
	c := w mainBoundsFinderCanvas.
	"Hack owner so #displayBoundsSetFrom: will update privateDisplayBounds"
	prevOwner := owner.
	owner := w.
	prevNotVisibleFlag := self privateFlagAt: 3.
	self privateFlagAt: 3 put: false.
	c fullUpdateProtrudingBounds: self.
	answer := self displayFullBounds.
	"Reset owner and privateDisplayBounds (if needed) so no one finds out what we've just done!"
	prevOwner == owner ifFalse: [
		owner := prevOwner.
		self world = w ifTrue: [
			self privateFlagAt: 3 put: false.
			self allOwnersReverseDo: [ :m | c into: m ].
			c fullUpdateProtrudingBounds: self.
			self allOwnersDo: [ :m | c outOfMorph ]]].
	self privateFlagAt: 3 put: prevNotVisibleFlag.
	^answer origin corner: answer corner -(1@0).
]

{ #category : 'submorphs-accessing' }
Morph >> findSubmorphBinary: aBlock [
	"Use binary search for finding a specific submorph of the receiver. Caller must be certain that the ordering holds for the submorphs."
	^submorphs findBinary: aBlock do: [ :found | found ] ifNone: [ :a :b | ]
]

{ #category : 'structure' }
Morph >> firstOwnerSuchThat: conditionBlock [

	self allOwnersDo: [:m | (conditionBlock value: m) ifTrue: [^ m]].
	^ nil

]

{ #category : 'submorphs-accessing' }
Morph >> firstSubmorph [
	^submorphs first
]

{ #category : 'user interface' }
Morph >> flash [

	self displayBounds ifNotNil: [ :r |
		Display flash: r ]
]

{ #category : 'user interface' }
Morph >> flashWith: aColor [

	self displayBounds ifNotNil: [ :r | Display flash: r with: aColor ]
]

{ #category : 'events-processing' }
Morph >> focusKeyboardFor: aKeyboardEvent [

	"For most widgets, tab / shift-tab control focus. For those that not, ctrl-tab / ctrl-shift-tab is an alternative."
	aKeyboardEvent isTab ifTrue:
		[
			(self tabToFocus or: [aKeyboardEvent controlKeyPressed]) ifTrue:
				[
					aKeyboardEvent shiftPressed
						ifFalse: [ aKeyboardEvent hand keyboardFocusNext ]
						ifTrue: [ aKeyboardEvent hand keyboardFocusPrevious ].
					^ true ]].

	"Cycle through windows with cmdAlt + < and cmdAlt + >.
	VM and platform peculiarities are hidden in #isCmdAltLessThan and #isCmdAltGreaterThan"
	"This was done as an attempt to mimic the Mac OSX keystrokes for 'Move focus to next window in active application'. Unfortunately, it only works if OS X is set to use any other keys for this. If (as for example, with German defaults), OS-X uses these keystrokes, then they are not sent to the VM. This is a long standing issues in Chromium and PhotoShop, for example..."
	self disableCode: [
		aKeyboardEvent isCmdAltLessThan ifTrue: [
			aKeyboardEvent hand activatePreviousWindow.
			^true ].
		aKeyboardEvent isCmdAltGreaterThan ifTrue: [
			aKeyboardEvent hand activateNextWindow.
			^true ]].
	"Alternative for Mac OS-X: option-Tab and option-shift-Tab"
	(aKeyboardEvent isTab and: [ aKeyboardEvent rawMacOptionKeyPressed ])
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifTrue: [ aKeyboardEvent hand activatePreviousWindow ]
				ifFalse: [ aKeyboardEvent hand activateNextWindow ].
			^ true ].
	"Alternative for non-Mac OS-X: alt-< and alt->"
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter == $< ]) ifTrue: [
		aKeyboardEvent hand activatePreviousWindow.
		^true ].
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter == $> ]) ifTrue: [
		aKeyboardEvent hand activateNextWindow.
		^true ].
	^false
]

{ #category : 'geometry' }
Morph >> fontPreferenceChanged [
	"Preferred fonts scale a number of window relations.
	Let morphs which rely on this updte themselves.
	
	Note that the fontPreferenceChanged message is typically
	sent to the current world.  As WorldMorph inherits from me
	the code below works fine for this."
	
	"I  do nothing myself but my submorphs may."
	
	self submorphsDo: [ :m | m fontPreferenceChanged. ]
]

{ #category : 'geometry' }
Morph >> fullBoundsInOwner [
	"Answer fullBoundsInOwner. Cache the found bounds.
	No special care needed for property #fullBoundsInOwner. Cache is invalidated when appropriate.
	Note: Answers an integer rectangle"

	(self valueOfProperty: #fullBoundsInOwner) ifNotNil: [ :fullBoundsInOwner |
		^fullBoundsInOwner ].

	^self setProperty: #fullBoundsInOwner toValue: self findFullBoundsInOwner.
]

{ #category : 'geometry services' }
Morph >> fullIncludesPixel: worldPoint [
	"Answer true if worldPoint is in some submorph, even if not inside our shape."

	(self includesPixel: worldPoint) ifTrue: [ ^ true ].
	self submorphsDo: [ :m |
		(m fullIncludesPixel: worldPoint) ifTrue: [ ^ true ]].
	^ false.
]

{ #category : 'accessing - flags' }
Morph >> fullRedrawNotNeeded [
	"Clear redraw flags for receiver and all submorphs (but only if set!)"

	self isSubmorphRedrawNeeded ifTrue: [
		self submorphsDo: [ :m |
			m fullRedrawNotNeeded ]].

	"Equivalent to
	self needsRedraw: false.
	self submorphNeedsRedraw: false.
	"
	id := id bitAnd: `3 bitInvert`
]

{ #category : 'caching' }
Morph >> fullReleaseCachedState [
	"Release the cached state of the receiver and its full submorph tree."

	self allMorphsDo: [:m | m releaseCachedState].

]

{ #category : 'geometry services' }
Morph >> fullyCoveredRectangle [
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is correct, although simplistic and uninteresting.
	Subclasses may implement smarter behavior, allowing for optimizations of update operations.
	The answer should be the largest fully covered rectangle we can know cheaply."

	^nil
]

{ #category : 'events-processing' }
Morph >> getKeyboardFocus [
	self world activeHand newKeyboardFocus: self
]

{ #category : 'submorphs-menus' }
Morph >> getMenu [
	"Answer the right-click (contextual) menu."
	"Spec for the menu and shortcut keys.
	Each entry is
		#(itemId 			'menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'helpText(optional)'    selectorArgument(optional)	enabledSelector)
		- itemId is a number (integer or float). It specifies the position of the item in the menu, and is also an id, allowing for overrides and deletions.
		- label is a string or symbol. Symbol means self updating menu entry. nil means just keyboard shortcut, no menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Typically #window, #model or #editor. If nil, receiver is the widget itself.
		- selector is #aSymbol. Usually no arguments, or one single argument in 8th entry. 
			If a Number (multiple of 1000), indicates a subMenu. ItemsIds must be in range. See BrowserWindow>>#classListMenuSpec.
			If nil, the menu entry is created but disabled. The keyboard shortcut is disabled too.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
		- selectorArgument is optional. If selector takes an argument, and selectorArgument is not provided, the argument is self.
	- enabledSelector is an optional symbol. The result of sending it to target will be used to enable or disable the menu item.
	Entries of size 1 mean 'remove menu item'.
	Entries of size 2 are separators.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"

	| menuNshortcutsSpecs itemSpec menu submenus |
	menuNshortcutsSpecs := self allMenuNshortcutsSpecs.
	menuNshortcutsSpecs ifEmpty: [ ^nil ].
	menu := MenuMorph new.
	submenus := nil.
	menuNshortcutsSpecs keys sorted do: [ :key |
		itemSpec := menuNshortcutsSpecs at: key.
		itemSpec size =1 ifTrue: [
				"Do nothing. This is a marker for removing a previously provided entry" ]
			ifFalse: [ itemSpec size = 2 ifTrue: [
				itemSpec second first == $X ifFalse: [ "Compatibility. Also a marker for removing previous entries."
					self addLineTo: menu fromSpec: itemSpec submenus: submenus ]]
			ifFalse: [ itemSpec size = 3 ifTrue: [
				self addTitleTo: menu fromSpec: itemSpec ]
			ifFalse: [ itemSpec fifth isNumber ifTrue: [
				submenus ifNil: [submenus := Dictionary new ].
				self addSubmenuTo: menu fromSpec: itemSpec submenus: submenus ]
			ifFalse: [
				self addItemTo: menu fromSpec: itemSpec submenus: submenus ]]]]].
	^ menu
]

{ #category : 'submorphs-add/remove' }
Morph >> goBehind [
	| regularLayerNumber |
	"Set our #layerNumber to the default for most morphs, allowing us
	to go behind them."
	regularLayerNumber  := 100.
	self layerNumber = regularLayerNumber ifFalse: [
		self setProperty: #layerNumber toValue: regularLayerNumber ].
	owner privateMoveBackMorph: self.

]

{ #category : 'halos and balloon help' }
Morph >> halo [
	self world ifNotNil: [ :w |
		w haloMorphsDo: [ :h |
			h target == self ifTrue: [^ h]]].
	^ nil
]

{ #category : 'halos and balloon help' }
Morph >> haloRedrawMayBeNeeded [
	self world ifNotNil: [ :w |
		w haloMorphsDo: [ :h |
			h target ifNotNil: [ :t |
				t haloShowsCoordinateSystem ifTrue: [
					self withAllOwnersDo: [ :m |
						m == t ifTrue: [ h redrawNeeded ]]]]]]
]

{ #category : 'halos and balloon help' }
Morph >> haloShowsCoordinateSystem [

	^Preferences at: #halosShowCoordinateSystem
]

{ #category : 'halos and balloon help' }
Morph >> haloSpecifications [

	^ Preferences at: #haloSpecifications
]

{ #category : 'events-processing' }
Morph >> handleMouseFocusEvent: aMouseEvent [
	"Handle the given event.
	This message is sent if the receiver currently has the mouse focus
	and is therefore receiving events directly from some hand,
	without going through #dispatchIn:."

	aMouseEvent sendEventTo: self.
]

{ #category : 'event handling testing' }
Morph >> handlesDragHover [
	"Do I want to receive unhandled mouseMove events when the hand is dragging something?  The default response is false."
	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #handlesDragHover.
]

{ #category : 'event handling testing' }
Morph >> handlesDragOver: aMouseEvent [
	"Do I want to receive dragEnter: and dragLeave: when the hand is dragging something?  The default response is false." 
	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesDragOver:'
]

{ #category : 'event handling testing' }
Morph >> handlesKeyboardFocus [
	"Return true if the receiver wishes to handle keyboard focus"

	^true
]

{ #category : 'event handling testing' }
Morph >> handlesMouseDown: aMouseButtonEvent [
	"Do I want to receive mouseButton messages ?
	- #mouseButton1Down:localPosition:
	- #mouseButton1Up:localPosition:
	- #mouseButton2Down:localPosition:
	- #mouseButton2Up:localPosition:
	- #mouseButton3Down:localPosition:
	- #mouseButton3Up:localPosition:
	- #mouseMove:localPosition:
	- #mouseButton2Activity
	
	NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true.
	
	Implementors could query the argument, and only answer true for (for example) button 2 up only."

	"Use a property test to allow individual instances to dynamically specify this."
	^ self hasProperty: #'handlesMouseDown:'
]

{ #category : 'event handling testing' }
Morph >> handlesMouseHover [
	"Do I want to receive unhandled mouseMove events when the button is up and the hand is empty?  The default response is false."
	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #handlesMouseHover.
]

{ #category : 'event handling testing' }
Morph >> handlesMouseOver: aMouseEvent [
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?  The default response is false." 

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesMouseOver:'
]

{ #category : 'event handling testing' }
Morph >> handlesMouseScroll: aMouseScrollEvent [
	^ self hasProperty: #'handlesMouseScroll:'
]

{ #category : 'event handling testing' }
Morph >> handlesMouseStillDown: evt [
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesMouseStillDown:'
]

{ #category : 'focus handling' }
Morph >> hasKeyboardFocus [

	self world ifNotNil: [ :w |
		w activeHand ifNotNil: [ :h |
			^ h keyboardFocus == self ]].
	^ false
]

{ #category : 'testing' }
Morph >> hasModel [
	^false
]

{ #category : 'focus handling' }
Morph >> hasMouseFocus [

	self world ifNotNil: [ :w |
		w activeHand ifNotNil: [ :h |
			^ h mouseFocus == self ]].
	^ false
]

{ #category : 'geometry testing' }
Morph >> hasOwnLocation [
	^false
]

{ #category : 'structure' }
Morph >> hasOwner: aMorph [
	"Return true if the receiver has aMorph in its owner chain"
	aMorph ifNil:[^true].
	self allOwnersDo:[:m| m = aMorph ifTrue:[^true]].
	^false
]

{ #category : 'accessing - properties' }
Morph >> hasProperty: aSymbol  [
	"Answer whether the receiver has the property named aSymbol"
	properties ifNil: [ ^false ].
	^properties includesKey: aSymbol
]

{ #category : 'submorphs-accessing' }
Morph >> hasSubmorphs [
	^submorphs size ~= 0
]

{ #category : 'geometry testing' }
Morph >> hasVariableExtent [
	^false
]

{ #category : 'drawing' }
Morph >> hide [
	self visible: false
]

{ #category : 'accessing - flags' }
Morph >> highlighted: aBoolean [

	self privateFlagAt: 5 put: aBoolean.
	self redrawNeeded
]

{ #category : 'testing' }
Morph >> honorsLayerOrder [
	"Most morphs don't go through the expense of the needed resorting."

	^false
]

{ #category : 'drawing' }
Morph >> icon [
	^ (self imageForm: 16@16 depth: 32)
		ifNil: [ Theme current morphsIcon ]
]

{ #category : 'drawing' }
Morph >> imageForm [
	^self imageForm: 32
]

{ #category : 'drawing' }
Morph >> imageForm: depth [

	^self imageForm: nil depth: depth.
]

{ #category : 'drawing' }
Morph >> imageForm: extentOrNil depth: depth [

	| answerExtent answer auxCanvas bounds |
	self ensureMinimumExtent.

	self requiresVectorCanvas ifTrue: [	
		^ self imageFormVG: extentOrNil depth: depth ].

	bounds := self fullBoundsInOwner.
	answerExtent := bounds extent.
	extentOrNil ifNotNil: [ answerExtent := answerExtent min: extentOrNil * 4 ].
	auxCanvas := MorphicCanvas depth: depth over: (bounds origin extent: answerExtent ceiling).
	[auxCanvas fullDraw: self] on: Error do: [auxCanvas form fillColor: Color red].
	answer := auxCanvas form.
	"answer divideByAlpha."
	"answer makeAllPixelsOpaque."
	answer fixAlpha.
	extentOrNil ifNotNil: [ answer := answer magnifyTo: extentOrNil ].
	^answer
]

{ #category : 'drawing' }
Morph >> imageFormVG: extentOrNil depth: depth [
	"Draw using a VectorCanvas
	Scale as needed. Draw unrotated."

	| prevNotVisibleFlag bounds extent fauxLocation answer auxCanvas |
	"Position and scale us in order to fill required extent, but disregard any rotation.
	Do it without triggering any invalidation at all."
	prevNotVisibleFlag := self privateFlagAt: 3.
	[
		bounds := self findFullBoundsInOwner.
		extentOrNil
			ifNotNil: [ | scale |
				extent := extentOrNil.
				scale := extent x asFloat / bounds width min: extent y asFloat / bounds height.
				fauxLocation := AffineTransformation withScale: scale position: bounds origin negated +1 * scale ]
			ifNil: [
				extent := bounds extent.
				fauxLocation := MorphicTranslation withTranslation: bounds origin negated ].
		answer := Form extent: extent depth: 32.
		auxCanvas := VectorCanvas onForm: answer.
		auxCanvas engine enableTargetTranslucency.
		auxCanvas geometryTransformation: fauxLocation.
		depth = 32 ifFalse: [
			"Only 32 bpp can hold translucent anti aliasing over transparent background"
			answer fillColor: Color veryLightGray ].
		self privateFlagAt: 3 put: false.
		[auxCanvas fullDraw: self] on: Error do: [auxCanvas form fillColor: Color red].
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^answer asFormOfDepth: depth.
]

{ #category : 'geometry services' }
Morph >> includesPixel: worldPoint [
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:

	- Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.

	- Some implementations (BoxMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	- For other morphs, not BoxMorph, (i.e. they run this implementation), if VectorGraphics is not active,
	answer true for any point that lies within our bounds. If this is the case, consider using VectorGraphics: In addition to
	many other advantages, this method will always answer a strictly correct answer.

	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph
	covers us. A common case is to find the topmost morph at some position. In that case, iterating front to back ensures
	that if any other morph covers us, it is found first.

	Note: This method is redefined by some subclasses. This implementation is only used for morphs drawn by
	VectorCanvas, either because the main canvas is a VectorCanvas, or because it is a HybridCanvas and we answer
	true to #requiresVectorCanvas.
	(See other implementors)

	Also see #ownsPixel:, #coversPixel:
	Also see #fullIncludesPixel:, #topMorphAt:, #topUnlockedMorphAt:"

	"Check if #ownsPixel:, if possible."
	self mainCanvas ifNotNil: [ :mainCanvas |
		mainCanvas canDoVectorGraphics ifTrue: [
			^ (mainCanvas morphIdAt: worldPoint) = self morphId ]].

	^ false.
]

{ #category : 'initialization' }
Morph >> initialize [
	"initialize the state of the receiver"

	owner := nil.
	submorphs := #().
	id := 0.
]

{ #category : 'debug and other' }
Morph >> inspectOwnerChain [
	self ownerChain inspectWithLabel: 'Owner chain for ', self printString
]

{ #category : 'geometry' }
Morph >> internalize: aPoint [
	"aPoint is in owner's coordinates. Answer is in own coordinates."

	^ aPoint
]

{ #category : 'geometry' }
Morph >> internalizeDistance: aPoint [
	"aPoint is a delta in owner's coordinates. Answer is in own coordinates."

	^ aPoint
]

{ #category : 'geometry' }
Morph >> internalizeDistanceFromWorld: aPoint [
	"aPoint is a delta in World coordinates. Answer is in own coordinates."

	| inOwners |
	inOwners := owner
		ifNotNil: [ owner internalizeDistanceFromWorld: aPoint ]
		ifNil: [ aPoint ].
	^self internalizeDistance: inOwners
]

{ #category : 'geometry' }
Morph >> internalizeFromWorld: aPoint [
	"aPoint is in World coordinates. Answer is in own coordinates."

	| inOwners |
	inOwners := owner
		ifNotNil: [ owner internalizeFromWorld: aPoint ]
		ifNil: [ aPoint ].
	^self internalize: inOwners
]

{ #category : 'initialization' }
Morph >> intoWorld: aWorld [
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."

	aWorld ifNil: [ ^self ].
	self needsRedraw: true.
	self wantsSteps ifTrue: [ self startStepping ].
	self submorphsDo: [ :m | m intoWorld: aWorld ].
]

{ #category : 'events-processing' }
Morph >> invalidDrop: aDropEvent [
	"A drag and drop operation was requested, but it was not accepted.
	Some morphs may want to do somthing in this case. Let them."
]

{ #category : 'updating' }
Morph >> invalidateBounds [
	"Report that the area occupied by this morph and submorphs should be redrawn, 
	but most likely we are not going to be redrawn on it, or maybe we'll not be fully visible. See senders."

	self displayFullBounds ifNotNil: [ :r |
		self invalidateDisplayRect: r for: nil ].

]

{ #category : 'change reporting' }
Morph >> invalidateDisplayRect: damageRect for: aMorph [
	"
	If we clip submorphs, then we clip damageRect.
	
	aMorph is the morph that changed and therefore should be redrawn. In some cases, damage reporting is done by no longer occupying some area, and requesting whatever appropriate morph to be drawn there. In such cases, aMorph should be nil. See senders."

	| clippedRect b |
	self visible ifFalse: [ ^self].

	clippedRect := damageRect.
	aMorph == self ifFalse: [
		self clipsSubmorphs ifTrue: [
			b := self displayBounds.
			b ifNil: [ ^self ].
			clippedRect := damageRect intersect: b ]].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect for: aMorph ].
]

{ #category : 'change reporting' }
Morph >> invalidateLocalRect: localRectangle [

	self
		invalidateDisplayRect:
			(self externalizeBoundsToWorld: localRectangle)
				encompassingIntegerRectangle
		for: self.
]

{ #category : 'testing' }
Morph >> is: aSymbol [
	^ aSymbol == #Morph or: [ super is: aSymbol ]
]

{ #category : 'geometry services' }
Morph >> isCloserThan: maxDistance toPixel: worldPoint [
	"Answer true if our closest point to aPoint is less than aNumber pixels away.
	In target surface (i.e. Display) coordinates.
	Uses precise testing of the morph contour if available:
	Morphs not in the BoxMorph hierarchy should answer #true to #wantsContour in order to use this service.
	Note: Considers only the external border. Any inner pixel is considered 'inside' regardless of us being transparent there.
	Note: Cheaper than #coversAnyPixelCloserThan:to: . Doesn't use #bitMask. Doesn't require maintenance."

	| center contourTop contourBottom |
	privateDisplayBounds ifNil: [
		^false ].
	center := privateDisplayBounds center.
	"Quick checks: If not even within aNumber distance to display bounds, fail"
	(center y - worldPoint y) abs < (privateDisplayBounds height  // 2 + maxDistance) ifFalse: [
		^false ].
	(center x - worldPoint x) abs < (privateDisplayBounds width // 2 + maxDistance) ifFalse: [
		^false ].
	"Precise check with contour, if available"
	(self valueOfProperty: #contour) ifNotNil: [ :contour | | y0 y1 x0 x1 |
		contourTop := self valueOfProperty: #contourY0.
		contourBottom := self valueOfProperty: #contourY1.
		"Contour rows to consider are those within requested distance."
		y0 := worldPoint y - maxDistance max: contourTop.
		y1 := worldPoint y + maxDistance min: contourBottom.
		y0 to: y1 do: [ :y |
			x0 := (contour at: (y - contourTop) * 2 + 1) - maxDistance.
			x1 := (contour at: (y - contourTop) * 2 + 2) + maxDistance.
			"If a vertical line of 2*aNumber height centered on aPoint is inside the contour, quick exit"
			(worldPoint x between: x0 and: x1) ifTrue: [ ^true ].
			"Check if aPoint is close enough to contour"
			(x0@y - worldPoint) r < maxDistance ifTrue: [ ^true ].
			(x1@y - worldPoint) r < maxDistance ifTrue: [ ^true ]].
		"Not inside, not close enough to contour"
		^ false ].
	"If contour is not available, and aPoint is close enough to displayBounds, answer true, as it is the best we can know."
	^ true
]

{ #category : 'testing' }
Morph >> isCollapsed [

	^ self visible not
]

{ #category : 'accessing - flags' }
Morph >> isHighlighted [

	^ self privateFlagAt: 5
]

{ #category : 'structure' }
Morph >> isInWorld [
	"Return true if this morph is in a world."

	^self world notNil
]

{ #category : 'testing' }
Morph >> isIncludedInTaskbar [
	"Answer true a button for us should be added to any TaskbarMorph."

	^ true
]

{ #category : 'drawing' }
Morph >> isKnownFailing [
	| w |
	w := self world.
	^w notNil and: [ w isKnownFailing: self ]
]

{ #category : 'accessing - flags' }
Morph >> isLayoutNeeded [

	^ self privateFlagAt: 4
]

{ #category : 'accessing - properties' }
Morph >> isLocked [
	"Answer whether the receiver is Locked.
	The idea is that a locked morph behaves as if it wasn't a separate object,
	but just part of the #drawOn: method in the owner. Locked morphs receive no events."

	^ self valueOfProperty: #locked ifAbsent: [ false ]
]

{ #category : 'geometry testing' }
Morph >> isOrAnyOwnerIsRotated [
	"True if we or any owner is rotated."

	^ false
]

{ #category : 'testing' }
Morph >> isOwnedByHand [
	^owner is: #HandMorph
]

{ #category : 'testing' }
Morph >> isOwnedByWorld [
	^owner isWorldMorph
]

{ #category : 'testing' }
Morph >> isOwnedByWorldOrHand [
	"I.e. are we a top morph?"

	^ self isOwnedByWorld or: [ self isOwnedByHand ]
]

{ #category : 'testing' }
Morph >> isPlacedMorph [
	"Meaning that:
	- we can moved around by the user or by code
	- we can be scaled
	- maybe we can be resized
	This all means, for instance, that we can be added to LayoutMorphs."

	^false
]

{ #category : 'testing' }
Morph >> isProportionalHeight [
	"Answer true if I have a layoutSpec which specifies a proportional layout"

	^false
]

{ #category : 'testing' }
Morph >> isProportionalWidth [
	"Answer true if I have a layoutSpec which specifies a proportional layout"

	^ false
]

{ #category : 'testing' }
Morph >> isReallyUnlocked [
	"Answer true only if none in the owner chain is locked (i.e. if we are really unlocked!)"
	^self isLocked not and: [ owner isReallyUnlocked ]
]

{ #category : 'testing' }
Morph >> isReallyVisible [
	"Answer true only if all the owner chain is visible (i.e. if we are really visible!)"
	^self visible and: [ owner isReallyVisible ]
]

{ #category : 'accessing - flags' }
Morph >> isRedrawNeeded [

	^ self privateFlagAt: 1
]

{ #category : 'accessing - properties' }
Morph >> isSticky [
	"Answer whether the receiver is Sticky.
	A morph that is made sticky can not be easily grabbed with the hand."

	^ self valueOfProperty: #sticky ifAbsent: [ false ]
]

{ #category : 'accessing - flags' }
Morph >> isSubmorphRedrawNeeded [

	^ self privateFlagAt: 2
]

{ #category : 'classification' }
Morph >> isWorldMorph [

	^ false
]

{ #category : 'dropping/grabbing' }
Morph >> justDroppedInto: newOwnerMorph event: aDropEvent  [
	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"

	newOwnerMorph activateWindow
]

{ #category : 'dropping/grabbing' }
Morph >> justGrabbedFrom: formerOwner [
	"The receiver was just grabbed from its former owner and is now attached to the hand."
]

{ #category : 'events' }
Morph >> keyDown: aKeyboardEvent [
	"Handle a key down event. The default response is to do nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyDown:' 
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ]
]

{ #category : 'events' }
Morph >> keyStroke: aKeyboardEvent [
	"Handle a keystroke event.
	Subclasses should call super, and check if the event was handled."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [
			aKeyboardEvent wasHandled: true.
			^self ].

	aKeyboardEvent anyModifierKeyPressed ifTrue: [
		(self deliverKeyboardShortcut: aKeyboardEvent)
			ifTrue: [ aKeyboardEvent wasHandled: true ]].
]

{ #category : 'events' }
Morph >> keyUp: aKeyboardEvent [
	"Handle a key up event. The default response is to do nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyUp:'
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ]
]

{ #category : 'focus handling' }
Morph >> keyboardFocusChange: aBoolean [
	"The message is sent to a morph when its keyboard focus change. The given argument indicates that the receiver is gaining keyboard focus (versus losing) the keyboard focus. Morphs that accept keystrokes should change their appearance in some way when they are the current keyboard focus. This default implementation does nothing."
]

{ #category : 'focus handling' }
Morph >> keyboardFocused [
	"Just given focus via a keyboard event (not a MouseMove or MouseClick).
	See also #keyboardUnfocused"
]

{ #category : 'events' }
Morph >> keyboardShortcut: aKeyboardEvent [
	"Handle a keyboard shortcut.
	See coments at Morph >> #getMenu and Morph >> #allMenuNshortcutsSpecs
	"
	| menuNshortcutsSpecs shortcutCharacter target argument |
	menuNshortcutsSpecs := self allMenuNshortcutsSpecs.
	menuNshortcutsSpecs ifEmpty: [ ^false ].
	"Pick the last shortcut definition. If they are repeated, this is the last override."
	menuNshortcutsSpecs keys sorted reverseDo: [ :key | | itemSpec |
		itemSpec := menuNshortcutsSpecs at: key.
		itemSpec size >= 5 ifTrue: [
			shortcutCharacter := itemSpec third.
			shortcutCharacter = aKeyboardEvent keyCharacter ifTrue: [
				target := itemSpec fourth
					ifNotNil: [ :targetOrGetter |
						targetOrGetter isSymbol
							ifTrue: [ 	self perform: targetOrGetter ]
							ifFalse: [ targetOrGetter ]]
					ifNil: [ self ].
				
				itemSpec fifth
					ifNotNil: [ :selector | | enabledSelector |
						
						enabledSelector := itemSpec size >= 9 ifTrue: [ itemSpec ninth ].
						(enabledSelector isNil or: [ target perform: enabledSelector ])
							ifTrue: [
							selector numArgs = 0
								ifTrue: [target perform: selector]
								ifFalse: [
									argument := itemSpec size >= 8 ifTrue: [ itemSpec eighth ] ifFalse: [ self ].
									target perform: selector with: argument ] ].
						^true ]
					ifNil: [
						^false ]
				]
			]
		].
	^false
]

{ #category : 'focus handling' }
Morph >> keyboardUnfocused [
	"Just lost keyboard focus via a keyboard event.
	See also #keyboardFocused"
]

{ #category : 'geometry testing' }
Morph >> knowsOwnLocalBounds [
	"Meaning that it doesn't need running #drawOn: and #postDrawOn: on a BoundsFinderCanvas
	to explore affected pixels to deduce actual morph bounds.

	Note: No morph can answer false to #requiresVectorCanvas and #knowsOwnLocalBounds.
	All morphs are required to answer true to at least one of these methods."

	^false
]

{ #category : 'naming' }
Morph >> label [
	"Answer the name to show in a list of windows-and-morphs to represent the receiver"

	^ self class name
]

{ #category : 'submorphs-accessing' }
Morph >> lastSubmorph [
	^submorphs last
]

{ #category : 'accessing' }
Morph >> layerNumber [
	"Morphs with smaller layer number will always cover those with larger ones.
	See #defaultLayerNumber."

	^self
		valueOfProperty: #layerNumber
		ifAbsent: [self defaultLayerNumber]
]

{ #category : 'accessing - flags' }
Morph >> layoutNeeded: aBoolean [

	self privateFlagAt: 4 put: aBoolean
]

{ #category : 'layout' }
Morph >> layoutSubmorphsIfNeeded [
	"Recompute the layout if necessary.
	NOP by default"
]

{ #category : 'geometry' }
Morph >> localBoundsForError [
	"Only to be used for drawing in an error condition. See senders."

	^`0@0 corner: 75@70`.
]

{ #category : 'accessing' }
Morph >> location [
	^ GeometryTransformation identity
]

{ #category : 'accessing' }
Morph >> location: aGeometryTransformation [

]

{ #category : 'accessing' }
Morph >> lock [
	self lock: true
]

{ #category : 'accessing - properties' }
Morph >> lock: aBoolean [
	self setProperty: #locked toValue: aBoolean
]

{ #category : 'menus' }
Morph >> lockUnlockMorph [
	"If the receiver is locked, unlock it; if unlocked, lock it"

	self isLocked ifTrue: [self unlock] ifFalse: [self lock]
]

{ #category : 'menus' }
Morph >> lockedString [
	"Answer the string to be shown in a menu to represent the 
	'locked' status"
	^ (self isLocked
		ifTrue: ['<on>']
		ifFalse: ['<off>']), 'be locked'
]

{ #category : 'accessing' }
Morph >> mainBoundsFinderCanvas [
	"Might answer nil if not in a world!"

	^ self mainCanvas ifNotNil: [ :c | c boundsFinderCanvas ]
]

{ #category : 'accessing' }
Morph >> mainCanvas [
	"Might answer nil if not in a world!"

	^ self topmostWorld ifNotNil: [ :w | w mainCanvas ].
]

{ #category : 'layout' }
Morph >> maximumLayoutHeight [
	"Some morphs may be more specific."

	^nil
]

{ #category : 'menus' }
Morph >> maybeAddCollapseItemTo: aMenu [
	"If appropriate, add a collapse item to the given menu"

	owner ifNotNil: [
		owner isWorldMorph ifTrue: [
			aMenu add: 'collapse' target: self action: #collapse ]]
]

{ #category : 'meta-actions' }
Morph >> maybeDuplicateMorph [
	"Maybe duplicate the morph"

	self okayToDuplicate ifTrue:
		[self duplicate openInHand]
]

{ #category : 'submorphs-menus' }
Morph >> menuNshortcutsKey [
	"
	Nil by default. Specific for each instance for PluggableLists. Maybe class name (and therefore specific for each class) for TextModelMorph or such (^self class name).
	"
	^nil
]

{ #category : 'submorphs-menus' }
Morph >> menuNshortcutsSpecsDo: aBlock [
	"Iterate over our own menu and shortcut specs.
	Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for themselves.
	See implementors."
]

{ #category : 'submorphs-menus' }
Morph >> menuNshortcutsSpecsFor: aWidget do: aBlock [
	"aWidget is a submorph, or submorph of a submorph, etc.
	Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for their submorphs.
	See implementors."
]

{ #category : 'geometry' }
Morph >> minimumExtent [
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ `20@20`
]

{ #category : 'layout' }
Morph >> minimumShrinkExtent [
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ self minimumExtent.
]

{ #category : 'layout' }
Morph >> minimumShrinkHeight [
	"This returns the minimum height that the morph may be shrunk to by a LayoutAdjustingMorph.
	It is expressed in the morph own coordinates, like morphExtent."

	^ self minimumExtent y.
]

{ #category : 'layout' }
Morph >> minimumShrinkWidth [
	"This returns the minimum width that the morph may be shrunk to by a LayoutAdjustingMorph.
	It is expressed in the morph own coordinates, like morphExtent."

	^ self minimumExtent x.
]

{ #category : 'submorphs-accessing' }
Morph >> morphBehindAt: worldPoint [
	"Answer the morph that
		- is behind us
		- covers worldPoint
		- is not locked, and no owner in the chain is
		- is visible, and all owners in the chain are
		- there's no other morph covering that point in between.
	Answer could be a sibling, a child of a sibling, our owner, some other ancestor, or some unrelated morph.
	Answer is nil only for the World."
	| top |
	owner ifNil: [
		^nil ].
	owner submorphsBehind: self do: [ :sibling |
		top := sibling topUnlockedMorphAt: worldPoint.
		top ifNotNil: [
			^top ]].
	(owner fullIncludesPixel: worldPoint)
		ifTrue: [
			^owner ].
	^owner morphBehindAt: worldPoint
]

{ #category : 'previewing' }
Morph >> morphBehindBeforePreview [

	^self valueOfProperty: #morphBehindBeforePreview
]

{ #category : 'previewing' }
Morph >> morphBehindBeforePreview: aMorph [

	self setProperty: #morphBehindBeforePreview toValue: aMorph
]

{ #category : 'geometry' }
Morph >> morphExtent [
	"In our own coordinates!
	nil if unknown."

	^ nil
]

{ #category : 'accessing' }
Morph >> morphId [
	"Non zero. A zero id in, for example, VectorEngine, means no Morph.
	reserve lower 8 bits of numeric ivar for boolean flags."
	"
	Morph clearIds
	"
	| morphId |
	morphId := id >> 8.
	morphId = 0 ifTrue: [
		LastMorphId isNil ifTrue: [ LastMorphId := 0 ].
		LastMorphId := LastMorphId + 1.
		id := LastMorphId << 8 + id.							"Keep any flags"
		morphId := LastMorphId ].
	^morphId
]

{ #category : 'geometry' }
Morph >> morphPosition [
	"Answer our position inside our owner, in owner's coordinates.
	Only subclasses of PlacedMorph actually have a position."

	^ 0@0
]

{ #category : 'geometry' }
Morph >> morphPosition: aPoint [
	"Change the position of this morph. Argument is in owner's coordinates.
	Ignored by morphs that are not movable."
]

{ #category : 'events' }
Morph >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse down event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'mouseButton1Down:localPosition:' 
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
]

{ #category : 'events' }
Morph >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse button 1 up event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'mouseButton1Up:localPosition:' 
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
]

{ #category : 'event handling' }
Morph >> mouseButton2Activity [
	"Invoke the menu"
	self getMenu ifNotNil: [ :menu |
		menu popUpInWorld: self world.
		"menu invokeModal" ]
]

{ #category : 'events' }
Morph >> mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse button 2 down event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"

	self
		valueOfProperty: #'mouseButton2Down:localPosition:'
		ifPresentDo: [ :handler |
			handler value: aMouseButtonEvent value: localEventPosition.
			^self ].

	self mouseButton2Activity.
]

{ #category : 'events' }
Morph >> mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition [
	"Handle a mouse button 2 up event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"

	self
		valueOfProperty: #'mouseButton2Up:localPosition:'
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
]

{ #category : 'events' }
Morph >> mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition [

	aMouseButtonEvent shiftPressed
		ifTrue: [self addHalo: aMouseButtonEvent ]
		ifFalse: [self root addHalo: aMouseButtonEvent].
]

{ #category : 'events' }
Morph >> mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition [
	"Ignored. Theoretically we should never get here since control is transferred to the halo on #mouseButton3Down: but subclasses may implement this differently."
]

{ #category : 'halos and balloon help' }
Morph >> mouseDownOnHelpHandle: anEvent [
	"The mouse went down in the show-balloon handle"
	
	| str |
	anEvent shiftPressed ifTrue: [^ self editBalloonHelpText].
	str := self balloonText.
	str ifNil: [str := self noHelpString].
	self showBalloon

]

{ #category : 'events' }
Morph >> mouseEnter: aMouseEnterEvent [
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseEnter: 
		ifPresentDo: [ :handler | handler value: aMouseEnterEvent ]
]

{ #category : 'events' }
Morph >> mouseHover: aMouseOverEvent localPosition: localEventPosition [
	"Handle a mouse move event.
	This message will only be sent to Morphs that answer true to #handlesMouseHover for events that have not been previously handled.
	We can query aMouseMoveEvent to know about pressed mouse buttons."
	"Allow instances to dynamically use properties for handling common events."
	self
		valueOfProperty: #mouseHover:localPosition:
		ifPresentDo: [ :handler |
			handler
				value: aMouseOverEvent
				value: localEventPosition ].
]

{ #category : 'events' }
Morph >> mouseLeave: aMouseLeaveEvent [
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.
	Note: a Morph must answer true to #handlesMouseOver: in order to receive this message."

	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: aMouseLeaveEvent ].
]

{ #category : 'events' }
Morph >> mouseMove: aMouseMoveEvent localPosition: localEventPosition [
	"Handle a mouse move event.
	We can query aMouseMoveEvent to know about pressed mouse buttons.
	Many submorphs will only do stuff like
		aMouseMoveEvent anyButtonPressed ifTrue: []."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseMove:localPosition: 
		ifPresentDo: [ :handler | handler value: aMouseMoveEvent value: localEventPosition ]
]

{ #category : 'events' }
Morph >> mouseScroll: aMouseScrollEvent localPosition: localEventPosition [
	"Handle a mouse scroll event.
	This message will only be sent to Morphs that answer true to #handlesMouseScroll:
	We can query aMouseScrollEvent to know about pressed mouse buttons."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseScroll:localPosition: 
		ifPresentDo: [ :handler | handler value: aMouseScrollEvent value: localEventPosition ]
]

{ #category : 'events' }
Morph >> mouseStillDown [
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	
	self "Allow instances to use this"
		valueOfProperty: #'mouseStillDown
'		ifPresentDo: [ :handler | handler value ]
]

{ #category : 'event handling' }
Morph >> mouseStillDownStepRate [
	"At what rate do I want to receive #mouseStillDown notifications?
	This is actually the time interval between notifications, in milliseconds."
	^1
]

{ #category : 'event handling' }
Morph >> mouseStillDownThreshold [
	"Return the number of milliseconds after which mouseStillDown should be sent"
	^200
]

{ #category : 'accessing - properties' }
Morph >> name [
	"Answer the value of morphName"
	^ self valueOfProperty: #morphName
]

{ #category : 'accessing - properties' }
Morph >> name: anObject [
	"Set the morphName property"
	self setProperty: #morphName toValue: anObject
]

{ #category : 'accessing - flags' }
Morph >> needsRedraw: aBoolean [

	self privateFlagAt: 1 put: aBoolean
]

{ #category : 'iteration of all morphs' }
Morph >> nextMorph [
	"Iterate over all morphs in the receiver structure (usually all morphs living in the receiver's world).
	Do a DFS (Depth First Seach) over the morphs tree"
	
	submorphs isEmpty
		ifFalse: [ ^submorphs first ].

	^self nextMorphPart2
]

{ #category : 'iteration of all morphs' }
Morph >> nextMorphPart2 [
	
	| next |
	owner ifNotNil: [
		next := owner submorphBehind: self.
		next ifNotNil: [ ^ next ].
		
		^ owner nextMorphPart2 ].

	^ self
	
]

{ #category : 'iteration of all morphs' }
Morph >> nextMorphThat: aBlock [
	"Keep looking for some morph that satisfies aBlock.
	Answer nil if none"

	| candidate satisfies |
	candidate := self.
	[
		candidate := candidate nextMorph.
		satisfies := aBlock value: candidate.
		satisfies not
			"But stop after a whole turn."
			and: [ candidate ~~ self ] ] whileTrue.

	^satisfies ifTrue: [ candidate ]
]

{ #category : 'halos and balloon help' }
Morph >> noHelpString [
	^ 'Help not yet supplied'
]

{ #category : 'submorphs-accessing' }
Morph >> noteNewOwner: aMorph [
	"I have just been added as a submorph of aMorph"
]

{ #category : 'object serialization' }
Morph >> objectForDataStream: refStrm  [
	"I am being written out on an object file"
	self prepareToBeSaved.	"Amen"
	^self
]

{ #category : 'halos and balloon help' }
Morph >> okayToBrownDragEasily [
	"Answer whether it it okay for the receiver to be brown-dragged easily -- i.e. repositioned within its container without extracting it.  At present this is just a hook -- nobody declines."

	^ true



"
	^ (self topRendererOrSelf owner isKindOf: PasteUpMorph) and:
		[self layoutPolicy isNil]"
]

{ #category : 'copying' }
Morph >> okayToDuplicate [
	"Answered false by morphs that can't simply be duplicated"

	^ true
]

{ #category : 'halos and balloon help' }
Morph >> okayToResizeEasily [
	"Answer whether it is appropriate to have the receiver be easily resized by the user from the halo"

	^ self hasVariableExtent
]

{ #category : 'halos and balloon help' }
Morph >> okayToRotateEasily [
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver."

	^ self hasOwnLocation
]

{ #category : 'halos and balloon help' }
Morph >> okayToScaleEasily [
	"Answer whether it is appropriate for a scale handle to be shown for the receiver."

	^ self hasOwnLocation
]

{ #category : 'initialization' }
Morph >> openInHand [
	"Attach the receiver to the current hand in the current morphic world"

	self runningWorld activeHand attachMorph: self
]

{ #category : 'initialization' }
Morph >> openInWorld [

	self runningWorld
		ifNil: [ UISupervisor whenUIinSafeState: [ self openInWorld ]]
		ifNotNil: [ :w | self openInWorld: w ]
]

{ #category : 'initialization' }
Morph >> openInWorld: aWorld [
	"Add this morph to the requested World."
	aWorld addMorph: self
]

{ #category : 'structure' }
Morph >> owner [
	"Returns the owner of this morph, which may be nil."

	^ owner
]

{ #category : 'debug and other' }
Morph >> ownerChain [
	"Answer a list of objects representing the receiver and all of its owners.   The first element is the receiver, and the last one is typically the world in which the receiver resides"

	| c next |
	c := OrderedCollection with: self.
	next := self.
	[(next := next owner) notNil] whileTrue: [c add: next].
	^c asArray
]

{ #category : 'structure' }
Morph >> owningWindow [
	"Return the first enclosing morph that is a kind of Window, or nil if none"

	^ self firstOwnerSuchThat: [ :m | m is: #SystemWindow ]
]

{ #category : 'geometry services' }
Morph >> ownsPixel: worldPoint [
	"Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.
	Requires VectorGraphics (#requiresVectorCanvas == true), as we need VectorCanvas' morphIdAt: service.

	Valid if running on VectorCanvas.

	In case of running on HybridCanvas, this method is only valid for morphs that
	are drawn by VectorCanvas (i.e. they answer true to #requiresVectorCanvas).

	See #coversPixel:
	See #includesPixel:"

	self mainCanvas ifNotNil: [ :mainCanvas |
		mainCanvas canDoVectorGraphics ifTrue: [
			^ (mainCanvas morphIdAt: worldPoint) = self morphId ]].
	^ false.
]

{ #category : 'geometry' }
Morph >> positionInWorld [
	"Answer is in world coordinates."

	^ self externalizeToWorld: `0@0`
]

{ #category : 'geometry' }
Morph >> positionInWorld: newPositionInWorld [
	"Change the position of this morph. Argument is in world coordinates.
	Ignored by morphs that are not movable."
]

{ #category : 'active model behavior' }
Morph >> postCopy [

	super postCopy.
	self removeAllActions
]

{ #category : 'drawing' }
Morph >> postDrawOn: aCanvas [
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.
	Possible second stage of drawing, after drawing submorphs, and on top of them.
	Answer true if anything was drawn."

	^false
]

{ #category : 'meta-actions' }
Morph >> potentialEmbeddingTargets [
	"Return the potential targets for embedding the receiver"
	| myRect myWorld fb |
	owner ifNil:[^#()].
	myWorld := owner world ifNil: [^#()].
	myRect := self displayFullBounds.
	^Array streamContents: [ :strm |
		myWorld allMorphsBut: self do: [ :m |
			(m isReallyVisible
				and: [ m isReallyUnlocked
					and: [
						fb := m displayFullBounds.
						(fb notNil and: [fb intersects: myRect])
							and: [(#(HaloMorph HaloHandleMorph) statePointsTo: m class name) not]]])
		 ifTrue: [ strm nextPut: m ]]].
]

{ #category : 'fileIn/out' }
Morph >> prepareToBeSaved [
	"Prepare this morph to be saved to disk. Subclasses should nil out any instance variables that holds state that should not be saved, such as cached Forms. Note that this operation may take more drastic measures than releaseCachedState; for example, it might discard the transcript of an interactive chat session."

	self releaseCachedState
]

{ #category : 'previewing' }
Morph >> previewing [

	^(self valueOfProperty: #previewing) = true
]

{ #category : 'previewing' }
Morph >> previewing: aBoolean [

	self setProperty: #previewing toValue: aBoolean.
	self redrawNeeded.
]

{ #category : 'iteration of all morphs' }
Morph >> previousMorph [
	"Iterate over all morphs in the receiver structure (usually all morphs living in the receiver's world),
	in 'backward'  direction.
	Do a DFS (Depth First Seach) over the morphs tree"
	
	| prev |
	owner ifNotNil: [
		prev := owner submorphInFrontOf: self.
		prev ifNotNil: [ ^prev veryLastLeaf ].
		^owner ].
	^self veryLastLeaf
]

{ #category : 'iteration of all morphs' }
Morph >> previousMorphThat: aBlock [
	"Keep looking for some morph that satisfies aBlock.
	Answer nil if none"

	| candidate satisfies |
	candidate := self.
	[
		candidate := candidate previousMorph.
		satisfies := aBlock value: candidate.
		satisfies not
			"But stop after a whole turn."
			and: [ candidate ~~ self ] ] whileTrue.

	^satisfies ifTrue: [ candidate ]
]

{ #category : 'printing' }
Morph >> printOn: aStream  [
	"Add the identity of the receiver to a stream"
	aStream isText
		ifTrue: [
			aStream
				withAttribute: (TextAnchor new anchoredFormOrMorph: (owner ifNil: [self] ifNotNil: [self imageForm: 32@32 depth: 32]))
				do: [ aStream nextPut: $* ].
			^ self].
	super printOn: aStream. "a(n) className"
	aStream 
		nextPut: $(;
		print: self identityHash;
		nextPut: $).
	self valueOfProperty: #morphName ifPresentDo: [ :x | 
		aStream nextPutAll: ' :: ';
			nextPutAll: x asString]
]

{ #category : 'private' }
Morph >> privateAddAllMorphs: aCollection atIndex: index [
	"Private. Add aCollection of morphs to the receiver"
	| myWorld itsWorld otherSubmorphs |
	myWorld := self world.
	otherSubmorphs := submorphs copyWithoutAll: aCollection.
	(index between: 0 and: otherSubmorphs size)
		ifFalse: [^ self error: 'index out of range'].
	index = 0
		ifTrue:[	submorphs := aCollection asArray, otherSubmorphs]
		ifFalse:[	index = otherSubmorphs size
			ifTrue:[	submorphs := otherSubmorphs, aCollection]
			ifFalse:[	submorphs := otherSubmorphs copyReplaceFrom: index + 1 to: index with: aCollection ]].
	aCollection do: [:m | | itsOwner |
		itsOwner := m owner.
		itsOwner ifNotNil: [
			itsWorld := m world.
			(itsWorld == myWorld) ifFalse: [
				itsWorld ifNotNil: [m redrawNeeded]].
			(itsOwner ~~ self) ifTrue: [
				m owner privateRemove: m.
				m owner removedMorph: m ]].
		m privateOwner: self.
		myWorld ifNotNil: [m redrawNeeded].
		(myWorld == itsWorld) ifFalse: [m intoWorld: myWorld].
		itsOwner == self ifFalse: [
			self addedMorph: m.
			m noteNewOwner: self ].
	].
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged
]

{ #category : 'private' }
Morph >> privateAddMorph: aMorph atIndex: index [

	self privateAddMorph: aMorph atIndex: index position: nil
]

{ #category : 'private' }
Morph >> privateAddMorph: aMorph atIndex: index position: aPointOrNil [

	| oldIndex myWorld itsWorld oldOwner |
	aMorph == self ifTrue: [^ self error: 'can not be own submorph'].
	((index >= 1) and: [index <= (submorphs size + 1)])
		ifFalse: [^ self error: 'index out of range'].
	myWorld := self world.
	oldOwner := aMorph owner.
	(oldOwner == self and: [(oldIndex := submorphs indexOf: aMorph) > 0]) ifTrue: [
		oldIndex = index ifTrue: [ ^self ].
		"aMorph's position changes within in the submorph chain"
		oldIndex < index ifTrue: [
			"moving aMorph to back"
			submorphs replaceFrom: oldIndex to: index-1 with: submorphs startingAt: oldIndex+1.
			submorphs at: index put: aMorph.
		] ifFalse: [
			"moving aMorph to front"
			oldIndex-1 to: index by: -1 do: [ :i |
				submorphs at: i+1 put: (submorphs at: i)].
			submorphs at: index put: aMorph.
		].
		aPointOrNil ifNotNil: [aMorph privatePosition: aPointOrNil].
	] ifFalse: [
		"adding a new morph"
		oldOwner ifNotNil: [
			itsWorld := aMorph world.
			itsWorld ifNotNil: [aMorph invalidateBounds].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs := submorphs copyReplaceFrom: index to: index-1 with: (Array with: aMorph).
		aPointOrNil ifNotNil: [aMorph privatePosition: aPointOrNil].
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
	].
	myWorld ifNotNil: [
		index = 1
			ifTrue: [aMorph redrawNeeded ]
			ifFalse: [aMorph invalidateBounds]].
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]
]

{ #category : 'private' }
Morph >> privateAnyOwnerHandlesMouseScroll: aMouseScrollEvent [
	| foundHandler |
	foundHandler := false.
	(self ownerChain allButFirst anySatisfy: [ :anOwner |
		anOwner isWorldMorph not and: [ anOwner handlesMouseScroll: aMouseScrollEvent ]]) ifTrue: [ foundHandler := true ].
	^ foundHandler.
]

{ #category : 'submorphs-add/remove' }
Morph >> privateDelete [
	"Remove the receiver as a submorph of its owner"
	owner ifNotNil:[owner removeMorph: self].
]

{ #category : 'private' }
Morph >> privateDisplayBounds [
	^privateDisplayBounds
]

{ #category : 'private' }
Morph >> privateDisplayBounds: aRectangle [
	privateDisplayBounds := aRectangle
]

{ #category : 'private' }
Morph >> privateFixLayerOrder [
	"Ensure that morphs with smaller #layerNumber are above morphs with larger #layerNumber.
	Keep current sorting inside each set with same #layerNumber."

	| layerNumbers sortedSubmorphs newIndex |
	self honorsLayerOrder ifFalse: [ ^self ].
	submorphs size < 2 ifTrue: [ ^self ].
	layerNumbers := Set new.
	submorphs do: [ :m | layerNumbers add: m layerNumber ].
	layerNumbers := layerNumbers asArray sort.
	sortedSubmorphs := Array new: submorphs size.
	newIndex := 1.
	layerNumbers do: [ :layerNumber |
		submorphs withIndexDo: [ :m :i |
			m layerNumber = layerNumber ifTrue: [
				sortedSubmorphs at: newIndex put: m.
				newIndex = i ifFalse: [ m invalidateBounds ].
				newIndex := newIndex + 1 ]]].
	submorphs := sortedSubmorphs.
]

{ #category : 'private' }
Morph >> privateFlagAt: bitIndex [
	"We can hold up to 8 1-bit flags.
	Initial value of all flags is false."

	^(id bitAt: bitIndex) = 1
]

{ #category : 'private' }
Morph >> privateFlagAt: bitIndex put: aBoolean [
	"We can hold up to 8 1-bit flags.
	Initial value of all flags is false."

	id := id bitAt: bitIndex put: (aBoolean ifTrue: [1] ifFalse: [0])
]

{ #category : 'private' }
Morph >> privateMove: aMorph toIndex: index [
	"aMorph's position changes within in the submorph chain
	Move aMorph to the requested index."

	| oldIndex |
	oldIndex := submorphs indexOf: aMorph.
	index = oldIndex ifTrue: [ ^self ].
	index < oldIndex
		ifTrue: [
			oldIndex-1 to: index by: -1 do: [ :i |
				submorphs at: i+1 put: (submorphs at: i)]]
		ifFalse: [
			submorphs replaceFrom: oldIndex to: index-1 with: submorphs startingAt: oldIndex+1 ].
	submorphs at: index put: aMorph.
	self world ifNotNil: [aMorph redrawNeeded].
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged.
]

{ #category : 'private' }
Morph >> privateMoveBackMorph: aMorph [
	"aMorph's position changes within in the submorph chain
	Move aMorph behind all siblings"
	
	self privateMove: aMorph toIndex: submorphs size
]

{ #category : 'private' }
Morph >> privateMoveFrontMorph: aMorph [
	"aMorph's position changes within in the submorph chain
	Move aMorph to be above all siblings"
	
	self privateMove: aMorph toIndex: 1
]

{ #category : 'private' }
Morph >> privateOwner: aMorph [
	"Private! Should only be used by methods that maintain the ower/submorph invariant."

	owner := aMorph.
]

{ #category : 'private' }
Morph >> privatePosition: aPoint [
	"Change the position of this morph. Argument is in owner's coordinates.
	Ignored by morphs that are not movable."
]

{ #category : 'private' }
Morph >> privateRemove: aMorph [
	"Private! Should only be used by methods that maintain the ower/submorph invariant."
	submorphs := submorphs copyWithout: aMorph.
]

{ #category : 'private' }
Morph >> privateSubmorphs [
	"Private! Use 'submorphs' instead."

	^ submorphs
]

{ #category : 'events-processing' }
Morph >> processDropFiles: aDropFilesEvent [
	"Handle a dropping file."
	
	aDropFilesEvent wasHandled: true.
	self dropFiles: aDropFilesEvent.
]

{ #category : 'events-processing' }
Morph >> processDropMorph: aDropEvent [
	"Handle a dropping morph."

	| aMorph |
	aMorph := aDropEvent contents.
	aDropEvent wasHandled: true.
	aMorph justDroppedInto: self event: aDropEvent.
	self acceptDroppingMorph: aMorph event: aDropEvent.
]

{ #category : 'events-processing' }
Morph >> processKeyDown: aKeyboardEvent [
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyDown: aKeyboardEvent.
]

{ #category : 'events-processing' }
Morph >> processKeyUp: aKeyboardEvent [
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyUp: aKeyboardEvent.
]

{ #category : 'events-processing' }
Morph >> processKeystroke: aKeyboardEvent [
	"A KeyboardEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	self keyStroke: aKeyboardEvent.
	self 
		valueOfProperty: #'keyStroke:'
		ifPresentDo: [ :handler |
			handler value: aKeyboardEvent.
			aKeyboardEvent wasHandled: true ].
]

{ #category : 'events-processing' }
Morph >> processMouseDown: aMouseButtonEvent localPosition: localEventPosition [
	"A #mouseDown MouseButtonEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	Notes:
		- MouseButtonEvent are sent to the current mouse focus, and if none,
			to the topmost morph under the hand that handles it."
	
	aMouseButtonEvent hand removePendingBalloon.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].
	
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(Preferences at: #focusFollowsMouse)
		ifTrue: [" MouseEnterToFocus. Clicks have no relation to keyboard focus. Just deliver the event."
			self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]
		ifFalse: [ "ClickToFocus. Still two different cases. Some morphs 'ignore' focus setting MouseDown events."
			self clickToFocusConsumesEvent
				ifTrue: [
					"#clickToFocusConsumesEvent means: Don't deliver a MouseDown if the
					soon-to-be MouseUp will set keyboard focus on us (see #processMouseUp:localPosition:).
					So, deliver it if we already have keyboard focus."
					self hasKeyboardFocus ifTrue: [
						self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]]
				ifFalse: [
					"(#clickToFocusConsumesEvent not) means: A keyboard focus setting MouseDown is also a
					regular MouseDown (after setting keyboard focus)"
					self hasKeyboardFocus ifFalse: [
						aMouseButtonEvent hand newKeyboardFocus: self ].
					self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]].

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue: [
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ].
]

{ #category : 'events-processing' }
Morph >> processMouseEnter: aMouseEnterEvent localPosition: localEventPosition [
	"A MouseEnterEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	aMouseEnterEvent hand hasSubmorphs ifTrue:
		[(self handlesDragOver: aMouseEnterEvent) ifTrue:
			[aMouseEnterEvent wasHandled: true.
			self dragEnter: aMouseEnterEvent].
		^ self].
	"Moved to #processMouseMove:localPosition:"
	"self wantsBalloon ifTrue: [
		aMouseEnterEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	"
	(self handlesMouseOver: aMouseEnterEvent) ifTrue: [
		aMouseEnterEvent wasHandled: true.
		self mouseEnter: aMouseEnterEvent ].
]

{ #category : 'events-processing' }
Morph >> processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition [
	"A MouseLeaveEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseLeaveEvent hand removePendingBalloon.
	aMouseLeaveEvent hand hasSubmorphs ifTrue:
		[(self handlesDragOver: aMouseLeaveEvent) ifTrue:
			[aMouseLeaveEvent wasHandled: true.
			self dragLeave: aMouseLeaveEvent].
		^ self].
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ].
]

{ #category : 'events-processing' }
Morph >> processMouseMove: aMouseMoveEvent localPosition: localEventPosition [
	"A MouseMoveEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	
	Notes:
		- MouseMoveEvents are not sent if the hand is carrying morphs around.
		- MouseMoveEvents are sent to the current mouse focus, and if none, to the topmost
			morph under the hand that handles it."

	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.

	self wantsBalloon ifTrue: [
		aMouseMoveEvent hand removePendingBalloon.
		aMouseMoveEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	
	(self clickToFocusConsumesEvent and: [ (Preferences at: #focusFollowsMouse) not ])
		ifTrue: [ "ClickToFocus, and focus setting MouseDown was ignored. (see #processMouseDown:localPosition:)"
			self hasKeyboardFocus ifTrue: [
				"If we already have keyboardFocus, the MouseDown was delivered. Deliver also the MouseMove."
				self mouseMove: aMouseMoveEvent localPosition: localEventPosition  ]]
		ifFalse: [
			"See #processMouseDown:localPosition: Mouse down was a regular event. Deliver also the MouseMove."
			self mouseMove: aMouseMoveEvent localPosition: localEventPosition ].

	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ].
]

{ #category : 'events-processing' }
Morph >> processMouseOver: aMouseOverEvent localPosition: localEventPosition [
	"A MouseOverEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseOverEvent hand noticeMouseOver: self event: aMouseOverEvent.

	aMouseOverEvent hand hasSubmorphs ifTrue:
		[self handlesDragHover ifTrue:
			[self dragHover: aMouseOverEvent localPosition: localEventPosition].
		^ self].
	
	"Open question: should any unhandled mouse move events be filtered out?
	(i.e. should mouseHover:localPosition: be called when a mouse button is pressed
	but the morph doesn't have mouse button handlers?
	Essentially, what are the limits of what is considered 'hovering'?"
	self handlesMouseHover ifTrue: [
		self
			mouseHover: aMouseOverEvent
			localPosition: localEventPosition ].
]

{ #category : 'events-processing' }
Morph >> processMouseScroll: aMouseScrollEvent localPosition: localEventPosition [
	"A MouseScrollEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	(self handlesMouseScroll: aMouseScrollEvent) ifTrue: [
		self
			mouseScroll: aMouseScrollEvent
			localPosition: localEventPosition.
		aMouseScrollEvent wasHandled: true ].
]

{ #category : 'events-processing' }
Morph >> processMouseStillDown [
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	self hasMouseFocus
		ifFalse: [
			^self stopStepping: #processMouseStillDown ].
	self mouseStillDown
]

{ #category : 'events-processing' }
Morph >> processMouseUp: aMouseButtonEvent localPosition: localEventPosition [
	"A #mouseUp MouseButtonEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	Notes:
		- MouseButtonEvent are sent to the current mouse focus, and if none,
			to the topmost morph under the hand that handles it."

	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	"If we click to focus, don't process this event as a regular click.
	See also #processMouseDown:localPosition:"
	"(self clickToFocusConsumesEvent and: [
		(Preferences at: #focusFollowsMouse) not and: [
			self hasKeyboardFocus not ]])
		ifTrue: [ aMouseButtonEvent hand newKeyboardFocus: self ]
		ifFalse: [ self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ]."
	(self clickToFocusConsumesEvent and: [ (Preferences at: #focusFollowsMouse) not ])
		ifTrue: [
			self hasKeyboardFocus
				ifFalse: [
					"Haven't set keyboard focus but must. Do it.
					MouseDown and any MouseMove were not delivered. Do not deliver #mouseButton1Up:localPosition:"
					aMouseButtonEvent hand newKeyboardFocus: self ]
				ifTrue: [
					"If we already have keyboardFocus, the MouseDown was delivered. Deliver also the MouseMove."
					self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ]]
		ifFalse: [
			"See #processMouseDown:localPosition: Mouse down was a regular event. Deliver also the MouseUp."
			self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ].

	self stopStepping: #processMouseStillDown.
]

{ #category : 'events-processing' }
Morph >> processUnknownEvent: aMorphicEvent [
	"An event of an unknown type was sent to the receiver. What shall we do?!"

	Smalltalk beep. 
	aMorphicEvent printString displayAt: `0@0`.
	aMorphicEvent wasHandled: true
]

{ #category : 'events-processing' }
Morph >> processWindowEvent: aWindowEvent [
	"Handle an event concerning our host OS window"

	aWindowEvent wasHandled: true.
	self windowEvent: aWindowEvent.

]

{ #category : 'updating' }
Morph >> redrawNeeded [
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way.
	Notes:
		Area used before any change is reported here.
		Area used after any changes is reported when displayBounds are updated.
			See updateBoundsIn:addDamageTo: and senders.
	"

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self basicDisplayBounds ifNotNil: [ :b |
		self invalidateDisplayRect: b for: self ].
	(self submorphsMightProtrude and: [self clipsSubmorphs not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].
	self haloRedrawMayBeNeeded.
	self removeProperty: #bitMask.
	self removeProperty: #fullBoundsInOwner.
]

{ #category : 'dropping/grabbing' }
Morph >> rejectDropMorphEvent: dropEvent [
	"The receiver has been rejected, and must be put back somewhere.
	 If the original owner and position are known, use them, 
	else Just keep it in the hand"

	((dropEvent formerOwner isNil) or: [ dropEvent formerPosition isNil ])
		ifTrue: [
			dropEvent hand grabMorph: self.
		]
		ifFalse: [
			dropEvent formerOwner addMorph: self.
				   self positionInWorld: dropEvent formerPosition.
		]
		
]

{ #category : 'events-processing' }
Morph >> rejectsEvent: aMorphicEvent [
	"Return true to reject the given event. Submorphs might still handle it."

	^ false
]

{ #category : 'active model behavior' }
Morph >> releaseActionMap [

	self removeAllActions
]

{ #category : 'caching' }
Morph >> releaseCachedState [
	"Release any state that can be recomputed on demand, such as the pixel values for a color gradient or the editor state for a TextMorph. This method may be called to save space when a morph becomes inaccessible. Implementations of this method should do 'super releaseCachedState'. This default implementation does nothing."

]

{ #category : 'events-alarms' }
Morph >> removeAlarm: aSelector [
	"Remove the given alarm"
	| scheduler |
	scheduler := self alarmScheduler.
	scheduler ifNotNil:[scheduler removeAlarm: aSelector for: self].
]

{ #category : 'active model behavior' }
Morph >> removeAllActions [

	self removeProperty: #actionMap
]

{ #category : 'submorphs-add/remove' }
Morph >> removeAllMorphs [
	| oldMorphs |
	submorphs isEmpty ifTrue: [ ^self ].
	submorphs do: [ :m |
		m invalidateBounds.
		m privateOwner: nil ].
	oldMorphs := submorphs.
	submorphs := #().
	oldMorphs do: [ :m |
		self removedMorph: m ].
	self someSubmorphPositionOrExtentChanged.

]

{ #category : 'submorphs-add/remove' }
Morph >> removeAllMorphsIn: aCollection [
	"greatly speeds up the removal of *lots* of submorphs"
	| set |
	aCollection isEmpty ifTrue: [ ^self ].
	set := IdentitySet newWithRoomForMoreThan: aCollection size * 4 // 3.
	aCollection do: [ :each | each owner == self ifTrue: [ set add: each ]].
	set isEmpty ifTrue: [ ^self ].
	set do: [ :m | m privateOwner: nil ].
	submorphs := submorphs reject: [ :each | set includes: each].
	set do: [ :m | self removedMorph: m ].
	self someSubmorphPositionOrExtentChanged.
	self redrawNeeded.
]

{ #category : 'halos and balloon help' }
Morph >> removeHalo [
	| h |
	h := self halo.
	h ifNotNil: [h delete]
]

{ #category : 'submorphs-add/remove' }
Morph >> removeMorph: aMorph [
	"Remove the given morph from my submorphs"

	aMorph owner == self ifFalse: [^self].
	aMorph invalidateBounds.
	self privateRemove: aMorph.
	aMorph privateOwner: nil.
	self removedMorph: aMorph.
	self someSubmorphPositionOrExtentChanged.

]

{ #category : 'accessing - properties' }
Morph >> removeProperty: aSymbol [
	"removes the property named aSymbol if it exists"

	properties ifNil: [ ^ self ].
	properties
		removeKey: aSymbol
		ifAbsent: nil.
	properties isEmpty ifTrue: [ properties := nil ]
]

{ #category : 'submorphs-add/remove' }
Morph >> removedMorph: aMorph [
	"Notify the receiver that aMorph was just removed from its children"

]

{ #category : 'submorphs-add/remove' }
Morph >> replaceSubmorph: oldMorph by: newMorph [
	| index |
	oldMorph stopStepping.
	index := submorphs indexOf: oldMorph.
	oldMorph privateDelete.
	self privateAddMorph: newMorph atIndex: index
]

{ #category : 'geometry testing' }
Morph >> requiresVectorCanvas [
	"True if we use VectorCanvas protocol, or we are rotated or zoomed.
	If False, we can be drawn by BitBltCanvas, and needs to implement:
		#morphExtent and #topLeftInOwner

	Note: No morph can answer false to #requiresVectorCanvas and #knowsOwnLocalBounds.
	All morphs are required to answer true to at least one of these methods."

	^ true
]

{ #category : 'testing' }
Morph >> resistsRemoval [
	"Answer whether the receiver is marked as resisting removal"

	^ false
]

{ #category : 'debug and other' }
Morph >> resumeAfterDrawError [

	self drawingFailsNot.
	self redrawNeeded
]

{ #category : 'debug and other' }
Morph >> resumeAfterStepError [
	"Resume stepping after an error has occured."
	self startStepping. "Will #step"
	self removeProperty:#errorOnStep. "Will remove prop only if #step was okay"

]

{ #category : 'structure' }
Morph >> root [
	"Return the root of the composite morph containing the receiver. The owner of the root is either nil, a WorldMorph, or a HandMorph. If the receiver's owner is nil, the root is the receiver itself. This method always returns a morph."

	(owner isNil or: [ self isOwnedByWorldOrHand ]) ifTrue: [ ^self ].
	^owner root
]

{ #category : 'geometry' }
Morph >> rotation [
	^0
]

{ #category : 'geometry' }
Morph >> rotation: radians scale: scale [
	"Change the rotation and scale of this morph. Arguments are an angle and a scale.
	Ignored by morphs that are not movable."
]

{ #category : 'geometry' }
Morph >> rotationDegrees [

	^ self rotation radiansToDegrees
]

{ #category : 'geometry' }
Morph >> rotationDegrees: degrees [
	"Change the rotation of this morph. Argument is an angle to be taken as the new rotation."

	self rotation: degrees degreesToRadians
]

{ #category : 'geometry' }
Morph >> scale [
	^1
]

{ #category : 'halos and balloon help' }
Morph >> setBalloonText: stringTextOrSymbol [
	"Set receiver's balloon help text. Pass nil to remove the help."

	stringTextOrSymbol
		ifNil: [ self removeProperty: #balloonText ]
		ifNotNil: [
			self
				setProperty: #balloonText
				toValue: stringTextOrSymbol ].
]

{ #category : 'accessing - properties' }
Morph >> setProperty: aSymbol toValue: anObject [
	"change the receiver's property named aSymbol to anObject"

	"the properties dictionary never has nil as value.
	Asking for a nil value is the same as removing the property."

	anObject ifNil: [
		self removeProperty: aSymbol.
		^nil].
	properties ifNil: [ properties := IdentityDictionary new ].
	properties at: aSymbol put: anObject.
	^anObject
]

{ #category : 'stepping' }
Morph >> shouldGetStepsFrom: aWorld [

	^self world == aWorld
]

{ #category : 'drawing' }
Morph >> show [
	"Make sure this morph is on-stage."
	self visible: true
]

{ #category : 'user interface' }
Morph >> showAndComeToFront [
	"Make me visible if not, set me on top of all other sibling morphs."
	self
		visibleBeforePreview: true;
		morphBehindBeforePreview: nil.
	self
		show;
		comeToFront.
]

{ #category : 'halos and balloon help' }
Morph >> showBalloon [
	"Pop up a balloon containing the receiver #balloonText,
	first removing any existing BalloonMorphs in the world."

	self world ifNotNil: [ :w | | balloon |

		w activeHand deleteBalloon.
		balloon := HoverHelpMorph target: self getter: #balloonText.

		"Do it in a while. In some cases, processing the event that
		might have triggered us might also remove any Help Balloon"
		UISupervisor whenUIinSafeState: [
			balloon popUpForHand: w activeHand ]].
]

{ #category : 'halos and balloon help' }
Morph >> showBalloon: msgString [
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	self world ifNotNil: [ :w |
		self showBalloon: msgString hand: w activeHand ].
]

{ #category : 'halos and balloon help' }
Morph >> showBalloon: msgStringOrText hand: aHand [
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| doWordWrap msg |
	doWordWrap := true.
	msg := msgStringOrText.
	(msgStringOrText notEmpty and: [ msgStringOrText last isLineSeparator ]) ifTrue: [
		doWordWrap := false.
		msg := msg allButLast ].
	self
		showBalloon: msg
		hand: aHand
		doWordWrap: doWordWrap.
]

{ #category : 'halos and balloon help' }
Morph >> showBalloon: msgStringOrText hand: aHand doWordWrap: aBoolean [
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| balloon |
	balloon := HoverHelpMorph contents: msgStringOrText doWordWrap: aBoolean.
	
	"Do it in a while. In some cases, processing the event that
	might have triggered us might also remove any Help Balloon"
	UISupervisor whenUIinSafeState: [
		balloon popUpForHand: aHand ].
]

{ #category : 'debug and other' }
Morph >> showBitMask [
	"Just to play with #bitMask from an inspector. Not to be called from code."
	| r |
	self flag: #bitMask.						"So this method is shown in senders of #bitMask"
	r := self displayFullBounds.
	r top to: r bottom-1 do: [ :y |
		r left to: r right-1 do: [ :x |
			(self coversPixel: x@y) ifTrue: [
				Display colorAt: x@y put: Color random ]]].
	Display forceToScreen
]

{ #category : 'debug and other' }
Morph >> showContour [
	"Just to play with #contour from an inspector. Not to be called from code."
	
	(self valueOfProperty: #contour) ifNotNil: [ :contourArray | | contourTop contourBottom  x0 x1 |
		contourTop := self valueOfProperty: #contourY0.
		contourBottom := self valueOfProperty: #contourY1.
		contourTop to: contourBottom do: [ :y |
			x0 := contourArray at: (y - contourTop) * 2 + 1.
			x1 := contourArray at: (y - contourTop) * 2 + 2.
			Display colorAt: x0+1@y put: `Color black`.
			Display colorAt: x0+2@y put: `Color black`.
			Display colorAt: x1-2@y put: `Color black`.
			Display colorAt: x1-1@y put: `Color black`.
			Display colorAt: x0@y put: `Color lightGreen`.
			Display colorAt: x1@y put: `Color white`.
		].
		Display forceToScreen.
	].
]

{ #category : 'layout' }
Morph >> someSubmorphPositionOrExtentChanged [
	"Some submorph changed position or extent.
	Some Morph classes need to adjust for that."
]

{ #category : 'stepping' }
Morph >> startStepping [
	"Start getting sent the 'step' message."
	"The #stepAt: message, or any message requiring an argument will be 
	called with the current millisecond timer value if no argument value is specified"

	self startStepping: #stepAt:
]

{ #category : 'stepping' }
Morph >> startStepping: aSelector [
	"Start getting sent the requested message at each Morphic step.
	See sample code snippet at Morph>>stepAt:"

	self startStepping: aSelector stepTime: nil
]

{ #category : 'stepping' }
Morph >> startStepping: aSelector in: waitMilliseconds stepTime: timeIntervalMillisecondsOrNil [
	"Start stepping the receiver"

	self world ifNotNil: [ :w |
		w
			startStepping: self
			at: Time localMillisecondClock+waitMilliseconds
			selector: aSelector
			stepTime: timeIntervalMillisecondsOrNil.
		"?"
		"self redrawNeeded" ]
]

{ #category : 'stepping' }
Morph >> startStepping: aSelector stepTime: timeIntervalMillisecondsOrNil [
	"Start stepping the receiver"

	self startStepping: aSelector in: 0 stepTime: timeIntervalMillisecondsOrNil
]

{ #category : 'stepping' }
Morph >> startSteppingStepTime: timeIntervalMillisecondsOrNil [
	"Start stepping the receiver"

	self startStepping: #stepAt: in: 0 stepTime: timeIntervalMillisecondsOrNil
]

{ #category : 'stepping' }
Morph >> step [
	^ self
]

{ #category : 'stepping' }
Morph >> stepAt: millisecondSinceLast [
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. 
	The desired time between steps is specified by this morph's answer to the stepTime message.
	The millisecondSinceLast parameter gives the time elapsed since the previous step."
	"
	m := BoxMorph new.
	m color: Color random.
	m openInWorld.
	m morphPosition: 10@10.
	t := 0.
	m when: #morphicStep evaluate: [ :delta |
		t := t + delta.
		t < 10000
			ifTrue: [
				(m owner is: #HandMorph) ifFalse: [
					m morphPosition: 3@2 * t // 100 ]]
			ifFalse: [ m stopStepping ]].
	m startSteppingStepTime: 20.
	"
	self step.
	self triggerEvent: #morphicStep with: millisecondSinceLast
]

{ #category : 'stepping' }
Morph >> stepTime [
	"Answer the desired time between steps in milliseconds. This default implementation requests that the 'step' method be called once every second."

	^ 1000
]

{ #category : 'menus' }
Morph >> stickinessString [
	"Answer the string to be shown in a menu to represent the  
	stickiness status"

	^ self isSticky asMenuItemTextPrefix,
		'resist being picked up'
]

{ #category : 'accessing - properties' }
Morph >> sticky: aBoolean [
	"change the receiver's sticky property"

	self setProperty: #sticky toValue: aBoolean
]

{ #category : 'stepping' }
Morph >> stopStepping [
	"Stop getting sent the 'step' message."

	self world ifNotNil: [ :w |
		w stopSteppingMorph: self ]
]

{ #category : 'stepping' }
Morph >> stopStepping: aSelector [
	"Stop getting sent the given message."

	self world ifNotNil: [ :w |
		 w stopStepping: self selector: aSelector ]
]

{ #category : 'fileIn/out' }
Morph >> storeDataOn: aDataStream [
	"Let all Morphs be written out.  All owners are weak references.  They only go out if the owner is in the tree being written."
	| cntInstVars cntIndexedVars ti instVarNames |

	"block my owner unless he is written out by someone else"
	cntInstVars := self class instSize.
	cntIndexedVars := self basicSize.
	instVarNames := self class allInstVarNames.
	ti := 1.  
	((instVarNames at: ti) = 'owner') & (Morph superclass == Object) ifFalse: [
		self error: 'this method is out of date'].
	aDataStream
		beginInstance: self class
		size: cntInstVars + cntIndexedVars.
	1 to: cntInstVars do: [ :i |
		(instVarNames at: i) caseOf: {
				['owner']  				-> [aDataStream nextPutWeak: owner]. 	"owner only written if in our tree"
				['id']  					-> [ aDataStream nextPut: (id bitAnd: 255) ].  "Clear id, but keep flags."
				['privateDisplayBounds']	-> [ aDataStream nextPut: nil ].  "Lazy initialized."
			}
			otherwise: [ aDataStream nextPut: (self instVarAt: i)]].
	1 to: cntIndexedVars do: [ :i |
		aDataStream nextPut: (self basicAt: i)]
]

{ #category : 'submorphs-accessing' }
Morph >> submorphBehind: aMorph [

	self submorphsBehind: aMorph do: [ :m | ^m ].
	^nil
]

{ #category : 'submorphs-accessing' }
Morph >> submorphCount [

	^ submorphs size
]

{ #category : 'submorphs-accessing' }
Morph >> submorphInFrontOf: aMorph [

	^submorphs before: aMorph
]

{ #category : 'accessing - flags' }
Morph >> submorphNeedsRedraw: aBoolean [

	self privateFlagAt: 2 put: aBoolean
]

{ #category : 'focus handling' }
Morph >> submorphToFocusKeyboard [
	"Answer a preferred submorph for initial keyboard focus, if desired."
	^nil
]

{ #category : 'submorphs-accessing' }
Morph >> submorphs [

	^ submorphs copy
]

{ #category : 'submorphs-accessing' }
Morph >> submorphsBehind: aMorph do: aBlock [
	| behind |
	behind := false.
	submorphs do:
		[:m | m == aMorph ifTrue: [behind := true]
						ifFalse: [behind ifTrue: [aBlock value: m]]].

]

{ #category : 'submorphs-accessing' }
Morph >> submorphsDo: aBlock [

	submorphs do: aBlock.
]

{ #category : 'submorphs-accessing' }
Morph >> submorphsInFrontOf: aMorph do: aBlock [
	| behind |
	behind := false.
	submorphs do:
		[:m | m == aMorph ifTrue: [behind := true]
						ifFalse: [behind ifFalse: [aBlock value: m]]].

]

{ #category : 'geometry testing' }
Morph >> submorphsMightProtrude [
	"Answered false by morphs that can guarantee that submorphs, and all morphs in their
	submorphs tree, will lie inside their bounds, either 'naturally' (by construction) or by
	being clipped by their owner. See #clipsSubmorphs and clipsSubmorphsReally.
	Allows for many optimizations to be done."

	^ true
]

{ #category : 'submorphs-accessing' }
Morph >> submorphsReverseDo: aBlock [

	submorphs reverseDo: aBlock.
]

{ #category : 'submorphs-accessing' }
Morph >> submorphsSatisfying: aBlock [
	^ submorphs select: [:m | (aBlock value: m) == true]
]

{ #category : 'event handling testing' }
Morph >> tabToFocus [
	"Answer true if tab and shift+tab are used to transfer keyboard focus between windgets.
	See senders."
	^true
]

{ #category : 'accessing' }
Morph >> taskbar [
	^self world ifNotNil: [ :w | w taskbar ]
]

{ #category : 'accessing' }
Morph >> toggleStickiness [
	"togle the receiver's Stickiness"
	self sticky: self isSticky not
]

{ #category : 'geometry' }
Morph >> topLeftInOwner [

	^self fullBoundsInOwner origin
]

{ #category : 'submorphs-accessing' }
Morph >> topMorphAt: worldPoint [

	^ self
		topMorphAt: worldPoint
		satisfying: [ :aMorph | true ]
]

{ #category : 'submorphs-accessing' }
Morph >> topMorphAt: worldPoint satisfying: aCondition [
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint, 
	satisfies the condition block, and is not covered by any other morph. 
	Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	"Not visible implies #includesPixel: is false. But still, don't waste time going into submorphs."
	(self visible and: [ aCondition value: self ]) ifFalse: [ ^ nil ].
	
	(self includesPixel: worldPoint) ifTrue: [ ^ self ].

	submorphs do: [ :m |
		(m topMorphAt: worldPoint satisfying: aCondition) ifNotNil: [ :foundMorph | ^foundMorph ] ]

	^ nil
]

{ #category : 'submorphs-accessing' }
Morph >> topUnlockedMorphAt: worldPoint [
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	self isLocked ifTrue: [ ^nil ].
	
	"Not visible implies #includesPixel: is false. But still, don't waste time going into submorphs."
	self visible ifFalse: [ ^nil ].
	
	(self includesPixel: worldPoint) ifTrue: [ ^ self ].
	submorphs do: [ :m |
		(m topUnlockedMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]].
	^nil
]

{ #category : 'structure' }
Morph >> topmostWorld [
	"Answer the last morph in the owner chain (i.e. the morph without owner) if it is a WorldMorph, or nil."
	^owner
		ifNotNil: [ owner topmostWorld ]
]

{ #category : 'halos and balloon help' }
Morph >> transferHalo: event from: formerHaloOwner [
	"Progressively transfer the halo to the next likely recipient"

	formerHaloOwner == self
		ifFalse: [
			self addHalo: event.
			^self ].

	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil: [
			owner transferHalo: event from: formerHaloOwner.
			^self ].
		"We're at the top level; just keep it on ourselves"
		^self ].

	self submorphsDo: [ :m |
		(m wantsHalo and: [ m fullIncludesPixel: event eventPosition ])
			ifTrue: [
				m transferHalo: event from: formerHaloOwner.
				^self ]].
	"We're at the bottom most level; just keep halo on ourselves"
]

{ #category : 'accessing' }
Morph >> unlock [
	self lock: false
]

{ #category : 'accessing' }
Morph >> unlockContents [
	self submorphsDo:
		[:m | m unlock]
]

{ #category : 'user interface' }
Morph >> unlockOneSubpart [
	| unlockables aMenu reply |
	unlockables := self submorphs select:
		[ :m | m isLocked].
	unlockables size <= 1 ifTrue: [^ self unlockContents].
	aMenu := SelectionMenu labelList: (unlockables collect: [:m | m shortPrintString]) selections: unlockables.
	reply := aMenu startUpWithCaption: 'Who should be be unlocked?'.
	reply ifNil: [^ self].
	reply unlock.
]

{ #category : 'active model behavior' }
Morph >> updateableActionMap [

	^self valueOfProperty: #actionMap ifAbsentPut: [ IdentityDictionary new ] 
]

{ #category : 'accessing - properties' }
Morph >> valueOfProperty: aSymbol [
	"Answer the value of the receiver's property named aSymbol.
	If property is not present, answer nil."

	^ properties
		  ifNotNil: [ properties at: aSymbol ifAbsent: nil ]
]

{ #category : 'accessing - properties' }
Morph >> valueOfProperty: aSymbol ifAbsent: aBlock [
	"if the receiver possesses a property of the given name, answer
	its value. If not then evaluate aBlock and answer the result of
	this block evaluation"
	^ properties
		ifNil: [ aBlock value ]
		ifNotNil: [ properties at: aSymbol ifAbsent: aBlock ]
]

{ #category : 'accessing - properties' }
Morph >> valueOfProperty: aSymbol ifAbsentPut: aBlock [
	"if the receiver possesses a property of the given name, answer
	its value. If not then evaluate aBlock to obtain the value to be
	stored and answered."

	^ self valueOfProperty: aSymbol ifAbsent: [
		self setProperty: aSymbol toValue: aBlock value ]
]

{ #category : 'accessing - properties' }
Morph >> valueOfProperty: aSymbol ifPresentDo: aBlock [
	"If the receiver has a property of the given name, evaluate
	aBlock on behalf of the value of that property"

	| value |
	properties ifNil: [^ self ].
	value := self valueOfProperty: aSymbol ifAbsent: [^self].
	^aBlock value: value
]

{ #category : 'accessing - properties' }
Morph >> valueOfProperty: aSymbol ifPresentDo: aBlock ifAbsent: absentBlock [
	"If the receiver has a property of the given name, evaluate
	aBlock on behalf of the value of that property, if not evaluate absentBlock"

	| value |
	
	value := self valueOfProperty: aSymbol ifAbsent: [^ absentBlock value ].
	
	^aBlock value: value
]

{ #category : 'structure' }
Morph >> veryLastLeaf [
	"Answer the last submorph, recursively, i.e. the very last leaf of the morph tree"

	^submorphs isEmpty
		ifTrue: [ self ]
		ifFalse: [ submorphs last veryLastLeaf ]
]

{ #category : 'accessing - flags' }
Morph >> visible [
	"Answer if I am visible -- default is true.
	Store value of 'hidden' instead, because flags default to false."

	^ (self privateFlagAt: 3) not
]

{ #category : 'drawing' }
Morph >> visible: aBoolean [
	"set the 'visible' attribute of the receiver to aBoolean
	Store value of 'hidden', because flags default to false."

	self visible == aBoolean
		ifTrue: [ ^ self ].

	owner ifNil: [
		self privateFlagAt: 3 put: aBoolean not.
		^self ].

	aBoolean
		ifTrue: [ self redrawNeeded ]
		ifFalse: [
			self removeHalo.
			self invalidateBounds ].
	self privateFlagAt: 3 put: aBoolean not.
	owner someSubmorphPositionOrExtentChanged.
]

{ #category : 'previewing' }
Morph >> visibleBeforePreview [

	^self valueOfProperty: #visibleBeforePreview
]

{ #category : 'previewing' }
Morph >> visibleBeforePreview: aBoolean [

	self setProperty: #visibleBeforePreview toValue: aBoolean
]

{ #category : 'halos and balloon help' }
Morph >> wantsBalloon [
	"Answer true if receiver wants to show a balloon help text is a few moments."

	^ (self balloonText notNil) and: [Preferences at: #balloonHelpEnabled]
]

{ #category : 'geometry testing' }
Morph >> wantsContour [
	"True if a morph requires the Canvas to update a #morphContour property on us.
	Any subclass might answer true, if it #requiresVectorCanvas.
	But if #requiresVectorCanvas is false, #contour can not be computed.
	This is not a problem, as #contour is useless for BoxMorph.
	
	Also see #bitMask"

	"^self requiresVectorCanvas and: [ self isOwnedByWorldOrHand ]"
	^false
]

{ #category : 'dropping/grabbing' }
Morph >> wantsDroppedMorph: aMorph event: evt [
	"Return true if the receiver wishes to accept the given morph, 
	 which is being dropped by a hand in response to the given event. 
	Note that for a successful drop operation both parties need to agree. 
	The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self.
	 Individual Morpks may override by setting the corresponding property
	 to an appropriate two argument closure."

	"DraggingGuideMorph is a bit special. It is usually used to D&D from one list to another.
	Any Morph willing to deal with them should redefine this method."
	(aMorph is: #DraggingGuideMorph) ifTrue: [
		^false ].

	^self valueOfProperty: #wantsDroppedMorph:event:
		 ifPresentDo: [ :wantsMorphEvt | wantsMorphEvt value: aMorph value: evt ]
		 ifAbsent: [ true ]
]

{ #category : 'halos and balloon help' }
Morph >> wantsHalo [
	^self visible
]

{ #category : 'halos and balloon help' }
Morph >> wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph [
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	(#(addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].

	(Preferences at: #selectiveHalos) ifFalse: [
		^true ].

	(#(#addDismissHandle: ) includes: aSelector)
		ifTrue: [ ^ self resistsRemoval not ].
	(#(#addDragHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToBrownDragEasily ].
	(#(#addResizeHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToResizeEasily | self okayToScaleEasily ].
	(#(#addRotateHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToRotateEasily ].
	(#(#addScaleHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToScaleEasily ].
	(#(#addRecolorHandle: ) includes: aSelector)
		ifTrue: [ ^ self wantsRecolorHandle ].
	^ true
]

{ #category : 'halos and balloon help' }
Morph >> wantsRecolorHandle [
	"Answer whether the receiver would like a recoloring halo handle to be put up.  Since this handle also presently affords access to the property-sheet, it is presently always allowed, even though SketchMorphs don't like regular recoloring"

	^ true
	

]

{ #category : 'stepping' }
Morph >> wantsSteps [
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message"

	^false
]

{ #category : 'dropping/grabbing' }
Morph >> wantsToBeDroppedInto: aMorph [
	"Return true if it's okay to drop the receiver into aMorph. This check is symmetric to #wantsDroppedMorph:event: to give both parties a chance of figuring out whether they like each other."
	^true
]

{ #category : 'user interface' }
Morph >> whenUIinSafeState: evaluableObject [
	self world
		ifNotNil: [ :w | w whenUIinSafeState: evaluableObject ]
		ifNil: evaluableObject
]

{ #category : 'structure' }
Morph >> window [
	"Return the first enclosing morph that is a kind of Window, or nil if none"
	^self owningWindow
]

{ #category : 'events' }
Morph >> windowEvent: anEvent [
	"Host window event"
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'windowEvent:' 
		ifPresentDo: [ :handler | handler value: anEvent ]
]

{ #category : 'structure' }
Morph >> withAllOwnersDo: aBlock [
	"Evaluate aBlock with the receiver and all of its owners.
	Start from us, go up to top owner."

	aBlock value: self.
	owner ifNotNil: [ owner withAllOwnersDo: aBlock ]
]

{ #category : 'structure' }
Morph >> withAllOwnersReverseDo: aBlock [
	"Evaluate aBlock with the receiver and all of its owners.
	Start from top owner, go down to us."

	owner ifNotNil: [ owner withAllOwnersReverseDo: aBlock ].
	aBlock value: self
]

{ #category : 'structure' }
Morph >> world [
	^owner
		ifNotNil: [ owner world ]
]
