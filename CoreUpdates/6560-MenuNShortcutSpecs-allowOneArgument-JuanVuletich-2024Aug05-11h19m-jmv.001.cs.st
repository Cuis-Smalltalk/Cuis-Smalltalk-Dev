'From Cuis7.1 [latest update: #6559] on 5 August 2024 at 11:23:54 am'!

!Morph methodsFor: 'events' stamp: 'jmv 8/5/2024 09:47:54'!
keyboardShortcut: aKeyboardEvent
	"Handle a keyboard shortcut.
	See coment at Morph >> #getMenu
	"
	| menuNshortcutsSpecs shortcutCharacter target |
	menuNshortcutsSpecs := self allMenuNshortcutsSpecs.
	menuNshortcutsSpecs ifEmpty: [ ^false ].
	menuNshortcutsSpecs valuesDo: [ :itemSpec |
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
					ifNotNil: [ :selector |
						itemSpec size >= 8
							ifTrue: [ target perform: selector with: itemSpec eighth ]
							ifFalse: [ target perform: selector ].
						^true ]
					ifNil: [
						^false ]
				]
			]
		].
	^false! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 8/5/2024 09:46:41'!
addItemTo: topMenu fromSpec: itemSpec submenus: submenusDictionary
	"
	See coment at Morph >> #getMenu
	"
	| submenuId menu label target selector arguments iconSymbol helpText menuItem |
	label := itemSpec second.
	label ifNil: [ ^self ]. "Only a keyboard shortcut, not a real menu item."
	submenuId := itemSpec first // 1000.
	menu := submenuId = 0 ifTrue: [ topMenu ] ifFalse: [ submenusDictionary at: submenuId ].
	target := itemSpec fourth
		ifNotNil: [ :targetOrGetter |
			targetOrGetter isSymbol
				ifTrue: [ 	self perform: targetOrGetter ]
				ifFalse: [ targetOrGetter ]]
		ifNil: [ self ].
	selector := itemSpec fifth.
	arguments := itemSpec size >= 8 ifTrue: [ {itemSpec eighth} ] ifFalse: [ #() ].
	menuItem := label isSymbol
		ifTrue: [ menu addUpdating: label target: target action: selector argumentList: arguments ]
		ifFalse: [ menu add: label target: target action: selector argumentList: arguments ].
	selector ifNil: [
		menuItem isEnabled: false ].
	iconSymbol := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
	iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
	helpText := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
	helpText ifNotNil: [ menuItem setBalloonText: helpText ].! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 8/5/2024 11:22:39'!
allMenuNshortcutsSpecs
	"Spec for the menu and shortcut keys.
	Each entry is
		#(itemId 			'menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'helpText(optional)'    selectorArgument(optional))
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
		- selectorArgument is optional, but required if selector takes an argument.
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	"Several nested morphs may provide their own menu items and keyboard shortcuts for submorphs.
	A morph could also provide its own."

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
			(menuItemSpec size = 2 and: [ 		menuItemSpec second first = $X ])
				"#(12 		'XXX') means 'remove menu item numbered 12'"
				ifTrue: [ menuNshortcutsSpecs removeKey: menuItemSpec first ifAbsent: nil ]
				ifFalse: [ menuNshortcutsSpecs at: menuItemSpec first put: menuItemSpec ]]].
	^ menuNshortcutsSpecs! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 8/5/2024 11:22:48'!
getMenu
	"Answer the right-click (contextual) menu."
	"Spec for the menu and shortcut keys.
	Each entry is
		#(itemId 			'menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'helpText(optional)'    selectorArgument(optional))
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
		- selectorArgument is optional, but required if selector takes an argument.
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"

	| menuNshortcutsSpecs itemSpec menu submenus |
	menuNshortcutsSpecs := self allMenuNshortcutsSpecs.
	menuNshortcutsSpecs ifEmpty: [ ^nil ].
	menu := MenuMorph new.
	submenus := nil.
	menuNshortcutsSpecs keys sorted do: [ :key |
		itemSpec := menuNshortcutsSpecs at: key.
		itemSpec size <= 2 ifTrue: [
				self addLineTo: menu fromSpec: itemSpec submenus: submenus ]
			ifFalse: [ itemSpec size = 3 ifTrue: [
				self addTitleTo: menu fromSpec: itemSpec ]
			ifFalse: [ itemSpec fifth isNumber ifTrue: [
				submenus ifNil: [submenus := Dictionary new ].
				self addSubmenuTo: menu fromSpec: itemSpec submenus: submenus ]
			ifFalse: [
				self addItemTo: menu fromSpec: itemSpec submenus: submenus ]]]].
	^ menu! !

