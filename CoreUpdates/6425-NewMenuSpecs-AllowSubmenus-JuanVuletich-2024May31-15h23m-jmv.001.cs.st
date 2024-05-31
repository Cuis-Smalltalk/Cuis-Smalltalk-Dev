'From Cuis7.1 [latest update: #6421] on 31 May 2024 at 3:29:33 pm'!

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/31/2024 12:57:17'!
addItemTo: topMenu fromSpec: itemSpec submenus: submenusDictionary
	| submenuId menu label target selector isEnabledSelector iconSymbol helpText menuItem |
	label := itemSpec second.
	label ifNil: [ ^self ]. "Only a keyboard shortcut, not a real menu item."
	submenuId := itemSpec first // 1000.
	menu := submenuId = 0 ifTrue: [ topMenu ] ifFalse: [ submenusDictionary at: submenuId ].
	target := itemSpec fourth
		ifNotNil: [ :targetGetter | self perform: targetGetter ]
		ifNil: [ self ].
	selector := itemSpec fifth.
	isEnabledSelector := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
	iconSymbol := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
	helpText := itemSpec size >= 8 ifTrue: [ itemSpec eighth ].
	menuItem := label isSymbol
		ifTrue: [ menu addUpdating: label target: target action: selector argumentList: #() ]
		ifFalse: [ menu add: label target: target action: selector argumentList: #() ].
	isEnabledSelector ifNotNil: [ menuItem isEnabled: (target perform: isEnabledSelector) ].
	iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
	helpText ifNotNil: [ menuItem setBalloonText: helpText ].! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/31/2024 12:58:33'!
addLineTo: topMenu fromSpec: itemSpec submenus: submenusDictionary
	| submenuId menu |
	submenuId := itemSpec first // 1000.
	menu := submenuId = 0 ifTrue: [ topMenu ] ifFalse: [ submenusDictionary at: submenuId ].
	menu addLine.! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/31/2024 12:57:04'!
addSubmenuTo: menu fromSpec: itemSpec submenus: submenusDictionary
	| submenu label submenuId target isEnabledSelector iconSymbol helpText menuItem |
	submenu := MenuMorph new.
	label := itemSpec second.
	target := itemSpec fourth
		ifNotNil: [ :targetGetter | self perform: targetGetter ]
		ifNil: [ self ].
	submenuId := itemSpec fifth // 1000.
	isEnabledSelector := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
	iconSymbol := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
	helpText := itemSpec size >= 8 ifTrue: [ itemSpec eighth ].
	menuItem := menu add: label subMenu: submenu.
	isEnabledSelector ifNotNil: [ menuItem isEnabled: (target perform: isEnabledSelector) ].
	iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
	helpText ifNotNil: [ menuItem setBalloonText: helpText ].
	submenusDictionary at: submenuId put: submenu! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/31/2024 12:14:20'!
addTitleTo: menu fromSpec: itemSpec
	menu addTitle: itemSpec second.
	itemSpec third ifTrue: [ menu addStayUpIcons ]! !


!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/31/2024 12:59:27'!
getMenu
	"Answer the right-click (contextual) menu."
	"Spec for the menu and shortcut keys the changesets list.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"

	| widgetKey menuNshortcutsSpecs itemSpec menu submenus |
	widgetKey := self menuNshortcutsKey ifNil: [ ^nil ].
	menuNshortcutsSpecs := self menuNshortcutsSpec: widgetKey for: self.
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

