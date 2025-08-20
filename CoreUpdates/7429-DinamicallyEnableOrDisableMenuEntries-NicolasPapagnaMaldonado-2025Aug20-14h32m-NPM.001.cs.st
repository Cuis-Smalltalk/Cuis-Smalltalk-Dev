'From Cuis7.5 [latest update: #7428] on 20 August 2025 at 2:39:24 pm'!

!Morph methodsFor: 'submorphs-menus' stamp: 'NPM 8/20/2025 14:33:49'!
addItemTo: topMenu fromSpec: itemSpec submenus: submenusDictionary
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
! !

!Morph methodsFor: 'submorphs-menus' stamp: 'NPM 8/20/2025 14:32:23'!
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
	^ menu! !

