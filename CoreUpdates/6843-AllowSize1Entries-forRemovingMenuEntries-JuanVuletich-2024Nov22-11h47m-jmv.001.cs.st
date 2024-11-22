'From Cuis7.1 [latest update: #6840] on 22 November 2024 at 11:51:47 am'!

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 11/22/2024 11:35:20'!
allMenuNshortcutsSpecs
	"Spec for the menu and shortcut keys.
	See #getMenu"
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
			menuNshortcutsSpecs at: menuItemSpec first put: menuItemSpec ]].
	^ menuNshortcutsSpecs.! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 11/22/2024 11:35:30'!
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
				itemSpec second first = $X ifFalse: [ "Compatibility. Also a marker for removing previous entries."
					self addLineTo: menu fromSpec: itemSpec submenus: submenus ]]
			ifFalse: [ itemSpec size = 3 ifTrue: [
				self addTitleTo: menu fromSpec: itemSpec ]
			ifFalse: [ itemSpec fifth isNumber ifTrue: [
				submenus ifNil: [submenus := Dictionary new ].
				self addSubmenuTo: menu fromSpec: itemSpec submenus: submenus ]
			ifFalse: [
				self addItemTo: menu fromSpec: itemSpec submenus: submenus ]]]]].
	^ menu! !

