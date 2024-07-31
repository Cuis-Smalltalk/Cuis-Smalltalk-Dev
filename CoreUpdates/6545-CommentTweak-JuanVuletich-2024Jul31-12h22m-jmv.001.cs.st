'From Cuis7.1 [latest update: #6544] on 31 July 2024 at 12:23:13 pm'!

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 7/29/2024 16:09:34'!
allMenuNshortcutsSpecs
	"menuSpec is the Spec for the menu and shortcut keys.
	Each entry is
		#(itemId 			'menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- itemId is a number. It specifies the position of the item in the menu, and is also an id, allowing for overrides and deletions.
		- label is a string or symbol. Symbol means self updating menu entry. nil means just keyboard shortcut, no menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Typically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
			If a Number (multiple of 1000), indicates a subMenu. ItemsIds must be in range. See BrowserWindow>>#classListMenuSpec.
			If nil, the menu entry is created but disabled. The keyboard shortcut is disabled too.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
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

