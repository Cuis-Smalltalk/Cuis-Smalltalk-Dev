'From Cuis7.1 [latest update: #6410] on 27 May 2024 at 11:09:43 am'!

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/27/2024 10:29:09'!
getMenu
	"Answer the right-click (contextual) menu."

	| widgetKey menuNshortcutsSpecs itemSpec label menu menuItem shortcutCharacter target selector iconSymbol helpText |
	widgetKey := self menuNshortcutsKey ifNil: [ ^nil ].
	menuNshortcutsSpecs := self menuNshortcutsSpec: widgetKey for: self.
	menuNshortcutsSpecs ifEmpty: [ ^nil ].

	menu := MenuMorph new.
	menuNshortcutsSpecs keys sorted do: [ :key |
		itemSpec := menuNshortcutsSpecs at: key.
		itemSpec size <= 2
			ifTrue: [ menu addLine ]
			ifFalse: [
				itemSpec size = 3
					ifTrue: [
						menu addTitle: itemSpec second.
						itemSpec third ifTrue: [ menu addStayUpIcons ]]
					ifFalse: [
						label := itemSpec second.
						shortcutCharacter := itemSpec third.
						target := itemSpec fourth
							ifNotNil: [ :targetGetter | self perform: targetGetter ]
							ifNil: [ self ].
						selector := itemSpec fifth.
						iconSymbol := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
						helpText := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
						menuItem := label isSymbol
							ifTrue: [ menu addUpdating: label target: target action: selector argumentList: #() ]
							ifFalse: [ menu add: label target: target action: selector argumentList: #() ].
						iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
						helpText ifNotNil: [ menuItem setBalloonText: helpText ]
					]
				]
		].
	^ menu! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/27/2024 10:23:02'!
menuNshortcutsSpec: aWidgetKey for: aWidget
	"menuSpec is the Spec for the menu and shortcut keys.
	It is an Array where each entry is
		#(itemNumber 'menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- itemNumber is an integer or float number, and is only used for ordering of menu items
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	"Several nested morphs may provide their own menu items and keyboard shortcuts for submorphs.
	A morph could also provide its own."

	| menuNshortcutsSpecs |

	menuNshortcutsSpecs := Dictionary new.
	self withAllOwnersReverseDo: [ :o |
		o ownMenuNshortcutsSpecs: aWidgetKey for: aWidget do: [ :menuItemSpec |
			(menuItemSpec size = 2 and: [ 		menuItemSpec second first = $X ])
				"#(12 		'XXX') means 'remove menu item numbered 12'"
				ifTrue: [ menuNshortcutsSpecs removeKey: menuItemSpec first ifAbsent: nil ]
				ifFalse: [ menuNshortcutsSpecs at: menuItemSpec first put: menuItemSpec ]]].
	^ menuNshortcutsSpecs! !

