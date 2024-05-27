'From Cuis7.1 [latest update: #6410] on 27 May 2024 at 2:38:19 pm'!

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 5/27/2024 12:02:36'!
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

	| widgetKey menuNshortcutsSpecs itemSpec label menu menuItem shortcutCharacter target selector isEnabledSelector iconSymbol helpText |
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
						isEnabledSelector := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
						iconSymbol := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
						helpText := itemSpec size >= 8 ifTrue: [ itemSpec eighth ].
						menuItem := label isSymbol
							ifTrue: [ menu addUpdating: label target: target action: selector argumentList: #() ]
							ifFalse: [ menu add: label target: target action: selector argumentList: #() ].
						isEnabledSelector ifNotNil: [ menuItem isEnabled: (target perform: isEnabledSelector) ].
						iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
						helpText ifNotNil: [ menuItem setBalloonText: helpText ]
					]
				]
		].
	^ menu! !

