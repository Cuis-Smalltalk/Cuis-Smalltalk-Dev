'From Cuis7.1 [latest update: #6441] on 4 June 2024 at 2:40:29 pm'!

!Morph methodsFor: 'events' stamp: 'jmv 6/4/2024 13:20:13'!
keyboardShortcut: aKeyboardEvent
	"Handle a keyboard shortcut.
	See coment at Morph >> #getMenu
	"
	| menuNshortcutsSpecs shortcutCharacter target widgetKey |
	widgetKey := self menuNshortcutsKey ifNil: [ ^false ].
	menuNshortcutsSpecs := self menuNshortcutsSpec: widgetKey for: self.
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
						target perform: selector.
						^true ]
					ifNil: [
						^false ]
				]
			]
		].
	^false! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 6/4/2024 13:20:06'!
addItemTo: topMenu fromSpec: itemSpec submenus: submenusDictionary
	"
	See coment at Morph >> #getMenu
	"
	| submenuId menu label target selector iconSymbol helpText menuItem |
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
	menuItem := label isSymbol
		ifTrue: [ menu addUpdating: label target: target action: selector argumentList: #() ]
		ifFalse: [ menu add: label target: target action: selector argumentList: #() ].
	selector ifNil: [
		menuItem isEnabled: false ].
	iconSymbol := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
	iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
	helpText := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
	helpText ifNotNil: [ menuItem setBalloonText: helpText ].! !

