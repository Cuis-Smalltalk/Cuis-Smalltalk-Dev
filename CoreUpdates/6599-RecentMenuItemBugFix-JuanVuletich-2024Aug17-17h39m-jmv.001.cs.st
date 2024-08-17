'From Cuis7.1 [latest update: #6598] on 17 August 2024 at 5:40:32 pm'!

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 8/17/2024 17:35:42'!
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
	arguments := (selector isNil or: [ selector numArgs = 0 ])
		ifTrue: [#()]
		ifFalse: [itemSpec size >= 8 ifTrue: [ {itemSpec eighth} ] ifFalse: [ {self} ]].
	menuItem := label isSymbol
		ifTrue: [ menu addUpdating: label target: target action: selector argumentList: arguments ]
		ifFalse: [ menu add: label target: target action: selector argumentList: arguments ].
	selector ifNil: [
		menuItem isEnabled: false ].
	iconSymbol := itemSpec size >= 6 ifTrue: [ itemSpec sixth ].
	iconSymbol ifNotNil: [ menuItem setIcon: iconSymbol ].
	helpText := itemSpec size >= 7 ifTrue: [ itemSpec seventh ].
	helpText ifNotNil: [ menuItem setBalloonText: helpText ].! !

