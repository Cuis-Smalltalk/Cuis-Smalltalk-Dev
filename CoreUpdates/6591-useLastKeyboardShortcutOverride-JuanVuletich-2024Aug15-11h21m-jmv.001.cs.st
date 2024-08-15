'From Cuis7.1 [latest update: #6590] on 15 August 2024 at 11:24:04 am'!

!Morph methodsFor: 'events' stamp: 'jmv 8/15/2024 11:23:41'!
keyboardShortcut: aKeyboardEvent
	"Handle a keyboard shortcut.
	See coment at Morph >> #getMenu
	"
	| menuNshortcutsSpecs shortcutCharacter target |
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

