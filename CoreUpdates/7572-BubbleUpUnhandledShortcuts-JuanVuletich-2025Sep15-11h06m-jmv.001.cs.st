'From Cuis7.5 [latest update: #7569] on 15 September 2025 at 11:44:03 am'!

!Morph methodsFor: 'events' stamp: 'jmv 9/15/2025 11:36:20'!
deliverKeyboardShortcut: aKeyboardEvent
	"aKeyboardEvent is keyboard shortcut command.
	Just handle it if it is appropriate for us.
	Otherwise, let a willing owner handle it."

	(self keyboardShortcut: aKeyboardEvent) ifTrue: [
		^true].
	owner ifNotNil: [
		^owner deliverKeyboardShortcut: aKeyboardEvent ].
	^false.! !


!Morph methodsFor: 'events' stamp: 'jmv 9/15/2025 11:43:35'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event.
	Subclasses should call super, and check if the event was handled."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [
			aKeyboardEvent wasHandled: true.
			^self ].

	aKeyboardEvent anyModifierKeyPressed ifTrue: [
		(self deliverKeyboardShortcut: aKeyboardEvent)
			ifTrue: [ aKeyboardEvent wasHandled: true ]].! !

!Morph methodsFor: 'events' stamp: 'jmv 9/15/2025 11:29:20'!
keyboardShortcut: aKeyboardEvent
	"Handle a keyboard shortcut.
	See coments at Morph >> #getMenu and Morph >> #allMenuNshortcutsSpecs
	"
	| menuNshortcutsSpecs shortcutCharacter target argument |
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
						selector numArgs = 0
							ifTrue: [target perform: selector]
							ifFalse: [
								argument := itemSpec size >= 8 ifTrue: [ itemSpec eighth ] ifFalse: [ self ].
								target perform: selector with: argument ].
						^true ]
					ifNil: [
						^false ]
				]
			]
		].
	^false! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 9/15/2025 11:28:39'!
allMenuNshortcutsSpecs
	"Spec for the menu and shortcut keys.
	This is the main method to get the specs, either to open a Menu or to start a Keyboard Shortcut action.
	See senders: #getMenu and #keyboardShortcut:. See #getMenu for details of the spec format.
	This default implementation allows any morph to define their own #menuNshortcutsSpecsDo: although few do.
	It also traverses owners, as an owner may specify menu-and-shortcut specs for their submorphs.
	Main users of this are SystemWindow. See comment at SystemWindow >> #menuNshortcutsSpecsFor:do:
	Any morph redefining this method will take full control of its own menu and keyboard shortcuts."

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


!SystemWindow methodsFor: 'submorphs-menus' stamp: 'jmv 9/15/2025 11:22:16'!
menuNshortcutsSpecsFor: aWidget do: aBlock
	"aWidget belongs in our submorphs tree.
	Some morphs (for instance PluggableMorphs) may have a #menuNshortcutsKey, provided by us on
	creation, that is used here to pick the proper menu-and-shortcut specs for it.
	SystemWindow also provides a class level registry that allows adding / modifying / removing shortcuts
	and menu entries by subclassing the Window class. See class ivar 'menuNshortcutsProviders'. "

	aWidget menuNshortcutsKey ifNotNil: [ :widgetKey |
		(widgetKey numArgs = 0
			ifTrue: [self perform: widgetKey]
			ifFalse: [self perform: widgetKey with: aWidget]) do: aBlock.
		self allRegisteredMenuAndShortcutsSpecProviders: widgetKey do: aBlock widget: aWidget ].! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 9/15/2025 11:42:05'!
keyboardShortcut: aKeyboardEvent
	"Handle a keyboard shortcut.
	See coment at Morph >> #getMenu
	"
	"ctrl and option (if on Mac) are used to enter #normalCharacter:
	Only cmd is considered a shortcut generating modifier."
	^(aKeyboardEvent commandAltKeyPressed and: [aKeyboardEvent keyValue between: 32 and: 126])
		ifTrue: [super keyboardShortcut: aKeyboardEvent]
		ifFalse: [false].! !
