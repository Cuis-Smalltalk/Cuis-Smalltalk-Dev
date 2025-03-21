'From Cuis7.3 [latest update: #7081] on 19 March 2025 at 12:22:05 pm'!

!SystemWindow methodsFor: 'submorphs-menus' stamp: 'jmv 3/19/2025 12:10:55'!
allRegisteredMenuAndShortcutsSpecProviders: widgetKey do: aBlock widget: aWidget
	"Hierarchy order allows more specific classes to override choices made by superclasses."
	(self class withAllSuperclassesDownFrom: (self menuAndShortcutRegistryRoot: widgetKey)) do:
		[ :windowClass |
			(windowClass registeredMenuAndShortcutsSpecProviders: widgetKey) ifNotNil: [ :providers |
				providers do:
					[ :eachMessageSend |
						(eachMessageSend valueWithPossibleArgument: aWidget) do: aBlock ]]].! !

!SystemWindow methodsFor: 'submorphs-menus' stamp: 'jmv 3/19/2025 12:06:18'!
menuAndShortcutRegistryRoot: widgetKey
	"Answer the topmost class whose menu and shortcut registry we honor.
	It is SystemWindow by default.
	But classes redefining menus may want to ignore providers registered assuming menus that they redefine.
	See inheritance."
	^SystemWindow! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 3/19/2025 12:15:40'!
menuAndShortcutRegistryRoot: widgetKey
	"Answer the topmost class whose menu and shortcut registry we honor.
	It is SystemWindow by default.
	But classes redefining menus may want to ignore providers registered assuming menus that they redefine.
	See inheritance."
	(#(classListMenuSpec messageCatListMenuSpec messageListMenuSpec) pointsTo: widgetKey)
		ifTrue: [ ^CodeFileBrowserWindow ].
	^super menuAndShortcutRegistryRoot: widgetKey! !


!EntryFieldMorph methodsFor: 'menu - shortcuts' stamp: 'jmv 3/19/2025 11:58:09'!
menuNshortcutsSpecsDo: aBlock
	"Iterate over our own menu and shortcut specs.
	Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for themselves.
	See implementors."

	"First take default editor menu and shortcut specs"
	self editor defaultMenuSpec do: aBlock.

	self window ifNotNil: [ :myWindow |

		"Then include additions, overrides and deletions registered for #Editor, #TextEditor, #SmalltalkEditor, etc.
		Hierarchy order allows more specific classes to override choices made by superclasses."
		(self editor class withAllSuperclassesDownFrom: Editor) do: [ :editorClass |
			myWindow allRegisteredMenuAndShortcutsSpecProviders: editorClass name do: aBlock widget: self ]].! !


!SystemWindow methodsFor: 'submorphs-menus' stamp: 'jmv 3/19/2025 12:15:04'!
menuNshortcutsSpecsFor: aWidget do: aBlock
	"aWidget is a submorph, or submorph of a submorph, etc.
	Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for their submorphs."

	aWidget menuNshortcutsKey ifNotNil: [ :widgetKey |
		(widgetKey numArgs = 0
			ifTrue: [self perform: widgetKey]
			ifFalse: [self perform: widgetKey with: aWidget]) do: aBlock.
		self allRegisteredMenuAndShortcutsSpecProviders: widgetKey do: aBlock widget: aWidget ].! !


!InnerTextMorph methodsFor: 'menu' stamp: 'jmv 3/19/2025 12:20:19'!
menuNshortcutsSpecsDo: aBlock
	"Iterate over our own menu and shortcut specs.
	Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for themselves.
	See implementors."

	"First take default editor menu and shortcut specs"
	self editor defaultMenuSpec do: aBlock.

	self window ifNotNil: [ :myWindow |

		"Then include additions, overrides and deletions registered for #Editor, #TextEditor, #SmalltalkEditor, etc.
		Hierarchy order allows more specific classes to override choices made by superclasses."
		(self editor class withAllSuperclassesDownFrom: Editor) do: [ :editorClass |
			myWindow allRegisteredMenuAndShortcutsSpecProviders: editorClass name do: aBlock widget: self ].

		"Also allow registering menu+shortcut entries for specific TextModels or TextProviders"
		myWindow
			allRegisteredMenuAndShortcutsSpecProviders: self
			textProviderOrModel class name
			do: aBlock
			widget: self ].! !

