'From Cuis7.1 [latest update: #6605] on 19 August 2024 at 12:47:19 pm'!

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 8/19/2024 12:37:38'!
menuNshortcutsSpecsDo: aBlock
	"Iterate over our own menu and shortcut specs.
	Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for themselves.
	See implementors."! !

!Morph methodsFor: 'submorphs-menus' stamp: 'jmv 8/19/2024 12:36:30'!
menuNshortcutsSpecsFor: aWidget do: aBlock
	"aWidget is a submorph, or submorph of a submorph, etc.
	Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for their submorphs.
	See implementors."! !


!InnerTextMorph methodsFor: 'menu' stamp: 'jmv 8/19/2024 12:37:44'!
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
			(myWindow class withAllSuperclassesDownFrom: SystemWindow) do: [ :windowClass |
				(windowClass registeredMenuAndShortcutsSpecProviders: editorClass name) ifNotNil: [ :providers |
					providers do: [ :eachMessageSend |
						(eachMessageSend valueWithPossibleArgument: self) do: aBlock ]]		]].
		
		(myWindow class withAllSuperclassesDownFrom: SystemWindow) do: [ :windowClass |
			"Also allow registering menu+shortcut entries for specific TextModels or TextProviders"
			(windowClass registeredMenuAndShortcutsSpecProviders: self textProviderOrModel class name) ifNotNil: [ :providers |
				providers do: [ :eachMessageSend |
					(eachMessageSend valueWithPossibleArgument: self) do: aBlock ]]		]].! !


!SystemWindow methodsFor: 'submorphs-menus' stamp: 'jmv 8/19/2024 12:42:57'!
menuNshortcutsSpecsFor: aWidget do: aBlock
	"aWidget is a submorph, or submorph of a submorph, etc.
	Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for their submorphs."

	aWidget menuNshortcutsKey ifNotNil: [ :widgetKey |
		(widgetKey numArgs = 0
			ifTrue: [self perform: widgetKey]
			ifFalse: [self perform: widgetKey with: aWidget]) do: aBlock.
		(self class withAllSuperclassesDownFrom: SystemWindow) do: [ :windowClass |
			(windowClass registeredMenuAndShortcutsSpecProviders: widgetKey) ifNotNil: [ :providers |
				providers do: [ :eachMessageSend |
					(eachMessageSend valueWithPossibleArgument: aWidget) do: aBlock ]]]].! !

