'From Cuis7.1 [latest update: #6591] on 15 August 2024 at 4:58:19 pm'!

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'jmv 8/15/2024 16:46:02'!
withAllSuperclassesDownFrom: aSuperclass
	
	| superclasses |
	^ self = aSuperclass
		ifTrue: [ OrderedCollection with: self]
		ifFalse: [
			superclasses := superclass withAllSuperclassesDownFrom: aSuperclass.
			superclasses add: self.
			superclasses ].! !


!UndefinedObject methodsFor: 'class hierarchy' stamp: 'jmv 8/15/2024 16:44:50'!
withAllSuperclassesDownFrom: aSuperclass

	self error: (self superclassNotValidErrorDescriptionFor: aSuperclass)! !


!InnerTextMorph methodsFor: 'menu' stamp: 'jmv 8/15/2024 16:53:42'!
menuNshortcutsSpecsDo: aBlock
	"Subclasses may implement specific ways of providing menu items and
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


!SystemWindow methodsFor: 'submorphs-menus' stamp: 'jmv 8/15/2024 16:54:33'!
menuNshortcutsSpecsFor: aWidget do: aBlock
	"Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for their submorphs."

	aWidget menuNshortcutsKey ifNotNil: [ :widgetKey |
		(self perform: widgetKey) do: aBlock.
		(self class withAllSuperclassesDownFrom: SystemWindow) do: [ :windowClass |
			(windowClass registeredMenuAndShortcutsSpecProviders: widgetKey) ifNotNil: [ :providers |
				providers do: [ :eachMessageSend |
					(eachMessageSend valueWithPossibleArgument: aWidget) do: aBlock ]]]].! !

!methodRemoval: Editor #editorClassesInHierarchyOrderDo: stamp: 'jmv 8/15/2024 16:57:01'!
Editor removeSelector: #editorClassesInHierarchyOrderDo:!
