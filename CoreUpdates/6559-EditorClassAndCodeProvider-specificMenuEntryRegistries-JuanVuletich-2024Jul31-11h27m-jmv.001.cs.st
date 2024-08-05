'From Cuis7.1 [latest update: #6552] on 31 July 2024 at 11:30:46 am'!

!Editor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 7/29/2024 16:00:01'!
editorClassesInHierarchyOrderDo: aBlock
	"maybe name differently"
	(self class withAllSuperclassesUpTo: Editor) reverseDo: aBlock! !


!InnerTextMorph methodsFor: 'menu' stamp: 'jmv 7/31/2024 11:30:11'!
menuNshortcutsSpecsDo: aBlock
	"Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for themselves.
	See implementors."

	"First take default editor menu and shortcut specs"
	self editor defaultMenuSpec do: aBlock.

	self window ifNotNil: [ :window |
		"Then include additions, overrides and deletions registered for #Editor, #TextEditor, #SmalltalkEditor, etc.
		Hierarchy order allows more specific classes to override choices made by superclasses."
		self editor editorClassesInHierarchyOrderDo: [ :editorClass |
			(window class registeredMenuAndShortcutsSpecProviders: editorClass name) ifNotNil: [ :providers |
				providers do: [ :eachMessageSend |
					(eachMessageSend valueWithPossibleArgument: self) do: aBlock ]]		].
		"Also allow registering menu+shortcut entries for specific TextModels or TextProviders"
		(window class registeredMenuAndShortcutsSpecProviders: self textProviderOrModel class name) ifNotNil: [ :providers |
			providers do: [ :eachMessageSend |
				(eachMessageSend valueWithPossibleArgument: self) do: aBlock ]]		.]! !

