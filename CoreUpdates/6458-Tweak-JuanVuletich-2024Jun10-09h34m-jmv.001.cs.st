'From Cuis7.1 [latest update: #6457] on 10 June 2024 at 9:35:11 am'!

!SystemWindow methodsFor: 'submorphs-menus' stamp: 'jmv 6/7/2024 15:39:22'!
ownMenuNshortcutsSpecs: aWidgetKey for: aWidget do: aBlock
	"Subclasses may implement specific ways of providing menu items and
	keyboard shortcuts for their submorphs."

	(self perform: aWidgetKey) do: aBlock.

	(self class registeredMenuAndShortcutsSpecProviders: aWidgetKey) ifNotNil: [ :providers |
		providers do: [ :eachMessageSend |
			(eachMessageSend valueWithPossibleArgument: aWidget) do: aBlock ]].! !

