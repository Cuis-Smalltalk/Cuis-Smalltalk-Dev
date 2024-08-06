'From Cuis7.1 [latest update: #6565] on 6 August 2024 at 4:29:05 pm'!

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 8/6/2024 16:27:19'!
buildMorphicWindow
	"Open a full morphic debugger with the given label"

	| bottomMorph |
	bottomMorph := LayoutMorph newRow.
	bottomMorph
		addMorph: self receiverInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: self receiverInspectorText proportionalWidth: 0.3;
		addAdjusterAndMorph: self contextVariableInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: self contextVariableInspectorText proportionalWidth: 0.3.

	self layoutMorph
		addMorph: self stackList proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55;
		addAdjusterAndMorph: bottomMorph proportionalHeight: 0.2.! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 8/6/2024 16:27:58'!
contextVariableInspector

	^ contextVariableInspector ifNil: [
		contextVariableInspector := (PluggableListMorph
			withModel: model contextVariablesInspector 
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:)
				doubleClickSelector: #inspectContextSelection;
				menuNshortcutsKey: #contextVariablesListMenuSpec ].		! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 8/6/2024 16:28:05'!
receiverInspector

	^ receiverInspector ifNil: [
		receiverInspector := (PluggableListMorph
			withModel: model receiverInspector
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:)
				doubleClickSelector: #inspectReceiverSelection;
				menuNshortcutsKey: #receiverVariablesListMenuSpec ].! !

