'From Cuis 6.0 [latest update: #5494] on 13 October 2022 at 3:40:32 pm'!

!DebuggerWindow methodsFor: 'GUI building' stamp: 'hlsf 10/13/2022 12:32:34'!
contextVariableInspector

	^ contextVariableInspector ifNil: [
		contextVariableInspector _ PluggableListMorph
			model: model contextVariablesInspector 
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #contextFieldListMenu
			keystrokeAction: #inspectorKey:from: ]! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'hlsf 10/13/2022 12:34:09'!
contextVariableInspectorText

	^ contextVariableInspectorText ifNil: [
		contextVariableInspectorText _ (TextModelMorph
			textProvider: model contextVariablesInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Context scope' ]! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'hlsf 10/13/2022 12:28:28'!
receiverInspector

	^ receiverInspector ifNil: [
		receiverInspector _ PluggableListMorph
			model: model receiverInspector
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #receiverFieldListMenu
			keystrokeAction: #inspectorKey:from: ]! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'hlsf 10/13/2022 12:31:07'!
receiverInspectorText

	^ receiverInspectorText ifNil: [
		receiverInspectorText _ (TextModelMorph
			textProvider: model receiverInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Receiver scope' ]! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'hlsf 10/13/2022 12:30:03'!
stackList

	^ stackList ifNil: [
		stackList _ PluggableListMorph
			model: model 
			listGetter: #contextStackList
			indexGetter: #contextStackIndex
			indexSetter: #toggleContextStackIndex:
			mainView: self
			menuGetter: #contextStackMenu
			keystrokeAction: #contextStackKey:from: ]! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'hlsf 10/13/2022 12:34:38'!
buildMorphicWindow
	"Open a full morphic debugger with the given label"

	| bottomMorph |

	self receiverInspector doubleClickSelector: #inspectSelection.
	self contextVariableInspector doubleClickSelector: #inspectSelection.

	bottomMorph _ LayoutMorph newRow.
	bottomMorph
		addMorph: self receiverInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: self receiverInspectorText proportionalWidth: 0.3;
		addAdjusterAndMorph: self contextVariableInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: self contextVariableInspectorText proportionalWidth: 0.3.

	self layoutMorph
		addMorph: self stackList proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55;
		addAdjusterAndMorph: bottomMorph proportionalHeight: 0.2! !

