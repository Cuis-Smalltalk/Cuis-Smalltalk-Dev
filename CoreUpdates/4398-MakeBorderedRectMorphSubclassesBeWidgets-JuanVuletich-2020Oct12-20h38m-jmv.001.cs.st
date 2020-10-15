'From Cuis 5.0 [latest update: #4392] on 12 October 2020 at 8:42:06 pm'!
!classDefinition: #PluggableMorph category: #'Morphic-Widgets'!
WidgetMorph subclass: #PluggableMorph
	instanceVariableNames: 'model'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #TileResizeMorph category: #'Morphic-Widgets'!
WidgetMorph subclass: #TileResizeMorph
	instanceVariableNames: 'selectedResize selectionColor action'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #ProgressBarMorph category: #'Morphic-Composite Widgets'!
WidgetMorph subclass: #ProgressBarMorph
	instanceVariableNames: 'value progressColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
!classDefinition: #StringRequestMorph category: #'Morphic-Composite Widgets'!
WidgetMorph subclass: #StringRequestMorph
	instanceVariableNames: 'response acceptBlock cancelBlock validationBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
!classDefinition: #MenuMorph category: #'Morphic-Menus'!
WidgetMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp titleMorph activeSubMenu popUpOwner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
WidgetMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity originalPosition showUp itemsPerPage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
