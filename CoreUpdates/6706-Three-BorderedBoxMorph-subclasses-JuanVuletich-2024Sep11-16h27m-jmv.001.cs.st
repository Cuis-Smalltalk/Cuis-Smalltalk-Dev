'From Cuis7.1 [latest update: #6702] on 11 September 2024 at 4:28:57 pm'!
!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedBoxMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp titleMorph activeSubMenu popUpOwner prevKbdFocus prevMouseFocus '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!classDefinition: #TileResizeMorph category: #'Morphic-Widgets'!
BorderedBoxMorph subclass: #TileResizeMorph
	instanceVariableNames: 'selectedResize selectionColor action '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedBoxMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity originalPosition showUp itemsPerPage downButtonForm upButtonForm '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!

!AutoCompleterMorph methodsFor: 'drawing' stamp: 'jmv 9/11/2024 15:51:03'!
drawContainingRectangle: aCanvas
	
	aCanvas frameAndFillRectangle: self localBounds fillColor: color borderWidth: borderWidth borderColor: borderColor.
! !

!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedBoxMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp titleMorph activeSubMenu popUpOwner prevKbdFocus prevMouseFocus'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!classDefinition: #TileResizeMorph category: #'Morphic-Widgets'!
BorderedBoxMorph subclass: #TileResizeMorph
	instanceVariableNames: 'selectedResize selectionColor action'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedBoxMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity originalPosition showUp itemsPerPage downButtonForm upButtonForm'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
