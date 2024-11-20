'From Cuis7.1 [latest update: #6826] on 20 November 2024 at 3:28:25 pm'!

!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 11/20/2024 15:28:06'!
buildMorphicWindow

	| textMorph |
	listMorph := (HierarchicalListMorph
			withModel: model
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:)
				menuNshortcutsKey: #listMenuSpec.
	listMorph autoDeselect: false.
	listMorph doubleClickSelector: #inspectSelection.
	"Expand the first level of the tree."
	listMorph selectionIndex: 1; navigateRight.
	textMorph := ((TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression (self is selected item)').
	self layoutMorph
		addMorph: listMorph proportionalHeight: 0.8;
		addAdjusterAndMorph: textMorph proportionalHeight: 0.2.
	self setLabel: model rootObject shortPrintString.
! !

