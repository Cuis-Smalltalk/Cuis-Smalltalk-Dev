"
Morphic view for ChangeList models. See category 'GUI building'.
"
Class {
	#name : 'ChangeListWindow',
	#superclass : 'CodeWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'menu commands' }
ChangeListWindow >> browseCurrentVersionsOfSelections [
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"

	| currentVersions |
	currentVersions := model currentVersionsOfSelections.
	currentVersions isEmpty ifTrue: [
		^ self inform: 'no selected methods have in-memory counterparts' ].
	MethodSetWindow
		openMessageListUnsorted: currentVersions
		label: 'Current versions of selected methods in ', model file localName.
]

{ #category : 'menu commands' }
ChangeListWindow >> browseVersions [

	| method class browser firstChange |
	method := model currentCompiledMethod.
	method isNil ifTrue: [
		model selectedClass ifNotNil: [ :selectedClass |
			^ VersionsBrowserWindow
				browseCommentOf: selectedClass ]].
	firstChange := model xtraVersion.
	firstChange ifNil: [ ^self ].
	class := method methodClass.
	browser := VersionsBrowserWindow
		browseVersionsOf: method
		class: class
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector
		extraChangeRecord: firstChange.
	^browser
]

{ #category : 'GUI building' }
ChangeListWindow >> buildMorphicWindow [
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPane desiredExtent desiredHeight wine availableHeight |
	listPane := (PluggableListMorphOfMany
		withModel: model
		listGetter: #list
		primarySelectionGetter: #listIndex
		primarySelectionSetter: #toggleListIndex:
		listSelectionGetter: #listSelectionAt:
		listSelectionSetter: #listSelectionAt:put:)
			menuNshortcutsKey: #listMenuSpec.
	desiredExtent := listPane desiredExtent.
	desiredHeight := desiredExtent y.
	wine := RealEstateAgent standardWindowExtent.
	availableHeight := wine y - self labelHeight - (borderWidth * 2).
	desiredExtent x > wine x ifTrue: [
		desiredHeight := desiredHeight + Theme current scrollbarThickness ].
	desiredHeight / availableHeight < 0.4
		ifTrue: [		"Number of items is small enough to make for a fixed list regardless of Window resize"
			self layoutMorph
				addMorph: listPane fixedHeight: desiredHeight;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 ]
		ifFalse: [		"Number of items is large enough to prefer a list proportional to Window resize"
			self layoutMorph
				addMorph: listPane proportionalHeight: 0.4;
				addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
			listPane limitLayoutHeight ].
]

{ #category : 'GUI building' }
ChangeListWindow >> createCodePaneMorph [

	^(TextModelMorph
		textProvider: model
		textGetter: #acceptedContents) emptyTextDisplayMessage: 'Selection detail (no change selected?)'
]

{ #category : 'menu & kbd shortcuts' }
ChangeListWindow >> listMenuSpec [
	"Spec for the menu and shortcut keys for the changes list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Change List' 									true)
		#(10	'File In selections' 									nil 	model 	fileInSelections 						nil
					'import the selected items into the image')
		#(20	'File Out selections...	' 							nil 	model 	fileOutSelections 						nil
		 			'create a new file containing the selected items')
		#(30	'File Out current version of selections...' 			nil 	model 	fileOutCurrentVersionsOfSelections 	nil
 					'create a new file containing the current (in-image) counterparts of the selected methods')
		#(40 	'-----')
		#(50	'Compare to Current' 								nil 	model 	compareToCurrentVersion 				nil
 					'open a separate window which shows the text differences between the on-file version and the in-image version.')
		#(60 	'-----')
		#(70	'Select new methods' 								nil 	model 	selectNewMethods 					nil
 					'select methods in the file that do not currently exist in the image')
		#(80	'Select changes for Absent Classes' 				nil 	model 	selectAllForAbsentClasses 				nil
					'select methods in the file for classes that are not defined in the image')
		#(90	'Select all changes for this Class' 					nil 	model 	selectAllForThisClass 					nil
					'select all methods in the file that belong to the currently-selected class')
		#(100	'Select Unchanged methods' 						nil 	model 	selectUnchangedMethods 				nil
					'select methods in the file whose in-image versions are the same as their in-file counterparts')
		#(105	'Select changes with contents matching'			nil	model	selectContentsMatching				nil
					'select all changes in the file whose text matches a pattern or includes a string')

		#(110	'Select methods Equivalent to current' 				nil 	model 	selectEquivalentMethods 				nil
					'select methods in the file whose in-image versions have the same behavior as their in-file counterparts')
		#(120	'Select methods Older than current' 				nil 	model 	selectMethodsOlderThanCurrent 		nil
					'select methods in the file that are older than the one currently in the image')
		#(130	'Select removals of Sent methods' 					nil 	model 	selectRemovalsOfSent 					nil
					'select all method removals of methods that have some sender in the image')
		#(140 	'-----')
		#(150	'Select All (a)' 									$a 	model 	selectAll 								nil
					'select all the items in the list')
		#(160	'Deselect All (A)' 									$A 	model 	deselectAll 								nil
					'deselect all the items in the list')
		#(170	'Invert Selections' 								nil 	model 	invertSelections 						nil
					'select every item that is not currently selected, and deselect every item that *is* currently selected')
		#(180 	'-----')
		#(190	'Browse class and method' 						nil 	window browseMethodFull 					nil
					'open a full browser showing the selected method')
		#(200	'Browse all versions of single selection' 			nil 	window browseVersions 						nil
					'open a version browser showing the versions of the currently selected method')
		#(210	'Browse current versions of selections' 				nil 	window browseCurrentVersionsOfSelections 	nil
					'open a message-list browser showing the current (in-image) counterparts of the selected methods')
		#(220	'Destroy current methods of selections' 			nil 	model 	destroyCurrentCodeOfSelections 		nil
					'remove (*destroy*) the in-image counterparts of all selected methods')
		#(230 	'-----')
		#(240	'Remove doIts' 									nil 	model 	removeDoIts 							nil
					'remove all items that are doIts rather than definitions')
		#(250	'Remove older versions' 							nil 	model 	removeOlderMethodVersions 			nil
					'remove all but the most recent versions of methods in the list')
		#(260	'Remove up-to-date versions' 					nil 	model 	removeUpToDate 						nil
					'remove all items whose code is the same as the counterpart in-image code')
		#(270	'Remove empty class comments' 					nil 	model 	removeEmptyClassComments 			nil
					'remove all empty class comments')
		#(280	'Remove selected items' 							nil 	model 	removeSelections 						nil
					'remove the selected items from the change-list')
		#(290	'Remove unselected items' 						nil 	model 	removeNonSelections 					nil
					'remove all the items not currently selected from the change-list')
	)
]

{ #category : 'GUI building' }
ChangeListWindow >> optionalButtonRow [
	"Answer a row of buttons to occur in a tool pane"

	| row buttons widths buttonColor |
	buttons := OrderedCollection new.
	widths := OrderedCollection new.
	buttonColor := self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button := PluggableButtonMorph 
					model: model
					action: tuple third.
		button color: buttonColor.
		button label: tuple second.
		buttons add: button.
		button setBalloonText: tuple fourth].
	buttons add: self lineDiffButton.
	widths add: 14.
	buttons add: self wordDiffButton.
	widths add: 16.
	model wantsPrettyDiffOption ifTrue: [
		buttons add:  self prettyLineDiffButton.
		widths add: 21.
		buttons add:  self prettyWordDiffButton.
		widths add: 23 ].
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row
]

{ #category : 'GUI building' }
ChangeListWindow >> optionalModelButtonTuples [

	^#(
		(11		'select all' 			selectAll				'select all entries')
		(14		'deselect all'		deselectAll			'deselect all entries')
		(19		'file in selections' 	fileInSelections		'file in all selected entries')
	)
]

{ #category : 'GUI building' }
ChangeListWindow >> windowColor [
	^ Theme current changeList
]
