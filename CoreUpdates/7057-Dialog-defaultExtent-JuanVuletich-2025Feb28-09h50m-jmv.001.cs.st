'From Cuis7.3 [latest update: #7055] on 28 February 2025 at 9:55:26 am'!

!Dialog methodsFor: 'initialization' stamp: 'jmv 2/28/2025 09:53:07'!
buildWidgets
	"Redefine as appropriate.
	At the end, do
		self ensureMinimumExtent.
	or
		self morphExtent: self minimumExtent + xtraSpace.
	or similar, as #defaultExtent is nil."

	self subclassResponsibility.! !


!Dialog methodsFor: 'defaults' stamp: 'jmv 2/28/2025 09:10:52'!
defaultExtent
	^nil! !


!FindInSourceDialog methodsFor: 'initialization' stamp: 'jmv 2/28/2025 09:13:08'!
buildWidgets

	| buttonRow titleMorph |
	titleMorph := (LabelMorph contents: 'Find in source code' fontSize: 16 emphasis: 1).
	titleMorph layoutSpec offAxisEdgeWeight: 0.
	searchField := EntryFieldMorph new.
	searchField
		crAction: [ self ok ];
		escAction: [ self cancel ].
	searchField layoutSpec offAxisEdgeWeight: 0.
	initialSearchString ifNotNil: [ searchField contents: initialSearchString ].
	scopeList := PluggableListMorph
		withModel: self
		listGetter: #scopesList
		indexGetter: #scopeIndex
		indexSetter: #scopeIndex:.
	scopeList 
		doubleClickSelector: #ok.
	buttonRow := LayoutMorph newRow.
	buttonRow
		addMorph: (PluggableButtonMorph model: self action: #ok label: 'Ok');
		addMorph: (PluggableButtonMorph model: self action: #cancel label: 'Cancel').
	layoutMorph
		gap: 14;
		addMorphKeepMorphHeight: titleMorph;
		addMorphKeepMorphHeight: searchField;
		addMorphUseAll: scopeList;
		addMorphKeepMorphHeight: buttonRow.

	self morphExtent: self minimumExtent * (2@1.5).! !


!SampleModalDialog methodsFor: 'initialization' stamp: 'jmv 2/28/2025 09:54:03'!
buildWidgets
	"
	This is a sample implementation. Actual subclasses should reimplement #buildWidgets (without calling super!!).
	self openModal
	"

	| typeRow buttonRow |
	typeRow := LayoutMorph newRow.
	typeRow
		addMorph: (LabelMorph contents: 'Type');
		addMorph: (LabelMorph contents: 'So what');
		addMorph: (LabelMorph contents: 'And then').
	buttonRow := LayoutMorph newRow.
	buttonRow
		padding: 10;
		gap: 20;
		addMorph: (PluggableButtonMorph model: self action: #ok label: 'Ok');
		addMorph: (PluggableButtonMorph model: self action: #cancel label: 'Cancel').
	layoutMorph
		addMorph: 
			((LabelMorph contents: 'This is a title for this Dialog' fontSize: 25)
				borderWidth: 2; borderColor: color lighter;
				padding: 20);
		addMorph: Sample01Star new;
		addMorph: typeRow;
		addMorph: buttonRow.

	"self ensureMinimumExtent."
	"To make the star somehow larger"
	self morphExtent: self minimumExtent + (0@200).! !

!methodRemoval: SampleModalDialog #defaultExtent stamp: 'jmv 2/28/2025 09:51:59'!
SampleModalDialog removeSelector: #defaultExtent!
!methodRemoval: FindInSourceDialog #defaultExtent stamp: 'jmv 2/28/2025 09:51:59'!
FindInSourceDialog removeSelector: #defaultExtent!

!Dialog reorganize!
('initialization' buildWidgets)
('defaults' defaultColor defaultExtent defaultPadding)
('layout' beColumn)
('geometry' extentChanged: minimumExtent)
!


!SampleModalDialog reorganize!
('initialization' buildWidgets)
!

