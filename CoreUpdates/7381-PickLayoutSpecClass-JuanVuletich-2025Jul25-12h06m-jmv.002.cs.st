'From Cuis7.5 [latest update: #7380] on 25 July 2025 at 12:34:00 pm'!

!PlacedMorph methodsFor: 'layout-properties' stamp: 'jmv 7/25/2025 11:56:30'!
assureLayoutSpecForOwner: toBeOwner
	"If we haven't been set a layout spec yet, or we carry one that can no longe be used,
	set a default one for our new context."

	(toBeOwner isLayoutSpecOk: layoutSpec ) ifFalse: [
		layoutSpec := nil ].

	layoutSpec ifNil: [
		layoutSpec := toBeOwner defaultLayoutSpecFor: self.
		layoutSpec morph: self ].! !

!PlacedMorph methodsFor: 'layout-properties' stamp: 'jmv 7/25/2025 12:01:06'!
defaultLayoutEdgesSpec
	"Layout specific. Return a default layout spec describing where the
	receiver should appear in a FormLayout.
	Not really useful in practice, just _some_  default."

	^LayoutEdgesSpec
		tlEdgesWeight: 0.5@0.5 offset: -100 @ -10
		brEdgesWeight: 0.5@0.5 offset: 100 @ 10! !

!PlacedMorph methodsFor: 'layout-properties' stamp: 'jmv 7/25/2025 11:51:03'!
defaultLayoutSizeSpec
	"Layout specific. Return a default layout spec describing where the
	receiver should appear in a LinearLayout."

	^LayoutSizeSpec useAll! !

!PlacedMorph methodsFor: 'layout-properties' stamp: 'jmv 7/25/2025 12:30:26'!
useDefaultLayoutSizeSpec
	"Set our default for LinearLayout. Answer it for convenience."

	self layoutSpec: self defaultLayoutSizeSpec.
	^layoutSpec! !


!BoxMorph methodsFor: 'layout-properties' stamp: 'jmv 7/25/2025 11:51:06'!
defaultLayoutSizeSpec
	"Layout specific. Return a default layout spec describing where the
	receiver should appear in a LinearLayout."

	^LayoutSizeSpec keepMorphExtent! !


!LayoutMorph methodsFor: 'testing' stamp: 'jmv 7/25/2025 12:00:29'!
isLayoutSpecOk: aLayoutSpecOrNil
	"Answer whether the argument can work with us."
	
	^self subclassResponsibility! !

!LayoutMorph methodsFor: 'layout in owner' stamp: 'jmv 7/25/2025 11:51:09'!
defaultLayoutSizeSpec
	"Layout specific. Return a default layout spec describing where the
	receiver should appear in a LinearLayout."

	^LayoutSizeSpec useAll! !

!LayoutMorph methodsFor: 'aux' stamp: 'jmv 7/25/2025 11:55:30'!
defaultLayoutSpecFor: aMorph
	^self subclassResponsibility! !


!FormLayoutMorph methodsFor: 'testing' stamp: 'jmv 7/25/2025 12:00:46'!
isLayoutSpecOk: aLayoutSpecOrNil
	"Answer whether the argument can work with us."
	
	^aLayoutSpecOrNil notNil and: [aLayoutSpecOrNil isLayoutEdgesSpec]! !

!FormLayoutMorph methodsFor: 'aux' stamp: 'jmv 7/25/2025 11:55:46'!
defaultLayoutSpecFor: aMorph
	^aMorph defaultLayoutEdgesSpec! !


!LinearLayoutMorph methodsFor: 'testing' stamp: 'jmv 7/25/2025 12:00:51'!
isLayoutSpecOk: aLayoutSpecOrNil
	"Answer whether the argument can work with us."
	
	^aLayoutSpecOrNil notNil and: [aLayoutSpecOrNil isLayoutSizeSpec]! !

!LinearLayoutMorph methodsFor: 'aux' stamp: 'jmv 7/25/2025 12:09:56'!
defaultLayoutSpecFor: aMorph
	^aMorph defaultLayoutSizeSpec! !


!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 7/25/2025 12:32:52'!
allocHeightForFactor: aFactor minLayoutExtentCache: cachedValues

	"Morphs with LayoutSizeSpecs (to be used in LinearLayoutMorphs) may specify
	proportional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minHeight
	so breakeven is when
	  amountToAlloc = minHeight / proportion * factor"
	| minHeight |
	minHeight := (self minimumLayoutExtent: cachedValues) y.
	^(self isProportionalHeight)
		ifFalse:  [ minHeight ]
		ifTrue: [ minHeight / (aFactor * layoutSpec proportionalLayoutHeight) ]! !

!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 7/25/2025 12:32:56'!
allocWidthForFactor: aFactor minLayoutExtentCache: cachedValues

	"Morphs with LayoutSizeSpecs (to be used in LinearLayoutMorphs) may specify
	proportional layouts and minimum extents
	  amountToAlloc * proportion * factor >= minWidth
	so breakeven is when
	  amountToAlloc = minWidth / proportion * factor"
	| minWidth |
	minWidth := (self minimumLayoutExtent: cachedValues) x.
	^(self isProportionalWidth)
		ifFalse:  [ minWidth ]
		ifTrue: [ minWidth / (aFactor * layoutSpec proportionalLayoutWidth) ]! !

!PlacedMorph methodsFor: 'layout-properties' stamp: 'jmv 7/25/2025 12:32:15'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in an owning layout.
	If asked before setting it, and before adding us to some owner,
	just answer nil."

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	owner notNil ifTrue: [
		self assureLayoutSpecForOwner: owner ].
	^ layoutSpec.! !

!PlacedMorph methodsFor: 'layout-properties' stamp: 'jmv 7/25/2025 12:25:56'!
layoutSpec: aLayoutSpec
	"Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout"
	layoutSpec == aLayoutSpec ifTrue: [ ^self ].
	aLayoutSpec morph: self.
	layoutSpec := aLayoutSpec.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !


!FindInSourceDialog methodsFor: 'initialization' stamp: 'jmv 7/25/2025 12:30:39'!
buildWidgets

	| buttonRow titleMorph |
	titleMorph := (LabelMorph contents: 'Find in source code' fontSize: 16 emphasis: 1).
	titleMorph useDefaultLayoutSizeSpec offAxisEdgeWeight: 0.
	searchField := EntryFieldMorph new.
	searchField
		crAction: [ self ok ];
		escAction: [ self cancel ].
	searchField useDefaultLayoutSizeSpec offAxisEdgeWeight: 0.
	initialSearchString ifNotNil: [ searchField contents: initialSearchString ].
	scopeList := PluggableListMorph
		withModel: self
		listGetter: #scopesList
		indexGetter: #scopeIndex
		indexSetter: #scopeIndex:.
	scopeList 
		doubleClickSelector: #ok.
	buttonRow := LinearLayoutMorph newRow.
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


!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 11:58:36'!
addMorphBack: aMorph
	"Add a submorph, at the top or left, with a default LayoutSpec if none was provided."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph assureLayoutSpecForOwner: self.
	super addMorphBack: aMorph.! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 11:58:40'!
addMorphFront: aMorph
	"Add a submorph, at the bottom or right, with a default LayoutSpec if none was provided."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph assureLayoutSpecForOwner: self.
	super addMorphFront: aMorph! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 11:58:45'!
addMorphFrontFromWorldPosition: aMorph

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph assureLayoutSpecForOwner: self.
	self addMorphFront: aMorph.
	self layoutSubmorphs.
! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 7/25/2025 11:58:02'!
privateAddAllMorphs: aCollection atIndex: index

	aCollection do: [ :m |
		m isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
		m assureLayoutSpecForOwner: self ].
	^ super privateAddAllMorphs: aCollection atIndex: index.! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 7/25/2025 11:58:52'!
privateAddMorph: aMorph atIndex: index position: aPoint

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph assureLayoutSpecForOwner: self.
	^ super privateAddMorph: aMorph atIndex: index position: aPoint.! !

!methodRemoval: LayoutMorph #layoutSpec stamp: 'jmv 7/25/2025 12:25:00'!
LayoutMorph removeSelector: #layoutSpec!
!methodRemoval: BoxMorph #layoutSpec stamp: 'jmv 7/25/2025 12:25:02'!
BoxMorph removeSelector: #layoutSpec!
