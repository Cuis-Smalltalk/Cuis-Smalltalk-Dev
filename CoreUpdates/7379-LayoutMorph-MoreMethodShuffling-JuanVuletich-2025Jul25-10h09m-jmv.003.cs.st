'From Cuis7.5 [latest update: #7372] on 25 July 2025 at 10:32:00 am'!

!LinearLayoutMorph commentStamp: 'jmv 7/25/2025 10:14:16' prior: 0!
A row or column of Morphs, does layout by placing them along either a horizontal axis or a vertical axis.

Submorphs might specify a LayoutSpec. If some don't, then, for a column, the column width is taken as the width, and any morph height is kept. Same for rows: submorph width would be maintained, and submorph height would be made equal to row height.

Notes:
  - Contained submorphs may have fixed or minimum sizes.
  - We can specify fixed separation.
  - We can offset to left or right.
  - Contained submorphs can, via LayoutSizeSpecs, ask for a proportion of
    the space which is left over after the minimum extent.

  One subtlety with the proportional layouts is that the sum of
  proportions may add up to more than 100%.

  We want to allocate fairly.

  Say we have 2 submorphs which ask for 80% and 50%.

  They should be normalized to get 80/130 and 50/130 of the excess space.

Instance Variables:
  direction - either #horizontal (row) or #vertical (column).
  gap - Empty space between Morphs:
	gap for both x and y axes or a point: (xGap @ yGap)
  axisEdgeWeight  - Along the Major or Layout Axis, subMorphs may be attracted 
	toward one side (0.0) or the other (1.0).  
	See comment in method #axisEdgeWeight:
  doAdoptWidgetsColor - see method #adoptWidgetsColor:
  useEdgeSpace - When true, x and y separation are kept between submorphs and the
	edge of our extent, othersize no space is kept between submorphs 
	and our outer edge.

!


!LayoutSizeSpec commentStamp: 'jmv 7/25/2025 10:15:05' prior: 0!
LayoutSizeSpecs are the basis for the LinearLayout mechanism. Any Morph can be given a LayoutSizeSpecs, but in order to honor it, its owner must be a LinearLayout.

A LayoutSpec specifies how a morph wants to be layed out. In particular, a LayoutSizeSpec It can specify either a fixed width or a fraction of some available owner width. Same goes for height. If a fraction is specified, a minimum extent is also possible.


Alternatives:
	- proportionalWidth notNil, fixedWidth notNil	->		Use fraction of available space, take fixedWidth as minimum desired width
	- proportionalWidth isNil, fixedWidth isNil		->		Use current morph width
	- proportionalWidth isNil, fixedWidth notNil	->		Use fixedWidth
	- proportionalWidth notNil, fixedWidth isNil	->		NOT VALID

Same goes for proportionalHeight and fixedHeight
!


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 10:12:04'!
addMorphFrontFromWorldPosition: aMorph

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec.
	self addMorphFront: aMorph.
	self layoutSubmorphs.
! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 10:25:51'!
borderWidth: anInteger

	super borderWidth: anInteger.
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/25/2025 10:26:00'!
padding: numberOrPoint
	"Honored by all kinds of Layouts"

	super padding: numberOrPoint.
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 10:22:13'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 10:22:17'!
defaultColor
	^ Theme current defaultWindowColor! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 10:22:21'!
defaultGap
	"Answer the default gap between submorphs for the receiver.
	Redefine this method for a different initial gap in your morphs.
	Only for Linear Layouts"

	^ 0! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 10:22:28'!
doClipSubmorphs
	self setProperty: #doClipSubmorphs toValue: true.! !

!LayoutMorph methodsFor: 'drawing' stamp: 'jmv 7/25/2025 10:18:14'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 7/25/2025 10:21:30'!
layoutSubmorphsIfNeeded
	"Recompute the layout if necessary."

	self isLayoutNeeded ifTrue: [
		self layoutSubmorphs ].! !

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 7/25/2025 10:23:03'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	self layoutNeeded: true.! !

!LayoutMorph methodsFor: 'geometry' stamp: 'jmv 7/25/2025 10:24:35'!
someSubmorphPositionOrExtentChanged
	"Some submorph changed position or extent.
	Must layout submorphs again."

	self layoutNeeded: true.! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 10:11:13'!
addMorph: aMorph layoutSpec: aLayoutSpec
	"Add a submorph, at the bottom or right, with aLayoutSpec"

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec: aLayoutSpec.
	self addMorphFront: aMorph.! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 10:11:49'!
addMorphBack: aMorph
	"Add a submorph, at the top or left, with a default LayoutSpec if none was provided."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec.
	super addMorphBack: aMorph.! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 10:11:13'!
addMorphFront: aMorph
	"Add a submorph, at the bottom or right, with a default LayoutSpec if none was provided."

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec.
	super addMorphFront: aMorph! !

!LayoutMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/25/2025 10:21:22'!
removedMorph: aMorph
	"One of my submorphs has been removed."

	super removedMorph: aMorph.
	self layoutSubmorphs! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 7/25/2025 10:12:09'!
privateAddAllMorphs: aCollection atIndex: index

	aCollection do: [ :m |
		m isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
		m layoutSpec ].
	^ super privateAddAllMorphs: aCollection atIndex: index.! !

!LayoutMorph methodsFor: 'private' stamp: 'jmv 7/25/2025 10:12:13'!
privateAddMorph: aMorph atIndex: index position: aPoint

	aMorph isPlacedMorph ifFalse: [ self error: 'Only PlacedMorphs can be used in Layouts' ].
	aMorph layoutSpec.
	^ super privateAddMorph: aMorph atIndex: index position: aPoint.! !

!LayoutMorph methodsFor: 'testing' stamp: 'jmv 7/25/2025 10:20:23'!
is: aSymbol
	^ aSymbol == #LayoutMorph or: [ super is: aSymbol ]! !

!LayoutMorph methodsFor: 'geometry testing' stamp: 'jmv 7/25/2025 10:18:45'!
clipsSubmorphs
	"LayoutMorphs usually not do not clip submorphs.
	A special case is Transcript. It doesn't include a ScrollPane (that usually does clipping).
	Note: for this to work, we can't be transparent, as transparent morphs can't clip submorphs.
		Senders may also call #doAdoptWidgetsColor.
	There may be other similar cases in the future."

	^self hasProperty: #doClipSubmorphs.! !

!LayoutMorph methodsFor: 'layout in owner' stamp: 'jmv 7/25/2025 10:19:11'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec := LayoutSizeSpec useAll.
	layoutSpec morph: self.

	^ layoutSpec ! !


!FormLayoutMorph methodsFor: 'geometry' stamp: 'jmv 7/25/2025 10:24:13'!
minimumExtent
	"Ojo!!"
	^100@100! !


!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 7/25/2025 10:13:48'!
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
		ifTrue: [ minHeight / (aFactor * self layoutSpec proportionalLayoutHeight) ]! !

!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 7/25/2025 10:14:04'!
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
		ifTrue: [ minWidth / (aFactor * self layoutSpec proportionalLayoutWidth) ]! !


!LayoutMorph class methodsFor: 'LinearLayout examples' stamp: 'jmv 7/25/2025 10:30:30'!
example101VG
"
	self example101VG
"
| pane row |
pane := (LinearLayoutMorph newColumn separation: 5)	name: #example1.
pane color: Color red.

row := LinearLayoutMorph newRow name: #Row1.
row
	color: Color red;
	addMorph: (Sample09Clock new name: #A)
		layoutSpec: (LayoutSizeSpec fixedWidth: 100); 
	addMorph: (Sample01Star new name: #B)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (Sample03Smiley new name: #C)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (ColoredBoxMorph new color: (Color h: 30 s: 0.6 v: 0.6); name: #D)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.15);
	addMorph: (Sample03Smiley new name: #E)
		layoutSpec: (LayoutSizeSpec fixedWidth: 200 fixedHeight: 200).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LinearLayoutMorph newRow separation: 5) name: #Row2.
row
	color: Color lightRed;
	addMorph: (Sample02Bezier new name: #F)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.8);
	addMorph: (Sample02Bezier new  name: #G)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.4);
	addMorph: (Sample02Bezier new  name: #H)
		layoutSpec: (LayoutSizeSpec proportionalWidth: 0.2).
pane addMorph: row layoutSpec: LayoutSizeSpec useAll.

row := (LinearLayoutMorph newRow separation: 5) name: #Row3.
row
	color: Color red;
	addMorph: (ColoredBoxMorph new color: (Color h: 120 s: 0.6 v: 0.6); name: #J)
		layoutSpec: (LayoutSizeSpec fixedWidth: 20 proportionalHeight: 0.8);
	addMorph: (ColoredBoxMorph new color: (Color h: 90 s: 0.6 v: 0.6); name: #K)
		layoutSpec: (LayoutSizeSpec  proportionalWidth: 0.5 fixedHeight: 40);
	addMorph: (ColoredBoxMorph new color: (Color h: 150 s: 0.6 v: 0.6); name: #L)
		layoutSpec: (LayoutSizeSpec fixedWidth: 30 proportionalHeight: 1.0).
pane addMorph: row layoutSpec: (LayoutSizeSpec fixedHeight: 60).
pane position: 150@130 extent: 800@600.
pane openInWorld! !

!LayoutMorph class methodsFor: 'FormLayout examples' stamp: 'jmv 7/25/2025 10:31:38'!
example1Form
"
	self example1Form
"
	| pane |
	pane := (FormLayoutMorph newForm padding: 5) name: #example1.
	pane 
		color: Color red;
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #A)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@20 brEdgesWeight: 0@0 offset: 100@40);
		addMorph: (ColoredBoxMorph new color: (Color blue); name: #B)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@0 offset: 30@20 brEdgesWeight: 1@0 offset: -30@40);
		addMorph: (ColoredBoxMorph new color: (Color lightGreen); name: #C)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@60 brEdgesWeight: 0.5@0 offset: -10@140);
		addMorph: (ColoredBoxMorph new color: (Color green); name: #D)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@0 offset: 10@60 brEdgesWeight: 1@0 offset: -30@140);
		addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@160 brEdgesWeight: 1@1 offset: -30@ -30).
	pane position: 150@130 extent: 400@300.
	pane openInWorld! !


!MenuMorph methodsFor: 'menu' stamp: 'jmv 7/25/2025 10:20:30'!
removeStayUpBox
	| box |
	submorphs isEmpty ifTrue: [^self].
	(submorphs first is: #LayoutMorph) ifFalse: [^self].
	box := submorphs first submorphs second.
	(box is: #PluggableButtonMorph) 
		ifTrue: [ box hide ]! !


!LayoutSpec methodsFor: 'private' stamp: 'jmv 7/25/2025 10:20:25'!
updateContainingLayout
	| owningLayout |
	morph ifNotNil: [
		owningLayout := morph owner.
		(owningLayout is: #LayoutMorph) ifTrue:
			[ owningLayout layoutSubmorphs ]].! !

!methodRemoval: LinearLayoutMorph #defaultColor stamp: 'jmv 7/25/2025 10:22:17'!
LinearLayoutMorph removeSelector: #defaultColor!
!methodRemoval: LinearLayoutMorph #clipsSubmorphs stamp: 'jmv 7/25/2025 10:18:45'!
LinearLayoutMorph removeSelector: #clipsSubmorphs!
!methodRemoval: LinearLayoutMorph #layoutSpec stamp: 'jmv 7/25/2025 10:19:11'!
LinearLayoutMorph removeSelector: #layoutSpec!
!methodRemoval: LinearLayoutMorph #padding: stamp: 'jmv 7/25/2025 10:26:01'!
LinearLayoutMorph removeSelector: #padding:!
!methodRemoval: LinearLayoutMorph #defaultGap stamp: 'jmv 7/25/2025 10:22:21'!
LinearLayoutMorph removeSelector: #defaultGap!
!methodRemoval: LinearLayoutMorph #drawOn: stamp: 'jmv 7/25/2025 10:18:14'!
LinearLayoutMorph removeSelector: #drawOn:!
!methodRemoval: LinearLayoutMorph #addMorphFrontFromWorldPosition: stamp: 'jmv 7/25/2025 10:12:04'!
LinearLayoutMorph removeSelector: #addMorphFrontFromWorldPosition:!
!methodRemoval: LinearLayoutMorph #privateAddAllMorphs:atIndex: stamp: 'jmv 7/25/2025 10:12:09'!
LinearLayoutMorph removeSelector: #privateAddAllMorphs:atIndex:!
!methodRemoval: LinearLayoutMorph #is: stamp: 'jmv 7/25/2025 10:20:09'!
LinearLayoutMorph removeSelector: #is:!
!methodRemoval: LinearLayoutMorph #doClipSubmorphs stamp: 'jmv 7/25/2025 10:22:28'!
LinearLayoutMorph removeSelector: #doClipSubmorphs!
!methodRemoval: LinearLayoutMorph #layoutSubmorphsIfNeeded stamp: 'jmv 7/25/2025 10:21:30'!
LinearLayoutMorph removeSelector: #layoutSubmorphsIfNeeded!
!methodRemoval: LinearLayoutMorph #addMorph:layoutSpec: stamp: 'jmv 7/25/2025 10:11:13'!
LinearLayoutMorph removeSelector: #addMorph:layoutSpec:!
!methodRemoval: LinearLayoutMorph #someSubmorphPositionOrExtentChanged stamp: 'jmv 7/25/2025 10:24:35'!
LinearLayoutMorph removeSelector: #someSubmorphPositionOrExtentChanged!
!methodRemoval: LinearLayoutMorph #addMorphBack: stamp: 'jmv 7/25/2025 10:11:49'!
LinearLayoutMorph removeSelector: #addMorphBack:!
!methodRemoval: LinearLayoutMorph #privateAddMorph:atIndex:position: stamp: 'jmv 7/25/2025 10:12:13'!
LinearLayoutMorph removeSelector: #privateAddMorph:atIndex:position:!
!methodRemoval: LinearLayoutMorph #addMorphFront: stamp: 'jmv 7/25/2025 10:11:13'!
LinearLayoutMorph removeSelector: #addMorphFront:!
!methodRemoval: LinearLayoutMorph #extentChanged: stamp: 'jmv 7/25/2025 10:23:03'!
LinearLayoutMorph removeSelector: #extentChanged:!
!methodRemoval: LinearLayoutMorph #defaultBorderWidth stamp: 'jmv 7/25/2025 10:22:13'!
LinearLayoutMorph removeSelector: #defaultBorderWidth!
!methodRemoval: LinearLayoutMorph #borderWidth: stamp: 'jmv 7/25/2025 10:25:51'!
LinearLayoutMorph removeSelector: #borderWidth:!
!methodRemoval: LinearLayoutMorph #removedMorph: stamp: 'jmv 7/25/2025 10:21:22'!
LinearLayoutMorph removeSelector: #removedMorph:!
!methodRemoval: LayoutMorph #minimumExtent stamp: 'jmv 7/25/2025 10:24:20'!
LayoutMorph removeSelector: #minimumExtent!

!LayoutMorph reorganize!
('accessing' addMorphFrontFromWorldPosition: adoptWidgetsColor: borderWidth: padding:)
('initialization' defaultBorderWidth defaultColor defaultGap doAdoptWidgetsColor doClipSubmorphs initialize)
('drawing' drawOn:)
('layout' layoutBounds layoutSubmorphs layoutSubmorphsIfNeeded)
('geometry' extentChanged: someSubmorphPositionOrExtentChanged)
('submorphs-add/remove' addMorph:layoutSpec: addMorphBack: addMorphFront: removedMorph:)
('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex:position:)
('testing' is:)
('geometry testing' clipsSubmorphs)
('layout in owner' layoutSpec)
!


!FormLayoutMorph reorganize!
('initialization' initialize)
('geometry' minimumExtent)
!


!LinearLayoutMorph reorganize!
('accessing' axisEdgeWeight axisEdgeWeight: gap gap: separation:)
('initialization' beColumn beRow initializeColumn initializeRow)
('adjust' addAdjusterMorph adjustBy:at:)
('convenience methods' addAdjusterAndMorph:fixedHeight: addAdjusterAndMorph:layoutSpec: addAdjusterAndMorph:proportionalHeight: addAdjusterAndMorph:proportionalWidth: addMorph:fixedHeight: addMorph:fixedWidth: addMorph:proportionalHeight: addMorph:proportionalWidth: addMorphKeepMorphHeight: addMorphUseAll: addMorphs: addMorphs:widthProportionalTo:)
('geometry' minimumExtent)
('testing' isColumn isRow)
!

