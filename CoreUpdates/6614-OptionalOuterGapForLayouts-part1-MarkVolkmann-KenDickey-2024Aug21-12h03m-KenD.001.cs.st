'From Cuis7.1 [latest update: #6613] on 21 August 2024 at 12:09:04 pm'!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BoxedMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation axisEdgeWeight doAdoptWidgetsColor useEdgeSpace '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutMorph commentStamp: 'KenD 8/21/2024 12:07:09' prior: 0!
A row or column of Morphs, does layout by placing them along either a horizontal axis or a vertical axis.

Submorphs might specify a LayoutSpec. If some don't, then, for a column, the column width is taken as the width, and any morph height is kept. Same for rows: submorph width would be maintained, and submorph height would be made equal to row height.

Notes:
  - Contained submorphs may have fixed or minimum sizes.
  - We can specify fixed separation.
  - We can offset to left or right.
  - Contained submorphs can, via LayoutSpecs, ask for a proportion of
    the space which is left over after the minimum extent.

  One subtlety with the proportional layouts is that the sum of
  proportions may add up to more than 100%.

  We want to allocate fairly.

  Say we have 2 submorphs which ask for 80% and 50%.

  They should be normalized to get 80/130 and 50/130 of the excess space.

Instance Variables:
  direction - either #horizontal (row) or #vertical (column).
  separation - Empty space between Morphs:
	separation for both x and y axes or a point: (xSeparation @ ySeparation)
  axisEdgeWeight  - Along the Major or Layout Axis, subMorphs may be attracted 
	toward one side (0.0) or the other (1.0).  
	See comment in method #axisEdgeWeight:
  doAdoptWidgetsColor - see method #adoptWidgetsColor:
  useEdgeSpace - When true, x and y separation are kept between submorphs and the
	edge of our extent, othersize no space is kept between submorphs 
	and our outer edge.

!


!LayoutMorph methodsFor: 'accessing' stamp: 'RMV 8/14/2024 18:32:08'!
useEdgeSpace
	^ useEdgeSpace! !

!LayoutMorph methodsFor: 'accessing' stamp: 'RMV 8/14/2024 18:31:59'!
useEdgeSpace: aBoolean
	useEdgeSpace := aBoolean! !

!LayoutMorph methodsFor: 'initialization' stamp: 'KenD 6/4/2020 14:22:33'!
initialize
	super initialize.
	separation := 0.
	axisEdgeWeight := 0.0.
	useEdgeSpace := true.
	doAdoptWidgetsColor := false! !


!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 8/15/2024 05:26:44'!
exampleEdgesColumn
	"Show 2 cases :1 useEdgeSpace true and 1 false"
	" self exampleEdgesColumn "
	| withEdge noEdge upSpec downSpec |
	withEdge := self newColumn :: useEdgeSpace: true;  separation: 20.
	noEdge := self newColumn :: useEdgeSpace: false;  separation: 20.
	upSpec := LayoutSpec keepMorphExtent :: offAxisEdgeWeight: 0.
	upSpec proportionalWidth: 0.8 minimum: 100; proportionalHeight: 0.8 minimum: 100.
	downSpec := LayoutSpec keepMorphExtent :: offAxisEdgeWeight: 1.
	downSpec proportionalWidth: 0.5 minimum: 100; proportionalHeight: 0.5 minimum: 100.
	
	withEdge addMorph: (BoxedMorph new :: color: Color blue;  layoutSpec: upSpec).
	noEdge addMorph: (BoxedMorph new :: color: Color blue;  layoutSpec: upSpec).
	withEdge addMorph: (LabelMorph new :: contents: 'WITH Edge').
	noEdge addMorph: (LabelMorph new :: contents: 'withOUT Edge').
	withEdge addMorph: (BoxedMorph new :: layoutSpec: downSpec).
	noEdge addMorph: (BoxedMorph new ::  layoutSpec: downSpec).
	
	withEdge openInWorld.
	noEdge openInWorld.! !

!LayoutMorph class methodsFor: 'examples' stamp: 'KenD 8/15/2024 05:26:38'!
exampleEdgesRow
	"Show 2 cases: 1 useEdgeSpace true and 1 false"
	" self exampleEdgesRow "
	| withEdge noEdge upSpec downSpec |
	withEdge := self newRow :: useEdgeSpace: true;  separation: 20.
	noEdge := self newRow :: useEdgeSpace: false;  separation: 20.
	upSpec := LayoutSpec keepMorphExtent :: offAxisEdgeWeight: 0.
	upSpec proportionalWidth: 0.8 minimum: 100; proportionalHeight: 0.8 minimum: 100.
	downSpec := LayoutSpec keepMorphExtent :: offAxisEdgeWeight: 1.
	downSpec proportionalWidth: 0.5 minimum: 100; proportionalHeight: 0.5 minimum: 100.
	
	withEdge addMorph: (BoxedMorph new :: color: Color blue;  layoutSpec: upSpec).
	noEdge addMorph: (BoxedMorph new :: color: Color blue;  layoutSpec: upSpec).
	withEdge addMorph: (LabelMorph new :: contents: 'WITH Edge').
	noEdge addMorph: (LabelMorph new :: contents: 'withOUT Edge').
	withEdge addMorph: (BoxedMorph new :: layoutSpec: downSpec).
	noEdge addMorph: (BoxedMorph new ::  layoutSpec: downSpec).
	
	withEdge openInWorld.
	noEdge openInWorld.! !

!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BoxedMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction separation axisEdgeWeight doAdoptWidgetsColor useEdgeSpace'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
LayoutMorph allSubInstancesDo: [ :each | each instVarNamed: 'useEdgeSpace' put: true ].!

