'From Cuis7.1 [latest update: #6697] on 11 September 2024 at 3:15:24 pm'!

!LayoutMorph commentStamp: 'jmv 9/11/2024 15:13:03' prior: 0!
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

