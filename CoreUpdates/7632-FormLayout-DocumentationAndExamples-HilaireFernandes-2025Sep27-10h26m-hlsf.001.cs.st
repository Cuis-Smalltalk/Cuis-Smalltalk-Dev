'From Cuis7.5 [latest update: #7621] on 27 September 2025 at 11:21:20 am'!

!FormLayoutMorph commentStamp: 'hlsf 9/27/2025 11:02:47' prior: 0!
Read 'FormLayout examples' in LayoutMorph class.!


!FormLayout commentStamp: 'hlsf 9/27/2025 11:02:22' prior: 0!
Read 'FormLayout examples' in LayoutMorph class.!


!LayoutEdgesSpec commentStamp: '<historical>' prior: 0!
To be used with FormLayout.
Read 'FormLayout examples' in LayoutMorph class.

- 'topLeftEdgesWeight' and 'bottomRightEdgesWeight' are points representing the ox and oy positionning, expressed in percentage in [0;1], of the top left and bottom right corners of the morph, in its FormLayoutMorph owner.
- topLeftOffset and bottomLeftOffset are points representing the ox and oy shifts from the above edges weights, expressed in pixels, postive or negative.

Therefore, the positionning of the morph in its FormLayout owner is determined by its top left and right bottom corners, computed by its 4 edge weights (percentage) in its owner and corrected by its 4 pixel offsets.
!


!LayoutMorph class methodsFor: 'FormLayout examples' stamp: 'hlsf 9/27/2025 11:08:30'!
example2Form
"
	self example2Form
"
	| pane |
	pane := FormLayoutMorph newForm padding: 0 :: name: #example2.
	pane 
		color: Color red;
		addMorph: (ColoredBoxMorph new ::  color: Color white; name: #canvas;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 0@0 offset: 0@0 
				brEdgesWeight: 1@1 offset: -50@-50) );
		addMorph: (ColoredBoxMorph new ::  color: Color blue; name: #zoomWheel;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 1@0 offset: -50@0 
				brEdgesWeight: 1@1 offset: 0@-50) );
		addMorph: (ColoredBoxMorph new :: color: Color lightGreen; name: #horizontalWheel;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 0@1 offset: 0@-50 
				brEdgesWeight: 1@1 offset: -50@0) ).
	pane position: 150@130 extent: 400@300.
	pane openInWorld! !

!LayoutMorph class methodsFor: 'FormLayout examples' stamp: 'hlsf 9/27/2025 11:09:55'!
example3Form
"
	self example3Form
"
	| pane |
	pane := FormLayoutMorph newForm padding: 0 :: name: #example3.
	pane 
		color: Color red;
		addMorph: (ColoredBoxMorph new :: color: Color white; name: #canvas;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 0@0 offset: 0@0 
				brEdgesWeight: 1@1 offset: -50@-50) );
		addMorph: (ColoredBoxMorph new :: color: Color blue; name: #zoomWheelCentered150px;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 1@0.5 offset: -50@-75 
				brEdgesWeight: 1@0.5 offset: 0@75) );
		addMorph: (ColoredBoxMorph new :: color: Color lightGreen; name: #horizontalWheelCentered150px;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 0.5@1 offset: -75@-50 
				brEdgesWeight: 0.5@1 offset: 75@0) ).
	pane position: 150@130 extent: 400@300.
	pane openInWorld! !

