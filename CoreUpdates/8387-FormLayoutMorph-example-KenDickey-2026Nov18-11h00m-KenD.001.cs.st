'From Cuis7.9 [latest update: #8386] on 18 November 2026 at 11:01:41 am'!

!LayoutMorph class methodsFor: 'FormLayout examples' stamp: 'KenD 1/Nov/2026, 9:02:16 pm (UTC)'!
example0Form
"
	self example0Form.
	
	Some layouts don't work well as rows & columns.
"

| pane |
	pane := FormLayoutMorph newForm padding: 10 :: name: #example0.
	pane 
		color: Color yellow;
		addMorph: (ColoredBoxMorph new :: color: Color brown; name: #A;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 0@0 offset: 0@0 
				brEdgesWeight: (3/11)@(7/11) offset: 0@0 ) );
		addMorph: (ColoredBoxMorph new :: color: Color brown; name: #B;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: (4/11)@0 offset: 0@0 
				brEdgesWeight: 1@(3/11) offset: 0@0) );
		addMorph: (ColoredBoxMorph new :: color: Color brown; name: #C;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: (4/11)@(4/11) offset: 0@0 
				brEdgesWeight: (7/11)@(7/11) offset: 0@0) );
		addMorph: (ColoredBoxMorph new :: color: Color brown; name: #D;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: (8/11)@(4/11) offset: 0@0 
				brEdgesWeight: 1@1 offset: 0@0) );
		addMorph: (ColoredBoxMorph new :: color: Color brown; name: #E;
			layoutSpec: (LayoutEdgesSpec 
				tlEdgesWeight: 0@(8/11) offset: 0@0 
				brEdgesWeight: (7/11)@1 offset: 0@0) ).
	pane position: 150@130 extent: 130@130.
	pane openInWorld.
	! !

