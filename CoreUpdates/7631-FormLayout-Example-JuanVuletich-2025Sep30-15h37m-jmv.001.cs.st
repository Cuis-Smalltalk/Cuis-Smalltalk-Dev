'From Cuis7.5 [latest update: #7631] on 30 September 2025 at 3:47:19 pm'!

!LayoutMorph class methodsFor: 'FormLayout examples' stamp: 'jmv 9/30/2025 15:45:44'!
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
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@60 brEdgesWeight: 0.5@0 offset: -10@120);
		addMorph: (ColoredBoxMorph new color: (Color green); name: #D)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@0 offset: 10@60 brEdgesWeight: 1@0 offset: -30@120);
		addMorph: (ColoredBoxMorph new color: (Color h: 60 s: 0.6 v: 0.6); name: #E)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@0 offset: 30@140 brEdgesWeight: 1@1 offset: -30@ -70).
	pane
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #F)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@1 offset: 30@ -60 brEdgesWeight: 0@1 offset: 100@ -40);
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #G)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@1 offset: 110@ -60 brEdgesWeight: 0.5@1 offset: -10@ -40);
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #H)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@1 offset: 10@ -60 brEdgesWeight: 1@1 offset: -30@ -40).
	pane
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #I)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@1 offset: 30@ -30 brEdgesWeight: 0@1 offset: 100@ -10);
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #J)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0@1 offset: 110@ -30 brEdgesWeight: 0.5@1 offset: -10@ -10);
		addMorph: (ColoredBoxMorph new color: (Color lightBlue); name: #K)
			layoutSpec: (LayoutEdgesSpec tlEdgesWeight: 0.5@1 offset: 10@ -30 brEdgesWeight: 1@1 offset: -30@ -10).
	pane position: 150@130 extent: 400@300.
	pane openInWorld! !

