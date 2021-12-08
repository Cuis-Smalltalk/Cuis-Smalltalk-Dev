'From Cuis 5.0 [latest update: #4894] on 29 September 2021 at 5:05:07 pm'!

!LayoutMorph methodsFor: 'layout in owner' stamp: 'jmv 3/3/2016 09:43'!
layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec _ LayoutSpec useAll.
	layoutSpec morph: self.

	^ layoutSpec ! !

