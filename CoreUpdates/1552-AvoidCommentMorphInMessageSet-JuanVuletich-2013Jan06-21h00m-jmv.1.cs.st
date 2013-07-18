'From Cuis 4.1 of 12 December 2012 [latest update: #1551] on 6 January 2013 at 9:03:28 pm'!

!MessageSetWindow methodsFor: 'GUI building' stamp: 'jmv 1/6/2013 21:02'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterAndMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterAndMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

