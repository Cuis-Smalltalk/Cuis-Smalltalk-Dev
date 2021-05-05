'From Cuis 5.0 [latest update: #4585] on 5 May 2021 at 3:29:14 pm'!
!classDefinition: #BrowserCommentTextMorph category: 'Morphic-Widgets'!
TextModelMorph subclass: #BrowserCommentTextMorph
	instanceVariableNames: 'separator proportionalHeight code '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!BrowserCommentTextMorph methodsFor: 'accessing' stamp: 'jmv 5/5/2021 14:56:15'!
separator: adjusterMorph code: codeMorph
	separator _ adjusterMorph.
	code _ codeMorph.! !


!BrowserCommentTextMorph methodsFor: 'displaying' stamp: 'jmv 5/5/2021 15:06:08'!
hidePane

	self hide.
	separator visible ifTrue: [
		proportionalHeight _ self layoutSpec proportionalLayoutHeight ].
	separator layoutSpec fixedHeight: 0.
	self layoutSpec proportionalHeight: 0.
	code layoutSpec proportionalHeight: 1.0.
	separator ifNotNil: [ separator hide ]! !

!BrowserCommentTextMorph methodsFor: 'displaying' stamp: 'jmv 5/5/2021 15:03:35'!
showPane

	self show.
	proportionalHeight ifNotNil: [
		self layoutSpec proportionalHeight: proportionalHeight.
		code layoutSpec proportionalHeight: 1.0 - proportionalHeight ].
	separator ifNotNil: [
		separator layoutSpec fixedHeight: Theme current layoutAdjusterThickness.
		separator show ]! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 5/5/2021 14:59:00'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations code comment separator |
	code _ self buildMorphicCodePane.
	comment _ self buildMorphicCommentPane.
	separator _ LayoutAdjustingMorph new.
	comment separator: separator code: code.
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: code proportionalHeight: 0.5;
		addMorph: separator fixedHeight: Theme current layoutAdjusterThickness;
		addMorph: comment proportionalHeight: 0.5.
	Preferences showAnnotations ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!methodRemoval: BrowserCommentTextMorph #separator: stamp: 'jmv 5/5/2021 15:14:42'!
BrowserCommentTextMorph removeSelector: #separator:!
!classDefinition: #BrowserCommentTextMorph category: 'Morphic-Widgets'!
TextModelMorph subclass: #BrowserCommentTextMorph
	instanceVariableNames: 'separator code proportionalHeight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
