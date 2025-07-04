'From Cuis7.5 [latest update: #7301] on 30 June 2025 at 10:32:15 am'!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BorderedBoxMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction gap axisEdgeWeight doAdoptWidgetsColor layout '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
!classDefinition: #LinearLayout category: #'Morphic-Layouts'!
Layout subclass: #LinearLayout
	instanceVariableNames: 'gap axisEdgeWeight '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LinearLayout methodsFor: 'accessing' stamp: 'jmv 6/27/2025 15:05:35'!
gap: aNumber axisEdgeWeight: aNumberOrNil
	gap := aNumber.
	axisEdgeWeight := aNumberOrNil.! !


!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2025 10:29:04'!
beColumn
	"Establish the major layout axis, with default edge weight" 
	direction := #vertical.
	axisEdgeWeight ifNil: [self axisEdgeWeight: #center].
	layout := ColumnLayout new.
	layout gap: gap axisEdgeWeight: axisEdgeWeight.
	self layoutSubmorphs.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 6/30/2025 10:29:09'!
beRow
	"Establish the major layout axis, with default edge weight" 
	direction := #horizontal.
	axisEdgeWeight ifNil: [self axisEdgeWeight: #rowLeft].
	layout := RowLayout new.
	layout gap: gap axisEdgeWeight: axisEdgeWeight.
	self layoutSubmorphs.! !

!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BorderedBoxMorph subclass: #LayoutMorph
	instanceVariableNames: 'direction gap axisEdgeWeight doAdoptWidgetsColor layout'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
!classDefinition: #LinearLayout category: #'Morphic-Layouts'!
Layout subclass: #LinearLayout
	instanceVariableNames: 'gap axisEdgeWeight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	LayoutMorph allSubInstancesDo: [ :lm | | l |
		l := lm isRow ifTrue: [ RowLayout new ] ifFalse: [ ColumnLayout new ].
		l gap: lm gap axisEdgeWeight: lm axisEdgeWeight.
		lm instVarNamed: 'layout' put: l ].!

