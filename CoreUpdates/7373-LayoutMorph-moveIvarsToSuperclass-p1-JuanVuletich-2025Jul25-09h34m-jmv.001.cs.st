'From Cuis7.5 [latest update: #7372] on 25 July 2025 at 9:41:55 am'!
!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BorderedBoxMorph subclass: #LayoutMorph
	instanceVariableNames: 'adoptsWidgetsColor layoutObject '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:36:17'!
doAdoptWidgetsColor
	adoptsWidgetsColor := true.! !

!LayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:39:00'!
initialize
	super initialize.
	adoptsWidgetsColor := false.! !


!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:40:21'!
beColumn
	"Establish the major layout axis, with default edge weight"
	layout := ColumnLayout new.
	layout gap: self defaultGap.
	layoutObject := layout.
	self layoutSubmorphs.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:40:17'!
beRow
	"Establish the major layout axis, with default edge weight"
	layout := RowLayout new.
	layout gap: self defaultGap.
	layoutObject := layout.
	self layoutSubmorphs.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:36:25'!
doAdoptWidgetsColor
	super doAdoptWidgetsColor.
	doAdoptWidgetsColor := true! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:39:57'!
initializeColumn

	layout := ColumnLayout new.
	layout gap: self defaultGap.
	layoutObject := layout.
	doAdoptWidgetsColor := false.
	self initialize.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:40:10'!
initializeForm

	layout := FormLayout new.
	layoutObject := layout.
	doAdoptWidgetsColor := false.
	self initialize.! !

!LinearLayoutMorph methodsFor: 'initialization' stamp: 'jmv 7/25/2025 09:40:06'!
initializeRow

	layout := RowLayout new.
	layout gap: self defaultGap.
	layoutObject := layout.
	doAdoptWidgetsColor := false.
	self initialize.! !

!classDefinition: #LayoutMorph category: #'Morphic-Layouts'!
BorderedBoxMorph subclass: #LayoutMorph
	instanceVariableNames: 'adoptsWidgetsColor layoutObject'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!

!LayoutMorph reorganize!
('initialization' doAdoptWidgetsColor initialize)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
LayoutMorph allSubInstancesDo: [ :m |
	m instVarNamed: 'adoptsWidgetsColor' put: (m instVarNamed: 'doAdoptWidgetsColor').
	m instVarNamed: 'layoutObject' put: (m instVarNamed: 'layout') ].!

