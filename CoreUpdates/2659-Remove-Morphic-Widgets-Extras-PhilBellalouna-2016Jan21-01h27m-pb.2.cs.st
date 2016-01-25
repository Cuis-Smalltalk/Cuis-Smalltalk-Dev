'From Cuis 4.2 of 25 July 2013 [latest update: #2658] on 24 January 2016 at 7:41:28.711078 pm'!

!Taskbar methodsFor: 'initialization' stamp: 'pb 1/21/2016 01:28'!
initialize
	super initialize.
	viewBox _ LayoutMorph newRow color: self defaultColor.
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			minorDirectionPadding: #right).
	viewBox separation: 5
! !

!classRemoval: #DragAndDropAreaMorph!
Smalltalk removeClassNamed: #DragAndDropAreaMorph!
!classRemoval: #FrameRateMorph!
Smalltalk removeClassNamed: #FrameRateMorph!
!classRemoval: #FunctionGraphMorph!
Smalltalk removeClassNamed: #FunctionGraphMorph!
!classRemoval: #LimitedHeightTextMorph!
Smalltalk removeClassNamed: #LimitedHeightTextMorph!
!classRemoval: #MagnifierMorph!
Smalltalk removeClassNamed: #MagnifierMorph!
!classRemoval: #MinimalStringMorph!
Smalltalk removeClassNamed: #MinimalStringMorph!
!classRemoval: #OneLineEditorMorph!
Smalltalk removeClassNamed: #OneLineEditorMorph!
!classRemoval: #PartsBinMorph!
Smalltalk removeClassNamed: #PartsBinMorph!
!classRemoval: #UpdatingStringMorph!
Smalltalk removeClassNamed: #UpdatingStringMorph!
