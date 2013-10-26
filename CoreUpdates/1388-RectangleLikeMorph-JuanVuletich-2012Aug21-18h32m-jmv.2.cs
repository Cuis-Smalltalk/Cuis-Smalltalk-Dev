'From Cuis 4.0 of 21 April 2012 [latest update: #1387] on 21 August 2012 at 6:36:04 pm'!
"Change Set:		1388-CuisCore-JuanVuletich-2012Aug21-18h32m
Date:			21 August 2012
Author:			Juan Vuletich

<your descriptive text goes here>"
TranscriptWindow allInstancesDo: [ :a | a delete ].
ProgressMorph allInstancesDo: [ :a | a delete ]!

!classDefinition: #EllipseMorph category: #'Morphic-Basic'!
BorderedRectMorph subclass: #EllipseMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!
!classDefinition: #FillInTheBlankMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #FillInTheBlankMorph
	instanceVariableNames: 'response done textPane responseUponCancel '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #FrameRateMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #FrameRateMorph
	instanceVariableNames: 'lastStepStamp lastStepDelta meanStepDelta '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #HoverHelpMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'contents paragraph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #ImageMorph category: #'Morphic-Basic'!
RectangleLikeMorph subclass: #ImageMorph
	instanceVariableNames: 'image '
	classVariableNames: 'DefaultForm '
	poolDictionaries: ''
	category: 'Morphic-Basic'!
!classDefinition: #InnerPluggableMorph category: #'Morphic-Views for Models'!
RectangleLikeMorph subclass: #InnerPluggableMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!classDefinition: #LayoutAdjustingMorph category: #'Morphic-Layouts'!
RectangleLikeMorph subclass: #LayoutAdjustingMorph
	instanceVariableNames: 'hand indicator '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
!classDefinition: #MenuLineMorph category: #'Morphic-Menus'!
RectangleLikeMorph subclass: #MenuLineMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedRectMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp popUpOwner activeSubMenu titleMorph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!classDefinition: #MinimalStringMorph category: #'Morphic-Basic'!
RectangleLikeMorph subclass: #MinimalStringMorph
	instanceVariableNames: 'font emphasis contents '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!
!classDefinition: #OneLineEditorMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #OneLineEditorMorph
	instanceVariableNames: 'font emphasis contents editor showCaret pauseBlinking caretRect keyboardFocusWatcher '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #PasteUpMorph category: #'Morphic-Worlds'!
BorderedRectMorph subclass: #PasteUpMorph
	instanceVariableNames: 'worldState backgroundImage backgroundImageData '
	classVariableNames: 'DisableDeferredUpdates WindowEventHandler '
	poolDictionaries: ''
	category: 'Morphic-Worlds'!
!classDefinition: #ProgressBarMorph category: #'Morphic-Widgets'!
BorderedRectMorph subclass: #ProgressBarMorph
	instanceVariableNames: 'value progressColor lastValue '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #RectangleIndicatorMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #RectangleIndicatorMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #StringMorph category: #'Morphic-Basic'!
RectangleLikeMorph subclass: #StringMorph
	instanceVariableNames: 'font emphasis contents hasFocus '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!
!classDefinition: #TranscriptMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #TranscriptMorph
	instanceVariableNames: 'form '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!ProgressMorph methodsFor: 'initialization' stamp: 'jmv 8/21/2012 16:46'!
initialize
	super initialize.
	self separation: 0.
	labelMorph _ StringMorph contents: '' font: AbstractFont default.
	subLabelMorph _ StringMorph contents: '' font: AbstractFont default.
	progress _ ProgressBarMorph new.
	progress morphExtent: 200 @ 15.
	self addMorph: labelMorph.
	self addMorph: subLabelMorph.
	self addMorph: progress fixedHeight: 15.! !

!classDefinition: #TranscriptMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #TranscriptMorph
	instanceVariableNames: 'form'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #StringMorph category: #'Morphic-Basic'!
RectangleLikeMorph subclass: #StringMorph
	instanceVariableNames: 'font emphasis contents hasFocus'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!
!classDefinition: #ProgressBarMorph category: #'Morphic-Widgets'!
BorderedRectMorph subclass: #ProgressBarMorph
	instanceVariableNames: 'value progressColor lastValue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #PasteUpMorph category: #'Morphic-Worlds'!
BorderedRectMorph subclass: #PasteUpMorph
	instanceVariableNames: 'worldState backgroundImage backgroundImageData'
	classVariableNames: 'DisableDeferredUpdates WindowEventHandler'
	poolDictionaries: ''
	category: 'Morphic-Worlds'!
!classDefinition: #OneLineEditorMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #OneLineEditorMorph
	instanceVariableNames: 'font emphasis contents editor showCaret pauseBlinking caretRect keyboardFocusWatcher'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #MinimalStringMorph category: #'Morphic-Basic'!
RectangleLikeMorph subclass: #MinimalStringMorph
	instanceVariableNames: 'font emphasis contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!
!classDefinition: #MenuMorph category: #'Morphic-Menus'!
BorderedRectMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget selectedItem stayUp popUpOwner activeSubMenu titleMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
!classDefinition: #LayoutAdjustingMorph category: #'Morphic-Layouts'!
RectangleLikeMorph subclass: #LayoutAdjustingMorph
	instanceVariableNames: 'hand indicator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
!classDefinition: #ImageMorph category: #'Morphic-Basic'!
RectangleLikeMorph subclass: #ImageMorph
	instanceVariableNames: 'image'
	classVariableNames: 'DefaultForm'
	poolDictionaries: ''
	category: 'Morphic-Basic'!
!classDefinition: #HoverHelpMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'contents paragraph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #FrameRateMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #FrameRateMorph
	instanceVariableNames: 'lastStepStamp lastStepDelta meanStepDelta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #FillInTheBlankMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #FillInTheBlankMorph
	instanceVariableNames: 'response done textPane responseUponCancel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
