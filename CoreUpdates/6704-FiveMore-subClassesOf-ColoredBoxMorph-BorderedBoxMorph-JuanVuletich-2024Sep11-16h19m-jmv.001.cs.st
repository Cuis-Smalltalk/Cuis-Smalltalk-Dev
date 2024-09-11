'From Cuis7.1 [latest update: #6702] on 11 September 2024 at 4:26:11 pm'!
!classDefinition: #HoverHelpMorph category: #'Morphic-Composite Widgets'!
ColoredBoxMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'contents textComposition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
!classDefinition: #TranscriptMorph category: #'Morphic-Composite Widgets'!
ColoredBoxMorph subclass: #TranscriptMorph
	instanceVariableNames: 'workspace lastIncludedIndex workspaceWindow lastUnfinishedEntrySize doImmediateUpdates '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
!classDefinition: #HaloHandleMorph category: #'Morphic-Halos'!
ColoredBoxMorph subclass: #HaloHandleMorph
	instanceVariableNames: 'mouseDownSelector mouseUpSelector mouseMoveSelector '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Halos'!
!classDefinition: #ProgressBarMorph category: #'Morphic-Composite Widgets'!
BorderedBoxMorph subclass: #ProgressBarMorph
	instanceVariableNames: 'value progressColor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
!classDefinition: #ResizeMorph category: #'Morphic-Widgets'!
ColoredBoxMorph subclass: #ResizeMorph
	instanceVariableNames: 'gridLineWidth gridColor selectionColor outlineMorph grid from to action '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!HoverHelpMorph methodsFor: 'drawing' stamp: 'jmv 9/11/2024 15:50:30'!
drawOn: aCanvas

	| r |
	r := self localBounds.
	aCanvas roundRect: r color: color radius: 4.
	aCanvas
		drawTextComposition: textComposition
		at: `4@4`
		extent: extent - 8
		color: `Color black`
		selectionColor: (Theme current textHighlightFocused: false).! !

!methodRemoval: ResizeMorph #defaultBorderWidth stamp: 'jmv 9/11/2024 16:22:55'!
ResizeMorph removeSelector: #defaultBorderWidth!
!methodRemoval: TranscriptMorph #defaultBorderWidth stamp: 'jmv 9/11/2024 16:22:03'!
TranscriptMorph removeSelector: #defaultBorderWidth!
!methodRemoval: HoverHelpMorph #defaultBorderWidth stamp: 'jmv 9/11/2024 16:23:42'!
HoverHelpMorph removeSelector: #defaultBorderWidth!
!classDefinition: #HoverHelpMorph category: #'Morphic-Composite Widgets'!
ColoredBoxMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'contents textComposition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
!classDefinition: #TranscriptMorph category: #'Morphic-Composite Widgets'!
ColoredBoxMorph subclass: #TranscriptMorph
	instanceVariableNames: 'workspace lastIncludedIndex workspaceWindow lastUnfinishedEntrySize doImmediateUpdates'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
!classDefinition: #HaloHandleMorph category: #'Morphic-Halos'!
ColoredBoxMorph subclass: #HaloHandleMorph
	instanceVariableNames: 'mouseDownSelector mouseUpSelector mouseMoveSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Halos'!
!classDefinition: #ProgressBarMorph category: #'Morphic-Composite Widgets'!
BorderedBoxMorph subclass: #ProgressBarMorph
	instanceVariableNames: 'value progressColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
!classDefinition: #ResizeMorph category: #'Morphic-Widgets'!
ColoredBoxMorph subclass: #ResizeMorph
	instanceVariableNames: 'gridLineWidth gridColor selectionColor outlineMorph grid from to action'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
