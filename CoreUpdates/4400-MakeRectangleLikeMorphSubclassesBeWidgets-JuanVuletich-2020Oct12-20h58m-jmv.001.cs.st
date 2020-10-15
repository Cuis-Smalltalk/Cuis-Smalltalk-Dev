'From Cuis 5.0 [latest update: #4396] on 12 October 2020 at 9:01:52 pm'!
!classDefinition: #ResizeMorph category: #'Morphic-Widgets'!
WidgetMorph subclass: #ResizeMorph
	instanceVariableNames: 'gridLineWidth gridColor selectionColor outlineMorph grid from to action'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #TranscriptMorph category: #'Morphic-Composite Widgets'!
WidgetMorph subclass: #TranscriptMorph
	instanceVariableNames: 'workspace lastIncludedIndex workspaceWindow lastUnfinishedEntrySize doImmediateUpdates'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
!classDefinition: #MenuLineMorph category: #'Morphic-Menus'!
WidgetMorph subclass: #MenuLineMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!ResizeMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 21:00:28'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0! !


!TranscriptMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 21:00:55'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0! !


!MenuLineMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 20:59:41'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 2! !

