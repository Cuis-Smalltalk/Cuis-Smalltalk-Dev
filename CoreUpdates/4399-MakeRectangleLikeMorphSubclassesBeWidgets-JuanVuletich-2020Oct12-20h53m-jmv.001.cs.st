'From Cuis 5.0 [latest update: #4395] on 12 October 2020 at 8:55:55 pm'!
!classDefinition: #DraggingGuideMorph category: #'Morphic-Widgets'!
WidgetMorph subclass: #DraggingGuideMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #LayoutAdjustingMorph category: #'Morphic-Layouts'!
WidgetMorph subclass: #LayoutAdjustingMorph
	instanceVariableNames: 'hand'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Layouts'!
!classDefinition: #FillInTheBlankMorph category: #'Morphic-Composite Widgets'!
WidgetMorph subclass: #FillInTheBlankMorph
	instanceVariableNames: 'response done textPane responseUponCancel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
!classDefinition: #HoverHelpMorph category: #'Morphic-Composite Widgets'!
WidgetMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'contents textComposition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!DraggingGuideMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 20:43:42'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0! !


!LayoutAdjustingMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 20:47:51'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 10/12/2020 20:46:53'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0! !

!LayoutAdjustingMorph reorganize!
('accessing' adoptWidgetsColor: cursor)
('initialization' defaultBorderWidth)
('adjusting' adjustOwnerAt:)
('drawing' drawOn:)
('events' mouseButton1Down:localPosition: mouseEnter: mouseLeave:)
('event handling testing' handlesMouseDown: handlesMouseOver:)
('stepping' stepAt: stepTime)
('testing')
!
!HoverHelpMorph reorganize!
('accessing' contents:)
('initialization' defaultBorderWidth defaultColor popUpForHand:)
('drawing' drawOn:)
!

