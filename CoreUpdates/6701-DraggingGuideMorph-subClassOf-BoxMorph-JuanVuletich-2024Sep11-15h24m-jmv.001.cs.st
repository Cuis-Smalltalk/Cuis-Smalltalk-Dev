'From Cuis7.1 [latest update: #6698] on 11 September 2024 at 3:27:29 pm'!
!classDefinition: #DraggingGuideMorph category: #'Morphic-Widgets'!
BoxMorph subclass: #DraggingGuideMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!DraggingGuideMorph methodsFor: 'drawing' stamp: 'jmv 9/11/2024 15:26:19'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."! !

!methodRemoval: DraggingGuideMorph #defaultColor stamp: 'jmv 9/11/2024 15:25:12'!
DraggingGuideMorph removeSelector: #defaultColor!
!methodRemoval: DraggingGuideMorph #initialize stamp: 'jmv 9/11/2024 15:25:36'!
DraggingGuideMorph removeSelector: #initialize!
!methodRemoval: DraggingGuideMorph #defaultBorderWidth stamp: 'jmv 9/11/2024 15:25:15'!
DraggingGuideMorph removeSelector: #defaultBorderWidth!

!DraggingGuideMorph reorganize!
('testing' is:)
('dropping/grabbing' justDroppedInto:event:)
('drawing' drawOn:)
!

