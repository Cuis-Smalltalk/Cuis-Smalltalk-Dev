'From Cuis 4.2 of 25 July 2013 [latest update: #1895] on 21 December 2013 at 4:03:54.432697 pm'!
!ProgressBarMorph methodsFor: 'drawing' stamp: 'sqr 12/20/2013 20:36'!
drawOn: aCanvas

	| twoBorders |
	super drawOn: aCanvas.
	twoBorders _ borderWidth + borderWidth.
	aCanvas
		fillRectangle: (borderWidth @ borderWidth extent: extent x * value @ extent y - twoBorders)
		color: progressColor! !

!classDefinition: #ProgressBarMorph category: #'Morphic-Widgets'!
BorderedRectMorph subclass: #ProgressBarMorph
	instanceVariableNames: 'value progressColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
