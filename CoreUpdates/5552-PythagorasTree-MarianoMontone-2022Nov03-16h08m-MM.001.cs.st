'From Cuis 6.0 [latest update: #5551] on 3 November 2022 at 4:15:27 pm'!
!classDefinition: #Sample10PythagorasTree category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample10PythagorasTree
	instanceVariableNames: 'depthLimit angle colors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!

!Sample10PythagorasTree commentStamp: 'MM 11/3/2022 16:09:54' prior: 0!
A Pythagoras Tree using Vector Graphics.

Scroll up/down to change angle.
Scroll up/down + Ctrl to change depth (you may need Preferences at: #ctrlArrowsScrollHorizontally put: true for this).

Examples: 

Sample10PythagorasTree new
	withMultipleColors;
	openInWorld.
	
Sample10PythagorasTree new openInWorld.

Sample10PythagorasTree new
	angle: 0.4;
	withMultipleColors;
	openInWorld.
!


!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'MM 11/3/2022 14:04:04'!
drawOn: aCanvas

	self drawTreeOn: aCanvas p1: 275@500 p2: 375@500 depth: 0! !

!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'MM 11/3/2022 16:11:16'!
drawTreeOn: aCanvas p1: p1 p2: p2 depth: depth

	| dx dy p3 p4 p5 color |
	
	depth = depthLimit ifTrue: [ ^ self].
	
	color _ self color.
	colors ifNotNil: [
		color _ colors at: (depth mod: colors size) + 1].
	
	dx := p2 x - (p1 x).
	dy := p1 y - (p2 y).
	
	p3 := (p2 x - dy) @ (p2 y - dx).
	p4 := (p1 x - dy) @ (p1 y - dx).
	p5 := (p4 x + (angle * (dx - dy))) @ (p4 y - (angle * (dx + dy))).
	
	aCanvas fillColor: color do: [
		aCanvas moveTo: p1;
			lineTo: p2;
			lineTo: p3;
			lineTo: p4;
			lineTo: p1].
			
	"aCanvas fillColor: color do: [
		aCanvas moveTo:  p3;
			lineTo: p4;
			lineTo: p5;
			lineTo: p3]."
			
	self drawTreeOn: aCanvas p1: p4 p2: p5 depth: depth + 1.
	self drawTreeOn: aCanvas p1: p5 p2: p3 depth: depth + 1.! !

!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'MM 11/3/2022 14:16:22'!
handlesMouseScroll: anEvent

	^ true! !

!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'MM 11/3/2022 14:19:52'!
initialize
	super initialize.
	
	depthLimit _ 9.
	angle _ 0.5.
	self withMultipleColors.! !

!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'MM 11/3/2022 16:11:49'!
mouseScroll: aMouseEvent localPosition: localEventPosition
	aMouseEvent direction
		caseOf: {
			[ #up ] 		-> 		[  angle := angle + 0.02. self redrawNeeded ].
			[ #down ] 	-> 		[ angle := (angle - 0.02) max: 0. self redrawNeeded ].
			[ #left ] -> [ depthLimit := depthLimit - 1 max: 1. self redrawNeeded].
			[ #right ] -> [ depthLimit := depthLimit + 1 min: 15. self redrawNeeded]
		}! !

!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'MM 11/3/2022 16:10:25'!
withMultipleColors
	colors _ (0 to: 360 count: 15) collect: [ :k | Color hue: k saturation: 0.8 brightness: 0.8]! !

!Sample10PythagorasTree methodsFor: 'accessing' stamp: 'MM 11/3/2022 14:03:08'!
angle
	"Answer the value of angle"

	^ angle! !

!Sample10PythagorasTree methodsFor: 'accessing' stamp: 'MM 11/3/2022 14:03:08'!
angle: anObject
	"Set the value of angle"

	angle := anObject! !

!Sample10PythagorasTree methodsFor: 'accessing' stamp: 'MM 11/3/2022 14:03:08'!
colors
	"Answer the value of colors"

	^ colors! !

!Sample10PythagorasTree methodsFor: 'accessing' stamp: 'MM 11/3/2022 14:03:08'!
colors: anObject
	"Set the value of colors"

	colors := anObject! !

!Sample10PythagorasTree methodsFor: 'accessing' stamp: 'MM 11/3/2022 14:03:08'!
depthLimit
	"Answer the value of depthLimit"

	^ depthLimit! !

!Sample10PythagorasTree methodsFor: 'accessing' stamp: 'MM 11/3/2022 14:03:08'!
depthLimit: anObject
	"Set the value of depthLimit"

	depthLimit := anObject! !
