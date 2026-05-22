"
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

"
Class {
	#name : 'Sample10PythagorasTree',
	#superclass : 'PlacedMorph',
	#instVars : [
		'depthLimit',
		'angle',
		'colors'
	],
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'accessing' }
Sample10PythagorasTree >> angle [
	"Answer the value of angle"

	^ angle
]

{ #category : 'accessing' }
Sample10PythagorasTree >> angle: anObject [
	"Set the value of angle"

	angle := anObject
]

{ #category : 'accessing' }
Sample10PythagorasTree >> colors [
	"Answer the value of colors"

	^ colors
]

{ #category : 'accessing' }
Sample10PythagorasTree >> colors: anObject [
	"Set the value of colors"

	colors := anObject
]

{ #category : 'accessing' }
Sample10PythagorasTree >> depthLimit [
	"Answer the value of depthLimit"

	^ depthLimit
]

{ #category : 'accessing' }
Sample10PythagorasTree >> depthLimit: anObject [
	"Set the value of depthLimit"

	depthLimit := anObject
]

{ #category : 'as yet unclassified' }
Sample10PythagorasTree >> drawOn: aCanvas [

	self drawTreeOn: aCanvas p1: 275@500 p2: 375@500 depth: 0
]

{ #category : 'as yet unclassified' }
Sample10PythagorasTree >> drawTreeOn: aCanvas p1: p1 p2: p2 depth: depth [

	| dx dy p3 p4 p5 color |
	
	depth = depthLimit ifTrue: [ ^ self].
	
	color := self color.
	colors ifNotNil: [
		color := colors at: (depth mod: colors size) + 1].
	
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
	
	"Enable to also draw triangles. Disable for a 'true' Pythagoras tree."
	true ifTrue: [
		aCanvas fillColor: color do: [
			aCanvas moveTo:  p3;
				lineTo: p4;
				lineTo: p5;
				lineTo: p3]].
	
	self drawTreeOn: aCanvas p1: p4 p2: p5 depth: depth + 1.
	self drawTreeOn: aCanvas p1: p5 p2: p3 depth: depth + 1.
]

{ #category : 'as yet unclassified' }
Sample10PythagorasTree >> handlesMouseScroll: anEvent [

	^ true
]

{ #category : 'as yet unclassified' }
Sample10PythagorasTree >> initialize [
	super initialize.
	
	depthLimit := 9.
	angle := 0.5.
	self withMultipleColors.
]

{ #category : 'as yet unclassified' }
Sample10PythagorasTree >> mouseScroll: aMouseScrollEvent localPosition: localEventPosition [
	aMouseScrollEvent direction
		caseOf: {
			[ #up ] 		-> 	[ angle := angle + 0.02. self redrawNeeded ].
			[ #down ] 	-> 	[ angle := (angle - 0.02) max: 0. self redrawNeeded ].
			[ #left ] 	-> 	[ depthLimit := depthLimit - 1 max: 1. self redrawNeeded].
			[ #right ] 	-> 	[ depthLimit := depthLimit + 1 min: 15. self redrawNeeded]
		}
]

{ #category : 'as yet unclassified' }
Sample10PythagorasTree >> withMultipleColors [
	colors := (0 to: 360 count: 15) collect: [ :k | Color hue: k saturation: 0.8 brightness: 0.8]
]
