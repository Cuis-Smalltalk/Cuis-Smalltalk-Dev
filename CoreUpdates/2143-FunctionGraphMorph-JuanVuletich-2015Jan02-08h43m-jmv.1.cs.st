'From Cuis 4.2 of 25 July 2013 [latest update: #2142] on 2 January 2015 at 8:46:34.619324 am'!
!classDefinition: #FunctionGraphMorph category: #'Morphic-Widgets'!
Morph subclass: #FunctionGraphMorph
	instanceVariableNames: 'xMin xMax yMin yMax functions colors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!FunctionGraphMorph commentStamp: 'jmv 12/23/2014 21:00' prior: 0!
| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x \\ d ] color: Color green.
g addFunction: [ :x | x // d ] color: Color red.
g openInWorld

| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x rem: d ] color: Color green.
g addFunction: [ :x | x quo: d ] color: Color red.
g openInWorld

| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x mod: d ] color: Color green.
g addFunction: [ :x | x div: d ] color: Color red.
g openInWorld
!


!FunctionGraphMorph methodsFor: 'accessing' stamp: 'jmv 11/18/2014 08:52'!
addFunction: aOneArgBlock color: aColor

	functions add: aOneArgBlock.
	colors add: aColor! !

!FunctionGraphMorph methodsFor: 'accessing' stamp: 'jmv 12/23/2014 20:34'!
domain: anInterval

	xMin _ anInterval first.
	xMax _ anInterval last.

	yMin _ anInterval first.
	yMax _ anInterval last.! !

!FunctionGraphMorph methodsFor: 'accessing' stamp: 'jmv 12/31/2014 18:25'!
morphExtent
	| e |
	e _ xMax - xMin.
	^e@e * 1.1! !

!FunctionGraphMorph methodsFor: 'accessing' stamp: 'jmv 12/31/2014 18:25'!
morphTopLeft
	| e |
	e _ xMax - xMin.
	^xMin@yMin - (e * 0.05)! !

!FunctionGraphMorph methodsFor: 'initialization' stamp: 'jmv 12/23/2014 20:35'!
initialize
	super initialize.
 	location negateYAxis.
	location addOffset: 300.
	location scaleBy: 40.
	functions _ OrderedCollection new.
	colors _ OrderedCollection new.! !

!FunctionGraphMorph methodsFor: 'geometry testing' stamp: 'jmv 12/20/2014 16:24'!
isOrthoRectangularMorph
	"Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and
	specified by my #morphExtent.
	If true, #morphContainsPoint: can simply check #morphExtent."
	^true! !

!FunctionGraphMorph methodsFor: 'drawing' stamp: 'jmv 12/31/2014 18:30'!
drawOn: aCanvas
"ojo anchos de linea
\\
rem:
mod:
"
"
| g |
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x mod: 1 ] color: Color green.
g addFunction: [ :x | x div: 1 ] color: Color red.
g openInWorld
"
	| r |
	r _ self morphLocalBounds.
	aCanvas
		fillRectangle: r
		color: (Color gray: 0.9).
	aCanvas
		frameAndFillRectangle: r fillColor: (Color gray: 0.9)
		borderWidth: 0.05 borderColor: Color white.
	aCanvas line: 0@yMin to: 0 @ yMax width: 0.05 color: Color lightGray.
	aCanvas line:xMin@0 to: xMax@0 width: 0.05 color: Color lightGray.

	xMin*1.0 to: xMax count: 600 do: [ :x |
		functions with: colors do: [ :f :c |
			aCanvas
				fillRectangle: (x@((f value: x)) extent: 0.1)
				color: c ]]! !


!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 12/31/2014 18:30'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect bw |

	r area = 0 ifTrue: [ ^self ].
	rect _ currentTransformation displayBoundsOfTransformOf: r.
	bw _ currentTransformation externalizeScalar: borderWidth.

	"draw the border of the rectangle"
	borderColor isTransparent ifFalse:[
		self setPaintColor: borderColor.
		port frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: (rect insetBy: bw) ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 12/31/2014 18:29'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red
	"
	| rect bw |
	rect _ currentTransformation displayBoundsOfTransformOf: r.
	bw _ currentTransformation externalizeScalar: borderWidth.
	self setPaintColor: borderColor.
	port
		frameRect: rect
		borderWidth: bw! !

