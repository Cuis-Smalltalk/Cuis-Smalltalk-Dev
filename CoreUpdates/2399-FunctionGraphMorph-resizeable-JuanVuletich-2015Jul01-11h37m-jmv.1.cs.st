'From Cuis 4.2 of 25 July 2013 [latest update: #2398] on 1 July 2015 at 11:39:51.257817 am'!
!classDefinition: #FunctionGraphMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #FunctionGraphMorph
	instanceVariableNames: 'xMin xMax yMin yMax functions colors '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!FunctionGraphMorph commentStamp: 'jmv 7/1/2015 11:28' prior: 0!
| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x \\ d ] color: Color green.
g addFunction: [ :x | x // d ] color: Color red.
(g embeddedInMorphicWindowLabeled: 'graph') openInWorld

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


!FunctionGraphMorph methodsFor: 'geometry' stamp: 'jmv 5/17/2015 09:20'!
toMorphic: aPoint
	^(self xToMorphic: aPoint x) @ (self yToMorphic: aPoint y)! !

!FunctionGraphMorph methodsFor: 'geometry' stamp: 'jmv 5/17/2015 09:17'!
xToMorphic: x
	^x - xMin / (xMax - xMin ) * extent x! !

!FunctionGraphMorph methodsFor: 'geometry' stamp: 'jmv 5/17/2015 09:16'!
yToMorphic: y
	^yMax - y / (yMax - yMin ) * extent y! !


!FunctionGraphMorph methodsFor: 'initialization' stamp: 'jmv 7/1/2015 11:16'!
initialize
	super initialize.
	extent _ 320@240.
	functions _ OrderedCollection new.
	colors _ OrderedCollection new! !

!FunctionGraphMorph methodsFor: 'drawing' stamp: 'jmv 5/17/2015 09:23'!
drawOn: aCanvas
	"
	| g |
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: 1 ] color: Color green.
	g addFunction: [ :x | x div: 1 ] color: Color red.
	g openInWorld
	"
	| r y |
	r _ self morphLocalBounds.
	aCanvas
		fillRectangle: r
		color: (Color gray: 0.9).
	aCanvas
		frameAndFillRectangle: r fillColor: (Color gray: 0.9)
		borderWidth: 0.05 borderColor: Color white.
	aCanvas line: (self toMorphic:0@yMin) to: (self toMorphic: 0 @ yMax) width: 2 color: Color lightGray.
	aCanvas line: (self toMorphic: xMin@0) to: (self toMorphic: xMax@0) width: 2 color: Color lightGray.

	xMin*1.0 to: xMax count: 600 do: [ :x |
		functions with: colors do: [ :f :c |
			y _ f value: x.
			(y between: yMin and: yMax) ifTrue: [
				aCanvas
					fillRectangle: ((self xToMorphic: x)@(self yToMorphic: y) extent: 5)
					color: c ]]]! !

!methodRemoval: FunctionGraphMorph #isOrthoRectangularMorph!
FunctionGraphMorph removeSelector: #isOrthoRectangularMorph!
!methodRemoval: FunctionGraphMorph #morphExtent!
FunctionGraphMorph removeSelector: #morphExtent!
!methodRemoval: FunctionGraphMorph #morphTopLeft!
FunctionGraphMorph removeSelector: #morphTopLeft!
!classDefinition: #FunctionGraphMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #FunctionGraphMorph
	instanceVariableNames: 'xMin xMax yMin yMax functions colors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!FunctionGraphMorph reorganize!
('accessing' addFunction:color: domain:)
('initialization' initialize)
('drawing' drawOn:)
('geometry' toMorphic: xToMorphic: yToMorphic:)
!

