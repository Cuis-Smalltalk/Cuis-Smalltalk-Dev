'From Cuis 4.0 of 21 April 2012 [latest update: #1508] on 6 December 2012 at 10:40:25 am'!

!FormCanvas methodsFor: 'private' stamp: 'jmv 3/18/2011 10:35'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor screen patternWord |
	paintColor _ shadowColor ifNil: [ aColor ].
	paintColor ifNil: [ paintColor _ Color transparent].
	(paintColor is: #Color) ifFalse:[
		(paintColor isKindOf: InfiniteForm) ifFalse:[^self error:'Cannot install color'].
		^port fillPattern: paintColor; combinationRule: Form paint].
	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isOpaque) ifTrue: [
		port fillPattern: paintColor.
		port combinationRule: Form paint.
		self depth = 8 ifTrue:[
			"In 8 bit depth it's usually a good idea to use a stipple pattern"
			port fillColor: (form balancedPatternFor: paintColor)].
		^self].

	self depth > 8 ifTrue:[
		"BitBlt setup for alpha mapped transfer"
		port fillPattern: paintColor.
		self depth = 16
			ifTrue:[port alphaBits: paintColor privateAlpha; combinationRule: 31]
			ifFalse:[port combinationRule: Form blend].
		^self].

	"Can't represent actual transparency -- use stipple pattern"
	screen _ Color translucentMaskFor: paintColor alpha depth: self depth.
	patternWord _ form pixelWordFor: paintColor.
	port fillPattern: (screen collect: [:maskWord | maskWord bitAnd: patternWord]).
	port combinationRule: Form paint! !


!TheWorldMenu methodsFor: 'action' stamp: 'jmv 11/4/2011 10:23'!
doMenuItem: aCollection with: event
	| realTarget selector nArgs |
	selector _ aCollection second.
	nArgs _ selector numArgs.
	realTarget _ aCollection first.
	realTarget == #myWorld ifTrue: [realTarget _ myWorld].
	realTarget == #myHand ifTrue: [realTarget _ myHand].
	realTarget == #theWorldMenu ifTrue: [realTarget _ self].
	^nArgs = 0 
		ifTrue:[realTarget perform: selector]
		ifFalse:[realTarget perform: selector with: event].
! !

