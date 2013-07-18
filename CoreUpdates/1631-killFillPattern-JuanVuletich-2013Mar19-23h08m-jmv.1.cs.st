'From Cuis 4.1 of 12 December 2012 [latest update: #1630] on 19 March 2013 at 11:39:50 pm'!
!classDefinition: #GrafPort category: #'Morphic-Support'!
BitBlt subclass: #GrafPort
	instanceVariableNames: 'alpha fillPattern '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!FormCanvas methodsFor: 'private' stamp: 'jmv 3/19/2013 23:27'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ shadowColor ifNil: [ aColor ].
	paintColor ifNil: [ paintColor _ Color transparent ].
	(paintColor is: #Color) ifFalse: [
		(paintColor isKindOf: InfiniteForm) ifFalse: [ ^self error: 'Cannot install color' ].
		^port fillColor: paintColor; combinationRule: Form paint ].

	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isOpaque or: [ self depth < 16]) ifTrue: [
		port fillColor: paintColor.
		port combinationRule: Form paint.
		^self ].

	"BitBlt setup for alpha mapped transfer"
	port fillColor: paintColor.
	self depth = 16
		ifTrue: [ port alphaBits: paintColor privateAlpha; combinationRule: 31 ]
		ifFalse: [ port combinationRule: Form blend ]! !

!methodRemoval: GrafPort #fillPattern:!
GrafPort removeSelector: #fillPattern:!
!classDefinition: #GrafPort category: #'Morphic-Support'!
BitBlt subclass: #GrafPort
	instanceVariableNames: 'alpha'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
