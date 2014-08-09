'From Cuis 4.2 of 25 July 2013 [latest update: #2062] on 6 August 2014 at 9:07:57.9884 am'!
"Change Set:		2061-CuisCore-AuthorName-2014Jul30-10h01m
Date:			30 July 2014
Author:			Juan Vuletich

<your descriptive text goes here>"
SystemWindow allSubInstancesDo: [ :w | w instVarNamed: 'borderColor' put: (w instVarNamed: 'widgetsColor') ]!

!classDefinition: #SystemWindow category: #'Morphic-Views for Models'!
PluggableMorph subclass: #SystemWindow
	instanceVariableNames: 'labelString updatablePanes widgetsColor layoutMorph adjusters '
	classVariableNames: 'TopWindow '
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!PluggableScrollPane methodsFor: 'initialization' stamp: 'jmv 7/14/2014 14:44'!
defaultColor
	"answer the default border color/fill style for the receiver"
	^ Color white! !


!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 7/30/2014 09:38'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	color isTransparent ifFalse: [
		self color: (Theme current buttonColorFrom: paneColor) ]! !


!BorderedRectMorph methodsFor: 'accessing' stamp: 'jmv 7/30/2014 09:24'!
borderWidth: anInteger
	borderWidth = anInteger ifFalse: [
		borderWidth _ anInteger max: 0.
		self redrawNeeded ]! !


!PluggableMorph methodsFor: 'initialization' stamp: 'jmv 8/6/2014 09:00'!
defaultColor
	"answer the default color/fill style for the receiver"
	"necesito algo en las subclases?????"
	^ Color lightGray! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'jmv 8/6/2014 08:58'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Color lightGray! !


!PluggableScrollPane methodsFor: 'access' stamp: 'jmv 7/14/2014 15:28'!
adoptWidgetsColor: paneColor
	color _ Theme current paneBackgroundFrom: paneColor.
	super adoptWidgetsColor: paneColor.
	scrollBar adoptWidgetsColor: paneColor.
	hScrollBar adoptWidgetsColor: paneColor! !


!SystemWindow methodsFor: 'initialization' stamp: 'jmv 7/30/2014 09:27'!
openInWorld
	"Ensure all widgets have proper colors before opening"
	self widgetsColor: self windowColor.
	super openInWorld! !

!SystemWindow methodsFor: 'panes' stamp: 'jmv 7/30/2014 09:29'!
widgetsColor

	^borderColor! !

!SystemWindow methodsFor: 'panes' stamp: 'jmv 8/6/2014 08:54'!
widgetsColor: aColor
	"aColor will be used for titles, borders, etc.
	A variation of it, #paneColorFrom:, will be used for panes background"

	borderColor _ aColor.
	self color: self textBackgroundColor.
	self adoptWidgetsColor: borderColor! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 8/6/2014 09:02'!
buttonRowForPreDebugWindow: aDebugger
	| row aButton triads buttons |
	buttons _ OrderedCollection new.
	triads _ OrderedCollection withAll: self preDebugButtonSpec.
	(aDebugger interruptedContext selector == #doesNotUnderstand:) ifTrue: [
		triads add: { 'Create'. #createMethod. 'create the missing method' }
	].
	triads do: [ :triad |
		aButton _ PluggableButtonMorph new model: self.
		aButton label: triad first.
		aButton action: triad second.
		aButton setBalloonText: triad third.
		buttons add: aButton].
	
	row _ LayoutMorph newRow.
	row color: Color transparent.
	row separation: 1.
	row addMorphs: buttons.
	^row! !

!classDefinition: #SystemWindow category: #'Morphic-Views for Models'!
PluggableMorph subclass: #SystemWindow
	instanceVariableNames: 'labelString updatablePanes layoutMorph adjusters'
	classVariableNames: 'TopWindow'
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
