"
Hierachy of UI Dialogs, both Modal and Non-Modal.
"
Class {
	#name : 'Dialog',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'layoutMorph'
	],
	#category : 'Morphic-Composite Widgets',
	#package : 'Morphic',
	#tag : 'Composite Widgets'
}

{ #category : 'instance creation' }
Dialog class >> open [
	"Open a Non-Modal Dialog. Answer user's response (if OK) or nil (if Cancel)
	This is an abstract class. Better play with SampleModalDialog. Inspect the result of:
		SampleModalDialog open
	"
	| world dialog |
	world := self runningWorld ifNil: [UISupervisor ui].
	dialog := self new beColumn buildWidgets.
	world addMorph: dialog centeredNear: world activeHand morphPosition.
	^ dialog.
]

{ #category : 'layout' }
Dialog >> beColumn [
	layoutMorph
		ifNotNil: [ layoutMorph beColumn ]
		ifNil: [
			layoutMorph := LinearLayoutMorph newColumn.
			self addMorphFront: layoutMorph ]
]

{ #category : 'initialization' }
Dialog >> buildWidgets [
	"Redefine as appropriate.
	At the end, do
		self ensureMinimumExtent.
	or
		self morphExtent: self minimumExtent + xtraSpace.
	or similar, as #defaultExtent is nil."

	self subclassResponsibility.
]

{ #category : 'defaults' }
Dialog >> defaultColor [
	^ Theme current defaultWindowColor
]

{ #category : 'defaults' }
Dialog >> defaultExtent [
	^nil
]

{ #category : 'defaults' }
Dialog >> defaultPadding [
	^ `30@10`
]

{ #category : 'geometry' }
Dialog >> extentChanged: oldExtent [
	"Our extent changed.
	- Resize our layoutMorph"

	layoutMorph ifNotNil: [
		layoutMorph
			position: self interiorOrigin
			extent: self interiorExtent ].
]

{ #category : 'geometry' }
Dialog >> minimumExtent [
	^layoutMorph minimumExtent + (borderWidth + padding * 2)
]
