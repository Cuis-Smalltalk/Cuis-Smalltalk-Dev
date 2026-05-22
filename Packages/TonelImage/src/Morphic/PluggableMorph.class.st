"
PluggableMorph have a model we observe upon, and whose state we reflect for the user to see and interact with.

We are "pluggable" in the sense of PluggableViews in MVC.
See subclasses for details.
"
Class {
	#name : 'PluggableMorph',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'model',
		'menuNshortcutsKey'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
PluggableMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"
	^ false
]

{ #category : 'access' }
PluggableMorph >> balloonText [
	"Answer balloon help text or nil, if no help is available.
	NB: subclasses may override such that they programatically
	construct the text, for economy's sake, such as model phrases in
	a Viewer"

	| balloonText |
	balloonText := super balloonText.
	balloonText isSymbol ifTrue: [ ^model perform: balloonText ].
	^ balloonText
]

{ #category : 'submorphs-add/remove' }
PluggableMorph >> clearDependencyAndEvents [

	model removeActionsWithReceiver: self.
	super clearDependencyAndEvents.
]

{ #category : 'initialization' }
PluggableMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 0
]

{ #category : 'initialization' }
PluggableMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ `Color lightGray`
]

{ #category : 'classification' }
PluggableMorph >> hasModel [
	^true
]

{ #category : 'initialization' }
PluggableMorph >> initialize [
	super initialize.
	extent := `200@100`
]

{ #category : 'access' }
PluggableMorph >> menuNshortcutsKey [
	"See senders"
	^menuNshortcutsKey
]

{ #category : 'access' }
PluggableMorph >> menuNshortcutsKey: aSymbol [
	"See senders"
	menuNshortcutsKey := aSymbol.
]

{ #category : 'access' }
PluggableMorph >> model  [
	^ model
]

{ #category : 'initialization' }
PluggableMorph >> model: anObject [
	"Set my model and make me me a dependent of the given object."

	model ifNotNil: [model removeActionsWithReceiver: self].
	anObject ifNotNil: [anObject addDependent: self].
	model := anObject
]

{ #category : 'caching' }
PluggableMorph >> releaseCachedState [
	"Release cached state of the receiver"

	(model ~~ self and: [model respondsTo: #releaseCachedState]) ifTrue:
		[model releaseCachedState].
	super releaseCachedState
]

{ #category : 'updating' }
PluggableMorph >> update: aspectSymbol [
	"Receive a change notice from an object we are observing. 
	The default behavior is to do nothing.
	Subclasses should act appropriately on the aspects they are interested in."

	^self
]
