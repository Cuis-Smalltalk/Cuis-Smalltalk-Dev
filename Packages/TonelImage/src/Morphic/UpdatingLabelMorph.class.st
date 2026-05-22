"
UpdatingLabelMorph new
     target: [self runningWorld activeHand morphPosition asString];
     getSelector: #value;
     stepTime: 10;
     openInWorld
"
Class {
	#name : 'UpdatingLabelMorph',
	#superclass : 'LabelMorph',
	#instVars : [
		'target',
		'getSelector',
		'stepTime'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
UpdatingLabelMorph class >> categoryInNewMorphMenu [

	^ 'User Interface'
]

{ #category : 'new-morph participation' }
UpdatingLabelMorph class >> initializedInstance [
	"Answer a digital clock"

	| newInst |
	newInst := self 
		contents: '' 
		font: (Preferences at: #windowTitleFont)
		emphasis: AbstractFont  boldCode.
	newInst 
		stepTime: 500; "half a second"
		target: [String streamContents: [ :strm | DateAndTime now printHMSOn: strm]] ;
		getSelector: #value;
		setBalloonText: [ Date today asString ].

	^ newInst
]

{ #category : 'geometry' }
UpdatingLabelMorph >> fitContents [
	"Don't shrink each time contents change.
	Might shrink during layout"
	self morphExtent: (extent max: self measureContents)
]

{ #category : 'accessing' }
UpdatingLabelMorph >> getSelector: aSymbol [
	getSelector := aSymbol
]

{ #category : 'initialization' }
UpdatingLabelMorph >> initialize [
	super initialize.
	target := self.
	getSelector := #contents.
	stepTime := 50
]

{ #category : 'accessing' }
UpdatingLabelMorph >> showPrintStringFor: anObject [

	self contents: anObject printString
]

{ #category : 'stepping' }
UpdatingLabelMorph >> step [

	self contents: (target perform: getSelector) asString.
]

{ #category : 'accessing' }
UpdatingLabelMorph >> stepTime [

	^stepTime
]

{ #category : 'accessing' }
UpdatingLabelMorph >> stepTime: aNumber [
	stepTime := aNumber
]

{ #category : 'accessing' }
UpdatingLabelMorph >> target: anObject [
	target := anObject
]

{ #category : 'stepping' }
UpdatingLabelMorph >> wantsSteps [
	"Return true if the receiver wants to its #step or #stepAt: methods be run"

	^true
]
