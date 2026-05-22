"
I represent a selection setting and actions to take depending on a change in the setting. An instance has three attributes: state, which is either on or off; on action; and off action. The on and off actions are blocks of code that execute whenever the instance changes state.
"
Class {
	#name : 'Switch',
	#superclass : 'ActiveModel',
	#instVars : [
		'on',
		'onAction',
		'offAction'
	],
	#category : 'Tools-Menus',
	#package : 'Tools',
	#tag : 'Menus'
}

{ #category : 'instance creation' }
Switch class >> new [
	"Answer an instance of me such that the on and off actions are set to nil
	('no action'), and the state is set to 'off'."

	^self newOff
]

{ #category : 'instance creation' }
Switch class >> newOff [
	"Answer an instance of me such that the on and off actions are set to nil
	('no action'), and the state is set to 'off'."

	^super new initializeOff
]

{ #category : 'instance creation' }
Switch class >> newOn [
	"Answer an instance of me such that the on and off actions are set to nil
	('no action'), and the state is set to 'on'."

	^super new initializeOn
]

{ #category : 'state' }
Switch >> clear [
	"Set the state of the receiver to 'off'. If the state of the receiver was 
	previously 'on', then 'self change' is sent. The receiver's off action is 
	NOT executed."

	self isOn
		ifTrue: 
			[on := false.
			self changed: self]
]

{ #category : 'action' }
Switch >> doAction: anAction  [
	"Execute anAction if it is non-nil."

	anAction
		ifNotNil: [anAction value]
]

{ #category : 'private' }
Switch >> initializeOff [
	on := false. 
	onAction := nil.
	offAction := nil
]

{ #category : 'private' }
Switch >> initializeOn [
	on := true. 
	onAction := nil.
	offAction := nil
]

{ #category : 'state' }
Switch >> isOff [
	"Answer whether the receiver is set off or not."
	^on not
]

{ #category : 'state' }
Switch >> isOn [
	"Answer whether the receiver is set on or not."
	^on
]

{ #category : 'action' }
Switch >> offAction: anAction  [
	"Set the off action of the receiver to anAction."

	offAction := anAction
]

{ #category : 'action' }
Switch >> onAction: anAction  [
	"Set the on action of the receiver to anAction."

	onAction := anAction
]

{ #category : 'state' }
Switch >> set [
	"Set the state of the receiver to 'on'. If the state of the receiver was 
	previously 'off', then 'self change' is sent. The receiver's on action is 
	NOT executed."

	self isOff
		ifTrue: 
			[on := true.
			self changed: self]
]

{ #category : 'state' }
Switch >> switch [
	"Change the state of the receiver from 'on' to 'off' or from 'off' to 'on' (see 
	Switch|turnOn, Switch|turnOff)."

	self isOn
		ifTrue: [self turnOff]
		ifFalse: [self turnOn]
]

{ #category : 'state' }
Switch >> turnOff [
	"Set the state of the receiver to 'off'. If the state of the receiver was 
	previously 'on', then 'self change' is sent and the receiver's off action is 
	executed."

	self isOn
		ifTrue: 
			[on := false.
			self changed: #isOn.
			self doAction: offAction]
]

{ #category : 'state' }
Switch >> turnOn [
	"Set the state of the receiver to 'on'. If the state of the receiver was 
	previously 'off', then 'self change' is sent and the receiver's on action is 
	executed."

	self isOff
		ifTrue: 
			[on := true.
			self changed: #isOn.
			self doAction: onAction]
]
