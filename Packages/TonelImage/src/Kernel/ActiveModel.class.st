"
The new event model.
This includes #when:send:to: and friends, in Object (generic implementation) and ActiveModel (optimized implementation).
It renders the old change/update mechanism in Object and Model as obsolete, although still used.
"
Class {
	#name : 'ActiveModel',
	#superclass : 'Object',
	#instVars : [
		'actionMap'
	],
	#classVars : [
		'ActionMaps'
	],
	#category : 'Kernel-Objects',
	#package : 'Kernel',
	#tag : 'Objects'
}

{ #category : 'accessing' }
ActiveModel class >> actionMapFor: anObject [

    ^self actionMaps
        at: anObject
        ifAbsent: nil
]

{ #category : 'accessing' }
ActiveModel class >> actionMaps [

	ActionMaps ifNil: [
		ActionMaps := EphemeronIdentityDictionary new ].
	^ActionMaps
]

{ #category : 'initialization' }
ActiveModel class >> flushEventSystem [
	"ActiveModel flushEventSystem"
	self actionMaps keysAndValuesDo: [ :rcvr :evtDict | 
		rcvr ifNotNil:[
			"make sure we don't modify evtDict while enumerating"
			evtDict keys do: [ :evtName | | msgSet |
				msgSet := evtDict at: evtName ifAbsent: nil.
				msgSet ifNil: [ rcvr removeActionsForEvent: evtName]]]].
]

{ #category : 'initialization' }
ActiveModel class >> recreateActionMapsCollection [
	"
	ActiveModel recreateActionMapsCollection
	"
	| newOne |
	newOne := EphemeronIdentityDictionary new.
	ActionMaps keysAndValuesDo: [ :k :v |
		newOne at: k put: v ].
	ActionMaps := newOne.
]

{ #category : 'releasing' }
ActiveModel class >> releaseActionMapFor: anObject [

	self actionMaps
		removeKey: anObject
		ifAbsent: nil
]

{ #category : 'accessing' }
ActiveModel class >> updateableActionMapFor: anObject [

	^self actionMaps
		at: anObject
		ifAbsentPut: [ IdentityDictionary new ]
]

{ #category : 'accessing' }
ActiveModel >> actionMap [

    ^actionMap
]

{ #category : 'copying' }
ActiveModel >> postCopy [

	actionMap := nil
]

{ #category : 'accessing' }
ActiveModel >> releaseActionMap [

    actionMap := nil
]

{ #category : 'accessing' }
ActiveModel >> removeAllActions [

    actionMap := nil
]

{ #category : 'accessing' }
ActiveModel >> updateableActionMap [

	actionMap ifNil: [
		actionMap := IdentityDictionary new ].
	^actionMap
]
