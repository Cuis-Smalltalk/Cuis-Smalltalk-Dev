"
An EphemeronIdentityDictionary is an identity dictionary that uses ephemerons for its associations, and hence is able to arrange that its keys are finalizable.  See Ephemeron's class comment.

Instance Variables
	mutex:		<Mutex>

mutex
	- an access protect that prevents the receiver getting corrupted as finalization occurs while lower priority processes are adding and/or rremoving ephemerons.

"
Class {
	#name : 'EphemeronIdentityDictionary',
	#superclass : 'IdentityDictionary',
	#instVars : [
		'mutex'
	],
	#category : 'System-Finalization',
	#package : 'System',
	#tag : 'Finalization'
}

{ #category : 'adding' }
EphemeronIdentityDictionary >> add: anAssociation [
	
	"Installs a copy of the association in myself.
	I cannot add the association because they are not shareable between weak key dictionaries, it needs myself as container.
	Moreover, I can only contain Ephemerons, and not other kinds of Association."
	self at: anAssociation key put: anAssociation value.

	^ anAssociation
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> associationAt: key ifAbsent: aBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super associationAt: key ifAbsent: aBlock]
]

{ #category : 'accessing' }
EphemeronIdentityDictionary >> associationClass [
	^Ephemeron
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> at: key ifAbsent: absentBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super at: key ifAbsent: absentBlock]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> at: key ifAbsentOrNilPut: aBlock [
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."

	^mutex critical: [ super at: key ifAbsentOrNilPut: aBlock ]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> at: key ifAbsentPut: aBlock [
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."

	^mutex critical: [ super at: key ifAbsentPut: aBlock ]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> at: key ifPresent: presentBlock ifAbsent: absentBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super at: key ifPresent: presentBlock ifAbsent: absentBlock ]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> at: key put: anObject [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super at: key put: anObject]
]

{ #category : 'private' }
EphemeronIdentityDictionary >> atNewIndex: index put: anEphemeron [

	anEphemeron container: self.
	^super atNewIndex: index put: anEphemeron
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> collect: aBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super collect: aBlock]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> do: aBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super do: aBlock]
]

{ #category : 'finalization' }
EphemeronIdentityDictionary >> finalizeEphemeron: anEphemeron [

	"Remove the ephemeron from myself.  Note that no finalization of the ephemeron's key is done.
	 Here we are using ephemerons to clean up property tables."

	self removeKey: anEphemeron key ifAbsent: nil
]

{ #category : 'finalization' }
EphemeronIdentityDictionary >> finalizeValues [
	"legacy systems might invoke this as it's an api on the old WeakRegistry"
	Smalltalk garbageCollect
]

{ #category : 'initialize-release' }
EphemeronIdentityDictionary >> init: n [
	mutex := Mutex new.
	super init: n
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> keys [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super keys]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> keysAndValuesDo: aBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super keysAndValuesDo: aBlock]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> keysAndValuesRemove: keyValueBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super keysAndValuesRemove: keyValueBlock]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> keysDo: aBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super keysDo: aBlock]
]

{ #category : 'copying' }
EphemeronIdentityDictionary >> postCopy [
	"Must copy the associations and tell them I (the copy) am their new container"

	mutex := Mutex new.
	mutex critical:
		[array := array collect: 
			[ :assoc |
			assoc ifNotNil:
				[assoc copy container: self ] ] ]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> rehash [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super rehash]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> removeAll [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super removeAll]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> removeKey: key ifAbsent: aBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us.
	Note: Usually removing from our instances is done automatically when the key is collected.
	This method usually doesn't need to be called by users of this class."

	^mutex critical: [super removeKey: key ifAbsent: aBlock]
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> select: aBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super select: aBlock]
]

{ #category : 'as yet unclassified' }
EphemeronIdentityDictionary >> slowSize [
	"Careful! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	| count |
	count := 0.
	self keysDo: [ :k | count := count + 1 ].
	^count
]

{ #category : 'access protected protocol' }
EphemeronIdentityDictionary >> valuesDo: aBlock [
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us."

	^mutex critical: [super valuesDo: aBlock]
]
