"
I describe the behavior of my sole instance, nil. nil represents a prior value for variables that have not been initialized, or for results which are meaningless.
"
Class {
	#name : 'UndefinedObject',
	#superclass : 'Object',
	#category : 'Kernel-Objects',
	#package : 'Kernel',
	#tag : 'Objects'
}

{ #category : 'accessing' }
UndefinedObject class >> allInstancesDo: aBlock [
	"It is well known there is a single instance"
	
	aBlock value: nil
]

{ #category : 'instance creation' }
UndefinedObject class >> initializedInstance [
	^ nil
]

{ #category : 'instance creation' }
UndefinedObject class >> new [
	self error: 'You may not create any more undefined objects--use nil'
]

{ #category : 'dependents access' }
UndefinedObject >> addDependent: ignored [
	self error: 'Nil should not have dependents'
]

{ #category : 'class hierarchy' }
UndefinedObject >> addSubclass: aClass [
	"Ignored -- necessary to support disjoint class hierarchies"
]

{ #category : 'class hierarchy' }
UndefinedObject >> allSuperclassesUpTo: aSuperclass  [

	self error: (self superclassNotValidErrorDescriptionFor: aSuperclass)
]

{ #category : 'bottom context' }
UndefinedObject >> canHandleSignal: exception [
	"When no more handler (on:do:) context left in sender chain this gets called"

	^ false
]

{ #category : 'bottom context' }
UndefinedObject >> handleSignal: exception [
	"When no more handler (on:do:) context left in sender chain this gets called.  Return from signal with default action."

	^ exception resumeUnchecked: exception defaultAction
]

{ #category : 'testing' }
UndefinedObject >> ifNil: aBlock [
	"A convenient test, in conjunction with Object ifNil:"

	^ aBlock value
]

{ #category : 'testing' }
UndefinedObject >> ifNil: nilBlock ifNotNil: ifNotNilBlock [
	"Evaluate the block for nil because I'm == nil"

	^ nilBlock value
]

{ #category : 'testing' }
UndefinedObject >> ifNotNil: aBlock [
	"A convenient test, in conjunction with Object ifNotNil:"

	^ self
]

{ #category : 'testing' }
UndefinedObject >> ifNotNil: ifNotNilBlock ifNil: nilBlock  [
	"If I got here, I am nil, so evaluate the block nilBlock"

	^ nilBlock value
]

{ #category : 'testing' }
UndefinedObject >> isEmptyOrNil [
	"Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"
	^ true
]

{ #category : 'testing' }
UndefinedObject >> isLiteral [
	^ true
]

{ #category : 'testing' }
UndefinedObject >> isNil [
	^true
]

{ #category : 'class hierarchy' }
UndefinedObject >> literalScannedAs: scannedLiteral notifying: requestor  [
	^ scannedLiteral
]

{ #category : 'testing' }
UndefinedObject >> notNil [
	^false
]

{ #category : 'printing' }
UndefinedObject >> printAsLiteralOn: aStream [
	^self printOn: aStream
]

{ #category : 'printing' }
UndefinedObject >> printOn: aStream  [
	aStream nextPutAll: 'nil'
]

{ #category : 'class hierarchy' }
UndefinedObject >> removeSubclass: aClass [
	"Ignored -- necessary to support disjoint class hierarchies"
]

{ #category : 'copying' }
UndefinedObject >> shallowCopy [
	"Only one instance of UndefinedObject should ever be made, so answer 
	with self."
]

{ #category : 'printing' }
UndefinedObject >> storeOn: aStream  [
	aStream nextPutAll: 'nil'
]

{ #category : 'class hierarchy' }
UndefinedObject >> subclass: nameOfClass   [
	instanceVariableNames: instVarNames
	classVariableNames: classVarNames
	poolDictionaries: poolDictnames
	category: category
	"Calling this method is now considered an accident.  If you really want to create a class with a nil superclass, then create the class and then set the superclass using #superclass:"
	Transcript show: ('Attempt to create ', nameOfClass, ' as a subclass of nil.  Possibly a class is being loaded before its superclass.'); newLine.
	^ProtoObject
		subclass: nameOfClass
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		poolDictionaries: poolDictnames
		category: category

]

{ #category : 'class hierarchy' }
UndefinedObject >> subclasses [
	"Return all the subclasses of nil"

	^Array streamContents: [ :classList |
		self subclassesDo: [ :class | classList nextPut: class ]].
]

{ #category : 'class hierarchy' }
UndefinedObject >> subclassesDo: aBlock [
	"Evaluate aBlock with all subclasses of nil."
	^Class subclassesDo:[:cl| 
		cl isMeta ifTrue:[aBlock value: cl soleInstance]].
]

{ #category : 'class hierarchy' }
UndefinedObject >> superclassNotValidErrorDescriptionFor: aClass  [

	^aClass name, ' not in superclasses chain'
]

{ #category : 'dependents access' }
UndefinedObject >> suspend [
	"Kills off processes that didn't terminate properly"
	"Display reverse; reverse."  "<-- So we can catch the suspend bug"
	Processor terminateActive
]

{ #category : 'class hierarchy' }
UndefinedObject >> typeOfClass [
	"Necessary to support disjoint class hierarchies."
	^#normal
]

{ #category : 'evaluating' }
UndefinedObject >> valueWithPossibleArgs: anArray [
	"To provide polymorphism with blocks. Allows nil to be used instead of an empty closure."
]

{ #category : 'evaluating' }
UndefinedObject >> valueWithPossibleArgument: anArg [
	"To provide polymorphism with blocks. Allows nil to be used instead of an empty closure."
]

{ #category : 'evaluating' }
UndefinedObject >> valueWithPossibleArgument: anArg and: secondArg [
	"To provide polymorphism with blocks. Allows nil to be used instead of an empty closure."
]

{ #category : 'accessing instances and variables' }
UndefinedObject >> whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock [

	^aNoneBlock value
]

{ #category : 'class hierarchy' }
UndefinedObject >> withAllSuperclassesDownFrom: aSuperclass [

	self error: (self superclassNotValidErrorDescriptionFor: aSuperclass)
]
