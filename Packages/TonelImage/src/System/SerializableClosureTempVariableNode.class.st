Class {
	#name : 'SerializableClosureTempVariableNode',
	#superclass : 'TempVariableNode',
	#instVars : [
		'capturedIndex'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'accessing' }
SerializableClosureTempVariableNode >> capturedIndex: idx [

	capturedIndex := idx
]

{ #category : 'testing' }
SerializableClosureTempVariableNode >> isCapturedByClosure [

	^capturedIndex notNil
]

{ #category : 'printing' }
SerializableClosureTempVariableNode >> printIndirectOn: aStream indent: level  [
	
	self isRemote
		ifTrue: [ aStream nextPutAll: 'capturedValues last at: ', capturedIndex printString ]
		ifFalse: [ aStream nextPutAll: 'capturedValues at: ', capturedIndex printString ]
]

{ #category : 'printing' }
SerializableClosureTempVariableNode >> printOn: aStream indent: level [

	"For temps local to the block"
	self isCapturedByClosure ifFalse: [
		^super printOn: aStream indent: level ].

	"For outer temps"
	aStream nextPut: $(.
	self printIndirectOn: aStream indent: level.
	aStream nextPut: $).
]
