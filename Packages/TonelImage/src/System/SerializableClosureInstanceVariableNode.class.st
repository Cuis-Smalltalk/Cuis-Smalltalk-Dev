Class {
	#name : 'SerializableClosureInstanceVariableNode',
	#superclass : 'InstanceVariableNode',
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'printing' }
SerializableClosureInstanceVariableNode >> printIndirectOn: aStream indent: level [

	aStream nextPutAll: 'theSelf instVarNamed: ''', name, ''''
]

{ #category : 'printing' }
SerializableClosureInstanceVariableNode >> printOn: aStream indent: level  [

	aStream nextPut: $(.
	self printIndirectOn: aStream indent: level.
	aStream nextPut: $).
]
