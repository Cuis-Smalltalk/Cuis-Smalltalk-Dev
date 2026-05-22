Class {
	#name : 'SerializableClosureAssignmentNode',
	#superclass : 'AssignmentNode',
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'printing' }
SerializableClosureAssignmentNode >> printOn: aStream indent: level [

	"For temps and args local to a block"
	(variable isTemp and: [variable isCapturedByClosure not]) ifTrue: [ ^super printOn: aStream indent: level ].

	"For outer temps, but also for ivars"
	aStream nextPutAll: '('.
	variable printIndirectOn: aStream indent: level.
	aStream nextPutAll: ' put: '.
	value printOn: aStream indent: level.
	aStream nextPutAll: ')'.
]
