Class {
	#name : 'BehaviorTestSubclass',
	#superclass : 'BehaviorTest',
	#category : 'BaseImageTests-Kernel-Classes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Classes'
}

{ #category : 'as yet unclassified' }
BehaviorTestSubclass class >> isAbstract [

	^true
]

{ #category : 'test data' }
BehaviorTestSubclass >> methodReferencingReferencedVariable [

	^referenced  
]
