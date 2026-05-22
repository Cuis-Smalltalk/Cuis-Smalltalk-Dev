"
AssertionFailure is the exception signaled from Object>>assert: when the assertion block evaluates to false.
"
Class {
	#name : 'AssertionFailure',
	#superclass : 'Exception',
	#category : 'Exceptions-Extensions',
	#package : 'Exceptions',
	#tag : 'Extensions'
}

{ #category : 'priv handling' }
AssertionFailure >> defaultAction [

	self noHandler
]

{ #category : 'description' }
AssertionFailure >> isResumable [

	^true
]
