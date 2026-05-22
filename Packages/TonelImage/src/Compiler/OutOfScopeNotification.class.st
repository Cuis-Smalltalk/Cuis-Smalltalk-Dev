Class {
	#name : 'OutOfScopeNotification',
	#superclass : 'Notification',
	#category : 'Compiler-Exceptions',
	#package : 'Compiler',
	#tag : 'Exceptions'
}

{ #category : 'as yet unclassified' }
OutOfScopeNotification >> defaultAction [

	self resume: false
]
