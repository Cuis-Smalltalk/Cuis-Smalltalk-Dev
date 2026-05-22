Class {
	#name : 'PoolDefinitionNotification',
	#superclass : 'Notification',
	#instVars : [
		'name'
	],
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'instance creation' }
PoolDefinitionNotification class >> named: aName [

	^self new initializeNamed: aName
]

{ #category : 'signaling' }
PoolDefinitionNotification class >> signalNamed: aName [

	^(self named: aName) signal
]

{ #category : 'exception handling' }
PoolDefinitionNotification >> defaultAction [

	| shouldResume |
	
	shouldResume := PopUpMenu confirm: 'The pool dictionary ', name,' does not exist.',
						'\Do you want it automatically created?' withNewLines.
						
	^ self resume: shouldResume 
]

{ #category : 'initialization' }
PoolDefinitionNotification >> initializeNamed: aName [

	name := aName 
]
