Class {
	#name : 'AddAsSubclassResponsibility',
	#superclass : 'TransferMethod',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
AddAsSubclassResponsibility class >> for: aMethod [

	self warnIfMethodIsImplementedInSuperClass: aMethod.

	^self new initializeFor: aMethod
]

{ #category : 'applying - private' }
AddAsSubclassResponsibility >> moveMethod [

	self implementSubclassResponsibilityIn: method methodClass superclass
	
]

{ #category : 'applying - private' }
AddAsSubclassResponsibility >> removeMethod [

	
]
