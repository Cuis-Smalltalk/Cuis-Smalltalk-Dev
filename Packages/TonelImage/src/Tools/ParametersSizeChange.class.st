Class {
	#name : 'ParametersSizeChange',
	#superclass : 'ChangeSelector',
	#instVars : [
		'newImplementors',
		'changedSenders'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'changes' }
ParametersSizeChange >> changedSenders [
	
	^changedSenders
]

{ #category : 'create new implementors - private' }
ParametersSizeChange >> createNewImplementorOf: anImplementor [

	| newImplementor |
	
	newImplementor := super createNewImplementorOf: anImplementor.
	
	newImplementors add: newImplementor 
	
	
]

{ #category : 'initialization' }
ParametersSizeChange >> initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders [

	super initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders.
	
	newImplementors := Set new.
	changedSenders := Set new
]

{ #category : 'changes' }
ParametersSizeChange >> newImplementors [
	
	^newImplementors
]

{ #category : 'rename senders - private' }
ParametersSizeChange >> renameSendersIn: aMethod [

	| changedSender |
	
	changedSender := super renameSendersIn: aMethod.
	
	changedSenders add: changedSender
]
