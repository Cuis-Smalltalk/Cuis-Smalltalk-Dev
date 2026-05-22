Class {
	#name : 'PushUpInstanceVariableApplier',
	#superclass : 'TransferInstanceVariableApplier',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'refactoring - parameters request' }
PushUpInstanceVariableApplier >> chooseInstanceVariable [

	| withSameInstanceVariable |

	super chooseInstanceVariable.
	
	withSameInstanceVariable := self findOtherSubclassesWithSameInstanceVariable.
	withSameInstanceVariable isEmpty ifFalse: [ 
		(PopUpMenu confirm: (self proceedMessageFor: withSameInstanceVariable)) ifFalse:  [ self endRequest ]].
]

{ #category : 'refactoring - creation' }
PushUpInstanceVariableApplier >> createRefactoring [

	^PushUpInstanceVariable named: instanceVariableName from: classToRefactor
	
]

{ #category : 'refactoring - parameters request' }
PushUpInstanceVariableApplier >> findOtherSubclassesWithSameInstanceVariable [

	^ (classToRefactor superclass allSubclasses
		select: [ :aSubclass | aSubclass definesInstanceVariableNamed: instanceVariableName ])
		reject: [ :aSubclass | aSubclass = classToRefactor ].
]

{ #category : 'refactoring - parameters request' }
PushUpInstanceVariableApplier >> proceedMessageFor: withSameInstanceVariable [

	^ String streamContents: [ :stream |
		stream
			nextPutAll: instanceVariableName;
			nextPutAll: ' is also declared in '.
		withSameInstanceVariable asCommaSeparated:  [:aClass| stream print: aClass] on: stream.
		stream nextPutAll: '. Do you want to proceed?' ]
]

{ #category : 'refactoring - parameters request' }
PushUpInstanceVariableApplier >> selectVariableLabel [
	
	^'Select instance variable to push up'
]
