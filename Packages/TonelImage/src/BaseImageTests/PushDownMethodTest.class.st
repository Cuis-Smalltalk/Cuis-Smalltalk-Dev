Class {
	#name : 'PushDownMethodTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
PushDownMethodTest >> assert: classToRefactor implementsSendingSubclassResponsibility: selector [

	| newMethod |

	newMethod := classToRefactor compiledMethodAt: selector ifAbsent: [ self fail ].
	self assert: newMethod isAbstract.
	self 
		assert: self methodToPushDownCategory 
		equals: (newMethod methodClass organization categoryOfElement: selector)
]

{ #category : 'class factory' }
PushDownMethodTest >> classToPushDownTo [

	^ #ClassToPushDownTo
]

{ #category : 'class factory' }
PushDownMethodTest >> classToPushDownTo2 [

	^ #ClassToPushDownTo2
]

{ #category : 'class factory' }
PushDownMethodTest >> classToRefactorName	 [

	^#ClassToPushMethodDown
]

{ #category : 'class factory' }
PushDownMethodTest >> methodToPushDownCategory [
	
	^'a category'
]

{ #category : 'class factory' }
PushDownMethodTest >> selectorToPushDown [

	^ #newMethod
]
