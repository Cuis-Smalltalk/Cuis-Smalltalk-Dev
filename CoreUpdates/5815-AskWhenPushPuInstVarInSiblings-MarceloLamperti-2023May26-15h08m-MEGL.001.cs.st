'From Cuis 6.0 [latest update: #5813] on 26 May 2023 at 3:34:23 pm'!

!PushUpInstanceVariable methodsFor: 'applying - private' stamp: 'MEGL 3/28/2023 13:08:11'!
removeSubclassesInstanceVariable

	classToRefactor superclass allSubclassesDo: [ :subclass |
		(subclass definesInstanceVariableNamed: instanceVariableToMove) ifTrue: [ subclass removeInstVarName: instanceVariableToMove].
	].! !


!PushUpInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 5/26/2023 15:27:05'!
chooseInstanceVariable

	| withSameInstanceVariable |

	super chooseInstanceVariable.
	
	withSameInstanceVariable := self findOtherSubclassesWithSameInstanceVariable.
	withSameInstanceVariable isEmpty ifFalse: [ 
		(PopUpMenu confirm: (self proceedMessageFor: withSameInstanceVariable)) ifFalse:  [ self endRequest ]].! !

!PushUpInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 3/28/2023 23:36:55'!
findOtherSubclassesWithSameInstanceVariable

	^ (classToRefactor superclass allSubclasses
		select: [ :aSubclass | aSubclass definesInstanceVariableNamed: instanceVariableName ])
		reject: [ :aSubclass | aSubclass = classToRefactor ].! !

!PushUpInstanceVariableApplier methodsFor: 'refactoring - parameters request' stamp: 'MEGL 5/26/2023 15:21:35'!
proceedMessageFor: withSameInstanceVariable

	^ String streamContents: [ :stream |
		stream
			nextPutAll: instanceVariableName;
			nextPutAll: ' is also declared in '.
		withSameInstanceVariable asCommaSeparated:  [:aClass| stream print: aClass] on: stream.
		stream nextPutAll: '. Do you want to proceed?' ]! !


!PushUpInstanceVariableApplier methodsFor: 'refactoring - creation' stamp: 'MEGL 5/26/2023 15:22:40'!
createRefactoring

	^PushUpInstanceVariable named: instanceVariableName from: classToRefactor
	! !

!methodRemoval: PushUpInstanceVariableApplier #confirmRefactorWhenInstanceVariableAlreadyExistsInTheHierarchy stamp: 'MEGL 5/26/2023 15:27:31'!
PushUpInstanceVariableApplier removeSelector: #confirmRefactorWhenInstanceVariableAlreadyExistsInTheHierarchy!
!methodRemoval: PushUpInstanceVariableApplier #requestRefactoringParameters stamp: 'MEGL 5/26/2023 15:27:10'!
PushUpInstanceVariableApplier removeSelector: #requestRefactoringParameters!

!PushUpInstanceVariable reorganize!
('applying' apply)
('applying - private' removeSubclassesInstanceVariable)
!


!PushUpInstanceVariableApplier reorganize!
('refactoring - parameters request' chooseInstanceVariable findOtherSubclassesWithSameInstanceVariable proceedMessageFor: selectVariableLabel)
('refactoring - creation' createRefactoring)
!

