'From Cuis 6.0 [latest update: #5901] on 12 July 2023 at 4:54:52 pm'!

!PushUpMethod methodsFor: 'applying - private' stamp: 'MEGL 7/12/2023 16:29:11'!
moveMethodAfterMovingAccessedVariables

	method methodClass superclass
		compile: method sourceCode
		classified: self methodCategory! !

!PushUpMethod methodsFor: 'applying - private' stamp: 'MEGL 7/12/2023 16:49:35'!
pushUpAccessedVariables

	|instanceVariablesToPushUp|

	instanceVariablesToPushUp := self class selectAccessedInstanceVariables: method.
	instanceVariablesToPushUp do: [:var | (PushUpInstanceVariable named: var from: method methodClass) apply ]! !


!PushUpMethod class methodsFor: 'warnings' stamp: 'MEGL 7/12/2023 16:47:28'!
accessingInstanceVariableWarningMessage: accessedInstanceVariables

	^String streamContents: [ :stream |
		stream 
			nextPutAll: 'The instance variable(s) ';
			nextPutAll: accessedInstanceVariables asCommaStringAnd;
			nextPutAll: ' will be pushed up' ]! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'MEGL 7/12/2023 16:49:35'!
selectAccessedInstanceVariables: aMethodToPushUp

	^aMethodToPushUp methodClass instVarNames
		select: [ :instVarName |  aMethodToPushUp accessesInstanceVariable: instVarName ]! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'MEGL 7/12/2023 16:49:35'!
warnIfMethodAccessInstanceVariable: aMethodToPushUp

	| accessedInstanceVariables |

	accessedInstanceVariables := self selectAccessedInstanceVariables: aMethodToPushUp.

	accessedInstanceVariables isEmpty ifTrue: [^self].
	
	accessedInstanceVariables do: [ :variable | PushUpInstanceVariable assertCanMove: variable from: aMethodToPushUp methodClass ].

	self refactoringWarning: (self accessingInstanceVariableWarningMessage: accessedInstanceVariables)! !


!PushUpMethod methodsFor: 'applying - private' stamp: 'MEGL 7/12/2023 16:28:50'!
moveMethod

	self 
		pushUpAccessedVariables;
		moveMethodAfterMovingAccessedVariables! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'MEGL 7/6/2023 17:33:03'!
for: aMethodToPushUp additionallyRemovingFrom: subclassesToRemoveMethodFrom

	self warnIfMethodAccessInstanceVariable: aMethodToPushUp.
	self assert: subclassesToRemoveMethodFrom areSiblingsOf: aMethodToPushUp methodClass.
	self assert: aMethodToPushUp isImplementedIn: subclassesToRemoveMethodFrom.
	self assert: aMethodToPushUp isEquivalentlyImplementedIn: subclassesToRemoveMethodFrom.
	self warnIfExistMethodToPushUpOnSuperClass: aMethodToPushUp.
	
	^self new initializeFor: aMethodToPushUp additionallyRemovingFrom: subclassesToRemoveMethodFrom.! !

!methodRemoval: PushUpMethod class #assertIsNotAccessingInstanceVariable: stamp: 'MEGL 7/12/2023 16:53:06'!
PushUpMethod class removeSelector: #assertIsNotAccessingInstanceVariable:!
!methodRemoval: PushUpMethod class #findAccessedInstanceVariables: stamp: 'MEGL 7/12/2023 16:49:35'!
PushUpMethod class removeSelector: #findAccessedInstanceVariables:!
!methodRemoval: PushUpMethod class #assertIsValidToPushUpMethod: stamp: 'MEGL 7/12/2023 16:53:09'!
PushUpMethod class removeSelector: #assertIsValidToPushUpMethod:!

!PushUpMethod reorganize!
('applying - private' moveMethod moveMethodAfterMovingAccessedVariables pushUpAccessedVariables removeMethod)
('initialization' initializeFor:additionallyRemovingFrom:)
!


!PushUpMethod class reorganize!
('instance creation' for: for:additionallyRemovingFrom:)
('pre-conditions' assert:areSiblingsOf: assert:isEquivalentlyImplementedIn: assert:isImplementedIn:)
('warnings' accessingInstanceVariableWarningMessage: methodToPushUpExistOnSuperclassWarningMessage selectAccessedInstanceVariables: warnIfExistMethodToPushUpOnSuperClass: warnIfMethodAccessInstanceVariable:)
('exceptions' errorMessageForMethodCannotAccessInstanceVariable errorMessageForMethodCannotBeRemovedMethodFrom: errorMessageForNoSiblings: errorMessageForNotEquivalentMethods: signalMethodCannotAccessInstanceVariable signalMethodCannotBeRemovedMethodFrom: signalNoSiblings: signalNotEquivalentMethodsFor:)
!

