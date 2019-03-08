'From Cuis 5.0 of 7 November 2016 [latest update: #3649] on 7 March 2019 at 11:05:12 pm'!

!ExtractToTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 3/7/2019 22:52:44'!
assert: aNewVariable isNotDefinedAsInstanceVariableInHierarchyOf: aClass 
	
	| classDefiningNewVariable |
	
	classDefiningNewVariable := aClass whichClassDefinesInstanceVariable: aNewVariable ifNone: [ ^self ].
	self signal: aNewVariable canNotHideInstanceVariableDefinedIn: classDefiningNewVariable ! !

!ExtractToTemporary class methodsFor: 'warnings' stamp: 'HAW 3/7/2019 22:54:39'!
errorMessageFor: aNewVariable canNotBeNamedAsInstanceVariableDefinedIn: aClass

	^aNewVariable, ' can not be named as instance variable defined in ', aClass name! !

!ExtractToTemporary class methodsFor: 'warnings' stamp: 'HAW 3/7/2019 22:54:39'!
signal: aNewVariable canNotHideInstanceVariableDefinedIn: aClass 

	self refactoringError: (self errorMessageFor: aNewVariable canNotBeNamedAsInstanceVariableDefinedIn: aClass)! !


!RenameTemporary class methodsFor: 'pre-conditions' stamp: 'HAW 3/7/2019 22:52:44'!
assert: aNewVariable isNotDefinedAsInstanceVariableInHierarchyOf: aClass 
	
	| classDefiningNewVariable |
	
	classDefiningNewVariable := aClass whichClassDefinesInstanceVariable: aNewVariable ifNone: [ ^self ].
	self signal: aNewVariable canNotHideInstanceVariableDefinedIn: classDefiningNewVariable ! !

!RenameTemporary class methodsFor: 'warnings' stamp: 'HAW 3/7/2019 22:54:39'!
errorMessageFor: aNewVariable canNotBeNamedAsInstanceVariableDefinedIn: aClass

	^aNewVariable, ' can not be named as instance variable defined in ', aClass name! !

!RenameTemporary class methodsFor: 'warnings' stamp: 'HAW 3/7/2019 22:54:39'!
signal: aNewVariable canNotHideInstanceVariableDefinedIn: aClass 

	self refactoringError: (self errorMessageFor: aNewVariable canNotBeNamedAsInstanceVariableDefinedIn: aClass)! !


!NewInstanceVariablePrecondition methodsFor: 'warnings' stamp: 'HAW 3/7/2019 22:29:07'!
assertIsDefinedInMethods
	
	| methodsDefiningNewVariable |
	
	methodsDefiningNewVariable := self methodsDefiningNewVariable.
	
	methodsDefiningNewVariable notEmpty ifTrue: [ self signalNewVariable: instVarName willBeHiddenAtAll: methodsDefiningNewVariable ].! !

!NewInstanceVariablePrecondition methodsFor: 'warnings' stamp: 'HAW 3/7/2019 22:29:57'!
signalNewVariable: newVariable willBeHiddenAtAll: methods

	^ self refactoringError: (self class errorMessageForNewVariable: newVariable willBeHiddenAtAll: methods).! !


!NewInstanceVariablePrecondition class methodsFor: 'warning messages' stamp: 'HAW 3/7/2019 23:02:55'!
errorMessageForNewVariable: newVariable willBeHiddenAtAll: methods

	^String streamContents: [ :stream |
		stream 
			nextPutAll: newVariable;
			nextPutAll: ' can not be named as temporary/parameter in '.
		methods asCommaSeparated: [:aMethod | aMethod printClassAndSelectorOn: stream ] on: stream ]! !


!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'HAW 3/7/2019 22:51:29'!
named: aNewVariable at: anIntervalToExtract from: aMethodSourceCode in: aClass

	|  trimmedNewVariable parseNodeToExtract trimmedSourceCodeToExtract methodNodeToExtract methodNodeToRefactor sourceCodeToExtract |
	
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	methodNodeToRefactor := aClass methodNodeFor: aMethodSourceCode.
	
	self assertIsNotEmpty: trimmedNewVariable.
	self assertIsValidVariableName: trimmedNewVariable.
	self assert: trimmedNewVariable isNotDefinedIn: methodNodeToRefactor.
	
	sourceCodeToExtract := aMethodSourceCode copyFrom: anIntervalToExtract first to: anIntervalToExtract last.
	trimmedSourceCodeToExtract := sourceCodeToExtract withBlanksTrimmed.
	self assertSourceCodeIsNotEmpty: trimmedSourceCodeToExtract.
	self assertIsNotReturn: trimmedSourceCodeToExtract. 
	self assert: trimmedSourceCodeToExtract isIncludedIn: methodNodeToRefactor.

	[ methodNodeToExtract := Parser parse: trimmedSourceCodeToExtract class: methodNodeToRefactor methodClass noPattern: true ]
		on: SyntaxErrorNotification 
		do: [ :anError | self signalColaborationToExtractHasSyntaxError: anError ].
	
	parseNodeToExtract := self paseNodeToExtractFrom: methodNodeToExtract. 

	self assert: trimmedNewVariable isNotDefinedAsInstanceVariableInHierarchyOf: methodNodeToRefactor methodClass.

	^self new initializeNamed: trimmedNewVariable with: parseNodeToExtract in: methodNodeToRefactor
 
	! !

!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'HAW 3/7/2019 22:51:29'!
named: aNewVariable with: aSourceCodeToExtract in: aMethodNodeToRefactor

	|  trimmedNewVariable parseNodeToExtract trimmedSourceCodeToExtract methodNodeToExtract |
	
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	
	self assertIsNotEmpty: trimmedNewVariable.
	self assertIsValidVariableName: trimmedNewVariable.
	self assert: trimmedNewVariable isNotDefinedIn: aMethodNodeToRefactor.
	
	trimmedSourceCodeToExtract := aSourceCodeToExtract withBlanksTrimmed.
	self assertSourceCodeIsNotEmpty: trimmedSourceCodeToExtract.
	self assertIsNotReturn: trimmedSourceCodeToExtract. 
	self assert: trimmedSourceCodeToExtract isIncludedIn: aMethodNodeToRefactor.

	[ methodNodeToExtract := Parser parse: trimmedSourceCodeToExtract class: aMethodNodeToRefactor methodClass noPattern: true ]
		on: SyntaxErrorNotification 
		do: [ :anError | self signalColaborationToExtractHasSyntaxError: anError ].
	
	parseNodeToExtract := self paseNodeToExtractFrom: methodNodeToExtract. 

	self assert: trimmedNewVariable isNotDefinedAsInstanceVariableInHierarchyOf: aMethodNodeToRefactor methodClass.

	^self new initializeNamed: trimmedNewVariable with: parseNodeToExtract in: aMethodNodeToRefactor
 
	! !


!RenameTemporary class methodsFor: 'instance creation' stamp: 'HAW 3/7/2019 22:51:29'!
from: anOldVariable to: aNewVariable in: aMethodNode

	|  trimmedNewVariable |
	
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	
	self assert: anOldVariable isDefinedIn: aMethodNode.
	self assertIsNotEmpty: trimmedNewVariable.
	self assertIsValidVariableName: trimmedNewVariable.
	self assert: trimmedNewVariable isNotDefinedIn: aMethodNode.
	
	self assert: trimmedNewVariable isNotDefinedAsInstanceVariableInHierarchyOf: aMethodNode methodClass.
	
	^self new initializeFrom: anOldVariable to: trimmedNewVariable in: aMethodNode
! !


!NewInstanceVariablePrecondition methodsFor: 'evaluating' stamp: 'HAW 3/7/2019 22:28:38'!
value
	
	self assertIsNotEmpty.
	self assertIsNotAReservedName.
	self assertIsValidInstanceVariableName.
	self assertIsNotAlreadyDefined.

	self assertIsDefinedInMethods.! !

!methodRemoval: NewInstanceVariablePrecondition class #warningMessageForNewVariable:willBeHiddenAtAll:!
NewInstanceVariablePrecondition class removeSelector: #warningMessageForNewVariable:willBeHiddenAtAll:!
!methodRemoval: NewInstanceVariablePrecondition #warnIfIsDefinedInMethods!
NewInstanceVariablePrecondition removeSelector: #warnIfIsDefinedInMethods!
!methodRemoval: NewInstanceVariablePrecondition #warnNewVariable:willBeHiddenAtAll:!
NewInstanceVariablePrecondition removeSelector: #warnNewVariable:willBeHiddenAtAll:!
!methodRemoval: RenameTemporary class #errorMessageFor:canNotHideInstanceVariableDefinedIn:!
RenameTemporary class removeSelector: #errorMessageFor:canNotHideInstanceVariableDefinedIn:!
!methodRemoval: RenameTemporary class #warn:willHideInstanceVariableDefinedIn:!
RenameTemporary class removeSelector: #warn:willHideInstanceVariableDefinedIn:!
!methodRemoval: RenameTemporary class #warnIf:isDefinedAsInstanceVariableInHierarchyOf:!
RenameTemporary class removeSelector: #warnIf:isDefinedAsInstanceVariableInHierarchyOf:!
!methodRemoval: RenameTemporary class #warningMessageFor:willHideInstanceVariableDefinedIn:!
RenameTemporary class removeSelector: #warningMessageFor:willHideInstanceVariableDefinedIn:!
!methodRemoval: ExtractToTemporary class #errorMessageFor:canNotHideInstanceVariableDefinedIn:!
ExtractToTemporary class removeSelector: #errorMessageFor:canNotHideInstanceVariableDefinedIn:!
!methodRemoval: ExtractToTemporary class #warn:willHideInstanceVariableDefinedIn:!
ExtractToTemporary class removeSelector: #warn:willHideInstanceVariableDefinedIn:!
!methodRemoval: ExtractToTemporary class #warnIf:isDefinedAsInstanceVariableInHierarchyOf:!
ExtractToTemporary class removeSelector: #warnIf:isDefinedAsInstanceVariableInHierarchyOf:!
!methodRemoval: ExtractToTemporary class #warningMessageFor:willHideInstanceVariableDefinedIn:!
ExtractToTemporary class removeSelector: #warningMessageFor:willHideInstanceVariableDefinedIn:!

!ExtractToTemporary class reorganize!
('pre-conditions' assert:isIncludedIn: assert:isNotDefinedAsInstanceVariableInHierarchyOf: assert:isNotDefinedIn: assertIsNotEmpty: assertIsNotReturn: assertIsOneStatement: assertIsValidVariableName: assertSourceCodeIsNotEmpty:)
('exceptions' errorMessageForInvalidTemporaryVariable: errorMessageForNewTemporaryVariable:isAlreadyDefinedIn: errorMessageForSourceCodeToExtractHasSyntaxError: errorMessageMethodNodeToRefactorDoesNotInclude: newVariableCanNotBeEmptyErrorMessage signalColaborationToExtractHasSyntaxError: signalColaborationToExtractHasToBeOneStatement signalInvalidTemporaryVariable: signalMethodNodeToRefactorDoesNotInclude: signalNewTemporaryVariable:isAlreadyDefinedIn: signalNewVariableCanNotBeEmpty signalSourceCodeToExtractCanNotBeEmpty signalSourceCodeToExtractCanNotIncludeReturn sourceCodeToExtractCanNotBeEmptyErrorMessage sourceCodeToExtractCanNotIncludeReturnErrorMessage sourceCodeToExtractHasToBeOneStatementErrorMessage)
('instance creation' named:at:from:in: named:with:in:)
('method node to extract' paseNodeToExtractFrom:)
('warnings' errorMessageFor:canNotBeNamedAsInstanceVariableDefinedIn: signal:canNotHideInstanceVariableDefinedIn:)
!


!RenameTemporary class reorganize!
('pre-conditions' assert:isDefinedIn: assert:isNotDefinedAsInstanceVariableInHierarchyOf: assert:isNotDefinedIn: assertIsNotEmpty: assertIsValidVariableName:)
('exceptions' errorMessageForInvalidTemporaryVariable: errorMessageForNewTemporaryVariable:isAlreadyDefinedIn: errorMessageForTemporaryVariable:notDefinedIn: newVariableCanNotBeEmptyErrorMessage signalInvalidTemporaryVariable: signalNewTemporaryVariable:isAlreadyDefinedIn: signalNewVariableCanNotBeEmpty signalTemporaryVariable:notDefinedIn:)
('instance creation' from:to:in:)
('warnings' errorMessageFor:canNotBeNamedAsInstanceVariableDefinedIn: signal:canNotHideInstanceVariableDefinedIn:)
!


!NewInstanceVariablePrecondition reorganize!
('initialization' initializeOf:for:)
('evaluating' value)
('pre-conditions' assertIsNotAReservedName assertIsNotAlreadyDefined assertIsNotEmpty assertIsValidInstanceVariableName)
('exceptions' signalAlreadyDefinedInAll: signalInvalidInstanceVariable signalNewInstanceVariableCanNotBeAReservedName signalNewVariableCanNotBeEmpty)
('warnings' assertIsDefinedInMethods methodsDefiningNewVariable signalNewVariable:willBeHiddenAtAll:)
!

