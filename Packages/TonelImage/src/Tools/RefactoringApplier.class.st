Class {
	#name : 'RefactoringApplier',
	#superclass : 'Object',
	#instVars : [
		'requestExitBlock',
		'refactoring',
		'changes'
	],
	#classVars : [
		'Appliers'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'appliers - getting' }
RefactoringApplier class >> addParameterApplier [
	
	^self applierAt: self addParameterApplierId ifAbsent: [ AddParameterApplier ]
]

{ #category : 'appliers - id' }
RefactoringApplier class >> addParameterApplierId [

	^#addParameterApplier
]

{ #category : 'appliers - private' }
RefactoringApplier class >> applierAt: anApplierId ifAbsent: absentBlock [

	^Appliers 
		at: anApplierId 
		ifPresent: [ :anApplierName | Smalltalk classNamed: anApplierName ]
		ifAbsent: absentBlock 
]

{ #category : 'value handling exceptions' }
RefactoringApplier class >> createAndValueHandlingExceptions: creationBlock [

	| applier |

	applier := creationBlock
		on: Refactoring refactoringErrorClass
		do: [ :refactoringError | ^self inform: refactoringError messageText ].

	applier value 
]

{ #category : 'common buttons' }
RefactoringApplier class >> createCancelButtonOn: aModel [
	
	^PluggableButtonMorph 
		model: aModel
		stateGetter: nil
		action: #delete
		label: 'Cancel'
		balloonText: 'Do not apply the refactoring'

]

{ #category : 'common buttons' }
RefactoringApplier class >> createJustRefactorButtonOn: aModel [

	^PluggableButtonMorph 
		model: aModel
		stateGetter: nil
		action: #justRefactor
		label: 'Refactor'
		balloonText: 'Applies the refactoring without showing the changes'
]

{ #category : 'common buttons' }
RefactoringApplier class >> createRefactorButtonOn: aModel [

	^PluggableButtonMorph 
		model: aModel
		stateGetter: nil
		action: #refactor
		label: 'Refactor...'
		balloonText: 'Applies the refactoring and shows the changes'
]

{ #category : 'common buttons' }
RefactoringApplier class >> createRemoveButtonOn: aModel [

	^PluggableButtonMorph
		model: aModel
		stateGetter: nil
		action: #remove
		label: 'Remove'
		balloonText: 'Removes the selected element from the refactoring'

]

{ #category : 'common buttons' }
RefactoringApplier class >> createSeeImplementorsButtonOn: aModel [

	^PluggableButtonMorph
		model: aModel
		stateGetter: nil
		action: #seeImplementors
		label: 'See Implementors'
		balloonText: 'Shows the implementors that will be affected by the refactoring'

]

{ #category : 'appliers - getting' }
RefactoringApplier class >> extractAsParameterApplier [
	
	^self applierAt: self extractAsParameterApplierId ifAbsent: [ ExtractAsParameterApplier ]
]

{ #category : 'appliers - id' }
RefactoringApplier class >> extractAsParameterApplierId [

	^#extractAsParameterApplier
]

{ #category : 'initialization' }
RefactoringApplier class >> initialize [

	Appliers := IdentityDictionary new.
]

{ #category : 'appliers - getting' }
RefactoringApplier class >> inlineMethodApplier [

	^self applierAt: self inlineMethodApplierId ifAbsent: [ InlineMethodApplier ]
]

{ #category : 'appliers - id' }
RefactoringApplier class >> inlineMethodApplierId [

	^#inlineMethodApplier
]

{ #category : 'appliers - registering' }
RefactoringApplier class >> registerAddParameterApplier: anAddParameterApplierClass [

	self registerApplierAt: self addParameterApplierId with: anAddParameterApplierClass 
]

{ #category : 'appliers - private' }
RefactoringApplier class >> registerApplierAt: anApplierId with: anApplierClass [

	Appliers at: anApplierId put: anApplierClass name 
]

{ #category : 'appliers - registering' }
RefactoringApplier class >> registerExtractAsParameterApplier: anExtractAsParameterApplierClass [

	self registerApplierAt: self extractAsParameterApplierId with: anExtractAsParameterApplierClass 
]

{ #category : 'appliers - registering' }
RefactoringApplier class >> registerInlineMethodApplier: anInlineMethodApplierClass [

	self registerApplierAt: self inlineMethodApplierId with: anInlineMethodApplierClass 
]

{ #category : 'appliers - registering' }
RefactoringApplier class >> registerRemoveParameterApplier: aRemoveParameterApplierClass [

	self registerApplierAt: self removeParameterApplierId with: aRemoveParameterApplierClass 
]

{ #category : 'appliers - registering' }
RefactoringApplier class >> registerRenameSelectorApplier: aRenameSelectorApplierClass [

	self registerApplierAt: self renameSelectorApplierId  with: aRenameSelectorApplierClass 
]

{ #category : 'appliers - getting' }
RefactoringApplier class >> removeParameterApplier [
	
	^self applierAt: self removeParameterApplierId ifAbsent: [ RemoveParameterApplier ]
]

{ #category : 'appliers - id' }
RefactoringApplier class >> removeParameterApplierId [

	^#removeParameterApplier
]

{ #category : 'appliers - getting' }
RefactoringApplier class >> renameSelectorApplier [
	
	^self applierAt: self renameSelectorApplierId ifAbsent: [ RenameSelectorApplier ]
]

{ #category : 'appliers - id' }
RefactoringApplier class >> renameSelectorApplierId [

	^#renameSelectorApplier
]

{ #category : 'appliers - resetting' }
RefactoringApplier class >> resetAddParameterApplier [
	
	self resetApplierAt: self addParameterApplierId
]

{ #category : 'appliers - private' }
RefactoringApplier class >> resetApplierAt: anApplierId [

	Appliers removeKey: anApplierId ifAbsent: nil
]

{ #category : 'appliers - resetting' }
RefactoringApplier class >> resetExtractAsParameterApplier [
	
	self resetApplierAt: self extractAsParameterApplierId
]

{ #category : 'appliers - resetting' }
RefactoringApplier class >> resetInlineMethodApplier [

	self resetApplierAt: self inlineMethodApplierId 
]

{ #category : 'appliers - resetting' }
RefactoringApplier class >> resetRemoveParameterApplier [
	
	self resetApplierAt: self removeParameterApplierId 
]

{ #category : 'appliers - resetting' }
RefactoringApplier class >> resetRenameSelectorApplier [
	
	self resetApplierAt: self renameSelectorApplierId 
]

{ #category : 'refactoring - applying' }
RefactoringApplier >> applyRefactoring [

	changes := refactoring apply
]

{ #category : 'exception handling' }
RefactoringApplier >> browseReferencesOn: aCanNotRefactorDueToReferencesError [
	
	Smalltalk 
		browseMessageList: aCanNotRefactorDueToReferencesError references 
		name: (self referencesBrowserTitleOn: aCanNotRefactorDueToReferencesError)
		autoHighlight: aCanNotRefactorDueToReferencesError referencee asString
		allOccurrences: false
]

{ #category : 'refactoring - creation' }
RefactoringApplier >> createAndSetRefactoringHandlingRefactoringExceptions: aCreatorBlock [

	self valueHandlingRefactoringExceptions: [ refactoring := aCreatorBlock value]
	
]

{ #category : 'refactoring - creation' }
RefactoringApplier >> createRefactoring [

	self subclassResponsibility 
]

{ #category : 'refactoring - creation' }
RefactoringApplier >> createRefactoringHandlingRefactoringExceptions [

	self createAndSetRefactoringHandlingRefactoringExceptions: [ self createRefactoring ]
	
]

{ #category : 'refactoring - parameters request' }
RefactoringApplier >> endRequest [

	^requestExitBlock value
]

{ #category : 'exception handling' }
RefactoringApplier >> handleCanNotRefactorDueToReferencesError: aCanNotRefactorDueToReferencesError [
	
	| options answer question |
	
	options := 
'Browse references
Cancel'.

	question := PopUpMenu labels: options icons: #(mailForwardIcon cancelIcon).
	answer := question startUpWithCaption: aCanNotRefactorDueToReferencesError messageText.
	
	answer = 1 ifTrue: [ self browseReferencesOn: aCanNotRefactorDueToReferencesError ].
	self endRequest.
]

{ #category : 'exception handling' }
RefactoringApplier >> handleRefactoringError: aRefactoringError  [

	self inform: aRefactoringError messageText.
	self endRequest 
]

{ #category : 'exception handling' }
RefactoringApplier >> handleRefactoringWarning: aRefactoringWarning [
	
	(self confirm: aRefactoringWarning messageText, '. Continue?')
		ifTrue: [ aRefactoringWarning resume ]
		ifFalse: [ self endRequest]
]

{ #category : 'exception handling' }
RefactoringApplier >> handleReferencesWarning: aReferencesWarning [
	
	| options answer question |
	
	options := 
'Browse references and Cancel
Browse references and Continue
Continue'.

	question := PopUpMenu labels: options icons: #(cancelIcon mailForwardIcon acceptIcon).
	answer := question startUpWithCaption: aReferencesWarning messageText.
	
	answer <= 2 ifTrue: [ self browseReferencesOn: aReferencesWarning ].
	answer = 1 ifTrue: [ self endRequest ].
	aReferencesWarning resume.
]

{ #category : 'exception handling' }
RefactoringApplier >> referencesBrowserTitleOn: aCanNotRefactorDueToReferencesError [

	^'References to ', aCanNotRefactorDueToReferencesError referencee asString
]

{ #category : 'request information' }
RefactoringApplier >> request: aLabel [
 
	^self request: aLabel initialAnswer: ''

]

{ #category : 'request information' }
RefactoringApplier >> request: aLabel initialAnswer: anAnswer [
	^self request: aLabel initialAnswer: anAnswer orCancel: requestExitBlock 
]

{ #category : 'refactoring - parameters request' }
RefactoringApplier >> requestRefactoringParameters [

	self subclassResponsibility 
]

{ #category : 'refactoring - parameters request' }
RefactoringApplier >> requestRefactoringParametersHandlingRefactoringExceptions [

	self valueHandlingRefactoringExceptions: [ self requestRefactoringParameters ]
	
]

{ #category : 'refactoring - changes' }
RefactoringApplier >> showChanges [

	self subclassResponsibility 	
	
]

{ #category : 'evaluating' }
RefactoringApplier >> value [

	requestExitBlock := [ ^self ].
	
	self 
		requestRefactoringParametersHandlingRefactoringExceptions;
		createRefactoringHandlingRefactoringExceptions;
		applyRefactoring;
		showChanges
	
	
]

{ #category : 'exception handling' }
RefactoringApplier >> valueHandlingRefactoringExceptions: aBlock [

	^[[[aBlock
		on: Refactoring referencesWarningClass
		do: [ :aReferencesRefactoringWarning | self handleReferencesWarning: aReferencesRefactoringWarning ]]
		on: Refactoring refactoringWarningClass 
		do: [ :aRefactoringWarning | self handleRefactoringWarning: aRefactoringWarning ]]
		on: Refactoring canNotRefactorDueToReferencesErrorClass
		do: [ :aCanNotRefactorDueToReferencesError | self handleCanNotRefactorDueToReferencesError: aCanNotRefactorDueToReferencesError ]]
		on: Refactoring refactoringErrorClass 
		do: [ :aRefactoringError | self handleRefactoringError: aRefactoringError ]
	
]
