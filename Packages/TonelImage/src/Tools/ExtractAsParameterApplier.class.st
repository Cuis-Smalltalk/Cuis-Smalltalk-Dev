Class {
	#name : 'ExtractAsParameterApplier',
	#superclass : 'AddParameterApplier',
	#instVars : [
		'interval',
		'intervals'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ExtractAsParameterApplier class >> from: anInterval on: aModel for: anOldSelector in: aClassToRefactor  [

	ExtractAsParameter assertCanExtractedFromAll: { anInterval } for: aClassToRefactor >> anOldSelector.
	
	^(super on: aModel for: anOldSelector in: aClassToRefactor) initializeInterval: anInterval 
]

{ #category : 'refactoring - parameters request' }
ExtractAsParameterApplier >> askNewParameterValue [

]

{ #category : 'refactoring - parameters request' }
ExtractAsParameterApplier >> askToReplaceAll [
	
	| extractAll |
	
	extractAll := PopUpMenu 
		confirm: 'Do you want to extract all occurrences?' 
		trueChoice: 'Yes, extract all occurrences' 
		falseChoice: 'No, extract only the selected one'.
		
	extractAll ifFalse: [ intervals := { interval } ].
]

{ #category : 'refactoring - creation' }
ExtractAsParameterApplier >> createRefactoringForKeywordSelector [
	
	^self refactoringClass 
		named: newParameter 
		extractedFromAll:  intervals
		at:  parameterIndex 
		newKeyword:  newKeyword 
		at: selectedClass >> oldSelector 
		implementors: implementors 
		senders: senders 
]

{ #category : 'refactoring - creation' }
ExtractAsParameterApplier >> createRefactoringForUnarySelector [
	
	^self refactoringClass 
		named: newParameter 
		extractedFromAll: intervals
		at: selectedClass >> oldSelector 
		implementors: implementors 
		senders: senders 
]

{ #category : 'initialization' }
ExtractAsParameterApplier >> initializeInterval: anInterval  [
	
	interval := anInterval.
	
	self initializeIntervals.
]

{ #category : 'initialization' }
ExtractAsParameterApplier >> initializeIntervals [
	
	intervals := self refactoringClass intervalsForEquivalentExpressionIn: selectedClass >> oldSelector at: interval.
]

{ #category : 'refactoring - creation' }
ExtractAsParameterApplier >> refactoringClass [

	^ExtractAsParameter 
]

{ #category : 'refactoring - parameters request' }
ExtractAsParameterApplier >> requestRefactoringParameters [

	intervals size > 1 ifTrue: [ self askToReplaceAll ].
	
	super requestRefactoringParameters.
]
