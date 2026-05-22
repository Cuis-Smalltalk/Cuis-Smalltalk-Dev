Class {
	#name : 'InlineTemporaryVariableApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'codeProvider',
		'temporaryToInline',
		'usageInterval',
		'methodToRefactor'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'refactoring - creation' }
InlineTemporaryVariableApplier class >> createAndValueHandlingExceptions: creationBlock [


	[super createAndValueHandlingExceptions: creationBlock]
		on: Refactoring refactoringErrorClass
		do: [ :refactoringError | ^self inform: refactoringError messageText ].

]

{ #category : 'refactoring - creation' }
InlineTemporaryVariableApplier class >> findLeftmostSourceRangeOfNode: aParseNode inside: anInterval using: completeSourceRanges [

	^(self findSourceRangesOfNode: aParseNode using: completeSourceRanges) 
		detect: [:nodeRange | anInterval includesAllOf: nodeRange].
]

{ #category : 'refactoring - creation' }
InlineTemporaryVariableApplier class >> findSourceRangeOfNode: aParseNode containingIndex: anIndex using: completeSourceRanges [

	^(self findSourceRangesOfNode: aParseNode using: completeSourceRanges) 
		detect: [:nodeRange | nodeRange includes: anIndex].
]

{ #category : 'refactoring - creation' }
InlineTemporaryVariableApplier class >> findSourceRangesOfNode: aParseNode using: completeSourceRanges [

	| ranges |
	
	ranges := OrderedCollection new.
	
	(completeSourceRanges associations select: [:aNodeAndRange | aNodeAndRange key equivalentTo: aParseNode])
	 	do: [:nodeAndRanges | ranges addAll: nodeAndRanges value].
	
	^ranges.
]

{ #category : 'refactoring - creation' }
InlineTemporaryVariableApplier class >> on: anEditor for: aSelectionInterval of: aMethodToRefactor [

	| noTempFoundBlock tempToInline usageInterval |

	noTempFoundBlock := [
		"This case will usually end in a failure because a temporary variable is not related to the selected interval"
		tempToInline := anEditor wordAt: aSelectionInterval first.
		usageInterval := aSelectionInterval.
	].

	anEditor
		withNodeUnderCursorDo: [:aNode | | methodNode sourceRanges |
			methodNode := aMethodToRefactor notOptimizedMethodNode.
			sourceRanges := methodNode completeSourceRanges.
			(aNode isVariableNode and: [methodNode argumentNames noneSatisfy: [:argument | argument = aNode name]])
				ifTrue: [
					tempToInline := aNode name.
					usageInterval := self findSourceRangeOfNode: aNode containingIndex: anEditor startIndex using: sourceRanges.
				]
				ifFalse: [ | assignmentNodeAndRange |
					assignmentNodeAndRange := self searchAssignmentInStatementContaining: aSelectionInterval first
						in: methodNode.
					assignmentNodeAndRange 	ifNil: [noTempFoundBlock value].
					tempToInline ifNil: [
						tempToInline := assignmentNodeAndRange key variable name.
						usageInterval := self findLeftmostSourceRangeOfNode: assignmentNodeAndRange key variable
							inside: assignmentNodeAndRange value using: sourceRanges.
					]
				]
		]
		ifAbsent: [
			noTempFoundBlock value.
		].

	^self new initializeOn: anEditor codeProvider forTemporary: tempToInline
		at: usageInterval of: aMethodToRefactor 
]

{ #category : 'refactoring - creation' }
InlineTemporaryVariableApplier class >> refactoringClass [

	^ InlineTemporaryVariable 
]

{ #category : 'refactoring - creation' }
InlineTemporaryVariableApplier class >> searchAssignmentInStatementContaining: anIndex in: aMethodNode [
	
	"Determine if anIndex is contained in a statement that is an assignment.
	 If a block node is found before and assignment node, we are sure that
	this is not the case - Fernando"
	
	(aMethodNode parseNodesPathAt: anIndex ifAbsent: [self shouldNotHappen ]) do: [:aNodeAndRange | 
		aNodeAndRange key isBlockNode ifTrue: [^nil].
		aNodeAndRange key isAssignmentNode ifTrue: [^aNodeAndRange ]
	].

	^nil.
]

{ #category : 'refactoring - creation' }
InlineTemporaryVariableApplier >> createRefactoring [

	^ self refactoringClass
		named: temporaryToInline
		atUsageInterval: usageInterval
		inMethod: methodToRefactor.
]

{ #category : 'initialization' }
InlineTemporaryVariableApplier >> initializeOn: aCodeProvider forTemporary: aTemporaryToInline at: aUsageInterval of: aMethodToRefactor [

	codeProvider := aCodeProvider.
	temporaryToInline := aTemporaryToInline.
	usageInterval  := aUsageInterval.
	methodToRefactor := aMethodToRefactor.
]

{ #category : 'refactoring - creation' }
InlineTemporaryVariableApplier >> refactoringClass [

	^ InlineTemporaryVariable 
]

{ #category : 'refactoring - changes' }
InlineTemporaryVariableApplier >> requestRefactoringParameters [

]

{ #category : 'refactoring - changes' }
InlineTemporaryVariableApplier >> showChanges [

	codeProvider currentMethodRefactored
]
