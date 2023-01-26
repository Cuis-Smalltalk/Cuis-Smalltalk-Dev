'From Cuis 6.0 [latest update: #5655] on 26 January 2023 at 11:58:33 am'!

!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 8/25/2022 21:10:01'!
searchAssignmentInStatementContaining: anIndex in: aMethodNode
	
	"Determine if anIndex is contained in a statement that is an assignment.
	 If a block node is found before and assignment node, we are sure that
	this is not the case - Fernando"
	
	(aMethodNode parseNodesPathAt: anIndex ifAbsent: [self shouldNotHappen ]) do: [:aNodeAndRange | 
		aNodeAndRange key isBlockNode ifTrue: [^nil].
		aNodeAndRange key isAssignmentNode ifTrue: [^aNodeAndRange ]
	].

	^nil.! !


!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 8/25/2022 21:31:59'!
findSourceRangesOfNode: aParseNode using: completeSourceRanges

	| ranges |
	
	ranges := OrderedCollection new.
	
	(completeSourceRanges associations select: [:aNodeAndRange | aNodeAndRange key equivalentTo: aParseNode])
	 	do: [:nodeAndRanges | ranges addAll: nodeAndRanges value].
	
	^ranges.! !

!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 8/25/2022 21:28:19'!
on: anEditor for: aSelectionInterval of: aMethodToRefactor

	| noTempFoundBlock tempToInline usageInterval |

	noTempFoundBlock := [
		"This case will usually end in a failure because a temporary variable is not related to the selected interval"
		tempToInline := anEditor wordUnder: aSelectionInterval first.
		usageInterval := aSelectionInterval.
	].
	
	anEditor
		withNodeUnderCursorDo: [:aNode | | methodNode sourceRanges |
			methodNode := aMethodToRefactor methodNode.
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
		at: usageInterval of: aMethodToRefactor ! !

