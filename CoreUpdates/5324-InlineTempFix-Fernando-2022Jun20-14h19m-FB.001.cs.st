'From Cuis 6.0 [latest update: #5301] on 20 June 2022 at 2:35:31 pm'!

!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 6/20/2022 14:23:01'!
findSourceRangeOfNode: aParseNode containingIndex: anIndex using: completeSourceRanges

	^(self findSourceRangesOfNode: aParseNode using: completeSourceRanges) 
		detect: [:nodeRange | nodeRange includes: anIndex].! !

!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 6/20/2022 14:21:26'!
findSourceRangesOfNode: aParseNode using: completeSourceRanges

	^(completeSourceRanges associations detect: [:aNodeAndRange | aNodeAndRange key equivalentTo: aParseNode]) value! !


!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 6/20/2022 14:21:51'!
findLeftmostSourceRangeOfNode: aParseNode inside: anInterval using: completeSourceRanges

	^(self findSourceRangesOfNode: aParseNode using: completeSourceRanges) 
		detect: [:nodeRange | anInterval includesAllOf: nodeRange].! !

!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 6/20/2022 14:34:44'!
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
					assignmentNodeAndRange := (methodNode parseNodesPathAt: aSelectionInterval first ifAbsent: 	[self shouldNotHappen.])
						detect: [:aNodeAndRange | aNodeAndRange key isAssignmentNode]
						ifNone: [noTempFoundBlock value].
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

