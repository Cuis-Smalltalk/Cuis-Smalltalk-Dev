'From Cuis 6.0 [latest update: #6073] on 17 November 2023 at 10:34:51 am'!

!Editor methodsFor: 'new selection' stamp: 'jmv 11/17/2023 10:32:12'!
wordRangeIncluding: aPositionInText
	
	^self wordRangeLeftDelimiters: '' rightDelimiters: '' startingAt: aPositionInText ! !


!TextEditor methodsFor: 'model access' stamp: 'jmv 11/17/2023 10:34:11'!
wordAt: aPositionInText
	
	| wordUnderCursorRange |
	wordUnderCursorRange := self wordRangeIncluding: aPositionInText.
	^(model actualContents copyFrom: wordUnderCursorRange first to: wordUnderCursorRange last) asPlainString.! !


!Editor methodsFor: 'new selection' stamp: 'jmv 11/17/2023 10:29:34'!
wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters startingAt: pointIndex
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter finalDelimiter direction match level string here hereChar start stop sSize |
	string := self privateCurrentString.
	sSize := string size.
	sSize < 2 ifTrue: [^1 to: 1].
	here := pointIndex min: sSize max: 2.
	initialDelimiter := string at: here - 1.
	match := leftDelimiters indexOf: initialDelimiter.
	match > 0
		ifTrue: [
			"delimiter is on left -- match to the right"
			start := here.
			direction := 1.
			here := here - 1.
			finalDelimiter := rightDelimiters at: match]
		ifFalse: [
			initialDelimiter := string at: here.
			match := rightDelimiters indexOf: initialDelimiter.
			match > 0
				ifTrue: [
					"delimiter is on right -- match to the left"
					stop := here - 1.
					direction := -1.
					finalDelimiter := leftDelimiters at: match]
				ifFalse: [
					"no delimiters -- select a token"
					direction := -1]].
	level := 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < sSize]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar := string at: (here := here + direction).
			match = 0
				ifTrue: [
					"token scan goes left, then right"
					hereChar isValidInIdentifiers 	"Consider $: as a word separator"
						ifTrue: [
							here = 1
								ifTrue: [
									start := 1.
									"go right if hit string start"
									direction := 1]]
						ifFalse: [
							direction < 0
								ifTrue: [
									start := here + 1.
									"go right if hit non-token"
									direction := 1]
								ifFalse: [level := 0]]]
				ifFalse: [
					"delimiter match just counts nesting level"
					hereChar = finalDelimiter
						ifTrue: [level := level - 1"leaving nest"]
						ifFalse: [
							hereChar = initialDelimiter 
								ifTrue: [level := level + 1"entering deeper nest"]]]].
	level > 0 ifTrue: [
		leftDelimiters size + rightDelimiters size = 0 ifFalse: [
			"If we failed to find final delimiter, then just select word."
			^self wordRangeLeftDelimiters: '' rightDelimiters: '' startingAt: pointIndex ].
		here := here + direction ].
	^ direction > 0
		ifTrue: [
			"If a word ends with $: (a keyword), consider it part of the word"
			hereChar = $: ifTrue: [here := here + 1].
			start to: here - 1]
		ifFalse: [
			"But if a word starts with $: (this is the argument to a keyword), then it is not part of the word."
			here + 1 to: stop]! !


!TextEditor methodsFor: 'model access' stamp: 'jmv 11/17/2023 10:34:20'!
wordUnderCursor
	
	^self wordAt: self pointIndex! !


!TextComposition methodsFor: 'selection' stamp: 'jmv 11/17/2023 10:08:43'!
selectionStartBlock: startBlock selectionStopBlock: stopBlockBlock
	selectionStartBlock := startBlock.
	selectionStopBlock := stopBlockBlock.! !


!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'jmv 11/17/2023 10:34:16'!
on: anEditor for: aSelectionInterval of: aMethodToRefactor

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
		at: usageInterval of: aMethodToRefactor ! !

!methodRemoval: TextEditor #wordUnder: stamp: 'jmv 11/17/2023 10:34:22'!
TextEditor removeSelector: #wordUnder:!
!methodRemoval: Editor #wordRangeUnder: stamp: 'jmv 11/17/2023 10:33:44'!
Editor removeSelector: #wordRangeUnder:!
