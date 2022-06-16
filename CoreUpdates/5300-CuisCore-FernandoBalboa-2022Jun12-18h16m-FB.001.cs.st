'From Cuis 6.0 [latest update: #5235] on 12 June 2022 at 7:29:07 pm'!

!MethodNode methodsFor: 'accessing' stamp: 'FB 6/12/2022 18:56:52'!
statements

	^block statements.! !


!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'FB 6/12/2022 19:25:43'!
declareNecessaryTemporaries

	temporariesToDeclareByInsertionPoint keysAndValuesDo: [:aSenderAndPosition :temporariesToDeclare |
		| sender positionToInsertTemporaries temporariesString |
		sender := aSenderAndPosition key.
		positionToInsertTemporaries := aSenderAndPosition value.
		temporariesString := temporariesToDeclare reduce: [:t1 : t2 | t1 value, ' ', t2 value].
		((sender methodNode parseNodesPathAt: positionToInsertTemporaries ifAbsent: 	[self shouldNotHappen ]) first key isTemporariesDeclaration)
			ifTrue: [
				self replaceRange: (positionToInsertTemporaries to: positionToInsertTemporaries)
				withNewSourceCode: temporariesString, ' |'
				inMethod: sender.
			]
			ifFalse: [
				self replaceRange: (positionToInsertTemporaries to: positionToInsertTemporaries)
				withNewSourceCode: '| ',  temporariesString, ' |', Character newLineCharacter asString, Character tab asString
				inMethod: sender
			]
	]! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 6/12/2022 19:25:34'!
buildInlinedSourceCodeStatementsFrom: statementsToInline withParametersFrom: aMessageNodeReference removingReturn: removeReturn

	| inlinedStatements senderCompleteSourceRanges argumentsAndTemporariesReplacements indentation |

	senderCompleteSourceRanges := aMessageNodeReference methodNode completeSourceRanges.
	self calculateReplacementsFrom: aMessageNodeReference
		usingRanges: senderCompleteSourceRanges.

	self declareTemporariesIfNeededFor: aMessageNodeReference.
	argumentsAndTemporariesReplacements := self replacementsMapForMessageSend: aMessageNodeReference.

	indentation := self indentationUpTo: aMessageNodeReference completeSourceRange first in: aMessageNodeReference compiledMethod sourceCode.
	inlinedStatements := (statementsToInline collect: [:aStatementToInline |
		self sourceCodeOfImplementorStatement: aStatementToInline withReplacements: argumentsAndTemporariesReplacements removingReturn: removeReturn
			 ])
			reduce: [:aStatementWithReplacements :nextStatementWithReplacements |
				aStatementWithReplacements, Character newLineCharacter asString, indentation,
					(nextStatementWithReplacements copyReplaceAll: String newLineString with: String newLineString asTokens: false)].

	^inlinedStatements.! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 6/12/2022 18:33:45'!
indentationUpTo: anIndex in: aSourceCode

	| indentation currentIndex character|

	currentIndex := anIndex - 1.
	((aSourceCode at: currentIndex) = $^) ifTrue: [currentIndex := currentIndex  -1]. "Skip the return character if present"
	indentation := ''.

	[currentIndex > 0 and: [
		character := (aSourceCode at: currentIndex).
		(character = Character tab) or: [character = Character space]
	]] whileTrue: [
		indentation := indentation, character asString.
		currentIndex := currentIndex - 1.
	].

	^(character = Character newLineCharacter) ifFalse: [^''] ifTrue: [^indentation reversed].! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'FB 6/12/2022 18:49:29'!
inlineMessageSend: aMessageNodeReference

	| statementsToInline |

	statementsToInline := self removeReturnSelfStatementIfNeeded: (methodToInline methodNode block statements).
	statementsToInline isEmpty ifTrue: [^self].
	statementsToInline last isReturn
		ifTrue: [self inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeReference.]
		ifFalse: [self inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReference
			 removingReturn: true].


	! !


!InlineTemporaryVariableApplier class methodsFor: 'refactoring - creation' stamp: 'FB 6/12/2022 18:47:21'!
on: anEditor for: aSelectionInterval of: aMethodToRefactor

	| noTempFoundBlock tempToInline usageInterval |

	noTempFoundBlock := [
		"This case will usually end in a failure because a temporary variable is not related to the selected interval"
		tempToInline := anEditor wordUnder: aSelectionInterval first.
		usageInterval := aSelectionInterval.
	].

	anEditor
		withNodeUnderCursorDo: [:aNode | | methodNode sourceRanges |
			methodNode :=aMethodToRefactor methodNode.
			sourceRanges := methodNode completeSourceRanges.
			aNode isVariableNode
				ifTrue: [
					tempToInline := aNode name.
					usageInterval := self findLeftmostSourceRangeOfNode: aNode inside: aSelectionInterval using: sourceRanges.
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
