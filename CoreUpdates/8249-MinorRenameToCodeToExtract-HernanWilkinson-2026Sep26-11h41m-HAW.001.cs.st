'From Cuis7.9 [latest update: #8184] on 28 September 2026 at 1:27:40 pm'!

!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'HAW 26/Sep/2026, 2:46:51 pm (UTC)'!
anyParameterReadsATemporaryAssignedInCodeToExtract

	"Such a parameter would be evaluated by the sender before the extracted method assigns the temporary"

	| assignedNames |

	assignedNames := self namesOfTemporariesAssignedInCodeToExtract.

	^parameterizableNodes anySatisfy: [ :parameterizableNode |
		self does: parameterizableNode key readAnyOf: assignedNames ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'HAW 26/Sep/2026, 2:46:51 pm (UTC)'!
namesOfTemporariesAssignedInCodeToExtract

	| assignedNames |

	assignedNames := Set new.
	nodes do: [ :aNode |
		aNode nodesDo: [ :eachNode |
			(eachNode isAssignmentNode and: [ eachNode variable isTemp ])
				ifTrue: [ assignedNames add: eachNode variable name ]]].

	^assignedNames! !


!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'HAW 26/Sep/2026, 2:46:50 pm (UTC)'!
canAddNewParamReplacement: newParamReplacement 

	^ (self intersectsWithAnyParamReplacement: newParamReplacement) not
		and: [ newParamReplacement anyParameterContainsReturnOrAssignment not
		and: [ newParamReplacement anyParameterReadsATemporaryAssignedInCodeToExtract not ]]! !

!methodRemoval: ExtractMethodParameterizableReplacement #anyParameterReadsATemporaryAssignedInThePiece stamp: 'HAW 26/Sep/2026, 2:46:51 pm (UTC)'!
ExtractMethodParameterizableReplacement removeSelector: #anyParameterReadsATemporaryAssignedInThePiece!
!methodRemoval: ExtractMethodParameterizableReplacement #namesOfTemporariesAssignedInThePiece stamp: 'HAW 26/Sep/2026, 2:46:51 pm (UTC)'!
ExtractMethodParameterizableReplacement removeSelector: #namesOfTemporariesAssignedInThePiece!

!ExtractMethodParameterizableReplacement reorganize!
('printing' sourceCode)
('testing' anyParameterContainsReturnOrAssignment anyParameterReadsATemporaryAssignedInCodeToExtract canBeUsedForNaming hasIntersectionWith: hasParameterizableIntervalsInOrder: hasSameIntervalWith: intersectsIntervalWith: isFromSameMethodWith: isIntervalToExtract: isOf: isParameterizableParseNode: parameterNodeIncludes: representSamePieceOfCodeWith:)
('private' allArgumentNodesOf: differencesWith: does:readAnyOf: hasAParamReceiver:ifTrue:ifFalse: isKeywordMessageSendNode: messageNodeOf: namesOfTemporariesAssignedInCodeToExtract receiverNodeOf: shouldAddArgumentsOf: sortBlockByRange)
('accessing' allArgumentsToParameterizeInOrderUsing: intervalToExtract methodToExtractFrom nodes nodesWithArguments paramReceivers parameterizableNodes parameterizableNodesIntervals parameterizableNodesSortedByInterval parameterizableNodesWithInterval)
('initialization' initializeFor:interval:parameterizableNodes:in: initializeNodesWithArguments initializeParamReceivers)
('converting' createWithSameParametersAs:)
!


!ExtractMethodReplacementsFinder reorganize!
('initialization' initializeOfCodeIn:at:to:)
('private-replacement finding' addReplacementAt:in:with:paramNodes: createReplacementAt:in: createReplacementAt:in:with:paramNodes: find findReplacementsAt: replacementsIntervalsOf:)
('replacements' methodsToReplace newMessageForReplacements: originalSelectionReplacement useReplacements:)
('evaluating')
('private-parameterizable code finding' acceptParamReplacementsWithNodesToParameterized:to: addNewParameterizable:in: addParameterizableFromReplacement: addReplacementsAsParameterizables argumentsByReplacementUsing: assertArgumentsAreValid: canAddNewParamReplacement: distinctAmong: findParameterizableReplacementsNodesIn: intersectsWithAnyParamReplacement: makeParameterizableReplacementsHaveTheSameParameters parameterSharingAmong: signalCannotApplyTheRefactoringDueToDuplicateTempVar useParameterizedToChangeReplacements)
('parameterizable code' distinctParametersOf:using: extractedCodeOrAnyParameterizableReplacement parameterSharing parameterizableReplacements parameterizableReplacementsAt: removeParameterizableReplacementAt:)
('testing' anyParameterizableNodeIncludes:at:in: hasFoundParameterizableReplacements)
('private' initializeFinding)
!

