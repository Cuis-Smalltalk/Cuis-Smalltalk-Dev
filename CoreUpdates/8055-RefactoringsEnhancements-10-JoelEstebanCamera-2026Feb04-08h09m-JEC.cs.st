'From Cuis7.9 [latest update: #8055] on 4 July 2026 at 5:19:29 pm'!
!classDefinition: #EquivalentNodesFinder category: #'Compiler-Support'!
NodesFinder subclass: #EquivalentNodesFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!EquivalentNodesFinder reorganize!
('private - visiting' addAllToFoundNodes: addEquivalentNodesInCascadeNode: addEquivalentNodesOf: addToEquivalentsIfOneStatementEquivalentTo: addToEquivalentsNodesPartiallyOf: areNodesToLookForAPartiallyCascadeNode: hasOneNodeToLookForAndEquivalentTo: selectionToKeepIsEqualOrHasNoIntersectionWith: shouldKeepSelection)
('ranges' equivalentNodesRanges)
('visiting - finding nodes' addSingleAssigmentNodeIfNecessary: addSingleBacktickNodeIfNecessary: addSingleBlockNodeIfNeccesary: addSingleBraceNodeIfNecessary: addSingleCascadeNodeIfNecessary: addSingleInstanceVariableNodeIfNecessary: addSingleLiteralNodeIfNeccesary: addSingleLiteralVariableNodeIfNecessary: addSingleMessageNodeIfNeccesary: addSingleReturnNodeIfNeccesary: addSingleTempVariableNodeIfNecessary: nodesToLookForMatchingConditionWith:)
!

!classDefinition: #ExtractMethodNewMethod category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodNewMethod
	instanceVariableNames: 'categoryOfNewSelector existingMethod newSourceCodeGenerator newSourceCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
