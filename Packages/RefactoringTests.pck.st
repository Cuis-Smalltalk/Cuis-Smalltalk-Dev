'From Cuis7.9 [latest update: #8166] on 11 September 2026 at 12:14:33 pm'!
'Description '!
!provides: 'RefactoringTests' 1 7!
!requires: 'DynamicallyCodeCreatingTestSupport' 1 0 nil!
!requires: '__Refactoring_TestData__' 1 0 nil!
SystemOrganization addCategory: #RefactoringTests!


!classDefinition: #CommonScopeNodeFinderTest category: #RefactoringTests!
DynamicallyCodeCreatingTest subclass: #CommonScopeNodeFinderTest
	instanceVariableNames: 'classToRefactor methodNode finder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'CommonScopeNodeFinderTest class' category: #RefactoringTests!
CommonScopeNodeFinderTest class
	instanceVariableNames: ''!

!classDefinition: #ExtractMethodFinderTest category: #RefactoringTests!
DynamicallyCodeCreatingTest subclass: #ExtractMethodFinderTest
	instanceVariableNames: 'classToRefactor sourceMethod finder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'ExtractMethodFinderTest class' category: #RefactoringTests!
ExtractMethodFinderTest class
	instanceVariableNames: ''!

!classDefinition: #ExtractToTemporaryFinderTest category: #RefactoringTests!
DynamicallyCodeCreatingTest subclass: #ExtractToTemporaryFinderTest
	instanceVariableNames: 'classToRefactor sourceMethod finder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'ExtractToTemporaryFinderTest class' category: #RefactoringTests!
ExtractToTemporaryFinderTest class
	instanceVariableNames: ''!

!classDefinition: #ParameterizableNodesFinderTest category: #RefactoringTests!
DynamicallyCodeCreatingTest subclass: #ParameterizableNodesFinderTest
	instanceVariableNames: 'finder classToRefactor methodNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'ParameterizableNodesFinderTest class' category: #RefactoringTests!
ParameterizableNodesFinderTest class
	instanceVariableNames: ''!

!classDefinition: #RefactoringTest category: #RefactoringTests!
DynamicallyCodeCreatingTest subclass: #RefactoringTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'RefactoringTest class' category: #RefactoringTests!
RefactoringTest class
	instanceVariableNames: ''!

!classDefinition: #AddAsSubclassResponsabilityTest category: #RefactoringTests!
RefactoringTest subclass: #AddAsSubclassResponsabilityTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'AddAsSubclassResponsabilityTest class' category: #RefactoringTests!
AddAsSubclassResponsabilityTest class
	instanceVariableNames: ''!

!classDefinition: #AddInstanceVariableTest category: #RefactoringTests!
RefactoringTest subclass: #AddInstanceVariableTest
	instanceVariableNames: 'newVariable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'AddInstanceVariableTest class' category: #RefactoringTests!
AddInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #AddParameterTest category: #RefactoringTests!
RefactoringTest subclass: #AddParameterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'AddParameterTest class' category: #RefactoringTests!
AddParameterTest class
	instanceVariableNames: ''!

!classDefinition: #ChangeKeywordsSelectorOrderTest category: #RefactoringTests!
RefactoringTest subclass: #ChangeKeywordsSelectorOrderTest
	instanceVariableNames: 'classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'ChangeKeywordsSelectorOrderTest class' category: #RefactoringTests!
ChangeKeywordsSelectorOrderTest class
	instanceVariableNames: ''!

!classDefinition: #ExtractAsParameterTest category: #RefactoringTests!
RefactoringTest subclass: #ExtractAsParameterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'ExtractAsParameterTest class' category: #RefactoringTests!
ExtractAsParameterTest class
	instanceVariableNames: ''!

!classDefinition: #ExtractMethodApplierTest category: #RefactoringTests!
RefactoringTest subclass: #ExtractMethodApplierTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'ExtractMethodApplierTest class' category: #RefactoringTests!
ExtractMethodApplierTest class
	instanceVariableNames: ''!

!classDefinition: #ExtractMethodTest category: #RefactoringTests!
RefactoringTest subclass: #ExtractMethodTest
	instanceVariableNames: 'classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'ExtractMethodTest class' category: #RefactoringTests!
ExtractMethodTest class
	instanceVariableNames: ''!

!classDefinition: #ExtractToTemporaryTest category: #RefactoringTests!
RefactoringTest subclass: #ExtractToTemporaryTest
	instanceVariableNames: 'classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'ExtractToTemporaryTest class' category: #RefactoringTests!
ExtractToTemporaryTest class
	instanceVariableNames: ''!

!classDefinition: #InlineMethodTest category: #RefactoringTests!
RefactoringTest subclass: #InlineMethodTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'InlineMethodTest class' category: #RefactoringTests!
InlineMethodTest class
	instanceVariableNames: ''!

!classDefinition: #InlineTemporaryVariableTest category: #RefactoringTests!
RefactoringTest subclass: #InlineTemporaryVariableTest
	instanceVariableNames: 'newVariable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'InlineTemporaryVariableTest class' category: #RefactoringTests!
InlineTemporaryVariableTest class
	instanceVariableNames: ''!

!classDefinition: #InsertSuperclassTest category: #RefactoringTests!
RefactoringTest subclass: #InsertSuperclassTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'InsertSuperclassTest class' category: #RefactoringTests!
InsertSuperclassTest class
	instanceVariableNames: ''!

!classDefinition: #MoveToInstanceOrClassMethodTest category: #RefactoringTests!
RefactoringTest subclass: #MoveToInstanceOrClassMethodTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'MoveToInstanceOrClassMethodTest class' category: #RefactoringTests!
MoveToInstanceOrClassMethodTest class
	instanceVariableNames: ''!

!classDefinition: #PushDownInstanceVariableTest category: #RefactoringTests!
RefactoringTest subclass: #PushDownInstanceVariableTest
	instanceVariableNames: 'classToRefactor instanceVariableToPushDown'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'PushDownInstanceVariableTest class' category: #RefactoringTests!
PushDownInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #PushDownMethodTest category: #RefactoringTests!
RefactoringTest subclass: #PushDownMethodTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'PushDownMethodTest class' category: #RefactoringTests!
PushDownMethodTest class
	instanceVariableNames: ''!

!classDefinition: #PushDownMethodToOneSubclassTest category: #RefactoringTests!
PushDownMethodTest subclass: #PushDownMethodToOneSubclassTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'PushDownMethodToOneSubclassTest class' category: #RefactoringTests!
PushDownMethodToOneSubclassTest class
	instanceVariableNames: ''!

!classDefinition: #PushDownMethodToSubclassesTest category: #RefactoringTests!
PushDownMethodTest subclass: #PushDownMethodToSubclassesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'PushDownMethodToSubclassesTest class' category: #RefactoringTests!
PushDownMethodToSubclassesTest class
	instanceVariableNames: ''!

!classDefinition: #PushUpInstanceVariableTest category: #RefactoringTests!
RefactoringTest subclass: #PushUpInstanceVariableTest
	instanceVariableNames: 'classToRefactor instanceVariableToPushUp classToRefactorSuperClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'PushUpInstanceVariableTest class' category: #RefactoringTests!
PushUpInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #PushUpMethodTest category: #RefactoringTests!
RefactoringTest subclass: #PushUpMethodTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'PushUpMethodTest class' category: #RefactoringTests!
PushUpMethodTest class
	instanceVariableNames: ''!

!classDefinition: #RemoveAllUnreferencedInstanceVariablesTest category: #RefactoringTests!
RefactoringTest subclass: #RemoveAllUnreferencedInstanceVariablesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'RemoveAllUnreferencedInstanceVariablesTest class' category: #RefactoringTests!
RemoveAllUnreferencedInstanceVariablesTest class
	instanceVariableNames: ''!

!classDefinition: #RemoveInstanceVariableTest category: #RefactoringTests!
RefactoringTest subclass: #RemoveInstanceVariableTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'RemoveInstanceVariableTest class' category: #RefactoringTests!
RemoveInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #RemoveParameterTest category: #RefactoringTests!
RefactoringTest subclass: #RemoveParameterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'RemoveParameterTest class' category: #RefactoringTests!
RemoveParameterTest class
	instanceVariableNames: ''!

!classDefinition: #RenameClassTest category: #RefactoringTests!
RefactoringTest subclass: #RenameClassTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'RenameClassTest class' category: #RefactoringTests!
RenameClassTest class
	instanceVariableNames: ''!

!classDefinition: #RenameGlobalTest category: #RefactoringTests!
RefactoringTest subclass: #RenameGlobalTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'RenameGlobalTest class' category: #RefactoringTests!
RenameGlobalTest class
	instanceVariableNames: ''!

!classDefinition: #RenameInstanceVariableTest category: #RefactoringTests!
RefactoringTest subclass: #RenameInstanceVariableTest
	instanceVariableNames: 'oldVariable newVariable classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'RenameInstanceVariableTest class' category: #RefactoringTests!
RenameInstanceVariableTest class
	instanceVariableNames: ''!

!classDefinition: #RenameSelectorTest category: #RefactoringTests!
RefactoringTest subclass: #RenameSelectorTest
	instanceVariableNames: 'oldSelector newSelector senderOfOldSelector classToRefactor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'RenameSelectorTest class' category: #RefactoringTests!
RenameSelectorTest class
	instanceVariableNames: ''!

!classDefinition: #RenameTemporaryTest category: #RefactoringTests!
RefactoringTest subclass: #RenameTemporaryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'RenameTemporaryTest class' category: #RefactoringTests!
RenameTemporaryTest class
	instanceVariableNames: ''!

!classDefinition: #SafelyRemoveClassTest category: #RefactoringTests!
RefactoringTest subclass: #SafelyRemoveClassTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'SafelyRemoveClassTest class' category: #RefactoringTests!
SafelyRemoveClassTest class
	instanceVariableNames: ''!

!classDefinition: #TemporaryToInstanceVariableTest category: #RefactoringTests!
RefactoringTest subclass: #TemporaryToInstanceVariableTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RefactoringTests'!
!classDefinition: 'TemporaryToInstanceVariableTest class' category: #RefactoringTests!
TemporaryToInstanceVariableTest class
	instanceVariableNames: ''!


!CommonScopeNodeFinderTest methodsFor: 'assertions' stamp: 'JEC 16/Jan/2026, 11:49:45 am (UTC)'!
assertFinderFoundMethodNodeScope

	self assertFinderFoundScopeFromNode: self methodNodeScope ! !

!CommonScopeNodeFinderTest methodsFor: 'assertions' stamp: 'JEC 16/Jan/2026, 11:49:59 am (UTC)'!
assertFinderFoundScopeFromNode: aParseNodeWithSourceRanges

	self 
		assert: finder commonScopeNode 
		equals: aParseNodeWithSourceRanges! !

!CommonScopeNodeFinderTest methodsFor: 'class factory' stamp: 'JEC 16/Jan/2026, 11:50:10 am (UTC)'!
classToRefactorName

	^ #ClassToDoExtractTemporary! !

!CommonScopeNodeFinderTest methodsFor: 'excercising' stamp: 'JEC 16/Jan/2026, 11:50:23 am (UTC)'!
compile: aSourceCode

	classToRefactor compile: aSourceCode.
	methodNode := (classToRefactor>>#m1) methodNode.! !

!CommonScopeNodeFinderTest methodsFor: 'excercising' stamp: 'JEC 16/Jan/2026, 11:50:33 am (UTC)'!
methodNodeScope

	^ methodNode -> (1 to: methodNode sourceText size)! !

!CommonScopeNodeFinderTest methodsFor: 'excercising' stamp: 'JEC 16/Jan/2026, 11:50:43 am (UTC)'!
parseNodeAt: aPosition

	^ (methodNode 
		parseNodesPathAt: aPosition
		ifAbsent: [ self shouldNotHappen ]) first! !

!CommonScopeNodeFinderTest methodsFor: 'excercising' stamp: 'JEC 16/Jan/2026, 11:50:49 am (UTC)'!
parseNodeIncluding: aPosition

	^ methodNode
		parseNodeIncluding: aPosition
		ifAbsent: [ self shouldNotHappen ]! !

!CommonScopeNodeFinderTest methodsFor: 'excercising' stamp: 'JEC 16/Jan/2026, 11:51:00 am (UTC)'!
parseNodeOfInterval: anInterval

	| parseNodes |
	
	parseNodes := methodNode 
		parseNodesPathAt: anInterval first 
		ifAbsent: [ self shouldNotHappen ].
	
	"Sometimes returns a LeafNode that is not expected, but the node expected is in the list"
	^ parseNodes detect: [ :parseNode | (parseNode isKindOf: LeafNode) not and: [ parseNode value = anInterval ] ]! !

!CommonScopeNodeFinderTest methodsFor: 'excercising' stamp: 'JEC 16/Jan/2026, 12:00:22 pm (UTC)'!
parseNodeToLookForUsing: aConditionBlock

	| parseNodeToLookFor |

	methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(aConditionBlock value: parseNode) 
			ifTrue: [ parseNodeToLookFor := parseNode -> sourceRanges asOrderedCollection first ] ].
	
	parseNodeToLookFor ifNil: [ self failWith: 'Node not found'].
	
	^ parseNodeToLookFor! !

!CommonScopeNodeFinderTest methodsFor: 'excercising' stamp: 'JEC 16/Jan/2026, 11:51:42 am (UTC)'!
useFinderToFindScopeForAll: aCollectionOfParseNodes

	finder := CommonScopeNodeFinder forAll: aCollectionOfParseNodes in: methodNode .
	methodNode accept: finder.! !

!CommonScopeNodeFinderTest methodsFor: 'setup/teardown' stamp: 'JEC 16/Jan/2026, 11:52:26 am (UTC)'!
setUp

	super setUp.
	classToRefactor := self createClassNamed: self classToRefactorName! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:52:39 am (UTC)'!
test001_WhenHavingMethodNodeToFindScope_ReturnsIt

	self compile: 'm1'.
	
	self useFinderToFindScopeForAll: { self methodNodeScope }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:52:53 am (UTC)'!
test002_WhenNodeTheNodeToFindScopeIsInsideABlockNode_RetunsTheBlockNodeAsTheScopeNode

	| expectedCommonScopeNode |

	self compile: 'm1 [ 1 ]'.
	expectedCommonScopeNode := self parseNodeAt: 4.
	
	self useFinderToFindScopeForAll: { self parseNodeIncluding: 6 }.
	
	self assertFinderFoundScopeFromNode: expectedCommonScopeNode! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:53:00 am (UTC)'!
test003_WhenNodesHaveDifferentScopes_ReturnsTheNearestScopeThatContainsAllTheScopes

	| expectedScopeNode |

	self compile: 'm1 [ [ 1 ]. [ 1 ] ]'.
	expectedScopeNode := self parseNodeAt: 4.
	
	self useFinderToFindScopeForAll: { 
			self parseNodeIncluding: 8 .
			self parseNodeIncluding: 15 }.
	
	self assertFinderFoundScopeFromNode: expectedScopeNode! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:53:11 am (UTC)'!
test004_CanFindScopeOfFirstNodeApparition

	| expectedScopeNode |

	self compile: 'm1 [ [ 1 ]. [ 1 ] ]'.
	expectedScopeNode := self parseNodeAt: 6.
	
	self useFinderToFindScopeForAll: { self parseNodeIncluding: 8 }.
	
	self assertFinderFoundScopeFromNode: expectedScopeNode! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:53:17 am (UTC)'!
test005_CanFindScopeOfANonFirstApparition

	| expectedScopeNode |

	self compile: 'm1 [ [ 1 ]. [ 1 ] ]'.
	expectedScopeNode := self parseNodeAt: 13.
	
	self useFinderToFindScopeForAll: { self parseNodeIncluding: 15 }.
	
	self assertFinderFoundScopeFromNode: expectedScopeNode! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:53:25 am (UTC)'!
test006_CanFindScopeOfLatestNodesAfterSeveralScopes

	self compile: 'm1 [ 1 ]. 1'.
	
	self useFinderToFindScopeForAll: { self parseNodeIncluding: 11 }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:53:33 am (UTC)'!
test007_CanFindTheScopeOfABlock

	| expectedScopeNode |

	self compile: 'm1 [ [ 1 ] ]'.
	expectedScopeNode := self parseNodeAt: 4.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 6 }.
	
	self assertFinderFoundScopeFromNode: expectedScopeNode! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:53:42 am (UTC)'!
test008_WhenCannotFindANode_ThrowsException

	| unreachableMethodNode |

	self compile: 'm1 '.
	classToRefactor compile: 'm2 [ ]'.
	unreachableMethodNode := (classToRefactor>>#m2) methodNode.
	
	self useFinderToFindScopeForAll: { 
		unreachableMethodNode -> (1 to: unreachableMethodNode sourceText size) }.
	
	self 
		should: [ finder commonScopeNode ] 
		raise: Error - MessageNotUnderstood 
		withMessageText: CommonScopeNodeFinder couldNotFindANodeErrorMessage! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:53:51 am (UTC)'!
test009_CanFindTheScopeOfAnAssignment

	self compile: 'm1 | a | a := 1'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 11 }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:53:57 am (UTC)'!
test010_CanFindTheScopeOfATemporaryVariable

	self compile: 'm1 | a | a := 1'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 10 }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 12:03:27 pm (UTC)'!
test011_CanFindTheScopeOfAMessageBlock

	| parseNodeToLookFor |

	self compile: 'm1 self value'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | parseNode isMessageNode ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:54:13 am (UTC)'!
test012_CanFindTheScopeOfABacktickNode

	self compile: 'm1 `1`'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 4 }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:54:19 am (UTC)'!
test013_CanFindTheScopeOfABraceNode

	self compile: 'm1 { 1 }'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 4 }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 12:02:53 pm (UTC)'!
test014_CanFindTheScopeOfACascadeNode

	| parseNodeToLookFor |

	self compile: 'm1 self value; value'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | parseNode isCascadeNode ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:54:31 am (UTC)'!
test015_CanFindTheScopeOfAInstanceVariableNode

	classToRefactor addInstVarName: 'anInstanceVariable'.
	self compile: 'm1 anInstanceVariable'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 4 }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 12:02:21 pm (UTC)'!
test016_CanFindTheScopeOfAMessageNodeInCascade

	| parseNodeToLookFor |

	self compile: 'm1 self value; value'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | 
		parseNode isMessageNode and: [ parseNode isCascade ] ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:56:07 am (UTC)'!
test017_CanFindTheScopeOfReturnNode

	self compile: 'm1 ^ 1'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 4 }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 12:01:49 pm (UTC)'!
test018_CanFindTheScopeOfSelectorNode

	| parseNodeToLookFor |

	self compile: 'm1 self value; value'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | 
		parseNode isMessageNode and: [ parseNode isCascade ]].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 12:00:52 pm (UTC)'!
test019_CanFindTheScopeOfTemporariesDeclarationNode

	| parseNodeToLookFor |

	self compile: 'm1 | a |'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | parseNode isTemporariesDeclaration ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:58:36 am (UTC)'!
test020_CanFindTheScopeOfTemporaryDeclarationNode

	| parseNodeToLookFor |

	self compile: 'm1 | a |'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | parseNode isTemporaryDeclaration ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 8:59:17 pm (UTC)'!
test021_CanFindTheScopeOfVariableNode

	| parseNodeToLookFor |

	self compile: 'm1 ^ self value'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | parseNode isVariableNode ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope ! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:56:53 am (UTC)'!
test022_CanFindTheScopeOfLiteralVariableNode

	self compile: 'm1 undeclared'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 4 }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:57:00 am (UTC)'!
test023_TheScopeOfTheMethodNodeBlockIsTheMethodNode

	self compile: 'm1 '.
	
	self useFinderToFindScopeForAll: { methodNode block -> (self methodNodeScope value)}.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:57:05 am (UTC)'!
test024_CanFindTheScopeOfAnEmptyBlock

	self compile: 'm1 []'.
	
	self useFinderToFindScopeForAll: { self parseNodeOfInterval: (4 to: 5) }.
	
	self assertFinderFoundMethodNodeScope! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 11:57:14 am (UTC)'!
test025_CanFindTheCommonScopeOfNodesRegardlessOfOrder

	| expectedScopeNode |

	self compile: 'm1
	[ 10. [ 10. ] ]'.
	expectedScopeNode := self parseNodeAt: 5.
	
	self useFinderToFindScopeForAll: { 
		self parseNodeOfInterval: (13 to: 14).
		self parseNodeOfInterval: (7 to: 8) . }.
	
	self assertFinderFoundScopeFromNode: expectedScopeNode! !

!CommonScopeNodeFinderTest methodsFor: 'tests' stamp: 'JEC 16/Jan/2026, 12:45:49 pm (UTC)'!
test026_WhenThereIsANodeToFindInTheMethodNode_ReturnsItAsTheScope

	self compile: 'm1 [ 10 ]. 10'.
	
	self useFinderToFindScopeForAll: { 
		self parseNodeOfInterval: (12 to: 13).
		self parseNodeOfInterval: (6 to: 7) . }.
	
	self assertFinderFoundMethodNodeScope ! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:21:41 pm (UTC)'!
test01FindsSelectionOnSourceMethod

	| replacements |

	self 
		compile: 'm1 10' 
		andFind: (4 to: 5).
	
	self assert: finder hasOneReplacement.
	replacements := finder replacements.
	
	self assert: 1 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 5).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:22:04 pm (UTC)'!
test02FindsRepetitionsAfterSelection

	| replacements |
	
	self 
		compile: 'm1 10. 10' 
		andFind: (4 to: 5).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 5).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (8 to: 9)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:28:01 pm (UTC)'!
test03FindsRepetitionsBeforeSelection

	| replacements |
	
	self 
		compile: 'm1 10. 10' 
		andFind: (8 to: 9).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 5).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (8 to: 9).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:28:12 pm (UTC)'!
test04DoesNotIncludeComments

	| replacements |
	
	self 
		compile: 'm1 10. "10"' 
		andFind: (4 to: 5).
	
	replacements := finder replacements.
	
	self assert: 1 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 5).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:28:17 pm (UTC)'!
test05DoesNotIncludeNotExtractableSourceCode

	| replacements |
	
	self 
		compile: 'm1 10. 100' 
		andFind: (4 to: 5).
	
	replacements := finder replacements.
	
	self assert: 1 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 5).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:28:34 pm (UTC)'!
test06FindsRepetitionsInOtherClassMethods

	| replacements |	

	classToRefactor compile: 'm1b 100+10'.	
	self 
		compile: 'm1 10' 
		andFind: (4 to: 5).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (9 to: 10)
		andIsOf: (classToRefactor >> #m1b) asMethodReference.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 5).

	! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:28:57 pm (UTC)'!
test07FindsRepetitionsInSubclassesMethods

	| replacements subclassToRefactor |
	
	classToRefactor compile: 'm1b 100+10'.
	subclassToRefactor := self createClassNamed: self subclassToRefactorName subclassOf: classToRefactor.
	subclassToRefactor compile: 'm3 10'.
	self 
		compile: 'm1 10. 10' 
		andFind: (4 to: 5).
	
	replacements := finder replacements.
	
	self assert: 4 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (9 to: 10)
		andIsOf: (classToRefactor >> #m1b) asMethodReference.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 5).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (8 to: 9).
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (4 to: 5)
		andIsOf: (subclassToRefactor >> #m3) asMethodReference.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:29:21 pm (UTC)'!
test08FindsRepetitionsWithMultipleSelection

	| replacements |

	self 
		compile: 'm1 10 factorial. 10 factorial. true ifTrue: [ 10 factorial. 10 factorial]' 
		andFind: (4 to: 29).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 29).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (47 to: 72).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:29:40 pm (UTC)'!
test09FindsRepetitionsWithCascade

	| replacements |
	
	self 
		compile: 'm1 self m1; m2. self m1; m2' 
		andFind: (4 to: 14).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 14).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (17 to: 27).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:30:02 pm (UTC)'!
test10FindsRepetitionsWithBrace

	| replacements |
	
	self 
		compile: 'm1 {1 . 2 }. {1 . 2 }' 
		andFind: (4 to: 11).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 11).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (14 to: 21)
	! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:30:22 pm (UTC)'!
test11FindsRepetitionsWithMoreThanTwoStatementsSelection

	| replacements |
	
	self 
		compile: 'm1 10 factorial. 15 factorial. 20 factorial. true ifTrue: [ 10 factorial. 15 factorial. 20 factorial]' 
		andFind: (4 to: 43).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 43).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (61 to: 100).	! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:32:06 pm (UTC)'!
test12FindsRepetitionsOfOneMessageSend

	| replacements |
	
	self 
		compile: 'm1 10 factorial. 10   factorial' 
		andFind: (4 to: 15).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 15).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (18 to: 31)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:32:19 pm (UTC)'!
test13FindsRepetitionsOfLiteral

	| replacements |
	
	self 
		compile: 'm1 10. 10 + 5' 
		andFind: (4 to: 5).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 5).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (8 to: 9)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:32:33 pm (UTC)'!
test14FindsRepetitionsOfLiteralVariable

	| replacements |
	
	self 
		compile: 'm1 Array. Array new' 
		andFind: (4 to: 8).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 8).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (11 to: 15)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:32:52 pm (UTC)'!
test15FindsRepetitionsOfInstanceVariable

	| replacements |
	
	classToRefactor addInstVarName: 'iv1'.
	self 
		compile: 'm1 iv1. iv1 + 2' 
		andFind: (4 to: 6).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 6).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (9 to: 11)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:33:12 pm (UTC)'!
test16FindsRepetitionsOfParameter

	| replacements |
	
	self 
		compile: 'm1: p p+1. p+1' 
		andFind: (7 to: 9).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (7 to: 9).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (12 to: 14)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:33:30 pm (UTC)'!
test17KeepsOriginalSelection

	| replacements |
	
	self 
		compile: 'm1 1 factorial. 1 factorial. 1 factorial. 1 factorial. 1 factorial' 
		andFind: (17 to: 40).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (17 to: 40).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (43 to: 66)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:33:46 pm (UTC)'!
test18FindsMultipleLiteralNodes

	| replacements |
	
	self 
		compile: 'm1 1. 1. 1. 1. 1' 
		andFind: (4 to: 7).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 7).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (10 to: 13)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:17:55 pm (UTC)'!
test19FindsBlockNodesWithTemporaries

	| replacements |
	
	classToRefactor compile: 'm2 true and: [ | t1 | false ]'.
	
	self 
		compile: 'm1 true and: [ | t1 | false ]' 
		andFind: (16 to: 27)
		to: (Message selector: #m3).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (16 to: 27). 
	self
		assertAnyReplacementOf: replacements 
		hasInterval: (16 to: 27)
		andIsOf: (classToRefactor >> #m2) asMethodReference! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:34:03 pm (UTC)'!
test20FindsTemporary

	| replacements |
	
	self 
		compile: 'm1 | t1 | t1. t1' 
		andFind: (11 to: 12).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (11 to: 12).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (15 to: 16)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:34:15 pm (UTC)'!
test21FindsTemporaryWhenSelectingDefinition

	| replacements |

	self 
		compile: 'm1 | t1 | t1' 
		andFind: (4 to: 12).
	
	replacements := finder replacements.
	
	self assert: 1 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 12).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:34:35 pm (UTC)'!
test22_1_FindsTemporaryWhenSelectingDefinitionInInnerClosure

	| sourceCode replacements |
	
	sourceCode := 'boundsUsing: aBlock 
	
	^self isSingleSelection 
		ifTrue: [ aBlock value: #selected first ]
		ifFalse: [ | points |
	
	points := #selected 
		inject: OrderedCollection new
		into: [ :allPoints :aSelectedWidget | | bounds |
			bounds := aBlock value: aSelectedWidget.
			allPoints add: bounds origin; add: bounds corner.
			allPoints ].
	
	Rectangle encompassing: points ]'.

	self compile: sourceCode andFind: (107 to: 370).
	
	replacements := finder replacements.
	
	self assert: 1 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (107 to: 370)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:34:50 pm (UTC)'!
test22FindsTemporaryWhenSelectingDefinitionInInnerClosure

	| replacements |
	
	self 
		compile: 'm1 | t1 | true and: [ |t2| t1 ]. t1' 
		andFind: (23 to: 29).
	
	replacements := finder replacements.
	
	self assert: 1 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (23 to: 29).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:35:07 pm (UTC)'!
test23WhenExtractingAllLiteralsUsesStringComparison

	| replacements |

	self 
		compile: 'm1 1+2. 2+1.' 
		andFind: (4 to: 4).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 4).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (11 to: 11)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Mar/2026, 5:20:26 pm (UTC)'!
test24NotWorkingCase

	| replacements |
	
	self shouldFail: [
		self 
			compile: 'm1 1+2. 2+1' 
			andFind: (4 to: 4).
		
		replacements := finder replacements.
		
		self assert: 2 equals: replacements size.
		self
			assertSourceMethodReplacement: replacements first 
			hasInterval: (4 to: 4).
		self
			assertSourceMethodReplacement: replacements first 
			hasInterval: (11 to: 11) ]! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Mar/2026, 5:20:53 pm (UTC)'!
test25NotWorkingCase

	| replacements |
	
	self shouldFail: [
		self 
			compile: 'm1 1. 1. 1. 1.' 
			andFind: (7 to: 10).
		
		replacements := finder replacements.
		
		self assert: 1 equals: replacements size.
		self
			assertSourceMethodReplacement: replacements first 
			hasInterval: (7 to: 10) ]
! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:35:27 pm (UTC)'!
test26FindsInsideParentesis

	| replacements |

	self 
		compile: 'm1 (self m1)' 
		andFind: (5 to: 11).
	
	replacements := finder replacements.
		
	self assert: 1 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (5 to: 11)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:35:35 pm (UTC)'!
test27FindsWithMoreThanOneStatementAndFirstWithParentesis

	| replacements |

	self 
		compile: 'm1 (self m1). self m1' 
		andFind: (4 to: 21).
	
	replacements := finder replacements.
		
	self assert: 1 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 21).
! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:35:57 pm (UTC)'!
test28FindsRepeatedBlocks

	| replacements |

	self 
		compile: 'm1 [self m1]. [self m1]' 
		andFind: (4 to: 12).
	
	replacements := finder replacements.
		
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 12).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (15 to: 23)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Mar/2026, 4:47:04 pm (UTC)'!
test29DoesNotFailWithUnnecesaryDot

	| sourceMethodName intervalToExtract newMessage |

	sourceMethodName := classToRefactor compile: 'm1. self m2'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (5 to: 11) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	
	self 
		shouldnt: [ finder value ] 
		raise: Error! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Mar/2026, 4:47:04 pm (UTC)'!
test30DoesNotFailWhenLookingForABlockEquivalentToAMethod

	| sourceMethodName intervalToExtract newMessage |

	sourceMethodName := classToRefactor compile: 'm1. [10 factorial]'.
	classToRefactor compile: 'm2. 10 factorial'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (5 to: 18) asSourceCodeInterval.
	newMessage := Message selector: #m3.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	
	self 
		shouldnt: [ finder value ] 
		raise: Error! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:36:05 pm (UTC)'!
test31FindsReturningNodes

	| replacements |
	
	self 
		compile: 'm1 ^ 1' 
		andFind: (4 to: 6).
	
	replacements := finder replacements.
	
	self assert: 1 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 6).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:36:23 pm (UTC)'!
test32FindsRepeatedReturningNodes

	| replacements |
	
	self 
		compile: 'm1 [ ^ 1 ]. [ ^ 1 ]' 
		andFind: (6 to: 8).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (6 to: 8).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (15 to: 17).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:36:43 pm (UTC)'!
test33FindRepetitionsInAPartiallyExtractedCascadeCode

	| replacements |
	
	self 
		compile: 'm1 self value; value; value. self value; value; value.' 
		andFind: (4 to: 20).
	
	replacements := finder replacements.
	
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (4 to: 20).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (30 to: 46)! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:36:52 pm (UTC)'!
test34_FindsParameterizableReplacements

	| paramReplacements |
	
	self 
		compile: 'm1 self foo: 10. self foo: self' 
		andFind: (4 to: 15).
	
	paramReplacements := finder parameterizableReplacements.
	
	self assert: 1 equals: paramReplacements size.
	self 
		assertAnyParamReplacementOf: paramReplacements 
		hasInterval: (18 to: 31) 
		hasParamsIntervals: { 28 to: 31 } 
		andIsOf: sourceMethod! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:37:09 pm (UTC)'!
test35_CanMakeParameterizedReplacementsToBeReplacements

	| replacements |
	
	self 
		compile: 'm1 self foo: 10. self foo: self' 
		andFind: (4 to: 15).
	
	self acceptParamReplacementsTo: (Message selector: #m2:).
	
	replacements := finder replacements.
	self assert: 2 equals: replacements size.

	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (18 to: 31)
		isOf: sourceMethod
		andHasEqualParamsInOrder: { 'self' }
		withIntervals: { 28 to: 31 }.
	
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (4 to: 15)
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '10' }
		withIntervals: { 14 to: 15 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:37:29 pm (UTC)'!
test36_WhenHavingReplacementsAlongWithParameterizedReplacements_ConvertsAllParameterizedToReplacements

	| replacements |
	
	self 
		compile: 'm1 self foo: 10. self foo: self. self foo: 10' 
		andFind: (4 to: 15).
	
	self acceptParamReplacementsTo: (Message selector: #m2:).
	
	replacements := finder replacements.
	self assert: 3 equals: replacements size.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (18 to: 31) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { 'self' }
		withIntervals: { 28 to: 31 }.
	
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (4 to: 15) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '10' }
		withIntervals: { 14 to: 15 }.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (34 to: 45)
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '10' }
		withIntervals: { 44 to: 45 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:37:45 pm (UTC)'!
test37_WhenExtractingMultipleLines_AndHaveParamReplacementsWithSameParamArgument_ConvertsAllToReplacements

	| sourceCode replacements |
	
	sourceCode := 'm1 
		self foo: 10. 
		self foo: 10.
		 
		self foo: 10.
		self foo: 20.
		
		self foo: 10.
		self foo: ''b''.'.
	self compile: sourceCode andFind: (7 to: 35).
	
	self acceptParamReplacementsTo: (Message selector: #k1:).
	
	replacements := finder replacements.
	self assert: 3 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (44 to: 71) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '20' }
		withIntervals: { 70 to: 71 }.
	
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (79 to: 107)
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '''b''' }
		withIntervals: { 105 to: 107 }.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (7 to: 35) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '10' }
		withIntervals: { 34 to: 35 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:37:57 pm (UTC)'!
test38_WhenExtractingMultipleLines_AndParamReplacementsHaveDifferentParamArguments_ConvertsAllToReplacements

	| sourceCode replacements |
	
	sourceCode := 'm1 
		self foo: 10. 
		self foo: 10.
		 
		self foo: 10.
		self foo: 20.
		
		self foo: ''a''.
		self foo: 10.'.
	self compile: sourceCode andFind: (7 to: 35).
	
	self acceptParamReplacementsTo: (Message selector: #k1:k2:).
	
	replacements := finder replacements.
	self assert: 3 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (44 to: 71) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '10' . '20' }
		withIntervals: { 54 to: 55 . 70 to: 71 }.
	
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (79 to: 107)
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '''a''' . '10' }
		withIntervals: { 89 to: 91 . 106 to: 107 }.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (7 to: 35) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '10' . '10' }
		withIntervals: { 17 to: 18 . 34 to: 35 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:38:10 pm (UTC)'!
test39_WhenExtractingMultipleLines_AndParamReplacementsHaveAllDifferentParamArguments_ConvertsAllToReplacements

	| replacements sourceCode |
	
	sourceCode := 'm1 
		self foo: 10. 
		self foo: 10.
		 
		self foo: 30.
		self foo: 20.
		
		self foo: ''a''.
		self foo: ''b''.'.
	self compile: sourceCode andFind: (7 to: 35).
	
	self acceptParamReplacementsTo: (Message selector: #k1:k2:).
	
	replacements := finder replacements.
	self assert: 3 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (44 to: 71) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '30' . '20' }
		withIntervals: { 54 to: 55 . 70 to: 71 }.
	
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (79 to: 108)
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '''a''' .  '''b''' }
		withIntervals: { 89 to: 91 . 106 to: 108 }.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (7 to: 35) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '10' . '10' }
		withIntervals: { 17 to: 18 . 34 to: 35 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:38:22 pm (UTC)'!
test40_WhenExtractingMultipleLines_AndOnlyOneParamReplacementsHaveAllDifferentParamArguments_ConvertsAllToReplacements

	| sourceCode replacements |
	
	sourceCode := 'm1 
		self foo: 10. 
		self foo: 10.
		 
		self foo: ''a''.
		self foo: ''b''.
		
		self foo: 10.
		self foo: 10.'.
	self compile: sourceCode andFind: (7 to: 35).
	
	self acceptParamReplacementsTo: (Message selector: #k1:k2:).
	
	replacements := finder replacements.
	self assert: 3 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (44 to: 73) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '''a''' .  '''b''' }
		withIntervals: { 54 to: 56 . 71 to: 73 }.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (7 to: 35) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '10' . '10' }
		withIntervals: { 17 to: 18 . 34 to: 35 }.
	
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (81 to: 108)
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '10' . '10' }
		withIntervals: { 91 to: 92 . 107 to: 108 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:38:31 pm (UTC)'!
test41_CannotFindRecursiveMethodsAsParameterizable

	| paramReplacements |
	
	self 
		compile: 'm1 self foo: (self foo: 1). self foo: 2' 
		andFind: (29 to: 39).
	
	paramReplacements := finder parameterizableReplacements.
	
	self assert: paramReplacements size equals: 1.
	
	self 
		assertAnyParamReplacementOf: paramReplacements
		hasInterval: (4 to: 26)
		hasParamsIntervals: { 14 to: 26 }
		andIsOf: sourceMethod! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:38:38 pm (UTC)'!
test42_CannotFindArgumentsOfExtractedCodeAsParameterizable

	| paramReplacements |
	
	self 
		compile: 'm1 self foo: (self foo: 1). self foo: 2' 
		andFind: (4 to: 26).
	
	paramReplacements := finder parameterizableReplacements.
	
	self assert: 1 equals: paramReplacements size.
	
	self 
		assertAnyParamReplacementOf: paramReplacements
		hasInterval: (29 to: 39)
		hasParamsIntervals: { 38 to: 39 } 
		andIsOf: sourceMethod! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:38:47 pm (UTC)'!
test43_CannotFindRecursiveCodeOfExtractedCodeAsParameterizable

	| paramReplacements |

	self 
		compile: 'm1 self foo: (self foo: 1). self foo: 2' 
		andFind: (15 to: 25).
	
	paramReplacements := finder parameterizableReplacements.
	
	self assert: 1 equals: paramReplacements size.

	self 
		assertAnyParamReplacementOf: paramReplacements
		hasInterval: (29 to: 39)
		hasParamsIntervals: { 38 to: 39 }
		andIsOf: sourceMethod! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 3:41:15 pm (UTC)'!
test44_CanExtractMethodWithATempVarAsParameterizableCode

	| replacements sourceCode |
	
	sourceCode := 'm1 
		| a |
		a := 10.
		self kk: a kk: [ ^ 1 ].
		self kk: 1 kk: 2.
		^ a'.
	self 
		compile: sourceCode 
		andFind: (26 to: 47)
		to: (Message selector: #m2: arguments: #('a')).
	
	self acceptParamReplacementsTo: (Message selector: #k1:k2:).
	
	replacements := finder replacements.
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (52 to: 67) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '1' . '2' }
		withIntervals: { 61 to: 61 . 67 to: 67 }.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (26 to: 47) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { 'a' .  '[ ^ 1 ]' }
		withIntervals: { 35 to: 35 . 41 to: 47 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 3:41:07 pm (UTC)'!
test45_CanExtractMethodWithATempVarAsParameterizableCodeInDifferentOrder

	| replacements sourceCode |
	
	sourceCode := 'm1 
		| a |
		a := 10.
		self kk: [ ^ 1 ] kk: a.
		self kk: 1 kk: 2.
		^ a'.
	self 
		compile: sourceCode 
		andFind: (26 to: 47)
		to: (Message selector: #m2: arguments: #('a')).
	
	self acceptParamReplacementsTo: (Message selector: #k1:k2:).
	
	replacements := finder replacements.
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (52 to: 67) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '1' . '2' }
		withIntervals: { 61 to: 61 . 67 to: 67 }.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (26 to: 47) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '[ ^ 1 ]' . 'a' }
		withIntervals: { 35 to: 41 . 47 to: 47 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 3:41:22 pm (UTC)'!
test46_CanExtractMethodWithParameterizableCode_HavingParameterizableTempVarNotFoundByTheFinder
	
	"This case is when extracting a piece of code with a tempVar that is not taken as a parameteriazable code 
	by the finder but it must be parameterized (the way it worked)"

	| sourceCode replacements |
	
	sourceCode := 'm1 
		| a |
		a := 10.
		self kk: a kk: [ ^ 1 ].
		self kk: a kk: 2.
		^ a'.
	self 
		compile: sourceCode 
		andFind: (26 to: 47)
		to: (Message selector: #m2: arguments: #('a')).
	
	self 
		acceptParamReplacementsWithNodesToParameterized: {self tempVariableNodeNamed: 'a'}
		to: (Message selector: #k1:k2:).
	
	"Pay attention to the param intervals, the only interval here is for the parameterizable node found and not
	the variable"
	replacements := finder replacements.
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (52 to: 67) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { 'a' . '2' }
		withIntervals: { 67 to: 67 }.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (26 to: 47) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { 'a' . '[ ^ 1 ]' }
		withIntervals: { 41 to: 47 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 3:41:27 pm (UTC)'!
test47_WhenAParameterizableTempVarFoundOutsideTheFinder_TheReplacementKeepsTheOrderOrTheParameterizableCode

	| sourceCode replacements |
	
	sourceCode := 'm1 
		| a |
		a := 10.
		self kk: [ ^ 1 ] kk: a.
		self kk: 2 kk: a.
		^ a'.
	self 
		compile: sourceCode 
		andFind: (26 to: 47)
		to: (Message selector: #m2: arguments: #('a')).
	
	self 
		acceptParamReplacementsWithNodesToParameterized: {self tempVariableNodeNamed: 'a'}
		to: (Message selector: #k1:k2:).
	
	"Pay attention to the param intervals, the only interval here is for the parameterizable node found and not
	the variable"
	replacements := finder replacements.
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (52 to: 67) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '2' . 'a' }
		withIntervals: { 61 to: 61 }.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (26 to: 47) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { '[ ^ 1 ]' . 'a' }
		withIntervals: { 35 to: 41 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 27/Mar/2026, 12:46:49 am (UTC)'!
test48_CannotExtractMethodWithParameterizableCode_WhenATempVarIsParameterizableMultipleTimes
	
	self should: [
		| sourceCode |
	
		sourceCode := 'm1 
			| a |
			a := 10.
			self qq: a qq: 5 qq: 10.
			self qq: 1 qq: a qq: a.
			^ a'.
		self 
			compile: sourceCode 
			andFind: (26 to: 51)
			to: (Message selector: #m2: arguments: #('a')).
		
		self 
			acceptParamReplacementsWithNodesToParameterized: {self tempVariableNodeNamed: 'a'}
			to: (Message selector: #k1:k2:k3:) ]
		raise: Error
		withMessageText: ExtractMethodReplacementsFinder cannotApplyTheRefactoringDueToDuplicateTempVarErrorMessage.
	
	self deny: (classToRefactor includesSelector: #m2).
	self deny: (classToRefactor includesSelector: #k1:k2:k3:).! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 3:48:34 pm (UTC)'!
test49_FindsParameterizableCodeInOtherClassMethods

	| replacements |	

	classToRefactor compile: 'm1b self foo: 1 foo: 1'.	
	self 
		compile: 'm1 self foo: 1 foo: 9' 
		andFind: (4 to: 21).
	
	self acceptParamReplacementsTo: (Message selector: #m2:).
	
	replacements := finder replacements.
	self assert: replacements size equals: 2.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (5 to: 22) 
		isOf: (classToRefactor >> #m1b) asMethodReference
		andHasEqualParamsInOrder: { ' 1' }
		withIntervals: { 21 to: 22 }.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (4 to: 21) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { ' 9' }
		withIntervals: { 20 to: 21 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 3:48:42 pm (UTC)'!
test50_FindsParameterizableCodeInSubclassesMethods

	| replacements subclassToRefactor |
	classToRefactor compile: 'm1b self foo: 1 foo: 1'.
	subclassToRefactor := self createClassNamed: self subclassToRefactorName subclassOf: classToRefactor.
	subclassToRefactor compile: 'm3 self foo: 1 foo: 5'.
	self 
		compile: 'm1 self foo: 1 foo: 9' 
		andFind: (4 to: 21).
	
	self acceptParamReplacementsTo: (Message selector: #m2:).
	
	replacements := finder replacements.
	self assert: replacements size equals: 3.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (5 to: 22) 
		isOf: (classToRefactor >> #m1b) asMethodReference
		andHasEqualParamsInOrder: { ' 1' }
		withIntervals: { 21 to: 22 }.
	
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (4 to: 21) 
		isOf: (subclassToRefactor >> #m3) asMethodReference
		andHasEqualParamsInOrder: { ' 5' }
		withIntervals: { 20 to: 21 }.
		
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (4 to: 21) 
		isOf: sourceMethod
		andHasEqualParamsInOrder: { ' 9' }
		withIntervals: { 20 to: 21 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:40:22 pm (UTC)'!
test51_WhenFindingParameterizableCode_AvoidsReplacementsIntervals

	"When extracting the first foo:10 foo:10, founds the second foo:10... as a replacement. 
	Then, the ParameterizableNodesReplacement will find as a parameterizable code foo:10 . foo: 'a'. 
	But, as foo:10 was part of the second replacement and, because of that was never added as a 
	parameterizableReplacement and showing no parameterizable code. Making it a weird behaviour."

	| sourceCode replacements paramReplacements |
	
	sourceCode := 'm1 
		self foo: 10. 
		self foo: 10. 
		
		self foo: 10. 
		self foo: 10.
		
		self foo: ''a''.
		self foo: ''b''.'.
	self compile: sourceCode andFind: (7 to: 35).
	
	replacements := finder replacements.
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (7 to: 35).
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval: (44 to: 72).
		
	paramReplacements := finder parameterizableReplacements.
	self assert: 1 equals: paramReplacements size.
	self 
		assertAnyParamReplacementOf: paramReplacements 
		hasInterval: (80 to: 109) 
		hasParamsIntervals: { 90 to: 92 . 107 to: 109 } 
		andIsOf: sourceMethod! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:18:30 pm (UTC)'!
test52_CanFindReplacementsInTwoEqualMethodsWithDifferentSelectors

	"This was an edge case. Up to now, both CompileMethod of these methods are equal because #= 
	is not comparing the selector, bringing a weird behaviour in the ExtractMethod (not finding the 
	not extracted method as replacement)"

	| sourceCode replacements |
	
	sourceCode := '

	self foo: 90.
	self foo: 22.
	self foo: 90.
	self foo: 90.'.
	
	classToRefactor compile: #m11 , sourceCode.	
	self 
		compile: #m1 , sourceCode 
		andFind: (6 to: 32).

	replacements := finder replacements.
	self assert: 2 equals: replacements size.
	self 
		assertAnyReplacementOf: replacements 
		isOfSourceMethodAndHasInterval:(6 to: 32).
	self
		assertAnyReplacementOf: replacements 
		hasInterval: (7 to: 33)
		andIsOf: (classToRefactor >> #m11) asMethodReference.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 14/Apr/2026, 9:20:49 pm (UTC)'!
test53_CanFindParameterizableReplacementsInTwoEqualMethodsWithDifferentSelectors

	"This was an edge case. Up to now, both CompileMethod of these methods are equal because #= 
	is not comparing the selector, bringing a weird behaviour in the ExtractMethod (not finding the 
	not extracted method as replacement)"

	| sourceCode paramReplacements |
	
	sourceCode := '

	self foo: 90.
	self foo: 22.
	self foo: 90.
	self foo: 90.'.
	
	classToRefactor compile: #m11 , sourceCode.	
	self 
		compile: #m1 , sourceCode 
		andFind: (6 to: 32).

	paramReplacements := finder parameterizableReplacements.
	self assert: paramReplacements size equals: 2.
	self 
		assertAnyParamReplacementOf: paramReplacements 
		hasInterval: (36 to: 62) 
		hasParamsIntervals: { 61 to: 62 } 
		andIsOf: sourceMethod.
	self 
		assertAnyParamReplacementOf: paramReplacements 
		hasInterval: (37 to: 63) 
		hasParamsIntervals: { 62 to: 63 } 
		andIsOf: (classToRefactor>>#m11) methodReference.
	! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 22/Aug/2026, 7:45:55 pm (UTC)'!
test54_CanFindParameterizableReplacementsOfMultipleWhenReceiversAreEquivalent
	
	| sourceCode paramReplacements |
	
	classToRefactor addInstVarName: 'instVarCollection'.
	sourceCode := 'm1
	| a b c aCollection |
	aCollection := OrderedCollection new.
	a := aCollection inject: 0 into: [:acc :each | acc + each].
	b := instVarCollection inject: 1 into: [:acc :each | acc * each].
	c := self inject: 1 into: [:acc :each | acc ** each].
	^ a + b + c'.
	
	self 
		compile: sourceCode 
		andFind: (72 to: 124)
		to: (Message selector: #k1:k2:k3: arguments: #('aCollection' 'a' 'b')).

	paramReplacements := finder parameterizableReplacements.
	self assert: paramReplacements size equals: 2.
	self 
		assertAnyParamReplacementOf: paramReplacements 
		hasInterval: (133 to: 191) 
		hasParamsIntervals: { 133 to: 149 . 159 to: 159 . 167 to: 191 }  
		andIsOf: sourceMethod.
	self 
		assertAnyParamReplacementOf: paramReplacements 
		hasInterval: (200 to: 246) 
		hasParamsIntervals: { 200 to: 203 . 213 to: 213 . 221 to: 246 } 
		andIsOf: sourceMethod.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 22/Aug/2026, 7:50:20 pm (UTC)'!
test55_CanAcceptParameterizableReplacementWithEquivalentReceivers

	| sourceCode replacements |
	
	classToRefactor addInstVarName: 'instVarCollection'.
	sourceCode := 'm1
	| a b c aCollection |
	aCollection := OrderedCollection new.
	a := aCollection inject: 0 into: [:acc :each | acc + each].
	b := instVarCollection inject: 1 into: [:acc :each | acc * each].
	c := self inject: 1 into: [:acc :each | acc ** each].
	^ a + b + c'.
	
	self 
		compile: sourceCode 
		andFind: (72 to: 124)
		to: (Message selector: #k1:k2:k3: arguments: #('aCollection' 'a' 'b')).
		
	self acceptParamReplacementsTo: (Message selector: #k1:k2:k3: arguments: #('aCollection' 'a' 'b')).

	replacements := finder replacements.
	self assert: replacements size equals: 3.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (133 to: 191) 
		isOf: sourceMethod 
		andHasEqualParamsInOrder: { 'instVarCollection' . '1' . '[:acc :each | acc * each]' }
		withIntervals: { 133 to: 149 . 159 to: 159 . 167 to: 191 }.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (200 to: 246) 
		isOf: sourceMethod 
		andHasEqualParamsInOrder: { 'self' . '1' . '[:acc :each | acc ** each]' }
		withIntervals: { 200 to: 203 . 213 to: 213 . 221 to: 246 }.
	self 
		assertAnyReplacementOf: replacements 
		hasInterval: (72 to: 124) 
		isOf: sourceMethod 
		andHasEqualParamsInOrder: { 'aCollection' . '0' . '[:acc :each | acc + each]' }
		withIntervals: { 72 to: 82 . 92 to: 92 . 100 to: 124 }.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 9/Apr/2026, 1:01:00 am (UTC)'!
test56_CannotFindParameterizableReplacementWhenParametersHaveReturns

	| sourceCode |
	
	sourceCode := 'findValidInSelfOr: aCollection
	self do: [:elem | elem isValid ifTrue: [^each]].
	aCollection do: [:group | group isValid ifTrue: [^each]].
	^self notFound'.
	
	self 
		compile: sourceCode 
		andFind: (33 to: 79)
		to: (Message selector: #k1:k2: arguments: #('a' 'b')).

	self deny: finder hasFoundParameterizableReplacements.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 9/Apr/2026, 1:02:22 am (UTC)'!
test57_CannotFindParameterizableReplacementWhenParametersHaveAssignments

	| sourceCode |
	
	sourceCode := 'findValidInSelfOr: aCollection
	| valid |
	self do: [:elem | elem isValid ifTrue: [ valid := each ]].
	aCollection do: [:group | group isValid ifTrue: [ valid := each ]].
	^valid'.
	
	self 
		compile: sourceCode 
		andFind: (44 to: 100)
		to: (Message selector: #k1:k2: arguments: #('a' 'b')).

	self deny: finder hasFoundParameterizableReplacements.! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 23/Aug/2026, 10:21:17 pm (UTC)'!
test58_DoesNotLookForNodesThatAreNotInTheCompleteSourceRanges

	"This test ensures that replacing #notOptimizedMethodNode with #methodNode causes a failure.
	This checks that ReplacementsFinder uses #notOptimizedMethodNode"
	
	self shouldNotRaiseErrorOrFindAnyParameterReplacements: [
			self 
				compile: 'm1 
	self foo ifNil: [ ^1 ] ifNotNil: [ ^2 ]' 
				andFind: (6 to: 9) ]! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 23/Aug/2026, 10:16:41 pm (UTC)'!
test59_DoesNotUseOptimizedNodesForFinding

	"This test ensures that replacing #notOptimizedMethodNode with #methodNode causes a failure.
	This checks that NodesInRangeFinder uses #notOptimizedMethodNode"
	
	self shouldNotRaiseErrorOrFindAnyParameterReplacements: [
		self 
			compile: 'm1 false ifTrue: [ ^ 10 ]. ^ 20' 
			andFind: (4 to: 25) ].! !

!ExtractMethodFinderTest methodsFor: 'tests' stamp: 'JEC 24/Aug/2026, 10:05:40 pm (UTC)'!
test60_DoesNotFailsWhenNotHavingAMessage

	"This is an edge case. So far, the finder creates replacements without the Message (with the method name added by the user), because that step is now performed after determining whether the method will have parameters or not. As a result, during creation, there are some validations related to the method name that we need to skip for now, since they will be performed later."

	| sourceCode |
	
	sourceCode := 'processIfValid
	| subtotal |
	self validate ifFalse: [ ^ nil ].
	subtotal := self price.
	^ subtotal * self taxRate.'.
	
	self shouldNotRaiseErrorOrFindAnyParameterReplacements: [
		self 
			compile: sourceCode 
			andFind: (66 to: 115)
			to: nil ].
	
	! !

!ExtractMethodFinderTest methodsFor: 'setup/teardown' stamp: 'HAW 2/Sep/2021, 10:02:12 pm (UTC)'!
setUp

	super setUp.
	classToRefactor := self createClassNamed: self classToRefactorName! !

!ExtractMethodFinderTest methodsFor: 'class names' stamp: 'HAW 2/Sep/2021, 8:54:18 pm (UTC)'!
classToRefactorName
	
	^#ClassToExtractMethod! !

!ExtractMethodFinderTest methodsFor: 'class names' stamp: 'HAW 5/Sep/2021, 12:53:12 am (UTC)'!
subclassToRefactorName
	
	^#SubclassToExtractMethod! !

!ExtractMethodFinderTest methodsFor: 'assertions' stamp: 'jmv 31/Jul/2023, 3:40:00 pm (UTC)'!
assert: expected equals: actual
	"This tests assumes the order of arguments used by Cuis before July 31, 2023,
	and also used by Squeak."

	super assert: actual equals: expected! !

!ExtractMethodFinderTest methodsFor: 'assertions' stamp: 'JEC 14/Apr/2026, 9:20:28 pm (UTC)'!
assertAnyParamReplacementOf: paramReplacements hasInterval: anInterval hasParamsIntervals: aCollectionOfParamIntervals andIsOf: aSourceMethod

	self assert: (paramReplacements anySatisfy: [ :paramReplacement |
		(paramReplacement isIntervalToExtract: anInterval)
			and: [ (paramReplacement hasParameterizableIntervalsInOrder: aCollectionOfParamIntervals) 
			and: [ paramReplacement isOf: aSourceMethod ] ] ])! !

!ExtractMethodFinderTest methodsFor: 'assertions' stamp: 'JEC 14/Apr/2026, 9:15:56 pm (UTC)'!
assertAnyReplacementOf: replacements hasInterval: anExpectedInterval andIsOf: aSourceMethod 

	self 
		assertAnyReplacementOf: replacements 
		hasInterval: anExpectedInterval 
		isOf: aSourceMethod 
		andHasEqualParamsInOrder: #()
		withIntervals: #()! !

!ExtractMethodFinderTest methodsFor: 'assertions' stamp: 'JEC 14/Apr/2026, 9:14:34 pm (UTC)'!
assertAnyReplacementOf: replacements 
	hasInterval: anExpectedInterval 
	isOf: aSourceMethod 
	andHasEqualParamsInOrder: aCollectionOfParams
	withIntervals: aCollectionOfParamIntervals

	self assert: (replacements anySatisfy: [ :replacement |
		replacement asSourceCodeInterval = anExpectedInterval
			and: [ (replacement isOf: aSourceMethod)
			and: [ (replacement hasEqualParams: aCollectionOfParams)
			and: [ replacement areParameterizableNodesIntervals: aCollectionOfParamIntervals ] ] ] ])! !

!ExtractMethodFinderTest methodsFor: 'assertions' stamp: 'JEC 14/Apr/2026, 9:17:10 pm (UTC)'!
assertAnyReplacementOf: replacements isOfSourceMethodAndHasInterval: expectedInterval

	self 
		assertAnyReplacementOf: replacements 
		hasInterval: expectedInterval
		andIsOf: sourceMethod! !

!ExtractMethodFinderTest methodsFor: 'assertions' stamp: 'JEC 23/Aug/2026, 10:20:29 pm (UTC)'!
shouldNotRaiseErrorOrFindAnyParameterReplacements: aBlock

	self 
		shouldnt: aBlock 
		raise: Error. 

	self deny: finder hasFoundParameterizableReplacements.! !

!ExtractMethodFinderTest methodsFor: 'excercising' stamp: 'JEC 14/Mar/2026, 7:02:07 pm (UTC)'!
acceptParamReplacementsTo: aMessage

	self acceptParamReplacementsWithNodesToParameterized: #() to: aMessage! !

!ExtractMethodFinderTest methodsFor: 'excercising' stamp: 'JEC 14/Mar/2026, 4:46:28 pm (UTC)'!
acceptParamReplacementsWithNodesToParameterized: nodesToBeParameterized to: aMessage

	finder 
		useParameterizedToChangeReplacements;
		acceptParamReplacementsWithNodesToParameterized: nodesToBeParameterized
			to: aMessage.! !

!ExtractMethodFinderTest methodsFor: 'excercising' stamp: 'JEC 14/Mar/2026, 3:52:34 pm (UTC)'!
compile: aSourceCode andFind: anInterval

	self 
		compile: aSourceCode
		andFind: anInterval
		to: (Message selector: #m2)! !

!ExtractMethodFinderTest methodsFor: 'excercising' stamp: 'JEC 14/Mar/2026, 4:47:04 pm (UTC)'!
compile: aSourceCode andFind: anInterval to: aMessage

	| sourceMethodName |
	
	sourceMethodName := classToRefactor compile: aSourceCode.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	
	finder := ExtractMethodReplacementsFinder 
		ofCodeIn: anInterval asSourceCodeInterval 
		at: sourceMethod 
		to: aMessage. 

	finder value.! !

!ExtractMethodFinderTest methodsFor: 'excercising' stamp: 'JEC 14/Mar/2026, 10:33:53 pm (UTC)'!
tempVariableNodeNamed: aName

	^ (TempVariableNode named: aName) key: aName! !

!ExtractToTemporaryFinderTest methodsFor: 'assertions' stamp: 'JEC 17/Jan/2026, 12:22:24 am (UTC)'!
assertFinderIsEmpty

	self assert: finder replacements isEmpty! !

!ExtractToTemporaryFinderTest methodsFor: 'assertions' stamp: 'JEC 17/Jan/2026, 12:23:19 am (UTC)'!
assertFinderThrowsException

	self 
		should: [ finder value ] 
		raise:  Error
		withMessageText: NodesInRangeFinder nodesNotFoundErrorDescription.
		
	self assert: finder replacements isNil! !

!ExtractToTemporaryFinderTest methodsFor: 'class factory' stamp: 'JEC 16/Jan/2026, 12:57:35 pm (UTC)'!
classToRefactorName
	
	^#ClassToExtractMethod! !

!ExtractToTemporaryFinderTest methodsFor: 'excercising' stamp: 'JEC 17/Jan/2026, 12:19:44 am (UTC)'!
compile: sourceCode

	classToRefactor compile: sourceCode.
	sourceMethod := (classToRefactor >> #m1) asMethodReference.! !

!ExtractToTemporaryFinderTest methodsFor: 'excercising' stamp: 'JEC 17/Jan/2026, 12:22:00 am (UTC)'!
createFinderToFindInterval: anInterval

	finder := ExtractToTemporaryReplacementsFinder 
		named: 'newVar' 
		at: anInterval asSourceCodeInterval 
		from: sourceMethod. ! !

!ExtractToTemporaryFinderTest methodsFor: 'setup/teardown' stamp: 'JEC 16/Jan/2026, 12:57:25 pm (UTC)'!
setUp

	super setUp.
	classToRefactor := self createClassNamed: self classToRefactorName! !

!ExtractToTemporaryFinderTest methodsFor: 'tests' stamp: 'JEC 17/Jan/2026, 12:22:00 am (UTC)'!
test001_CannotFindIntervalsOfCodeInUnarySelector

	| methodsToExtractFrom |
	
	self compile: 'm1 1'.
	self createFinderToFindInterval: (4 to: 4).

	finder value.
	
	self assert: finder hasOneReplacement.
	methodsToExtractFrom := finder replacements.
	self assert: 1 equals: methodsToExtractFrom size.
	self assert: (4 to: 4) equals: methodsToExtractFrom first asSourceCodeInterval.! !

!ExtractToTemporaryFinderTest methodsFor: 'tests' stamp: 'JEC 17/Jan/2026, 12:22:14 am (UTC)'!
test002_CannotFindIntervalsOfCodeInKeywordSelector
	
	classToRefactor compile: 'm1: first m2: second first'.
	sourceMethod := (classToRefactor >> #m1:m2:) asMethodReference.
	self createFinderToFindInterval: (5 to: 9). 

	finder value.
	
	self assertFinderIsEmpty.! !

!ExtractToTemporaryFinderTest methodsFor: 'tests' stamp: 'JEC 17/Jan/2026, 12:22:00 am (UTC)'!
test003_CannotFindPartsOfOtherPiecesOfCode

	| methodsToExtractFrom |
	
	self compile: 'm1 1. 100'.
	self createFinderToFindInterval: (4 to: 4). 

	finder value.
	
	self assert: finder hasOneReplacement.
	methodsToExtractFrom := finder replacements.
	self assert: 1 equals: methodsToExtractFrom size.
	self assert: (4 to: 4) equals: methodsToExtractFrom first asSourceCodeInterval.! !

!ExtractToTemporaryFinderTest methodsFor: 'tests' stamp: 'JEC 17/Jan/2026, 12:22:38 am (UTC)'!
test004_CannotFindReturns
	
	self compile: 'm1 ^ 1'.
	self createFinderToFindInterval: (4 to: 6). 

	finder value.
	
	self assertFinderIsEmpty! !

!ExtractToTemporaryFinderTest methodsFor: 'tests' stamp: 'JEC 17/Jan/2026, 12:23:19 am (UTC)'!
test005_WhenTryingToFindCodeWithSintaxErrorsThrowsException
	
	self compile: 'm1 1+2'.
	self createFinderToFindInterval: (4 to: 5).

	self assertFinderThrowsException.! !

!ExtractToTemporaryFinderTest methodsFor: 'tests' stamp: 'JEC 17/Jan/2026, 12:22:48 am (UTC)'!
test006_CannotFindMoreThanOneStatement
	
	self compile: 'm1 1+2. 3+4'.
	self createFinderToFindInterval: (4 to: 11).

	finder value.
	
	self assertFinderIsEmpty.! !

!ExtractToTemporaryFinderTest methodsFor: 'tests' stamp: 'JEC 17/Jan/2026, 12:23:19 am (UTC)'!
test007_WhenTryingToFindPartOfAMessageSendThrowsException
	
	self compile: 'm1 ^ self m2'.
	self createFinderToFindInterval: (11 to: 12).

	self assertFinderThrowsException.! !

!ExtractToTemporaryFinderTest methodsFor: 'tests' stamp: 'JEC 17/Jan/2026, 12:22:57 am (UTC)'!
test008_CannotFindLeftSideOfAssignment

	self compile: 'm1 | a | a := 10'.
	self createFinderToFindInterval: (10 to: 10).

	finder value.
	
	self assertFinderIsEmpty.! !

!ExtractToTemporaryFinderTest methodsFor: 'tests' stamp: 'JEC 17/Jan/2026, 12:23:19 am (UTC)'!
test009_WhenTryingToFindEmptyCodeThrowsException

	self compile: 'm1  10'.
	self createFinderToFindInterval: (3 to: 4).

	self assertFinderThrowsException.! !

!ExtractToTemporaryFinderTest methodsFor: 'tests' stamp: 'JEC 17/Jan/2026, 12:25:37 am (UTC)'!
test010_CanFindAValidPieceOfCode

	| methodsToExtractFrom |

	self compile: 'm1 10'.
	self createFinderToFindInterval: (4 to: 5).
	
	finder value.

	self assert: finder hasOneReplacement.
	methodsToExtractFrom := finder replacements.
	self assert: 1 equals: methodsToExtractFrom size.
	self assert: (4 to: 5) equals: methodsToExtractFrom first asSourceCodeInterval.! !

!ParameterizableNodesFinderTest methodsFor: 'assertions' stamp: 'JEC 24/Mar/2026, 4:25:49 pm (UTC)'!
assert: parameterizableNodes areEquivalentToAll: expectedNodes parameterizing: nodesForParameterization

	parameterizableNodes first
		with: expectedNodes 
		do: [ :parameterizableNode :expectedNode | 
			self assert: (parameterizableNode equivalentTo: expectedNode key) ].
	
	expectedNodes do: [ :expectedNode | 
		| expectedNodesIncluded |
		
		"If there are multiple nodes, the code interval in the parameterizableNodes is the union of all node intervals"
		expectedNodesIncluded := (expectedNode value includesAllOf: parameterizableNodes second)
			or: (parameterizableNodes second includesAllOf: expectedNode value).

		self assert: expectedNodesIncluded ].
	
	self assert: parameterizableNodes third size equals: nodesForParameterization size.
	
	parameterizableNodes third 
		with: nodesForParameterization 
		do: [ :foundParamNode :expectedParamNode |
			self assert: (foundParamNode key equivalentTo: expectedParamNode key).
			
			"Some nodes have a difference of an space in their range, because of the space"
			self assert: (foundParamNode value includesAllOf: expectedParamNode value) ]! !

!ParameterizableNodesFinderTest methodsFor: 'assertions' stamp: 'JEC 24/Mar/2026, 4:25:28 pm (UTC)'!
assert: parameterizableNode isEquivalentTo: expectedNodes parameterizing: nodeForParameterization
	
	self 
		assert: parameterizableNode 
		areEquivalentToAll: { expectedNodes } 
		parameterizing: nodeForParameterization! !

!ParameterizableNodesFinderTest methodsFor: 'assertions' stamp: 'JEC 27/Jan/2026, 9:42:29 pm (UTC)'!
assertHasOneParameterizableNode: parameterizableNodes

	^ self assert: parameterizableNodes size equals: 1! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Mar/2026, 10:42:54 pm (UTC)'!
test001_CannotFindAsParameterizableTheCodeUsedToStartTheSearch

	| nodeToFind |

	self compile: 'm1 self foo: 1.'.
	nodeToFind := self parseNodeAt: 9.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Mar/2026, 10:47:35 pm (UTC)'!
test002_CannotFindParameterizableNodesWithEquivalentMessageNodes

	| nodeToFind |

	self compile: 'm1 self foo: 1. self foo: 1'.
	nodeToFind := self parseNodeAt: 9.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 24/Mar/2026, 4:26:16 pm (UTC)'!
test003_CanFindParameterizableNodesFromMessageNodesWithOneKeyword

	| nodeToFind expectedNode parameterizableNodes nodesForParameterization |

	self compile: 'm1 self foo: 1. self foo: 2'.
	nodeToFind := self parseNodeAt: 9.
	expectedNode := self parseNodeAt: 22.
	nodesForParameterization := { self parseNodeAt: 27 }.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assertHasOneParameterizableNode: parameterizableNodes.
	self 
		assert: parameterizableNodes first 
		isEquivalentTo: expectedNode 
		parameterizing: nodesForParameterization.! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 24/Mar/2026, 4:26:35 pm (UTC)'!
test004_CanFindParameterizableNodesFromParameterizableNodes

	| nodeToFind expectedNode parameterizableNodes nodeForParameterization anotherNodeForParameterization |

	self compile: 'm1 self foo: (self foo: 1). self foo: 2'.
	nodeToFind := self parseNodeAt: 34.
	expectedNode := self parseNodeAt: 9.
	nodeForParameterization := self parseNodeAt: 20.
	anotherNodeForParameterization := self parseNodeAt: 25.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assert: parameterizableNodes size equals: 2.
	self 
		assert: parameterizableNodes first 
		isEquivalentTo: expectedNode 
		parameterizing: { nodeForParameterization }.
	
	self 
		assert: parameterizableNodes second 
		isEquivalentTo: nodeForParameterization
		parameterizing: { anotherNodeForParameterization }.! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 24/Mar/2026, 4:26:40 pm (UTC)'!
test005_CanRecursivelyFindParameterizableNodes

	| nodeToFind expectedNode parameterizableNodes nodesForParameterization |

	self compile: 'm1 self bar: (self foo: 1). self foo: 2'.
	nodeToFind := self parseNodeAt: 34.
	expectedNode := self parseNodeAt: 20.
	nodesForParameterization := { self parseNodeAt: 25 }.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assertHasOneParameterizableNode: parameterizableNodes.
	self 
		assert: parameterizableNodes first 
		isEquivalentTo: expectedNode 
		parameterizing: nodesForParameterization! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 24/Mar/2026, 4:26:45 pm (UTC)'!
test006_MethodNodesWithMultipleKeywordsCanBeFoundAsParameterizable

	| nodeToFind expectedNode parameterizableNodes nodesForParameterization |

	self compile: 'm1 self foo: (self f: 10) bar: 10. self foo: 2 bar: 10'.
	nodeToFind := self parseNodeAt: 41.
	expectedNode := self parseNodeAt: 9.
	nodesForParameterization := { self parseNodeAt: 20 }.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assertHasOneParameterizableNode: parameterizableNodes.
	self 
		assert: parameterizableNodes first 
		isEquivalentTo: expectedNode 
		parameterizing: nodesForParameterization! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 24/Mar/2026, 4:26:49 pm (UTC)'!
test007_MethodNodesWithMultipleKeywordsCanBeFoundAsParameterizable

	| nodeToFind expectedNode parameterizableNodes nodesForParameterization |

	self compile: 'm1 self foo: 2 bar: 10. self foo: 2 bar: (self f: 10)'.
	nodeToFind := self parseNodeAt: 30.
	expectedNode := self parseNodeAt: 9.
	nodesForParameterization := { self parseNodeAt: 21 }.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assertHasOneParameterizableNode: parameterizableNodes.
	self 
		assert: parameterizableNodes first 
		isEquivalentTo: expectedNode 
		parameterizing: nodesForParameterization! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Mar/2026, 10:55:48 pm (UTC)'!
test008_CannotFindParameterizableNodesFromEquivalentAssignmentNodes

	| nodeToFind |

	self compile: 'm1 | a b | a := self foo: 10. b := self foo: 10'.
	nodeToFind := self parseNodeAt: 33.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 24/Mar/2026, 4:26:56 pm (UTC)'!
test009_AssignmentNodesAreFoundAsParameterizableWhenTheyHaveParameterizableMethodNodes

	| nodeToFind expectedNode parameterizableNodes nodesForParameterization |

	self compile: 'm1 | a b | a := self foo: 10. b := self foo: 20'.
	nodeToFind := self parseNodeAt: 33.
	expectedNode := self parseNodeAt: 14.
	nodesForParameterization := { self parseNodeAt: 27 }.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assertHasOneParameterizableNode: parameterizableNodes.
	self 
		assert: parameterizableNodes first 
		isEquivalentTo: expectedNode 
		parameterizing: nodesForParameterization! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Mar/2026, 10:58:46 pm (UTC)'!
test010_CannotFindParameterizableNodesFromLiteralNodes

	| nodeToFind |

	self compile: 'm1 10. 20'.
	nodeToFind := self parseNodeAt: 4.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 12:16:00 am (UTC)'!
test011_CannotFindParameterizableNodesFromAVariableNode

	| nodeToFind |

	self compile: 'm1 | a b | a. b'.
	nodeToFind := self parseNodeAt: 12.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Mar/2026, 10:59:30 pm (UTC)'!
test012_CannotFindParameterizablesNodesFromArgumentsOfBinaryMessages

	| nodeToFind |

	self compile: 'm1 1 + 1 . 1 + 2'.
	nodeToFind := self parseNodeAt: 6.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Mar/2026, 11:00:21 pm (UTC)'!
test013_CannotFindTheReceiverOfABinaryMessageAsParameterizable

	| nodeToFind expectedNode nodesForParameterization |

	self compile: 'm1 2 + 1 . self foo + 1'.
	nodeToFind := self parseNodeAt: 17.
	expectedNode := self parseNodeAt: 6.
	nodesForParameterization := { self parseNodeAt: 4 }.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Mar/2026, 11:08:09 pm (UTC)'!
test015_CannotFindParameterizableNodesWhenThereAreMultipleEquivalentCodePieces

	"This is an edge case that was throwing an unexpected exception"

	| nodesToFind |

	self compile: 'm1 10 factorial. 10 factorial. true ifTrue: [ 10 factorial. 10 factorial ]'.
	nodesToFind := self parseNodesAtAll: { 7 . 21 }.
	
	self findParameterizableNodesInSameMethodFromAll: nodesToFind.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 24/Mar/2026, 4:27:06 pm (UTC)'!
test016_CanFindAParameterizableNodeWhenIsRepeatedInTheCode

	| nodeToFind expectedNode nodesForParameterization parameterizableNodes |

	self compile: 'm1 self foo: 10. self foo: self'.
	nodeToFind := self parseNodeAt: 9.
	expectedNode := self parseNodeAt: 23.
	nodesForParameterization := { self parseNodeAt: 28 }.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assertHasOneParameterizableNode: parameterizableNodes.
	self 
		assert: parameterizableNodes first 
		isEquivalentTo: expectedNode 
		parameterizing: nodesForParameterization! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 24/Mar/2026, 4:27:34 pm (UTC)'!
test017_CanFindAListOfConsecutiveParameterizableNodes_WhenHavingARepeatedArgument

	| nodesToFind expectedNodes nodesForParameterization parameterizableNodes |

	self compile: 'm1 self foo: 1. self foo: 2. self foo: 1. self foo: 1'.
	nodesToFind := self parseNodesAtAll: { 9 . 22 }.
	expectedNodes := self parseNodesAtAll: { 35 . 48 }.
	nodesForParameterization := { self parseNodeAt: 53 }.
	
	self findParameterizableNodesInSameMethodFromAll: nodesToFind.

	parameterizableNodes := finder parameterizableNodes.
	self assertHasOneParameterizableNode: parameterizableNodes.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: expectedNodes 
		parameterizing: nodesForParameterization! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 29/Mar/2026, 11:44:29 pm (UTC)'!
test018_CanFindParameterizableNodesInADifferentMethodThanTheOneThatWasBeingExtracted

	| nodesToFind expectedNodes nodesForParameterization parameterizableNodes |

	self compile: 'm1 self foo: 1. self foo: 2. self foo: 1. self foo: 1'.
	nodesToFind := self parseNodesAtAll: { 9 . 22 }.
	expectedNodes := self parseNodesAtAll: { 22 . 35 }.
	nodesForParameterization := self parseNodesAtAll: { 27 . 40 }.
	
	self findParameterizableNodesNotInSameMethodFromAll: nodesToFind.
	
	parameterizableNodes := finder parameterizableNodes.
	self assertHasOneParameterizableNode: parameterizableNodes.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: expectedNodes 
		parameterizing: nodesForParameterization! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 30/Mar/2026, 2:40:47 am (UTC)'!
test019_WhenSearchingMultipleParameterizableNodes_WillKeepTheFirstThatCanBeParameterizable

	"This tests shows that extracting the first
	self foo: 10.
	self foo: 10.
	will find as parameterizable
	self foo: 10.
	self foo: 'a'.
	This is an expected behaviour because the NodeFinder does not have more information about what to extract"

	| nodesToFind expectedNodes nodesForParameterization parameterizableNodes |

	self compile: 'm1 
		self foo: 10. 
		self foo: 10. 
		
		self foo: 10. 
		self foo: 10.
		
		self foo: ''a''.
		self foo: ''b''.'.
	nodesToFind := self parseNodesAtAll: { 12 . 29 }.
	expectedNodes := self parseNodesAtAll: { 66 . 85 }.
	nodesForParameterization := { self parseNodeAt: 90 }.
	
	self findParameterizableNodesInSameMethodFromAll: nodesToFind.
	
	parameterizableNodes := finder parameterizableNodes.
	self assertHasOneParameterizableNode: parameterizableNodes.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: expectedNodes 
		parameterizing: nodesForParameterization! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 30/Mar/2026, 2:40:54 am (UTC)'!
test020_WhenSearchingMultipleParameterizableNodes_WithRejectedIntervals_SkipsThemAndContinuesSearching

	| nodesToFind expectedNodes nodesForParameterization parameterizableNodes |

	self compile: 'm1 
		self foo: 10. 
		self foo: 10. 
		
		self foo: 10. 
		self foo: 10.
		
		self foo: ''a''.
		self foo: ''b''.'.
	nodesToFind := self parseNodesAtAll: { 12 . 29 }.
	expectedNodes := self parseNodesAtAll: { 85 . 102 }.
	nodesForParameterization := { self parseNodeAt: 90 . self parseNodeAt: 107 }.
	
	self 
		findParameterizableNodesInSameMethodFromAll: nodesToFind
		rejectingIntervals: { 44 to: 72 }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assertHasOneParameterizableNode: parameterizableNodes.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: expectedNodes 
		parameterizing: nodesForParameterization! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 30/Mar/2026, 12:19:29 am (UTC)'!
test021_CanHaveMultipleRejectedIntervals

	| nodesToFind |

	self compile: 'm1 
		self foo: 10. 
		self foo: 10. 
		
		self foo: 10. 
		self foo: 10.
		
		self foo: ''a''.
		self foo: ''b''.'.
	nodesToFind := self parseNodesAtAll: { 12 . 29 }.
	
	self 
		findParameterizableNodesInSameMethodFromAll: nodesToFind
		rejectingIntervals: { 44 to: 72 . 80 to: 92 }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 30/Mar/2026, 12:59:27 am (UTC)'!
test022_CanHaveMultipleParameterizableNodes

	| nodesToFind parameterizableNodes |

	self compile: 'm1 
		self foo: 10. 
		self foo: 10. 
		
		self foo: 2. 
		self foo: 3.
		
		self foo: ''a''.
		self foo: ''b''.'.
	nodesToFind := self parseNodesAtAll: { 12 . 29 }.
	
	self findParameterizableNodesInSameMethodFromAll: nodesToFind.
	
	parameterizableNodes := finder parameterizableNodes.
	self assert: parameterizableNodes size equals: 2.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: (self parseNodesAtAll: { 49 . 65 }) 
		parameterizing: { self parseNodeAt: 54 . self parseNodeAt: 70 }.
	self 
		assert: parameterizableNodes second 
		areEquivalentToAll: (self parseNodesAtAll: { 83 . 100 }) 
		parameterizing: { self parseNodeAt: 89 . self parseNodeAt: 105 }.! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 30/Mar/2026, 2:41:44 am (UTC)'!
test023_CanFindParameterizableCode_WhenSearchingWithRejectedIntervals

	| parameterizableNodes |

	self compile: 'm1 self foo: (self foo: 1). self foo: 2'.
	
	self 
		findParameterizableNodesInSameMethodFromAll: { self parseNodeAt: 9 }
		rejectingIntervals: { 4 to: 26 }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assert: parameterizableNodes size equals: 1.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: { self parseNodeAt: 34 }
		parameterizing: { self parseNodeAt: 39 }! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 7/Apr/2026, 8:16:38 pm (UTC)'!
test024_WhenTheReceiverIsATempVarCanBeParameterizable

	| parameterizableNodes |

	self 
		compile: 'm1: aCollection
	| total product |
	total := aCollection inject: 0 into: [:acc :each | acc + each].
	product := aCollection inject: 1 into: [:acc :each | acc * each].
	^total printString , '' - '' , product printString'
		withSelector: #m1:.
	
	self findParameterizableNodesInSameMethodFromAll: { self parseNodeAt: 58 }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assert: parameterizableNodes size equals: 1.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: { self parseNodeAt: 125 }
		parameterizing: { self parseNodeAt: 113 . self parseNodeAt: 133 . self parseNodeAt: 141 }! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 7/Apr/2026, 8:17:15 pm (UTC)'!
test025_WhenReceiversAreInstVars_WontBeParameterized

	| parameterizableNodes |

	classToRefactor addInstVarName: 'instVarCollection'.
	self compile: 'm1
	| total product |
	total := instVarCollection inject: 0 into: [:acc :each | acc + each].
	product := instVarCollection inject: 1 into: [:acc :each | acc * each].
	^total printString , '' - '' , product printString'.
	
	self findParameterizableNodesInSameMethodFromAll: { self parseNodeAt: 51 }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assert: parameterizableNodes size equals: 1.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: { self parseNodeAt: 124 }
		parameterizing: { self parseNodeAt: 132 . self parseNodeAt: 140 }! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 22/Aug/2026, 7:41:01 pm (UTC)'!
test026_WhenOneReceiverIsATempVarAndOthersAreDifferentKindOfVariables_AllWillBeParameterized

	| parameterizableNodes |

	classToRefactor addInstVarName: 'instVarCollection'.
	self compile: 'm1
	| a b c aCollection |
	aCollection := OrderedCollection new.
	a := aCollection inject: 0 into: [:acc :each | acc + each].
	b := instVarCollection inject: 1 into: [:acc :each | acc * each].
	c := self inject: 1 into: [:acc :each | acc ** each].
	^ a + b + c'.
	
	self findParameterizableNodesInSameMethodFromAll: { self parseNodeAt: 88 }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assert: parameterizableNodes size equals: 2.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: { self parseNodeAt: 153 }
		parameterizing: { self parseNodeAt: 135 . self parseNodeAt: 159 . self parseNodeAt: 168 }.
	self 
		assert: parameterizableNodes second 
		areEquivalentToAll: { self parseNodeAt: 207 }
		parameterizing: { self parseNodeAt: 202 . self parseNodeAt: 213 . self parseNodeAt: 221 }! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 7/Apr/2026, 8:19:10 pm (UTC)'!
test027_WhenOneReceiversArePseudoVariablesAndInstVars_WontBeParameterized

	| parameterizableNodes |

	classToRefactor addInstVarName: 'instVarCollection'.
	self compile: 'm1
	| a b |
	a := self inject: 0 into: [:acc :each | acc + each].
	b := instVarCollection inject: 1 into: [:acc :each | acc * each].
	^a + b'.
	
	self findParameterizableNodesInSameMethodFromAll: { self parseNodeAt: 24 }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assert: parameterizableNodes size equals: 1.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: { self parseNodeAt: 91 }
		parameterizing: { self parseNodeAt: 99 . self parseNodeAt: 107 }! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 7/Apr/2026, 8:20:24 pm (UTC)'!
test028_WhenReceiversAreNonVariablesOrLiterals_TheyWontBeFoundAsParameterizable

	self compile: 'm1
	| a b c aCollection |
	aCollection := OrderedCollection new.
	a := aCollection inject: 0 into: [:acc :each | acc + each].
	b := (OrderedCollection new add: 10; yourself) inject: 1 into: [:acc :each | acc * each].
	c := #() inject: 1 into: [:acc :each | acc - each].
	^a + b + c'.
	
	self findParameterizableNodesInSameMethodFromAll: { self parseNodeAt: 84 }.
	
	self deny: finder hasFoundParameterizableReplacements.! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 7/Apr/2026, 8:21:39 pm (UTC)'!
test029_WhenFindingMultiplesPiecesOfCode_AndReceiversAreNotEquivalents_TheyWontBeFoundAsParameterizable

	classToRefactor addInstVarName: 'instVarCollection'.
	self compile: 'm1
	| a b c d aCollection |
	aCollection := OrderedCollection new.
	a := aCollection inject: 0 into: [:acc :each | acc + each].
	b := instVarCollection inject: 1 into: [:acc :each | acc * each].
	c := self inject: 1 into: [:acc :each | acc ^ each].
	d := undefinedVar inject: 1 into: [:acc :each | acc - each].
	^ a + b + c + d'.
	
	self findParameterizableNodesInSameMethodFromAll: { self parseNodeAt: 71 . self parseNodeAt: 132 }.
	
	self deny: finder hasFoundParameterizableReplacements.! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 7/Apr/2026, 8:30:02 pm (UTC)'!
test030_WhenFindingMultiplesPiecesOfCode_WhenReceiversAreEquivalentsCanBeFoundAsParameterizable

	| parameterizableNodes |

	classToRefactor addInstVarName: 'instVarCollection'.
	self compile: 'm1
	| a b c d aCollection |
	aCollection := OrderedCollection new.
	a := aCollection inject: 0 into: [:acc :each | acc + each].
	b := instVarCollection inject: 1 into: [:acc :each | acc * each].
	c := aCollection inject: 1 into: [:acc :each | acc ^ each].
	d := instVarCollection inject: 1 into: [:acc :each | acc - each].
	^ a + b + c + d'.
	
	self findParameterizableNodesInSameMethodFromAll: { self parseNodeAt: 71 . self parseNodeAt: 132 }.
	
	parameterizableNodes := finder parameterizableNodes.
	self assert: parameterizableNodes size equals: 1.
	self 
		assert: parameterizableNodes first 
		areEquivalentToAll: { self parseNodeAt: 199 . self parseNodeAt: 260 }
		parameterizing: { self parseNodeAt: 202 . 
			self parseNodeAt: 222 . 
			self parseNodeAt: 230 . 
			self parseNodeAt: 297 }! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 12:09:40 am (UTC)'!
test031_CannotFindAsParameterizableSameUniaryMessages

	| nodeToFind |

	self compile: 'm1 self foo. self foo'.
	nodeToFind := self parseNodeAt: 9.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 12:09:45 am (UTC)'!
test032_CannotFindAsParameterizableUniaryMessages

	| nodeToFind |

	self compile: 'm1 self foo. self bar'.
	nodeToFind := self parseNodeAt: 9.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 12:09:12 am (UTC)'!
test033_CannotFindParameterizableCodeWhenABinaryMessageIsTheReceiver

	| nodeToFind |

	self compile: 'm1 
	| a b |
	a:= 1.
	b := 1.
	(a + b) inject: 0 into: [:acc :each | acc + each].
	(a + b) inject: 1 into: [:acc :each | acc + each].'.
	nodeToFind := self parseNodeAt: 40.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 15/Apr/2026, 12:09:08 am (UTC)'!
test034_CannotFindParameterizableCodeFromACascadeCode

	| nodeToFind |

	self compile: 'm1 
	| aCollection |
	aCollection := OrderedCollection new.
	aCollection
		add: 1;
		add: 2.
	aCollection
		add: 3;
		add: 2.'.
	nodeToFind := self parseNodeAt: 82.
	
	self findParameterizableNodesInSameMethodFromAll: { nodeToFind }.
	
	self deny: finder hasFoundParameterizableReplacements! !

!ParameterizableNodesFinderTest methodsFor: 'tests' stamp: 'JEC 22/Aug/2026, 7:07:36 pm (UTC)'!
test035_WhenReceiverIsALiteralVariable_ItWontBeFoundAsParameterizable

	self compile: 'm1
	| a b c aCollection |
	aCollection := OrderedCollection new.
	a := Smalltalk valueWithArguments: #(1 2 3).
	b := Smalltalk valueWithArguments: {5}.
	^a + b'.
	
	self findParameterizableNodesInSameMethodFromAll: { self parseNodeAt: 81 }.
	
	self deny: finder hasFoundParameterizableReplacements.! !

!ParameterizableNodesFinderTest methodsFor: 'exercising' stamp: 'JEC 6/Apr/2026, 4:12:03 pm (UTC)'!
compile: aSourceCode

	self compile: aSourceCode withSelector: #m1! !

!ParameterizableNodesFinderTest methodsFor: 'exercising' stamp: 'JEC 6/Apr/2026, 4:11:46 pm (UTC)'!
compile: aSourceCode withSelector: aSelector

	classToRefactor compile: aSourceCode.
	methodNode := (classToRefactor>>aSelector) methodNode.! !

!ParameterizableNodesFinderTest methodsFor: 'exercising' stamp: 'JEC 29/Mar/2026, 11:51:59 pm (UTC)'!
findParameterizableNodesFromAll: nodesToLookFor withSelection: aSelection rejectingIntervals: rejectingIntervals

	finder := ParameterizableNodesFinder 
		for: (nodesToLookFor collect: [ :node | node key ])
		in: methodNode 
		keepingSelection: aSelection
		rejectingIntervals: rejectingIntervals.
	methodNode accept: finder.! !

!ParameterizableNodesFinderTest methodsFor: 'exercising' stamp: 'JEC 29/Mar/2026, 11:50:39 pm (UTC)'!
findParameterizableNodesInSameMethodFromAll: nodesToLookFor

	self 
		findParameterizableNodesInSameMethodFromAll: nodesToLookFor 
		rejectingIntervals: #() ! !

!ParameterizableNodesFinderTest methodsFor: 'exercising' stamp: 'JEC 29/Mar/2026, 11:50:14 pm (UTC)'!
findParameterizableNodesInSameMethodFromAll: nodesToLookFor rejectingIntervals: rejectingIntervals 

	"The only way to know if is looking for parameterizable nodes in the method that the code is being extracted is
	having a selection interval"

	| aSelectionToKeep |
	 
	aSelectionToKeep := nodesToLookFor first value first to: nodesToLookFor last value last.

	self 
		findParameterizableNodesFromAll: nodesToLookFor 
		withSelection: aSelectionToKeep
		rejectingIntervals: rejectingIntervals.! !

!ParameterizableNodesFinderTest methodsFor: 'exercising' stamp: 'JEC 29/Mar/2026, 11:55:58 pm (UTC)'!
findParameterizableNodesNotInSameMethodFromAll: nodesToLookFor

	"The only way to know if is looking for parameterizable nodes in the method that the code is being extracted is
	having a selection interval"

	self 
		findParameterizableNodesFromAll: nodesToLookFor 
		withSelection: NodesFinder noSelection
		rejectingIntervals: #()! !

!ParameterizableNodesFinderTest methodsFor: 'exercising' stamp: 'JEC 15/Mar/2026, 11:28:17 pm (UTC)'!
parseNodeAt: aPosition 

	| nodesAtPosition |

	nodesAtPosition := methodNode 
		parseNodesPathAt: aPosition 
		ifAbsent: [ self shouldNotHappen ].
	
	^ nodesAtPosition first! !

!ParameterizableNodesFinderTest methodsFor: 'exercising' stamp: 'JEC 15/Mar/2026, 11:07:02 pm (UTC)'!
parseNodesAtAll: aCollectionOfPositions 

	^ aCollectionOfPositions collect: [ :aPosition | self parseNodeAt: aPosition ]! !

!ParameterizableNodesFinderTest methodsFor: 'setup/teardown' stamp: 'JEC 27/Jan/2026, 9:41:02 pm (UTC)'!
classToRefactorName
	
	^#ClassToExtractMethod! !

!ParameterizableNodesFinderTest methodsFor: 'setup/teardown' stamp: 'JEC 27/Jan/2026, 9:41:13 pm (UTC)'!
setUp

	super setUp.
	classToRefactor := self createClassNamed: self classToRefactorName! !

!RefactoringTest methodsFor: 'assertions' stamp: 'jmv 31/Jul/2023, 3:39:13 pm (UTC)'!
assert: expected equals: actual
	"This tests assumes the order of arguments used by Cuis before July 31, 2023,
	and also used by Squeak."

	super assert: actual equals: expected! !

!RefactoringTest methodsFor: 'assertions' stamp: 'jmv 20/Mar/2024, 7:59:45 pm (UTC)'!
assertCreation: aCreationBlock failsWith: aStringOrBlock

	self 
		should: aCreationBlock 
		raise: self refactoringError
		withMessageText: aStringOrBlock ! !

!RefactoringTest methodsFor: 'assertions' stamp: 'jmv 20/Mar/2024, 10:23:28 pm (UTC)'!
assertCreation: aCreationBlock warnsWith: aStringOrBlock

	| warningSignaled |
	
	warningSignaled := false.
	aCreationBlock 
		on: self refactoringWarning
		do: [ :aWarning |
			self assert: aStringOrBlock value equals: aWarning messageText.
			warningSignaled := true.
			aWarning resume ].

	self assert: warningSignaled description: 'Warning was not signaled'! !

!RefactoringTest methodsFor: 'exceptions' stamp: 'HAW 17/Aug/2018, 8:38:44 pm (UTC)'!
canNotRefactorDueToReferencesRefactoringError
	
	^ Refactoring canNotRefactorDueToReferencesErrorClass - MessageNotUnderstood.! !

!RefactoringTest methodsFor: 'exceptions' stamp: 'HAW 3/Jun/2017, 3:58:26 pm (UTC)'!
refactoringError

	^ Refactoring refactoringErrorClass - MessageNotUnderstood.! !

!RefactoringTest methodsFor: 'exceptions' stamp: 'HAW 3/Jun/2017, 4:05:52 pm (UTC)'!
refactoringWarning

	^ Refactoring refactoringWarningClass - MessageNotUnderstood.! !

!RefactoringTest methodsFor: 'exceptions' stamp: 'HAW 18/Dec/2019, 8:02:50 pm (UTC)'!
referencesRefactoringWarning

	^ Refactoring referencesWarningClass - MessageNotUnderstood.! !

!AddAsSubclassResponsabilityTest methodsFor: 'tests' stamp: 'jmv 26/Nov/2024, 4:06:20 pm (UTC)'!
test01AMethodOnClassToRefactorShouldBeAddedAsSubclassResponsibilityInSuperclass

	| classToRefactorSuperclass classToRefactor selector push methodCategory |

	selector := #newMethod.
   
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	methodCategory := 'xyz'.
	classToRefactor compile: selector asString classified: methodCategory. 
           
	push := AddAsSubclassResponsibility for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self assert: (classToRefactor includesSelector: selector).
	self assert: methodCategory equals: (classToRefactorSuperclass organization categoryOfElement: selector)! !

!AddAsSubclassResponsabilityTest methodsFor: 'tests' stamp: 'jmv 26/Nov/2024, 4:06:30 pm (UTC)'!
test02ShouldWarnWhenMethodWithSameNameExistsInSuperclass
	
	| classToRefactor classToRefactorSuperclass selector |
	
	selector := #newMethod.
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: selector, '^ 1 '.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector asString.

	self 
		assertCreation: [ AddAsSubclassResponsibility for: classToRefactor >> selector ]
		warnsWith: [ AddAsSubclassResponsibility methodImplementedInSuperclassWarningMessage ].
		
	! !

!AddAsSubclassResponsabilityTest methodsFor: 'tests' stamp: 'jmv 26/Nov/2024, 4:06:35 pm (UTC)'!
test03ShouldNotWarnWhenMethodInSuperclassIsAbstract
	
	| classToRefactor classToRefactorSuperclass selector |
	
	selector := #newMethod.
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: selector, ' self ', #subclassResponsibility.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector.

	self 
		shouldnt: [ AddAsSubclassResponsibility for: classToRefactor >> selector ]
		raise: self refactoringWarning 
	! !

!AddAsSubclassResponsabilityTest methodsFor: 'class factory' stamp: 'jmv 26/Nov/2024, 4:06:08 pm (UTC)'!
classToRefactorName	

	^#ClassToAddMethodInSuperClassAsSubclassResponsibility! !

!AddInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 11/Jun/2017, 10:12:55 pm (UTC)'!
classToRefactorName

	^#ClassToAddInstVar! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:42:17 pm (UTC)'!
test01NewVariableNameCanNotBeEmpty
	
	newVariable := '   '.
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: self class ]
		failsWith: [NewInstanceVariablePrecondition newVariableCanNotBeEmptyErrorMessage ]
! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:42:21 pm (UTC)'!
test02NewVariableNameCanNotContainBlanks
	
	newVariable := 'a b'.
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: self class ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable ]

	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:42:25 pm (UTC)'!
test03NewVariableNameCanNotContainInvalidCharacters

	newVariable := '2a'.
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: self class ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable ]

! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 11/Jun/2017, 10:38:49 pm (UTC)'!
test04ValidNewVariableNameGetBlanksTrimmed

	newVariable := ' a '.
			
	self assert: newVariable withBlanksTrimmed equals: (AddInstanceVariable named: newVariable to: self class) newVariable! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:42:31 pm (UTC)'!
test05NewVariableNameCanNotBeDefinedInClass

	| classToRefactor |
	
	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	
	self 
		assertCreation: [AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactor )]
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:42:43 pm (UTC)'!
test06NewVariableNameCanNotBeDefinedInSuperclasses

	|  classToRefactorSuperclass classToRefactor  |

	newVariable := 'a'.
	classToRefactorSuperclass := self createClassNamed: #SuperclassWithInstVar instanceVariableNames: newVariable.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	
	self 
		assertCreation: [AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSuperclass)]
! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:42:47 pm (UTC)'!
test07NewVariableNameCanNotBeDefinedInAnySubclass

	| classToRefactor classToRefactorSubclass |
	
	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass := self createClassNamed: #SubclassWithInstVar subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	self 
		assertCreation: [AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSubclass)]
	
		! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 4:33:14 pm (UTC)'!
test08AddCreatesNewInstanceVariable

	| classToRefactor add | 
	
	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.

	add := AddInstanceVariable named: newVariable to: classToRefactor.
	add apply.
	
	self assert: (classToRefactor definesInstanceVariableNamed: newVariable).
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/Mar/2019, 2:43:31 am (UTC)'!
test09FailsWhenNewVariableEqualsTemporaryVariableOfAMethodInClass

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1.
	classToRefactor compile: selector, ' | ', newVariable, ' | ', newVariable, ' := 10.'.
	
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/Mar/2019, 2:43:45 am (UTC)'!
test10FailsWhenNewVariableEqualsArgumentOfAMethodInClass

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1:.
	classToRefactor compile: selector, newVariable.
	
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/Mar/2019, 2:44:12 am (UTC)'!
test11FailsWhenNewVariableEqualsArgumentOfABlockInAMethodInClass

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1.
	classToRefactor compile: selector, ' [ :', newVariable, ' | ] value: 1'.
		
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/Mar/2019, 2:44:28 am (UTC)'!
test12FailsWhenNewVariableEqualsTemporaryOfABlockInAMethodInClass

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1.
	classToRefactor compile: selector, ' [ | ', newVariable, ' | ] value'.
		
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/Mar/2019, 2:44:46 am (UTC)'!
test13FailsWhenNewVariableIsHiddenInAnyMethodOfAnySubclass

	| selector classToRefactor subclass |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	subclass := self createClassNamed: #SubclassOfClassToRefactor subclassOf: classToRefactor.
	selector := #m1.
	subclass compile: selector, ' | ', newVariable, ' | '.
		
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (subclass>>selector)) ].
	
	! !

!AddInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:43:17 pm (UTC)'!
test14NewVariableNameCanNotBeAReservedName

	| classToRefactor |

	classToRefactor := self createClassNamed: self classToRefactorName.
	
	ClassBuilder reservedNames do: [ :aReservedName | 
		self 
			assertCreation: [AddInstanceVariable named: aReservedName to: classToRefactor ]
			failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariableCanNotBeAReservedName: aReservedName ]]
		! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:44:32 pm (UTC)'!
test01AddingParameterToUnaryMessageAddsColonAndParameter

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue equals: (senderMethod methodNode block statements first arguments first literalValue) printString


! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:44:49 pm (UTC)'!
test02AddingParameterToKeywordMessageAddsNewKeywordAndParameter

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, ' 1'.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)


! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:44:55 pm (UTC)'!
test03AddingParameterToKeywordMessageMultipleSendsInSameMethodSameLiteral

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' 1.'.
	classToRefactor compile: senderSelector asString, collaboration, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)


! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:45:02 pm (UTC)'!
test04AddingParameterToKeywordMessageWithMessageSendAsLastFormalParameter

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' self size.'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)


! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:45:09 pm (UTC)'!
test05AddingParameterToKeywordMessageWithBinaryMessageSendAsLastFormalParameter

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' self size + 4.'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)


! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:45:13 pm (UTC)'!
test06AddingParameterToKeywordMessageWithKeywordMessageSendAsLastFormalParameter

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' self ', oldSelector asString, ' (self at: 1 put: 2).'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue)


! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 17/Dec/2019, 11:12:33 pm (UTC)'!
test07AddingParameterToUnaryMessageChangesReferencesToSelector

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' #', oldSelector asString, ' size'.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: 0 equals: (senderMethod indexOfLiteral: oldSelector).
	self assert: 1 equals: (senderMethod indexOfLiteral: newSelector).
	
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 17/Dec/2019, 11:12:33 pm (UTC)'!
test08AddingParameterToKeywordMessageChangesReferencesToSelector

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword collaboration |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: (oldSelector asString, ' oldParam').
	collaboration :=  ' #', oldSelector asString, ' size.'.
	classToRefactor compile: senderSelector asString, collaboration.
	
	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode hasArgumentOrTemporaryNamed: newParameter).
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: 0 equals: (senderMethod indexOfLiteral: oldSelector).
	self assert: 1 equals: (senderMethod indexOfLiteral: newSelector).
	

! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 17/Aug/2018, 4:24:53 pm (UTC)'!
test09FailsIfNewParameterValueCanNotBeCompiled

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '+' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter newParameterValueDoesNotCompileErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 17/Aug/2018, 4:25:16 pm (UTC)'!
test10FailsIfNewParameterValueIsEmpty

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: ' ' toUnarySelector: thisContext selector
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter newParameterValueCanNotBeEmptyErrorMessage ]
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 17/Aug/2018, 8:57:23 pm (UTC)'!
test11NewParameterNameCanNotBeEmpty

	| newParameter |
	
	newParameter := ' '.
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidParameterName: newParameter withBlanksTrimmed ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 17/Aug/2018, 4:34:07 pm (UTC)'!
test12NewParameterNameCanNotStartWithNumber

	| newParameter |
	
	newParameter := '1x'.
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidParameterName: newParameter  ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 17/Aug/2018, 4:34:28 pm (UTC)'!
test13NewParameterNameCanNotContainSpaces

	| newParameter |
	
	newParameter := 'an Integer'.
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidParameterName: newParameter  ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 17/Aug/2018, 7:14:45 pm (UTC)'!
test14NewParameterCanNotBeDefinedAsInstanceVariableInAnyImplementor

	| classToRefactor oldSelector newParameter  |
	
	oldSelector := 'm1' asSymbol.
	newParameter := 'newParam'.
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newParameter.
	classToRefactor compile: oldSelector asString.
	
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' toUnarySelector: oldSelector 
			implementors: {classToRefactor>>oldSelector} senders: {} ]
		 failsWith: [ AddParameter errorMessageForNewParameter: newParameter definedAsInstanceVariableIn: {classToRefactor} ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 2/Sep/2018, 11:59:27 pm (UTC)'!
test15NewParameterCanNotBeDefinedAsParameterInAnyImplementor

	| classToRefactor oldSelector newParameter implementors |
	
	oldSelector := 'm1:' asSymbol.
	newParameter := 'newParam'.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, newParameter.
	implementors := {classToRefactor>>oldSelector}.
	
	self 
		assertCreation: [ AddParameter 
			named: newParameter initializedWith: '1' using: 'm2:' asSymbol toKeywordSelector: oldSelector 
			implementors: implementors senders: {} ]
		 failsWith: [ AddParameter errorMessageForNewParameterDefinedAsLocal: newParameter ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 18/Aug/2018, 4:21:52 pm (UTC)'!
test16WhenCreatedForUnarySelectorMustBeUnary

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' toUnarySelector: 'm1:' asSymbol 
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter selectorMustBeUnaryErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 18/Aug/2018, 4:27:42 pm (UTC)'!
test17WhenCreatedForBinarySelectorMustBeBinary

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: 'm1:' asSymbol toKeywordSelector: thisContext selector
			implementors: {thisContext method} senders: {} ]
		 failsWith: [ AddParameter selectorMustBeKeywordErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 5/Sep/2018, 12:10:41 am (UTC)'!
test18CanNotAddParameterToBinaryKeyword

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' at: 1 addingLast: true initializedWith: '1' to: '+' asSymbol implementing: 'm1:' asSymbol
			addingToImplementors: '' addingToSenders: ''
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter selectorCanNotBeBinaryErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 18/Aug/2018, 7:48:13 pm (UTC)'!
test19AllImplementorsMustImplementOldSelector

	| implementors |
	
	implementors := {thisContext method}.
	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' toUnarySelector: (thisContext selector, 'x') asSymbol
			implementors: implementors senders: {} ]
		 failsWith: [ AddParameter errorMessageForInvalidImplementors: implementors ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 18/Aug/2018, 7:53:37 pm (UTC)'!
test20AllSendersShouldSendOldSelector

	| oldSelector invalidSenders |
	
	oldSelector := thisContext selector.
	invalidSenders := {thisContext method}.
	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' toUnarySelector: oldSelector
			implementors: {thisContext method} senders: invalidSenders ]
		 failsWith: [ AddParameter errorMessageForInvalidSenders: invalidSenders of: oldSelector ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 18/Aug/2018, 8:24:22 pm (UTC)'!
test21NewKeywordToAddToOldSelectorCanNotBeUnary

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: 'm1' asSymbol toKeywordSelector: 'm1:' asSymbol
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter notValidKeywordForNewParameterErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 18/Aug/2018, 8:26:55 pm (UTC)'!
test22NewKeywordToAddToOldSelectorCanNotBeBinary

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: '+' asSymbol toKeywordSelector: 'm1:' asSymbol
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter notValidKeywordForNewParameterErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 18/Aug/2018, 8:27:58 pm (UTC)'!
test23NewKeywordToAddToOldSelectorCanNotBeKeywordWithMoreThanOneParameter

	self 
		assertCreation: [ AddParameter 
			named: 'newParam' initializedWith: '1' using: 'm1:m2:' asSymbol toKeywordSelector: 'm1:' asSymbol
			implementors: {} senders: {} ]
		 failsWith: [ AddParameter notValidKeywordForNewParameterErrorMessage ]
	
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 24/Aug/2018, 9:10:04 pm (UTC)'!
test24AddingParameterToSendersTakeCaresOfSeparators

	| refactoring classToRefactor oldSelector newSelector newParameter newParameterValue senderSelector |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, Character newLineCharacter asString,'	'.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	self shouldnt: [ refactoring apply ] raise: Error.
	
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:45:17 pm (UTC)'!
test25IfNewParameterValueIsKeywordMessageSendAddParenthesisToItForUnaryMessages

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector newSelector senderMethod senderMethodNode senderSourceCode senderSourceRange |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	newParameterValue := 'self at: 1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: newSelector).
	
	senderMethodNode := senderMethod methodNode.
	senderSourceRange := senderMethodNode encoder rangeForNode: senderMethodNode block statements first ifAbsent: [ self fail ].
	senderSourceCode := senderMethodNode sourceText copyFrom: senderSourceRange first to: senderSourceRange last.
	self assert: (senderSourceCode endsWith: '(',newParameterValue,')')
	
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:45:21 pm (UTC)'!
test26IfNewParameterValueIsKeywordMessageSendAddParenthesisToItForKeywordMessages

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector newSelector senderMethod newSelectorAddedKeyword senderMethodNode senderSourceCode senderSourceRange |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := 'self at: 1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' oldParam'.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, ' 1'.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: newSelector).
	
	senderMethodNode := senderMethod methodNode.
	senderSourceRange := senderMethodNode encoder rangeForNode: senderMethodNode block statements first ifAbsent: [ self fail ].
	senderSourceCode := senderMethodNode sourceText copyFrom: senderSourceRange first to: senderSourceRange last.
	self assert: (senderSourceCode endsWith: '(',newParameterValue,')')
	
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 25/Aug/2018, 3:23:42 pm (UTC)'!
test27NewParameterValueCanNotHaveMoreThanOneStatement

	self 
		assertCreation: [ AddParameter 
			named: 'newParameter' initializedWith: 'self m1. self m2.' toUnarySelector: thisContext selector 
			implementors: {thisContext method} senders: {} ]
		failsWith: [ AddParameter newParameterValueCanNotHaveMoreThanOneStatementErrorMessage ].
	
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:45:25 pm (UTC)'!
test28WhenSenderLastParameterIsABlockAddedParameterIsNotAddedInsideTheBlock

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector newSelector senderMethod newSelectorAddedKeyword senderMethodNode senderSourceCode senderSourceRange senderFirstParameterValue |

	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector,newSelectorAddedKeyword) asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	senderFirstParameterValue := '[ 1 factorial ]'.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' oldParam'.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, senderFirstParameterValue.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: newSelector).
	
	senderMethodNode := senderMethod methodNode.
	senderSourceRange := senderMethodNode encoder rangeForNode: senderMethodNode block statements first arguments first closureCreationNode ifAbsent: [ self fail ].
	senderSourceCode := senderMethodNode sourceText copyFrom: senderSourceRange first to: senderSourceRange last.
	self assert: senderFirstParameterValue equals: senderSourceCode
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:45:49 pm (UTC)'!
test29AddingParameterAsFirstOneWorksAsExpected

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := 'm2:m1:' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'm1: p1'.
	classToRefactor compile: senderSelector asString, ' self m1: 1'.
	
	refactoring := AddParameter 
		named: newParameter at: 1 initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	self assert: (newImplementorMethodNode arguments at: 1) name equals: newParameter.
	
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments first literalValue).
	self assert: senderMethod sourceCode equals: senderSelector asString, ' self m2: 2 m1: 1'.
! !

!AddParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:45:54 pm (UTC)'!
test30AddingParameterInTheMiddleWorksAsExpected

	| refactoring classToRefactor oldSelector newSelector newImplementorMethodNode newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:m3:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := 'm1:m2:m3:' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'm1: p1 m3: p3'.
	classToRefactor compile: senderSelector asString, ' self m1: 1 m3: 3'.
	
	refactoring := AddParameter 
		named: newParameter at: 2 initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorMethodNode :=  (classToRefactor>>newSelector) methodNode.
	self assert: (newImplementorMethodNode arguments at: 2) name equals: newParameter.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	
	self assert: newParameterValue asNumber equals: (senderMethod methodNode block statements first arguments second literalValue).
	self assert: senderMethod sourceCode equals: senderSelector asString, ' self m1: 1 m2: 2 m3: 3'.
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 7/Jan/2019, 5:45:01 pm (UTC)'!
test31AddingParameterInTheMiddleWithManySendersWorksAsExpected

	| refactoring classToRefactor oldSelector newSelector newParameter newParameterValue senderSelector senderMethod newSelectorAddedKeyword |
	
	oldSelector := 'm1:m3:' asSymbol.
	newSelectorAddedKeyword := 'm2:' asSymbol.
	newSelector := 'm1:m2:m3:' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '2'.
	senderSelector := 'sender' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'm1: p1 m3: p3'.
	classToRefactor compile: senderSelector asString, '
	self m1: 1 m3: 3.
	self
		m1: 4
		m3: 6'.
	
	refactoring := AddParameter 
		named: newParameter at: 2 initializedWith: newParameterValue using: newSelectorAddedKeyword toKeywordSelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderMethod sourceCode equals: senderSelector asString, '
	self m1: 1 m2: 2 m3: 3.
	self
		m1: 4
		m2: 2 m3: 6'.
! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 7/Jan/2019, 5:44:50 pm (UTC)'!
test32IndexCanNotBeLessThanOne
	
	self 
		assertCreation: [ 
			AddParameter 
				named: 'newParam' 
				at: 0
				initializedWith: '1' 
				using: 'm2:' asSymbol 
				toKeywordSelector: 'm1:' asSymbol  
				implementors: {} 
				senders: {} ]
		failsWith: [ AddParameter errorMessageForInvalidParameterIndex: 0 for: 1 ]! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 7/Jan/2019, 5:44:43 pm (UTC)'!
test33IndexCanNotBeBiggerThanNumberOfParametersPlusOne
	
	self 
		assertCreation: [ 
			AddParameter 
				named: 'newParam' 
				at: 3
				initializedWith: '1' 
				using: 'm2:' asSymbol 
				toKeywordSelector: 'm1:' asSymbol  
				implementors: {} 
				senders: {} ]
		failsWith: [ AddParameter errorMessageForInvalidParameterIndex: 3 for: 1 ]! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 7/Jan/2019, 5:44:36 pm (UTC)'!
test34IndexMustBeInteger
	
	self 
		assertCreation: [ 
			AddParameter 
				named: 'newParam' 
				at: 1.5
				initializedWith: '1' 
				using: 'm2:' asSymbol 
				toKeywordSelector: 'm1:' asSymbol  
				implementors: {} 
				senders: {} ]
		failsWith: [ AddParameter errorMessageForInvalidParameterIndex: 1.5 for: 1 ]! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 7/Jan/2019, 5:44:23 pm (UTC)'!
test35AddingParameterRetractsInsertionPointWhenAtInsertionPointIsANewLine

	| refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector |
	
	oldSelector := 'm1' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, Character newLineCharacter asString, Character newLineCharacter asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	self shouldnt: [ refactoring apply ] raise: Error.
	
	! !

!AddParameterTest methodsFor: 'tests' stamp: 'HAW 7/Jan/2019, 5:44:13 pm (UTC)'!
test36ParentesisAreAddedToSendersOfUnaryMessage

	self shouldFail: [ | refactoring classToRefactor oldSelector newParameter newParameterValue senderSelector |
	
	oldSelector := 'm1' asSymbol.
	newParameter := 'newParam'.
	newParameterValue := '1'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString, ', self size'.

	refactoring := AddParameter 
		named: newParameter initializedWith: newParameterValue toUnarySelector: oldSelector  
		implementors: {classToRefactor>>oldSelector} senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self assert: senderSelector asString, ' (self ', oldSelector asString, '), self size' equals: (classToRefactor>>senderSelector) sourceCode 
	]! !

!AddParameterTest methodsFor: 'class factory' stamp: 'HAW 24/Aug/2018, 9:20:56 pm (UTC)'!
classToRefactorName
	
	^#ClassToAddParameter! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'class factory' stamp: 'JEC 29/Oct/2025, 10:29:39 pm (UTC)'!
classToRefactorName

	^ #ClassToChangeSelectorOrder! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'class factory' stamp: 'JEC 11/Jan/2026, 1:55:20 pm (UTC)'!
compile: aSourceCode

	self createClassToRefactorIfNecessary.
	classToRefactor compile: aSourceCode! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'class factory' stamp: 'JEC 11/Jan/2026, 1:56:24 pm (UTC)'!
createClassToRefactorIfNecessary

	classToRefactor ifNil: [ classToRefactor := self createClassNamed: self classToRefactorName ]! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'source code creation' stamp: 'HAW 17/Mar/2019, 9:43:44 am (UTC)'!
selectorAndParametersFor: aKeywordSelector

	^String streamContents: [ :stream |
		aKeywordSelector keywords
			withIndexDo: [ :aKeyword :index |
				stream
					nextPutAll: aKeyword;
					space;
					nextPut: $p;
					print: index ]
			separatedBy: [  stream space ]]
				! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 17/Mar/2019, 10:18:58 am (UTC)'!
test01CannotChangeSelectorOrderInUnaryMessages

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #m1 to: #m1 implementors: #() senders: #() ]
		failsWith: [ ChangeKeywordsSelectorOrder selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage ]! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 17/Mar/2019, 10:18:58 am (UTC)'!
test02CannotChangeSelectorOrderInBinaryMessages

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #+ to: #+ implementors: #() senders: #() ]
		failsWith: [ ChangeKeywordsSelectorOrder selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage ]! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 17/Mar/2019, 10:18:58 am (UTC)'!
test03CannotChangeSelectorOrderInKeywordMessagesWithOneParameterOnly

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #m1: to: #m1: implementors: #() senders: #() ]
		 failsWith: [ ChangeKeywordsSelectorOrder selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage ]! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:48:37 pm (UTC)'!
test04ChangesKeywordsOrder

	|  refactoring oldSelector newSelector |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	self compile: (self selectorAndParametersFor: oldSelector).

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: #().
	refactoring apply.

	self assert: (classToRefactor includesSelector: newSelector).
	self deny: (classToRefactor includesSelector: oldSelector)! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:49:19 pm (UTC)'!
test05ChangesParametersOrder

	| refactoring oldSelector newSelector renamedMethod parameters selectorAndParameters |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: #().
	refactoring apply.

	renamedMethod := classToRefactor >> newSelector.
	parameters := selectorAndParameters substrings reject: [ :aKeywordOrParameter | oldSelector keywords includes: aKeywordOrParameter  ].
	self
		assert: (OrderedCollection with: parameters second with: parameters first)
		equals: renamedMethod methodNode argumentNames ! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'HAW 17/Mar/2019, 9:31:03 am (UTC)'!
test06NewSelectorMustIncludeOldSelectorKeywords

	self
		assertCreation: [ ChangeKeywordsSelectorOrder from: #m1:m2: to: #m3:m1: implementors: #() senders: #() ]
		failsWith: [ ChangeKeywordsSelectorOrder newSelectorDoesNotIncludeOldSelectorKeywordsErrorMessage ]
		! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:49:38 pm (UTC)'!
test07ChangesKeywordsAndParametersOrderForMoreThanTwoKeywords

	| refactoring renamedMethod oldSelector newSelector selectorAndParameters parameters |

	oldSelector := #m1:m2:m3:.
	newSelector := #m2:m3:m1:.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: #().
	refactoring apply.

	renamedMethod := classToRefactor >> newSelector.
	parameters := selectorAndParameters substrings reject: [ :aKeywordOrParameter | oldSelector keywords includes: aKeywordOrParameter  ].
	self
		assert: (OrderedCollection with: parameters second with: parameters third with: parameters first)
		equals: renamedMethod methodNode argumentNames ! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:50:16 pm (UTC)'!
test08_ChangesArguments_WhenTheyAreLiterals

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: 1 m2: 2.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, ' self m2: 2 m1: 1.'! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:52:00 pm (UTC)'!
test09_ChangesArguments_WhenTheyAreCollaborations

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: self value m2: self hash.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, ' self m2: self hash m1: self value.'! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:52:09 pm (UTC)'!
test10_ChangesArguments_WhenTheyAreBlocks

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: [ ] m2: [].'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, ' self m2: [] m1: [ ].'! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:52:18 pm (UTC)'!
test11_ChangesArguments_WhenTheyAreCollaborationsAndBlocks

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: [ |a| a:= 10. a ] m2: self value.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, ' self m2: self value m1: [ |a| a:= 10. a ].'! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:52:26 pm (UTC)'!
test12_ChangesArguments_WhenTheKeywordsAreNotTheSameSize

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #a:bb:ccc:.
	newSelector := #ccc:a:bb:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self a: 1 bb: 2 ccc: 3.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderSelector, ' self ccc: 3 a: 1 bb: 2.'
		equals: senderMethod sourceCode! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:52:35 pm (UTC)'!
test13_ChangesArguments_WhenArgumentsAndKeywordsAreNotTheSameSize

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #a:bb:ccc:.
	newSelector := #ccc:a:bb:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self a: 111 bb: 22 ccc: 3.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderSelector, ' self ccc: 3 a: 111 bb: 22.'
		equals: senderMethod sourceCode! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:52:42 pm (UTC)'!
test14_ChangesArguments_WhenTheKeywordsAreNotTheSameSize

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #a:bb:ccc:.
	newSelector := #ccc:a:bb:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self a: self value bb: 123412341234 ccc: 3.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderSelector, ' self ccc: 3 a: self value bb: 123412341234.'
		equals: senderMethod sourceCode! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:52:51 pm (UTC)'!
test15_ChangingArguments_KeepsTheBlanksAtTheArgumentPosition

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #a:bb:ccc:.
	newSelector := #ccc:a:bb:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self 
		a: self value 
		bb: 123412341234 
		ccc: 3.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderSelector, ' self 
		ccc: 3 
		a: self value 
		bb: 123412341234.'
		equals: senderMethod sourceCode! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:52:57 pm (UTC)'!
test16_CanChangeSelectorOrder_WhenHavingMultipleCollaborationsInSameMessage

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, '
	self m1: 1 m2: 2.
	self m1: 1 m2: 2.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, '
	self m2: 2 m1: 1.
	self m2: 2 m1: 1.'! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:53:12 pm (UTC)'!
test17_CanChangeSelectorOrder_WhenHavingMultipleMessagesWithTheCollaboration

	| refactoring oldSelector newSelector anotherSenderSelector selectorAndParameters senderSelector senderMethod anotherSenderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	anotherSenderSelector := #m4.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: 1 m2: 2.'.
	self compile: anotherSenderSelector, ' self m1: 111 m2: 22.'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector . classToRefactor >> anotherSenderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	anotherSenderMethod := classToRefactor  >> anotherSenderSelector.
	self 
		assert: senderMethod sourceCode
		equals: senderSelector, ' self m2: 2 m1: 1.'.
	
	self 
		assert: anotherSenderMethod sourceCode
		equals: anotherSenderSelector, ' self m2: 22 m1: 111.'! !

!ChangeKeywordsSelectorOrderTest methodsFor: 'tests' stamp: 'JEC 11/Jan/2026, 1:53:22 pm (UTC)'!
test18_CanChangeSelectorOrder_WhenHavingTheChangingCollaborationRecursively

	| refactoring oldSelector newSelector selectorAndParameters senderSelector senderMethod |

	oldSelector := #m1:m2:.
	newSelector := #m2:m1:.
	senderSelector := #m3.
	selectorAndParameters := self selectorAndParametersFor: oldSelector.
	self compile: selectorAndParameters.
	self compile: senderSelector, ' self m1: (self m1: 1 m2: 2) m2: (self m1: 11 m2: 22).'.

	refactoring := ChangeKeywordsSelectorOrder 
		from: oldSelector 
		to: newSelector 
		implementors: { classToRefactor >> oldSelector } 
		senders: { classToRefactor >> senderSelector }.
	refactoring apply.

	senderMethod := classToRefactor >> senderSelector.
	self 
		assert: senderSelector, ' self m2: (self m2: 22 m1: 11) m1: (self m2: 2 m1: 1).'
		equals: senderMethod sourceCode.! !

!ExtractAsParameterTest methodsFor: 'assertions' stamp: 'HAW 22/Sep/2021, 8:02:51 pm (UTC)'!
assertCanExtractAsParameter: toExtract

	self assertCanExtractFrom: toExtract size: toExtract size introducing: toExtract! !

!ExtractAsParameterTest methodsFor: 'assertions' stamp: 'HAW 8/Jul/2024, 3:22:22 pm (UTC)'!
assertCanExtractFrom: sourceCode interval: interval introducing: toIntroduced 

	| refactoring classToRefactor oldSelector newSelector newImplementorSourceCode newParameter senderSelector senderMethod |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	senderSelector := 'sender_m1' asSymbol.
	
	classToRefactor := self createDefaultClassToRefactor. 
	classToRefactor compile: oldSelector asString, ' ', sourceCode.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.
	
	refactoring := ExtractAsParameter 
		named: newParameter 
		extractedFrom: interval + 3
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorSourceCode :=  (classToRefactor>>newSelector) sourceCode.
	self assert: newSelector, ' ', newParameter, ' ', (sourceCode first: interval first - 1), newParameter, (sourceCode last: sourceCode size - interval last) equals: newImplementorSourceCode.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderSelector asString, ' self ', newSelector asString, ' ', toIntroduced equals: senderMethod sourceCode! !

!ExtractAsParameterTest methodsFor: 'assertions' stamp: 'HAW 22/Sep/2021, 3:51:22 pm (UTC)'!
assertCanExtractFrom: sourceCode size: toExtractSize introducing: toIntroduced 

	self assertCanExtractFrom: sourceCode interval: (1 to: toExtractSize) introducing: toIntroduced ! !

!ExtractAsParameterTest methodsFor: 'assertions' stamp: 'HAW 22/Sep/2021, 8:03:34 pm (UTC)'!
assertCannotExtractAsParameter: newParameterValue withErrorMessage: anErrorMessage

	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' ', newParameterValue.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (4 to: 3+newParameterValue size) 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: anErrorMessage ! !

!ExtractAsParameterTest methodsFor: 'assertions' stamp: 'HAW 22/Sep/2021, 8:06:03 pm (UTC)'!
assertCannotExtractInvalidNodeAsParameter: newParameterValue

	self 
		assertCannotExtractAsParameter: newParameterValue 
		withErrorMessage: ExtractAsParameter errorMessageForInvalidExpressionToExtractAsParameter! !

!ExtractAsParameterTest methodsFor: 'assertions' stamp: 'HAW 22/Sep/2021, 8:05:39 pm (UTC)'!
assertCannotExtractInvalidSelectionAsParameter: newParameterValue

	self 
		assertCannotExtractAsParameter: newParameterValue 
		withErrorMessage: ExtractAsParameter errorMessageForInvalidSelection ! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:07:02 pm (UTC)'!
test01CanExtractLiterals

	self assertCanExtractAsParameter: '1'
! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:07:26 pm (UTC)'!
test02CannotExtract_self

	self assertCannotExtractInvalidNodeAsParameter: 'self'.
	
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:08:15 pm (UTC)'!
test03CannotExtract_super

	self assertCannotExtractInvalidNodeAsParameter: 'super'.
	
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:09:04 pm (UTC)'!
test04CannotExtract_thisContext

	self assertCannotExtractInvalidNodeAsParameter: 'thisContext'.
	
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:08:57 pm (UTC)'!
test05CanExtractMessageSendToLiteralWithLiterals

	self assertCanExtractAsParameter: '1 + 1'.
	
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:09:15 pm (UTC)'!
test06CanExtract_true

	self assertCanExtractAsParameter: 'true'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:09:30 pm (UTC)'!
test07CannotExtract_comment

	self assertCannotExtractInvalidSelectionAsParameter: '"comment"'.
	
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:09:52 pm (UTC)'!
test08CanExtractSelectionWithSeparators

	self assertCanExtractFrom: ' 10 ' size: 4 introducing: '10'
	
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:10:22 pm (UTC)'!
test09RemovesTrailingDotsFromSelection

	| refactoring classToRefactor oldSelector newSelector newImplementorSourceCode newParameter senderSelector senderMethod toExtract |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	senderSelector := 'sender_m1' asSymbol.
	toExtract := '1.'.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' ', toExtract, ' self printString'.
	classToRefactor compile: senderSelector asString, ' self ', oldSelector asString.
	
	refactoring := ExtractAsParameter 
		named: newParameter 
		extractedFrom: (4 to: 5) 
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorSourceCode :=  (classToRefactor>>newSelector) sourceCode.
	self assert: newSelector, ' ', newParameter, ' ', newParameter, '. self printString' equals: newImplementorSourceCode.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderSelector asString, ' self ', newSelector asString, ' 1' equals: senderMethod sourceCode! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:10:35 pm (UTC)'!
test10CanExtractWithParenthesis

	self assertCanExtractFrom: '(1)' size: 3 introducing: '1' 
	
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:10:45 pm (UTC)'!
test11CanExtract_symbol

	self assertCanExtractAsParameter: '#assert:'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:10:55 pm (UTC)'!
test12CanExtract_false

	self assertCanExtractAsParameter: 'false'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:11:03 pm (UTC)'!
test13CanExtract_nil

	self assertCanExtractAsParameter: 'nil'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:11:34 pm (UTC)'!
test14CanExtactFromKeywordSelector

	| refactoring classToRefactor oldSelector newSelector newImplementorSourceCode newParameter senderSelector senderMethod toExtract newKeyword |
	
	oldSelector := 'm1:' asSymbol.
	newKeyword := 'm2:' asSymbol.
	newSelector := (oldSelector, newKeyword) asSymbol.
	newParameter := 'newParam'.
	senderSelector := 'sender_m1' asSymbol.
	toExtract := '1.'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector, ' p1 ', toExtract, ' self printString'.
	classToRefactor compile: senderSelector, ' self ', oldSelector, ' #x '.
	
	refactoring := ExtractAsParameter 
		named: newParameter 
		extractedFrom: (8 to: 9) 
		at: 2
		newKeyword: newKeyword
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorSourceCode :=  (classToRefactor>>newSelector) sourceCode.
	self assert: oldSelector, ' p1 ', newKeyword, ' ', newParameter, '  ', newParameter, '. self printString' equals: newImplementorSourceCode.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderSelector, ' self ', oldSelector, ' #x ', newKeyword, ' 1 ' equals: senderMethod sourceCode! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:12:01 pm (UTC)'!
test15CanExtractMessageSendToLiteral

	| refactoring classToRefactor oldSelector newSelector newImplementorSourceCode newParameter senderSelector senderMethod toExtract |
	
	oldSelector := 'm1' asSymbol.
	newSelector := (oldSelector, ':') asSymbol.
	newParameter := 'newParam'.
	senderSelector := 'sender_m1' asSymbol.
	toExtract := '10 factorial'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector, ' ', toExtract.
	classToRefactor compile: senderSelector, ' self ', oldSelector.
	
	refactoring := ExtractAsParameter 
		named: newParameter 
		extractedFrom: (4 to: 15) 
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector).
	
	newImplementorSourceCode :=  (classToRefactor>>newSelector) sourceCode.
	self assert: newSelector, ' ', newParameter, ' ', newParameter equals: newImplementorSourceCode.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: senderSelector, ' self ', newSelector, ' ', toExtract equals: senderMethod sourceCode! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:12:21 pm (UTC)'!
test16CannotExtractMessageSendWithInvalidNode

	self assertCannotExtractInvalidNodeAsParameter: '10 + self'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:12:38 pm (UTC)'!
test17CannotExtractMessageSendToInvalidNode

	self assertCannotExtractInvalidNodeAsParameter: 'self + 10'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:12:59 pm (UTC)'!
test18CanExtractManyMessageSends

	self assertCanExtractAsParameter: '10 + 10 + 10'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:13:19 pm (UTC)'!
test19CannotExtractManyMessageSendsWithInvalidNode

	self assertCannotExtractInvalidNodeAsParameter: '10 + 10 + self'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:13:38 pm (UTC)'!
test20CanExtractPartOfAValidExpression

	self assertCanExtractFrom: '10 + 10' size: 2 introducing: '10'
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:13:48 pm (UTC)'!
test21CanExtractABlock

	self assertCanExtractAsParameter: '[10]'
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:14:01 pm (UTC)'!
test22CannotExtractBlockWithInvalidNode

	self assertCannotExtractInvalidNodeAsParameter: '[self]'
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:14:17 pm (UTC)'!
test23CannotExtractBlockWithMessageSendWithInvalidNode

	self assertCannotExtractInvalidNodeAsParameter: '[1 + self]'
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:14:34 pm (UTC)'!
test24CanExtractBlockWithManyExpressions

	self assertCanExtractAsParameter: '[1 + 1. 2 + 2]'
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:14:44 pm (UTC)'!
test25CanExtractBlockWithParameters

	self assertCanExtractAsParameter: '[:p1 | 1 + p1]'
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:15:06 pm (UTC)'!
test26CannotExtractBlockThatReferencesMethodVariable

	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' | var1 | [ 1 + var1 ]'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (13 to: 24) 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageForInvalidExpressionToExtractAsParameter 

! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:15:23 pm (UTC)'!
test27CanExtractBlockWithParameterAndLocalVariable

	self assertCanExtractAsParameter: '[:p1 | | v1 | v1 := 1. v1 + p1]'
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:16:13 pm (UTC)'!
test28CanExtractBlockEvaluation

	| toExtract |
	
	toExtract := '[:p1 | p1] value: 1'.
	self assertCanExtractFrom: toExtract size: toExtract size introducing: '(', toExtract ,')'
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:16:32 pm (UTC)'!
test29CannotExtractInvalidInterval

	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (2 to: 1) 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractMethodNewMethod noSelectionErrorMessage ! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:16:45 pm (UTC)'!
test30CannotExtractIntervalOutOfBounds

	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (2 to: 20) 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractMethodNewMethod outOfBoundsSelectionErrorMessage ! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:17:03 pm (UTC)'!
test31CannotExtractIfOriginalMethodIsNotInImplementors

	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 1'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (4 to: 4) 
			at: classToRefactor>>oldSelector  
			implementors: {} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageForOrigialMethodMustBeInImplementorsToChange! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 8:21:15 pm (UTC)'!
test32CannotExtractMoreThanOneExpression

	self assertCannotExtractInvalidSelectionAsParameter: '1+1. 2+2' ! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 11:54:04 pm (UTC)'!
test33WhenAddingToUnaryMessageSelectorMustBeUnary
	
	| classToRefactor oldSelector  |
	
	oldSelector := 'm1:' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' p1 1'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (8 to: 8) 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: AddParameter selectorMustBeUnaryErrorMessage ! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 22/Sep/2021, 11:54:11 pm (UTC)'!
test34WhenAddingToKeywordMessageSelectorMustBeKeyword
	
	| classToRefactor oldSelector  |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 1'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFrom: (4 to: 4) 
			at: 1
			newKeyword: #m2:
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: AddParameter selectorMustBeKeywordErrorMessage ! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2021, 10:19:43 pm (UTC)'!
test35CanExtractManyIntervals
	
	| classToRefactor oldSelector  refactoring |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 10 + 10'.
	
	refactoring := ExtractAsParameter 
		named: 'newParam' 
		extractedFromAll: { (4 to: 5). (9 to: 10) } 
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {}.
		
	refactoring apply.
	
	self assert: 'm1: newParam newParam + newParam' equals: (classToRefactor >> #m1:) sourceCode
		! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2021, 10:20:26 pm (UTC)'!
test36CannotExtractIfAnyIntervalIsNotValid
	
	| classToRefactor oldSelector |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 10 + self'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFromAll: { (4 to: 5). (9 to: 12) } 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageForInvalidExpressionToExtractAsParameter 
		
	
		! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2021, 10:20:36 pm (UTC)'!
test37CannotExtractIfSourceToExtractAreNotEqualInAllIntervals
	
	| classToRefactor oldSelector |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 10 + 11'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFromAll: { (4 to: 5). (9 to: 10) } 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageNotAllExpressionsToExtractAreEqual
		
	
		! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2021, 10:20:55 pm (UTC)'!
test38CanExtractManyBlocksIfAreEqual
	
	| classToRefactor oldSelector  refactoring |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' [10] + [10]'.
	
	refactoring := ExtractAsParameter 
		named: 'newParam' 
		extractedFromAll: { (4 to: 7). (11 to: 14) } 
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {}.
		
	refactoring apply.
	
	self assert: 'm1: newParam newParam + newParam' equals: (classToRefactor >> #m1:) sourceCode
		! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2021, 10:21:22 pm (UTC)'!
test39CanExtractManyBlocksIfSourceCodeNotEqualButSameAst
	
	| classToRefactor oldSelector  refactoring |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' [10] + [ 10 ]'.
	
	refactoring := ExtractAsParameter 
		named: 'newParam' 
		extractedFromAll: { (4 to: 7). (11 to: 16) } 
		at: classToRefactor>>oldSelector  
		implementors: {classToRefactor>>oldSelector} 
		senders: {}.
		
	refactoring apply.
	
	self assert: 'm1: newParam newParam + newParam' equals: (classToRefactor >> #m1:) sourceCode
		! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 24/Sep/2021, 10:21:48 pm (UTC)'!
test40CannotExtractWithoutIntervals
	
	| classToRefactor oldSelector |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' 10 + 11'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFromAll: {} 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageForNoExpressionToExtract
		
	
		! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 25/Sep/2021, 12:32:04 am (UTC)'!
test41DetectsAllRangesForLiteral
	
	| classToRefactor selector intervals |
	
	selector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector, ' 10 + 10'.
	
	intervals := ExtractAsParameter intervalsForEquivalentExpressionIn: classToRefactor >> selector at: (4 to: 5).

	self assert: { (4 to: 5). (9 to: 10) } equals: intervals asArray.! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 25/Sep/2021, 12:32:28 am (UTC)'!
test42DoesNotMixesRangesForDifferentLiterals
	
	| classToRefactor selector intervals |
	
	selector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector, ' 10 + 100'.
	
	intervals := ExtractAsParameter intervalsForEquivalentExpressionIn: classToRefactor >> selector at: (4 to: 5).

	self assert: { (4 to: 5) } equals: intervals asArray.! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 25/Sep/2021, 12:32:48 am (UTC)'!
test43DetectsRangesForEquivalentBlocks
	
	| classToRefactor selector intervals |
	
	selector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector, ' [10] + [ 10 ]'.
	
	intervals := ExtractAsParameter intervalsForEquivalentExpressionIn: classToRefactor >> selector at: (4 to: 7).

	self assert: { (4 to: 7). (11 to: 16) } equals: intervals asArray.! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 25/Sep/2021, 12:33:01 am (UTC)'!
test44DetectsRangesForEquivalentMessageSends
	
	| classToRefactor selector intervals |
	
	selector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector, ' 10 factorial + 10    factorial'.
	
	intervals := ExtractAsParameter intervalsForEquivalentExpressionIn: classToRefactor >> selector at: (4 to: 15).

	self assert: { (4 to: 15). (19 to: 33) } equals: intervals asArray.! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 25/Sep/2021, 12:34:32 am (UTC)'!
test45DoesNotDetectEquiallySemanticBlocks
	
	| classToRefactor selector intervals |
	
	"This is something to improve - Hernan"
	
	selector := 'm1' asSymbol.
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector, ' [:p1 | 1 ] + [:p2 | 1]'.
	
	intervals := ExtractAsParameter intervalsForEquivalentExpressionIn: classToRefactor >> selector at: (4 to: 13).

	self assert: { (4 to: 13) } equals: intervals asArray.! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 6/Dec/2021, 3:44:09 pm (UTC)'!
test46CanExtractMessageSendToClassWithValidParameters

	| toExtract |
	
	toExtract := 'Array with: 1'.
	
	self assertCanExtractFrom: toExtract size: toExtract size introducing: '(', toExtract, ')'
	
	! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 6/Dec/2021, 3:44:47 pm (UTC)'!
test47CanExtractEmptyBraceArray

	self assertCanExtractAsParameter: '{}'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 6/Dec/2021, 3:46:33 pm (UTC)'!
test48CanExtractBraceArrayWithExtractableElements

	self assertCanExtractAsParameter: '{1. {}. Array with: 1}'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 6/Dec/2021, 3:49:29 pm (UTC)'!
test49CannotExtractBraceArrayWithNoExtractableElements

	self assertCannotExtractInvalidNodeAsParameter: '{1. self m2 }'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 6/Dec/2021, 4:05:47 pm (UTC)'!
test50CanExtractReferenceToGlobal

	self assertCanExtractAsParameter: 'Smalltalk'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 6/Dec/2021, 4:05:01 pm (UTC)'!
test51CannotExtractNotExistingGlobalReference

	self assertCannotExtractInvalidNodeAsParameter: '__Smalltalk__'! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 8/Jul/2024, 1:35:25 pm (UTC)'!
test52CanExtractReferenceToInstVar

	self assertCanExtractAsParameter: self instVarName ! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 8/Jul/2024, 1:39:29 pm (UTC)'!
test53CanExtractReferenceToClassVar

	self assertCanExtractAsParameter: self classVarName ! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 8/Jul/2024, 3:26:06 pm (UTC)'!
test54CannotExtractInstVarWhenInLeftPartOfAssigment
	
	| classToRefactor oldSelector |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createDefaultClassToRefactor.
	classToRefactor compile: oldSelector asString, ' ', self instVarName, ':= 1'.
	
	self
		assertCreation: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFromAll: { oldSelector size + 2 to: oldSelector size + 2 + self instVarName size - 1 } 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		failsWith: ExtractAsParameter errorMessageForInvalidExpressionToExtractAsParameter 
		
	
		! !

!ExtractAsParameterTest methodsFor: 'tests' stamp: 'HAW 8/Jul/2024, 4:13:57 pm (UTC)'!
test55CanExtractInstVarEvenIsLeftPartOfAssigmentOfOtherNode
	
	| classToRefactor oldSelector sourceCode |
	
	oldSelector := 'm1' asSymbol.
	
	classToRefactor := self createDefaultClassToRefactor.
	sourceCode := oldSelector asString, ' ', self instVarName, ':= 1. ', self instVarName.
	classToRefactor compile: sourceCode.
	
	self 
		shouldnt: [ ExtractAsParameter 
			named: 'newParam' 
			extractedFromAll: { sourceCode size - self instVarName size + 1 to: sourceCode size } 
			at: classToRefactor>>oldSelector  
			implementors: {classToRefactor>>oldSelector} 
			senders: {} ]
		raise: RefactoringError 
		
	
		! !

!ExtractAsParameterTest methodsFor: 'class factory' stamp: 'HAW 9/Sep/2021, 2:00:32 am (UTC)'!
classToRefactorName
	
	^#ClassToIntroduceParameter! !

!ExtractAsParameterTest methodsFor: 'class factory' stamp: 'HAW 8/Jul/2024, 1:33:19 pm (UTC)'!
classVarName
	
	^'ClassVar1'! !

!ExtractAsParameterTest methodsFor: 'class factory' stamp: 'HAW 13/Mar/2025, 5:59:28 pm (UTC)'!
createDefaultClassToRefactor

	^ self 
		createClassNamed: self classToRefactorName 
		subclassOf: self defaultSuperclass 
		instanceVariableNames: self instVarName 
		classVariableNames: self classVarName! !

!ExtractAsParameterTest methodsFor: 'class factory' stamp: 'HAW 8/Jul/2024, 1:33:01 pm (UTC)'!
instVarName
	
	^'iv1'! !

!ExtractMethodApplierTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 2:02:33 pm (UTC)'!
test01InvalidMessageNameWhenKeywordWithoutParamenter

	self assertSignals: ExtractMethodNewMethod invalidMethodNameError with: 'm1:' argumentNames: {'p1'} ! !

!ExtractMethodApplierTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 2:04:21 pm (UTC)'!
test02InvalidMessageNameWhenMultipleParamentersBetweenKeywords

	self assertSignals: ExtractMethodNewMethod invalidMethodNameError with: 'm1: a b m2: c' argumentNames: {'p1'. 'p2'}.
	! !

!ExtractMethodApplierTest methodsFor: 'tests' stamp: 'HAW 19/Jul/2023, 2:05:24 pm (UTC)'!
test03WrongNumberOfArgumentsWhenParsedArgumensAreNotTheSameAsUserEnteredArguments

	self assertSignals: ExtractMethodNewMethod wrongNumberOfArgumentsGivenErrorMessage with: 'm1: a' argumentNames: {}.
	! !

!ExtractMethodApplierTest methodsFor: 'assertions' stamp: 'HAW 19/Jul/2023, 2:03:49 pm (UTC)'!
assertSignals: anErrorMessage with: userAnswer argumentNames: argumentNames  

	| argumentNodes |
	
	argumentNodes := argumentNames collect: [ :argumetName | self argumentNamed: argumetName ].
	
	self
		should: [ ExtractMethodApplier new saveBinaryOrKeywordSelector: userAnswer withArguments: argumentNodes ]
		raise: RefactoringError 
		withMessageText: anErrorMessage ! !

!ExtractMethodApplierTest methodsFor: 'object factory' stamp: 'HAW 19/Jul/2023, 2:06:29 pm (UTC)'!
argumentNamed: anArgumentName

	"The instance is not complete, but it is enoght for testing - Hernan"
	^TempVariableNode new name: anArgumentName
		! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 28/Sep/2019, 4:49:10 am (UTC)'!
assertClassHasDefined: aSelector withBody: newSourceCode

	self
		assert: newSourceCode
		equals: (classToRefactor >> aSelector) sourceCode! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 23/Jun/2019, 10:30:02 pm (UTC)'!
assertClassHasDefined: aSelector withBody: newSourceCode inCategory: aCategory

	self assertClassHasDefined: aSelector withBody: newSourceCode.
	self assert: aCategory equals: (classToRefactor >> aSelector) category! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'JEC 14/Apr/2026, 7:31:34 pm (UTC)'!
assertExtract: codeToExtract 
	from: extractingSourceCode
	to: newMessage
	withReplacements: sourceCodesToReplace
	definesNewMethodNamed: newMethodSelector
	withBody: newMethodCode
	andUpdateMethodsTo: updatedSourceCodes

	| category compiledSelectors replacements |
	
	category := #category.
	
	compiledSelectors :=self 
		compileAll: ({ extractingSourceCode } , sourceCodesToReplace) 
		classified: category.
		
	replacements := compiledSelectors withIndexCollect: [ :selector :index |
		| interval |
		
		interval := index = 1
			ifTrue: [ self intervalOf: codeToExtract locatedIn: extractingSourceCode ]
			ifFalse: [ self intervalOf: codeToExtract locatedIn: (sourceCodesToReplace at: index -1) ].
			
		ExtractMethodReplacement 
			fromInterval: interval 
			of: classToRefactor >> selector  
			to: newMessage ].

	(ExtractMethod
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: (self intervalOf: codeToExtract locatedIn: extractingSourceCode) 
			of: classToRefactor >>  (compiledSelectors first)
			to: newMessage 
			categorizedAs: category
			withArgumentNames: Dictionary new) 
		replacements: replacements) apply.

	self assertClassHasDefined: newMethodSelector withBody: newMethodCode inCategory: category.
	
	updatedSourceCodes with: compiledSelectors do: [ :updatedSourceCode :selector | 
		self assertClassHasDefined: selector withBody: updatedSourceCode inCategory: category ]! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'JEC 14/Apr/2026, 4:28:43 pm (UTC)'!
assertExtracting: codeToExtract from: originalCode named: newMessage defines: newCode andUpdates: updatedCode

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: newMessage
		withReplacements: #()
		definesNewMethodNamed: newMessage selector
		withBody: newCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'pau 17/Jul/2023, 10:25:23 pm (UTC)'!
assertExtracting: codeToExtract from: originalCode named: newMessage defines: newMethodCodeWithRenamedArg andUpdates: updatedCode withRenamedArguments: renamedArgs

	| category originalSelector |
	category := #category.
	originalSelector := classToRefactor compile: originalCode classified: category.
	
	(ExtractMethod
		fromInterval: (self intervalOf: codeToExtract locatedIn: originalCode)
		of: classToRefactor >> originalSelector
		to: newMessage
		categorizedAs: category
		withArgumentNames: renamedArgs) apply.

	self
		assertClassHasDefined: originalSelector withBody: updatedCode inCategory: category;
		assertClassHasDefined: newMessage selector withBody: newMethodCodeWithRenamedArg inCategory: category! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'JEC 14/Apr/2026, 10:50:01 pm (UTC)'!
assertWrongArgumentCountWhenParameterizableNodesDeterminedBy: aBlock

	| codeToExtract codeToBeParameterized originalCode intervalToExtract methodToExtractCodeFrom newMessage parameterizableNodes |
	codeToExtract := 'self foo: 1. self foo: 2.'.
	codeToBeParameterized := 'self foo: 3. self foo: 4'.
	originalCode := 'm1 ', codeToBeParameterized , '. ' , codeToExtract.
	methodToExtractCodeFrom := self compileMethod: originalCode.
	intervalToExtract := self intervalOf: codeToExtract locatedIn: originalCode.
	newMessage := Message selector: #m2A:m2B: arguments: #( 'a' 'b' ).

	parameterizableNodes := aBlock value: methodToExtractCodeFrom.

	self
		should: [
			(ExtractMethod
				newDefinition: (self newMethodDefinitionFrom: intervalToExtract of: methodToExtractCodeFrom to: newMessage withParameterizableNodes: parameterizableNodes)
				replacements: (Array
					with: (ExtractMethodReplacement
							fromInterval: (self intervalOf: codeToBeParameterized locatedIn: originalCode)
							of: methodToExtractCodeFrom
							to: (Message selector: #m2A:m2B: arguments: #( '3' '4' )))
					with: (ExtractMethodReplacement
							fromInterval: intervalToExtract
							of: methodToExtractCodeFrom
							to: (Message selector: #m2A:m2B: arguments: #( '1' '2' ))))) apply]
		raise: self refactoringError
		withMessageText: ExtractMethodNewMethod wrongNumberOfArgumentsGivenErrorMessage! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'jmv 26/Mar/2024, 4:55:29 pm (UTC)'!
creationWithMessage: aMessage onInterval: anIntervalToExtract ofMethod: methodToExtractCodeFrom failsWith: aRefactoringExceptionMessageEvaluationBlock errorCondition: anExceptionHandlingCondition

	self
		should: [
			ExtractMethod  
				fromInterval: anIntervalToExtract
				of: methodToExtractCodeFrom
				to: aMessage
				categorizedAs: ClassOrganizer defaultMethodCategory 
				withArgumentNames: aMessage arguments]
		raise: anExceptionHandlingCondition
		withMessageText: aRefactoringExceptionMessageEvaluationBlock! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 25/May/2020, 5:13:01 am (UTC)'!
creationWithSelectorNamed: aSelectorName onInterval: anIntervalToExtract ofMethod: aMethodToRefactor failsWith: aRefactoringErrorMessageEvaluationBlock errorCondition: anExceptionHandlingCondition 

	self
		creationWithMessage: (Message selector: aSelectorName)
		onInterval: anIntervalToExtract
		ofMethod: aMethodToRefactor
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: anExceptionHandlingCondition! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 25/May/2020, 5:09:14 am (UTC)'!
tryingToExtract: someCode from: anExistingSourceCode failsWith: aRefactoringErrorMessageEvaluationBlock

	| existingSelector |
	existingSelector := classToRefactor compile: anExistingSourceCode.

	self
		creationWithSelectorNamed: #aValidSelector
		onInterval: (self intervalOf: someCode locatedIn: anExistingSourceCode)
		ofMethod: classToRefactor >> existingSelector
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 25/May/2020, 5:11:49 am (UTC)'!
tryingToExtract: someCode from: anExistingSourceCode using: aMessage failsWith: aRefactoringErrorMessageEvaluationBlock

	| existingSelector |
	existingSelector := classToRefactor compile: anExistingSourceCode.

	self
		creationWithMessage: aMessage
		onInterval: (self intervalOf: someCode locatedIn: anExistingSourceCode)
		ofMethod: classToRefactor >> existingSelector
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 25/May/2020, 5:12:03 am (UTC)'!
tryingToExtract: someCode from: anExistingSourceCode using: aMessage raisesWarning: aRefactoringWarningMessageEvaluationBlock

	| existingSelector |
	existingSelector := classToRefactor compile: anExistingSourceCode.

	self
		creationWithMessage: aMessage
		onInterval: (self intervalOf: someCode locatedIn: anExistingSourceCode)
		ofMethod: classToRefactor >> existingSelector
		failsWith: aRefactoringWarningMessageEvaluationBlock
		errorCondition: self refactoringWarning! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 25/May/2020, 5:09:47 am (UTC)'!
tryingToExtractOnInterval: anIntervalToExtract failsWith: aRefactoringErrorMessageEvaluationBlock

	self
		creationWithSelectorNamed: #aValidSelector
		onInterval: anIntervalToExtract
		ofMethod: self methodToExtractCodeFrom
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 25/May/2020, 5:10:02 am (UTC)'!
tryingToExtractWithSelectorNamed: aSelectorName failsWith: aRefactoringErrorMessageEvaluationBlock

	| originalSource selectorOfOriginalMethod |
	originalSource := 'm1 ^ 8'.
	selectorOfOriginalMethod := classToRefactor compile: originalSource.

	self
		creationWithSelectorNamed: aSelectorName
		onInterval: (self intervalOf: '8' locatedIn: originalSource)
		ofMethod: classToRefactor >> selectorOfOriginalMethod
		failsWith: aRefactoringErrorMessageEvaluationBlock
		errorCondition: self refactoringError! !

!ExtractMethodTest methodsFor: 'assertions' stamp: 'RNG 25/May/2020, 5:08:00 am (UTC)'!
tryingToExtractWithSelectorNamed: aSelectorName raisesWarning: aRefactoringWarningMessageEvaluationBlock

	| originalSource selectorOfOriginalMethod |
	originalSource := 'm1 ^ 8'.
	selectorOfOriginalMethod := classToRefactor compile: originalSource.

	self
		creationWithSelectorNamed: aSelectorName
		onInterval: (self intervalOf: '8' locatedIn: originalSource)
		ofMethod: classToRefactor >> selectorOfOriginalMethod
		failsWith: aRefactoringWarningMessageEvaluationBlock
		errorCondition: self refactoringWarning
		! !

!ExtractMethodTest methodsFor: 'class factory' stamp: 'RNG 21/Apr/2019, 1:31:05 am (UTC)'!
classToRefactorName

	^ #ClassToExtractMethod! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'JEC 14/Apr/2026, 7:13:23 pm (UTC)'!
compileAll: sourceCodesToCompile classified: category

	^ sourceCodesToCompile 
		collect: [ :sourceCode | classToRefactor compile: sourceCode classified: category ]! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'JEC 14/Apr/2026, 10:49:48 pm (UTC)'!
compileMethod: originalCode

	| originalSelector |
	originalSelector := classToRefactor compile: originalCode classified: #category.
	^ (classToRefactor >> originalSelector) asMethodReference! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'RNG 13/May/2019, 3:21:30 am (UTC)'!
compileMethodToExtractCodeFrom

	classToRefactor compile: self sourceCodeOfMethodToExtractCodeFrom! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'JEC 11/Feb/2026, 10:48:06 pm (UTC)'!
findNodeOf: aCodeToFindNode in: methodNode

	| from codeInterval parseNodesAtFirst |

	from := methodNode sourceText 
		findString: aCodeToFindNode 
		startingAt: methodNode selectorLastPosition + 1.
	codeInterval := from to: from + aCodeToFindNode size - 1.
	
	parseNodesAtFirst := methodNode 
		parseNodesPathAt: codeInterval first 
		ifAbsent: [ self shouldNotHappen ].
	
	^ parseNodesAtFirst detect: [ :parseNode | parseNode value = codeInterval ].! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'RNG 13/May/2019, 3:22:01 am (UTC)'!
methodToExtractCodeFrom

	^ classToRefactor >> self selectorOfMethodToExtractCodeFrom! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'JEC 14/Apr/2026, 10:47:21 pm (UTC)'!
newMethodDefinitionFrom: anInterval of: aMethod to: aMessage withParameterizableNodes: nodes

	^ ExtractMethodNewMethod
		fromInterval: anInterval
		of: aMethod
		to: aMessage
		categorizedAs: #category
		withArgumentNames: Dictionary new
		withParameterizableNodes: nodes! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'RNG 13/May/2019, 3:25:05 am (UTC)'!
selectorOfMethodToExtractCodeFrom

	^ #mExtractMethodExample:! !

!ExtractMethodTest methodsFor: 'method and source code helpers' stamp: 'RNG 8/Sep/2019, 10:40:40 pm (UTC)'!
sourceCodeOfMethodToExtractCodeFrom

	^ self selectorOfMethodToExtractCodeFrom, ' anArgument

	<anAnnoyingPragma>
	| localVar |
	<anotherAnnoyingPragma>
	self m1.
	self m2: ''hey true''.
	self m3: anArgument , ''^ 21''.
	localVar _ Set with: ''hello''.
	localVar := #($x $y $z) size and: [ (9) * 3 ].
	^ localVar + ((4))'! !

!ExtractMethodTest methodsFor: 'refactoring helpers' stamp: 'JEC 30/Dec/2025, 12:24:50 am (UTC)'!
addInsVarAndSubclassify: aNewInstVar

	classToRefactor addInstVarName: aNewInstVar.
	classToRefactor := self 
		createClassNamed: ('SubclassOf' , classToRefactor name) asSymbol 
		subclassOf: classToRefactor.! !

!ExtractMethodTest methodsFor: 'refactoring helpers' stamp: 'JEC 30/Dec/2025, 12:23:23 am (UTC)'!
compileAndSubclassify: aCode

	classToRefactor compile: aCode.
	classToRefactor := self 
		createClassNamed: ('SubclassOf' , classToRefactor name) asSymbol 
		subclassOf: classToRefactor.! !

!ExtractMethodTest methodsFor: 'refactoring helpers' stamp: 'JEC 12/Feb/2026, 2:10:49 am (UTC)'!
intervalOf: aPieceOfSourceCode locatedIn: sourceCode

	^ self intervalOf: aPieceOfSourceCode locatedIn: sourceCode startingAt: 1! !

!ExtractMethodTest methodsFor: 'refactoring helpers' stamp: 'JEC 12/Feb/2026, 2:11:03 am (UTC)'!
intervalOf: aPieceOfSourceCode locatedIn: sourceCode startingAt: aPosition

	| interval |
	interval := sourceCode intervalOfSubCollection: aPieceOfSourceCode startingAt: aPosition.
	^ (interval first to: interval last - 1) asSourceCodeInterval! !

!ExtractMethodTest methodsFor: 'refactoring helpers' stamp: 'JEC 27/Mar/2026, 12:42:50 am (UTC)'!
parseNodeAt: aPosition in: aMethodNode

	^ aMethodNode 
		parseNodeIncluding: aPosition 
		ifAbsent: [ self shouldNotHappen ] ! !

!ExtractMethodTest methodsFor: 'set up' stamp: 'HAW 28/Aug/2021, 9:39:00 pm (UTC)'!
setUp

	super setUp.
	classToRefactor _ self createClassNamed: self classToRefactorName.
	! !

!ExtractMethodTest methodsFor: 'tests - parameterizable code' stamp: 'JEC 14/Apr/2026, 7:36:45 pm (UTC)'!
test151_CanExtractMethodWithSingleParameterizedCode

	| codeToExtract newMethodCode originalCode updatedCode intervalToExtract methodToExtractCodeFrom originalSelector parameterizableNodes newMessage category |

	category := #category.
	codeToExtract := 'self foo: 1.'.
 	originalCode := 'm1 self foo: 1. self foo: 2. self foo: 1'.
	newMethodCode := 'm2: a

	^ self foo: a'.
	updatedCode := 'm1 self m2: 1. self m2: 2. self m2: 1'.

	originalSelector := classToRefactor compile: originalCode classified: category .
	intervalToExtract := self intervalOf: codeToExtract locatedIn: originalCode.
	methodToExtractCodeFrom := (classToRefactor >> originalSelector) asMethodReference.
	
	parameterizableNodes := #( '1' ) collect: [ :codeString |
		self findNodeOf: codeString in: methodToExtractCodeFrom methodNode ].
	
	newMessage := Message selector: #m2: arguments: #( 'a' ).
	
	(ExtractMethod 
		newDefinition:
			(ExtractMethodNewMethod 
				fromInterval: intervalToExtract
				of: methodToExtractCodeFrom
				to: newMessage
				categorizedAs: category
				withArgumentNames: Dictionary new
				withParameterizableNodes: parameterizableNodes)  
		replacements: 
			(Array 
				with: (ExtractMethodReplacement 
						fromInterval: (
							self intervalOf: 'self foo: 1' locatedIn: originalCode startingAt: intervalToExtract last)
						of: methodToExtractCodeFrom 
						to: (Message selector: #m2: arguments: #( '1' )))
				with: (ExtractMethodReplacement 
						fromInterval: (
							self intervalOf: 'self foo: 2' locatedIn: originalCode startingAt: intervalToExtract last)
						of: methodToExtractCodeFrom 
						to: (Message selector: #m2: arguments: #( '2' )))
				with: (ExtractMethodReplacement 
						fromInterval: intervalToExtract 
						of: methodToExtractCodeFrom 
						to: (Message selector: #m2: arguments: #( '1' ))) )) apply.

	self
		assertClassHasDefined: methodToExtractCodeFrom selector withBody: updatedCode inCategory: category;
		assertClassHasDefined: newMessage selector withBody: newMethodCode inCategory: category! !

!ExtractMethodTest methodsFor: 'tests - parameterizable code' stamp: 'JEC 14/Apr/2026, 7:37:13 pm (UTC)'!
test152_CanExtractMethodWithMultipleParameterizedNodes

	| newMethodCode originalCode updatedCode intervalToExtract methodToExtractCodeFrom originalSelector parameterizableNodes newMessage methodNode category |

	category := #category.
 	originalCode := 'm1
		self bb: 10. 
		self bb: 10.
		 
		self bb: 10.
		self bb: 10.
		
		self bb: ''a''.
		self bb: ''b''.'.
	newMethodCode := 'm2: a m2: b

	self bb: a.
		self bb: b'.
	updatedCode := 'm1
		self bb: 10. 
		self bb: 10.
		 
		self m2: 10 m2: 10.
		
		self m2: ''a'' m2: ''b''.'.

	originalSelector := classToRefactor compile: originalCode classified: category .
	intervalToExtract := (41 to: 66) asSourceCodeInterval.
	methodToExtractCodeFrom := (classToRefactor >> originalSelector) asMethodReference.
	methodNode := methodToExtractCodeFrom methodNode.
	
	parameterizableNodes := OrderedCollection new.
	parameterizableNodes add: (self parseNodeAt: 50 in: methodNode).
	parameterizableNodes add: (self parseNodeAt: 65 in: methodNode).
	
	newMessage := Message selector: #m2:m2: arguments: #('a' 'b').
	
	(ExtractMethod 
		newDefinition:
			(ExtractMethodNewMethod 
				fromInterval: intervalToExtract
				of: methodToExtractCodeFrom
				to: newMessage
				categorizedAs: category
				withArgumentNames: Dictionary new
				withParameterizableNodes: parameterizableNodes)  
		replacements: 
			(Array 
				with: (ExtractMethodReplacement 
						fromInterval: intervalToExtract
						of: methodToExtractCodeFrom 
						to: (Message selector: #m2:m2: arguments: #( '10' '10' ))
						withParameterizableNodes: parameterizableNodes)
				with: (ExtractMethodReplacement 
						fromInterval: (74 to: 101) asSourceCodeInterval
						of: methodToExtractCodeFrom 
						to: (Message selector: #m2:m2: arguments: { '''a''' .  '''b''' } )) )) apply.

	self
		assertClassHasDefined: methodToExtractCodeFrom selector withBody: updatedCode inCategory: category;
		assertClassHasDefined: newMessage selector withBody: newMethodCode inCategory: category! !

!ExtractMethodTest methodsFor: 'tests - parameterizable code' stamp: 'JEC 14/Apr/2026, 10:47:58 pm (UTC)'!
test153_CannotExtractMethodWithParameterizedCodeWhenHavingNoneParameterizableNodes

	self assertWrongArgumentCountWhenParameterizableNodesDeterminedBy: [ :method | #() ]! !

!ExtractMethodTest methodsFor: 'tests - parameterizable code' stamp: 'JEC 14/Apr/2026, 10:51:27 pm (UTC)'!
test154_CannotExtractMethodWithParameterizedCodeWhenNotHavingAllTheParameterizableNodes

	self assertWrongArgumentCountWhenParameterizableNodesDeterminedBy: [ :method |
		#( '1' ) collect: [ :codeString | self findNodeOf: codeString in: method methodNode ] ]! !

!ExtractMethodTest methodsFor: 'tests - parameterizable code' stamp: 'JEC 14/Apr/2026, 10:52:16 pm (UTC)'!
test155_CannotExtractMethodWithParameterizedCodeWhenHavingMoreParameterizableNodes

	self assertWrongArgumentCountWhenParameterizableNodesDeterminedBy: [ :method | 
		#( '1' '2' '1' ) collect: [ :codeString | self findNodeOf: codeString in: method methodNode ] ]! !

!ExtractMethodTest methodsFor: 'tests - parameterizable code' stamp: 'JEC 14/Apr/2026, 10:53:07 pm (UTC)'!
test156_CannotExtractMethodWithParameterizedCodeWhenHavingNonParameterizableNodesAsIfTheyAre

	self assertWrongArgumentCountWhenParameterizableNodesDeterminedBy: [ :method | 
		#( '3' '4' ) collect: [ :codeString | self findNodeOf: codeString in: method methodNode ] ]! !

!ExtractMethodTest methodsFor: 'tests - parameterizable code' stamp: 'JEC 14/Apr/2026, 10:53:46 pm (UTC)'!
test157_CannotExtractMethodWithParameterizedCodeWhenParameterizableNodesAreNotInTheIntervalToExtract

	self assertWrongArgumentCountWhenParameterizableNodesDeterminedBy: [ :method |
		#( '1' '2' ) collect: [ :codeString |
			| node |
			node := self findNodeOf: codeString in: method methodNode.
			node key -> (1 to: 1) ] ]! !

!ExtractMethodTest methodsFor: 'tests - parameterizable code' stamp: 'JEC 14/Apr/2026, 7:39:24 pm (UTC)'!
test158_CanExtractMethodWithAReturnNodeInParameterizedCode

	| codeToExtract newMethodCode originalCode updatedCode intervalToExtract methodToExtractCodeFrom originalSelector parameterizableNodes newMessage category |

	category := #category.
	codeToExtract := 'self foo: [ ^ 1 ].'.
 	originalCode := 'm1 ' , codeToExtract , ' self foo: 2.'.
	newMethodCode := 'm2: a

	^ self foo: a'.
	updatedCode := 'm1 self m2: [ ^ 1 ]. self m2: 2.'.

	originalSelector := classToRefactor compile: originalCode classified: category .
	intervalToExtract := self intervalOf: codeToExtract locatedIn: originalCode.
	methodToExtractCodeFrom := (classToRefactor >> originalSelector) asMethodReference.
	
	parameterizableNodes := #( '[ ^ 1 ]' ) collect: [ :codeString |
		self findNodeOf: codeString in: methodToExtractCodeFrom methodNode ].
	
	newMessage := Message selector: #m2: arguments: #( 'a' ).
	
	(ExtractMethod 
		newDefinition:
			(ExtractMethodNewMethod 
				fromInterval: intervalToExtract
				of: methodToExtractCodeFrom
				to: newMessage
				categorizedAs: category
				withArgumentNames: Dictionary new
				withParameterizableNodes: parameterizableNodes)  
		replacements: 
			(Array 
				with: (ExtractMethodReplacement 
						fromInterval: (self intervalOf: codeToExtract locatedIn: originalCode)
						of: methodToExtractCodeFrom 
						to: (Message selector: #m2: arguments: #( '[ ^ 1 ]' ))
						withParameterizableNodes: parameterizableNodes)
				with: (ExtractMethodReplacement 
						fromInterval: (self intervalOf: 'self foo: 2' locatedIn: originalCode)
						of: methodToExtractCodeFrom 
						to: (Message selector: #m2: arguments: #( '2' ))) )) apply.

	self
		assertClassHasDefined: methodToExtractCodeFrom selector withBody: updatedCode inCategory: category;
		assertClassHasDefined: newMessage selector withBody: newMethodCode inCategory: category! !

!ExtractMethodTest methodsFor: 'tests - parameterizable code' stamp: 'JEC 14/Apr/2026, 8:47:34 pm (UTC)'!
test159_CanExtractMethodWithMultipleAssignmentsAndParameterizableCode

	| codeToExtract newMethodCode originalCode updatedCode intervalToExtract methodToExtractCodeFrom originalSelector parameterizableNodes newMessage category |

	category := #category.
	codeToExtract := 'a := aCollection inject: 0 into: [:acc :each | acc + each].
	b := instVarCollection inject: 1 into: [:acc :each | acc * each].'.
 	originalCode := 'm1
	| a b c d aCollection |
	aCollection := OrderedCollection new.
	' , codeToExtract , '
	c := aCollection inject: 1 into: [:acc :each | acc ^ each].
	d := instVarCollection inject: 1 into: [:acc :each | acc - each].
	^ a + b + c + d'.
	newMethodCode := 'k1: aCollection k2: aRenameMe1 k3: aRenameMe2 k4: aRenameMe3 assigningWith: anAssignmentBlock

	| a b |
	a := aCollection inject: aRenameMe1 into: aRenameMe2.
	b := instVarCollection inject: 1 into: aRenameMe3.
	^ anAssignmentBlock value: a value: b'.
	updatedCode := 'm1
	| a b c d aCollection |
	aCollection := OrderedCollection new.
	self k1: aCollection k2: 0 k3: [:acc :each | acc + each] k4: [:acc :each | acc * each] assigningWith: [ :newA :newB | a := newA. b := newB. ].
	self k1: aCollection k2: 1 k3: [:acc :each | acc ^ each] k4: [:acc :each | acc - each] assigningWith: [ :newC :newD | c := newC. d := newD. ].
	^ a + b + c + d'.

	originalSelector := classToRefactor compile: originalCode classified: category .
	intervalToExtract := self intervalOf: codeToExtract locatedIn: originalCode.
	methodToExtractCodeFrom := (classToRefactor >> originalSelector) asMethodReference.
	
	parameterizableNodes := #( '0' '[:acc :each | acc + each]' '[:acc :each | acc * each]' ) asOrderedCollection
		collect: [ :codeString | self findNodeOf: codeString in: methodToExtractCodeFrom methodNode ].
	
	parameterizableNodes addFirst: (self parseNodeAt: 74 in: methodToExtractCodeFrom methodNode) .
	
	newMessage := Message selector: #k1:k2:k3:k4: arguments: #( 'aCollection' 'aRenameMe1' 'aRenameMe2' 'aRenameMe3' ).
	
	(ExtractMethod 
		newDefinition:
			(ExtractMethodNewMethod 
				fromInterval: intervalToExtract
				of: methodToExtractCodeFrom
				to: newMessage
				categorizedAs: category
				withArgumentNames: Dictionary new
				withParameterizableNodes: parameterizableNodes)  
		replacements: 
			(Array 
				with: (ExtractMethodReplacement 
						fromInterval: (self intervalOf: codeToExtract locatedIn: originalCode)
						of: methodToExtractCodeFrom 
						to: (Message selector: #k1:k2:k3:k4: arguments: #( 'aCollection' '0' '[:acc :each | acc + each]' '[:acc :each | acc * each]' ))
						withParameterizableNodes: parameterizableNodes)
				with: (ExtractMethodReplacement 
						fromInterval: (self intervalOf: 'c := aCollection inject: 1 into: [:acc :each | acc ^ each].
	d := instVarCollection inject: 1 into: [:acc :each | acc - each].' locatedIn: originalCode)
						of: methodToExtractCodeFrom 
						to: (Message selector: #k1:k2:k3:k4: arguments: #( 'aCollection' '1' '[:acc :each | acc ^ each]' '[:acc :each | acc - each]' ))) )) apply.

	self
		assertClassHasDefined: methodToExtractCodeFrom selector withBody: updatedCode inCategory: category;
		assertClassHasDefined: #k1:k2:k3:k4:assigningWith: withBody: newMethodCode inCategory: category! !

!ExtractMethodTest methodsFor: 'tests - parameterizable code' stamp: 'JEC 31/May/2026, 7:53:22 pm (UTC)'!
test160_CanExtractMethodWithParameterizableNodeThatIncludesTempVarsDefinedOutside

	| codeToExtract newMethodCode originalCode updatedCode intervalToExtract methodToExtractCodeFrom originalSelector parameterizableNodes newMessage category |

	category := #category.
	codeToExtract := 'self foo: [ self baz: var ].'.
 	originalCode := 'm1 | var | var := 10. ' , codeToExtract , ' self foo: 2.'.
	newMethodCode := 'm2: a

	^ self foo: a'.
	updatedCode := 'm1 | var | var := 10. self m2: [ self baz: var ]. self m2: 2.'.

	originalSelector := classToRefactor compile: originalCode classified: category .
	intervalToExtract := self intervalOf: codeToExtract locatedIn: originalCode.
	methodToExtractCodeFrom := (classToRefactor >> originalSelector) asMethodReference.
	
	parameterizableNodes := #( '[ self baz: var ]' ) collect: [ :codeString |
		self findNodeOf: codeString in: methodToExtractCodeFrom methodNode ].
	
	newMessage := Message selector: #m2: arguments: #( 'a' ).
	
	(ExtractMethod 
		newDefinition:
			(ExtractMethodNewMethod 
				fromInterval: intervalToExtract
				of: methodToExtractCodeFrom
				to: newMessage
				categorizedAs: category
				withArgumentNames: Dictionary new
				withParameterizableNodes: parameterizableNodes)  
		replacements: 
			(Array 
				with: (ExtractMethodReplacement 
						fromInterval: (self intervalOf: codeToExtract locatedIn: originalCode)
						of: methodToExtractCodeFrom 
						to: (Message selector: #m2: arguments: #( '[ self baz: var ]' ))
						withParameterizableNodes: parameterizableNodes)
				with: (ExtractMethodReplacement 
						fromInterval: (self intervalOf: 'self foo: 2' locatedIn: originalCode)
						of: methodToExtractCodeFrom 
						to: (Message selector: #m2: arguments: #( '2' ))) )) apply.

	self
		assertClassHasDefined: methodToExtractCodeFrom selector withBody: updatedCode inCategory: category;
		assertClassHasDefined: newMessage selector withBody: newMethodCode inCategory: category! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:47:55 pm (UTC)'!
test009_ExtractingALiteralCreatesANewMethodAndChangesExistingCodeToCallThatNewMethod

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '4'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:14 pm (UTC)'!
test015_ExtractingAListOfStatementsCreatesANewMethodWithoutReturn

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ 'self m3.
	self m4: 5'.
	originalCode _ 'm1 ' , codeToExtract.
	newMethodCode _ 'm2

	', codeToExtract.
	updatedCode _ 'm1 self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:18 pm (UTC)'!
test017_AStringContainingTheReturnCharacterCanBeExtracted
	"this is to make sure the return is found using AST logic, not by text"

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '''^ 3'' size'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:22 pm (UTC)'!
test018_ExtractingAListOfStatementsIncludingDotsRemovesThemAndContinuesWithMethodExtraction

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '
	self m3.
	self m4: 5.'.
	originalCode _ 'm1
	' , codeToExtract.
	newMethodCode _ 'm2

	self m3.
	self m4: 5'.
	updatedCode _ 'm1
	
	self m2.'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:27 pm (UTC)'!
test019_ASelectionContainingAdditionalParenthesesAtTheBeginningAndOrEndCanBeExtracted

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '((42))'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ 42'.
	updatedCode _ 'm1 ^ ((self m2))'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:31 pm (UTC)'!
test020_ASelectionContainingALocalVariableIsParameterizedOnTheExtractedMessage

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ 'local + 3'.
	originalCode _ 'm1 | local | ^ ' , codeToExtract.
	newMethodCode _ 'm2: local

	^ ' , codeToExtract.
	updatedCode _ 'm1 | local | ^ self m2: local'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2: arguments: #('local'))
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:36 pm (UTC)'!
test021_ASelectionContainingAdditionalParenthesesOnOneSideCanBeExtractedAndItIsNotTrimmed

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '(9) * 3'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:42 pm (UTC)'!
test022_ItIsPossibleToExtractTheLastExpressionOfAMethod

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ 'localVar + ((4))'.
	originalCode _ 'm1 | localVar | ^ ' , codeToExtract.
	newMethodCode _ 'm2: localVar

	^ ' , codeToExtract.
	updatedCode _ 'm1 | localVar | ^ self m2: localVar'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2: arguments: #('localVar'))
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:46 pm (UTC)'!
test026_ItIsPossibleToExtractAnEntireBlockIncludingALocalAssignment

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '[ |something| something _ 3 ]'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:50 pm (UTC)'!
test027_ItIsPossibleToExtractExpressionsWithOptimizedSelectors

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '2 ifNil: [ #boo ] ifNotNil: [ #yay ]'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:05 pm (UTC)'!
test029_ExtractingABinaryExpressionToAKeywordMessageIntroducesParenthesesToNotBreakOriginalPrecedence

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '2 + arg'.
	originalCode _ 'm1: arg ^ ' , codeToExtract , ' * 3'.
	newMethodCode _ 'm2: arg

	^ ' , codeToExtract.
	updatedCode _ 'm1: arg ^ (self m2: arg) * 3'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2: arguments: #('arg'))
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:50:05 pm (UTC)'!
test030_ItIsPossibleToExtractAQuotedExpression

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '`3 + 4`'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:50:11 pm (UTC)'!
test031_ItIsPossibleToExtractExpressionsWithOptimizedSelectorsWhereTheReceiverIsNotASimpleLiteral

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '2 factorial ifNotNil: [ #boo ]'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:50:27 pm (UTC)'!
test032_ItIsPossibleToExtractATemporaryDeclarationOfABlockIfItIsNotUsedOutsideOfSelectionInterval

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '| a | a _ 3 factorial'.
	originalCode _ 'm1 ^ [ ' , codeToExtract , ' ]'.
	newMethodCode _ 'm2

	' , codeToExtract.
	updatedCode _ 'm1 ^ [ self m2 ]'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:55 pm (UTC)'!
test033_ItIsPossibleToExtractATemporaryDeclarationIfItIsNotUsedOutsideOfSelectionInterval

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '| a | a _ 3 factorial'.
	originalCode _ 'm1 ' , codeToExtract.
	newMethodCode _ 'm2

	' , codeToExtract.
	updatedCode _ 'm1 self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:49:59 pm (UTC)'!
test036_ItIsPossibleToExtractACascadeExpression

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '(3 + 2) factorial; yourself'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ ' , codeToExtract.
	updatedCode _ 'm1 ^ self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:50:17 pm (UTC)'!
test037_ItIsPossibleATempDeclarationAlongWithABlock

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '| temp | []'.
	originalCode _ 'm1 ' , codeToExtract.
	newMethodCode _ 'm2

	' , codeToExtract.
	updatedCode _ 'm1 self m2'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:50:21 pm (UTC)'!
test038_ItIsPossibleToExtractCodeThatContainsMultipleParenthesisWithSpacesBetweenThem

	| codeToExtract newMethodCode originalCode updatedCode |
	codeToExtract _ '(  (3 + 4))'.
	originalCode _ 'm1 ^ ' , codeToExtract.
	newMethodCode _ 'm2

	^ 3 + 4'.
	updatedCode _ 'm1 ^ (  (self m2))'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:50:32 pm (UTC)'!
test044_CanExtractManyStatementsInsideABlock

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract _ 'self m1.
	self m2'.
	originalCode _ 'm1 true ifTrue: [' , codeToExtract, ']'.
	newMethodCode _ 'm2

	self m1.
	self m2'.
	updatedCode _ 'm1 true ifTrue: [self m2]'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m2)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:50:37 pm (UTC)'!
test045_CanExtractCodeWithMoreThanOneBlockAndABlockAtTheEndInsideAnotherBlock

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract _ 'true ifTrue: [ self m1 ].
	true ifTrue: [ self m2 ].'.
	originalCode _ 'm1 [true] whileTrue: [ ' , codeToExtract, '
	self m3]'.
	newMethodCode _ 'm4

	true ifTrue: [ self m1 ].
	true ifTrue: [ self m2 ]'.
	updatedCode _ 'm1 [true] whileTrue: [ self m4.
	self m3]'.

	self
		assertExtracting: codeToExtract from: originalCode named: (Message selector: #m4)
		defines: newMethodCode andUpdates: updatedCode! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:50:41 pm (UTC)'!
test046_CanModifyArgumentsNamesInExtractedMethod

	| codeToExtract newMethodCodeWithRenamedArg originalCode updatedCode newMessage |
	
	codeToExtract := 'true ifTrue: [ arg ].'.
	originalCode := 'm1: arg
	[true] whileTrue: [ ' , codeToExtract, ']'.
	newMethodCodeWithRenamedArg := 'm3: argModified

	^ true ifTrue: [ argModified ]'.
	updatedCode := 'm1: arg
	[true] whileTrue: [ self m3: arg.]'.
	
	newMessage := Message selector: #m3: argument: 'arg'. 
	
	self 
		assertExtracting: codeToExtract 
		from: originalCode 
		named: newMessage 
		defines: newMethodCodeWithRenamedArg 
		andUpdates: updatedCode 
		withRenamedArguments: (Dictionary with:'arg'->'argModified' ). 
	! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:50:53 pm (UTC)'!
test047_OnlyModifiedArgumentsNamesAreModifiedInExtractedMethod

	| codeToExtract newMethodCodeWithRenamedArg originalCode updatedCode newMessage |
	
	codeToExtract := 'arg1 ifTrue: [ arg2 ].'.
	originalCode := 'm1: arg1 m2: arg2
	[true] whileTrue: [ ' , codeToExtract, ']'.
	newMethodCodeWithRenamedArg := 'm3: arg1Modified m4: arg2

	^ arg1Modified ifTrue: [ arg2 ]'.
	updatedCode := 'm1: arg1 m2: arg2
	[true] whileTrue: [ self m3: arg1 m4: arg2.]'.
	
	newMessage := Message selector: #m3:m4: arguments: #('arg1' 'arg2'). 
	self 
		assertExtracting: codeToExtract 
		from: originalCode 
		named: newMessage 
		defines: newMethodCodeWithRenamedArg 
		andUpdates: updatedCode 
		withRenamedArguments: (Dictionary with:'arg1'->'arg1Modified'). 
	
! !

!ExtractMethodTest methodsFor: 'tests - successful' stamp: 'JEC 13/Apr/2026, 1:50:49 pm (UTC)'!
test049_CanExtractFromOptimizedMessageSends

	| codeToExtract newMethodCodeWithRenamedArg originalCode updatedCode newMessage |
	
	codeToExtract := 'x + 1'.
	originalCode := 'm1 1 to: 2 do: [ :x | x + 1]'.
	newMethodCodeWithRenamedArg := 'm2: x

	^ x + 1'.
	updatedCode := 'm1 1 to: 2 do: [ :x | self m2: x]'.
	
	newMessage := Message selector: #m2: argument: 'x'. 
	self 
		assertExtracting: codeToExtract 
		from: originalCode 
		named: newMessage 
		defines: newMethodCodeWithRenamedArg 
		andUpdates: updatedCode 
	
! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:46:22 pm (UTC)'!
test001_NewSelectorShouldNotBeEmpty

	self
		tryingToExtractWithSelectorNamed: #''
		failsWith: [ NewSelectorPrecondition newSelectorCanNotBeEmptyErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:46:26 pm (UTC)'!
test002_NewSelectorShouldNotContainSeparators

	self
		tryingToExtractWithSelectorNamed: #'my selector'
		failsWith: [ NewSelectorPrecondition newSelectorCanNotContainSeparatorsErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:46:30 pm (UTC)'!
test003_ExtractingToSelectorAlreadyDefinedInTheClassRaisesAWarning

	self compileMethodToExtractCodeFrom.
	self
		tryingToExtractWithSelectorNamed: self selectorOfMethodToExtractCodeFrom
		raisesWarning: [
			NewSelectorPrecondition
				warningMessageFor: self selectorOfMethodToExtractCodeFrom
				isAlreadyDefinedIn: classToRefactor ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:46:38 pm (UTC)'!
test004_NewSelectorShouldNotStartWithANumber

	self
		tryingToExtractWithSelectorNamed: #'2selector'
		failsWith: [ NewSelectorPrecondition invalidStartingCharacterOfNewSelectorErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:46:55 pm (UTC)'!
test005_NewUnarySelectorShouldNotStartWithASymbol

	self
		tryingToExtractWithSelectorNamed: #-
		failsWith: [ NewSelectorPrecondition invalidStartingCharacterOfNewSelectorErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:47:16 pm (UTC)'!
test006_IntervalToExtractIsNotBeforeMethodSourceCodeBoundaries

	self compileMethodToExtractCodeFrom.
	self
		tryingToExtractOnInterval: (-1 to: 2) asSourceCodeInterval
		failsWith: [ ExtractMethodNewMethod outOfBoundsSelectionErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:47:38 pm (UTC)'!
test007_IntervalToExtractIsNotAfterMethodSourceCodeBoundaries

	self compileMethodToExtractCodeFrom.
	self
		tryingToExtractOnInterval: (1 to: self sourceCodeOfMethodToExtractCodeFrom size + 2) asSourceCodeInterval
		failsWith: [ ExtractMethodNewMethod outOfBoundsSelectionErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:48:09 pm (UTC)'!
test010_IntervalToExtractDoesNotContainsPartOfTheOriginalSelector

	self
		tryingToExtract: 'm1 ^ 8'
		from: 'm1 ^ 8'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:48:21 pm (UTC)'!
test011_IntervalToExtractDoesNotContainsPartOfLocalVariablesDefinition

	self
		tryingToExtract: '| localVar |'
		from: 'm1 | localVar | ^ localVar + 3'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingTemporaryVariablesDefinition ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:48:36 pm (UTC)'!
test012_IntervalToExtractDoesNotContainsPartOfPragmasDefinition

	self
		tryingToExtract: '<annoyingPragma>'
		from: 'm1 <annoyingPragma>'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:03 pm (UTC)'!
test013_IntervalToExtractShouldBeAValidSmalltalkExpression

	self
		tryingToExtract: '+ ((4))'
		from: 'm1 ^ 3 + ((4))'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:26 pm (UTC)'!
test014_ItIsNotPossibleToExtractTheLeftSideOfAnAssignment

	| firstOccurrence secondOccurrence intervalToExtract |
	
	self compileMethodToExtractCodeFrom.
	firstOccurrence := self sourceCodeOfMethodToExtractCodeFrom
		indexOfSubCollection: 'localVar' startingAt: 1.
	secondOccurrence := self sourceCodeOfMethodToExtractCodeFrom
		indexOfSubCollection: 'localVar' startingAt: firstOccurrence + 1.
	intervalToExtract := secondOccurrence toSelfPlus: 'localVar' size.
	self
		tryingToExtractOnInterval: intervalToExtract asSourceCodeInterval
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingLeftSideOfAssignment ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:07 pm (UTC)'!
test016_CanNotExtractPartOfALiteral

	self
		tryingToExtract: 'true'
		from: 'm1 ^ ''hey true'''
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:10 pm (UTC)'!
test023_CanNotExtractWithAnInvalidInterval

	self compileMethodToExtractCodeFrom.
	self
		tryingToExtractOnInterval: (10 to: 9) asSourceCodeInterval
		failsWith: [ ExtractMethodNewMethod noSelectionErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:15 pm (UTC)'!
test025_NewUnarySelectorShouldNotContainInvalidCharacters

	self
		tryingToExtractWithSelectorNamed: 'hola)' asSymbol
		failsWith: [ NewSelectorPrecondition invalidCharacterInsideNewSelectorErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:39 pm (UTC)'!
test028_ItIsNotPossibleToExtractALocalVariableInsideOfItsDeclaration

	self
		tryingToExtract: 'localVar'
		from: 'm1 | localVar | ^ #foo'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingTemporaryVariablesDefinition ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:19 pm (UTC)'!
test034_TryingToExtractAMethodWithLessArgumentsThanNeededFails

	self
		tryingToExtract: 'localVar1 + localVar2 + 2'
		from: 'm1 | localVar1 localVar2 | ^ localVar1 + localVar2 + 2'
		using: (Message selector: #m1: arguments: #('localVar1'))
		failsWith: [ ExtractMethodNewMethod wrongNumberOfArgumentsGivenErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:56 pm (UTC)'!
test035_TryingToExtractAMethodWithMoreArgumentsThanNeededFails

	self
		tryingToExtract: 'localVar1 + localVar2 + 2'
		from: 'm1 | localVar1 localVar2 | ^ localVar1 + localVar2 + 2'
		using: (Message selector: #m1:m2:m3: arguments: #('localVar1' 'localVar2' 'localVar3'))
		failsWith: [ ExtractMethodNewMethod wrongNumberOfArgumentsGivenErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:34 pm (UTC)'!
test039_TryingToExtractToAMethodThatExistsInASuperclassRaisesAWarning

	self
		tryingToExtract: '42'
		from: 'm1 ^ 42'
		using: (Message selector: #yourself)
		raisesWarning: [ NewSelectorPrecondition warningMessageFor: #yourself isAlreadyDefinedIn: Object ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:30 pm (UTC)'!
test040_CanNotExtractTemporaryAssignmentWithDeclarationIfTheVariableIsUsedOutsideOfSelectionInterval

	self
		tryingToExtract: '|var| var _ 42.'
		from: 'm1 |var| var _ 42. ^ var'
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForTemporaryAssignmentWithUsagesOutsideOfSelectionInterval ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:44 pm (UTC)'!
test041_CannotExtractPartOfMethodSignature

	self
		tryingToExtract: 'arg'
		from: 'm1: arg ^ self'
		using: (Message selector: #m2: arguments: #('arg'))
		failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForExtractingPartOfMethodSignature ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:51:52 pm (UTC)'!
test048_NewSelectorMustBeUnaryWhenNoArguments

	self
		tryingToExtract: '10'
		from: 'm1 10'
		using: (Message selector: #m2: arguments: #())
		failsWith: [ ExtractMethodNewMethod wrongNumberOfArgumentsGivenErrorMessage ]! !

!ExtractMethodTest methodsFor: 'tests - validations' stamp: 'JEC 13/Apr/2026, 1:48:51 pm (UTC)'!
test053_NotWorkingCase_IncompleteCascadeMessage

	self shouldFail: [
		self
			tryingToExtract: 'self m1;m2'
			from: 'm1 self m1;m2;m3'
			failsWith: [ SourceCodeOfMethodToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]
	]! !

!ExtractMethodTest methodsFor: 'tests - many repetitions' stamp: 'JEC 13/Apr/2026, 1:45:51 pm (UTC)'!
test042_ExtractsManyRepetitionsOnSameMethod

	| sourceMethodName finder sourceMethod extractMethod intervalToExtract newMessage |
	
	sourceMethodName := classToRefactor compile: 'm1 10. 10'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (4 to: 5) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	extractMethod := ExtractMethod 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: intervalToExtract 
			of: sourceMethod 
			to: newMessage 
			categorizedAs: #something
			withArgumentNames: Dictionary new)
		replacements: finder replacements.
		
	extractMethod apply.
	
	self assert: 
 'm2

	^ 10' equals: (classToRefactor >> #m2) sourceCode.
	self assert: 'm1 self m2. self m2' equals: (classToRefactor >> #m1) sourceCode.
	! !

!ExtractMethodTest methodsFor: 'tests - many repetitions' stamp: 'JEC 13/Apr/2026, 1:45:56 pm (UTC)'!
test043_ExtractsRepetitionsOnMoreThanOneMethod

	| sourceMethodName finder sourceMethod extractMethod intervalToExtract newMessage |
	
	sourceMethodName := classToRefactor compile: 'm1 10'.
	classToRefactor compile: 'm1b 100+10'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (4 to: 5) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	extractMethod := ExtractMethod 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: intervalToExtract 
			of: sourceMethod 
			to: newMessage 
			categorizedAs: #something
			withArgumentNames: Dictionary new)
		replacements: finder replacements.
		
	extractMethod apply.
	
	self assert: 'm1 self m2' equals: (classToRefactor >> #m1) sourceCode.
	self assert: 'm1b 100+self m2' equals: (classToRefactor >> #m1b) sourceCode.
	! !

!ExtractMethodTest methodsFor: 'tests - many repetitions' stamp: 'JEC 13/Apr/2026, 1:46:01 pm (UTC)'!
test050_ExtractsManyRepetitionsWithMultipleStatementsOnSameMethod

	| sourceMethodName finder sourceMethod extractMethod intervalToExtract newMessage |
	
	sourceMethodName := classToRefactor compile: 'm1 10 factorial. 10 factorial. true ifTrue: [ 10 factorial. 10 factorial]'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (4 to: 29) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	extractMethod := ExtractMethod 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: intervalToExtract 
			of: sourceMethod 
			to: newMessage 
			categorizedAs: #something
			withArgumentNames: Dictionary new)
		replacements: finder replacements.
		
	extractMethod apply.
	
	self assert: 
 'm2

	10 factorial. 10 factorial' equals: (classToRefactor >> #m2) sourceCode.
	self assert: 'm1 self m2. true ifTrue: [ self m2]' equals: (classToRefactor >> #m1) sourceCode.
	! !

!ExtractMethodTest methodsFor: 'tests - many repetitions' stamp: 'JEC 13/Apr/2026, 1:46:06 pm (UTC)'!
test051_ExtractsManyRepetitionsWithMoreThanTwoStatementsOnSameMethod

	| sourceMethodName finder sourceMethod extractMethod intervalToExtract newMessage |
	
	sourceMethodName := classToRefactor compile: 'm1 10 factorial. 15 factorial. 20 factorial. true ifTrue: [ 10 factorial. 15 factorial. 20 factorial]'.
	
	sourceMethod := (classToRefactor >> sourceMethodName) asMethodReference.
	intervalToExtract := (4 to: 43) asSourceCodeInterval.
	newMessage := Message selector: #m2.
	finder := ExtractMethodReplacementsFinder ofCodeIn: intervalToExtract at: sourceMethod to: newMessage. 
	finder value.
	
	extractMethod := ExtractMethod 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: intervalToExtract 
			of: sourceMethod 
			to: newMessage 
			categorizedAs: #something
			withArgumentNames: Dictionary new)
		replacements: finder replacements.
		
	extractMethod apply.
	
	self assert: 
 'm2

	10 factorial. 15 factorial. 20 factorial' equals: (classToRefactor >> #m2) sourceCode.
	self assert: 'm1 self m2. true ifTrue: [ self m2]' equals: (classToRefactor >> #m1) sourceCode.
	! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:26:43 pm (UTC)'!
test054_WhenExtractingATemporaryVariableAssignment_RemovesTheDeclarationIfItIsNotUsed

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3'.
	originalCode := 'm1 | a | ' , codeToExtract.
	newMethodCode := 'm2

	| a |
	^ ', codeToExtract.
	updatedCode := 'm1 self m2'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:26:31 pm (UTC)'!
test055_WhenExtractingATemporaryVariableAssignment_DontRemoveAnyOtherDeclaration

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3.'.
	originalCode := 'm1 | a b | ' , codeToExtract , ' ^ b := 1'.
	newMethodCode := 'm2

	| a |
	^ a := 3'.
	updatedCode := 'm1 | b | self m2. ^ b := 1'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:26:22 pm (UTC)'!
test056_WhenExtractingATempVarAssignment_AndTheVarIsWrittenAfterTheExtractedCode_AssignsTheReturnedValueToTheTemporary

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3.'.
	originalCode := 'm1 | a | ' , codeToExtract, ' a := 4'.
	newMethodCode := 'm2

	| a |
	', codeToExtract ,'
	^ a'.
	updatedCode := 'm1 | a | a := self m2. a := 4'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:26:14 pm (UTC)'!
test057_WhenExtractingATempVarAssignment_AndTheVarIsReadAfterTheExtractedCode_AssignsTheReturnedValueToTheTemporary

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3.'.
	originalCode := 'm1 | a | ' , codeToExtract, ' a + 4'.
	newMethodCode := 'm2

	| a |
	', codeToExtract, '
	^ a'.
	updatedCode := 'm1 | a | a := self m2. a + 4'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:26:05 pm (UTC)'!
test058_WhenExtractingATempVarAssignment_AndTheVarIsUsedAsAnArgumentAfterTheExtractedCode_AssignsTheReturnedValueToTheTemporary

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3.'.
	originalCode := 'm1 | a | ' , codeToExtract, ' 4 + a'.
	newMethodCode := 'm2

	| a |
	', codeToExtract, '
	^ a'.
	updatedCode := 'm1 | a | a := self m2. 4 + a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:25:54 pm (UTC)'!
test059_WhenExtractingATempVarAssignment_AndTheVarIsReturnedAfterTheExtractedCode_AssignsTheReturnedValueToTheTemporary

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3.'.
	originalCode := 'm1 | a | ' , codeToExtract, ' ^ a'.
	newMethodCode := 'm2

	| a |
	', codeToExtract, '
	^ a'.
	updatedCode := 'm1 | a | a := self m2. ^ a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:25:43 pm (UTC)'!
test060_WhenExtractingATempVarAssignment_AndTheVarIsReadAfterTheExtractedCode_AssignsTheReturnedValueToTheTemporary

	"This is an edge case"

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3.'.
	originalCode := 'm1 | a | ' , codeToExtract, ' a'.
	newMethodCode := 'm2

	| a |
	', codeToExtract, '
	^ a'.
	updatedCode := 'm1 | a | a := self m2. a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:25:31 pm (UTC)'!
test061_WhenExtractingATempVarAssignment_AndTheVarIsReadInTheExtractedCode_TheNewMethodReturnsTheVariable

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 1. self foo: a.'.
	originalCode := 'm1 | a | ' , codeToExtract, ' ^ a'.
	newMethodCode := 'm2

	| a |
	', codeToExtract, '
	^ a'.
	updatedCode := 'm1 | a | a := self m2. ^ a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:25:23 pm (UTC)'!
test062_CanExtractATempVarAssignmentThatTheVarAtTheBeginingAndEndOfTheExtractedCode

	"
	This is a bugfix in SourceCodeOfMethodToBeExtractedPrecondition>>isLeftSideOfAssignment
	Precisely in SourceCodeOfMethodToBeExtractedPrecondition>>startAndEndParseNodesAreTheSame.
	It only checked that the nodes were the same but not the intervals.
	"

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 1. self foo: a.'.
	originalCode := 'm1 | a | ' , codeToExtract, ' a'.
	newMethodCode := 'm2

	| a |
	', codeToExtract, '
	^ a'.
	updatedCode := 'm1 | a | a := self m2. a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:25:03 pm (UTC)'!
test063_WhenExtractingMultipleTempVar_OneIsAssignedAndTheOtherIsRead_TheOneAssignedIsReturned

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'b := a + 2'.
	originalCode := 'm1 | a b | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| b |
	^ ', codeToExtract.
	updatedCode := 'm1 | a | a := 1. self m2: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:24:56 pm (UTC)'!
test064_WhenExtractingATempVarAssignment_AndTheExtractedCodeReadsTheTempVarBeforeTheAssignment_DeclaresANewTempVarForTheAssignmentAndReturnsIt

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := a + 2'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| newA |
	newA := a + 2.
	^ newA'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:24:50 pm (UTC)'!
test065_WhenExtractingATempVarAssignment_AndTheExtractedCodeReadsTheTempVarBeforeTheAssignment_DeclaresANewTempVarForTheAssignmentAndReturnsIt

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'self foo: a. a := a + 2'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| newA |
	self foo: a. newA := a + 2.
	^ newA'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:24:44 pm (UTC)'!
test066_WhenExtractingATempVarAssignment_AndTheExtractedCodeReadsTheTempVarAfterTheAssignment_DeclaresANewTempVarForTheAssignmentAndUsesItAfterThat

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := a + 2. self foo: a'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| newA |
	newA := a + 2. self foo: newA.
	^ newA'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:24:37 pm (UTC)'!
test067_WhenExtractingATempVarAssignment_WithMultipleAssignments_DeclaresANewTempVarForTheFirstAssignmentAndUsesItAfterThat

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'a := a + 2. self foo: a. a := a + 1'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| newA |
	newA := a + 2. self foo: newA. newA := newA + 1.
	^ newA'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:24:31 pm (UTC)'!
test068_WhenExtractingATempVarAssignment_WithSameMultipleAssignments_DeclaresANewTempVarForTheFirstAssignmentAndUsesItAfterThat

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'a := a + 2. self foo: a. a := a + 2'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| newA |
	newA := a + 2. self foo: newA. newA := newA + 2.
	^ newA'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:23:42 pm (UTC)'!
test069_CanExtractMultipleTemporaryVariableAssignmentsAlongWithReadsOfSameVariable

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'self foo: a. a := a + 2. self bar: a. a := 5'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| newA |
	self foo: a. newA := a + 2. self bar: newA. newA := 5.
	^ newA'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:23:20 pm (UTC)'!
test070_CanExtractTemporaryVariableAssignmentsOfMultipleVariables

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'a := 1. b := 1'.
	originalCode := 'm1 | a b | ' , codeToExtract.
	newMethodCode := 'm2

	| a b |
	', codeToExtract.
	updatedCode := 'm1 self m2'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:23:06 pm (UTC)'!
test071_WhenExtractingMultipleTempVarsAssignments_AndOnlyOneIsUsedBeforeExtractedCode_IsReturnedAndAssigned

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 1. b := 1'.
	originalCode := 'm1 | a b | a := 2. ' , codeToExtract.
	newMethodCode := 'm2

	| a b |
	a := 1. b := 1.
	^ a'.
	updatedCode := 'm1 | a | a := 2. a := self m2'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:22:55 pm (UTC)'!
test072_WhenExtractingMultipleTempVarsAssignments_AndAllAreUsedBeforeExtractedCode_AllAreAssignedInABlock

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'a := 1. b := 1'.
	originalCode := 'm1 | a b | a := 2. b := 2. ' , codeToExtract.
	newMethodCode := 'm2AssigningWith: anAssignmentBlock

	| a b |
	a := 1. b := 1.
	^ anAssignmentBlock value: a value: b'.
	updatedCode := 'm1 | a b | a := 2. b := 2. self m2AssigningWith: [ :newA :newB | a := newA. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2AssigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:22:38 pm (UTC)'!
test073_WhenExtractingMultipleTempVarsAssignments_AndOnlyOneIsReadInExtractedCode_UsesANewTempVar

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'self foo: a. a := 1. b := 1.'.
	originalCode := 'm1 | a b | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| newA b |
	self foo: a. newA := 1. b := 1.
	^ newA'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a.'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:22:14 pm (UTC)'!
test074_WhenExtractingMultipleTempVarsAssignments_AndAllAreReadInExtractedCode_UsesNewTempVars

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'self foo: a. self bar: b. a := 1. b := 1.'.
	originalCode := 'm1 | a b | a := 1. b := 2. ' , codeToExtract.
	newMethodCode := 'm2A: a m2B: b assigningWith: anAssignmentBlock

	| newA newB |
	self foo: a. self bar: b. newA := 1. newB := 1.
	^ anAssignmentBlock value: newA value: newB'.
	updatedCode := 'm1 | a b | a := 1. b := 2. self m2A: a m2B: b assigningWith: [ :newA :newB | a := newA. b := newB. ].'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2A:m2B: arguments: #('a' 'b'))
		withReplacements: #()
		definesNewMethodNamed: #m2A:m2B:assigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:21:26 pm (UTC)'!
test075_CanExtractTemporaryVariableAssignmentsFromABlock

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'a := 1'.
	originalCode := 'm1 [ | a |' , codeToExtract, ' ]'.
	newMethodCode := 'm2

	| a |
	^ a := 1'.
	updatedCode := 'm1 [ self m2 ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:21:05 pm (UTC)'!
test076_CanExtractMultipleTemporaryVariablesAssignments_WhenTheDeclarationIsInDifferentOrdenThanAssignation

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'a := 1. b := 2'.
	originalCode := 'm1 [ | b a | ' , codeToExtract, ' ]'.
	newMethodCode := 'm2

	| b a |
	a := 1. b := 2'.
	updatedCode := 'm1 [ self m2 ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:20:47 pm (UTC)'!
test077_WhenTheNewTempVarExistsAsAnInstVar_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	classToRefactor addInstVarName: 'newA'.
	codeToExtract := 'a := a + 2'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| newA1 |
	newA1 := a + 2.
	^ newA1'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:20:38 pm (UTC)'!
test078_WhenTheNewTempVarExistsAsAnInstVar_AndTheAnotherNewOneAlso_CreatesANewOneWithAConsecutiveNumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	classToRefactor addInstVarName: 'newA'.
	classToRefactor addInstVarName: 'newA1'.
	codeToExtract := 'a := a + 2'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| newA2 |
	newA2 := a + 2.
	^ newA2'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:20:29 pm (UTC)'!
test079_WhenTheNewTempVarExistsAsAnInstVarInASuperclass_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	self addInsVarAndSubclassify: 'newA'.
	codeToExtract := 'a := a + 2'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a

	| newA1 |
	newA1 := a + 2.
	^ newA1'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:20:20 pm (UTC)'!
test080_WhenTheNewTempVarExistsAsATempVarInTheMethod_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := ' a := a + 2. b := a + 2'.
	originalCode := 'm1 | a newA b | a := 1. b := 2.' , codeToExtract.
	newMethodCode := 'm2: a assigningWith: anAssignmentBlock

	| newA1 b |
	newA1 := a + 2. b := newA1 + 2.
	^ anAssignmentBlock value: newA1 value: b'.
	updatedCode := 'm1 | a newA b | a := 1. b := 2. self m2: a assigningWith: [ :newA1 :newB | a := newA1. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:assigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:20:10 pm (UTC)'!
test081_WhenTheNewTempVarExistsAsATempVarInTheMethod_AndTheAnotherNewOneAlso_CreatesANewOneWithAConsecutiveNumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := ' a := a + 2. b := a + 2'.
	originalCode := 'm1 | a newA b newA1 | a := 1. b := 2.' , codeToExtract.
	newMethodCode := 'm2: a assigningWith: anAssignmentBlock

	| newA2 b |
	newA2 := a + 2. b := newA2 + 2.
	^ anAssignmentBlock value: newA2 value: b'.
	updatedCode := 'm1 | a newA b newA1 | a := 1. b := 2. self m2: a assigningWith: [ :newA2 :newB | a := newA2. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:assigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:19:58 pm (UTC)'!
test082_WhenTheNewTempVarExistsAsATempVarInTheMethod_AndTheAnotherNewOneAsAnInstVar_CreatesANewOneWithAConsecutiveNumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	classToRefactor addInstVarName: 'newA1'.
	codeToExtract := ' a := a + 2. b := a + 2'.
	originalCode := 'm1 | a newA b | a := 1. b := 2.' , codeToExtract.
	newMethodCode := 'm2: a assigningWith: anAssignmentBlock

	| newA2 b |
	newA2 := a + 2. b := newA2 + 2.
	^ anAssignmentBlock value: newA2 value: b'.
	updatedCode := 'm1 | a newA b | a := 1. b := 2. self m2: a assigningWith: [ :newA2 :newB | a := newA2. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:assigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:19:29 pm (UTC)'!
test083_WhenTheNewMethodWithAssignmentBlockExistsInClass_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	classToRefactor compile: 'm2AssigningWith: arg'.
	codeToExtract := 'a := 2. b := 2'.
	originalCode := 'm1 | a b | a := 1. b := 1. ' , codeToExtract.
	newMethodCode := 'm2AssigningWith1: anAssignmentBlock

	| a b |
	a := 2. b := 2.
	^ anAssignmentBlock value: a value: b'.
	updatedCode := 'm1 | a b | a := 1. b := 1. self m2AssigningWith1: [ :newA :newB | a := newA. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2AssigningWith1:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:19:16 pm (UTC)'!
test084_WhenTheNewMethodWithAssignmentBlockExistsInClass_AndTheAnotherNewOneAlsoExists_CreatesANewOneWithAConsecutiveNumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	classToRefactor compile: 'm2AssigningWith: arg'.
	classToRefactor compile: 'm2AssigningWith1: arg'.
	codeToExtract := 'a := 2. b := 2'.
	originalCode := 'm1 | a b | a := 1. b := 1. ' , codeToExtract.
	newMethodCode := 'm2AssigningWith2: anAssignmentBlock

	| a b |
	a := 2. b := 2.
	^ anAssignmentBlock value: a value: b'.
	updatedCode := 'm1 | a b | a := 1. b := 1. self m2AssigningWith2: [ :newA :newB | a := newA. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2AssigningWith2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:18:56 pm (UTC)'!
test085_WhenTheNewMethodWithAssignmentBlockExistsInClass_WithAnotherParameters_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	classToRefactor compile: 'm2: arg1 assigningWith: arg2'.
	codeToExtract := 'a := a + 2. b := 2'.
	originalCode := 'm1 | a b | a := 1. b := 1. ' , codeToExtract.
	newMethodCode := 'm2: a assigningWith1: anAssignmentBlock

	| newA b |
	newA := a + 2. b := 2.
	^ anAssignmentBlock value: newA value: b'.
	updatedCode := 'm1 | a b | a := 1. b := 1. self m2: a assigningWith1: [ :newA :newB | a := newA. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:assigningWith1:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:18:49 pm (UTC)'!
test086_WhenTheNewMethodWithAssignmentBlockExistsInASuperclass_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	self compileAndSubclassify: 'm2: arg1 assigningWith: arg2'. 
	codeToExtract := 'a := a + 2. b := 2'.
	originalCode := 'm1 | a b | a := 1. b := 1. ' , codeToExtract.
	newMethodCode := 'm2: a assigningWith1: anAssignmentBlock

	| newA b |
	newA := a + 2. b := 2.
	^ anAssignmentBlock value: newA value: b'.
	updatedCode := 'm1 | a b | a := 1. b := 1. self m2: a assigningWith1: [ :newA :newB | a := newA. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:assigningWith1:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:18:40 pm (UTC)'!
test087_WhenTheNewBlockTempVarParameterExistsAsAnInstVar_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	classToRefactor addInstVarName: 'anAssignmentBlock'.
	codeToExtract := 'a := a + 2. b := 2'.
	originalCode := 'm1 | a b | a := 1. b := 1. ' , codeToExtract.
	newMethodCode := 'm2: a assigningWith: anAssignmentBlock1

	| newA b |
	newA := a + 2. b := 2.
	^ anAssignmentBlock1 value: newA value: b'.
	updatedCode := 'm1 | a b | a := 1. b := 1. self m2: a assigningWith: [ :newA :newB | a := newA. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:assigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:18:29 pm (UTC)'!
test088_WhenTheNewBlockTempVarParameterExistsAsAnInstVar_AndAlsoNextOne_CreatesANewOneWithTheFollowingNumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	classToRefactor addInstVarName: 'anAssignmentBlock'.
	classToRefactor addInstVarName: 'anAssignmentBlock1'.
	codeToExtract := 'a := a + 2. b := 2'.
	originalCode := 'm1 | a b | a := 1. b := 1. ' , codeToExtract.
	newMethodCode := 'm2: a assigningWith: anAssignmentBlock2

	| newA b |
	newA := a + 2. b := 2.
	^ anAssignmentBlock2 value: newA value: b'.
	updatedCode := 'm1 | a b | a := 1. b := 1. self m2: a assigningWith: [ :newA :newB | a := newA. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:assigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:18:10 pm (UTC)'!
test089_WhenTheNewBlockTempVarParameterExistsAsAnInstVarInHierarchy_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	self addInsVarAndSubclassify: 'anAssignmentBlock'.
	codeToExtract := 'a := a + 2. b := 2'.
	originalCode := 'm1 | a b | a := 1. b := 1. ' , codeToExtract.
	newMethodCode := 'm2: a assigningWith: anAssignmentBlock1

	| newA b |
	newA := a + 2. b := 2.
	^ anAssignmentBlock1 value: newA value: b'.
	updatedCode := 'm1 | a b | a := 1. b := 1. self m2: a assigningWith: [ :newA :newB | a := newA. b := newB. ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:assigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:17:37 pm (UTC)'!
test090_WhenTheAssignmentBlockParameterNameExistsInTheExtractedTempVars_CreatesItWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'b := anAssignmentBlock := [ anAssignmentBlock value + b ].'.
	originalCode := 'm1 | anAssignmentBlock b | anAssignmentBlock := [ 0 ]. b := 1. ' , codeToExtract.
	newMethodCode := 'm2A: anAssignmentBlock m2B: b assigningWith: anAssignmentBlock1

	| newAnAssignmentBlock newB |
	newB := newAnAssignmentBlock := [ anAssignmentBlock value + b ].
	^ anAssignmentBlock1 value: newAnAssignmentBlock value: newB'.
	updatedCode := 'm1 | anAssignmentBlock b | anAssignmentBlock := [ 0 ]. b := 1. self m2A: anAssignmentBlock m2B: b assigningWith: [ :newAnAssignmentBlock :newB | anAssignmentBlock := newAnAssignmentBlock. b := newB. ].'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2A:m2B: arguments: #('anAssignmentBlock' 'b'))
		withReplacements: #()
		definesNewMethodNamed: #m2A:m2B:assigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:17:00 pm (UTC)'!
test128_CanExtractAnAssignmentWithAPartiallyCascadeCode

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := self value; value'.
	originalCode := 'm1 | a | ' , codeToExtract, '; value'.
	newMethodCode := 'm2

	| a |
	', codeToExtract, '.
	^ a'.
	updatedCode := 'm1 | a | a := self m2; value'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:16:40 pm (UTC)'!
test129_CanExtractMultipleAssignmentsButReturningOnlyOne

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'taxAmount := baseAmount * taxRate.
	finalAmount := baseAmount + taxAmount.'.
	originalCode := 'process: anInvoice
	
	| baseAmount taxRate taxAmount finalAmount |
	
	baseAmount := anInvoice amount.
	taxRate := 0.21.
	',
	codeToExtract
	,'
	^finalAmount'.
	newMethodCode := 'k1: baseAmount k2: taxRate

	| taxAmount finalAmount |
	', codeToExtract, '
	^ finalAmount'.
	updatedCode := 'process: anInvoice
	
	| baseAmount taxRate finalAmount |
	
	baseAmount := anInvoice amount.
	taxRate := 0.21.
	finalAmount := self k1: baseAmount k2: taxRate.
	^finalAmount'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #k1:k2: arguments: #('baseAmount' 'taxRate'))
		withReplacements: #()
		definesNewMethodNamed: #k1:k2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:15:36 pm (UTC)'!
test134_WhenTwoAlmostIdenticalMethodsExist_ExtractingOneWithReturn_ReplacingOneWithout_BothGetReplacedWithAssignmentBlock

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 1. b := 2.'.
	mACode := 'mWithReturn | a b | ' , codeToExtract , ' ^ a + b' .
	mBCode := 'mWithoutReturn | a b | ' , codeToExtract , ' self foo: a. self bar: b'.
	newMethodCode := 'n1AssigningWith: anAssignmentBlock

	| a b |
	a := 1. b := 2.
	^ anAssignmentBlock value: a value: b'.
	updatedCodeMA    := 'mWithReturn | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ]. ^ a + b'.
	
	updatedCodeMB := 'mWithoutReturn | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ]. self foo: a. self bar: b'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1AssigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:14:57 pm (UTC)'!
test135_WhenTwoAlmostIdenticalMethodsExist_ExtractingOneWithoutReturn_ReplacingOneWith_BothGetReplacedWithAssignmentBlock

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 1. b := 2.'.
	mACode := 'mWithReturn | a b | ' , codeToExtract , ' ^ a + b' .
	mBCode := 'mWithoutReturn | a b | ' , codeToExtract , ' self foo: a. self bar: b'.
	newMethodCode := 'n1AssigningWith: anAssignmentBlock

	| a b |
	a := 1. b := 2.
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mWithReturn | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ]. ^ a + b'.
	
	updatedCodeMB := 'mWithoutReturn | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ]. self foo: a. self bar: b'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1AssigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:10:52 pm (UTC)'!
test136_WhenTwoAlmostIdenticalMethodsExist_ExtractingOneWithReturn_ReplacingOneWithout_BothGetReplacedWithAssignmentBlock

	"The difference between this one and the 134 is that the method without returns does not have an usage 
	of its tempvars"

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 1. b := 2.'.
	mACode := 'mWithReturn | a b | ' , codeToExtract , ' ^ a + b' .
	mBCode := 'mWithoutReturn | a b | ' , codeToExtract.
	newMethodCode := 'n1AssigningWith: anAssignmentBlock

	| a b |
	a := 1. b := 2.
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mWithReturn | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ]. ^ a + b'.
	
	updatedCodeMB := 'mWithoutReturn self n1AssigningWith: [ :newA :newB | ].'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1AssigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:08:32 pm (UTC)'!
test137_WhenTwoAlmostIdenticalMethodsExist_ExtractingOneWithoutReturn_ReplacingOneWith_BothGetReplacedWithAssignmentBlock

	"The difference between this one and the 135 is that the method without returns does not have an usage 
	of its tempvars"

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 1. b := 2.'.
	mACode := 'mWithReturn | a b | ' , codeToExtract , ' ^ a + b' .
	mBCode := 'mWithoutReturn | a b | ' , codeToExtract .
	newMethodCode := 'n1AssigningWith: anAssignmentBlock

	| a b |
	a := 1. b := 2.
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mWithReturn | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ]. ^ a + b'.
	
	updatedCodeMB := 'mWithoutReturn self n1AssigningWith: [ :newA :newB | ].'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1AssigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:08:38 pm (UTC)'!
test138_WhenTwoAlmostIdenticalMethodsExist_ExtractingOneWithSingleReturn_ReplacingOneWithMultipleReturn_BothGetReplacedWithAssignmentBlock

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 1. b := 2.'.
	mACode := 'mWithSingleReturn | a b | ' , codeToExtract , ' ^ a' .
	mBCode := 'mWithMultipleReturn | a b | ' , codeToExtract , ' ^ a + b'.
	newMethodCode := 'n1AssigningWith: anAssignmentBlock

	| a b |
	a := 1. b := 2.
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mWithSingleReturn | a | self n1AssigningWith: [ :newA :newB | a := newA. ]. ^ a'.
	
	updatedCodeMB := 'mWithMultipleReturn | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ]. ^ a + b'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1AssigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:06:47 pm (UTC)'!
test139_WhenTwoAlmostIdenticalMethodsExist_ExtractingOneWithMultipleReturn_ReplacingOneWithSingleReturn_BothGetReplacedWithAssignmentBlock

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 1. b := 2.'.
	mACode := 'mWithSingleReturn | a b | ' , codeToExtract , ' ^ a' .
	mBCode := 'mWithMultipleReturn | a b | ' , codeToExtract , ' ^ a + b'.
	newMethodCode := 'n1AssigningWith: anAssignmentBlock

	| a b |
	a := 1. b := 2.
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mWithSingleReturn | a | self n1AssigningWith: [ :newA :newB | a := newA. ]. ^ a'.
	
	updatedCodeMB := 'mWithMultipleReturn | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ]. ^ a + b'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1AssigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:05:48 pm (UTC)'!
test140_WhenMultipleAlmostIdenticalMethodsExistWithMutlipleTemporaryVariables_ExtractingOne_AllGetReplacedWithAssignmentBlock

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode mCCode updatedCodeMC |
	
	codeToExtract := 'a := 1. b := 2.'.
	mACode := 'mWithSingleReturn | a b | ' , codeToExtract , ' ^ a' .
	mBCode := 'mWithMultipleReturn | a b | ' , codeToExtract , ' ^ a + b'.
	mCCode := 'mWithNeither | a b | ' , codeToExtract.
	newMethodCode := 'n1AssigningWith: anAssignmentBlock

	| a b |
	a := 1. b := 2.
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mWithSingleReturn | a | self n1AssigningWith: [ :newA :newB | a := newA. ]. ^ a'.
	
	updatedCodeMB := 'mWithMultipleReturn | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ]. ^ a + b'.
	
	updatedCodeMC := 'mWithNeither self n1AssigningWith: [ :newA :newB | ].'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode . mCCode }
		definesNewMethodNamed: #n1AssigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB . updatedCodeMC }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:02:52 pm (UTC)'!
test141_WhenTwoAlmostIdenticalMethodsExist_NoneUsesTemporariesAfterExtractedCode_NoneGetReplacedWithAssignmentBlock

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 1. b := 2.'.
	mACode := 'mA | a b | ' , codeToExtract.
	mBCode := 'mB | a b | ' , codeToExtract.
	newMethodCode := 'n1

	| a b |
	a := 1. b := 2'.
	updatedCodeMA    := 'mA self n1.'.
	updatedCodeMB := 'mB self n1.'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - temporary assignment' stamp: 'JEC 14/Apr/2026, 4:01:52 pm (UTC)'!
test142_WhenTwoAlmostIdenticalMethodsExist_TheyUsesDifferentTempVarsAfterExtractedCode_BothGetReplacedWithAssignmentBlock

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 1. b := 2.'.
	mACode := 'mA | a b | ' , codeToExtract, ' ^ a'.
	mBCode := 'mB | a b | ' , codeToExtract, ' ^ b'.
	newMethodCode := 'n1AssigningWith: anAssignmentBlock

	| a b |
	a := 1. b := 2.
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mA | a | self n1AssigningWith: [ :newA :newB | a := newA. ]. ^ a'.
	
	updatedCodeMB := 'mB | b | self n1AssigningWith: [ :newA :newB | b := newB. ]. ^ b'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1AssigningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 4:00:48 pm (UTC)'!
test091_CanExtractAReturn

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := '^ 3'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2

	' , codeToExtract .
	updatedCode := 'm1 ^ self m2'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 4:00:40 pm (UTC)'!
test092_CanExtractAPieceOfCodeWithAnUnnestedReturn

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'self value. ^ 3'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2

	' , codeToExtract .
	updatedCode := 'm1 ^ self m2'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 4:00:30 pm (UTC)'!
test093_CanExtractANestedReturn

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := '[ ^ 3 ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: 3 ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 4:00:25 pm (UTC)'!
test094_WhenThereAreNestedAndUnnestedReturnsInTheExtractedCode_IsUsingTheReturnBlock

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := '[ ^ 1 ]. ^ 2'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: 1 ]. ^ 2'.
	updatedCode := 'm1 ^ self m2ReturningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 4:00:17 pm (UTC)'!
test095_CanExtractTheSameReturnExpresionFromNestedAndUnnestedCode

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := '[ ^ 1 ]. ^ 1'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: 1 ]. ^ 1'.
	updatedCode := 'm1 ^ self m2ReturningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 4:00:11 pm (UTC)'!
test096_CanExtractReturnInsideFromABlock

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := '^ 1'.
	originalCode := 'm1 [ ' , codeToExtract , ' ]'.
	newMethodCode := 'm2

	^ 1'.
	updatedCode := 'm1 [ ^ self m2 ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 4:00:04 pm (UTC)'!
test097_CanExtractAPieceOfCodeWithAnUnnestedReturnInsideABlock

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := '[ ^ 1 ]. ^ 1'.
	originalCode := 'm1 [ ' , codeToExtract , ' ]'.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: 1 ]. ^ 1'.
	updatedCode := 'm1 [ ^ self m2ReturningWith: [ :res | ^ res ] ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:58:28 pm (UTC)'!
test098_CanExtractAReturnOfAKeywordMessageSend

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := '^ self add: 1 to: 2'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2

	^ self add: 1 to: 2'.
	updatedCode := 'm1 ^ self m2'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:58:18 pm (UTC)'!
test099_CanExtractAReturnOfAKeywordMessageSendInANestedBlock

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := '[ ^ self add: 1 to: 2 ]. ^ self add: 1 to: 2'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: (self add: 1 to: 2) ]. ^ self add: 1 to: 2'.
	updatedCode := 'm1 ^ self m2ReturningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:58:11 pm (UTC)'!
test100_CanExtractANestedReturnOfBlock

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := '[ ^ [ self add: 1 to: 2 ] ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: [ self add: 1 to: 2. ] ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:58:05 pm (UTC)'!
test101_CanExtractANestedReturnOfArray

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := '[ ^ { self add: 1 to: 2 } ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: { self add: 1 to: 2 } ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:57:41 pm (UTC)'!
test102_CanExtractAnUnnestedReturnWithArguments

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'self value: a. self foo: b. ^ a + b'.
	originalCode := 'm1 | a b | a := 1. b := 2. ' , codeToExtract.
	newMethodCode := 'm2A: a m2B: b

	self value: a. self foo: b. ^ a + b'.
	updatedCode := 'm1 | a b | a := 1. b := 2. ^ self m2A: a m2B: b'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2A:m2B: arguments: #('a' 'b'))
		withReplacements: #()
		definesNewMethodNamed: #m2A:m2B:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:57:31 pm (UTC)'!
test103_CanExtractANestedReturnWithArguments

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'self value: a. self foo: b. [ ^ a + b ]'.
	originalCode := 'm1 | a b | a := 1. b := 2. ' , codeToExtract.
	newMethodCode := 'm2A: a m2B: b returningWith: aReturningBlock

	self value: a. self foo: b. [ ^ aReturningBlock value: (a + b) ]'.
	updatedCode := 'm1 | a b | a := 1. b := 2. self m2A: a m2B: b returningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2A:m2B: arguments: #('a' 'b'))
		withReplacements: #()
		definesNewMethodNamed: #m2A:m2B:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:57:04 pm (UTC)'!
test104_CanExtractANestedReturnOfAVariable

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'self foo: a. [ ^ a ]'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a returningWith: aReturningBlock

	self foo: a. [ ^ aReturningBlock value: a ]'.
	updatedCode := 'm1 | a | a := 1. self m2: a returningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:56:40 pm (UTC)'!
test105_WhenTheNewParameterExistsAsAnInstVar_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |

	classToRefactor addInstVarName: 'aReturningBlock'.
	codeToExtract := '[ ^ 2 ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock1

	[ ^ aReturningBlock1 value: 2 ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:56:13 pm (UTC)'!
test106_WhenTheNextNewParameterExistsAsAnInstVar_CreatesANewOneWithTheNextNumber

	| codeToExtract newMethodCode originalCode updatedCode |

	classToRefactor addInstVarName: 'aReturningBlock'.
	classToRefactor addInstVarName: 'aReturningBlock1'.
	codeToExtract := '[ ^ 2 ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock2

	[ ^ aReturningBlock2 value: 2 ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:55:56 pm (UTC)'!
test107_WhenTheNewInstVarInReturningBlockExistsAsInstVarInHierarchy_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |

	self addInsVarAndSubclassify: 'aReturningBlock'.
	codeToExtract := '[ ^ 2 ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock1

	[ ^ aReturningBlock1 value: 2 ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:55:48 pm (UTC)'!
test108_WhenTheNewTempVarInReturningBlockExistsInMethod_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := '[ ^ 2 ]'.
	originalCode := 'm1 | res | ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: 2 ]'.
	updatedCode := 'm1 | res | self m2ReturningWith: [ :res1 | ^ res1 ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:55:40 pm (UTC)'!
test109_WhenTheNewTempVarInReturningBlockExistsInMethod_AndAlsoTheNextOne_CreatesANewOneWithTheFollowingNumber

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := '[ ^ 2 ]'.
	originalCode := 'm1 | res res1 | ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: 2 ]'.
	updatedCode := 'm1 | res res1 | self m2ReturningWith: [ :res2 | ^ res2 ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:55:31 pm (UTC)'!
test110_WhenTheNewInstVarInReturningBlockExistsAsInstVar_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |

	classToRefactor addInstVarName: 'res'.
	codeToExtract := '[ ^ 2 ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: 2 ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res1 | ^ res1 ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:55:13 pm (UTC)'!
test111_WhenTheNewTempVarInReturningBlockExistsAsInstVarInHierarchy_CreatesANewOneWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |

	self addInsVarAndSubclassify: 'res'.
	codeToExtract := '[ ^ 2 ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ ^ aReturningBlock value: 2 ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res1 | ^ res1 ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:55:03 pm (UTC)'!
test112_WhenTheNewMethodNameWithReturningBlockExists_CreatesItWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |

	classToRefactor compile: 'm2ReturningWith: arg'.
	codeToExtract := '[ ^ 2 ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith1: aReturningBlock

	[ ^ aReturningBlock value: 2 ]'.
	updatedCode := 'm1 self m2ReturningWith1: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith1:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:54:56 pm (UTC)'!
test113_WhenTheNewMethodNameWithReturningBlockExists_AndAlsoTheNextOne_CreatesANewOneWithTheFollowingNumber

	| codeToExtract newMethodCode originalCode updatedCode |

	classToRefactor compile: 'm2ReturningWith: arg'.
	classToRefactor compile: 'm2ReturningWith1: arg'.
	codeToExtract := '[ ^ 2 ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith2: aReturningBlock

	[ ^ aReturningBlock value: 2 ]'.
	updatedCode := 'm1 self m2ReturningWith2: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith2:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:54:47 pm (UTC)'!
test114_WhenTheNewMethodNameWithReturningBlockExistsInHierarchy_CreatesItWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |

	self compileAndSubclassify: 'm2ReturningWith: arg'. 
	codeToExtract := '[ ^ 2 ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith1: aReturningBlock

	[ ^ aReturningBlock value: 2 ]'.
	updatedCode := 'm1 self m2ReturningWith1: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith1:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:54:16 pm (UTC)'!
test115_WhenTheNewMethodNameWithParametersAndReturningBlockExists_CreatesItWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |

	classToRefactor compile: 'm2A: a m2B: b returningWith: arg'.
	codeToExtract := 'self foo: a. self bar: b. [ ^ a ]'.
	originalCode := 'm1 | a b | a := 1. b := 2. ' , codeToExtract.
	newMethodCode := 'm2A: a m2B: b returningWith1: aReturningBlock

	self foo: a. self bar: b. [ ^ aReturningBlock value: a ]'.
	updatedCode := 'm1 | a b | a := 1. b := 2. self m2A: a m2B: b returningWith1: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2A:m2B: arguments: #('a' 'b'))
		withReplacements: #()
		definesNewMethodNamed: #m2A:m2B:returningWith1:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:53:41 pm (UTC)'!
test116_WhenTheReturningBlockParameterNameExistsInTheExtractedTempVars_CreatesItWithANumber

	| codeToExtract newMethodCode originalCode updatedCode |

	codeToExtract := 'self foo: aReturningBlock. [ ^ a ]'.
	originalCode := 'm1 | aReturningBlock | aReturningBlock := []. ' , codeToExtract.
	newMethodCode := 'm2: aReturningBlock returningWith: aReturningBlock1

	self foo: aReturningBlock. [ ^ aReturningBlock1 value: a ]'.
	updatedCode := 'm1 | aReturningBlock | aReturningBlock := []. self m2: aReturningBlock returningWith: [ :res | ^ res ]'.
	
	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('aReturningBlock'))
		withReplacements: #()
		definesNewMethodNamed: #m2:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:52:43 pm (UTC)'!
test117_CanExtractAnAssignmentAndANestedReturn

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3. ^ 1'.
	originalCode := 'm1 | a |' , codeToExtract.
	newMethodCode := 'm2

	| a |
	' , codeToExtract .
	updatedCode := 'm1 ^ self m2'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:52:31 pm (UTC)'!
test118_CanExtractAReturningAssignmentToATempVar

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := '^ a := 3'.
	originalCode := 'm1 | a |' , codeToExtract.
	newMethodCode := 'm2

	| a |
	' , codeToExtract .
	updatedCode := 'm1 ^ self m2'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:52:23 pm (UTC)'!
test119_WhenExtractingMultipleTempVarsAssignmentsAndAnUnnestedReturn_DoNotAddAssignmentBlock

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3. b := 1. ^ a + b'.
	originalCode := 'm1 | a b |' , codeToExtract.
	newMethodCode := 'm2

	| a b |
	' , codeToExtract .
	updatedCode := 'm1 ^ self m2'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:51:58 pm (UTC)'!
test120_WhenExtractingMultipleTempVarsUsedOutsideExtractedCodeAndAnUnnestedReturn_WillUseAssignmentBlockAndReturningBlock

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3. b := 1. [ ^ a + b ].'.
	originalCode := 'm1 | a b | ' , codeToExtract, ' self foo: a. self bar: b'.
	newMethodCode := 'm2AssigningWith: anAssignmentBlock returningWith: aReturningBlock

	| a b |
	a := 3. b := 1. [ ^ aReturningBlock value: (a + b) ].
	^ anAssignmentBlock value: a value: b' .
	updatedCode := 'm1 | a b | self m2AssigningWith: [ :newA :newB | a := newA. b := newB. ] returningWith: [ :res | ^ res ]. self foo: a. self bar: b'.
	
	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2AssigningWith:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:51:33 pm (UTC)'!
test121_CanExtractAnAssignmentAndAnUnnestedReturn

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3. [ ^ a ]'.
	originalCode := 'm1 | a |' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	| a |
	a := 3. [ ^ aReturningBlock value: a ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:50:52 pm (UTC)'!
test122_WhenExtractingMultipleVariablesUsedAsParametersAndANestedReturn_DoNotUseAssignmentNorReturningBlocks

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := a + b + 1. b := a + 1. self foo: b. self bar: a. ^ a + b'.
	originalCode := 'm1 | a b | a := 1. b := 2. ' , codeToExtract.
	newMethodCode := 'm2A: a m2B: b

	| newA newB |
	newA := a + b + 1. newB := newA + 1. self foo: newB. self bar: newA. ^ newA + newB'.
	updatedCode := 'm1 | a b | a := 1. b := 2. ^ self m2A: a m2B: b'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2A:m2B: arguments: #('a' 'b'))
		withReplacements: #()
		definesNewMethodNamed: #m2A:m2B:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:48:43 pm (UTC)'!
test123_CanReturnANestedRenamedTempVar

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := a + 1. [ ^ a ]'.
	originalCode := 'm1 | a | a := 1. ' , codeToExtract.
	newMethodCode := 'm2: a returningWith: aReturningBlock

	| newA |
	newA := a + 1. [ ^ aReturningBlock value: newA ].
	^ newA'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a returningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }
	! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:47:53 pm (UTC)'!
test124_CanReturnMultipleNestedRenamedTempVars

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := a + b + 1. b := a + 1. self foo: b. self bar: a. [ ^ a + b ]'.
	originalCode := 'm1 | a b | a := 1. b := 2. ' , codeToExtract.
	newMethodCode := 'm2A: a m2B: b assigningWith: anAssignmentBlock returningWith: aReturningBlock

	| newA newB |
	newA := a + b + 1. newB := newA + 1. self foo: newB. self bar: newA. [ ^ aReturningBlock value: (newA + newB) ].
	^ anAssignmentBlock value: newA value: newB'.
	updatedCode := 'm1 | a b | a := 1. b := 2. self m2A: a m2B: b assigningWith: [ :newA :newB | a := newA. b := newB. ] returningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2A:m2B: arguments: #('a' 'b'))
		withReplacements: #()
		definesNewMethodNamed: #m2A:m2B:assigningWith:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:47:19 pm (UTC)'!
test125_WhenExtractingAnUnnestedReturningTempVariableCode_DoesNotAssignItInTheExtractedMethod

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := a + b + 1. [ ^ a + b + 1 ]. ^ b'.
	originalCode := 'm1 | a b | a := 1. b := 2. ' , codeToExtract.
	newMethodCode := 'm2A: a m2B: b returningWith: aReturningBlock

	| newA |
	newA := a + b + 1. [ ^ aReturningBlock value: (newA + b + 1) ]. ^ b'.
	updatedCode := 'm1 | a b | a := 1. b := 2. ^ self m2A: a m2B: b returningWith: [ :res | ^ res ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2A:m2B: arguments: #('a' 'b'))
		withReplacements: #()
		definesNewMethodNamed: #m2A:m2B:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:46:38 pm (UTC)'!
test126_CanExtractBlocksWithTempVarsDefinedAndReturnedInThem

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := '[ :res | ^ res ]'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	[ :res | ^ aReturningBlock value: res ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res1 | ^ res1 ]'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:46:30 pm (UTC)'!
test127_CanExtractADefinitionAssignationAndReturningOfATempVar

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := '| a | a := 1. ^ a'.
	originalCode := 'm1 ' , codeToExtract.
	newMethodCode := 'm2

	', codeToExtract.
	updatedCode := 'm1 ^ self m2'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:46:19 pm (UTC)'!
test143_CanExtractMultipleAssignmentWithNestedReturnOfASingleVariable_AndThatReturningVarIsUsedAfterExtractedCode

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3. b := 1. [ ^ a ].'.
	originalCode := 'm1 | a b | ', codeToExtract ,' self foo: a'.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	| a b |
	a := 3. b := 1. [ ^ aReturningBlock value: a ].
	^ a'.
	updatedCode := 'm1 | a | a := self m2ReturningWith: [ :res | ^ res ]. self foo: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:46:07 pm (UTC)'!
test144_CanExtractMultipleAssignmentWithNestedReturnOfMultipleVariables

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := 3. b := 1. [ ^ a + b ].'.
	originalCode := 'm1 | a b | ', codeToExtract.
	newMethodCode := 'm2ReturningWith: aReturningBlock

	| a b |
	a := 3. b := 1. [ ^ aReturningBlock value: (a + b) ]'.
	updatedCode := 'm1 self m2ReturningWith: [ :res | ^ res ].'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2)
		withReplacements: #()
		definesNewMethodNamed: #m2ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:45:18 pm (UTC)'!
test145_CanExtractMultipleAssignmentWithNestedReturnOfMultipleVariables_TheReturnedVarIsUsedAfterExtractedCode

	| codeToExtract newMethodCode originalCode updatedCode |
	
	codeToExtract := 'a := a + 2. b := 3. [ ^ a ].'.
	originalCode := 'm1 | a b | a := 1. ', codeToExtract , ' self foo: a'.
	newMethodCode := 'm2: a returningWith: aReturningBlock

	| newA b |
	newA := a + 2. b := 3. [ ^ aReturningBlock value: newA ].
	^ newA'.
	updatedCode := 'm1 | a | a := 1. a := self m2: a returningWith: [ :res | ^ res ]. self foo: a'.

	self
		assertExtract: codeToExtract 
		from: originalCode
		to: (Message selector: #m2: arguments: #('a'))
		withReplacements: #()
		definesNewMethodNamed: #m2:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCode }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:44:09 pm (UTC)'!
test146_WhenTwoAlmostIdenticalMethodsExist_CanExtractMultipleAssignmentsWithNestedReturnWithoutUsingThemAfterExtractedCode

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 3. b := 1. [ ^ a + b ].'.
	mACode := 'mA | a b | ' , codeToExtract.
	mBCode := 'mB | a b | ' , codeToExtract.
	newMethodCode := 'n1ReturningWith: aReturningBlock

	| a b |
	a := 3. b := 1. [ ^ aReturningBlock value: (a + b) ]'.
	updatedCodeMA    := 'mA self n1ReturningWith: [ :res | ^ res ].'.
	updatedCodeMB := 'mB self n1ReturningWith: [ :res | ^ res ].'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1ReturningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:43:27 pm (UTC)'!
test147_WhenTwoAlmostIdenticalMethodsExist_CanExtractMultipleAssignmentsWithNestedReturnWithUsingThemAfterExtractedCode

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 3. b := 1. [ ^ a + b ].'.
	mACode := 'mA | a b | ' , codeToExtract , ' self foo: a. self bar: b'.
	mBCode := 'mB | a b | ' , codeToExtract , ' self baz: a andAlso: b'.
	newMethodCode := 'n1AssigningWith: anAssignmentBlock returningWith: aReturningBlock

	| a b |
	a := 3. b := 1. [ ^ aReturningBlock value: (a + b) ].
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mA | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ] returningWith: [ :res | ^ res ]. self foo: a. self bar: b'.
	
	updatedCodeMB := 'mB | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ] returningWith: [ :res | ^ res ]. self baz: a andAlso: b'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1AssigningWith:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:42:43 pm (UTC)'!
test148_WhenTwoAlmostIdenticalMethodsExist_CanExtractMultipleAssignmentsWithNestedReturnWithOneMethodUsingTheTempVarsAndTheOtherNot

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 3. b := 1. [ ^ a + b ].'.
	mACode := 'mA | a b | ' , codeToExtract , ' self foo: a. self bar: b'.
	mBCode := 'mB | a b | ' , codeToExtract.
	newMethodCode := 'n1AssigningWith: anAssignmentBlock returningWith: aReturningBlock

	| a b |
	a := 3. b := 1. [ ^ aReturningBlock value: (a + b) ].
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mA | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ] returningWith: [ :res | ^ res ]. self foo: a. self bar: b'.
	
	updatedCodeMB := 'mB self n1AssigningWith: [ :newA :newB | ] returningWith: [ :res | ^ res ].'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1AssigningWith:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:42:17 pm (UTC)'!
test149_WhenTwoAlmostIdenticalMethodsExist_CanExtractMultipleAssignmentsWithNestedReturnWithOneMethodUsingTheTempVarsAndTheOtherJustOne

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode |
	
	codeToExtract := 'a := 3. b := 1. [ ^ a + b ].'.
	mACode := 'mA | a b | ' , codeToExtract , ' self foo: a. self bar: b'.
	mBCode := 'mB | a b | ' , codeToExtract , ' self foo: a'.
	newMethodCode := 'n1AssigningWith: anAssignmentBlock returningWith: aReturningBlock

	| a b |
	a := 3. b := 1. [ ^ aReturningBlock value: (a + b) ].
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mA | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ] returningWith: [ :res | ^ res ]. self foo: a. self bar: b'.
	
	updatedCodeMB := 'mB | a | self n1AssigningWith: [ :newA :newB | a := newA. ] returningWith: [ :res | ^ res ]. self foo: a'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode }
		definesNewMethodNamed: #n1AssigningWith:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB }! !

!ExtractMethodTest methodsFor: 'tests - return' stamp: 'JEC 14/Apr/2026, 3:16:20 pm (UTC)'!
test150_WhenMultipleAlmostIdenticalMethodsExistWithMutlipleTemporaryVariablesAndNestedReturn_ExtractingOne_AllGetReplacedWithAssignmentBlock

	| codeToExtract newMethodCode updatedCodeMA updatedCodeMB mACode mBCode mCCode updatedCodeMC |
	
	codeToExtract := 'a := 3. b := 1. [ ^ a + b ].'.
	mACode := 'mA | a b | ' , codeToExtract , ' self foo: a. self bar: b'.
	mBCode := 'mB | a b | ' , codeToExtract , ' self foo: a'.
	mCCode := 'mC | a b | ' , codeToExtract.
	
	newMethodCode := 'n1AssigningWith: anAssignmentBlock returningWith: aReturningBlock

	| a b |
	a := 3. b := 1. [ ^ aReturningBlock value: (a + b) ].
	^ anAssignmentBlock value: a value: b'.
	
	updatedCodeMA    := 'mA | a b | self n1AssigningWith: [ :newA :newB | a := newA. b := newB. ] returningWith: [ :res | ^ res ]. self foo: a. self bar: b'.
	
	updatedCodeMB := 'mB | a | self n1AssigningWith: [ :newA :newB | a := newA. ] returningWith: [ :res | ^ res ]. self foo: a'.
	
	updatedCodeMC := 'mC self n1AssigningWith: [ :newA :newB | ] returningWith: [ :res | ^ res ].'.
	
	self
		assertExtract: codeToExtract 
		from: mACode
		to: (Message selector: #n1)
		withReplacements: { mBCode . mCCode }
		definesNewMethodNamed: #n1AssigningWith:returningWith:
		withBody: newMethodCode
		andUpdateMethodsTo: { updatedCodeMA . updatedCodeMB . updatedCodeMC }! !

!ExtractToTemporaryTest methodsFor: 'assertions' stamp: 'jmv 26/Apr/2023, 7:10:28 pm (UTC)'!
assertExtracting: codeToExtract from: sourceCodeOfMethodToRefactor toVariableNamed: newVariable updatesTo: sourceCodeAfterRefactoring

	| intervalToExtract methodToRefactor actualSourceCodeAfterRefactoring |
	classToRefactor compile: sourceCodeOfMethodToRefactor.
	intervalToExtract := self intervalOf: codeToExtract locatedIn: sourceCodeOfMethodToRefactor.
	methodToRefactor := classToRefactor >> #m1.

	actualSourceCodeAfterRefactoring := (ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor) apply.
	
	self assert: sourceCodeAfterRefactoring equals: actualSourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'assertions' stamp: 'JEC 8/Oct/2025, 10:55:00 pm (UTC)'!
assertExtractingAll: codeToExtract from: sourceCodeOfMethodToRefactor toVariableNamed: newVariable updatesTo: sourceCodeAfterRefactoring

	| intervalsToExtract |

	intervalsToExtract := self allIntervalsOf: codeToExtract locatedIn: sourceCodeOfMethodToRefactor.
	
	self 
		assertExtractingIntervals: intervalsToExtract 
		from: sourceCodeOfMethodToRefactor 
		toVariableNamed: newVariable 
		updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'assertions' stamp: 'JEC 8/Oct/2025, 10:59:00 pm (UTC)'!
assertExtractingIntervals: intervalsToExtract 
	from: sourceCodeOfMethodToRefactor 
	toVariableNamed: newVariable 
	updatesTo: sourceCodeAfterRefactoring

	| methodToRefactor |

	classToRefactor compile: sourceCodeOfMethodToRefactor.
	methodToRefactor := classToRefactor >> #m1.

	(ExtractToTemporary named: newVariable intervals: intervalsToExtract from: methodToRefactor) apply.

	self assert: sourceCodeAfterRefactoring equals: (classToRefactor >> #m1) sourceCode! !

!ExtractToTemporaryTest methodsFor: 'class factory' stamp: 'RNG 23/Feb/2020, 1:20:46 am (UTC)'!
classToRefactorName

	^ #ClassToDoExtractTemporary! !

!ExtractToTemporaryTest methodsFor: 'set up' stamp: 'RNG 25/Mar/2020, 2:30:57 am (UTC)'!
setUp

	super setUp.
	classToRefactor _ self createClassNamed: self classToRefactorName! !

!ExtractToTemporaryTest methodsFor: 'test helpers' stamp: 'JEC 12/Oct/2025, 3:14:00 am (UTC)'!
allIntervalsOf: aPieceOfSourceCode locatedIn: sourceCode

	| intervals subsequenceSize |
	
	intervals := OrderedCollection new.
	subsequenceSize := aPieceOfSourceCode size.

	1 to: (sourceCode size - subsequenceSize + 1) do: [ :index |
		(sourceCode findString: aPieceOfSourceCode startingAt: index) = index
			ifTrue: [ intervals add: (index toSelfPlus: subsequenceSize - 1) asSourceCodeInterval ] ].
	
	^ intervals! !

!ExtractToTemporaryTest methodsFor: 'test helpers' stamp: 'RNG 9/May/2020, 1:06:24 am (UTC)'!
intervalOf: aPieceOfSourceCode locatedIn: sourceCode

	| interval |
	interval _ sourceCode intervalOfSubCollection: aPieceOfSourceCode.
	^ (interval first to: interval last - 1) asSourceCodeInterval! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'jmv 26/Apr/2023, 7:11:11 pm (UTC)'!
test12ItIsPossibleToExtractASingleLiteralFromAMethodWithoutAnyTempsOrArguments

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ 42'.
	sourceCodeAfterRefactoring := 'm1

	| new |
	new := 42.
	^ new'.

	self assertExtracting: '42' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'jmv 26/Apr/2023, 7:11:38 pm (UTC)'!
test13ItIsPossibleToExtractASingleExpressionFromAMethodThatHasAlreadyOtherTemporary

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	| four |
	four := 4.
	^ four + 2'.
	sourceCodeAfterRefactoring := 'm1

	| four two |
	four := 4.
	two := 2.
	^ four + two'.

	self assertExtracting: '2' from: sourceCode toVariableNamed: 'two' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'jmv 26/Apr/2023, 7:11:50 pm (UTC)'!
test14ItIsPossibleToExtractASingleExpressionFromAMethodThatHasAnEmptyTemporariesDeclaration

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	| |
	^ 3 factorial'.
	sourceCodeAfterRefactoring := 'm1

	| f |
	f := 3 factorial.
	^ f'.

	self assertExtracting: '3 factorial' from: sourceCode toVariableNamed: 'f' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'jmv 26/Apr/2023, 7:10:45 pm (UTC)'!
test15ItIsPossibleToExtractCodeAndGenerateAssignmentUsingAnsiAssignmentStyle

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ 42'.
	sourceCodeAfterRefactoring := 'm1

	| new |
	new := 42.
	^ new'.

	self assertExtracting: '42' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'jmv 26/Apr/2023, 7:11:59 pm (UTC)'!
test16ItIsPossibleToExtractASingleLiteralFromABlockWithoutAnyTempsOrArguments

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ [ self m2 ]'.
	sourceCodeAfterRefactoring := 'm1

	^ [ | new |
	new := self m2 ]'.

	self assertExtracting: 'self m2' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'jmv 26/Apr/2023, 7:12:04 pm (UTC)'!
test19ItIsPossibleToExtractAnIntervalWithSomeExtraSpaces

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ 42'.
	sourceCodeAfterRefactoring := 'm1

	| new |
	new := 42.
	^ new'.

	self assertExtracting: ' 42' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'jmv 26/Apr/2023, 7:12:09 pm (UTC)'!
test20ItIsPossibleToExtractAnEmptyBlock

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ []'.
	sourceCodeAfterRefactoring := 'm1

	| new |
	new := [].
	^ new'.

	self assertExtracting: '[]' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'jmv 26/Apr/2023, 7:12:14 pm (UTC)'!
test22ItIsPossibleToExtractACascadeExpression

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	^ 3 factorial; yourself'.
	sourceCodeAfterRefactoring := 'm1

	| new |
	new := 3 factorial; yourself.
	^ new'.

	self assertExtracting: '3 factorial; yourself' from: sourceCode toVariableNamed: 'new' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'jmv 26/Apr/2023, 7:12:18 pm (UTC)'!
test24WhenExtractingAnEntireStatementItOnlyGeneratesTheVariableAssignment

	| sourceCode sourceCodeAfterRefactoring |
	sourceCode := 'm1

	self run'.
	sourceCodeAfterRefactoring := 'm1

	| action |
	action := self run'.

	self assertExtracting: 'self run' from: sourceCode toVariableNamed: 'action' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'HAW 21/Jul/2023, 8:56:47 pm (UTC)'!
test26ExtractsCorrectlyWithParentesisInsideParentesis

	| sourceCode sourceCodeAfterRefactoring |
	
	sourceCode := 'm1 ^((self m2) + (self m3)) m1.'.
	sourceCodeAfterRefactoring := 'm1 | action |
	action := (self m2) + (self m3).
	^(action) m1.'.

	self assertExtracting: '((self m2) + (self m3))' from: sourceCode toVariableNamed: 'action' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'HAW 24/Jul/2023, 7:51:16 pm (UTC)'!
test27ExtractRightNodeWhenDuplicates

	| sourceCode sourceCodeAfterRefactoring |
	
	sourceCode := 'm1
self m2.
^self m2.'.
	sourceCodeAfterRefactoring := 'm1
| a |
	a := self m2.
^self m2.'.

	self assertExtracting: 'self m2' from: sourceCode toVariableNamed: 'a' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'HAW 24/Jul/2023, 7:54:10 pm (UTC)'!
test28ExtractRightNodeWhenDuplicatesAndSelectedLastOne

	| sourceCode sourceCodeAfterRefactoring intervalToExtract methodToRefactor actualSourceCodeAfterRefactoring |
	
	sourceCode := 'm1
self m2.
^self m2.'.
	sourceCodeAfterRefactoring := 'm1
| a |
	self m2.
a := self m2.
	^a.'.

	classToRefactor compile: sourceCode.
	intervalToExtract := (14 to: 20) asSourceCodeInterval.
	methodToRefactor := classToRefactor >> #m1.
	actualSourceCodeAfterRefactoring := (ExtractToTemporary named: 'a' at: intervalToExtract from: methodToRefactor) apply.
	self assert: sourceCodeAfterRefactoring equals: actualSourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 16/Mar/2025, 1:05:20 am (UTC)'!
test29ExctractsCorrectlyWhenTempVariablesDeclarationDoesNotHaveSpaces

	| sourceCode sourceCodeAfterRefactoring |
	
	sourceCode := 'm1
|var|
var := 10.
^var + 50.'.
	sourceCodeAfterRefactoring := 'm1
|var anotherVar|
var := 10.
anotherVar := var + 50.
	^anotherVar.'.

	self assertExtracting: 'var + 50' from: sourceCode toVariableNamed: 'anotherVar' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 14/Oct/2025, 8:48:00 pm (UTC)'!
test30ItsPossibleToExtractMoreThanOneIntervalOfSameCode

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	10. 
	10.'.
	sourceCodeAfterRefactoring := 'm1 
	| var |
	var := 10.
	var. 
	var.'.
	
	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 20/Oct/2025, 10:14:00 pm (UTC)'!
test31ItsPossibleToExtractIntervalsThatItsScopesAreNested

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	[ 10. [ 10. ] ]'.
	sourceCodeAfterRefactoring := 'm1 
	[ | var |
	var := 10.
	var. [ var. ] ]'.
	
	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 14/Oct/2025, 8:48:00 pm (UTC)'!
test32ItsPossibleToExtractSameValuesFromDifferentsScopes

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	[ 10 ]. 
	[ 10 ]'.
	sourceCodeAfterRefactoring := 'm1 
	| var |
	var := 10.
	[ var ]. 
	[ var ]'.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 14/Oct/2025, 8:49:00 pm (UTC)'!
test33ItsPossibleToExtractMoreThanOneIntervalOfSameCode

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	[ 10 ]. 
	[ 10 ]'.
	sourceCodeAfterRefactoring := 'm1 
	| var |
	var := [ 10 ].
	var. 
	var'.
	
	self assertExtractingAll: '[ 10 ]' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 17/Oct/2025, 7:13:00 pm (UTC)'!
test35ExtractAllSemanticallyEquivalentCodePieces

	| sourceCode sourceCodeAfterRefactoring methodToRefactor intervalsToExtract |

	sourceCode := 'm1 
	[10] . 
	[   10 ] . 
	[   10   ]'.
	sourceCodeAfterRefactoring := 'm1 
	| var |
	var := [10].
	var . 
	var . 
	var'.
	intervalsToExtract := { 
		(6 to: 9) asSourceCodeInterval . 
		(15 to: 22) asSourceCodeInterval .
		(28 to: 37) asSourceCodeInterval }.

	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	(ExtractToTemporary named: 'var' intervals: intervalsToExtract from: methodToRefactor) apply.
	
	self assert: sourceCodeAfterRefactoring equals: (classToRefactor >> #m1) sourceCode! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 14/Oct/2025, 8:57:00 pm (UTC)'!
test36CanExtractMultiplePiecesOfCodeHavingTemporaryVariables

	| sourceCode sourceCodeAfterRefactoring methodToRefactor |

	sourceCode := 'm1 
	| foo | 
	foo := 50 . 
	foo . 
	10. 
	10. 
	10'.
	sourceCodeAfterRefactoring := 'm1 
	| foo var | 
	foo := 50 . 
	foo . 
	var := 10.
	var. 
	var. 
	var'.

	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 20/Oct/2025, 10:14:00 pm (UTC)'!
test37CanExtractMultiplePiecesOfCodeHavingTemporaryVariablesWithDeclarationWithoutSpaces

	| sourceCode sourceCodeAfterRefactoring methodToRefactor |

	sourceCode := 'm1 
	|foo| 
	foo := 50 . 
	foo . 
	10. 
	10. 
	10'.
	sourceCodeAfterRefactoring := 'm1 
	|foo var| 
	foo := 50 . 
	foo . 
	var := 10.
	var. 
	var. 
	var'.

	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 14/Oct/2025, 9:00:00 pm (UTC)'!
test38CanExtractMultiplePiecesOfCodeFromABlockHavingTemporaryVariables

	| sourceCode sourceCodeAfterRefactoring methodToRefactor |

	sourceCode := 'm1 [ | foo | foo := 50 . foo . 10. 10. 10 ]'.
	sourceCodeAfterRefactoring := 'm1 [ | foo var | foo := 50 . foo . var := 10.
	var. var. var ]'.

	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 14/Oct/2025, 9:01:00 pm (UTC)'!
test39CanExtractMultiplePiecesOfCodeHavingTemporaryVariablesAndALotOfSpaceBeforeFirstSentence

	| sourceCode sourceCodeAfterRefactoring methodToRefactor |

	sourceCode := 'm1 | foo |         foo := 50 . foo . 10. 10. 10'.
	sourceCodeAfterRefactoring := 'm1 | foo var |         foo := 50 . foo . var := 10.
	var. var. var'.

	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 12/Oct/2025, 3:12:00 am (UTC)'!
test41WhenThereIsOnlyOneIntervalUsesTheSingleExtractToTemporaryClass

	| methodToRefactor refactor |

	classToRefactor compile: 'm1 10. 10.'.
	methodToRefactor := classToRefactor >> #m1.

	refactor := ExtractToTemporary 
		named: 'var' 
		intervals: { (4 to: 6) asSourceCodeInterval } 
		from: methodToRefactor.
	
	self assert: refactor class equals: ExtractToTemporary! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 12/Oct/2025, 3:13:00 am (UTC)'!
test43ExctractsOnlyOneIntervalOfRepeatedIntervals

	| sourceCode expectedSourceCodeAfterRefactoring actualCodeAfterRefactoring |

	sourceCode := 'm1 10. 10.'.
	expectedSourceCodeAfterRefactoring := 'm1 | var |
	var := 10. 10.'.
	classToRefactor compile: sourceCode.

	actualCodeAfterRefactoring := (
		ExtractToTemporary 
			named: 'var' 
			intervals: { (4 to: 5) asSourceCodeInterval . (4 to: 5) asSourceCodeInterval } 
			from: classToRefactor >> #m1) apply.

	self assert: expectedSourceCodeAfterRefactoring equals: actualCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 17/Oct/2025, 7:13:00 pm (UTC)'!
test44CanExtractMultipleBlocksOfCodeInsideAKeywordCollaboration

	| sourceCode sourceCodeAfterRefactoring actualSourceCodeAfterRefactoring intervalsToExtract methodToRefactor |

	sourceCode := 'm1 
	
	1 isNumber
		ifTrue: [ |a| a := 10. a+1 ]
		ifFalse: [    |a|    a := 10    . a+1     ]'.
	sourceCodeAfterRefactoring := 'm1 
	
	| var |
	var := [ |a| a := 10. a+1 ].
	1 isNumber
		ifTrue: var
		ifFalse: var'.

	classToRefactor compile: sourceCode.
	intervalsToExtract := { 
		self intervalOf: '[ |a| a := 10. a+1 ]' locatedIn: sourceCode .
		self intervalOf: '[    |a|    a := 10    . a+1     ]' locatedIn: sourceCode }.
	methodToRefactor := classToRefactor >> #m1.
	
	actualSourceCodeAfterRefactoring := 
		(ExtractToTemporary named: 'var' intervals: intervalsToExtract from: methodToRefactor) apply.
	
	self assert: sourceCodeAfterRefactoring equals: actualSourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 4/Nov/2025, 12:10:00 am (UTC)'!
test45CanExtractMultipleWhenThereAreMoreThanOneCommonScope

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	[ 
		[ 
			[ [ [ 10 ] ] ]. 
			[ [ [ 10 ] ] ] 
		] 
	]'.
	sourceCodeAfterRefactoring := 'm1 
	[ 
		[ 
			| var |
	var := 10.
	[ [ [ var ] ] ]. 
			[ [ [ var ] ] ] 
		] 
	]'.

	self assertExtractingAll: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - successful' stamp: 'JEC 11/Nov/2025, 12:42:00 am (UTC)'!
test46ChoosesClosestNodeWhenExtractingSingleCodePieceWithMultiplePossibleScopes

	| sourceCode sourceCodeAfterRefactoring |

	sourceCode := 'm1 
	[ 
		[ 
			[ [ [ 10 ] ] ].
		] 
	]'.
	sourceCodeAfterRefactoring := 'm1 
	[ 
		[ 
			[ [ [ | var |
	var := 10 ] ] ].
		] 
	]'.

	self assertExtracting: '10' from: sourceCode toVariableNamed: 'var' updatesTo: sourceCodeAfterRefactoring! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 25/Feb/2020, 7:13:05 pm (UTC)'!
test01NewVariableCanNotBeEmpty

	| newVariable sourceCodeToExtract sourceCode intervalToExtract |
	newVariable _ ''.
	sourceCodeToExtract _ '2'.
	sourceCode _ 'm1 ^ 2'.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: (classToRefactor >> #m1) ]
		failsWith: [ NewTemporaryPrecondition errorMessageForEmptyTemporaryVariable ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 25/Feb/2020, 7:13:01 pm (UTC)'!
test02NewVariableHasToBeValid

	| newVariable sourceCodeToExtract sourceCode intervalToExtract |
	newVariable _ 'a b'.
	sourceCodeToExtract _ '2'.
	sourceCode _ 'm1 ^ ', sourceCodeToExtract.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: (classToRefactor >> #m1) ]
		failsWith: [ NewTemporaryPrecondition errorMessageForInvalidTemporaryVariable: newVariable ]
	! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 25/Feb/2020, 7:12:58 pm (UTC)'!
test03NewVariableNameCanNotBeDefinedInMethod

	| newVariable sourceCodeToExtract sourceCode intervalToExtract methodToRefactor |
	newVariable _ 'new'.
	sourceCodeToExtract _ '2'.
	sourceCode _ 'm1 | ', newVariable, ' | ^ ', sourceCodeToExtract.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariable: newVariable isAlreadyDefinedIn: methodToRefactor methodNode ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 25/May/2020, 12:01:16 am (UTC)'!
test04FailsIfNewTemporaryIsEqualToInstanceVariableInClass

	| newVariable sourceCode sourceCodeToExtract methodToRefactor intervalToExtract |
	newVariable _ 'new'.
	classToRefactor addInstVarName: newVariable.
	sourceCodeToExtract _ '78'.
	sourceCode _ 'm1 ^ ', sourceCodeToExtract.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: classToRefactor ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'jmv 13/Jun/2024, 5:07:57 pm (UTC)'!
test05FailsIfNewTemporaryIsEqualToInstanceVariableInAnySuperclass

	| newVariable sourceCode sourceCodeToExtract methodToRefactor superclassToRefactor intervalToExtract |
	newVariable := 'new'.
	superclassToRefactor := self createClassNamed: #ClassToRefactorSuperclass instanceVariableNames: newVariable.

	"classToRefactor superclass: superclassToRefactor."
	classToRefactor instVarNamed: 'superclass' put: superclassToRefactor.
	Object flushCache.

	sourceCodeToExtract := '78'.
	sourceCode := 'm1 ^ ', sourceCodeToExtract.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: superclassToRefactor ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 12/Jul/2020, 9:10:14 pm (UTC)'!
test06SourceCodeToExtractCanNotIncludeReturn

	| newVariable sourceCode sourceCodeToExtract  intervalToExtract methodToRefactor |
	newVariable := 'new'.
	sourceCodeToExtract := '^ 1'.
	sourceCode := 'm1 ', sourceCodeToExtract.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeIncludingAReturnStatement ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 25/Feb/2020, 7:12:44 pm (UTC)'!
test07SourceCodeToExtractCanNotBeEmpty

	| newVariable sourceCode sourceCodeToExtract intervalToExtract methodToRefactor |
	newVariable _ 'new'.
	sourceCodeToExtract _ ' '.
	sourceCode _ 'm1 ^ 1'.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ ExtractToTemporary errorMessageForSourceCodeToExtractCanNotBeEmpty]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 12/Jul/2020, 8:53:38 pm (UTC)'!
test08SourceCodeToExtractCanNotHaveSyntaxErrors

	| newVariable sourceCode sourceCodeToExtract intervalToExtract methodToRefactor expectedSyntaxErrorMessage |
	newVariable _ 'new'.
	sourceCodeToExtract _ '1 +'.
	sourceCode _ 'm1 ^ 1 + 3'.
	intervalToExtract _ self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.
	 
	[ Parser parse: sourceCodeToExtract class: classToRefactor noPattern: true.
		self fail. ]
		on: SyntaxErrorNotification 
		do: [ :anError | expectedSyntaxErrorMessage _ anError messageText].
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeToExtractHasSyntaxError: expectedSyntaxErrorMessage ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 12/Jul/2020, 8:18:15 pm (UTC)'!
test09SourceCodeToExtractHasToBeOneStatement

	| newVariable sourceCode sourceCodeToExtract intervalToExtract methodToRefactor |
	newVariable := 'new'.
	sourceCodeToExtract := '1+2. 3+4'.
	sourceCode := 'm1 ', sourceCodeToExtract.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.
	
	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeToExtractHasToBeOneStatement ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 25/Feb/2020, 7:12:27 pm (UTC)'!
test10FailsIfIntervalToExtractIsBeforeMethodSourceCodeBoundaries

	| newVariable sourceCode methodToRefactor |	
	newVariable _ 'new'.
	sourceCode _ 'm1 ^ 1'.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: (-1 to: 2) from: methodToRefactor ]
		failsWith: [ ExtractToTemporary errorMessageForSourceCodeSelectionOutOfBounds ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 25/Feb/2020, 7:22:19 pm (UTC)'!
test11FailsIfIntervalToExtractIsAfterMethodSourceCodeBoundaries

	| newVariable sourceCode methodToRefactor |	
	newVariable _ 'new'.
	sourceCode _ 'm1 ^ 1'.
	classToRefactor compile: sourceCode.
	methodToRefactor _ classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: (sourceCode size - 1 to: sourceCode size + 2) from: methodToRefactor ]
		failsWith: [ ExtractToTemporary errorMessageForSourceCodeSelectionOutOfBounds ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 12/Jul/2020, 10:47:52 pm (UTC)'!
test17CannotExtractPartOfMethodName

	| newVariable sourceCode methodToRefactor intervalToExtract sourceCodeToExtract |	
	newVariable := 'new'.
	sourceCode := 'm1 ^ 1'.
	classToRefactor compile: sourceCode.
	sourceCodeToExtract := 'm1'.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForExtractingPartOfMethodSignature ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 12/Jul/2020, 8:20:08 pm (UTC)'!
test18CannotExtractPartOfMessageSend

	| newVariable sourceCode methodToRefactor intervalToExtract sourceCodeToExtract |	
	newVariable := 'new'.
	sourceCode := 'm1 ^ self m2'.
	classToRefactor compile: sourceCode.
	sourceCodeToExtract := 'm2'.
	intervalToExtract := self intervalOf: sourceCodeToExtract locatedIn: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 12/Jul/2020, 8:20:48 pm (UTC)'!
test21CannotExtractLeftSideOfAssignment

	| newVariable sourceCode methodToRefactor intervalToExtract |
	newVariable := 'new'.
	sourceCode := 'm1 | existing | existing := 2'.
	classToRefactor compile: sourceCode.
	intervalToExtract := (17 to: 24) asSourceCodeInterval. "second occurrence of 'existing' variable "
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForSourceCodeContainingInvalidExpression ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 26/Apr/2020, 7:23:20 pm (UTC)'!
test23CannotUseAReservedNameAsTheNewTemporaryVariable

	| intervalToExtract methodToRefactor newVariable sourceCode |

	ClassBuilder reservedNames do: [ :reservedName |
		newVariable := reservedName asString.
		sourceCode := 'm1 ^ 2'.
		classToRefactor compile: sourceCode.
		intervalToExtract := self intervalOf: '2' locatedIn: sourceCode.
		methodToRefactor := classToRefactor >> #m1.

		self 
			assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
			failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariableCanNotBeAReservedName: newVariable ] ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'RNG 12/Jul/2020, 10:48:18 pm (UTC)'!
test25CannotExtractArgumentNamesFromMethodSignature

	| intervalToExtract methodToRefactor newVariable sourceCode |
	newVariable := 'new'.
	sourceCode := 'm1: arg ^ self'.
	classToRefactor compile: sourceCode.
	intervalToExtract := self intervalOf: 'arg' locatedIn: sourceCode.
	methodToRefactor := classToRefactor >> #m1:.

	self 
		assertCreation: [ ExtractToTemporary named: newVariable at: intervalToExtract from: methodToRefactor ]
		failsWith: [ SourceCodeOfTemporaryToBeExtractedPrecondition errorMessageForExtractingPartOfMethodSignature ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'JEC 12/Oct/2025, 3:10:00 am (UTC)'!
test34CannotExtractWhenIntervalsAreFromDifferentPiecesOfCode

	| sourceCode methodToRefactor |

	sourceCode := 'm1 10. 99.'.
	classToRefactor compile: sourceCode.
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ 
			ExtractToTemporary 
				named: 'var' 
				intervals: { (4 to: 5) asSourceCodeInterval . (8 to: 9) asSourceCodeInterval } 
				from: methodToRefactor ]
		failsWith: [ ExtractToTemporary extractingDifferentPiecesOfCodeErrorMessage ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'JEC 12/Oct/2025, 3:12:00 am (UTC)'!
test40CannotExtractAnyPieceOfCodeWithoutIntervals

	| methodToRefactor |

	classToRefactor compile: 'm1 10. 10.'.
	methodToRefactor := classToRefactor >> #m1.

	self 
		assertCreation: [ ExtractToTemporary named: 'var' intervals: {} from: methodToRefactor ]
		failsWith: [ ExtractToTemporary cannotExtractTemporaryWithoutIntervalsErrorMessage ]! !

!ExtractToTemporaryTest methodsFor: 'tests - validations' stamp: 'JEC 12/Oct/2025, 3:13:00 am (UTC)'!
test42AppliesPreconditionsWhenTryingToExtractMultiplePiecesOfCode

	classToRefactor compile: 'm1 10. 10'.
	
	self 
		assertCreation: [ ExtractToTemporary 
				named: '' 
				intervals: { (4 to: 5) asSourceCodeInterval . (8 to: 9) asSourceCodeInterval } 
				from: (classToRefactor >> #m1) ]
		failsWith: [ NewTemporaryPrecondition errorMessageForEmptyTemporaryVariable ]! !

!InlineMethodTest methodsFor: 'refactoring helpers' stamp: 'FB 3/Nov/2021, 11:50:39 pm (UTC)'!
messageNodeReferenceOf: anImplementor inMethod: aMethodName atIndex: aSourceCodeIndex

	| nodeToInline methodNode sourceCodeRange nodeAndRange |
	
	methodNode := (anImplementor >> aMethodName asSymbol) methodNode.
	nodeAndRange := (methodNode
		parseNodesPathAt: aSourceCodeIndex ifAbsent: [self fail]) first.
	nodeToInline := nodeAndRange key.
	(nodeToInline isKindOf: MessageNode ) ifFalse: [self failWith: 'There is no message node at the selected
		class, method and index'].
	sourceCodeRange := nodeAndRange value.
	
	^MessageNodeReference messageNode: nodeToInline selector: methodNode selector
		class: anImplementor completeSourceRange: sourceCodeRange .! !

!InlineMethodTest methodsFor: 'tests - validations' stamp: 'FB 9/Apr/2022, 3:51:52 am (UTC)'!
test12CanNotInlineMethodAccessingPrivateVariablesNotVisibleToSender

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	implementorClass addInstVarName: 'y'.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' y := 10. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 42. 
	self assertCreation: [
		InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.	
	] failsWith: [InlineMethod methodAccessPrivateVariablesNotVisibleToSenderErrorMessage ]! !

!InlineMethodTest methodsFor: 'tests - validations' stamp: 'FB 4/Nov/2021, 3:50:29 am (UTC)'!
test13CanNotInlineMethodWithMultipleReturns

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y | y := 10. (y > 9) ifTrue: [^y]. ^20.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 42. 
	self assertCreation: [
		InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	] failsWith: [InlineMethod methodHasMultipleReturnsErrorMessage]! !

!InlineMethodTest methodsFor: 'tests - validations' stamp: 'FB 4/Nov/2021, 3:50:36 am (UTC)'!
test14CanNotInlineMethodWithExplicitAndImplicitReturns

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y | y := 10. (y > 9) ifTrue: [^y]'. "Implicit ^self"
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 42. 
	self assertCreation: [
		InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	] failsWith: [InlineMethod methodHasMultipleReturnsErrorMessage]! !

!InlineMethodTest methodsFor: 'tests - validations' stamp: 'FB 14/Apr/2022, 7:23:05 pm (UTC)'!
test24CanNotInlineCascadingMessageSend

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector  
		methodWithUsagesSource implementorAndSenderClass |
	 
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm2' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^5'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm3' asSymbol.
	methodWithUsagesSource := methodWithUsageSelector, ' self m1 ; m2.'.
	
	implementorAndSenderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 15.
	
	self assertCreation: [
		InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	] failsWith: [InlineMethod messageSendCanNotBeCascadeErrorMessage ]! !

!InlineMethodTest methodsFor: 'tests - validations' stamp: 'FB 21/May/2022, 9:10:00 pm (UTC)'!
test25CanNotInlineMethodFromAnotherClassReferencingSuper

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. super m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 51. 
	
	self assertCreation: [
		InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	] failsWith: [InlineMethod methodToInlineReferencesSuperErrorMessage ]! !

!InlineMethodTest methodsFor: 'tests - validations' stamp: 'FB 21/May/2022, 9:22:49 pm (UTC)'!
test26CanNotInlineMethodFromAnotherClassReferencingSelfWhenReceiverIsNotAVariable

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 51. 
	
	self assertCreation: [
		InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	] failsWith: [InlineMethod methodToInlineReferencesSelfErrorMessage]! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 3/Nov/2021, 11:40:24 pm (UTC)'!
test01InlineMethodOfSameClassWithoutReturnValue

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' self doSomething.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | self m1. ^t'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 15. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t | self doSomething. ^t'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 4/Nov/2021, 3:47:31 am (UTC)'!
test02InlineMethodOfSameClassWithParametersAndWithoutReturnValue

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1:with:' asSymbol.
	methodToInlineSource := 'm1: firstParam with: secondParam
	self use: firstParam. 
	self doSomethingWithParam: firstParam andParam: secondParam.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | self m1: 5 with: ''aString''.
	 ^t'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 15. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t | self use: 5.
self doSomethingWithParam: 5 andParam: ''aString''.
	 ^t'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 4/Nov/2021, 3:47:48 am (UTC)'!
test03InlineMethodOfSameClassReturningConstantLiteral

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^10.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | t := self m1. ^t'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 20. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t | t := 10. ^t'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 4/Nov/2021, 3:47:57 am (UTC)'!
test04InlineMethodOfSameClassWithMultipleStatementsAndReturn

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' self m3.
self m4: 5.
 ^10.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | t := self m1. ^t'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 20. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t | self m3.
self m4: 5.
t := 10. ^t'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 4/Jul/2022, 11:44:38 pm (UTC)'!
test05InlineMethodOfSameClassWithUnusedReturn

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' self m3. ^10.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' self m1. ^20'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 9. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' self m3.
10. ^20'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 4/Nov/2021, 3:48:13 am (UTC)'!
test06InlineMethodOfSameClassThatDeclaresTemporaryVariables

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | t := self m1. ^20'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 20. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y z | y := 10.
self m3.
t := y. ^20'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 4/Nov/2021, 3:49:35 am (UTC)'!
test07InlineMethodOfSameClassThatDeclaresTemporaryVariablesAlreadyDeclared

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | t := self m1. ^20'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 22. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y y1 z | y1 := 10.
self m3.
t := y1. ^20'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'HAW 10/Sep/2026, 7:59:13 pm (UTC)'!
test08InlineMethodDeclaresVariablesInClosestScope

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | t := [self m1]. ^20'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 22. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y | t := [| y1 z |
	y1 := 10.
self m3.
y1]. ^20'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'HAW 10/Sep/2026, 7:59:19 pm (UTC)'!
test09InlineMethodReturnedInFullClosureKeepsTheReturnSymbol

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. ^y.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | t := [^self m1]. ^20'. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 23. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y | t := [| y1 z |
	y1 := 10.
^y1]. ^20'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'HAW 10/Sep/2026, 7:59:24 pm (UTC)'!
test10InlineMethodOfAnotherClass

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 expectedNewSourceCode refactoring implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | ^', implementorClass name, ' new ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 51. 
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y y1 z | y1 := 10.
^y1'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'HAW 10/Sep/2026, 7:59:29 pm (UTC)'!
test11InlineClassMethodOfAnotherClass

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 expectedNewSourceCode refactoring implementorClass senderClass |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t y | ^', implementorClass name, ' ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 46. 
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' | t y y1 z | y1 := 10.
',  implementorClass name, ' m3.
^y1'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 5/Nov/2021, 2:06:01 am (UTC)'!
test15InlineUsagesOnSameClase

	| methodToInlineSource methodToInlineSelector nodeToInline1 nodeToInline2 methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | t y | t := self m1. y := self m1. ^20'.  
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline1 := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 22.
	nodeToInline2 := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 35.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline1 with: nodeToInline2)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' | t y y1 z y2 z1 | y1 := 10.
self m3.
t := y1. y2 := 10.
self m3.
y := y2. ^20'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 6/Nov/2021, 8:27:57 pm (UTC)'!
test16InlineUsageAndRemoveMethod

	| methodToInlineSource methodToInlineSelector nodeToInline1 methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y z | y := 10. self m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | t y | t := self m1. ^20'.  
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline1 := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 22.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline1)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' | t y y1 z | y1 := 10.
self m3.
t := y1. ^20'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.
	self deny: (implementorClass canUnderstand: methodToInlineSelector).! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 30/Jun/2022, 12:52:09 am (UTC)'!
test17InlineMethodWithASingleReturnLineWhenLiteralResultIsNotAssigned

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^1.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' self m1. ^20'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 10.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' 1. ^20'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'HAW 10/Sep/2026, 7:59:34 pm (UTC)'!
test18InlineMethodUsedInLastStatementOfClosure

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | x y | x := 1. y := 2. ^x + y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | b | b := [1 + self m1]. ^b value'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 26.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' | b | b := [| x y |
	x := 1.
y := 2.
1 + (x + y)]. ^b value'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'HAW 10/Sep/2026, 7:59:40 pm (UTC)'!
test19InlineMethodIndentation

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | x y | 
		x := 1.
		y := 2.
		^x + y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | b |
		b := [
			1 + self m1
		].
		^b value'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 32.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' | b |
		b := [
			| x y |
	x := 1.
			y := 2.
			1 + (x + y)
		].
		^b value'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 5/Jul/2022, 12:01:39 am (UTC)'!
test20InlineMethodUsedInReturn

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^5'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' ^3 * self m1'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 14.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' ^3 * 5'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 5/Jul/2022, 12:01:53 am (UTC)'!
test21InlineMethodAddsParenthesesWhenNeeded

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, '^5 + 10'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' ^3 * self m1'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 14.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' ^3 * (5 + 10)'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 9/Apr/2022, 3:52:48 am (UTC)'!
test22InlineMethodOnSeveralSenders

	| methodToInlineSource methodToInlineSelector nodeToInline1 nodeToInline2 methodWithUsageSelector1 methodWithUsageSelector2 
		methodWithUsagesSource methodWithUsagesSource2 implementorClass senderClass expectedNewSourceCode1 expectedNewSourceCode2 refactoring  |
	 
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^1.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector1 := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsageSelector1, ' ^self m1.'.
	methodWithUsageSelector2 := 'm3' asSymbol.
	methodWithUsagesSource2 := methodWithUsageSelector2, ' ^self m1.'.
	senderClass compile: methodWithUsagesSource.
	senderClass compile: methodWithUsagesSource2.
	
	nodeToInline1 := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector1 atIndex: 10.
	nodeToInline2 := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector2 atIndex: 10.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline1 with: nodeToInline2)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode1 := methodWithUsageSelector1,' ^1.'.
	self assert: expectedNewSourceCode1 equals: (senderClass >> methodWithUsageSelector1) sourceCode.
	
	expectedNewSourceCode2 := methodWithUsageSelector2,' ^1.'.
	self assert: expectedNewSourceCode2 equals: (senderClass >> methodWithUsageSelector2) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 9/Apr/2022, 4:03:03 am (UTC)'!
test23InlineMethodFromSameClassAccessingPrivateVariables

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector  
		methodWithUsagesSource implementorAndSenderClass expectedNewSourceCode refactoring  |
	 
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	implementorAndSenderClass addInstVarName: 'y'.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' y := 1. ^y.'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsageSelector, ' ^self m1.'.
	
	implementorAndSenderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 10.
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,' y := 1.
^y.'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'HAW 10/Sep/2026, 7:59:45 pm (UTC)'!
test27InlineMethodFromAnotherClassReferencingSelfWhenReceiverIsAVariable

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsageSelector methodWithUsageSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' | y | y := 10. self m3. ^y.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' | t | t := ', implementorClass name, ' new . ',
	 '^ t ', methodToInlineSelector. 
	senderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsageSelector atIndex: 58. 
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
		
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector, ' | t y | t := ', implementorClass name, ' new . y := 10.
t m3.
^y'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsageSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 5/Jul/2022, 12:02:04 am (UTC)'!
test28InlineMethodWithReturnUsedInSenderReturningImplicitSelf

	| implementorAndSenderClass methodToInlineSource methodToInlineSelector 
		nodeToInline methodWithUsageSelector methodWithUsageSource expectedNewSourceCode refactoring |
	
	implementorAndSenderClass  := self createClassNamed: self implementorClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^ 2 asString'.
	implementorAndSenderClass compile: methodToInlineSource.
	
	methodWithUsageSelector := 'm2' asSymbol.
	methodWithUsageSource := methodWithUsageSelector, ' self ' , methodToInlineSelector. 
	implementorAndSenderClass compile: methodWithUsageSource.
	
	nodeToInline := self messageNodeReferenceOf: implementorAndSenderClass inMethod: methodWithUsageSelector atIndex: 9. 
	
	refactoring := InlineMethod 
		from: implementorAndSenderClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: false.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsageSelector,'  2 asString'.
	self assert: expectedNewSourceCode equals: (implementorAndSenderClass >> methodWithUsageSelector) sourceCode! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 29/Jun/2022, 11:27:41 pm (UTC)'!
test29InlineMethodWithASingleReturnLineWhenNonLiteralResultIsNotAssigned

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^self doSomething.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' self m1. ^20'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 10.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' self doSomething. ^20'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 29/Jun/2022, 11:48:13 pm (UTC)'!
test30InlineMethodUsedInBrace

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^10'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' ^self m2: { 2. self m1. 4 }'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 24.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' ^self m2: { 2. 10. 4 }'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 5/Jul/2022, 12:18:46 am (UTC)'!
test31InlineMethodUsedInBraceAddingParentheses

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^10 + 5'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' ^self m2: { 2. self m1 * 3. 4 }'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 24.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' ^self m2: { 2. (10 + 5) * 3. 4 }'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 5/Jul/2022, 12:47:48 am (UTC)'!
test32InlineMethodUsedReturningWithDotInMessageNode

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^10.'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' self m1 + 10. ^20.'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 10.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' 10 + 10. ^20.'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'FB 28/Aug/2022, 1:07:50 am (UTC)'!
test33InlineMethodReturningMessageNodeDoesNotAddParenthesesWhenNotNeeded

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |
	
	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, '^5 + 10'.
	implementorClass compile: methodToInlineSource.
	
	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | res | res := self m1. ^res'.
	senderClass compile: methodWithUsagesSource.
	
	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 24.
	
	refactoring := InlineMethod 
		from: implementorClass >> methodToInlineSelector 
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.
	
	refactoring apply.
	
	expectedNewSourceCode := methodWithUsagesSelector,' | res | res := 5 + 10. ^res'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'HAW 10/Sep/2026, 7:56:45 pm (UTC)'!
test34InlineMethodAsLastStatementOfClosureDoesNotLeaveADot

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |

	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' self doSomething.'.
	implementorClass compile: methodToInlineSource.

	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' | b | b := [self m1 ]. ^b value'.
	senderClass compile: methodWithUsagesSource.

	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 21.

	refactoring := InlineMethod
		from: implementorClass >> methodToInlineSelector
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.

	refactoring apply.

	expectedNewSourceCode := methodWithUsagesSelector,' | b | b := [self doSomething ]. ^b value'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'HAW 10/Sep/2026, 7:56:50 pm (UTC)'!
test35InlineMethodAsLastStatementOfMethodDoesNotLeaveADot

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |

	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, ' ^self doSomething.'.
	implementorClass compile: methodToInlineSource.

	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, ' ^self m1'.
	senderClass compile: methodWithUsagesSource.

	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 10.

	refactoring := InlineMethod
		from: implementorClass >> methodToInlineSelector
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.

	refactoring apply.

	expectedNewSourceCode := methodWithUsagesSelector,' ^self doSomething'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'tests - successful' stamp: 'HAW 10/Sep/2026, 9:04:39 pm (UTC)'!
test36InlineMethodKeepsTheLayoutOfAMethodBody

	| methodToInlineSource methodToInlineSelector nodeToInline methodWithUsagesSelector methodWithUsagesSource
	 implementorClass senderClass expectedNewSourceCode refactoring |

	implementorClass  := self createClassNamed: self implementorClass.
	senderClass  := self createClassNamed: self senderClass.
	methodToInlineSelector := 'm1' asSymbol.
	methodToInlineSource := methodToInlineSelector, '

	| x y |

	x := 1.
	y := 2.

	^x + y'.
	implementorClass compile: methodToInlineSource.

	methodWithUsagesSelector := 'm2' asSymbol.
	methodWithUsagesSource := methodWithUsagesSelector, '

	^self m1'.
	senderClass compile: methodWithUsagesSource.

	nodeToInline := self messageNodeReferenceOf: senderClass inMethod: methodWithUsagesSelector atIndex: 12.

	refactoring := InlineMethod
		from: implementorClass >> methodToInlineSelector
		intoSendersAndUsages: (Array with: nodeToInline)
		removeMethod: true.

	refactoring apply.

	expectedNewSourceCode := methodWithUsagesSelector, '

	| x y |

	x := 1.
	y := 2.
	^x + y'.
	self assert: expectedNewSourceCode equals: (senderClass >> methodWithUsagesSelector) sourceCode.! !

!InlineMethodTest methodsFor: 'class factory' stamp: 'FB 22/Nov/2021, 9:09:44 pm (UTC)'!
anotherSenderClass

	^#AnotherSenderOfMethodToInlineClass! !

!InlineMethodTest methodsFor: 'class factory' stamp: 'FB 26/Jun/2021, 11:23:34 pm (UTC)'!
implementorClass

	^#ImplementorOfMethodToInlineClass! !

!InlineMethodTest methodsFor: 'class factory' stamp: 'FB 17/Sep/2021, 3:31:21 am (UTC)'!
senderClass

	^#SenderOfMethodToInlineClass! !

!InlineTemporaryVariableTest methodsFor: 'class factory' stamp: 'FB 12/Jul/2020, 6:20:19 pm (UTC)'!
classToRefactor

	^#ClassWithMethodToInlineTemporaryVariable! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 21/Nov/2020, 10:07:04 pm (UTC)'!
test03RefactoringRemovesUnusedTemporaryVariableDeclaration

	|classToRefactor originalSource methodToRefactorName refactoring tempVarToInline expectedSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName , ' |', tempVarToInline,'| ^nil.'.
	classToRefactor compile: originalSource.
	expectedSourceCode := methodToRefactorName, ' ^nil.'.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (5 to: 6)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	self assert: expectedSourceCode
		equals: ((classToRefactor >> methodToRefactorName asSymbol)) sourceCode.! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 21/Nov/2020, 10:07:44 pm (UTC)'!
test04AssignmentToVariableToInlineIsDeleted

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 2. ^nil.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^nil.'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 4/Dec/2020, 1:24:05 pm (UTC)'!
test05RefactoringScopeIsBetweenAssignments

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1. t1 := 2. t1 := 3. ^nil.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (20 to: 21)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | t1 | t1 := 1. t1 := 3. ^nil.'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 15/Feb/2021, 9:58:26 pm (UTC)'!
test06InlinedVariableUsagesAreReplacedWithTheVariablesValue

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := self m1: 1. self m2: t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' self m2: self m1: 1'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 15/Feb/2021, 10:05:02 pm (UTC)'!
test07RefactoringDoesNotAddParenthesesWhenTempIsLiteralAndReferenceIsReceiverOfUnaryMessage

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1. ^t1 abs'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^1 abs'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 15/Feb/2021, 10:05:24 pm (UTC)'!
test08RefactoringDoesNotAddParenthesesWhenTempIsLiteralAndReferenceIsArgumentOfBinaryMessage

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1. ^5 * t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^5 * 1'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 15/Feb/2021, 10:06:27 pm (UTC)'!
test09RefactoringAddsParenthesesWhenTempIsBinaryAndReferenceIsArgumentOfBinaryMessage

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1 + 2. ^5 * t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^5 * (1 + 2)'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 15/Feb/2021, 10:06:31 pm (UTC)'!
test10RefactoringDoesNotAddParenthesesWhenTempIsBinaryAndReferenceIsReceiverOfBinaryMessage

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1 + 2. ^t1 * 5'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^1 + 2 * 5'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 15/Feb/2021, 10:10:13 pm (UTC)'!
test11RefactoringAddsParenthesesWhenTempIsBinaryAndReferenceIsReceiverOfUnaryMessage

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1 - 2. ^t1 abs'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^(1 - 2) abs'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 20/Feb/2021, 8:43:27 pm (UTC)'!
test12RefactoringAddsParenthesesWhenTempIsBinaryAndReferenceIsPartOfMessageWhereReceiverIsAnotherReferenceToTemp

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1 * 5. ^t1 + t1 + t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^1 * 5 + (1 * 5) + (1 * 5)'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 17/Feb/2021, 12:30:24 am (UTC)'!
test13RefactoringDoesNotAddParenthesesWhenTempIsBinaryAndReferenceIsArgumentOfKeywordMessage

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := 1 + 2. ^self m2: t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^self m2: 1 + 2'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 17/Feb/2021, 12:29:05 am (UTC)'!
test14RefactoringAddsParenthesesWhenTempIsKeywordMessageAndReferenceIsReceiverOfBinaryMessage

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := self m2: 5. ^t1 * 2'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^(self m2: 5) * 2'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 17/Feb/2021, 1:03:38 am (UTC)'!
test15RefactoringDoesNotAddParenthesesWhenTempIsKeywordMessageAndReferenceIsArgumentOfBinaryMessage

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := self m2: 5. ^2 * t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^2 * self m2: 5'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 20/Feb/2021, 8:37:35 pm (UTC)'!
test16RefactoringAddsParenthesesWhenTempIsKeywordAndReferenceIsPartOfMessageWhereArgumentIsAnotherReferenceToTemp

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := self m2: 5. ^t1 + t1 + 2 + t1 + t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^(self m2: 5) + (self m2: 5) + 2 + (self m2: 5) + self m2: 5'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 20/Feb/2021, 11:56:15 pm (UTC)'!
test17RefactoringDoesNotAddParenthesesWhenBothValueAndReferenceAlreadyHaveThem

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := (1 + 2). ^t1 + t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^(1 + 2) + (1 + 2)'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 10/Mar/2021, 11:03:50 pm (UTC)'!
test18RefactoringInlinesUsageOnNextAssignment

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 t2 | t1 := 2 * 3. t2 := 5 * t1. t1 := 3 + t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (14 to: 15)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | t1 t2 | t2 := 5 * (2 * 3). t1 := 3 + (2 * 3)'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode ! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 24/Apr/2021, 10:16:08 pm (UTC)'!
test20RefactoringBlockTemporaryDoesNotChangeFollowingBlocksDeclaringTemporaryWithSameName

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ | t | t := 10. ^t+1]'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (23 to: 24)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b1 b2| b1 := [ ^2 * 2]. b2 := [ | t | t := 10. ^t+1]'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 24/Apr/2021, 10:16:29 pm (UTC)'!
test21RefactoringBlockTemporaryDoesNotChangePreviousBlocksDeclaringTemporaryWithSameName

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ | t | t := 10. ^t+1]'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (58 to: 59)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ ^10+1]'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 30/Apr/2021, 1:08:23 am (UTC)'!
test22RefactoringNestedBlockTemporaryDoesNotChangeFollowingBlocksDeclaringTemporaryWithSameName

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b1 b2| b1 := [ | b3 | b3 := [ | t | t := 2. ^t * 2]. ^b3 ]. b2 := [ | t | t := 10. ^t+1]'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (38 to: 39)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b1 b2| b1 := [ | b3 | b3 := [ ^2 * 2]. ^b3 ]. b2 := [ | t | t := 10. ^t+1]'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 2/May/2021, 11:16:02 pm (UTC)'!
test23RefactoringBlockTemporaryFromDeclarationDoesNotChangePreviousBlocksDeclaringTemporaryWithSameName

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ | t | t := 10. ^t+1]'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (54 to: 55)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ ^10+1]'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 2/May/2021, 11:25:41 pm (UTC)'!
test24RefactoringOnlyRemovesDeclarationOfInlinedTemporary

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't2'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ | t t2 t3 | t := 10. ^t+1]'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (56 to: 57)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b1 b2| b1 := [ | t | t := 2. ^t * 2]. b2 := [ | t t3 | t := 10. ^t+1]'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode

	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 22/Dec/2021, 12:27:55 am (UTC)'!
test26RefactoringAddsParenthesesWhenTempIsKeywordMessageAndReferenceIsArgumentOfKeywordMessage

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 | t1 := self m2: 5. ^t1 do: 10'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (11 to: 12)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^(self m2: 5) do: 10'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
	
	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 28/Dec/2021, 11:55:21 pm (UTC)'!
test27RefactoringIsAllowedWhenVariableIsUsedInBlockButThereAreNoMoreAssignments

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 b | t1 := 2. b := [5 * t1]. ^b value.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (13 to: 14)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b | b := [5 * 2]. ^b value.'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
	
	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 19/Mar/2022, 5:23:51 pm (UTC)'!
test29InlineReferenceAtEndOfMethodWithoutStatementEndingCharacter

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 'a'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | a | a := 1. ^self printOn: a'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (6 to: 6)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' ^self printOn: 1'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
	
	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 19/Mar/2022, 5:26:10 pm (UTC)'!
test30InlineFromReferenceLocatedAfterAssignment

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 'a'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | a | a := 1. self printOn: a. ^a'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (36 to: 36)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' self printOn: 1. ^1'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
	
	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 4/Apr/2022, 3:28:22 am (UTC)'!
test31InlineTemporaryUsedAfterAssignmentInBlock

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b t1 | b := [ t1 := 30. ^t1]. ^t1'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (21 to: 22)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b t1 | b := [ ^30]. ^t1'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
	
	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - successful' stamp: 'FB 12/Sep/2022, 12:39:21 am (UTC)'!
test32InlineTemporaryUsedAfterBlockWhenNotReferencedByIt

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring expectedNewSourceCode |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't2'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | b t1 t2 | b := [ t1 := 30. 15.]. t2 := 20. ^b2 value: 10 + t2'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (40 to: 41)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	refactoring apply.
	
	expectedNewSourceCode := methodToRefactorName,' | b t1 | b := [ t1 := 30. 15.]. ^b2 value: 10 + 20'.
	self assert: expectedNewSourceCode equals: (classToRefactor >> methodToRefactorName asSymbol) sourceCode
! !

!InlineTemporaryVariableTest methodsFor: 'tests - validations' stamp: 'FB 29/Dec/2021, 12:02:39 am (UTC)'!
test01VariableToInlineCanNotBeEmpty

	|classToRefactor originalSource methodToRefactorName tempVarToInline |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := ' '.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName , ' |', tempVarToInline,'| ^nil.'.
	classToRefactor compile: originalSource.
	
	self assertCreation: [InlineTemporaryVariable named: tempVarToInline atUsageInterval: (5 to: 6)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.]
	 failsWith: [InlineTemporaryVariable selectionIsNotATemporaryVariableErrorMessage].! !

!InlineTemporaryVariableTest methodsFor: 'tests - validations' stamp: 'FB 29/Dec/2021, 12:02:39 am (UTC)'!
test02VariableToInlineMustExistInSelector

	|classToRefactor originalSource methodToRefactorName tempVarToInline |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | |  ^nil.'.
	classToRefactor compile: originalSource.
	
	self assertCreation: [InlineTemporaryVariable named: tempVarToInline atUsageInterval: (5 to: 6)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.]
	 failsWith: [InlineTemporaryVariable selectionIsNotATemporaryVariableErrorMessage].! !

!InlineTemporaryVariableTest methodsFor: 'tests - validations' stamp: 'FB 12/Jul/2022, 11:02:45 pm (UTC)'!
test19CanNotRefactorTemporaryWhenAssignedAfterBeingReferencedInBlock

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 b| t1 := 2. b := [5 * t1]. t1 := 3. ^b value.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (12 to: 13)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	[refactoring apply. self fail] 
		on: Error
		do: [:error | self assert: InlineTemporaryVariable temporaryAssignedAfterUsedInBlockErrorMessage 
			equals: error messageText ]
	
	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - validations' stamp: 'FB 12/Jul/2022, 10:07:21 pm (UTC)'!
test25CanNotRefactorTemporaryFromDeclarationWhenThereAreMultipleAssignments

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 b| t1 := 2. b := 5 * t1. t1 := 3. ^b value.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (6 to: 7)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	[refactoring apply. self fail.] 
		on: Error
		do: [:error | self assert: InlineTemporaryVariable moreThanOneAssignmentErrorMessage 
			equals: error messageText ]
	
	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - validations' stamp: 'FB 12/Jul/2022, 10:07:26 pm (UTC)'!
test28CanNotRefactorTemporaryFromDeclarationWhenVariableIsReferencedButNotAssigned

	| classToRefactor originalSource methodToRefactorName tempVarToInline refactoring |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 't1'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | t1 b | b := 5 * t1. ^b value.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (6 to: 7)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	[refactoring apply. self fail.] 
		on: Error
		do: [:error | self assert: InlineTemporaryVariable assignmentNotFoundErrorMessage 
			equals: error messageText ]
	
	
! !

!InlineTemporaryVariableTest methodsFor: 'tests - validations' stamp: 'FB 12/Sep/2022, 12:27:01 am (UTC)'!
test33InlineTemporaryFromReferenceAfterAssignmentInBlock

	|classToRefactor originalSource methodToRefactorName tempVarToInline refactoring |
	
	classToRefactor  := self createClassNamed: self classToRefactor.
	tempVarToInline := 'a'.
	methodToRefactorName := 'm1'.
	originalSource := methodToRefactorName,' | a | a := 1. 10 > 9 ifTrue: [a := 10]. ^a.'.
	classToRefactor compile: originalSource.
	
	refactoring := InlineTemporaryVariable named: tempVarToInline atUsageInterval: (45 to: 45)
		 inMethod: classToRefactor >> methodToRefactorName asSymbol.
	
	[refactoring apply. self fail.] 
		on: Error
		do: [:error | self assert: InlineTemporaryVariable temporaryIsAssignedInBlockBeforeReferenceErrorMessage 
			equals: error messageText ]
	
	
! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:01:00 pm (UTC)'!
test01ChangesTheSuperclassOfTheClassToRefactor

	| classToRefactor refactoring oldSuperclass newSuperclass |
	
	classToRefactor := self createClassNamed: #ClassToAddSuperclass.
	oldSuperclass := classToRefactor superclass.
	
	refactoring := InsertSuperclass to: classToRefactor named: #AddedSuperclass.
	newSuperclass := refactoring apply.
	
	self assert: newSuperclass equals: classToRefactor superclass.
	self assert: (newSuperclass subclasses includes: classToRefactor).
	self assert: oldSuperclass equals: newSuperclass superclass.! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 13/Mar/2025, 5:59:15 pm (UTC)'!
test02ClassToRefactorStructureIsMaintained

	| classToRefactor refactoring instanceVariables category classVariables poolDictionaries |
	
	instanceVariables := 'instVar1 instVar2'.
	classVariables := 'ClassVar1 ClassVar2'.
	poolDictionaries := 'SharedAAA'.
	category :=  self classCategoryOfTestData.
	
	[ Smalltalk at: poolDictionaries asSymbol put: Dictionary new.
	classToRefactor := self 
		createClassNamed: #ClassToAddSuperclass
		subclassOf: self defaultSuperclass 
		instanceVariableNames: instanceVariables
		classVariableNames: classVariables
		poolDictionaries: poolDictionaries
		category: category.
	
	refactoring := InsertSuperclass to: classToRefactor named: #AddedSuperclass.
	refactoring apply.
	
	self assert: instanceVariables equals: classToRefactor instanceVariablesString.
	self assert: classVariables equals: classToRefactor classVariablesString.
	self assert: poolDictionaries equals: classToRefactor sharedPoolsString.
	self assert: category equals: classToRefactor category ] ensure: [ Smalltalk removeKey: poolDictionaries asSymbol ].! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:17:48 pm (UTC)'!
test03NewSuperclassCategoryIsTheSameAsClassToRefactorCategory

	| classToRefactor refactoring classToRefactorCategory newSuperclass |

	classToRefactorCategory := self classCategoryOfTestData.
	classToRefactor := self createClassNamed: #ClassToAddSuperclass category: classToRefactorCategory.
	
	refactoring := InsertSuperclass to: classToRefactor named: #AddedSuperclass.
	newSuperclass := refactoring apply.
	
	self assert: classToRefactorCategory equals: newSuperclass category.! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:19:28 pm (UTC)'!
test04NewSuperclassHasNoVariables

	| classToRefactor refactoring classToRefactorCategory newSuperclass |

	classToRefactorCategory := self classCategoryOfTestData.
	classToRefactor := self createClassNamed: #ClassToAddSuperclass category: classToRefactorCategory.
	
	refactoring := InsertSuperclass to: classToRefactor named: #AddedSuperclass.
	newSuperclass := refactoring apply.
	
	self assert: newSuperclass instVarNames isEmpty.
	self assert: newSuperclass classVarNames isEmpty.
	self assert: newSuperclass sharedPools isEmpty.! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:21:16 pm (UTC)'!
test05NewSuperclassShouldNotExist

	| classToRefactor existingClass existingClassName |
	
	classToRefactor := self createClassNamed: #ClassToAddSuperclass.
	existingClassName := #AlreadyExistingClass.
	existingClass := self createClassNamed: existingClassName.
	
	self
		assertCreation: [ InsertSuperclass to: classToRefactor named: existingClassName ]
		failsWith: [ NewClassPrecondition errorMessageForAlreadyExistClassNamed: existingClass name ].! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:22:13 pm (UTC)'!
test06NewSuperclassNameHasToBeASymbol

	self 
		assertCreation: [ InsertSuperclass to: self class named: 'aString' ]
		failsWith: [ NewClassPrecondition newNameMustBeSymbolErrorMessage ].! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 24/May/2019, 2:09:41 pm (UTC)'!
test07NewSuperclassNameHasToStartWithUppercaseLetter
	
	self
		assertCreation: [ InsertSuperclass to: self class named: #_A ]
		failsWith: [ NewClassPrecondition newNameMustStartWithRightLetterErrorMessage ].! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:22:31 pm (UTC)'!
test08WhenAppliedToMetaclassWorksAsWithClass

	| classToRefactor refactoring newSuperclass |
	
	classToRefactor := self createClassNamed: #ClassToAddSuperclass.
	
	refactoring := InsertSuperclass to: classToRefactor class named: #AddedSuperclass.
	newSuperclass := refactoring apply.
	
	self assert: newSuperclass equals: classToRefactor superclass.! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:22:56 pm (UTC)'!
test09NewSuperclassNameCanNotBeEmpty

	self 
		assertCreation: [ InsertSuperclass to: self class named: '' asSymbol ]
		failsWith: [ NewClassPrecondition newClassNameCanNotBeEmptyErrorMessage ].! !

!InsertSuperclassTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 1:13:27 pm (UTC)'!
test10NewSuperclassNameCanNotHaveSpaces
	
	self
		assertCreation: [ InsertSuperclass to: self class named: 'With spaces' asSymbol ]
		failsWith: [ NewClassPrecondition newClassNameCanNotHaveSeparatorsErrorMessage ].! !

!MoveToInstanceOrClassMethodTest methodsFor: 'tests' stamp: 'LMY 8/Dec/2019, 10:08:54 pm (UTC)'!
test01CannotMoveWhenReferencingInstanceVariables
	
	| classToRefactor selector referencedVariable |

	selector := #newMethod.
	referencedVariable := 'a'.

	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: referencedVariable.
	classToRefactor compile: selector asString,' ^', referencedVariable.
	
	self 
		assertCreation: [ MoveToInstanceOrClassMethod for: classToRefactor >> selector ]
		failsWith: [ MoveToInstanceOrClassMethod referencingInstanceVariablesErrorMessage ]
! !

!MoveToInstanceOrClassMethodTest methodsFor: 'tests' stamp: 'LMY 8/Dec/2019, 10:43:15 pm (UTC)'!
test02CannotMoveToClassWhenLocalVariableIsTheSameAsClassInstanceVariable
	
	| classToRefactor selector referencedVariable |

	selector := #newMethod.
	referencedVariable := 'a'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor class addInstVarName: referencedVariable.
	classToRefactor compile: selector asString, '|', referencedVariable, '|'.
	
	self 
		assertCreation: [ MoveToInstanceOrClassMethod for: classToRefactor >> selector ]
		failsWith: [ MoveToInstanceOrClassMethod localVariableConflictsWithInstanceVariableErrorMessage ]
! !

!MoveToInstanceOrClassMethodTest methodsFor: 'tests' stamp: 'LMY 9/Dec/2019, 3:35:40 am (UTC)'!
test03CannotMoveToInstanceWhenLocalVariableIsTheSameAsClassInstanceVariable
	
	| classToRefactor selector referencedVariable |

	selector := #newMethod.
	referencedVariable := 'a'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor addInstVarName: referencedVariable.
	classToRefactor class compile: selector asString, '|', referencedVariable, '|'.
	
	self 
		assertCreation: [ MoveToInstanceOrClassMethod for: classToRefactor class >> selector ]
		failsWith: [ MoveToInstanceOrClassMethod localVariableConflictsWithInstanceVariableErrorMessage ]
! !

!MoveToInstanceOrClassMethodTest methodsFor: 'tests' stamp: 'LMY 9/Dec/2019, 3:44:00 am (UTC)'!
test04InstanceMethodIsMovedToClassMethod
	
	| classToRefactor selector moveMethod |

	selector := #newMethod.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector asString.
	
	moveMethod := MoveToInstanceOrClassMethod for: classToRefactor >> selector.
	moveMethod apply.
	
	self assert: (classToRefactor class includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
! !

!MoveToInstanceOrClassMethodTest methodsFor: 'tests' stamp: 'LMY 9/Dec/2019, 3:51:21 am (UTC)'!
test05ClassMethodIsMovedToInstanceMethod
	
	| classToRefactor selector moveMethod |

	selector := #newMethod.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor class compile: selector asString.
	
	moveMethod := MoveToInstanceOrClassMethod for: classToRefactor class >> selector.
	moveMethod apply.
	
	self assert: (classToRefactor includesSelector: selector).
	self deny: (classToRefactor class includesSelector: selector).
! !

!MoveToInstanceOrClassMethodTest methodsFor: 'class factory' stamp: 'LMY 8/Dec/2019, 9:35:59 pm (UTC)'!
classToRefactorName

	^#ClassToMoveMethod! !

!PushDownInstanceVariableTest methodsFor: 'class factory' stamp: 'MSC 21/Dec/2019, 1:49:48 pm (UTC)'!
classToRefactorName
	^#ClassToPushInstanceVariableDown.! !

!PushDownInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 27/Dec/2019, 12:13:23 am (UTC)'!
test01AnInstanceVariableDoesNotExistOnClassToFactorCannotBePushedDownToSubclasses
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	
	self assertPushDownCreationFailsWith: PushDownInstanceVariable instanceVariableDoesNotExistOnClassToRefactor .
	
	self deny: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushDown).
	classToRefactor subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushDown).
	].! !

!PushDownInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 27/Dec/2019, 12:10:51 am (UTC)'!
test02AnInstanceVariableThatIsAccessedOnClassToRefactorCannotBePushedDownToAllSubclasses

	|  selector |
	
	selector := #newMethod.
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: instanceVariableToPushDown.
	classToRefactor compile: selector, '^ ', instanceVariableToPushDown.
	
	self createClassNamed: #SubclassWithoutInstVarOne subclassOf: classToRefactor.
	self createClassNamed: #SubclassWithoutInstVarTwo subclassOf: classToRefactor.
	
	self assertPushDownCreationFailsWith: (PushDownInstanceVariable 
		errorMessageForInstanceVariable: instanceVariableToPushDown 
		isAccessedInMethodsOf: classToRefactor).
	
	self assert: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushDown).
	classToRefactor subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushDown)].! !

!PushDownInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 27/Dec/2019, 12:49:31 am (UTC)'!
test03AnInstanceVariableThatIsNotAccessedOnClassToRefactorShouldBePushedDownToAllSubclasses

	|  pushDown |
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: instanceVariableToPushDown.
	self createClassNamed: #SubclassWithoutInstVarOne subclassOf: classToRefactor.
	self createClassNamed: #SubclassWithoutInstVarTwo subclassOf: classToRefactor.
	
	pushDown := PushDownInstanceVariable named: instanceVariableToPushDown from: classToRefactor.
	pushDown apply.
	
	self deny: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushDown).
	classToRefactor subclassesDo: [ :subClass |
		self assert: (subClass definesInstanceVariableNamed: instanceVariableToPushDown).
	].! !

!PushDownInstanceVariableTest methodsFor: 'setup' stamp: 'MSC 22/Dec/2019, 2:01:42 am (UTC)'!
setUp

	super setUp.
	
	instanceVariableToPushDown := 'a'.! !

!PushDownInstanceVariableTest methodsFor: 'assertions' stamp: 'jmv 20/Mar/2024, 8:00:17 pm (UTC)'!
assertPushDownCreationFailsWith: aStringOrBlock

	self
		assertCreation: [ PushDownInstanceVariable named: instanceVariableToPushDown from: classToRefactor. ]
		failsWith: aStringOrBlock ! !

!PushDownMethodTest methodsFor: 'assertions' stamp: 'HAW 13/Jul/2023, 1:15:23 am (UTC)'!
assert: classToRefactor implementsSendingSubclassResponsibility: selector

	| newMethod |

	newMethod := classToRefactor compiledMethodAt: selector ifAbsent: [ self fail ].
	self assert: newMethod isAbstract.
	self 
		assert: self methodToPushDownCategory 
		equals: (newMethod methodClass organization categoryOfElement: selector)! !

!PushDownMethodTest methodsFor: 'class factory' stamp: 'HAW 29/May/2023, 7:05:28 pm (UTC)'!
classToPushDownTo

	^ #ClassToPushDownTo! !

!PushDownMethodTest methodsFor: 'class factory' stamp: 'HAW 29/May/2023, 7:05:42 pm (UTC)'!
classToPushDownTo2

	^ #ClassToPushDownTo2! !

!PushDownMethodTest methodsFor: 'class factory' stamp: 'HAW 29/May/2023, 6:52:33 pm (UTC)'!
classToRefactorName	

	^#ClassToPushMethodDown! !

!PushDownMethodTest methodsFor: 'class factory' stamp: 'HAW 13/Jul/2023, 1:13:14 am (UTC)'!
methodToPushDownCategory
	
	^'a category'! !

!PushDownMethodTest methodsFor: 'class factory' stamp: 'HAW 29/May/2023, 6:58:30 pm (UTC)'!
selectorToPushDown

	^ #newMethod! !

!PushDownMethodToOneSubclassTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 7:07:12 pm (UTC)'!
test01MethodShouldPushedDownOnlyToSelectedSubclass

	| classToPushDownTo sibling classToRefactor push |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	sibling := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown asString.
           
	self 
		assertCreation: [ push := PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo ]
		warnsWith: [ PushDownMethodToOneSubclass warningMessageNotARefactoringBecauseNotImplementedIn: { sibling } ].
	push apply.
   
	self assert: (classToPushDownTo includesSelector: self selectorToPushDown).
	self deny: (sibling includesSelector: self selectorToPushDown).
	self deny: (classToRefactor includesSelector: self selectorToPushDown).
	
	! !

!PushDownMethodToOneSubclassTest methodsFor: 'tests' stamp: 'HAW 13/Jul/2023, 1:15:48 am (UTC)'!
test02MethodShouldPushedDownIfVarNotShadowedInClassToPushDownTo

	| classToPushDownTo sibling classToRefactor instVar push |
	
	instVar := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	sibling := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor instanceVariableNames: instVar.
	
	classToRefactor compile: self selectorToPushDown, '|', instVar, '|' classified: self methodToPushDownCategory.
	sibling compile: self selectorToPushDown asString.
           
	self
		shouldnt: [ push := PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo ]
		raise: self refactoringError.
	push apply.
	
	self assert: (classToPushDownTo includesSelector: self selectorToPushDown).
	self assert: classToRefactor implementsSendingSubclassResponsibility: self selectorToPushDown.! !

!PushDownMethodToOneSubclassTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 7:08:29 pm (UTC)'!
test03MethodShouldNotPushedDownIfVarShadowedInClassToPushDownTo

	| classToPushDownTo classToRefactor instVar |
	
	instVar := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor instanceVariableNames: instVar.
	
	classToRefactor compile: self selectorToPushDown, '|', instVar, '|'.
	   
	self
		assertCreation: [ PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo ]
		failsWith: [ PushDownMethodToOneSubclass errorMessageCanNotPushDownWithShadowedInstVarsOf:
			{classToPushDownTo -> {instVar }} asDictionary].
	! !

!PushDownMethodToOneSubclassTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 7:08:41 pm (UTC)'!
test04ExistingMethodInSubclassShouldBeOverriden
	
	| classToRefactor classToPushDownMethodTo methodToPushDownSourceCode push |
	
	methodToPushDownSourceCode := self selectorToPushDown, ' ^ 2'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: methodToPushDownSourceCode.
	 
	classToPushDownMethodTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToPushDownMethodTo compile: self selectorToPushDown, ' ^ 1'.

	self 
		assertCreation: [ push := PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownMethodTo ]
		warnsWith: [ PushDownMethodToOneSubclass warningMesssageForMessageAlreadyImplementedIn: { classToPushDownMethodTo } ].
	push apply.
		
	self assert: (classToPushDownMethodTo includesSelector: self selectorToPushDown).
	self deny: (classToRefactor includesSelector: self selectorToPushDown).
	self assert: methodToPushDownSourceCode equals: (classToPushDownMethodTo >> self selectorToPushDown) sourceCode! !

!PushDownMethodToOneSubclassTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 7:08:55 pm (UTC)'!
test05AMethodCannotBePushedToClassThatIsNotInHierarchy

	| classNotInRefactorHierarchy classToRefactor |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classNotInRefactorHierarchy := self createClassNamed: self classToPushDownTo.
	
	classToRefactor compile: self selectorToPushDown asString.
         
	self 
		assertCreation: [ PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classNotInRefactorHierarchy] 
		failsWith: [ PushDownMethodToOneSubclass errorMessage: classNotInRefactorHierarchy isNotSubclassOf: classToRefactor ].
	! !

!PushDownMethodToOneSubclassTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 7:09:19 pm (UTC)'!
test06WarnWhenMethodNotImplementedInSiblings

	| classToPushDownTo classToRefactor sibling |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	sibling := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown asString.
         
	self 
		assertCreation: [ PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo] 
		warnsWith: [ PushDownMethodToOneSubclass warningMessageNotARefactoringBecauseNotImplementedIn: { sibling } ].
	! !

!PushDownMethodToOneSubclassTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 7:09:27 pm (UTC)'!
test07ShouldNotWarnWhenMethodImplementedInSiblings

	| classToPushDownTo classToRefactor sibling |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	sibling := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown asString.
 	sibling compile: self selectorToPushDown asString.
         
	self 
		shouldnt: [ PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo] 
		raise: self refactoringWarning ! !

!PushDownMethodToOneSubclassTest methodsFor: 'tests' stamp: 'HAW 13/Jul/2023, 1:16:03 am (UTC)'!
test08ShouldImplementAsSubclassResponsibilityWhenImplementedInAllSubclasses

	| classToPushDownTo classToRefactor sibling push expectedClassToRefactorSourceCode |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	sibling := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown asString classified: self methodToPushDownCategory.
 	sibling compile: self selectorToPushDown asString.
         
	push := PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo.
	push apply.
	
	expectedClassToRefactorSourceCode := String streamContents: [ :stream |
		stream nextPutAll: self selectorToPushDown; newLine; newLine; tab; nextPutAll: 'self '; nextPutAll: #subclassResponsibility asString ].

	self assert: classToRefactor implementsSendingSubclassResponsibility: self selectorToPushDown.
	self assert: expectedClassToRefactorSourceCode equals: (classToRefactor >> self selectorToPushDown) sourceCode.! !

!PushDownMethodToOneSubclassTest methodsFor: 'tests' stamp: 'HAW 17/Jul/2023, 4:24:30 pm (UTC)'!
test09AClassMethodShouldBePushedDownToSubclasses

	| classToRefactorSubclass classToRefactor push |
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	
	classToRefactor class compile: self selectorToPushDown classified: self methodToPushDownCategory.
           
	push := PushDownMethodToOneSubclass for: classToRefactor class >> self selectorToPushDown to: classToRefactorSubclass class.
	push apply.
   
	self deny: (classToRefactor class includesSelector: self selectorToPushDown).
	self assert: (classToRefactorSubclass class includesSelector: self selectorToPushDown).
	! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'HAW 13/Jul/2023, 1:16:23 am (UTC)'!
test01AnEmptyMethodOnClassToRefactorShouldBePushedDownToSubclasses

	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor push |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown asString classified: self methodToPushDownCategory.
           
	push := PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown.
	push apply.

	self assert: classToRefactor implementsSendingSubclassResponsibility: self selectorToPushDown.
	self assert: (classToRefactorSubclass1 includesSelector: self selectorToPushDown).
	self assert: (classToRefactorSubclass2 includesSelector: self selectorToPushDown).
	! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 7:05:18 pm (UTC)'!
test02CanNotPushDownWhenATemporaryVarIsAnInstVarOnASubclass
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor newVariable |
	
	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	classToRefactor compile: self selectorToPushDown, '|', newVariable, '|'.
           
	self
		assertCreation: [ PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown ]
		 failsWith: [ PushDownMethodToSubclasses errorMessageCanNotPushDownWithShadowedInstVarsOf:
			{classToRefactorSubclass2 -> {newVariable }} asDictionary ].
	   
	self deny: (classToRefactorSubclass1 includesSelector: self selectorToPushDown).
	self deny: (classToRefactorSubclass2 includesSelector: self selectorToPushDown).
	self assert: (classToRefactor includesSelector: self selectorToPushDown).! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 7:57:20 pm (UTC)'!
test03CanNotPushDownWhenAnArgumentIsAnInstVarOnASubclass
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor newVariable selectorToPushDown |
	
	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	selectorToPushDown := #newMethod:.
	classToRefactor compile: selectorToPushDown, newVariable.
           
	self
		assertCreation: [ PushDownMethodToSubclasses for: classToRefactor >> selectorToPushDown ]
		 failsWith: [ PushDownMethodToSubclasses errorMessageCanNotPushDownWithShadowedInstVarsOf:
			{classToRefactorSubclass2 -> {newVariable }} asDictionary].
	   
	self deny: (classToRefactorSubclass1 includesSelector: selectorToPushDown).
	self deny: (classToRefactorSubclass2 includesSelector: selectorToPushDown).
	self assert: (classToRefactor includesSelector: selectorToPushDown).! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'HAW 13/Jul/2023, 1:16:32 am (UTC)'!
test04AMethodAccessesClassToRefactorInstVarShouldBePushedDownToSubclasses
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor newVariable push |

	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown, '^ 1 + ', newVariable classified: self methodToPushDownCategory.
           
	push := PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown.
	push apply.

	self assert: classToRefactor implementsSendingSubclassResponsibility: self selectorToPushDown.
	self assert: (classToRefactorSubclass1 includesSelector: self selectorToPushDown).
	self assert: (classToRefactorSubclass2 includesSelector: self selectorToPushDown).! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'HAW 13/Jul/2023, 1:16:43 am (UTC)'!
test05AMethodWritesClassToRefactorInstVarShouldBePushedDownToSubclasses
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor newVariable push |

	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown, ' a = 1' classified: self methodToPushDownCategory.
           
	push := PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown.
	push apply.
   
	self assert: classToRefactor implementsSendingSubclassResponsibility: self selectorToPushDown.
	self assert: (classToRefactorSubclass1 includesSelector: self selectorToPushDown).
	self assert: (classToRefactorSubclass2 includesSelector: self selectorToPushDown).! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 6:58:09 pm (UTC)'!
test06AMethodInClassToRefactorExistsInSubClassesShouldOverrideTheExistingSubClassesMethod
	
	| classToRefactor classToRefactorSubclass newVariable subclassMethodContent |
	
	newVariable := 'a'.	
	subclassMethodContent := self selectorToPushDown, '
	^ 2 + ', newVariable, '.'.

	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactor compile: subclassMethodContent.
	 
	classToRefactorSubclass := self createClassNamed: #Subclass subclassOf: classToRefactor.
	classToRefactorSubclass compile: self selectorToPushDown, '^ 1 + ', newVariable.

	self 
		assertCreation: [ PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown ]
		warnsWith: [ PushDownMethodToSubclasses warningMesssageForMessageAlreadyImplementedIn: { classToRefactorSubclass } ].
		
	self assert: (classToRefactorSubclass includesSelector: self selectorToPushDown).
	self assert: (classToRefactor includesSelector: self selectorToPushDown).! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'HAW 13/Jul/2023, 1:16:58 am (UTC)'!
test07AMethodInClassToRefactorShouldBeReplacedBySubclassResponsibility

	| classToRefactor push  expectedBody selectorToPushDown selectorAndArguments |

	classToRefactor := self createClassNamed: self classToRefactorName.
	self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.

	selectorToPushDown := #newMethod:.
	selectorAndArguments := selectorToPushDown, ' anABC'.
	classToRefactor compile: selectorAndArguments classified: self methodToPushDownCategory.

	push := PushDownMethodToSubclasses for: classToRefactor >> selectorToPushDown.
	push apply.

	expectedBody := String streamContents: [ :stream |
		stream nextPutAll: selectorAndArguments ; newLine; newLine; tab; nextPutAll: 'self '; nextPutAll: #subclassResponsibility asString ].

	self assert: classToRefactor implementsSendingSubclassResponsibility: selectorToPushDown.
	self assert: expectedBody equals: (classToRefactor >> selectorToPushDown) sourceCode. 
'newMethod: anABC
 
 	self subclassResponsibility' ! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 7:57:59 pm (UTC)'!
test08AMethodInClassToRefactorExistsInSubClassesShouldNotOverrideTheExistingSubClassesMethodIfEquivalent

	| classToRefactor classToRefactorSubclass newVariable subclassMethodContent push |

	newVariable := 'a'.	
	subclassMethodContent := self selectorToPushDown, '
	^ 2 + ', newVariable, '.'.

	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactor compile: subclassMethodContent.

	classToRefactorSubclass := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass compile: subclassMethodContent.

	push := PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown.
	push apply.

	self assert: (classToRefactorSubclass includesSelector: self selectorToPushDown).
	self assert: subclassMethodContent equals: (classToRefactorSubclass >> self selectorToPushDown) getSource.
	self assert: (classToRefactor includesSelector: self selectorToPushDown).
	self deny: (subclassMethodContent = (classToRefactor >> self selectorToPushDown) getSource).! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'HAW 29/May/2023, 6:58:20 pm (UTC)'!
test09CannotPushDownMethodWithoutSubclasses

	| classToRefactor |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	
	classToRefactor compile: self selectorToPushDown.
           
	self
		assertCreation: [ PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown ]
		 failsWith: [ PushDownMethodToSubclasses errorMessageNoSubclassToPushDownMethodTo ].
	! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'HAW 17/Jul/2023, 4:24:49 pm (UTC)'!
test10AClassMethodShouldBePushedDownToSubclasses

	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor push |
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor class compile: self selectorToPushDown classified: self methodToPushDownCategory.
           
	push := PushDownMethodToSubclasses for: classToRefactor class >> self selectorToPushDown.
	push apply.
   
	self assert: classToRefactor class implementsSendingSubclassResponsibility: self selectorToPushDown.
	self assert: (classToRefactorSubclass1 class includesSelector: self selectorToPushDown).
	self assert: (classToRefactorSubclass2 class includesSelector: self selectorToPushDown).! !

!PushDownMethodToSubclassesTest methodsFor: 'tests' stamp: 'TS 10/Jul/2023, 12:47:34 am (UTC)'!
test11CannotPushDownClassMethodWithoutSubclasses

	| classToRefactor |
	
	classToRefactor := self createClassNamed: self classToRefactorName.

	classToRefactor class compile: self selectorToPushDown.
           
	self
		assertCreation: [ PushDownMethodToSubclasses for: classToRefactor class >> self selectorToPushDown ]
		 failsWith: [ PushDownMethodToSubclasses errorMessageNoSubclassToPushDownMethodTo ].! !

!PushUpInstanceVariableTest methodsFor: 'class factory' stamp: 'MSC 15/Dec/2019, 4:44:48 pm (UTC)'!
classToRefactorName
	^#ClassToPushInstanceVariableUp.! !

!PushUpInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 27/Dec/2019, 12:18:51 am (UTC)'!
test01AnInstanceVariableThatIsNotUsedOnClassToRefactorShouldBePushedUpToSuperclass

	|  pushUp |
	
	classToRefactorSuperClass := self createClassNamed: #SuperclassWithoutInstVar.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass instanceVariableNames: instanceVariableToPushUp.

	pushUp := PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor.
	pushUp apply.
	
	self assert: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	classToRefactorSuperClass subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushUp)
	].! !

!PushUpInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 27/Dec/2019, 12:18:52 am (UTC)'!
test02AnInstanceVariableOnMultipleSubclassesShouldBeRemovedFromAllSubclassesAndPushedUpToSuperClass

	|  pushUp  |
	
	classToRefactorSuperClass := self createClassNamed: #SuperclassWithoutInstVar.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass  instanceVariableNames: instanceVariableToPushUp.
	self createClassNamed: #AnoterSubClassWithInstVar subclassOf: classToRefactorSuperClass instanceVariableNames: instanceVariableToPushUp.
	
	pushUp := PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor.
	pushUp apply.
	
	self assert: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	classToRefactorSuperClass subclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushUp)
	].! !

!PushUpInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 27/Dec/2019, 12:13:23 am (UTC)'!
test03AnInstanceVariableDoesNotExistOnClassToRefactorCannotBePushedUp
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	
	self assertPushUpCreationFailsWith: PushUpInstanceVariable instanceVariableDoesNotExistOnClassToRefactor.
	
	self deny: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushUp).! !

!PushUpInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 27/Dec/2019, 12:34:42 am (UTC)'!
test04AnInstanceVariableThatIsUsedOnSuperClassAsTemporaryVariableCannotBePushedUp

	| selector |
	
	selector := #newMethod.
	
	classToRefactorSuperClass := self createClassNamed: #SuperclassWithVariableAsTemporaryVariable.
	classToRefactorSuperClass compile: selector, '| ' , instanceVariableToPushUp , ' |'.
	
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass  instanceVariableNames: instanceVariableToPushUp.
	
	self assertPushUpCreationFailsWith: (PushUpInstanceVariable 
		errorMessageForInstanceVariable: instanceVariableToPushUp 
		isDefinedInMethodsOf: classToRefactorSuperClass).
	
	self deny: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	self assert: (classToRefactor definesInstanceVariableNamed: instanceVariableToPushUp).! !

!PushUpInstanceVariableTest methodsFor: 'tests' stamp: 'MEGL 26/May/2023, 7:30:45 pm (UTC)'!
test05AnInstanceVariableOnMultipleSubclassesInDifferentLevelsShouldBeRemovedFromAllSubclassesAndPushedUpToSuperClass 

	|  pushUp subclassWithoutInstVar  |

	classToRefactorSuperClass := self createClassNamed: #SuperclassWithoutInstVar.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperClass  instanceVariableNames: instanceVariableToPushUp.
	subclassWithoutInstVar := self createClassNamed: #SubClassWithoutInstVar subclassOf: classToRefactorSuperClass.
	self createClassNamed: #SubSubClassWithInstVar subclassOf: subclassWithoutInstVar instanceVariableNames: instanceVariableToPushUp.

	pushUp := PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor.
	pushUp apply.

	self assert: (classToRefactorSuperClass definesInstanceVariableNamed: instanceVariableToPushUp).
	classToRefactorSuperClass allSubclassesDo: [ :subClass |
		self deny: (subClass definesInstanceVariableNamed: instanceVariableToPushUp)
	].! !

!PushUpInstanceVariableTest methodsFor: 'setup' stamp: 'MSC 22/Dec/2019, 2:12:16 am (UTC)'!
setUp

	super setUp.
	
	instanceVariableToPushUp := 'a'.! !

!PushUpInstanceVariableTest methodsFor: 'assertions' stamp: 'jmv 20/Mar/2024, 8:00:22 pm (UTC)'!
assertPushUpCreationFailsWith: aStringOrBlock

	self
		assertCreation: [ PushUpInstanceVariable named: instanceVariableToPushUp from: classToRefactor. ]
		failsWith: aStringOrBlock.! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MSC 12/Nov/2019, 3:47:47 pm (UTC)'!
test01AnEmptyMethodOnClassToRefactorShouldBePushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector push |

	selector := #newMethod.
   
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector asString.
           
	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
	
	! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MEGL 21/Aug/2023, 8:25:50 pm (UTC)'!
test02AMethodReadingInstVarWarnsWhenPushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector newVariable refactoring |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor compile: selector, '^ 1 + ', newVariable.
	
	self
		assertCreation: [ refactoring := PushUpMethod for: classToRefactor >> selector ]
		 warnsWith: [ PushUpMethod accessingInstanceVariableWarningMessage: { newVariable }].
		
	refactoring apply.
	
	self assert: (classToRefactorSuperclass definesInstanceVariableNamed: newVariable).
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor definesInstanceVariableNamed: newVariable).
	self deny: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MEGL 21/Aug/2023, 8:26:59 pm (UTC)'!
test03AMethodWritingInstVarWarnsWhenPushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector newVariable refactoring |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor compile: selector, ' a = 1'.

	self
		assertCreation: [ refactoring := PushUpMethod for: classToRefactor >> selector ]
		warnsWith: [ PushUpMethod accessingInstanceVariableWarningMessage: { newVariable }].
		
	refactoring apply.
	
	self assert: (classToRefactorSuperclass definesInstanceVariableNamed: newVariable).
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor definesInstanceVariableNamed: newVariable).
	self deny: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MSC 12/Nov/2019, 3:47:57 pm (UTC)'!
test04AMethodWritesSuperClassInstVarShouldBePushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector newVariable push |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' a = 1'.	

	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MSC 12/Nov/2019, 3:48:00 pm (UTC)'!
test05AMethodAccessesSuperClassClassToRefactorInstVarShouldBePushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector newVariable push |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, '^ 1 + ', newVariable.

	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
	   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MSC 12/Nov/2019, 3:48:03 pm (UTC)'!
test06AMethodAccessesSuperClassClassToRefactorInstVarShouldBePushedUpToSuperclass

	| classToRefactorSuperclass classToRefactor selector newVariable push |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, '^ 1 + ', newVariable.

	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
	   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:00:48 pm (UTC)'!
test07ShouldWarnWhenNotEquivalentMethodExistsInSuperclass
	
	| classToRefactor classToRefactorSuperclass newVariable selector superclassMethodContent |
	
	newVariable := 'a'.	
	selector := #newMethod.
	superclassMethodContent := selector, '
	^ 2 + ', newVariable, '.'.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.
	classToRefactorSuperclass compile: selector, '^ 1 + ', newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: superclassMethodContent.

	self 
		assertCreation: [ PushUpMethod for: classToRefactor >> selector ]
		warnsWith: [ PushUpMethod methodWithDifferentImplementationInSuperclass: { selector } ].
		
	! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:33:57 pm (UTC)'!
test08RemovesEquivalentMethodsFromSiblings

	| classToRefactorSuperclass classToRefactor selector push sourceCode classToRefactorSibling |

	selector := #newMethod.
   	sourceCode := selector, ' ^10 factorial'.
	
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: sourceCode.
 	classToRefactorSibling := self createClassNamed: (self classToRefactorName,'Sibling') asSymbol subclassOf: classToRefactorSuperclass.
	classToRefactorSibling compile: sourceCode.
           
	push := PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: { classToRefactor >> selector  -> {classToRefactorSibling} } asDictionary pushingUp: Set new.
	push apply.
   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
	self deny: (classToRefactorSibling includesSelector: selector).
	
	! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:34:55 pm (UTC)'!
test09CannotRemoveNotImplementedMethodFromAdditionalSubclasses

	| classToRefactorSuperclass classToRefactor selector sourceCode classToRefactorSibling |

	selector := #newMethod.
   	sourceCode := selector, ' ^10 factorial'.
	
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: sourceCode.
 	classToRefactorSibling := self createClassNamed: (self classToRefactorName,'Sibling') asSymbol subclassOf: classToRefactorSuperclass.
	   
	self 
		assertCreation: [  PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: { classToRefactor >> selector -> {classToRefactorSibling} } asDictionary pushingUp: Set new ]
		failsWith: [ PushUpMethod errorMessageForMethodCannotBeRemovedMethodFrom: { classToRefactorSibling } ].

	
	
	! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 13/Mar/2025, 6:04:11 pm (UTC)'!
test10CannotRemoveMethodFromNoSiblings

	| classToRefactorSuperclass classToRefactor selector sourceCode classToRefactorSibling |

	selector := #newMethod.
   	sourceCode := selector, ' ^10 factorial'.
	
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: sourceCode.
 	classToRefactorSibling := self createClassNamed: (self classToRefactorName,'NoSibling') asSymbol subclassOf: self defaultSuperclass.
	classToRefactorSibling compile: sourceCode.
    
	self 
		assertCreation: [  PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: { classToRefactor >> selector -> { classToRefactorSibling } } asDictionary pushingUp: Set new ]
		failsWith: [ PushUpMethod errorMessageForNoSiblings: { classToRefactorSibling } ].

	
	
	! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:35:07 pm (UTC)'!
test11CannotRemoveNotEquivalantMethodsFromSiblings

	| classToRefactorSuperclass classToRefactor selector classToRefactorSibling |

	selector := #newMethod.
   	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' ^10 factorial'.
 	classToRefactorSibling := self createClassNamed: (self classToRefactorName,'NoSibling') asSymbol subclassOf: classToRefactorSuperclass.
	classToRefactorSibling compile: selector, ' ^11 factorial'.
    
	self 
		assertCreation: [  PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: { classToRefactor >> selector  -> {classToRefactorSibling} } asDictionary pushingUp: Set new ]
		failsWith: [ PushUpMethod errorMessageForNotEquivalentMethods: { classToRefactorSibling } ].

	
	
	! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 26/May/2023, 11:50:19 pm (UTC)'!
test12ShouldNotWarnWhenEquivalentMethodExistsInSuperclass
	
	| classToRefactor classToRefactorSuperclass selector sourceCode |
	
	selector := #newMethod.
	sourceCode := selector, ' ^10 factorial'.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: sourceCode.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: sourceCode.

	self 
		shouldnt: [ PushUpMethod for: classToRefactor >> selector ]
		raise: self refactoringWarning 
	! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MEGL 12/Jul/2023, 8:38:15 pm (UTC)'!
test13FailsWhenAccessingInstVarDefinedAsTemporaryInSuperclass

	| classToRefactorSuperclass classToRefactor selector superclassSelector newVariable |

	newVariable := 'a'.	
	superclassSelector := #superClassNewMethod.
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: superclassSelector, '|', newVariable, '|', '^ 1 + ', newVariable.
	
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor compile: selector, '^ 1 + ', newVariable.
	
	self
		assertCreation: [ PushUpMethod for: classToRefactor >> selector ]
		failsWith: [ PushUpInstanceVariable errorMessageForInstanceVariable: newVariable isDefinedInMethodsOf: classToRefactorSuperclass ].! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 12/Jul/2023, 11:27:47 pm (UTC)'!
test14ShouldNotWarnWhenMethodExistsInSuperclassIsSubclassResponsibility
	
	| classToRefactor classToRefactorSuperclass selector superclassMethodContent |
	
	selector := #newMethod.
	superclassMethodContent := selector, ' self ', #subclassResponsibility.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: superclassMethodContent.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' ^1'.

	self 
		shouldnt: [ PushUpMethod for: classToRefactor >> selector ]
		raise: self refactoringWarning 
	
	! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:35:23 pm (UTC)'!
test15AMethodThatDependsOnOtherMethodPushesUpMethodsToSuperclass

	| classToRefactorSuperclass classToRefactor selector dependantSelector refactoring |

	selector := #newMethod.
	dependantSelector := #dependantMethod.

	classToRefactorSuperclass := self createClassNamed: #Superclass.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' ^ self ', dependantSelector.
	classToRefactor compile: dependantSelector, ' ^ 1'.

	refactoring := PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: Dictionary new pushingUp: { classToRefactor >> dependantSelector } asSet.
	refactoring apply.

	self assert: (classToRefactorSuperclass includesSelector: selector).
	self assert: (classToRefactorSuperclass includesSelector: dependantSelector).
	self deny: (classToRefactor includesSelector: selector).
	self deny: (classToRefactor includesSelector: dependantSelector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:35:32 pm (UTC)'!
test16AMethodThatDependsOnACycleOfMethodsPushesUpMethodsToSuperclass

	| classToRefactorSuperclass classToRefactor selector dependantSelector refactoring otherDependantSelector |

	selector := #newMethod.
	dependantSelector := #dependantMethod.
	otherDependantSelector := #otherDependantSelector.

	classToRefactorSuperclass := self createClassNamed: #Superclass.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' ^ self ', dependantSelector.
	classToRefactor compile: dependantSelector, ' ^ self ', otherDependantSelector.
	classToRefactor compile: otherDependantSelector, ' ^ self ', selector.

	refactoring := PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: Dictionary new pushingUp: { classToRefactor >> dependantSelector . classToRefactor >> otherDependantSelector } asSet.
	refactoring apply.

	self assert: (classToRefactorSuperclass includesSelector: selector).
	self assert: (classToRefactorSuperclass includesSelector: dependantSelector).
	self assert: (classToRefactorSuperclass includesSelector: otherDependantSelector).
	self deny: (classToRefactor includesSelector: selector).
	self deny: (classToRefactor includesSelector: dependantSelector).
	self deny: (classToRefactor includesSelector: otherDependantSelector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:35:40 pm (UTC)'!
test17FailsWhenPushingADependantMethodAccessingInstVarDefinedAsTemporaryInSuperclass

	| classToRefactorSuperclass classToRefactor selector superclassSelector dependantSelector newVariable |

	newVariable := 'a'.	
	superclassSelector := #superClassNewMethod.
	selector := #newMethod.
	dependantSelector := #dependantMethod.

	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: superclassSelector, '|', newVariable, '|', '^ 1 + ', newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor compile: selector, ' ^ self ', dependantSelector.
	classToRefactor compile: dependantSelector, ' ^ 1 + ', newVariable.

	self
		assertCreation: [ PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: Dictionary new pushingUp: { classToRefactor >> dependantSelector } asSet ]
		failsWith: [ PushUpInstanceVariable errorMessageForInstanceVariable: newVariable isDefinedInMethodsOf: classToRefactorSuperclass ].! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:35:48 pm (UTC)'!
test18ShouldWarnWhenNotEquivalentMethodOfDependantMethodExistsInSuperclass

	| classToRefactor classToRefactorSuperclass newVariable selector dependantSelector superclassMethodContent |

	newVariable := 'a'.	
	selector := #newMethod.
	dependantSelector := #dependantMethod.
	superclassMethodContent := dependantSelector, '
	^ 2 + ', newVariable, '.'.

	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.
	classToRefactorSuperclass compile: dependantSelector, '^ 1 + ', newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' ^ self ', dependantSelector.
	classToRefactor compile: superclassMethodContent.

	self 
		assertCreation: [ PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: Dictionary new pushingUp: { classToRefactor >> dependantSelector } asSet ]
		warnsWith: [ PushUpMethod methodWithDifferentImplementationInSuperclass: { dependantSelector } ].! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:19:55 pm (UTC)'!
test19ShouldSelectSiblingClassesWithEquivalentMethod

	| superclass classToTest classWithEquivalentMethod selector methodContent equivalentMethod|

	selector := #newMethod.
	methodContent :=  selector, '^ 2'.

	superclass := self createClassNamed: #Superclass.

	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: methodContent.
	
	classWithEquivalentMethod := self createClassNamed: #ClassWithEquivalentMethod subclassOf: superclass.
	classWithEquivalentMethod compile: methodContent.
	
	equivalentMethod := PushUpMethod equivalentMethodsOfSiblingsOfAll: { classToTest >> selector }.
	   
	self assert: (equivalentMethod size = 1).
	self assert: (equivalentMethod includesKey: (classToTest >> selector)).
	self assert: ((equivalentMethod at: classToTest >> selector) = { classWithEquivalentMethod }).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:19:55 pm (UTC)'!
test20ShouldNotSelectSiblingClassesWithNoEquivalentMethod

	| superclass classToTest classWithEquivalentMethod selector  equivalentMethod|

	selector := #newMethod.

	superclass := self createClassNamed: #Superclass.

	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: selector, '^ 2'.
	
	classWithEquivalentMethod := self createClassNamed: #ClassWithEquivalentMethod subclassOf: superclass.
	classWithEquivalentMethod compile: selector, '^ 3'.
	
	equivalentMethod := PushUpMethod equivalentMethodsOfSiblingsOfAll: { classToTest >> selector }.
	   
	self assert: (equivalentMethod isEmpty).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'HAW 23/Jan/2024, 7:19:55 pm (UTC)'!
test21WhenMultipleMethodsShouldSelectSiblingClassesWithEquivalentMethodForEachOne

	| superclass classToTest classWithEquivalentMethod selector anotherSelector methodContent anotherMethodContent equivalentMethod|

	selector := #newMethod.
	anotherSelector := #newMethod2.
	methodContent :=  selector, '^ 2'.
	anotherMethodContent := anotherSelector, '^ 1'.

	superclass := self createClassNamed: #Superclass.

	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: methodContent.
	classToTest compile: anotherMethodContent.
	
	classWithEquivalentMethod := self createClassNamed: #ClassWithEquivalentMethod subclassOf: superclass.
	classWithEquivalentMethod compile: methodContent.
	classWithEquivalentMethod compile: anotherMethodContent.
	
	equivalentMethod := PushUpMethod equivalentMethodsOfSiblingsOfAll: { classToTest >> selector . classToTest >> anotherSelector }.
	   
	self assert: (equivalentMethod size = 2).
	self assert: (equivalentMethod includesKey: (classToTest >> selector)).
	self assert: (equivalentMethod includesKey: (classToTest >> anotherSelector)).
	self assert: ((equivalentMethod at: classToTest >> selector) = { classWithEquivalentMethod }).
	self assert: ((equivalentMethod at: classToTest >> anotherSelector) = { classWithEquivalentMethod }).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MEGL 21/Aug/2023, 12:55:09 am (UTC)'!
test22ShouldSelectDependantSelectors

	| superclass classToTest selector dependantSelector methodContent dependantMethodContent dependantSelectors|

	selector := #newMethod.
	dependantSelector := #dependantMethod.
	methodContent :=  selector, '^ self ', dependantSelector.
	dependantMethodContent :=  dependantSelector, '^ 2'.

	superclass := self createClassNamed: #Superclass.
	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: methodContent.
	classToTest compile: dependantMethodContent.
	
	dependantSelectors := PushUpMethod selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: ( classToTest >> selector ).
	   
	self assert: (dependantSelectors size = 1).
	self assert: (dependantSelectors includes: dependantSelector).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MEGL 21/Aug/2023, 12:58:47 am (UTC)'!
test23ShouldNotSelectDependantSelectorsWhenThereIsNone

	| superclass classToTest selector methodContent dependantSelectors|

	selector := #newMethod.
	methodContent :=  selector, '^ 2'.

	superclass := self createClassNamed: #Superclass.
	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: methodContent.
	
	dependantSelectors := PushUpMethod selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: ( classToTest >> selector ).
	   
	self assert: (dependantSelectors isEmpty).! !

!PushUpMethodTest methodsFor: 'tests' stamp: 'MEGL 21/Aug/2023, 12:57:41 am (UTC)'!
test24ShouldSelectCyclicDependantSelectors

	| superclass classToTest selector dependantSelector otherDependantSelector methodContent dependantMethodContent otherDependantMethodContent dependantSelectors|

	selector := #newMethod.
	dependantSelector := #dependantMethod.
	otherDependantSelector := #otherDependantSelector.
	methodContent :=  selector, '^ self ', dependantSelector.
	dependantMethodContent :=  dependantSelector, '^ self ', otherDependantSelector.
	otherDependantMethodContent := otherDependantSelector, '^ self ', selector.

	superclass := self createClassNamed: #Superclass.
	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: methodContent.
	classToTest compile: dependantMethodContent.
	classToTest compile: otherDependantMethodContent.
	
	dependantSelectors := PushUpMethod selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: ( classToTest >> selector ).
	   
	self assert: (dependantSelectors size = 2).
	self assert: (dependantSelectors includes: dependantSelector).
	self assert: (dependantSelectors includes: otherDependantSelector).! !

!PushUpMethodTest methodsFor: 'class factory' stamp: 'MSC 12/Nov/2019, 3:48:09 pm (UTC)'!
classToRefactorName	

	^#ClassToPushMethodUp! !

!RemoveAllUnreferencedInstanceVariablesTest methodsFor: 'tests' stamp: 'HAW 2/Aug/2018, 8:15:11 pm (UTC)'!
test01NoVariableIsRemovedWhenClassHasNoInstanceVariable

	| classToRefactor refactoring removedInstanceVariables |
	
	classToRefactor := self createClassNamed: #ClassWithoutInstVar.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: removedInstanceVariables isEmpty! !

!RemoveAllUnreferencedInstanceVariablesTest methodsFor: 'tests' stamp: 'HAW 2/Aug/2018, 8:15:14 pm (UTC)'!
test02UnreferencedVariablesAreRemoved

	| classToRefactor refactoring removedInstanceVariables unreferencedVariable |
	
	unreferencedVariable := 'a'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: unreferencedVariable.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: 1 equals: removedInstanceVariables size.
	self assert: (removedInstanceVariables includes: unreferencedVariable)
	! !

!RemoveAllUnreferencedInstanceVariablesTest methodsFor: 'tests' stamp: 'HAW 2/Aug/2018, 8:15:18 pm (UTC)'!
test03VariablesWithReferencesAreNotRemoved

	| classToRefactor refactoring removedInstanceVariables unreferencedVariableName referencedVariableName |
	
	referencedVariableName := 'a'.
	unreferencedVariableName := 'b'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: referencedVariableName, ' ', unreferencedVariableName.
	classToRefactor compile: 'm1 ^', referencedVariableName.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: 1 equals: removedInstanceVariables size.
	self assert: (removedInstanceVariables includes: unreferencedVariableName).
	
	
! !

!RemoveAllUnreferencedInstanceVariablesTest methodsFor: 'tests' stamp: 'HAW 2/Aug/2018, 8:22:44 pm (UTC)'!
test04VariablesWithReferencesInSubclassesAreNotRemoved

	| classToRefactor refactoring removedInstanceVariables unreferencedVariableName referencedVariableName classToRefactorSubclass |
	
	referencedVariableName := 'a'.
	unreferencedVariableName := 'b'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: referencedVariableName, ' ', unreferencedVariableName.
	classToRefactorSubclass := self createClassNamed: #ClassWithReferenceInstVar subclassOf: classToRefactor.
	classToRefactorSubclass compile: 'm1 ^', referencedVariableName.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: 1 equals: removedInstanceVariables size.
	self assert: (removedInstanceVariables includes: unreferencedVariableName).
	
! !

!RemoveInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 12/Jun/2017, 11:10:08 pm (UTC)'!
test01CanNotRemoveAnInstanceVariableNotDefinedInClass

	| variableToRemove classToRefactor |
	
	variableToRemove := 'a'.
	classToRefactor := self createClassNamed: #ClassWithoutInstVar.
	
	self 
		assertCreation: [ RemoveInstanceVariable named: variableToRemove from: classToRefactor ]
		failsWith: [ RemoveInstanceVariable errorMessageForInstanceVariable: variableToRemove notDefinedIn: classToRefactor ].
		
	
	! !

!RemoveInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 2/Aug/2018, 7:24:17 pm (UTC)'!
test02CanNotRemoveInstanceVariableWithReferences

	| variableToRemove classToRefactor selector |
	
	variableToRemove := 'a'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: variableToRemove.
	selector := #m1.
	classToRefactor compile: selector,' ^', variableToRemove.
	
	self 
		assertCreation: [ RemoveInstanceVariable named: variableToRemove from: classToRefactor ]
		failsWith: [ RemoveInstanceVariable errorMessageForInstanceVariable: variableToRemove isReferencedInAll: (Array with: classToRefactor>>selector) ].
		
	
	! !

!RemoveInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 2/Aug/2018, 7:24:28 pm (UTC)'!
test03WhenAppliedRemovesInstanceVariable

	| variableToRemove classToRefactor remove |
	
	variableToRemove := 'a'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: variableToRemove.
	
	remove := RemoveInstanceVariable named: variableToRemove from: classToRefactor.
	remove apply.
	
	self deny: (classToRefactor definesInstanceVariableNamed: variableToRemove)	
	
	! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 2/Jan/2020, 10:58:49 am (UTC)'!
test01CanNotRemoveParameterFromUnaryMessages

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 1
			named: 'aParameter' 
			from: #m1
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter canNotRemoveParameterFromUnaryMessagesErrorMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 2/Jan/2020, 10:59:13 am (UTC)'!
test02CanNotRemoveParameterFromBinaryMessages

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 1
			named: '+' asSymbol 
			from: #+ 
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter canNotRemoveParameterFromBinaryMessagesErrorMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 2/Sep/2018, 11:46:52 pm (UTC)'!
test03CanNotRemoveParameterNotInMessage

	| classToRefactor selectorToRemoveParameterFrom parameterNotInMessage |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter'.
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			named: parameterNotInMessage 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: {classToRefactor >> selectorToRemoveParameterFrom } 
			senders: {} ]
		failsWith: [ RemoveParameter errorMessageForParameterNotInMessage: parameterNotInMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 2/Sep/2018, 11:46:03 pm (UTC)'!
test04AllImplementorsMustImplementSelector

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove implementors |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	parameterToRemove := 'parameter'.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter'.
	implementors := {thisContext method}.
	
	self 
		assertCreation: [ RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: implementors 
			senders: {} ]
		failsWith: [ RemoveParameter errorMessageForInvalidImplementors: implementors ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 2/Sep/2018, 11:49:28 pm (UTC)'!
test05AllSendersShouldSendSelector

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove invalidSenders |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	parameterToRemove := 'parameter'.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter'.
	invalidSenders := {thisContext method}.
	
	self 
		assertCreation: [ RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: {} 
			senders: invalidSenders ]
		failsWith: [ RemoveParameter errorMessageForInvalidSenders: invalidSenders of: selectorToRemoveParameterFrom ]
	
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 2/Sep/2018, 11:57:35 pm (UTC)'!
test06NoImplementorReferencesParameterToRemove

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove implementorsReferencingParameter |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol.
	parameterToRemove := 'parameter'.
	classToRefactor compile: selectorToRemoveParameterFrom asString, ' parameter ^parameter'.
	implementorsReferencingParameter := {classToRefactor>>selectorToRemoveParameterFrom }.
	
	self 
		assertCreation: [ RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: implementorsReferencingParameter 
			senders: {} ]
		failsWith: [ RemoveParameter errorMessageForParameterToRemoveIsReferenced: parameterToRemove ]! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 3/Sep/2018, 9:58:11 pm (UTC)'!
test07FirstParameterIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToRemove, keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	classToRefactor compile: keywordToRemove,parameterToRemove, ' 
	', keywordToKeep, parameterToKeep.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: keywordToKeep asSymbol).
	self assert: (classToRefactor sourceCodeAt: keywordToKeep asSymbol) equals: (keywordToKeep, parameterToKeep) ! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 10/Sep/2026, 10:19:18 pm (UTC)'!
test08LastParameterIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToKeep := 'm1:'.
	keywordToRemove := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToKeep, keywordToRemove) asSymbol. 
	parameterToKeep := 'parameter1'.
	parameterToRemove := 'parameter2'.
	classToRefactor compile: keywordToKeep,parameterToKeep, ' ', keywordToRemove, parameterToRemove,'
	| t1 |'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: keywordToKeep asSymbol).
	self assert: (classToRefactor sourceCodeAt: keywordToKeep asSymbol) equals: (keywordToKeep, parameterToKeep,'
	| t1 |') ! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 4/Sep/2018, 6:34:22 pm (UTC)'!
test09MiddleParameterIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring newSelector |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := ('m0:',keywordToRemove, keywordToKeep) asSymbol. 
	newSelector := ('m0:', keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	classToRefactor compile: 'm0: parameter0 
	',keywordToRemove,parameterToRemove, ' 
	', keywordToKeep, parameterToKeep.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: newSelector).
	self assert: (classToRefactor sourceCodeAt: newSelector) equals: ('m0: parameter0 
	',keywordToKeep, parameterToKeep) ! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 3/Sep/2018, 9:34:00 pm (UTC)'!
test10ParameterFromOneKeywordSelectorIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove refactoring |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol. 
	parameterToRemove := 'parameter1'.
	classToRefactor compile: selectorToRemoveParameterFrom asString,parameterToRemove.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {}.
	refactoring apply.
	
	self assert: (classToRefactor canUnderstand: 'm1' asSymbol).! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:46:15 pm (UTC)'!
test11ParameterOfSendersOfMoreThanOneKeywordSelectorsIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToRemove, keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: keywordToRemove,parameterToRemove, ' ', keywordToKeep, parameterToKeep.
	classToRefactor compile: senderSelector asString,'
		self 
			', keywordToRemove, ' 1 
			', keywordToKeep, ' 2 '.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: keywordToKeep asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,'
		self 
			', keywordToKeep, ' 2 ')
	
	! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:46:19 pm (UTC)'!
test12ParameterOfSendersOfOneKeywordSelectorsIsRemovedCorrectly

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	selectorToRemoveParameterFrom := 'm1:' asSymbol. 
	parameterToRemove := 'parameter1'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: selectorToRemoveParameterFrom,parameterToRemove.
	classToRefactor compile: senderSelector asString,'
		self 
			', selectorToRemoveParameterFrom, '
			 (1+2).'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: 'm1' asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,'
		self 
			m1.')
	
	! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:46:23 pm (UTC)'!
test13ParameterIsRemovedCorrecltyFromSendersWithMoreThanOneSend

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToRemove := 'm1:'.
	keywordToKeep := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToRemove, keywordToKeep) asSymbol. 
	parameterToRemove := 'parameter1'.
	parameterToKeep := 'parameter2'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: keywordToRemove,parameterToRemove, ' ', keywordToKeep, parameterToKeep.
	classToRefactor compile: senderSelector asString,'
		self ', keywordToRemove, ' (1+1) ', keywordToKeep, ' 2.
		self ', keywordToRemove, ' 3 ', keywordToKeep, ' 4.'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: keywordToKeep asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,'
		self ', keywordToKeep, ' 2.
		self ', keywordToKeep, ' 4.')
	
	! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 2/Jan/2020, 3:33:19 am (UTC)'!
test14CanNotRemoveParameterIndexLessThanOne

	| parameterNotInMessage |
	
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 0
			named: parameterNotInMessage 
			from: #m1:  
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter invalidParameterIndexErrorMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 2/Jan/2020, 3:36:16 am (UTC)'!
test15CanNotRemoveParameterIndexBiggerThanOldSelectorNumberOfKeywords

	| parameterNotInMessage |
	
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 2
			named: parameterNotInMessage 
			from: #m1:  
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter invalidParameterIndexErrorMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 2/Jan/2020, 3:38:57 am (UTC)'!
test16ParameterIndexMustBeInteger

	| parameterNotInMessage |
	
	parameterNotInMessage := 'otherParameter'.

	self 
		assertCreation: [ RemoveParameter 
			atIndex: 1.5
			named: parameterNotInMessage 
			from: #m1:m2: 
			implementors: {} 
			senders: {} ]
		failsWith: [ RemoveParameter invalidParameterIndexErrorMessage ]
! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 10/Sep/2026, 10:19:24 pm (UTC)'!
test17LastKeywordOnItsOwnLineIsRemovedWithItsLine

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToKeep := 'm1:'.
	keywordToRemove := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToKeep, keywordToRemove) asSymbol. 
	parameterToKeep := 'parameter1'.
	parameterToRemove := 'parameter2'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: keywordToKeep,parameterToKeep, ' ', keywordToRemove, parameterToRemove.
	classToRefactor compile: senderSelector asString,'
		self 
			', keywordToKeep, ' 1 
			', keywordToRemove, ' 2'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: keywordToKeep asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,'
		self 
			', keywordToKeep, ' 1')! !

!RemoveParameterTest methodsFor: 'tests' stamp: 'HAW 10/Sep/2026, 10:19:29 pm (UTC)'!
test18LastKeywordBeforeAClosingParenthesisIsRemovedWithTheSpaceBeforeIt

	| classToRefactor selectorToRemoveParameterFrom parameterToRemove keywordToRemove keywordToKeep parameterToKeep refactoring senderSelector senderMethod |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	keywordToKeep := 'm1:'.
	keywordToRemove := 'm2:'.
	selectorToRemoveParameterFrom := (keywordToKeep, keywordToRemove) asSymbol. 
	parameterToKeep := 'parameter1'.
	parameterToRemove := 'parameter2'.
	senderSelector := 'sender' asSymbol.
	classToRefactor compile: keywordToKeep,parameterToKeep, ' ', keywordToRemove, parameterToRemove.
	classToRefactor compile: senderSelector asString,' ^(self ', keywordToKeep, ' 1 ', keywordToRemove, ' 2) yourself'.
	
	refactoring := RemoveParameter 
			named: parameterToRemove 
			from: classToRefactor>>selectorToRemoveParameterFrom  
			implementors: { classToRefactor>>selectorToRemoveParameterFrom } 
			senders: {classToRefactor>>senderSelector}.
	refactoring apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderSelector.
	self assert: (senderMethod sendsSelector: keywordToKeep asSymbol).
	self assert: senderMethod sourceCode equals: (senderSelector asString,' ^(self ', keywordToKeep, ' 1) yourself')! !

!RemoveParameterTest methodsFor: 'class factory' stamp: 'HAW 1/Sep/2018, 4:24:23 pm (UTC)'!
classToRefactorName
	
	^#ClassToRemoveParameter! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 1/Jun/2017, 11:44:41 pm (UTC)'!
test01NewClassNameHasToBeDifferentToOldOne

	self 
		assertCreation: [ RenameClass from: self class to: self class name ]
		failsWith: [ RenameClass newNameEqualsOldNameErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:36:20 pm (UTC)'!
test02NewClassNameHasToBeASymbol

	self 
		assertCreation: [ RenameClass from: self class to: 'aString' ]
		failsWith: [ NewClassPrecondition newNameMustBeSymbolErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 24/May/2019, 2:09:47 pm (UTC)'!
test03NewClassNameHasToStartWithUppercaseLetter

	self 
		assertCreation: [ RenameClass from: self class to: #_A ]
		failsWith: [ NewClassPrecondition newNameMustStartWithRightLetterErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 13/Dec/2018, 9:59:19 pm (UTC)'!
test04_01NewClassNameShouldNotBeAGlobalVariable

	| newClassName |
	
	newClassName := #Smalltalk.
	
	self 
		assertCreation: [ RenameClass from: self class to: newClassName in: Smalltalk ]
		failsWith: [ NewClassPrecondition errorMessageForAlreadyExistGlobalNamed: newClassName ]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:33:08 pm (UTC)'!
test04NewClassShouldNotExist

	| newClassName |
	
	newClassName := #Object.
	
	self 
		assertCreation: [ RenameClass from: self class to: newClassName in: Smalltalk ]
		failsWith: [ NewClassPrecondition errorMessageForAlreadyExistClassNamed: newClassName ]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:33:35 pm (UTC)'!
test05NewClassShouldNotBeUndeclare

	| newClassName undeclared |
	
	newClassName := #UndeclareClass.
	undeclared := Dictionary new.
	undeclared at: newClassName put: nil.
	
	self 
		assertCreation: [ RenameClass from: self class to: newClassName in: Smalltalk undeclared: undeclared ]
		failsWith: [ NewClassPrecondition errorMessageForNewClassIsUndeclared: newClassName ]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 4/Jun/2017, 11:12:10 pm (UTC)'!
test06OldClassIsRenamedToNewClass

	| oldClassName newClassName classToRefactor rename |

	oldClassName :=  'OldClassTest07' asSymbol.
	newClassName := 'NewClassTest07' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	self assert: (Smalltalk classNamed: oldClassName) isNil.
	self deny: (Smalltalk classNamed: newClassName) isNil.! !

!RenameClassTest methodsFor: 'tests' stamp: 'jmv 7/Jul/2025, 9:21:18 pm (UTC)'!
test07OldClassDirectReferencesAreRenamed

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest07' asSymbol.
	newClassName := 'NewClassTest07' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ', oldClassName asString, ' new'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteral: newClassName).
	self deny: (referencingMethod hasLiteral: oldClassName)! !

!RenameClassTest methodsFor: 'tests' stamp: 'jmv 7/Jul/2025, 9:21:23 pm (UTC)'!
test08OldClassLiteralReferencesAreRenamed

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest08' asSymbol.
	newClassName := 'NewClassTest8' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' #', oldClassName asString, ' size'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteral: newClassName).
	self deny: (referencingMethod hasLiteral: oldClassName)! !

!RenameClassTest methodsFor: 'tests' stamp: 'jmv 7/Jul/2025, 9:21:28 pm (UTC)'!
test09OtherClassDirectReferencesAreNotRenamed

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest09' asSymbol.
	newClassName := 'NewClassTest09' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ', oldClassName asString, ' new. Object new'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteral: newClassName).
	self deny: (referencingMethod hasLiteral: oldClassName).
	self assert: (referencingMethod hasLiteral: #Object).
! !

!RenameClassTest methodsFor: 'tests' stamp: 'jmv 7/Jul/2025, 9:21:33 pm (UTC)'!
test10OtherLiteralReferencesAreNotRenamed

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest10' asSymbol.
	newClassName := 'NewClassTest10' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' #', oldClassName asString, ' size. #Object size'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteral: newClassName).
	self deny: (referencingMethod hasLiteral: oldClassName).
	self assert: (referencingMethod hasLiteral: #Object).
! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 9/Aug/2018, 8:30:56 pm (UTC)'!
test11OldClassDirectAndLiteralReferencesAreRenameAtOnce

	| oldClassName newClassName classToRefactor selector rename renamedReferences |
	
	oldClassName := 'OldClassTest11' asSymbol.
	newClassName := 'NewClassTest11' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ', oldClassName asString, ' new. #', oldClassName asString, ' size'.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	renamedReferences := rename apply.
	
	self assert: 1 equals: renamedReferences size.
	self assert: selector equals: renamedReferences anyOne selector.
	self assert: newClassName equals: renamedReferences anyOne classSymbol.
	self assert: ((classToRefactor sourceCodeAt: selector) includesSubString: '#', newClassName)! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 4/Apr/2018, 11:56:09 pm (UTC)'!
test12CanNotRenameAMetaclass

	self 
		assertCreation: [ RenameClass from: self class class to: #Object ]
		failsWith: [ RenameClass classToRenameCanNotBeMetaclassErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 13/Aug/2018, 10:35:38 pm (UTC)'!
test13NewClassNameHasToBeASymbol

	self 
		assertCreation: [ RenameClass from: self class to: '' asSymbol ]
		failsWith: [ NewClassPrecondition newClassNameCanNotBeEmptyErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 1:20:07 pm (UTC)'!
test14NewClassNameCanNotHaveSpaces

	| oldClassName classToRefactor |

	oldClassName :=  'OldClassTest14' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.

	self 
		assertCreation: [ RenameClass from: classToRefactor to: 'With spaces' asSymbol ]
		failsWith: [ NewClassPrecondition newClassNameCanNotHaveSeparatorsErrorMessage]! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 9/Aug/2018, 6:44:13 pm (UTC)'!
test15ItDoesNotRenameReferencesToClassVariableWithSameName

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences |

	oldClassName :=  'OldClassTest15' asSymbol.
	newClassName := 'NewClassTest15' asSymbol.
	"First I create a class var with the same name as the class to rename and a reference to it - Hernan"
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldClassName.
	classWithClassVariable compile: 'm1 ^', oldClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	
	
	! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 9/Aug/2018, 6:45:20 pm (UTC)'!
test16ItDoesNotRenameReferencesToClassVariableWithSameNameDefinedInAnySuperclass

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences superClassOfClassWithClassVarReference |

	oldClassName :=  'OldClassTest16' asSymbol.
	newClassName := 'NewClassTest16' asSymbol.

	superClassOfClassWithClassVarReference := self createClassNamed: 'ClassReferencingClassVarSuperclass' asSymbol.
	superClassOfClassWithClassVarReference addClassVarName: oldClassName.
	
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol subclassOf: superClassOfClassWithClassVarReference.
	classWithClassVariable compile: 'm1 ^', oldClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	
	
	! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 9/Aug/2018, 6:46:34 pm (UTC)'!
test17ItDoesNotRenameReferencesToClassVariableWithSameNameAsNewClassDefinedInAnySuperclass

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences superClassOfClassWithClassVarReference |

	oldClassName :=  'OldClassTest17' asSymbol.
	newClassName := 'NewClassTest17' asSymbol.

	superClassOfClassWithClassVarReference := self createClassNamed: 'ClassReferencingClassVarSuperclass' asSymbol.
	superClassOfClassWithClassVarReference addClassVarName: newClassName.
	
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol subclassOf: superClassOfClassWithClassVarReference.
	classWithClassVariable compile: 'm1 ^', newClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	
	
	! !

!RenameClassTest methodsFor: 'tests' stamp: 'jmv 7/Jul/2025, 9:21:46 pm (UTC)'!
test18WorksCorrectlyWhenSourceCodeEndsWithClassToRename

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	oldClassName := 'OldClassTest18' asSymbol.
	newClassName := 'NewClassTest18' asSymbol.
	classToRefactor := self createClassNamed: oldClassName.
	selector := #m1.
	classToRefactor compile: selector, ' ^', oldClassName asString.
	
	rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
	rename apply.
	
	referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
	self assert: (referencingMethod hasLiteral: newClassName).
	self deny: (referencingMethod hasLiteral: oldClassName)! !

!RenameClassTest methodsFor: 'tests' stamp: 'jmv 7/Jul/2025, 9:21:39 pm (UTC)'!
test19RenamesClassWhenMethodReferencesClassAndDoesNothing

	| oldClassName newClassName classToRefactor selector rename referencingMethod |
	
	"Currently the rename class is not handling this case because message #allCallsOn: does not return a method
	that references a class but does nothing because it is optimize to return self... it is a really wierd case so I decided
	no to solve it by now - Hernan"
	self shouldFail: [
		oldClassName := 'OldClassTest18' asSymbol.
		newClassName := 'NewClassTest18' asSymbol.
		classToRefactor := self createClassNamed: oldClassName.
		selector := #m1.
		classToRefactor compile: selector, ' ', oldClassName asString.
	
		rename := RenameClass from: classToRefactor to: newClassName in: Smalltalk undeclared: Undeclared.
		rename apply.
	
		referencingMethod := (Smalltalk classNamed: newClassName) compiledMethodAt: selector.
		self assert: (referencingMethod hasLiteral: newClassName).
		self deny: (referencingMethod hasLiteral: oldClassName)] ! !

!RenameClassTest methodsFor: 'tests' stamp: 'HAW 30/Dec/2021, 8:31:36 pm (UTC)'!
test20ItDoesNotRenameReferencesToClassVariableWithSameNameAndReferencedInClassMethod

	| classWithClassVariable oldClassName classToRefactor newClassName rename renamedReferences |

	oldClassName :=  'OldClassTest15' asSymbol.
	newClassName := 'NewClassTest15' asSymbol.
	"First I create a class var with the same name as the class to rename and a reference to it - Hernan"
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldClassName.
	classWithClassVariable class compile: 'm1 ^', oldClassName.

	classToRefactor := self createClassNamed: oldClassName.

	rename := RenameClass from: classToRefactor to: newClassName.
	renamedReferences := rename apply.
	
	self assert: renamedReferences isEmpty.
	self assert: (classWithClassVariable definesClassVariableNamedInHierarchy: oldClassName)
	
	
	! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 9:53:53 pm (UTC)'!
define: anOldName with: aGlobalValue toBeRenamedAs: aNewName while: aBlock

	| oldNameAsSymbol newNameAsSymbol |
	
	oldNameAsSymbol := anOldName asSymbol.
	newNameAsSymbol := aNewName asSymbol.
	
	self assert: (Smalltalk at: oldNameAsSymbol ifAbsent: [ nil ]) isNil.
	self assert: (Smalltalk at: newNameAsSymbol ifAbsent: [ nil ]) isNil.
	
	[ 
		Smalltalk at: oldNameAsSymbol put: aGlobalValue.
		aBlock value: oldNameAsSymbol value: newNameAsSymbol value: aGlobalValue ] ensure: [
			Smalltalk removeKey: oldNameAsSymbol ifAbsent: [].
			Smalltalk removeKey: newNameAsSymbol ifAbsent: []].
		! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:20:03 pm (UTC)'!
test00GlobalCanNotBeClass

	self 
		assertCreation: [ RenameGlobal from: #Object to: 'NewObject__' asSymbol ]
		failsWith: [ RenameGlobal globalCanNotBeClassErrorMessage]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:20:37 pm (UTC)'!
test01NewNameHasToBeDifferentToOldOne

	self 
		define: 'OldNameToRename__' 
		with: Object new 
		toBeRenamedAs: 'OldNameToRename__' 
		while: [ :oldName :newName :globalValue |
			self 
				assertCreation: [ RenameGlobal from: oldName to: newName ]
				failsWith: [ RenameGlobal newNameEqualsOldNameErrorMessage]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:26:40 pm (UTC)'!
test02NewNameHasToBeASymbol

	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: newName asString ]
			failsWith: [ NewGlobalPrecondition newNameMustBeSymbolErrorMessage]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:27:23 pm (UTC)'!
test03NewNameShouldNotBeAClass

	| className |
	
	className := #Object.
	
	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: className in: Smalltalk ]
			failsWith: [ NewGlobalPrecondition errorMessageForAlreadyExistClassNamed: className ]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:27:33 pm (UTC)'!
test04NewNameShouldNotBeAGlobalVariable

	| globalName |
	
	globalName := #Smalltalk.
	
	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: globalName in: Smalltalk ]
			failsWith: [ NewGlobalPrecondition errorMessageForAlreadyExistGlobalNamed: globalName ]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:27:45 pm (UTC)'!
test06OldNameIsRenamedToNewName

	| rename |

	self withDefaultsDo: [ :oldName :newName :globalValue |
		rename := RenameGlobal from: oldName to: newName in: Smalltalk.
		rename apply.

		self assert: (Smalltalk at: oldName ifAbsent: [nil]) isNil.
		self assert: (Smalltalk at: newName) equals: globalValue ].! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'jmv 7/Jul/2025, 9:21:50 pm (UTC)'!
test07OldDirectReferencesAreRenamed

	| rename classToRefactor referencingMethod selector |

	self withDefaultsDo: [ :oldName :newName :globalValue |
		classToRefactor := self createClassNamed: #AClassToRefactor.
		selector := 'm1' asSymbol.
		classToRefactor compile: selector, ' ^', oldName asString.

		rename := RenameGlobal from: oldName to: newName in: Smalltalk.
		rename apply.

		referencingMethod := classToRefactor compiledMethodAt: selector.
		self assert: (referencingMethod hasLiteral: newName).
		self deny: (referencingMethod hasLiteral: oldName).
		self assert: (classToRefactor new perform: selector) equals: globalValue ].
		
			
	
! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'jmv 7/Jul/2025, 9:21:57 pm (UTC)'!
test08OldNameLiteralReferencesAreRenamed

	| rename classToRefactor referencingMethod selector |

	self withDefaultsDo: [ :oldName :newName :globalValue |
		classToRefactor := self createClassNamed: #AClassToRefactor.
		selector := 'm1' asSymbol.
		classToRefactor compile: selector, ' ^#', oldName asString.

		rename := RenameGlobal from: oldName to: newName in: Smalltalk.
		rename apply.

		referencingMethod := classToRefactor compiledMethodAt: selector.
		self assert: (referencingMethod hasLiteral: newName).
		self deny: (referencingMethod hasLiteral: oldName).
		self assert: (classToRefactor new perform: selector) equals: newName ].
		
			! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:28:21 pm (UTC)'!
test09NewNameHasToBeASymbol

	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: '' asSymbol ]
			failsWith: [ NewGlobalPrecondition newNameCanNotBeEmptyErrorMessage]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:28:30 pm (UTC)'!
test10NewNameCanNotHaveSpaces

	self withDefaultsDo: [ :oldName :newName :globalValue |
		self 
			assertCreation: [ RenameGlobal from: oldName to: 'With spaces' asSymbol ]
			failsWith: [ NewGlobalPrecondition newNameCanNotHaveSeparatorsErrorMessage]]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 10:33:27 pm (UTC)'!
test11ItDoesNotRenameReferencesToClassVariableWithSameName

	| rename classWithClassVariable renamedReferences oldNameAsString |

	oldNameAsString := 'OldNameToRename__'.
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldNameAsString asSymbol.
	classWithClassVariable compile: 'm1 ^', oldNameAsString.

	self 
		define: oldNameAsString 
		with: Object new 
		toBeRenamedAs: 'NewNameRenamed__' 
		while: [ :oldName :newName :globalValue |

			rename := RenameGlobal from: oldName to: newName.
			renamedReferences := rename apply.
	
			self assert: renamedReferences isEmpty ]
	
	! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 10:33:34 pm (UTC)'!
test12ItDoesNotRenameReferencesToClassVariableWithSameNameDefinedInAnySuperclass

	| classWithClassVariable rename renamedReferences superClassOfClassWithClassVarReference oldNameAsString |

	oldNameAsString := 'OldNameRenamed__'.

	superClassOfClassWithClassVarReference := self createClassNamed: 'ClassReferencingClassVarSuperclass' asSymbol.
	superClassOfClassWithClassVarReference addClassVarName: oldNameAsString asSymbol.
	
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol subclassOf: superClassOfClassWithClassVarReference.
	classWithClassVariable compile: 'm1 ^', oldNameAsString.

	self 
		define: oldNameAsString 
		with: Object new 
		toBeRenamedAs: 'NewNameRenamed__' 
		while: [ :oldName :newName :globalValue |

			rename := RenameGlobal from: oldName to: newName.
			renamedReferences := rename apply.
	
			self assert: renamedReferences isEmpty ]
	
	
	! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:29:03 pm (UTC)'!
test13OldNameMustBeDefined
	
	self 
		assertCreation: [ RenameGlobal from: 'AGlobal__' asSymbol to: 'AGlobal' asSymbol in: Smalltalk ]
		failsWith: [ RenameGlobal errorMessageForGlobalNotDefined: 'AGlobal__' asSymbol ]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:34:06 pm (UTC)'!
test14ItDoesNotRenameReferencesToClassVariableFromClassSideWithSameName

	| rename classWithClassVariable renamedReferences oldNameAsString |

	oldNameAsString := 'OldNameToRename__'.
	classWithClassVariable := self createClassNamed: 'ClassReferencingClassVar' asSymbol.
	classWithClassVariable addClassVarName: oldNameAsString asSymbol.
	classWithClassVariable class compile: 'm1 ^', oldNameAsString.

	self 
		define: oldNameAsString 
		with: Object new 
		toBeRenamedAs: 'NewNameRenamed__' 
		while: [ :oldName :newName :globalValue |

			rename := RenameGlobal from: oldName to: newName.
			renamedReferences := rename apply.
	
			self assert: renamedReferences isEmpty ]
	
	! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:49:43 pm (UTC)'!
test15OldNameCanNotBeEmpty
	
	self 
		assertCreation: [ RenameGlobal from: '' asSymbol to: 'AGlobal' asSymbol in: Smalltalk ]
		failsWith: [ RenameGlobal oldNameCanNotBeEmptyErrorMessage ]! !

!RenameGlobalTest methodsFor: 'tests' stamp: 'HAW 26/Mar/2019, 11:26:58 pm (UTC)'!
withDefaultsDo: aBlock

	self 
		define: 'OldNameToRename__' 
		with: Object new 
		toBeRenamedAs: 'NewNameToRename__' 
		while: aBlock! !

!RenameInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 11/Jun/2017, 10:13:05 pm (UTC)'!
classToRefactorName

	^#ClassToRenameInstVar! !

!RenameInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 1/Jun/2017, 6:28:02 pm (UTC)'!
createClassToRefactor
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: oldVariable ! !

!RenameInstanceVariableTest methodsFor: 'class factory' stamp: 'jmv 20/Mar/2024, 8:00:33 pm (UTC)'!
createClassToRefactorAndAssertRenameCreationFailsWith: aStringOrBlock

	self createClassToRefactor.
	self assertRenameCreationFailsWith: aStringOrBlock ! !

!RenameInstanceVariableTest methodsFor: 'assertions' stamp: 'jmv 20/Mar/2024, 8:00:27 pm (UTC)'!
assertRenameCreationFailsWith: aStringOrBlock

	self
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: aStringOrBlock ! !

!RenameInstanceVariableTest methodsFor: 'setup' stamp: 'HAW 29/May/2017, 1:33:44 am (UTC)'!
setUp

	super setUp.
	
	oldVariable := 'old'.
	newVariable := 'new'.! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 27/May/2017, 3:21:06 pm (UTC)'!
test01CanNotRenameAnInstanceVariableNotDefinedInClass

	classToRefactor := self createClassNamed: #ClassWithoutInstVar instanceVariableNames: ''.
	
	self assertRenameCreationFailsWith: [ RenameInstanceVariable errorMessageForInstanceVariable: oldVariable notDefinedIn: classToRefactor ]
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:45:01 pm (UTC)'!
test02NewVariableNameCanNotBeEmpty
	
	newVariable := '   '.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition newVariableCanNotBeEmptyErrorMessage ]
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:45:05 pm (UTC)'!
test03NewVariableNameCanNotContainBlanks
	
	newVariable := 'a b'.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable]
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:45:10 pm (UTC)'!
test04NewVariableNameCanNotContainInvalidCharacters

	newVariable := '2a'.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable]
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 29/May/2017, 1:33:30 am (UTC)'!
test05ValidNewVariableNameGetBlanksTrimmed

	newVariable := ' a '.
	self createClassToRefactor.
			
	self
		shouldnt: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		raise: self refactoringError ! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:45:17 pm (UTC)'!
test06NewVariableNameCanNotBeDefinedInClass

	newVariable := oldVariable.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactor )]
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:45:22 pm (UTC)'!
test07NewVariableNameCanNotBeDefinedInSuperclasses

	|  classToRefactorSuperclass  |

	classToRefactorSuperclass := self createClassNamed: #SuperclassWithInstVar instanceVariableNames: newVariable.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: oldVariable.
	
	self assertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSuperclass)]
	
	
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 3/Mar/2019, 12:45:39 pm (UTC)'!
test08NewVariableNameCanNotBeDefinedInAnySubclass

	| classToRefactorSubclass |

	self createClassToRefactor.
	classToRefactorSubclass := self createClassNamed: #SubclassWithInstVar subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	self assertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSubclass)]
	
		! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 27/May/2017, 3:36:03 pm (UTC)'!
test09RenameCreatesNewInstanceVariableAndDeletesOldOne

	| rename |

	self createClassToRefactor.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor .
	rename apply.
	
	self assert: (classToRefactor definesInstanceVariableNamed: newVariable).
	self deny: (classToRefactor definesInstanceVariableNamed: oldVariable) 
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 29/May/2017, 1:05:15 am (UTC)'!
test10ReadReferencesToOldVariableAreRenamed

	| selector method rename |

	selector := #m1.
	self createClassToRefactor.
	classToRefactor compile: selector , ' ^' , oldVariable.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactor >> selector.
	self assert: (method readsInstanceVariable: newVariable).
	self deny: (method readsInstanceVariable: oldVariable) 
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 29/May/2017, 1:07:27 am (UTC)'!
test11WriteReferencesToOldVariableAreRenamed

	|  selector method rename |

	selector := #m1.
	self createClassToRefactor.
	classToRefactor compile: selector , ' ' , oldVariable, ' := 10'.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactor >> selector.
	self assert: (method writesInstanceVariable: newVariable).
	self deny: (method writesInstanceVariable: oldVariable) 
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 29/May/2017, 1:10:59 am (UTC)'!
test12ReferencesInSubclassesAreRenamed

	| selector method classToRefactorSubclass rename |

	selector := #m1.
	self createClassToRefactor.
	classToRefactorSubclass _ self createClassNamed: #SubclassWithInstVar subclassOf: classToRefactor instanceVariableNames: ''.
	classToRefactorSubclass compile: selector , ' ' , oldVariable, ' := 10. ^' , oldVariable.

	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactorSubclass >> selector.
	self assert: (method readsInstanceVariable: newVariable).
	self assert: (method writesInstanceVariable: newVariable).
	self deny: (method readsInstanceVariable: oldVariable).
	self deny: (method writesInstanceVariable: oldVariable) ! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 29/May/2017, 1:46:36 am (UTC)'!
test13ReferencesToOtherVariablesAreNotRenamed

	| variableToKeep selector method rename |

	variableToKeep := 'keep'.
	selector := #m1.
	classToRefactor := self createClassNamed:  self classToRefactorName instanceVariableNames: oldVariable, ' ', variableToKeep.
	classToRefactor compile: selector , ' ' , variableToKeep, ' := ' , oldVariable, '. ^' , variableToKeep.

	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactor >> selector.
	self assert: (method readsInstanceVariable: variableToKeep).
	self assert: (method writesInstanceVariable: variableToKeep) 
! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 29/May/2017, 1:15:59 am (UTC)'!
test14NewVariableOfExistingInstancesReferToOldVariableValue

	| rename instance |

	self createClassToRefactor.
	instance := classToRefactor new.
	instance instVarNamed: oldVariable put: 1.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor .
	rename apply.
	
	self assert: 1 equals:  (instance instVarNamed: newVariable).
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/Mar/2019, 2:45:27 am (UTC)'!
test15FailsWhenNewVariableEqualsTemporaryVariableOfAMethodInClass

	| selector |

	self createClassToRefactor.
	selector := #m1.
	classToRefactor compile: selector, ' | ', newVariable, ' | ', newVariable, ' := 10.'.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/Mar/2019, 2:45:44 am (UTC)'!
test16FailsWhenNewVariableEqualsArgumentOfAMethodInClass

	| selector |

	self createClassToRefactor.
	selector := #m1:.
	classToRefactor compile: selector, newVariable.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/Mar/2019, 2:45:59 am (UTC)'!
test17FailsWhenNewVariableEqualsArgumentOfABlockInAMethodInClass

	| selector |

	self createClassToRefactor.
	selector := #m1.
	classToRefactor compile: selector, ' [ :', newVariable, ' | ] value: 1'.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/Mar/2019, 2:46:25 am (UTC)'!
test18FailsWhenNewVariableEqualsTemporaryOfABlockInAMethodInClass

	| selector |

	self createClassToRefactor.
	selector := #m1.
	classToRefactor compile: selector, ' [ | ', newVariable, ' | ] value'.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 8/Mar/2019, 2:46:39 am (UTC)'!
test19FailsWhenNewVariableIsHiddenInAnyMethodOfAnySubclass

	| selector subclass |

	self createClassToRefactor.
	subclass := self createClassNamed: #SubclassOfClassToRefactor subclassOf: classToRefactor.
	selector := #m1.
	subclass compile: selector, ' | ', newVariable, ' | '.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (subclass>>selector)) ].
	! !

!RenameInstanceVariableTest methodsFor: 'tests' stamp: 'HAW 13/Mar/2025, 10:06:18 pm (UTC)'!
test20ClassChangeIsAddedWhenRenamingAnInstanceVariable

	| rename classChanges refactorClassChangeSet refactorClassChangeSets package |

	"I have to do this because on this case I want to keep track of the changes - Hernan"
	classToRefactor := self 
		createClassNamed: self classToRefactorName 
		subclassOf: Object 
		instanceVariableNames: oldVariable 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.

	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor .
	rename apply.
	
	refactorClassChangeSets := ChangeSet allChangeSetsWithClass: classToRefactor.
	self assert: refactorClassChangeSets size equals: 1.
	
	refactorClassChangeSet := refactorClassChangeSets anyOne.	
	classChanges := refactorClassChangeSet classChangeAt: classToRefactor name.
	self assert: (classChanges includes: #change).
	
	ChangeSet allChangeSets remove: refactorClassChangeSet.
	package := CodePackage packageOfClass: classToRefactor ifNone: nil.
	self assert: package notNil.
	package hasUnsavedChanges: false ! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 21/Mar/2025, 12:46:45 am (UTC)'!
allClassCategoriesOfTestData

	^Array 
		with: self classCategoryOfTestData 
		with: self anotherClassCategoryOfTestData 
		with: self siblingClassCategory ! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 4/Mar/2019, 6:59:42 pm (UTC)'!
anotherClassCategoryOfTestData

	^self classCategoryOfTestData,'_Another'! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 15/Aug/2018, 4:03:16 pm (UTC)'!
classToRefactorName

	^#ClassToRenameSelector! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 13/Mar/2025, 10:07:17 pm (UTC)'!
createClassToRefactor

	classToRefactor := self 
		createClassWithImplementorAndSenderNamed: self classToRefactorName 
		subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData
! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 26/May/2017, 3:14:02 am (UTC)'!
createClassWithImplementorAndSenderInMetaTooNamed: aName subclassOf: aSuperclass categorizedAd: aCategory

	| newClass |
	
	newClass := self createClassWithImplementorAndSenderNamed: aName subclassOf: aSuperclass categorizedAd: aCategory.
	newClass class compile: oldSelector asString.
	newClass class compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	^newClass
! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 29/May/2017, 1:49:40 am (UTC)'!
createClassWithImplementorAndSenderNamed: aName subclassOf: aSuperclass categorizedAd: aCategory

	| newClass |
	
	newClass := self createClassNamed: aName subclassOf: aSuperclass category: aCategory.
	newClass compile: oldSelector asString.
	newClass compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	^newClass
! !

!RenameSelectorTest methodsFor: 'class factory' stamp: 'HAW 26/Aug/2026, 11:55:32 am (UTC)'!
siblingClassCategory

	^self classCategoryOfTestData,'-sibling'! !

!RenameSelectorTest methodsFor: 'assertions' stamp: 'jmv 8/Jul/2025, 6:46:37 pm (UTC)'!
assertWasNotRenamedInClass: aClass 

	| senderMethod |
	
	senderMethod := aClass compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsSelector: newSelector).
	self assert: (senderMethod sendsSelector: oldSelector)! !

!RenameSelectorTest methodsFor: 'assertions' stamp: 'jmv 8/Jul/2025, 6:46:41 pm (UTC)'!
assertWasRenamedInClass: aClass
	
	| senderMethod |
	
	self assert: (aClass includesSelector: newSelector).
	self deny: (aClass includesSelector: oldSelector).
	senderMethod := aClass compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
! !

!RenameSelectorTest methodsFor: 'assertions' stamp: 'HAW 7/Mar/2020, 10:44:59 pm (UTC)'!
assertWasRenamedInClassAndMeta: aClass
	
	self
		assertWasRenamedInClass: aClass;
		assertWasRenamedInClass: aClass class! !

!RenameSelectorTest methodsFor: 'setup' stamp: 'HAW 29/May/2017, 1:46:55 am (UTC)'!
setUp

	super setUp.
	
	oldSelector := 'oldXyz__' asSymbol.
	newSelector := 'newXyz__' asSymbol.
	senderOfOldSelector := 'm1__' asSymbol.! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 15/Aug/2018, 4:03:44 pm (UTC)'!
test01MessageWithNoSendersCreatesNewMethodAndRemovesOldOne
	
	| rename |
	
	classToRefactor := self createClassNamed: self classToRefactorName..
	classToRefactor compile: oldSelector asString.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #().
	rename apply.
	self deny: (classToRefactor includesSelector: oldSelector).
	self assert: (classToRefactor includesSelector: newSelector) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:46:46 pm (UTC)'!
test02SendersOfMessageAreRenamed
	
	| rename senderMethod |
	
	self createClassToRefactor.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>senderOfOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector)! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:46:59 pm (UTC)'!
test03OtherMessagesSendsAreNotRenamed

	|  rename senderMethod selectorToKeep |

	selectorToKeep := #toKeep.
	self createClassToRefactor.
	classToRefactor compile: senderOfOldSelector asString , ' self ' , oldSelector asString , '. self ' , selectorToKeep asString.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>senderOfOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: senderOfOldSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector).
	self assert: (senderMethod sendsSelector: selectorToKeep) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 15/Aug/2018, 3:52:37 pm (UTC)'!
test04OldSelectorCanNotBeEmpty

	self 
		assertCreation: [ RenameSelector from: '' asSymbol to: newSelector implementors: #() senders: #() ]
		failsWith: [ RenameSelector oldSelectorCanNotBeEmptyErrorMessage ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 15/Aug/2018, 3:52:37 pm (UTC)'!
test05NewSelectorCanNotBeEmpty

	self 
		assertCreation: [ RenameSelector from: oldSelector to: '' asSymbol implementors: #() senders: #() ]
		failsWith: [ RenameSelector newSelectorCanNotBeEmptyErrorMessage ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 30/Dec/2019, 10:01:00 pm (UTC)'!
test06CanRenameWithoutImplementors

	self shouldntFail: [ RenameSelector from: oldSelector to: newSelector implementors: #() senders: #() ]
		! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 18/Aug/2018, 7:48:43 pm (UTC)'!
test07AllImplementorsMustImplementOldSelector

	| invalidImplementors |
	
	invalidImplementors := Array with: Object>>#printString.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: invalidImplementors senders: #() ]
		failsWith: [ RenameSelector errorMessageForInvalidImplementors: invalidImplementors ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 15/Aug/2018, 3:52:37 pm (UTC)'!
test08AllSendersShouldSendOldSelector

	| implementors invalidSenders |
	
	oldSelector := #printString.
	implementors := Array with: Object>>oldSelector.
	invalidSenders := Array with: Object>>#size.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: implementors senders: invalidSenders ]
		failsWith: [ RenameSelector errorMessageForInvalidSenders: invalidSenders of: oldSelector ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 15/Aug/2018, 3:52:37 pm (UTC)'!
test09NewSelectorMustBeOfSameTypeAsOldSelector

	oldSelector := #size.
	newSelector := #+.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: #() senders: #() ]
		failsWith: [ RenameSelector errorMessageForNewSelector: newSelector isNotOfSameTypeAs: oldSelector ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 15/Aug/2018, 3:52:37 pm (UTC)'!
test10NewSelectorMustHaveSameNumberOfArgumentsAsOldSelector

	oldSelector := #printOn:.
	newSelector := #do:separatedBy:.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: #() senders: #() ]
		failsWith: [ RenameSelector errorMessageForNewSelector: newSelector doesNotHaveSameNumberOfArgumentsAs: oldSelector ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 15/Aug/2018, 3:52:37 pm (UTC)'!
test11NewImplementorsKeepMethodCategory
	
	| rename oldSelectorCategory |
	
	oldSelectorCategory := 'oldSelectorCategory'.
	self createClassToRefactor.
	classToRefactor organization classify: oldSelector under: oldSelectorCategory.
		
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>senderOfOldSelector).
	rename apply.
	
	self assert: oldSelectorCategory equals: (classToRefactor organization categoryOfElement: newSelector)
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:47:03 pm (UTC)'!
test12RenamesRecursiveMethodsWhenNotInSenders

	| rename senderMethod |
	
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString , ' self ' , oldSelector asString.
		
	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #().
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: newSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector)
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:47:23 pm (UTC)'!
test13RenamesRecursiveMethodsWhenInSenders

	| rename senderMethod implementorsAndSenders |
	
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString , ' self ' , oldSelector asString.
	implementorsAndSenders := Array with: classToRefactor>>oldSelector.
	
	rename := RenameSelector from: oldSelector to: newSelector implementors: implementorsAndSenders senders: implementorsAndSenders.
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: newSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector)
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:56:35 pm (UTC)'!
test14RenamesSymbolsEqualToOldSelector

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' #' , oldSelector asString, ' size'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 13/Mar/2025, 5:59:15 pm (UTC)'!
test15ScopeToRenameCanBeClassOnly
	
	| rename anotherClassSendingMessage |
	
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: self defaultSuperclass categorizedAd: self classCategoryOfTestData.
	
	anotherClassSendingMessage := self createClassNamed: #ClassToAvoidRenameSelector.
	anotherClassSendingMessage compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	rename := RenameSelector from: oldSelector to: newSelector in: classToRefactor.
	rename apply.
	
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage ! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 13/Mar/2025, 5:59:15 pm (UTC)'!
test16ScopeToRenameCanBeHierarchyOnly
	
	| rename anotherClassSendingMessage superclassToRefactor subclassToRefactor |
	
	superclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SuperclassToRenameSelector subclassOf: self defaultSuperclass categorizedAd: self classCategoryOfTestData.
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: superclassToRefactor categorizedAd: self classCategoryOfTestData.
	subclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SubclassToRenameSelector subclassOf: classToRefactor categorizedAd: self classCategoryOfTestData.

	anotherClassSendingMessage := self createClassNamed: #ClassToAvoidRenameSelector.
	anotherClassSendingMessage compile: senderOfOldSelector asString , ' self ' , oldSelector asString.

	rename := RenameSelector from: oldSelector to: newSelector inHierarchyOf: classToRefactor.
	rename apply.
	
	self assertWasRenamedInClassAndMeta: superclassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: subclassToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage ! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 26/Aug/2026, 11:54:38 am (UTC)'!
test17CategoryScopeRenamesInAllCategoryTree
	
	| rename classToRefactorInSiblingCategory  anotherClassToRefactor |
	
	classToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: self 
		classToRefactorName subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData.
	anotherClassToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #AnotherclassToRenameSelector 
		subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData.
	classToRefactorInSiblingCategory := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #AnotherclassToRenameSelectorInSiblingCategory 
		subclassOf: self defaultSuperclass 
		categorizedAd: self siblingClassCategory.
	
	rename := RenameSelector from: oldSelector to: newSelector inCategoryOf: classToRefactor organizedBy: SystemOrganization.
	rename apply.

	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: anotherClassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactorInSiblingCategory
		! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 21/Mar/2025, 12:51:49 am (UTC)'!
test18HierarchyAndCategoryRenamesInHierarchyAndCategoryTree
	
	| rename superclassToRefactor subclassToRefactor anotherClassToRefactor classInOtherCategoryToRefactor |
	
	superclassToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #SuperclassToRenameSelector 
		subclassOf: self defaultSuperclass 
		categorizedAd: self anotherClassCategoryOfTestData.
	classToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName 
		subclassOf: superclassToRefactor 
		categorizedAd: self classCategoryOfTestData.
	subclassToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #SubclassToRenameSelector 
		subclassOf: classToRefactor 
		categorizedAd: self classCategoryOfTestData.
	anotherClassToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #AnotheclassToRenameSelector 
		subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData.
	classInOtherCategoryToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #OtherCatClassToRenameSelector 
		subclassOf: self defaultSuperclass 
		categorizedAd: self anotherClassCategoryOfTestData.
	
	rename := RenameSelector 
		from: oldSelector 
		to: newSelector 
		inCategoriesAndHierarchyOf: classToRefactor 
		organizedBy: SystemOrganization.
	rename apply.

	self assertWasRenamedInClassAndMeta: superclassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: subclassToRefactor.
	self assertWasRenamedInClassAndMeta: anotherClassToRefactor.
	self assertWasRenamedInClassAndMeta: classInOtherCategoryToRefactor.
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 13/Mar/2025, 5:59:15 pm (UTC)'!
test19ScopeToRenameCanBeTheCompleteSystem
	
	| rename anotherClassToRefactor |
	
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: self defaultSuperclass categorizedAd: self classCategoryOfTestData.
	anotherClassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #AnotherclassToRenameSelector subclassOf: self defaultSuperclass categorizedAd: self classCategoryOfTestData.

	rename := RenameSelector from: oldSelector to: newSelector inSystem: Smalltalk.
	"This is just to be sure that no more than the expected methods will be renamed because I'm using Smalltalk as system.
	I could mock Smalltalk but I want to have a real test using Smalltalk, not a mock, that it is why I have these assertions here - Hernan"
	self assert: 4 equals: rename implementorsSize.
	self assert: 4 equals: rename sendersSize.
	
	rename apply.

	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: anotherClassToRefactor  
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 15/Aug/2018, 3:52:37 pm (UTC)'!
test20CanNotRenameToItSelf

	self 
		assertCreation: [ RenameSelector from: oldSelector to: oldSelector asSymbol implementors: #() senders: #() ]
		failsWith: [ RenameSelector newSelectorEqualToOldSelectorErrorMessage ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 15/Aug/2018, 3:52:37 pm (UTC)'!
test21NewSelectorCanNotBeImplementedOnAnyClassImplementingOldSelector

	| implementors |
	
	oldSelector := #printString.
	newSelector := #size.
	implementors := Array with: Object>>oldSelector.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: implementors senders: #() ]
		failsWith: [ RenameSelector errorMessageForNewSelector: newSelector implementedInAll: (Array with: Object) ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 15/Aug/2018, 3:52:37 pm (UTC)'!
test22WarnsWhenOverridesSuperclassImplementation

	| implementors |
	
	oldSelector := #negated.
	newSelector := #size.
	implementors := Array with: Number>>oldSelector.
	
	self 
		assertCreation: [ RenameSelector from: oldSelector to: newSelector implementors: implementors senders: #() ]
		warnsWith: [ RenameSelector warningMessageForImplementationOf: newSelector in: Number willOverrideImplementationIn: Object ]
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:56:51 pm (UTC)'!
test23OldSelectorIsRenamedWhenDefinedInLiteralArray

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ')'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:56:58 pm (UTC)'!
test24OldSelectorIsRenamedWhenDefinedInLiteralArrayMoreThanOnce

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,')'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:57:04 pm (UTC)'!
test25OldSelectorIsRenamedWhenDefinedInMoreThanOneLiteralArrayMoreThanOnce

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,'), #(#' , oldSelector asString, ' #' , oldSelector asString,')'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:57:09 pm (UTC)'!
test26DoesNotRenameLiteralsThatBeginsWithOldSelectorInsideLiteralArray

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,'1)'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector). 
	self assert: (senderMethod hasLiteral: (oldSelector,'1') asSymbol) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:57:14 pm (UTC)'!
test27DoesNotRenameLiteralsThatBeginsWithOldSelectorAndEndWithColonInsideLiteralArray

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	self createClassToRefactor.
	classToRefactor compile: oldSelector asString.
	classToRefactor compile: referencerToOldSelector asString , ' ^#(#' , oldSelector asString, ' #' , oldSelector asString,':)'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod hasLiteral: oldSelector).
	self assert: (senderMethod hasLiteral: newSelector). 
	self assert: (senderMethod hasLiteral: (oldSelector,':') asSymbol) 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:47:53 pm (UTC)'!
test28CanRenameFromBinaryToKeywordOfOneParameter

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	oldSelector := '&&' asSymbol.
	newSelector := 'abc:' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' aParameter'.
	classToRefactor compile: referencerToOldSelector asString , ' ^self ' , oldSelector asString,' 1'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	self assert: (classToRefactor includesSelector: newSelector).
	self deny: (classToRefactor includesSelector: oldSelector).
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector). 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'jmv 8/Jul/2025, 6:47:57 pm (UTC)'!
test29CanRenameFromKeywordOfOneParameterToBinary

	| rename senderMethod referencerToOldSelector |
	
	referencerToOldSelector := #m1.
	oldSelector := 'abc:' asSymbol.
	newSelector := '&&' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' aParameter'.
	classToRefactor compile: referencerToOldSelector asString , ' ^self ' , oldSelector asString,' 1'.

	rename := RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: (Array with: classToRefactor>>referencerToOldSelector).
	rename apply.
	
	self assert: (classToRefactor includesSelector: newSelector).
	self deny: (classToRefactor includesSelector: oldSelector).
	senderMethod := classToRefactor compiledMethodAt: referencerToOldSelector.
	self deny: (senderMethod sendsSelector: oldSelector).
	self assert: (senderMethod sendsSelector: newSelector). 
! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 13/Dec/2018, 11:00:10 pm (UTC)'!
test30CanNotRenameFromBinaryToKeywordOfMoreThanOneParameter

	oldSelector := '&&' asSymbol.
	newSelector := 'abc:def:' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: oldSelector asString, ' aParameter'.

	self 
		should: [ RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #() ]
		raise: Error
		withMessageText: (RenameSelector errorMessageForNewSelector: newSelector isNotOfSameTypeAs: oldSelector)! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 13/Dec/2018, 10:59:39 pm (UTC)'!
test31CanNotRenameFromKeywordOfOneParameterToBinary

	oldSelector := 'abc:def:' asSymbol.
	newSelector := '&&' asSymbol.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: 'abc: p1 def: p2'.

	self 
		should: [ RenameSelector from: oldSelector to: newSelector implementors: (Array with: classToRefactor>>oldSelector) senders: #() ]
		raise: Error
		withMessageText: (RenameSelector errorMessageForNewSelector: newSelector isNotOfSameTypeAs: oldSelector)! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 13/Mar/2025, 5:59:15 pm (UTC)'!
test32HierarchyScopeRenamesSelectorsFromSuperclassDefiningSelector
	
	| rename superclassToRefactor subclassToRefactor |
	
	superclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SuperclassToRenameSelector subclassOf: self defaultSuperclass categorizedAd: self classCategoryOfTestData.
	classToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName subclassOf: superclassToRefactor categorizedAd: self classCategoryOfTestData.
	subclassToRefactor := self createClassWithImplementorAndSenderInMetaTooNamed: #SubclassToRenameSelector subclassOf: superclassToRefactor categorizedAd: self classCategoryOfTestData.

	rename := RenameSelector from: oldSelector to: newSelector inHierarchyOf: classToRefactor.
	rename apply.
	
	self assertWasRenamedInClassAndMeta: superclassToRefactor.
	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasRenamedInClassAndMeta: subclassToRefactor.
	! !

!RenameSelectorTest methodsFor: 'tests' stamp: 'HAW 13/Mar/2025, 5:59:15 pm (UTC)'!
test33DoNotRenameSendersAndImplementorsWhoseCompilerClassIsDifferentToCompiler
	
	"Requested by Phil B. to avoid refactoring OMeta2 classes - Hernan"
	
	| rename anotherClassSendingMessage  |
	
	classToRefactor := self 
		createClassWithImplementorAndSenderInMetaTooNamed: self classToRefactorName 
		subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData.
	
	anotherClassSendingMessage := self 
		createClassWithImplementorAndSenderInMetaTooNamed: #ClassToAvoidRenameSelector 
		subclassOf: self defaultSuperclass 
		categorizedAd: self classCategoryOfTestData.
	
	anotherClassSendingMessage class compile: 'compilerClass ^nil'.

	rename := RenameSelector 
		from: oldSelector 
		to: newSelector 
		inCategoryOf: classToRefactor 
		organizedBy: SystemOrganization.
	rename apply.

	self assertWasRenamedInClassAndMeta: classToRefactor.
	self assertWasNotRenamedInClass: anotherClassSendingMessage.
	self assertWasRenamedInClass: anotherClassSendingMessage class.
	! !

!RenameTemporaryTest methodsFor: 'class factory' stamp: 'HAW 26/Jun/2017, 1:53:44 am (UTC)'!
classToRefactorName

	^#ClassToRenameTemp! !

!RenameTemporaryTest methodsFor: 'class factory' stamp: 'HAW 26/Jun/2017, 1:53:44 am (UTC)'!
methodNodeOf: aSourceCode

	^self methodNodeOf: aSourceCode in: self class! !

!RenameTemporaryTest methodsFor: 'class factory' stamp: 'HAW 10/Aug/2018, 3:23:12 am (UTC)'!
methodNodeOf: aSourceCode in: aClass

	^aClass methodNodeFor: aSourceCode ! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 4/Oct/2017, 9:04:33 pm (UTC)'!
test01VariableToRenameHasToBeDefined

	| newVariable oldVariable methodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1'.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ RenameTemporary errorMessageForTemporaryVariable: oldVariable notDefinedIn: methodNode ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 25/Feb/2020, 4:02:32 am (UTC)'!
test02NewVariableNameCanNotBeEmpty

	| methodNode newVariable oldVariable |
	
	oldVariable := 'old'.
	newVariable := ' '.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForEmptyTemporaryVariable ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 25/Feb/2020, 4:06:33 am (UTC)'!
test03NewVariableHasToBeValid

	| methodNode newVariable oldVariable |
	
	oldVariable := 'old'.
	newVariable := 'a b'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForInvalidTemporaryVariable: newVariable ]
	! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 25/Feb/2020, 4:07:12 am (UTC)'!
test04NewVariableNameCanNotBeDefinedInMethod

	| methodNode oldVariable |
	
	oldVariable := 'old'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	self 
		assertCreation: [RenameTemporary from: oldVariable to: oldVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariable: oldVariable isAlreadyDefinedIn: methodNode ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 25/May/2020, 12:01:16 am (UTC)'!
test05FailsIfNewTemporaryIsEqualToInstanceVariableInClass

	| methodNode oldVariable newVariable classToRefactor |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | ' in: classToRefactor.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: classToRefactor ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 25/May/2020, 12:01:16 am (UTC)'!
test06FailsIfNewTemporaryIsEqualToInstanceVariableInAnySuperclass

	| methodNode oldVariable newVariable classToRefactor superclassToRefactor |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	superclassToRefactor := self createClassNamed: #ClassToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: superclassToRefactor.
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | ' in: classToRefactor.
	
	self 
		assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageFor: newVariable canNotBeNamedDueToInstanceVariableDefinedIn: superclassToRefactor ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 17/Dec/2019, 11:12:33 pm (UTC)'!
test07RenameCreatesNewTemporaryVariableAndDeletesOldOne

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable)! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 9/Aug/2018, 11:28:42 pm (UTC)'!
test08RenameChangesReferencesFromOldVariableToNewVariable

	| methodNode oldVariable newVariable rename newMethodNode assigmentNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | ', oldVariable, ' := 1 + ', oldVariable.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	assigmentNode := newMethodNode block statements first.
	self assert: newVariable equals: assigmentNode variable name.
	self assert: newVariable equals: assigmentNode value arguments first name.! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 17/Dec/2019, 11:12:33 pm (UTC)'!
test09RenamesArguments

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1: ', oldVariable, ' ^', oldVariable.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable).
	self assert: newVariable equals: newMethodNode block statements first expr name.! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 25/Feb/2020, 4:07:53 am (UTC)'!
test10NewVariableNameCanNotBeDefinedAsBlockArgument

	| methodNode oldVariable newVariable |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | [:', newVariable, ' | ^1 ]'.
	
	self 
		assertCreation: [RenameTemporary from: oldVariable to: newVariable in: methodNode ]
		failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariable: newVariable isAlreadyDefinedIn: methodNode ]! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 17/Dec/2019, 11:12:33 pm (UTC)'!
test11RenamesBlockArguments

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [:', oldVariable, '| ^', oldVariable, ']'.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable).
	"asserts it renamed block argument - Hernan"
	self assert: newVariable equals: newMethodNode block statements first arguments first name.
	"asserts it renamed reference to block argument - Hernan"
	self assert: newVariable equals: newMethodNode block statements first block statements first expr name! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 17/Dec/2019, 11:12:33 pm (UTC)'!
test12RenamesBlockTemporaries

	| methodNode oldVariable newVariable rename newMethodNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [ |', oldVariable, '| ^', oldVariable, ']'.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self deny: (newMethodNode hasArgumentOrTemporaryNamed: oldVariable).
	self assert: (newMethodNode hasArgumentOrTemporaryNamed: newVariable).
	"asserts it renamed block temporary - Hernan"
	self assert: newVariable equals: newMethodNode block statements first temporaries first name.
	"asserts it renamed reference to block temporary - Hernan"
	self assert: newVariable equals: newMethodNode block statements first block statements first expr name! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 17/Dec/2019, 11:15:28 pm (UTC)'!
test13OldNodeMustBePartOfMethodNode

	| methodNode oldVariable newVariable oldVariableNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 | ', oldVariable, ' | '.
	oldVariableNode := methodNode tempNodes anyOne.
	
	self 
		assertCreation: [RenameTemporary fromOldVariableNode: oldVariableNode copy to: newVariable in: methodNode ]
		failsWith: [RenameTemporary oldVariableNodeNotPartOfMethodNodeErrorDescription ]	! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 17/Dec/2019, 11:15:28 pm (UTC)'!
test14OldVariableNodeMustBeArgOrTempNode

	| methodNode newVariable oldVariableNode |
	
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 self'.
	oldVariableNode := methodNode block statements first.
	
	self 
		assertCreation: [RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode ]
		failsWith: [RenameTemporary oldVariableNodeMustBeArgOrTempNodeErrorDescription ]	! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 17/Dec/2019, 11:15:28 pm (UTC)'!
test15RenameVariablesWithSameNameInDifferentBlocksCorrectly

	| methodNode oldVariable newVariable oldVariableNode newMethodNode rename |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [|',oldVariable,'| ^',oldVariable, '].[|',oldVariable,'| ^',oldVariable, '].'.
	oldVariableNode := methodNode block statements first temporaries first.
	
	rename := RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode.
	newMethodNode := rename methodNodeAfterApply.
	
	self 
		assert: 'm1 [|',newVariable,'| ^',newVariable, '].[|',oldVariable,'| ^',oldVariable, '].'
		equals: newMethodNode sourceText! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 25/Feb/2020, 4:08:04 am (UTC)'!
test16CantRenameVariableToANameAlreadyUsedInAnUpperBlock

	| methodNode oldVariable newVariable oldVariableNode |
	
	oldVariable := 'old'.
	newVariable := 'new'.
	
	methodNode := self methodNodeOf: 'm1 [|',newVariable,'| [|',oldVariable,'| ^', oldVariable,']. ^',newVariable,' ]'.
	oldVariableNode := methodNode block statements first statements first temporaries first.
	
	self 
		assertCreation: [RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode ]
		failsWith: [
			NewTemporaryPrecondition errorMessageForNewTemporaryVariable: newVariable isAlreadyDefinedIn: methodNode ]
	! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'HAW 29/Feb/2020, 10:09:43 pm (UTC)'!
test17RenamesOneCharVariableAfterAReturnWithoutCharsAfterThatCorrectly

	| methodNode oldVariable newVariable rename newSource |
	
	"This test is due to a fixed error in the parser. 
	See ParserTest>>#testRangesAreOkWhenReturningAVariableWithoutSpaceAfterThat"
	
	oldVariable := 'o'.
	newVariable := 'n'.

	methodNode := self methodNodeOf: 'm1 |',oldVariable,'| ^',oldVariable.
	
	rename := RenameTemporary from: oldVariable to: newVariable in: methodNode.
	newSource := rename apply.
	
	self 
		assert: 'm1 |',newVariable,'| ^',newVariable
		equals: newSource
		! !

!RenameTemporaryTest methodsFor: 'tests' stamp: 'RNG 26/Apr/2020, 7:19:20 pm (UTC)'!
test18CantRenameVariableToAReservedName

	| methodNode oldVariable newVariable |
	
	oldVariable := 'old'.
	
	ClassBuilder reservedNames do: [ :reservedName |
		newVariable := reservedName asString.
		methodNode := self methodNodeOf: 'm1 |' , oldVariable , '| ^ ' , oldVariable.

		self 
			assertCreation: [ RenameTemporary from: oldVariable to: newVariable in: methodNode ]
			failsWith: [ NewTemporaryPrecondition errorMessageForNewTemporaryVariableCanNotBeAReservedName: newVariable ] ]! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 1/Aug/2018, 8:43:08 pm (UTC)'!
test02ClassesWithNoReferencesAndNoSubclassesAreSafetelyRemoved

	| classToRemove safeRemove | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	safeRemove := SafelyRemoveClass of: classToRemove.
	safeRemove apply.
	
	self assert: classToRemove isObsolete ! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 1/Aug/2018, 8:43:11 pm (UTC)'!
test03RemovingTheMetaclassRemovesTheClass

	| classToRemove safeRemove | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	safeRemove := SafelyRemoveClass of: classToRemove class.
	safeRemove apply.
	
	self assert: classToRemove isObsolete ! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 17/Aug/2018, 8:40:06 pm (UTC)'!
test04CanNotRemoveClassWithReferencesOutsideHierarchy

	| classToRemove classReferencing methodNameReferencingClass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classReferencing := self createClassNamed: #ClassReferencingClassToRemove.
	methodNameReferencingClass := #m1.
	classReferencing compile: methodNameReferencingClass asString, ' ^', classToRemove name asString.
	
	self
		should: [ SafelyRemoveClass of: classToRemove ]
		raise: self canNotRefactorDueToReferencesRefactoringError 
		withExceptionDo: [ :anError | | reference |
			self 
				assert: (SafelyRemoveClass errorMessageForCanNotRemove: classToRemove dueToReferencesToAll: (Array with: classToRemove))
				equals: anError messageText.
			self assert: 1 equals: anError numberOfReferences.
			reference := anError anyReference.
			self assert: classReferencing name equals: reference classSymbol.
			self assert: methodNameReferencingClass equals: reference methodSymbol ]! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 1/Aug/2018, 8:43:21 pm (UTC)'!
test05CanRemoveClassWithReferencesFromItself

	| classToRemove methodNameReferencingClass remove | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	methodNameReferencingClass := #m1.
	classToRemove compile: methodNameReferencingClass asString, ' ^', classToRemove name asString.
	
	remove := SafelyRemoveClass of: classToRemove.
	remove apply.
	
	self assert: classToRemove isObsolete ! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 1/Aug/2018, 8:43:27 pm (UTC)'!
test06WarnIfClassToRemoveHasSubclasses

	| classToRemove classToRemoveSubclass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	
	self 
		assertCreation: [ SafelyRemoveClass of: classToRemove ]
		warnsWith: [ SafelyRemoveClass warningMessageFor: classToRemove hasSubclasses: (Array with: classToRemoveSubclass) ]! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 17/Aug/2018, 8:38:44 pm (UTC)'!
test07CanNotRemoveClassWhenSubclassesHaveReferencesOutsideTheHierarchy

	| classToRemove classToRemoveSubclass classReferencing methodNameReferencingClass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	classReferencing := self createClassNamed: #ClassReferencingClassToRemoveSubclass.
	methodNameReferencingClass := #m1.
	classReferencing compile: methodNameReferencingClass asString, ' ^', classToRemoveSubclass name asString.
	
	self
		should: [ SafelyRemoveClass of: classToRemove ]
		raise: self canNotRefactorDueToReferencesRefactoringError 
		withExceptionDo: [ :anError | | reference |
			self 
				assert: (SafelyRemoveClass errorMessageForCanNotRemove: classToRemove dueToReferencesToAll: (Array with: classToRemoveSubclass))
				equals: anError messageText.
			self assert: 1 equals: anError numberOfReferences.
			reference := anError anyReference.
			self assert: classReferencing name equals: reference classSymbol.
			self assert: methodNameReferencingClass equals: reference methodSymbol ]	
! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 1/Aug/2018, 8:47:12 pm (UTC)'!
test08HierarchyIsRemovedIfSubclassesWarningIsResumed

	| classToRemove classToRemoveSubclass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	
	self safelyRemoveHierarchyOf: classToRemove.
	
	self assert: classToRemove isObsolete.
	self assert: classToRemoveSubclass isObsolete
		
	
		! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 1/Aug/2018, 8:47:17 pm (UTC)'!
test09CanRemoveIfReferencesToSubclassesAreInTheHierarchy

	| classToRemove classToRemoveSubclass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	classToRemove compile: 'm1 ^', classToRemoveSubclass name asString.
	classToRemoveSubclass compile: 'm2 ^', classToRemove name asString.
	
	self safelyRemoveHierarchyOf: classToRemove.
	
	self assert: classToRemove isObsolete.	
	self assert: classToRemoveSubclass isObsolete.! !

!SafelyRemoveClassTest methodsFor: 'tests' stamp: 'HAW 18/Dec/2019, 8:04:18 pm (UTC)'!
test10WarnWhenHasReferencesToName

	| classToRemove classReferencing methodNameReferencingClass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classReferencing := self createClassNamed: #ClassReferencingClassToRemove.
	methodNameReferencingClass := #m1.
	classReferencing compile: methodNameReferencingClass asString, ' ^#', classToRemove name asString.
	
	self
		should: [ SafelyRemoveClass of: classToRemove ]
		raise: self referencesRefactoringWarning 
		withExceptionDo: [ :aWarning | | reference |
			self 
				assert: (SafelyRemoveClass warningMessageForReferencesToNames: (Array with: classToRemove))
				equals: aWarning messageText.
			self assert: 1 equals: aWarning numberOfReferences.
			reference := aWarning anyReference.
			self assert: classReferencing name equals: reference classSymbol.
			self assert: methodNameReferencingClass equals: reference methodSymbol ]! !

!SafelyRemoveClassTest methodsFor: 'test support' stamp: 'HAW 1/Aug/2018, 7:48:56 pm (UTC)'!
assertSubclassesReturnsACopy

	"This is a precondition for the remove to work properly with subclasses of the class to remove. 
	I do not put it in the refactoring to avoid innecesary checks and because tests verify preconditions. 
	It is not a seprate test because I do not want senders of this precondition to run if it fails - Hernan"
	self deny: self class superclass subclasses == self class superclass subclasses! !

!SafelyRemoveClassTest methodsFor: 'test support' stamp: 'HAW 29/May/2023, 8:22:33 pm (UTC)'!
safelyRemoveHierarchyOf: classToRemove
		
	| remove |
	
	"see comment of #assertSubclassesReturnsACopy - Hernan"
	self assertSubclassesReturnsACopy.
	
	self 
		assertCreation: [ remove := SafelyRemoveClass of: classToRemove ]
		warnsWith: [ SafelyRemoveClass warningMessageFor: classToRemove hasSubclasses: classToRemove allSubclasses ].
		
	remove apply.! !

!SafelyRemoveClassTest methodsFor: 'class factory' stamp: 'HAW 11/Jul/2018, 8:56:05 pm (UTC)'!
classToRemoveName

	"I can not use the symbol directly because it would be a reference - Hernan"
	^ 'ClassToRemove' asSymbol.
	! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 25/May/2019, 4:26:04 am (UTC)'!
methodSourceCodeNamed: aSelector withTemporaryVariableDeclaration: aTemporaryVariableName

	^aSelector, '| ', aTemporaryVariableName, ' |'.! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 25/May/2019, 4:31:12 am (UTC)'!
methodSourceCodeNamed: aSelector withTemporaryVariableDeclarations: temporaryVariableNames
	^String streamContents: [ :stream |
		stream nextPutAll: aSelector, '| '.
		temporaryVariableNames do: [ :temporaryVariableName |
			stream nextPutAll: temporaryVariableName, ' ' ].	
		stream nextPut: $|. ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 25/May/2019, 4:26:04 am (UTC)'!
test01ApplyChangesTemporaryVariableToInstanceVariable

	| variableName classToRefactor messageName refactoring sourceText methodNode changedMethodNode |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	sourceText _ self methodSourceCodeNamed: messageName withTemporaryVariableDeclaration: variableName.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	self assert: (classToRefactor definesInstanceVariableNamed: variableName).
	self assert: (classToRefactor canUnderstand: messageName).
	
	changedMethodNode _ self methodNodeIn: classToRefactor named: messageName.
	self assert: 0 equals: changedMethodNode temporaries size.! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 25/May/2019, 4:26:04 am (UTC)'!
test02ApplyDoesntChangeRestOfSourceCode

	| variableName classToRefactor messageName refactoring sourceText methodNode changedMethodNode restOfSourceCode |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	sourceText _ self methodSourceCodeNamed: messageName withTemporaryVariableDeclaration: variableName. 
	restOfSourceCode _ variableName, ' := 2.
	^', variableName.
	sourceText _ sourceText, restOfSourceCode.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	changedMethodNode _ self methodNodeIn: classToRefactor named: messageName.
	self assert: (changedMethodNode sourceText findString: restOfSourceCode :: > 0).! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'HAW 19/May/2019, 9:35:13 pm (UTC)'!
test03TemporaryToChangeShouldExist

	|  classToRefactor messageName methodNode sourceText |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	sourceText _ messageName.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	self 
		should: [TemporaryToInstanceVariable named: 'a' fromMethod: methodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable inexistentTemporaryErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: sourceText ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 25/May/2019, 4:26:04 am (UTC)'!
test04TemporaryShouldNotExistInOtherClassMethod

	|  classToRefactor firstMessageName firstMethodNode firstSourceText variableName secondMessageName secondSourceText |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	variableName _ 'a'.
	firstMessageName _ #m1.
	firstSourceText _ self 
		methodSourceCodeNamed: firstMessageName 
		withTemporaryVariableDeclaration: variableName.
	firstMethodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: firstMessageName 
		sourceCode: firstSourceText.
	
	secondMessageName _ #m2.
	secondSourceText _ self 
		methodSourceCodeNamed: secondMessageName 
		withTemporaryVariableDeclaration: variableName.
	classToRefactor compile: secondSourceText.
	
	self 
		should: [TemporaryToInstanceVariable named: variableName fromMethod: firstMethodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsInOtherMethodsErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: firstMessageName in: classToRefactor hasSourceEqualTo: firstSourceText.
			self assertMethodNamed: secondMessageName in: classToRefactor hasSourceEqualTo: secondSourceText ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 25/May/2019, 4:26:04 am (UTC)'!
test05InstanceVariableShouldNotExistInSubclass

	| classToRefactor messageName methodNode sourceText variableName subclassToRefactor |

	variableName _ 'a'.
	
	classToRefactor _ self createClassNamed: self classToRefactorName.
	subclassToRefactor _ self 
		createClassNamed: #SubclassToRefactor
		subclassOf: classToRefactor
		instanceVariableNames: variableName
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
	
	messageName _ #m1.
	sourceText _ self 
		methodSourceCodeNamed: messageName 
		withTemporaryVariableDeclaration: variableName.
	methodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.

	self 
		should: [TemporaryToInstanceVariable named: variableName fromMethod: methodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsAsInstVarInSubclassesErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: sourceText.
			self assert: (subclassToRefactor instVarNames includes: variableName) ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 25/May/2019, 4:26:04 am (UTC)'!
test06TemporaryShouldNotExistInSubclassMethod

	| classToRefactor firstMessageName firstMethodNode firstSourceText variableName secondMessageName secondSourceText subclassToRefactor |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	subclassToRefactor _ self 
		createClassNamed: #SubclassToRefactor
		subclassOf: classToRefactor
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.
	
	variableName _ 'a'.
	firstMessageName _ #m1.
	firstSourceText _ self 
		methodSourceCodeNamed: firstMessageName 
		withTemporaryVariableDeclaration: variableName.
	firstMethodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: firstMessageName 
		sourceCode: firstSourceText.
	
	secondMessageName _ #m2.
	secondSourceText _ self 
		methodSourceCodeNamed: secondMessageName 
		withTemporaryVariableDeclaration: variableName.
	subclassToRefactor compile: secondSourceText.
	
	self 
		should: [TemporaryToInstanceVariable named: variableName fromMethod: firstMethodNode]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsInOtherMethodsErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: firstMessageName in: classToRefactor hasSourceEqualTo: firstSourceText.
			self assertMethodNamed: secondMessageName in: subclassToRefactor hasSourceEqualTo: secondSourceText ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 28/May/2019, 11:13:24 pm (UTC)'!
test07RemovesPipesWhenRemovingLastTemporaryInMethod

	| variableName classToRefactor messageName refactoring sourceText methodNode |
	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	sourceText _ self methodSourceCodeNamed: messageName withTemporaryVariableDeclaration: variableName.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: 'm1 '.! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 25/May/2019, 4:30:25 am (UTC)'!
test08DoesntRemoveOtherVariables

	| variableName classToRefactor messageName refactoring sourceText methodNode otherVariableName |
	classToRefactor _ self createClassNamed: self classToRefactorName.
	messageName _ #m1.
	variableName _ 'a'.
	otherVariableName _ 'b'.
	sourceText _ self 
		methodSourceCodeNamed: messageName 
		withTemporaryVariableDeclarations: {variableName. otherVariableName}.
	methodNode _ self compileMethodNodeIn: classToRefactor named: messageName sourceCode: sourceText.
	
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: 'm1|  b |'.! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 28/May/2019, 4:17:27 am (UTC)'!
test09TemporaryShouldNotExistInOtherBlockInSameMethod

	| classToRefactor messageName methodNode sourceText variableName blockWithVariableString |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	variableName _ 'a'.
	messageName _ #m1.
	blockWithVariableString _ '[ | ', variableName, ' | ].'.
	sourceText _ messageName, '
	', blockWithVariableString, '
	', blockWithVariableString.
	methodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.
	
	self 
		should: [ TemporaryToInstanceVariable named: variableName fromMethod: methodNode ]
		raise: RefactoringError
		withExceptionDo: [ :error |
			self 
				assert: TemporaryToInstanceVariable temporaryExistsInOtherBlockErrorDescription 
				equals: error messageText.
			
			self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: sourceText. ].! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 28/May/2019, 11:52:12 pm (UTC)'!
test10RemovesPipesWhenRemovingLastTemporaryInBlock

	| classToRefactor messageName methodNode sourceText variableName blockWithVariableString newSourceCode refactoring |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	variableName _ 'a'.
	messageName _ #m1.
	blockWithVariableString _ '[ | ', variableName, ' | ].'.
	sourceText _ messageName, '
	', blockWithVariableString.
	methodNode _ self 
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.
	refactoring _ TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
	refactoring apply.
	
	newSourceCode _ messageName, '
	[   ].'.
	
	self assertMethodNamed: messageName in: classToRefactor hasSourceEqualTo: newSourceCode.! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 22/Dec/2019, 10:18:26 pm (UTC)'!
test11TemporaryShouldNotExistAsArgumentInOtherBlocksInSameMethod

	| classToRefactor messageName methodNode sourceText variableName blockWithTempString blockWithArgumentString |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	variableName _ 'a'.
	messageName _ #m1.
	blockWithTempString _ '[ | ', variableName, ' | ].'.
	blockWithArgumentString _ '[ :', variableName, ' | ].'.
	sourceText _ messageName, ' ', blockWithTempString, ' ', blockWithArgumentString.
	methodNode _ self
		compileMethodNodeIn: classToRefactor 
		named: messageName 
		sourceCode: sourceText.
		
	self assertCreation: [ TemporaryToInstanceVariable named: variableName fromMethod: methodNode ]
		failsWith: [ TemporaryToInstanceVariable temporaryExistsInOtherBlockErrorDescription ].
! !

!TemporaryToInstanceVariableTest methodsFor: 'testing' stamp: 'EB 22/Dec/2019, 10:38:40 pm (UTC)'!
test12CantExtractArgument

	| classToRefactor methodNode sourceText |

	classToRefactor _ self createClassNamed: self classToRefactorName.
	
	sourceText := 'm1: a'.
	methodNode _ self
		compileMethodNodeIn: classToRefactor 
		named: #m1:
		sourceCode: sourceText.
		
	self assertCreation: [ TemporaryToInstanceVariable named: 'a' fromMethod: methodNode ]
		failsWith: [ TemporaryToInstanceVariable inexistentTemporaryErrorDescription ].
! !

!TemporaryToInstanceVariableTest methodsFor: 'assertions' stamp: 'HAW 19/May/2019, 9:29:33 pm (UTC)'!
assertMethodNamed: aMethodName in: aClass hasSourceEqualTo: aSourceCode

	| changedMethodNode |

	changedMethodNode _ self methodNodeIn: aClass named: aMethodName.
	self assert: aSourceCode equals: changedMethodNode sourceText.
			! !

!TemporaryToInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 19/May/2019, 9:29:44 pm (UTC)'!
classToRefactorName

	^ #ClassToChangeVariable.! !

!TemporaryToInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 19/May/2019, 9:31:47 pm (UTC)'!
compileMethodNodeIn: aClass named: aSelector sourceCode: sourceCode

	aClass compile: sourceCode.

	^self methodNodeIn: aClass named: aSelector.! !

!TemporaryToInstanceVariableTest methodsFor: 'class factory' stamp: 'HAW 19/May/2019, 9:31:40 pm (UTC)'!
methodNodeIn: aClass named: aSelector

	^ aClass compiledMethodAt: aSelector :: methodNode.! !
