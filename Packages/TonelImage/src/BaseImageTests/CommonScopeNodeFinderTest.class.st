Class {
	#name : 'CommonScopeNodeFinderTest',
	#superclass : 'DynamicallyCodeCreationTest',
	#instVars : [
		'classToRefactor',
		'methodNode',
		'finder'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
CommonScopeNodeFinderTest >> assertFinderFoundMethodNodeScope [

	self assertFinderFoundScopeFromNode: self methodNodeScope 
]

{ #category : 'assertions' }
CommonScopeNodeFinderTest >> assertFinderFoundScopeFromNode: aParseNodeWithSourceRanges [

	self 
		assert: finder commonScopeNode 
		equals: aParseNodeWithSourceRanges
]

{ #category : 'class factory' }
CommonScopeNodeFinderTest >> classToRefactorName [

	^ #ClassToDoExtractTemporary
]

{ #category : 'excercising' }
CommonScopeNodeFinderTest >> compile: aSourceCode [

	classToRefactor compile: aSourceCode.
	methodNode := (classToRefactor>>#m1) methodNode.
]

{ #category : 'excercising' }
CommonScopeNodeFinderTest >> methodNodeScope [

	^ methodNode -> (1 to: methodNode sourceText size)
]

{ #category : 'excercising' }
CommonScopeNodeFinderTest >> parseNodeAt: aPosition [

	^ (methodNode 
		parseNodesPathAt: aPosition
		ifAbsent: [ self shouldNotHappen ]) first
]

{ #category : 'excercising' }
CommonScopeNodeFinderTest >> parseNodeIncluding: aPosition [

	^ methodNode
		parseNodeIncluding: aPosition
		ifAbsent: [ self shouldNotHappen ]
]

{ #category : 'excercising' }
CommonScopeNodeFinderTest >> parseNodeOfInterval: anInterval [

	| parseNodes |
	
	parseNodes := methodNode 
		parseNodesPathAt: anInterval first 
		ifAbsent: [ self shouldNotHappen ].
	
	"Sometimes returns a LeafNode that is not expected, but the node expected is in the list"
	^ parseNodes detect: [ :parseNode | (parseNode isKindOf: LeafNode) not and: [ parseNode value = anInterval ] ]
]

{ #category : 'excercising' }
CommonScopeNodeFinderTest >> parseNodeToLookForUsing: aConditionBlock [

	| parseNodeToLookFor |

	methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(aConditionBlock value: parseNode) 
			ifTrue: [ parseNodeToLookFor := parseNode -> sourceRanges asOrderedCollection first ] ].
	
	parseNodeToLookFor ifNil: [ self failWith: 'Node not found'].
	
	^ parseNodeToLookFor
]

{ #category : 'setup/teardown' }
CommonScopeNodeFinderTest >> setUp [

	super setUp.
	classToRefactor := self createClassNamed: self classToRefactorName
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test001_WhenHavingMethodNodeToFindScope_ReturnsIt [

	self compile: 'm1'.
	
	self useFinderToFindScopeForAll: { self methodNodeScope }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test002_WhenNodeTheNodeToFindScopeIsInsideABlockNode_RetunsTheBlockNodeAsTheScopeNode [

	| expectedCommonScopeNode |

	self compile: 'm1 [ 1 ]'.
	expectedCommonScopeNode := self parseNodeAt: 4.
	
	self useFinderToFindScopeForAll: { self parseNodeIncluding: 6 }.
	
	self assertFinderFoundScopeFromNode: expectedCommonScopeNode
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test003_WhenNodesHaveDifferentScopes_ReturnsTheNearestScopeThatContainsAllTheScopes [

	| expectedScopeNode |

	self compile: 'm1 [ [ 1 ]. [ 1 ] ]'.
	expectedScopeNode := self parseNodeAt: 4.
	
	self useFinderToFindScopeForAll: { 
			self parseNodeIncluding: 8 .
			self parseNodeIncluding: 15 }.
	
	self assertFinderFoundScopeFromNode: expectedScopeNode
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test004_CanFindScopeOfFirstNodeApparition [

	| expectedScopeNode |

	self compile: 'm1 [ [ 1 ]. [ 1 ] ]'.
	expectedScopeNode := self parseNodeAt: 6.
	
	self useFinderToFindScopeForAll: { self parseNodeIncluding: 8 }.
	
	self assertFinderFoundScopeFromNode: expectedScopeNode
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test005_CanFindScopeOfANonFirstApparition [

	| expectedScopeNode |

	self compile: 'm1 [ [ 1 ]. [ 1 ] ]'.
	expectedScopeNode := self parseNodeAt: 13.
	
	self useFinderToFindScopeForAll: { self parseNodeIncluding: 15 }.
	
	self assertFinderFoundScopeFromNode: expectedScopeNode
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test006_CanFindScopeOfLatestNodesAfterSeveralScopes [

	self compile: 'm1 [ 1 ]. 1'.
	
	self useFinderToFindScopeForAll: { self parseNodeIncluding: 11 }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test007_CanFindTheScopeOfABlock [

	| expectedScopeNode |

	self compile: 'm1 [ [ 1 ] ]'.
	expectedScopeNode := self parseNodeAt: 4.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 6 }.
	
	self assertFinderFoundScopeFromNode: expectedScopeNode
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test008_WhenCannotFindANode_ThrowsException [

	| unreachableMethodNode |

	self compile: 'm1 '.
	classToRefactor compile: 'm2 [ ]'.
	unreachableMethodNode := (classToRefactor>>#m2) methodNode.
	
	self useFinderToFindScopeForAll: { 
		unreachableMethodNode -> (1 to: unreachableMethodNode sourceText size) }.
	
	self 
		should: [ finder commonScopeNode ] 
		raise: Error - MessageNotUnderstood 
		withMessageText: CommonScopeNodeFinder couldNotFindANodeErrorMessage
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test009_CanFindTheScopeOfAnAssignment [

	self compile: 'm1 | a | a := 1'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 11 }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test010_CanFindTheScopeOfATemporaryVariable [

	self compile: 'm1 | a | a := 1'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 10 }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test011_CanFindTheScopeOfAMessageBlock [

	| parseNodeToLookFor |

	self compile: 'm1 self value'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | parseNode isMessageNode ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test012_CanFindTheScopeOfABacktickNode [

	self compile: 'm1 `1`'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 4 }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test013_CanFindTheScopeOfABraceNode [

	self compile: 'm1 { 1 }'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 4 }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test014_CanFindTheScopeOfACascadeNode [

	| parseNodeToLookFor |

	self compile: 'm1 self value; value'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | parseNode isCascadeNode ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test015_CanFindTheScopeOfAInstanceVariableNode [

	classToRefactor addInstVarName: 'anInstanceVariable'.
	self compile: 'm1 anInstanceVariable'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 4 }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test016_CanFindTheScopeOfAMessageNodeInCascade [

	| parseNodeToLookFor |

	self compile: 'm1 self value; value'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | 
		parseNode isMessageNode and: [ parseNode isCascade ] ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test017_CanFindTheScopeOfReturnNode [

	self compile: 'm1 ^ 1'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 4 }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test018_CanFindTheScopeOfSelectorNode [

	| parseNodeToLookFor |

	self compile: 'm1 self value; value'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | 
		parseNode isMessageNode and: [ parseNode isCascade ]].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test019_CanFindTheScopeOfTemporariesDeclarationNode [

	| parseNodeToLookFor |

	self compile: 'm1 | a |'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | parseNode isTemporariesDeclaration ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test020_CanFindTheScopeOfTemporaryDeclarationNode [

	| parseNodeToLookFor |

	self compile: 'm1 | a |'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | parseNode isTemporaryDeclaration ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test021_CanFindTheScopeOfVariableNode [

	| parseNodeToLookFor |

	self compile: 'm1 ^ self value'.
	parseNodeToLookFor := self parseNodeToLookForUsing: [ :parseNode | parseNode isVariableNode ].
	
	self useFinderToFindScopeForAll: { parseNodeToLookFor }.
	
	self assertFinderFoundMethodNodeScope 
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test022_CanFindTheScopeOfLiteralVariableNode [

	self compile: 'm1 undeclared'.
	
	self useFinderToFindScopeForAll: { self parseNodeAt: 4 }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test023_TheScopeOfTheMethodNodeBlockIsTheMethodNode [

	self compile: 'm1 '.
	
	self useFinderToFindScopeForAll: { methodNode block -> (self methodNodeScope value)}.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test024_CanFindTheScopeOfAnEmptyBlock [

	self compile: 'm1 []'.
	
	self useFinderToFindScopeForAll: { self parseNodeOfInterval: (4 to: 5) }.
	
	self assertFinderFoundMethodNodeScope
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test025_CanFindTheCommonScopeOfNodesRegardlessOfOrder [

	| expectedScopeNode |

	self compile: 'm1
	[ 10. [ 10. ] ]'.
	expectedScopeNode := self parseNodeAt: 5.
	
	self useFinderToFindScopeForAll: { 
		self parseNodeOfInterval: (13 to: 14).
		self parseNodeOfInterval: (7 to: 8) . }.
	
	self assertFinderFoundScopeFromNode: expectedScopeNode
]

{ #category : 'tests' }
CommonScopeNodeFinderTest >> test026_WhenThereIsANodeToFindInTheMethodNode_ReturnsItAsTheScope [

	self compile: 'm1 [ 10 ]. 10'.
	
	self useFinderToFindScopeForAll: { 
		self parseNodeOfInterval: (12 to: 13).
		self parseNodeOfInterval: (6 to: 7) . }.
	
	self assertFinderFoundMethodNodeScope 
]

{ #category : 'excercising' }
CommonScopeNodeFinderTest >> useFinderToFindScopeForAll: aCollectionOfParseNodes [

	finder := CommonScopeNodeFinder forAll: aCollectionOfParseNodes in: methodNode .
	methodNode accept: finder.
]
