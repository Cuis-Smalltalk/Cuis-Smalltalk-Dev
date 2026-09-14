'From Cuis7.9 [latest update: #8170] on 12 September 2026 at 7:05:25 pm'!
!classDefinition: #ExpressionReplacement category: #'Tools-Refactoring'!
Object subclass: #ExpressionReplacement
	instanceVariableNames: 'methodNode interval sourceCode precedence'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExpressionReplacement commentStamp: '<historical>' prior: 0!
I replace an expression written in a method with another expression, keeping the parentheses right.

I am given the method node, the range of the expression to replace, the source code of the new expression and its precedence: 1 unary, 2 binary, 3 keyword, 4 for an assignment or a cascade, which need parentheses inside any message. See ParseNode class precedence.

I replace the range together with every pair of parentheses around it, and write the new expression with exactly one pair when the parent node needs it: as the receiver of a message of lower precedence, as the receiver of a keyword message when it is a keyword message too, or as an argument of a message of the same or lower precedence when it is not unary. The parent decides, see ParseNode>>requiresParenthesesToReplace:withAnExpressionOfPrecedence:.

I am meant to be the one place where a refactoring decides about parentheses when it rewrites a send: extract method uses me, and remove parameter, add parameter, inline method and inline temporary should.!

!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom sendCollaboration extractMethodProgrammer newCompiledMethod parameterizableNodes expressionReplacement '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ParseNode methodsFor: 'testing' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
requiresParenthesesToReplace: aChildNode withAnExpressionOfPrecedence: aPrecedence

	^false! !


!ParseNode class methodsFor: 'precedence' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
assignmentAndCascadePrecedence

	^4! !

!ParseNode class methodsFor: 'precedence' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
binaryPrecedence

	^2! !

!ParseNode class methodsFor: 'precedence' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
keywordPrecedence

	^3! !

!ParseNode class methodsFor: 'precedence' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
unaryPrecedence

	^1! !


!CascadeNode methodsFor: 'testing' stamp: 'HAW 12/Sep/2026, 9:09:02 pm (UTC)'!
requiresParenthesesToReplace: aChildNode withAnExpressionOfPrecedence: aPrecedence

	^messages first
		requiresParenthesesOnTheReceiverForAnExpressionOfPrecedence: aPrecedence! !


!MessageNode methodsFor: 'testing' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
requiresParenthesesOnAnArgumentForAnExpressionOfPrecedence: aPrecedence

	^aPrecedence >= precedence
		and: [ aPrecedence > self class unaryPrecedence ]! !

!MessageNode methodsFor: 'testing' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
requiresParenthesesOnTheReceiverForAnExpressionOfPrecedence: aPrecedence

	^aPrecedence > precedence
		or: [ self isKeywordMessageSend and: [ aPrecedence = precedence ] ]! !

!MessageNode methodsFor: 'testing' stamp: 'HAW 12/Sep/2026, 10:01:08 pm (UTC)'!
requiresParenthesesToReplace: aChildNode withAnExpressionOfPrecedence: aPrecedence

	^receiver == aChildNode
		ifTrue: [ self requiresParenthesesOnTheReceiverForAnExpressionOfPrecedence: aPrecedence ]
		ifFalse: [ self requiresParenthesesOnAnArgumentForAnExpressionOfPrecedence: aPrecedence ]! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 12/Sep/2026, 6:12:20 pm (UTC)'!
expandOverParenthesesOn: aSourceCode

	^self
		expandOn: aSourceCode
		whileEnclosedBy: [ :aFirstCharacter :aLastCharacter |
			aFirstCharacter = $( and: [ aLastCharacter = $) ] ]! !

!SourceCodeInterval methodsFor: 'private - source code' stamp: 'HAW 12/Sep/2026, 6:12:20 pm (UTC)'!
expandOn: aSourceCode whileEnclosedBy: anEnclosingCharactersCondition

	| firstCharAndIndex lastCharAndIndex |

	firstCharAndIndex := self skipSeparatorsFrom: start by: -1 in: aSourceCode.
	lastCharAndIndex := self skipSeparatorsFrom: stop by: 1 in: aSourceCode.
	(anEnclosingCharactersCondition
		value: firstCharAndIndex key
		value: lastCharAndIndex key) ifFalse: [ ^self ].

	^(self class from: firstCharAndIndex value to: lastCharAndIndex value)
		expandOn: aSourceCode
		whileEnclosedBy: anEnclosingCharactersCondition! !


!ExpressionReplacement methodsFor: 'initialization' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
initializeOf: anInterval in: aMethodNode by: aSourceCode withPrecedence: aPrecedence

	interval := anInterval.
	methodNode := aMethodNode.
	sourceCode := aSourceCode.
	precedence := aPrecedence! !

!ExpressionReplacement methodsFor: 'evaluating' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
value

	| intervalToReplace |

	intervalToReplace := self intervalToReplace.

	^methodNode sourceText
		copyReplaceFrom: intervalToReplace first
		to: intervalToReplace last
		with: self sourceCodeToWrite! !

!ExpressionReplacement methodsFor: 'accessing' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
intervalToReplace

	^interval expandOverParenthesesOn: methodNode sourceText! !

!ExpressionReplacement methodsFor: 'accessing' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
sourceCodeToWrite

	^self requiresParentheses
		ifTrue: [ '(' , sourceCode , ')' ]
		ifFalse: [ sourceCode ]! !

!ExpressionReplacement methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
enclosingNodes

	| startAncestors endAncestors |

	startAncestors := methodNode parseNodesPathAt: interval first ifAbsent: [ ^#() ].
	endAncestors := methodNode parseNodesPathAt: interval last ifAbsent: [ ^#() ].

	^startAncestors intersection: endAncestors! !

!ExpressionReplacement methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 9:04:43 pm (UTC)'!
isAWholeExpression: aSourceRange

	| sourceText |

	sourceText := methodNode sourceText.

	^(aSourceRange trimToMatchExpressionOn: sourceText)
		= (interval trimToMatchExpressionOn: sourceText)! !

!ExpressionReplacement methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
parentIn: anEnclosingNodes requiresParenthesesToReplace: aReplacedNode

	| parentNode |

	parentNode := (anEnclosingNodes at: 2 ifAbsent: [ ^false ]) key.

	^parentNode
		requiresParenthesesToReplace: aReplacedNode
		withAnExpressionOfPrecedence: precedence! !

!ExpressionReplacement methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
requiresParentheses

	| enclosingNodes replacedNodeAndRange |

	enclosingNodes := self enclosingNodes.
	replacedNodeAndRange := enclosingNodes at: 1 ifAbsent: [ ^false ].
	(self isAWholeExpression: replacedNodeAndRange value) ifFalse: [ ^false ].

	^self
		parentIn: enclosingNodes
		requiresParenthesesToReplace: replacedNodeAndRange key! !


!ExpressionReplacement class methodsFor: 'instance creation' stamp: 'HAW 12/Sep/2026, 9:03:45 pm (UTC)'!
of: anInterval in: aMethodNode by: aSourceCode withPrecedence: aPrecedence

	^self new
		initializeOf: anInterval
		in: aMethodNode
		by: aSourceCode
		withPrecedence: aPrecedence! !


!ExtractMethodProgrammer methodsFor: 'send collaboration' stamp: 'HAW 12/Sep/2026, 9:04:20 pm (UTC)'!
sendCollaborationPrecedence

	^self shouldAssignSendCollaborationToSingleVariable
		ifTrue: [ ParseNode assignmentAndCascadePrecedence ]
		ifFalse: [ newMessage selector precedence ]! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 12/Sep/2026, 6:12:20 pm (UTC)'!
expandToMatchExpressionOn: aSourceCode
	"takes a source code and if the source range references an expression
	that can be expanded and still is valid, it returns the source range
	'grown'. Examples: (*3 + 4*) to *(3 + 4)*; `*3 + 4*` to *`3 + 4`*"

	^self
		expandOn: aSourceCode
		whileEnclosedBy: [ :aFirstCharacter :aLastCharacter |
			self canBeExpandedStartingWith: aFirstCharacter endingWith: aLastCharacter ]! !


!ExtractMethodProgrammer methodsFor: 'send collaboration' stamp: 'HAW 12/Sep/2026, 9:04:20 pm (UTC)'!
sendCollaborationString

	| collaborationString |

	collaborationString := 'self ' , self messageFullName.
	
	self shouldAddAssignmentBlock 
		ifTrue: [ collaborationString := self sendCollaborationStringForMultipleVariablesTo: collaborationString ].
	
	self shouldAddReturningBlock
		ifTrue: [ collaborationString := self sendCollaborationStringUsingReturnBlockTo: collaborationString ].
		
	self shouldAssingSendCollaborationToSingleVariableIfTrueDo:
		[ :tempVar | collaborationString := tempVar, ' := ', collaborationString ].
		
	self shouldAddReturn 
		ifTrue: [ ^ '^ ' , collaborationString ].
	
	^ collaborationString! !


!ExtractMethodReplacement methodsFor: 'applying' stamp: 'HAW 12/Sep/2026, 9:04:20 pm (UTC)'!
apply

	| updatedSourceCode |

	self initializeSendCollaboration.
	expressionReplacement := ExpressionReplacement
		of: intervalToExtract
		in: methodToExtractFrom notOptimizedMethodNode
		by: sendCollaboration
		withPrecedence: extractMethodProgrammer sendCollaborationPrecedence.
	updatedSourceCode := extractMethodProgrammer
		removeTemporaryDeclarationsIfNecessaryIn: expressionReplacement value.

	self sourceClass
		compile: updatedSourceCode
		classified: methodToExtractFrom category! !

!ExtractMethodReplacement methodsFor: 'adjustment' stamp: 'HAW 12/Sep/2026, 9:04:20 pm (UTC)'!
adjustmentForNextReplacement

	^expressionReplacement sourceCodeToWrite size - expressionReplacement intervalToReplace size! !

!methodRemoval: ExtractMethodReplacement #updatedSourceCodeOfExistingMethod stamp: 'HAW 12/Sep/2026, 9:57:11 pm (UTC)'!
ExtractMethodReplacement removeSelector: #updatedSourceCodeOfExistingMethod!
!methodRemoval: ExtractMethodReplacement #shouldBeEnclosedWithParens stamp: 'HAW 12/Sep/2026, 9:57:11 pm (UTC)'!
ExtractMethodReplacement removeSelector: #shouldBeEnclosedWithParens!
!methodRemoval: ExtractMethodProgrammer #shouldBeEnclosedWithParens stamp: 'HAW 12/Sep/2026, 9:57:11 pm (UTC)'!
ExtractMethodProgrammer removeSelector: #shouldBeEnclosedWithParens!
!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom sendCollaboration extractMethodProgrammer newCompiledMethod parameterizableNodes expressionReplacement'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
