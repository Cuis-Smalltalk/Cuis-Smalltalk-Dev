'From Cuis7.9 [latest update: #8184] on 28 September 2026 at 7:00:21 pm'!
!classDefinition: #InlineMethod category: #'Tools-Refactoring'!
ScopedRefactoring subclass: #InlineMethod
	instanceVariableNames: 'methodToInline messageSendsToInline updatedSendersCode methodNodeToInline replacementsByMessageSend temporariesDeclarationsByNode temporariesToDeclareByInsertionPoint implementorCompleteSourceRanges removeMethod temporariesOfBlocksByScope '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!VariableNode methodsFor: 'accessing' stamp: 'HAW 28/Sep/2026, 9:40:46 pm (UTC)'!
nameAvoiding: aCollectionOfNames

	| freshName suffix |

	freshName := self name.
	suffix := 1.
	[ aCollectionOfNames includes: freshName ] whileTrue: [
		freshName := self name , suffix asString.
		suffix := suffix + 1 ].

	^freshName! !


!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 28/Sep/2026, 9:40:25 pm (UTC)'!
renameTemporaries: aCollectionOfTemporaries ofBlockIn: aSetOfDeclaredNames

	| namesOfBlocks namesInScope |

	namesOfBlocks := temporariesOfBlocksByScope
		at: aSetOfDeclaredNames
		ifAbsentPut: [ Set new ].
	namesInScope := aSetOfDeclaredNames difference: namesOfBlocks.

	^aCollectionOfTemporaries collect: [ :aTemporary | | name |
		name := aTemporary nameAvoiding: namesInScope.
		aSetOfDeclaredNames add: name.
		aTemporary -> (namesOfBlocks add: name) ]! !

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 28/Sep/2026, 8:27:24 pm (UTC)'!
replace: aCollectionOfRenamedTemporaries inStatementsInlinedIn: aMessageNodeReference

	| replacements |

	replacements := self replacementsMapForMessageSend: aMessageNodeReference.
	aCollectionOfRenamedTemporaries do: [ :aTemporaryAndNewName |
		(self implementorRangesOf: aTemporaryAndNewName key) do: [ :aRange |
			replacements at: aRange put: aTemporaryAndNewName value ] ]! !

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 28/Sep/2026, 8:27:24 pm (UTC)'!
temporariesDeclarationOfBlockInlinedIn: aMessageNodeReference

	^(self temporariesOfBlockInlinedIn: aMessageNodeReference)
		ifEmpty: [ '' ]
		ifNotEmpty: [ :temporaryNames | '| ' , (' ' join: temporaryNames) , ' | ' ]! !

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 28/Sep/2026, 9:41:48 pm (UTC)'!
temporariesOfBlockInlinedIn: aMessageNodeReference

	| temporaries declaredNames renamedTemporaries |

	temporaries := methodNodeToInline temporariesDeclaration allDeclaredVariableNodes.
	declaredNames := self
		declaredTemporariesFor: (self enclosingBlockOf: aMessageNodeReference)
		ofSender: aMessageNodeReference compiledMethod.
	renamedTemporaries := self
		renameTemporaries: temporaries
		ofBlockIn: declaredNames.
	self replace: renamedTemporaries inStatementsInlinedIn: aMessageNodeReference.

	^renamedTemporaries collect: [ :aTemporaryAndNewName |
		aTemporaryAndNewName value ]! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 28/Sep/2026, 8:27:24 pm (UTC)'!
implementorRangesOf: aParseNode

	^self findRangesOf: aParseNode in: implementorCompleteSourceRanges! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 28/Sep/2026, 8:26:44 pm (UTC)'!
sourceCodeOf: aCollectionOfStatements replacedFor: aMessageNodeReference

	| replacements senderIndentation |

	replacements := self replacementsMapForMessageSend: aMessageNodeReference.
	senderIndentation := aMessageNodeReference lineIndentation.

	^aCollectionOfStatements collect: [ :aStatementToInline |
		self
			sourceCodeOfImplementorStatement: aStatementToInline
			withReplacements: replacements
			removingReturn: true
			indentedAs: senderIndentation ]! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 28/Sep/2026, 8:28:18 pm (UTC)'!
sourceCodeOfBlockEvaluating: aCollectionOfStatements inlinedIn: aMessageNodeReference

	| declaration statements |

	self
		calculateReplacementsFrom: aMessageNodeReference
		usingRanges: aMessageNodeReference methodNode completeSourceRanges.
	declaration := self
		temporariesDeclarationOfBlockInlinedIn: aMessageNodeReference.
	statements := self
		sourceCodeOf: aCollectionOfStatements
		replacedFor: aMessageNodeReference.

	^'[ ' , declaration ,
		((' ' join: statements) withoutPossibleSuffix: '.') , ' ] value'! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 8:26:44 pm (UTC)'!
replace: aMessageNodeReference usedInStatementAt: aStatementSourceRange byHoisting: aCollectionOfStatements

	| inlinedStatements |

	inlinedStatements := self
		sourceCodeOf: aCollectionOfStatements
		inlinedIn: aMessageNodeReference.

	self
		replace: aMessageNodeReference
		usedInStatementAt: aStatementSourceRange
		by: inlinedStatements last
		withPrecedence: aCollectionOfStatements last expr expressionPrecedence.
	self
		insert: inlinedStatements allButLast
		before: aStatementSourceRange
		in: aMessageNodeReference compiledMethod! !


!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 28/Sep/2026, 8:26:44 pm (UTC)'!
declareTemporaries: temporariesToDeclare inBlockOf: aMessageNodeReference

	| renamedTemporaries |

	renamedTemporaries := self
		declareTemporaries: temporariesToDeclare
		in: (self enclosingBlockOf: aMessageNodeReference)
		of: aMessageNodeReference compiledMethod.

	self replace: renamedTemporaries inStatementsInlinedIn: aMessageNodeReference! !

!InlineMethod methodsFor: 'temporaries declaration - private' stamp: 'HAW 28/Sep/2026, 9:41:24 pm (UTC)'!
renameTemporariesIfNeeded: temporariesToDeclare accordingToNode: aMethodOrBlockNode of: aSenderMethod

	"If a temporary to declare already exists in the scope, a new fresh name
	will be found for it. The method returns an OrderedCollection of (variableNode -> newVariableName)
	replacements. If the variable does not need a rename, it will still be added to the collection.
	This method also adds the new name to the map that tracks temporaries
	names in scope for the requested method or block node across the whole refactoring - Fernando"

	| declaredNames renamedTemporaries |

	declaredNames := self
		declaredTemporariesFor: aMethodOrBlockNode
		ofSender: aSenderMethod.
	renamedTemporaries := temporariesToDeclare collect: [ :aTemporary |
		aTemporary -> (declaredNames add: (aTemporary nameAvoiding: declaredNames)) ].

	^renamedTemporaries asOrderedCollection! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 28/Sep/2026, 8:26:44 pm (UTC)'!
sourceCodeOf: aCollectionOfStatements inlinedIn: aMessageNodeReference

	self
		calculateReplacementsFrom: aMessageNodeReference
		usingRanges: aMessageNodeReference methodNode completeSourceRanges.
	self declareTemporariesIfNeededFor: aMessageNodeReference.

	^self sourceCodeOf: aCollectionOfStatements replacedFor: aMessageNodeReference! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 8:26:44 pm (UTC)'!
inlineStatements: aCollectionOfStatements ofMessageWithReturnValue: aMessageNodeReference usedInStatementAt: aStatementSourceRange

	| evaluatesInBlock |

	evaluatesInBlock := self
		shouldEvaluateInBlock: aCollectionOfStatements
		at: aStatementSourceRange
		of: aMessageNodeReference.

	evaluatesInBlock
		ifTrue: [
			self
				replace: aMessageNodeReference
				usedInStatementAt: aStatementSourceRange
				byBlockEvaluating: aCollectionOfStatements ]
		ifFalse: [
			self
				replace: aMessageNodeReference
				usedInStatementAt: aStatementSourceRange
				byHoisting: aCollectionOfStatements ]! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 8:26:44 pm (UTC)'!
replace: aMessageNodeReference usedInStatementAt: aStatementSourceRange byBlockEvaluating: aCollectionOfStatements

	self
		replace: aMessageNodeReference
		usedInStatementAt: aStatementSourceRange
		by: (self
			sourceCodeOfBlockEvaluating: aCollectionOfStatements
			inlinedIn: aMessageNodeReference)
		withPrecedence: ParseNode unaryPrecedence! !

!InlineMethod methodsFor: 'initialization' stamp: 'HAW 28/Sep/2026, 9:40:25 pm (UTC)'!
methodToInline: aCompiledMethod usages: aCollectionOfMesageNodeReferences removingMethod: removingMethod

	methodToInline := aCompiledMethod.
	messageSendsToInline := aCollectionOfMesageNodeReferences.
	replacementsByMessageSend := Dictionary new.
	temporariesDeclarationsByNode := Dictionary new.
	temporariesToDeclareByInsertionPoint := Dictionary new.
	temporariesOfBlocksByScope := IdentityDictionary new.
	updatedSendersCode := Dictionary new.
	methodNodeToInline := methodToInline notOptimizedMethodNode.
	implementorCompleteSourceRanges := methodNodeToInline completeSourceRanges.
	removeMethod := removingMethod.! !

!methodRemoval: InlineMethod #blockEvaluating:declaring: stamp: 'HAW 28/Sep/2026, 8:27:24 pm (UTC)'!
InlineMethod removeSelector: #blockEvaluating:declaring:!
!methodRemoval: InlineMethod #replace:usedInStatementAt:by:lastWithPrecedence: stamp: 'HAW 28/Sep/2026, 8:26:44 pm (UTC)'!
InlineMethod removeSelector: #replace:usedInStatementAt:by:lastWithPrecedence:!
!classDefinition: #InlineMethod category: #'Tools-Refactoring'!
ScopedRefactoring subclass: #InlineMethod
	instanceVariableNames: 'methodToInline messageSendsToInline updatedSendersCode methodNodeToInline replacementsByMessageSend temporariesDeclarationsByNode temporariesToDeclareByInsertionPoint implementorCompleteSourceRanges removeMethod temporariesOfBlocksByScope'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
