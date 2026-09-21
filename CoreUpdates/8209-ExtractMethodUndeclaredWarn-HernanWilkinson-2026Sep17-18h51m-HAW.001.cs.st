'From Cuis7.9 [latest update: #8184] on 19 September 2026 at 8:28:50 pm'!

!Parser class methodsFor: 'parsing' stamp: 'HAW 19/Sep/2026, 11:14:03 pm (UTC)'!
parseIgnoringUndeclared: sourceCode class: aClass noPattern: aBoolean

	^[ self parse: sourceCode class: aClass noPattern: aBoolean ]
		on: UndeclaredVariableWarning
		do: [ :warning | warning resume: false ]! !


!ExtractMethodCodeModifier methodsFor: 'private' stamp: 'HAW 19/Sep/2026, 11:14:14 pm (UTC)'!
parseIgnoringUndeclared: aSourceCode

	^Parser parseIgnoringUndeclared: aSourceCode class: methodNode methodClass noPattern: true ! !


!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 19/Sep/2026, 11:14:30 pm (UTC)'!
sourceCodeToExtractParsed

	"The piece is parsed on its own to look at its shape, where the arguments and temporaries of the
	method it comes from are not declared. Resuming with false keeps their names out of Undeclared."

	^Parser parseIgnoringUndeclared: sourceCodeToExtract class: self sourceClass noPattern: true ! !


!ExtractMethodReturnCodeModifier methodsFor: 'modify source code - private' stamp: 'HAW 19/Sep/2026, 11:09:23 pm (UTC)'!
extractedNodeToModifySourceCode

	"This is needed because if we extracted TempVars, in the new parsed code they will be 
	LiteralVariableNodes and won't be equivalent.
	Also, the names could be different (when the temp vars were renamed).
	"

	^ (self parseIgnoringUndeclared: (String streamContents: [ :stream | renamedVarsExtractedNode printOn: stream indent: 0]))
		block statements first "Must be only the return".! !

!ExtractMethodReturnCodeModifier methodsFor: 'modify source code - private' stamp: 'HAW 19/Sep/2026, 11:09:23 pm (UTC)'!
returnSourceRangesIn: aSourceCode

	| sourceCodeMethodNode extractedNodeSourceCode |

	sourceCodeMethodNode := self parseIgnoringUndeclared: aSourceCode.
	
	extractedNodeSourceCode := self extractedNodeToModifySourceCode.
	
	^ (sourceCodeMethodNode 
		completeSourceRangesOf: extractedNodeSourceCode
		ifAbsent: [ self completeSourceRangesOfExtractedNodeIn: sourceCodeMethodNode ])
		asSortedCollection: [ :leftRange :rightRange | leftRange first < rightRange first ].! !


!ExtractMethodTempVarCodeModifier methodsFor: 'modify source code - private' stamp: 'HAW 19/Sep/2026, 11:09:23 pm (UTC)'!
renameVariableIn: sourceCode

	| sourceCodeMethodNode firstTempVarAssignmentSourceRange variableSourceRangesAfterFirstAssignment |

	sourceCodeMethodNode := self parseIgnoringUndeclared: sourceCode.
	
	firstTempVarAssignmentSourceRange := self firstTempVarAssignmentSourceRangeIn: sourceCodeMethodNode.
	
	variableSourceRangesAfterFirstAssignment := self 
		variableSourceRangesAfterFirstAssignmentIn: sourceCodeMethodNode 
		using: firstTempVarAssignmentSourceRange.
		
	^ sourceCode 
		copyReplacing: variableSourceRangesAfterFirstAssignment
		with: self variableName ! !


!ExtractMethodNewMethodSourceCode methodsFor: 'private - source code' stamp: 'HAW 17/Sep/2026, 9:54:23 pm (UTC)'!
returnCharacterIfNeeded

	^ (self needsExplicitReturn: self sourceCodeToExtractParsed) ifTrue: [ '' ] ifFalse: [ '^ ' ]! !

!methodRemoval: ExtractMethodCodeModifier #parsed: stamp: 'HAW 19/Sep/2026, 11:09:23 pm (UTC)'!
ExtractMethodCodeModifier removeSelector: #parsed:!

!ExtractMethodReturnCodeModifier reorganize!
('private' detectParseNodeAt:satisfies:ifFoundDo:ifAbsent: existsBlockNodeThatContainsExtractedNode isBlockNodeIncludedInTheExtractedInterval:)
('modify source code' modifyExtractedSourceCode: shouldModifyExtractedSourceCode)
('modify source code - private' apparitionIndex extractedNodeToModifySourceCode returnCodeToReplace returnSourceRangesIn:)
('initialization' initializeFor:from:of:to:sourceRange:renamedVarsNode:)
('testing' is: isNestedReturn isUnnestedReturn)
('accessing' firstSourceCodeIndex)
!


!ExtractMethodTempVarCodeModifier reorganize!
('testing' anyParseNodeOfRangeATemporaryDeclaration: hasExtractedAssignment hasExtractedDeclaration hasExtractedTheCompleteVariableContext is: isAssignedToAPartiallyCascadeCode isDeclaredBy: isTempVarEquivalentTo: isTheOnlyDeclaredVariableIn:within: isUsedAsAnArgument isUsedOutsideExtractedInterval shouldBeAssigned shouldBeDeclared shouldBeReturned shouldDeclarationBeRemoved shouldRenameVariable)
('accessing' firstSourceCodeIndex methodOrBlockNodeDeclaringTemporary newVariableName realVariableName variableName)
('modify source code' modifyExtractedSourceCode: shouldModifyExtractedSourceCode)
('printing' assignVariableUsing: printOn: writeAssignationOn: writeNameOn: writeNewNameDeclarationOn: writeNewNameOn: writeRealNameOn:)
('remove - private' intervalToRemove: intervalToRemove:withLastDeclaredVariableIn: intervalToRemove:withMultipleDeclarationsIn: intervalToRemove:withOneDeclarationIn: intervalToRemoveDeclarationIn:)
('modify source code - private' firstTempVarAssignmentSourceRangeIn: renameNodeToNewVariableName: renameVariableIn: variableSourceRangesAfterFirstAssignmentIn:using:)
('private' completeTempVarAssignmentsDo: isNodePartiallyExtracted:)
('remove' removeTemporaryDeclarationIn:)
!

