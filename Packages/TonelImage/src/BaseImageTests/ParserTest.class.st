Class {
	#name : 'ParserTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'temporary variables tests' }
ParserTest >> testItGeneratesATempDeclarationNodeForEachTempInABlock [

	| methodNode blockNode tempsDeclarationNode allTempDeclarationNodes |
	methodNode := self class methodNodeFor: 'm1 [ | a b c | 42 ]'.
	blockNode := methodNode block statements first.
	tempsDeclarationNode := blockNode temporariesDeclaration.
	allTempDeclarationNodes := tempsDeclarationNode temporaryDeclarationNodes.
	
	self
		assert: allTempDeclarationNodes size equals: 3;
		assert: allTempDeclarationNodes first variableName equals: 'a';
		assert: allTempDeclarationNodes second variableName equals: 'b';
		assert: allTempDeclarationNodes third variableName equals: 'c'.
]

{ #category : 'temporary variables tests' }
ParserTest >> testItGeneratesATempDeclarationNodeForEachTempInAMethod [

	| methodNode tempsDeclarationNode allTempDeclarationNodes |
	methodNode := self class methodNodeFor: 'm1 | a b c | ^a + b + c'.
	tempsDeclarationNode := methodNode temporariesDeclaration.
	allTempDeclarationNodes := tempsDeclarationNode temporaryDeclarationNodes.
	
	self
		assert: allTempDeclarationNodes size equals: 3;
		assert: allTempDeclarationNodes first variableName equals: 'a';
		assert: allTempDeclarationNodes second variableName equals: 'b';
		assert: allTempDeclarationNodes third variableName equals: 'c'.
]

{ #category : 'temporary variables tests' }
ParserTest >> testItGeneratesAnEmptyTempsDeclarationNodeForAMethodWithoutTemps [

	| methodNode tempsDeclarationNode |
	methodNode := self class methodNodeFor: 'm1 ^42'.
	tempsDeclarationNode := methodNode temporariesDeclaration.
	
	self assert: tempsDeclarationNode allDeclaredVariableNodes isEmpty.
]

{ #category : 'test' }
ParserTest >> testMulticharBinarySelectors [
	| result expression |
	self assert: (Compiler evaluate: '3+4') = 7.

	expression _ '1@-2'.
	Preferences
		withTemporaryValue: #st80
		of: #atMinusDigitMeaning
		do: [ result _ Compiler evaluate: expression].
	self assert: result x = 1.
	self assert: result y = 2 negated.
	Preferences
		withTemporaryValue: #disabled
		of: #atMinusDigitMeaning
		do: [ result _ Compiler evaluate: expression].
	self assert: result = nil.
	Preferences
		withTemporaryValue: #ansiSmalltalk
		of: #atMinusDigitMeaning
		do: [ result _ [Compiler evaluate: expression] on: MessageNotUnderstood do: [#dnu]].
	self assert: result = #dnu.

	expression _ '1@-2.'.

	Preferences
		withTemporaryValue: #st80
		of: #atMinusDigitMeaning
		do: [ result _ [Compiler evaluate: expression] on: MessageNotUnderstood do: [#dnu]].
	self assert: result isPoint.
	self assert: result x = 1.
	self assert: result y = 2 negated.
	Preferences
		withTemporaryValue: #disabled
		of: #atMinusDigitMeaning
		do: [ result _ [Compiler evaluate: expression] on: MessageNotUnderstood do: [#dnu]].
	self assert: result = nil.
	Preferences
		withTemporaryValue: #ansiSmalltalk
		of: #atMinusDigitMeaning
		do: [ result _ [Compiler evaluate: expression] on: MessageNotUnderstood do: [#dnu]].
	self assert: result = #dnu.
]

{ #category : 'test' }
ParserTest >> testOptimizedNodesWithNilDoNotFailsWhenAskForEquivalentTo [

	| methodNode |
	
	methodNode := self class methodNodeFor: 
	'example
	
	2 to: 4 do: [:i | self at: i].
	^ self'.
	
	self 
		shouldnt: [ methodNode equivalentTo: methodNode ] 
		raise: Error
	
]

{ #category : 'test' }
ParserTest >> testRangesAreOkWhenReturningAVariableWithoutSpaceAfterThat [

	| methodNode ranges |
	
	"See Parser>>#variable"
	
	methodNode := self class methodNodeFor: 'm1 |v| ^v'.
	
	ranges := methodNode rangeForNode: methodNode tempNodes first ifAbsent: [ self fail ].
	
	self assert: ranges size equals: 2.
	self assert: ranges includes: (5 to: 5).
	self assert: ranges includes: (9 to: 9).
	
	
	
	
]
