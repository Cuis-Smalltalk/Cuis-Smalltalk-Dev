'From Cuis 6.0 [latest update: #5190] on 23 May 2022 at 12:01:42 pm'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/21/2022 22:44:31'!
browseEqEqSentToCharacterConstants
	"
	Smalltalk browseEqEqSentToCharacterConstants
	"
	| hasMatch visitor |
	hasMatch _ false.
	visitor _ ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isConstantCharacter or: [ node arguments first isConstantCharacter ]]]) ifTrue: [
						hasMatch _ true ]]
		select: [ :node |
			hasMatch not ].
	Smalltalk browseAllSelect: [ :method |
		hasMatch _ false.
		method decompile accept: visitor.
		hasMatch ].! !


!ParseNode methodsFor: 'testing' stamp: 'jmv 5/21/2022 22:44:16'!
isConstantCharacter  "Overridden in LiteralNode"
	^false! !


!LiteralNode methodsFor: 'testing' stamp: 'jmv 5/21/2022 22:44:10'!
isConstantCharacter
	^ key isCharacterLike! !

