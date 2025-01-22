'From Cuis7.3 [latest update: #6996] on 22 January 2025 at 8:26:14 am'!

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 1/22/2025 08:25:53'!
browseEqEqSentToNumericConstants
	"
	Smalltalk browseEqEqSentToNumericConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isLiteralNumber or: [ node arguments first isLiteralNumber ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 1/22/2025 08:26:02'!
browseEqSentToCharacterConstants
	"
	Smalltalk browseEqSentToCharacterConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#= #~= ) identityIncludes: node selector key) and: [
					node receiver isLiteralCharacter or: [ node arguments first isLiteralCharacter ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 1/22/2025 08:26:06'!
browseEqSentToNumericConstants
	"
	Smalltalk browseEqSentToNumericConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#= #~= ) identityIncludes: node selector key) and: [
					node receiver isLiteralNumber or: [ node arguments first isLiteralNumber ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 1/22/2025 08:26:10'!
browseEqSentToSmallIntegerConstants
	"
	Smalltalk browseEqSentToSmallIntegerConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#= #~= ) identityIncludes: node selector key) and: [
					node receiver isLiteralSmallInteger or: [ node arguments first isLiteralSmallInteger ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].! !


!ParseNode methodsFor: 'testing' stamp: 'jmv 1/22/2025 08:16:27'!
isLiteralCharacter  "Overridden in LiteralNode"
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'jmv 1/22/2025 08:17:12'!
isLiteralNumber  "Overridden in LiteralNode"
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'jmv 1/22/2025 08:21:06'!
isLiteralSmallInteger  "Overridden in LiteralNode"
	^false! !


!LiteralNode methodsFor: 'testing' stamp: 'jmv 1/22/2025 08:16:30'!
isLiteralCharacter
	^ key isCharacter! !

!LiteralNode methodsFor: 'testing' stamp: 'jmv 1/22/2025 08:17:15'!
isLiteralNumber
	^ key isNumber! !

!LiteralNode methodsFor: 'testing' stamp: 'jmv 1/22/2025 08:22:59'!
isLiteralSmallInteger
	^ key isNumber and: [ key class == SmallInteger ]! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 1/22/2025 08:25:49'!
browseEqEqSentToCharacterConstants
	"
	Smalltalk browseEqEqSentToCharacterConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isLiteralCharacter or: [ node arguments first isLiteralCharacter ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 1/22/2025 08:25:58'!
browseEqEqSentToSmallIntegerConstants
	"
	Smalltalk browseEqEqSentToSmallIntegerConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isLiteralSmallInteger or: [ node arguments first isLiteralSmallInteger ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].! !


!ParseNode methodsFor: 'testing' stamp: 'jmv 1/22/2025 08:16:50'!
isConstantCharacter
	"Compatibility"

	^self isLiteralCharacter! !

!ParseNode methodsFor: 'testing' stamp: 'jmv 1/22/2025 08:17:32'!
isConstantNumber
	"Compatibility"

	^self isLiteralNumber! !

!methodRemoval: LiteralNode #isConstantCharacter stamp: 'jmv 1/22/2025 08:16:56'!
LiteralNode removeSelector: #isConstantCharacter!
!methodRemoval: LiteralNode #isConstantNumber stamp: 'jmv 1/22/2025 08:17:36'!
LiteralNode removeSelector: #isConstantNumber!
