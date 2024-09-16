'From Cuis7.1 [latest update: #6723] on 16 September 2024 at 10:17:38 am'!

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/16/2024 10:17:00'!
parseCascadeAndChain

	| beforeKeywordPos |
	beforeKeywordPos := currentTokenSourcePosition.
	self parseKeyword.

	"At least one message send before cascade and chain"
	beforeKeywordPos =​ currentTokenSourcePosition ifTrue: [ ^self ].

	currentTokenFirst = $; ifTrue:
		[
			self scanPast: #cascadeSeparator.
			^self parseCascadeAndChain
		].
	currentTokenFirst = $: ifTrue:
		[
			self scanPast: #chainSeparator.

			"These lines implement double colon chains"
			currentTokenFirst = $: ifFalse: [^self].
			self scanPast: #chainSeparator.
			
			^self parseCascadeAndChain
		].! !

