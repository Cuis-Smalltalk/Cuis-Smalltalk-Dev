'From Cuis 5.0 of 7 November 2016 [latest update: #3521] on 13 December 2018 at 7:37:45 pm'!

!Parser methodsFor: 'scanning' stamp: 'jmv 12/13/2018 19:37:11'!
transformVerticalBarAndUpArrowIntoABinarySelector
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a serie of immediately following vertical bars, up arrows and a binary selector.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0.
	Colon $: can be used as binary, but '::' means Chain."
	| toMakeBinary |
	toMakeBinary _ #(verticalBar upArrow colon).
	"Special case: '::' is not a binary selector but the Chain operator"
	(hereType = #colon and: [tokenType = #colon]) ifTrue: [^ self ].
	(toMakeBinary identityIncludes: hereType) ifFalse: [
		^ self ].
	here := here asString.
	hereType := #binary.
	[(toMakeBinary identityIncludes: tokenType) and: [hereMark + here size = mark]]
		whileTrue: [
			here := here , token asString.
			hereEnd := hereEnd + 1.
			self scanToken].
	(tokenType == #binary and: [hereMark + here size = mark])
		ifTrue: [
			here := here asString , token.
			hereType := #binary.
			hereEnd := hereEnd + token size.
			self scanToken].! !


!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 12/13/2018 19:37:20'!
isBinary
	| c |
	(currentToken isNil or: [self isName or: [self isKeyword]]) 
		ifTrue: [^false].
	"Special case: '::' is not a binary selector but the Chain operator"
	(sourcePosition - currentTokenSourcePosition = 1 and: [
		(source at: currentTokenSourcePosition ifAbsent: nil) = $: and: [		
			(source at: sourcePosition ifAbsent: nil) = $: ]])
				ifTrue: [^ false ].
	1 to: currentToken size do: [ :i |
		c := currentToken at: i.
		((self isBinarySelectorCharacter: c) or: [c == $:])
			ifFalse: [^false]].
	^true! !

