'From Cuis 6.0 [latest update: #5139] on 6 May 2022 at 10:34:55 am'!

!Parser methodsFor: 'scanning' stamp: 'jmv 5/6/2022 10:34:02'!
transformVerticalBarAndUpArrowIntoABinarySelector
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a serie of immediately following vertical bars, up arrows and a binary selector.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0.
	Colon $: can be used as binary, but '::' means Chain."

	| toMakeBinary |
	"Special case: '::' is not a binary selector but the Chain operator"
	(hereType = #colon and: [tokenType = #colon]) ifTrue: [^ self ].

	"Note: Also include #binary, to allow stuff like #+| where a special character comes after after a regular binary operator character. "
	toMakeBinary _ #(binary verticalBar upArrow colon).
	(toMakeBinary identityIncludes: hereType) ifFalse: [
		^ self ].
	here := here asPlainString.
	hereType := #binary.
	[(toMakeBinary identityIncludes: tokenType) and: [hereMark + here size = mark]]
		whileTrue: [
			here := here , token asPlainString.
			hereEnd := hereEnd + 1.
			self scanToken ].
	(tokenType == #binary and: [hereMark + here size = mark])
		ifTrue: [
			here := here asPlainString , token.
			hereType := #binary.
			hereEnd := hereEnd + token size.
			self scanToken ].! !

