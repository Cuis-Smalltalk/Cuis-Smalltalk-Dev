'From Cuis 6.0 [latest update: #5349] on 4 July 2022 at 10:22:28 am'!

!Parser methodsFor: 'error handling' stamp: 'jmv 7/4/2022 10:12:51'!
notify: string at: location
	| adjustedLocation |
	adjustedLocation _ location - self sourceDelta.
	requestor
		ifNil: [
			(encoder == self or: [encoder isNil]) ifTrue: [
				^ self fail ]. "failure setting up syntax error"
			SyntaxErrorNotification
				inClass: encoder classEncoding
				category: category
				withCode: 
					(source contents
						copyReplaceFrom: adjustedLocation
						to: adjustedLocation - 1
						with: string , ' ->')
				doitFlag: doitFlag
				errorMessage: string
				location: adjustedLocation]
		ifNotNil: [
			requestor
				notify: string , ' ->'
				at: adjustedLocation
				in: source].
	^self fail! !

!Parser methodsFor: 'scanning' stamp: 'jmv 7/4/2022 10:00:32'!
transformVerticalBarAndUpArrowIntoABinarySelector
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a series of consecutive vertical bars, up arrows, colons and regular binary selector characters.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0.
	Colon $: can be used as binary, but '::' means Chain.
	Another special case is binary selectors ending in $-, like in
	1@-2
	This could be interpreted as `1 @ -2` or `1 @- 2`. Preference #atMinusDigitMeaning tells what to do."

	| toMakeBinary |
	"Special case: '::' is not a binary selector but the Chain operator"
	(hereType = #colon and: [tokenType = #colon]) ifTrue: [^ self ].
	"Note: Also include #binary, to allow stuff like #+| where a special character comes after after a regular binary operator character. "
	toMakeBinary _ #(binary verticalBar upArrow colon).
	(toMakeBinary identityIncludes: hereType) ifFalse: [
		^ self ].
	here _ here asPlainString.
	hereType _ #binary.
	[(toMakeBinary identityIncludes: tokenType) and: [hereMark + here size = mark]]
		whileTrue: [
			(token == #- and: [hereChar isDigit])
				ifTrue: [
					(Preferences at: #atMinusDigitMeaning) == #disabled
						"Disallow `1@-2`. Insists on some whitespace to disambiguate."
						ifTrue: [ ^self expected: 'A space character after selector' ].
					(Preferences at: #atMinusDigitMeaning) == #st80
						ifTrue: [ ^self ].				"interpret `1@-2` like `1 @ -2`"
					"Assume (Preferences at: #atMinusDigitMeaning) == #ansiSmalltalk
					interpret `1@-2` like `1 @- 2`, i.e. consider #@- a valid binary selector"
					].
			here _ here , token asPlainString.
			hereEnd _ hereEnd + 1.
			self scanToken ].! !

