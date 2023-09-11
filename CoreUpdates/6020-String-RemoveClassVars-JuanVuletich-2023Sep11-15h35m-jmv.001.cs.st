'From Cuis 6.0 [latest update: #6019] on 11 September 2023 at 3:38:09 pm'!
!classDefinition: #String category: #'Kernel-Text'!
CharacterSequence variableByteSubclass: #String
	instanceVariableNames: ''
	classVariableNames: 'CSLineEnders CSNonSeparators CSSeparators CaseInsensitiveOrder CaseSensitiveOrder LowercasingTable Tokenish UppercasingTable '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:20:24'!
basicCaseInsensitiveOrder
	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
	| newOrder order |
	newOrder := Array new: 256.
	order := -1.
	newOrder at: 0+1 put:  (order := order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order := order+1)].
	#(92 94 124 126 183 215 247) do: [ :c | "\^|~·÷×"
		newOrder at: c + 1 put: (order := order+1)].
	16r90 to: 16r9F do: [:c| "special math operators"
		newOrder at: c + 1 put: (order := order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order := order+1.
		newOrder at: upperAndLowercase first numericValue + 1 put: order.
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase second numericValue + 1 put: order ]].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order := order+1)]].
	^newOrder asByteArray.! !

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:21:00'!
basicCaseSensitiveOrder
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
	| newOrder order lowercase |
	newOrder := Array new: 256.
	order := -1.
	newOrder at: 0+1 put:  (order := order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order := order+1)].
	#(92 94 124 126 183 215 247) do: [ :c | "\^|~·÷×"
		newOrder at: c + 1 put: (order := order+1)].
	16r90 to: 16r9F do: [:c| "special math operators"
		newOrder at: c + 1 put: (order := order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase first numericValue + 1 put: (order := order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase := upperAndLowercase size = 1
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase numericValue + 1 put: (order := order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order := order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	^newOrder asByteArray.! !

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:21:32'!
caseInsensitiveOrder
	^`String basicCaseInsensitiveOrder`! !

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:21:57'!
caseSensitiveOrder
	^`String basicCaseSensitiveOrder`! !

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:15:14'!
charSetLineEnders
	"CR and LF--characters that terminate a line"
	^`CharacterSet new
		add: Character cr;
		add: Character lf;
		yourself`.! !

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:16:33'!
charSetNonSeparators
	"CR and LF--characters that terminate a line"
	^`CharacterSet separators complement`.! !

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:16:24'!
charSetSeparators
	"CR and LF--characters that terminate a line"
	^`CharacterSet separators`.! !

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:27:58'!
lowercasingTable
	"a table for translating to lower case"
	^`String withAll: ((0 to: 255) collect: [ :c | (Character numericValue: c) asLowercase ])`
! !

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:30:19'!
tokenishTable
	"a table for testing tokenish (for fast numArgs)"
	^`String withAll: (
		((0 to: 255) collect: [ :c | (Character numericValue: c) ])
			collect: 	[ :c | c tokenish ifTrue: [c] ifFalse: [$~]]	)`! !

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:28:26'!
uppercasingTable
	"a table for translating to upper case"
	^`String withAll: ((0 to: 255) collect: [ :c | (Character numericValue: c) asUppercase ])`
! !


!String methodsFor: 'services' stamp: 'jmv 9/11/2023 14:31:47'!
numArgs
	"Answer either the number of arguments that the receiver would take if considered a selector. 
	Answer -1 if it couldn't be a selector. 
		Note that currently this will answer -1 for anything begining with an uppercase letter even though 
		the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."
	| firstChar numColons start ix |
	self isEmpty ifTrue: [ ^ -1 ].
	"Binary messages"
	(self allSatisfy: [ :c | c isValidInBinarySelectors ])
		ifTrue: [ ^1 ].
	"Unary and keyword messages"
	firstChar := self at: 1.
	firstChar isValidStartOfIdentifiers ifTrue: [
		"Fast reject if any chars are non-alphanumeric"
		(String
			findSubstring: `'~' asByteString`
			in: self
			startingAt: 1
			matchTable: String tokenishTable) > 0 ifTrue: [ ^ -1 ].
		"Fast colon count"
		numColons := 0.
		start := 1.
		[ (ix := self findString: ':' startingAt: start) > 0 ] whileTrue: [
			numColons := numColons + 1.
			start := ix + 1 ].
		^ numColons ].
	^ -1.! !


!String class methodsFor: 'character collation' stamp: 'jmv 9/11/2023 14:22:24'!
does: aCharacter caseInsensitiveCollateBefore: otherCharacter
	"
	String does: $a caseInsensitiveCollateBefore: $b
	String does: $a caseInsensitiveCollateBefore: $B
	String does: $A caseInsensitiveCollateBefore: $b
	"
	^ (self caseInsensitiveOrder at: aCharacter numericValue + 1) <
		(self caseInsensitiveOrder at: otherCharacter numericValue + 1)! !

!String class methodsFor: 'character collation' stamp: 'jmv 9/11/2023 14:23:19'!
does: aCharacter collateAfter: otherCharacter
	^ (self caseSensitiveOrder at: aCharacter numericValue + 1) >
		(self caseSensitiveOrder at: otherCharacter numericValue + 1)! !

!String class methodsFor: 'character collation' stamp: 'jmv 9/11/2023 14:23:30'!
does: aCharacter collateBefore: otherCharacter
	^ (self caseSensitiveOrder at: aCharacter numericValue + 1) <
		(self caseSensitiveOrder at: otherCharacter numericValue + 1)! !

!String class methodsFor: 'services' stamp: 'jmv 9/11/2023 14:23:06'!
compare: string1 with: string2
	"Return 1, 2 or 3, if string1 is <, =, or > string2,
	with the collating order of characters given by the order array."

	^self compare: string1 with: string2 collated: self caseSensitiveOrder! !

!String class methodsFor: 'services' stamp: 'jmv 9/11/2023 14:22:12'!
compareIgnoreCase: string1 with: string2
	"Return 1, 2 or 3, if string1 is <, =, or > string2, 
	with the collating order of characters given by the order array."

	^self compare: string1 with: string2 collated: self caseInsensitiveOrder! !

!String class methodsFor: 'services' stamp: 'jmv 9/11/2023 14:22:49'!
findStringIgnoreCase: subString in: body startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0.
	Ignore case"

	^ self findSubstring: subString in: body startingAt: start matchTable: self caseInsensitiveOrder.! !

!String class methodsFor: 'services' stamp: 'jmv 9/11/2023 14:23:42'!
is: aStringOrByteArray equalTo: another
	"Take adventage of String primitive for comparing, for example, ByteArrays"

	^ (self compare: aStringOrByteArray with: another collated: self caseSensitiveOrder) = 2! !

!String class methodsFor: 'services' stamp: 'jmv 9/11/2023 14:15:47'!
string: aString lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| start end endWithoutDelimiters |
	start := 1.
	[
		end := aString indexOfAnyOf: self charSetLineEnders startingAt: start ifAbsent: [ 0 ].
		end = 0
			ifTrue: [
				"Last line was found. Evaluate and exit.
				Note. If last char in receiver is a line separator, there's an extra empty line"
				endWithoutDelimiters := end := aString size.
				aBlock value: start value: endWithoutDelimiters value: end.
				^aString ].

		"Account for CrLf sequence"
		endWithoutDelimiters := end - 1.
		(end < aString size
			and: [(aString basicAt: end + 1) = `Character lf codePoint`
			and: [(aString basicAt: end) = `Character cr codePoint` ]])
				ifTrue: [ end := end + 1].

		aBlock value: start value: endWithoutDelimiters value: end.
		start := end + 1 ] repeat! !

!String class methodsFor: 'services' stamp: 'jmv 9/11/2023 14:16:57'!
substringsIn: aByteStringOrByteArray
	"Answer an array of the substrings that compose the argument.
	If argument is an UTF-8 encoded ByteArray, answer is an array of same."

	| end beginning |
	end := 0.
	^Array streamContents: [ :result |
		"find one substring each time through this loop"
		[ 
			"find the beginning of the next substring"
			beginning := aByteStringOrByteArray indexOfAnyOf: self charSetNonSeparators startingAt: end+1 ifAbsent: nil.
			beginning notNil ] 
		whileTrue: [
			"find the end"
			end := aByteStringOrByteArray indexOfAnyOf: self charSetSeparators startingAt: beginning ifAbsent: [ aByteStringOrByteArray size + 1 ].
			end := end - 1.
			result nextPut: (aByteStringOrByteArray copyFrom: beginning to: end).
		]
	].! !

!String class methodsFor: 'services' stamp: 'jmv 9/11/2023 14:30:57'!
translateToLowercase: aStringOrByteArray
	"translate the characters in the string by the given table, in place"
	self translate: aStringOrByteArray from: 1 to: aStringOrByteArray size table: self lowercasingTable! !

!String class methodsFor: 'services' stamp: 'jmv 9/11/2023 14:31:14'!
translateToUppercase: aStringOrByteArray
	"translate the characters in the string by the given table, in place"
	self translate: aStringOrByteArray from: 1 to: aStringOrByteArray size table: self uppercasingTable! !

!String class methodsFor: 'accessing' stamp: 'jmv 9/11/2023 14:23:35'!
findString: subString in: body startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	^ self findSubstring: subString in: body startingAt: start matchTable: self caseSensitiveOrder.! !

!methodRemoval: String class #initialize stamp: 'jmv 9/11/2023 15:36:50'!
String class removeSelector: #initialize!
!classDefinition: #String category: #'Kernel-Text'!
CharacterSequence variableByteSubclass: #String
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Text'!
String initialize!
