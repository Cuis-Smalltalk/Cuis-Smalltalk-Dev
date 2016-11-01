'From Cuis 4.2 of 25 July 2013 [latest update: #2958] on 31 October 2016 at 10:22:57 pm'!

!Collection methodsFor: 'testing' stamp: 'jmv 10/29/2016 21:23:08'!
isEmpty
	"Answer whether the receiver contains any elements."

	self do: [ :any | ^ false ].
	^true! !


!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 10/29/2016 21:18:02'!
do: aBlock
	"evaluate aBlock with each character in the set"

	Character characterTable do: [ :c |
		(self includes: c) ifTrue: [ aBlock value: c ] ]
! !


!String class methodsFor: 'initialization' stamp: 'jmv 10/31/2016 22:21:34'!
initialize
	"
	String initialize
	"

	| order newOrder lowercase |

	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 94 124 126) do: [ :c | "\^|~"
		newOrder at: c + 1 put: (order _ order+1)].
	#(oplus otimes odot dot circle bullet) do: [:each|
		newOrder at: (Character perform: each) numericValue + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order _ order+1.
		newOrder at: upperAndLowercase first numericValue + 1 put: order.
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase second numericValue + 1 put: order ]].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	CaseInsensitiveOrder _ newOrder asByteArray.
	
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 94 124 126) do: [ :c | "\^|~"
		newOrder at: c + 1 put: (order _ order+1)].
	#(oplus otimes odot dot circle bullet) do: [:each|
		newOrder at: (Character perform: each) numericValue + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase first numericValue + 1 put: (order _ order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase _ upperAndLowercase size = 1
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase numericValue + 1 put: (order _ order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	CaseSensitiveOrder _ newOrder asByteArray.

	"a table for translating to lower case"
	LowercasingTable _ String withAll: (Character characterTable collect: [:c | c asLowercase]).

	"a table for translating to upper case"
	UppercasingTable _ String withAll: (Character characterTable collect: [:c | c asUppercase]).

	"a table for testing tokenish (for fast numArgs)"
	Tokenish _ String withAll: (Character characterTable collect:
									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).

	"CR and LF--characters that terminate a line"
	CSLineEnders _ CharacterSet new.
	CSLineEnders add: Character cr.
	CSLineEnders add: Character lf.

 	"separators and non-separators"
	CSSeparators _ CharacterSet separators.
	CSNonSeparators _ CSSeparators complement! !

!methodRemoval: Character class #allCharacters!
Character class removeSelector: #allCharacters!
String initialize!
