'From Cuis 6.0 [latest update: #5982] on 22 August 2023 at 3:35:02 pm'!

!Character class methodsFor: 'class initialization' stamp: 'jmv 8/22/2023 15:34:00'!
initialize
	"
	Character initialize
	"
	Character isImmediateClass ifTrue: [
		CharacterTable := nil ].
	self initializeLookupTables.
	self initializeUnicodeCodePoints.! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 8/15/2023 18:06:26'!
nonImmediateNumericValue: anInteger
	"Answer the Character whose value is anInteger, if instances happen to be regular (i.e. not in Spur)"


	CharacterTable ifNil: [
		CharacterTable := (0 to: 255) collect: [ :i | Character basicNew instVarAt: 1 put: i; yourself ]].
	^CharacterTable at: anInteger + 1! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 8/15/2023 18:06:55'!
numericValue: anInteger
	"Answer the Character whose value is anInteger."

	^ Character isImmediateClass
		ifTrue: [ self immediateNumericValue: anInteger ]
		ifFalse: [ self nonImmediateNumericValue: anInteger ].! !


!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 8/15/2023 18:02:59'!
do: aBlock
	"evaluate aBlock with each character in the set"

	0 to: 255 do: [ :i | | c |
		c := Character numericValue: i.
		(self includes: c) ifTrue: [ aBlock value: c ] ].! !


!String class methodsFor: 'initialization' stamp: 'jmv 8/15/2023 17:04:32'!
initialize
	"
	String initialize
	"

	| order newOrder lowercase chars |
	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
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
	CaseInsensitiveOrder := newOrder asByteArray.
	
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
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
	CaseSensitiveOrder := newOrder asByteArray.


	chars := (0 to: 255) collect: [ :c | Character numericValue: c ].
	"a table for translating to lower case"
	LowercasingTable := String withAll: (chars collect: [:c | c asLowercase]).
	"a table for translating to upper case"
	UppercasingTable := String withAll: (chars collect: [:c | c asUppercase]).
	"a table for testing tokenish (for fast numArgs)"
	Tokenish := String withAll: (chars collect:
									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).

	"CR and LF--characters that terminate a line"
	CSLineEnders := CharacterSet new.
	CSLineEnders add: Character cr.
	CSLineEnders add: Character lf.

 	"separators and non-separators"
	CSSeparators := CharacterSet separators.
	CSNonSeparators := CSSeparators complement! !

!methodRemoval: Character class #characterTable stamp: 'jmv 8/22/2023 15:33:33'!
Character class removeSelector: #characterTable!
Character initialize!
String initialize!
