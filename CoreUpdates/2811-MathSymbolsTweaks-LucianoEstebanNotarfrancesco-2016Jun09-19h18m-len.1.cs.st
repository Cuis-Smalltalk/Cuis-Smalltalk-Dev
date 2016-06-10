'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 9 June 2016 at 7:48:12.040872 pm'!

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/8/2016 21:04'!
CC
	"
	Character CC
	"
	^ self value: 16r86! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/8/2016 21:04'!
HH
	"
	Character HH
	"
	^ self value: 16r87! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/8/2016 21:05'!
NN
	"
	Character NN
	"
	^ self value: 16r88! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/8/2016 21:06'!
PP
	"
	Character PP
	"
	^ self value: 16r89! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/8/2016 21:06'!
QQ
	"
	Character QQ
	"
	^ self value: 16r8A! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/8/2016 21:06'!
RR
	"
	Character RR
	"
	^ self value: 16r8B! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/8/2016 21:04'!
ZZ
	"
	Character ZZ
	"
	^ self value: 16r8C! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/9/2016 19:44'!
circle
	"
	Character circle
	"
	^ self value: 16r9E! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/8/2016 06:59'!
odot
	"
	Character odot
	"
	^ self value: 16r8E! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/8/2016 06:52'!
oplus
	"
	Character oplus
	"
	^ self value: 16r8F! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/8/2016 06:52'!
otimes
	"
	Character otimes
	"
	^ self value: 16r90! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/9/2016 19:32'!
partial
	"
	Character partial
	"
	^ self value: 16r81! !


!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/30/2016 21:32'!
doesNotExist
	"
	Character doesNotExist
	"
	^ self value: 16r83! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/30/2016 21:33'!
exists
	"
	Character exists
	"
	^ self value: 16r82! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 6/9/2016 19:20'!
times
	"
	Character times
	"
	^ self value: 16rD7! !


!String class methodsFor: 'initialization' stamp: 'len 6/9/2016 19:46'!
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
	#(92 124 126) do: [ :c | "\|~"
		newOrder at: c + 1 put: (order _ order+1)].
	#(oplus otimes odot dot circle bullet) do: [:each|
		newOrder at: (Character perform: each) asciiValue + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order _ order+1.
		newOrder at: upperAndLowercase first asciiValue + 1 put: order.
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase second asciiValue + 1 put: order ]].
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
	#(92 124 126) do: [ :c | "\|~"
		newOrder at: c + 1 put: (order _ order+1)].
	#(oplus otimes odot dot circle bullet) do: [:each|
		newOrder at: (Character perform: each) asciiValue + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase first asciiValue + 1 put: (order _ order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase _ upperAndLowercase size = 1
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase asciiValue + 1 put: (order _ order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	CaseSensitiveOrder _ newOrder asByteArray.

	"a table for translating to lower case"
	LowercasingTable _ String withAll: (Character allCharacters collect: [:c | c asLowercase]).

	"a table for translating to upper case"
	UppercasingTable _ String withAll: (Character allCharacters collect: [:c | c asUppercase]).

	"a table for testing tokenish (for fast numArgs)"
	Tokenish _ String withAll: (Character allCharacters collect:
									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).

	"CR and LF--characters that terminate a line"
	CSLineEnders _ CharacterSet new.
	CSLineEnders add: Character cr.
	CSLineEnders add: Character lf.

 	"separators and non-separators"
	CSSeparators _ CharacterSet separators.
	CSNonSeparators _ CSSeparators complement! !

!methodRemoval: Character class #complexNumbers!
Character class removeSelector: #complexNumbers!
!methodRemoval: Character class #directProduct!
Character class removeSelector: #directProduct!
!methodRemoval: Character class #directSum!
Character class removeSelector: #directSum!
!methodRemoval: Character class #hamilton!
Character class removeSelector: #hamilton!
!methodRemoval: Character class #integerNumbers!
Character class removeSelector: #integerNumbers!
!methodRemoval: Character class #naturalNumbers!
Character class removeSelector: #naturalNumbers!
!methodRemoval: Character class #partialDifferential!
Character class removeSelector: #partialDifferential!
!methodRemoval: Character class #primeNumbers!
Character class removeSelector: #primeNumbers!
!methodRemoval: Character class #rationalNumbers!
Character class removeSelector: #rationalNumbers!
!methodRemoval: Character class #realNumbers!
Character class removeSelector: #realNumbers!
!methodRemoval: Character class #ring!
Character class removeSelector: #ring!
!methodRemoval: Character class #tensorProduct!
Character class removeSelector: #tensorProduct!
!methodRemoval: Character class #thereDoesNotExists!
Character class removeSelector: #thereDoesNotExists!
String initialize!
SmalltalkCompleter initialize!
