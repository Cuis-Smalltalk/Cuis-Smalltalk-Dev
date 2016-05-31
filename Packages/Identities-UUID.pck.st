'From Cuis 4.2 of 25 July 2013 [latest update: #2781] on 31 May 2016 at 12:06:44.61949 pm'!
'Description Code was filed out from Squeak 4.5 and ported to Cuis with few changes by David Graham.'!
!provides: 'Identities-UUID' 1 7!
!requires: 'Sound' 1 3 nil!
!classDefinition: #UUID category: #'Identities-UUID'!
ByteArray variableByteSubclass: #UUID
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Identities-UUID'!
!classDefinition: 'UUID class' category: #'Identities-UUID'!
UUID class
	instanceVariableNames: ''!

!classDefinition: #UUIDPrimitivesTest category: #'Identities-UUID-Tests'!
TestCase subclass: #UUIDPrimitivesTest
	instanceVariableNames: ''
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Identities-UUID-Tests'!
!classDefinition: 'UUIDPrimitivesTest class' category: #'Identities-UUID-Tests'!
UUIDPrimitivesTest class
	instanceVariableNames: ''!

!classDefinition: #UUIDTest category: #'Identities-UUID-Tests'!
TestCase subclass: #UUIDTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Identities-UUID-Tests'!
!classDefinition: 'UUIDTest class' category: #'Identities-UUID-Tests'!
UUIDTest class
	instanceVariableNames: ''!

!classDefinition: #UUIDGenerator category: #'Identities-UUID'!
Object subclass: #UUIDGenerator
	instanceVariableNames: 'timeLow timeMid timeHiAndVersion clockSeqHiAndReserved clockSeqLow node randomCounter randomGenerator semaphoreForGenerator'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Identities-UUID'!
!classDefinition: 'UUIDGenerator class' category: #'Identities-UUID'!
UUIDGenerator class
	instanceVariableNames: ''!


!UUID commentStamp: '<historical>' prior: 0!
A class to generate UUID
by John M McIntosh johnmci@smalltalkconsulting.com

See http://www.webdav.org/specs/draft-leach-uuids-guids-01.txt

If a plugin does not exist then we generate a UUID version 4 type GUUID!

!UUIDGenerator commentStamp: '<historical>' prior: 0!
This class generates a pseudo-random UUID
by John M McIntosh johnmci@smalltalkconsulting.com

See http://www.webdav.org/specs/draft-leach-uuids-guids-01.txt!

!UUID methodsFor: 'comparing' stamp: 'jmv 6/17/2015 18:03'!
< aMagnitude 
	"Answer whether the receiver is less than the argument."

	| x y |
	1 to: self size do: [ :i |
		(x := self at: i) = (y := aMagnitude at: i) ifFalse: [ ^x < y ] ].
	^false.! !

!UUID methodsFor: 'comparing' stamp: 'JMM 11/22/2001 17:30'!
<= aMagnitude 
	"Answer whether the receiver is less than or equal to the argument."

	^(self > aMagnitude) not! !

!UUID methodsFor: 'comparing' stamp: 'JMM 11/22/2001 17:30'!
> aMagnitude 
	"Answer whether the receiver is greater than the argument."

	^aMagnitude < self! !

!UUID methodsFor: 'comparing' stamp: 'JMM 11/22/2001 17:30'!
>= aMagnitude 
	"Answer whether the receiver is greater than or equal to the argument."

	^(self < aMagnitude) not! !

!UUID methodsFor: 'converting' stamp: 'ar 2/8/2004 12:16'!
asString
	| result data |
	data := String new: 36.
	result := WriteStream on: data.
	1 to: 4 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	5 to: 6 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	7 to: 8 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	9 to: 10 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	11 to: 16 do:[:i| self printHexAt: i to: result].
	^data.
	! !

!UUID methodsFor: 'converting' stamp: 'JMM 11/22/2001 13:09'!
asUUID: aString
	| stream token byte |
	stream := ReadStream on: (aString copyReplaceAll: '-' with: '') asUppercase.
	1 to: stream size/2 do: [:i | 
		token := stream next: 2.
		byte := Integer readFrom: (ReadStream on: token ) base: 16.
		self at: i put: byte].
	^self
! !

!UUID methodsFor: 'converting' stamp: 'JMM 11/22/2001 13:13'!
createStringStartingAt: index for: bytes

	| results candidate data |
	data := String new: bytes*2.
	results := WriteStream on: data.
	index to: index+bytes -1 do: 
		[:i |
		candidate := ((self at: i) printStringBase: 16) last: 2.
		candidate first = $r ifTrue: [candidate := String with: $0 with: candidate last].
		results nextPutAll: candidate].
	^data asLowercase! !

!UUID methodsFor: 'initalize-release' stamp: 'ar 2/3/2002 19:42'!
initialize
	self primMakeUUID.! !

!UUID methodsFor: 'testing' stamp: 'JMM 10/9/2001 14:17'!
isNilUUID
	1 to: self size do: [:i | (self at: i) ~= 0 ifTrue: [^false]].
	^true.! !

!UUID methodsFor: 'system primitives' stamp: 'ar 2/3/2002 19:42'!
primMakeUUID
	<primitive: 'primitiveMakeUUID' module: 'UUIDPlugin'>
	UUIDGenerator default generateBytes: self forVersion: 4.! !

!UUID methodsFor: 'converting' stamp: 'ar 2/8/2004 12:16'!
printHexAt: index to: aStream
	| map v |
	map := '0123456789abcdef'.
	v := self at: index.
	aStream nextPut: (map at: (v bitShift: -4) + 1). 
	aStream nextPut: (map at: (v bitAnd: 15) + 1).
! !

!UUID methodsFor: 'printing' stamp: 'JMM 10/9/2001 14:46'!
printOn: aStream
	aStream nextPutAll: 'an UUID('.
	self asString printOn: aStream.
	aStream nextPutAll: ')'! !

!UUID methodsFor: 'printing' stamp: 'ar 2/8/2004 12:16'!
printString
	^self asString! !

!UUID class methodsFor: 'instance creation' stamp: 'dvf 9/10/2004 23:10'!
fromString: aString
	| object |
	aString size ~= 36 ifTrue: [Error signal].
	object := self nilUUID. 
	object asUUID: aString.
	^object! !

!UUID class methodsFor: 'instance creation' stamp: 'nk 7/30/2004 21:49'!
new
	^(self new: 16)! !

!UUID class methodsFor: 'instance creation' stamp: 'nk 6/28/2004 16:10'!
nilUUID
	"Must call basicNew: here because I have a non-trivial initialize method."

	^self basicNew: 16! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'JMM 11/22/2001 17:14'!
testCreation
	| uuid |
	uuid := UUID new.
	self should: [uuid size = 16].
	self shouldnt: [uuid isNilUUID].
	self should: [uuid asString size = 36].
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'JMM 11/22/2001 17:27'!
testCreationEquality
	| uuid1 uuid2 |
	uuid1 := UUID new.
	uuid2 := UUID new.
	self should: [uuid1 = uuid1].
	self should: [uuid2 = uuid2].
	self shouldnt: [uuid1 = uuid2].
	self shouldnt: [uuid1 hash = uuid2 hash].
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'JMM 11/22/2001 17:17'!
testCreationFromString
	| uuid string |
	string := UUID nilUUID asString.
	uuid := UUID fromString: string.
	self should: [uuid size = 16].
	self should: [uuid = UUID nilUUID].
	self should: [uuid isNilUUID].
	self should: [uuid asString size = 36].
	self should: [uuid asArray asSet size = 1].
	self should: [(uuid asArray asSet asArray at: 1) = 0].
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'JMM 11/22/2001 17:18'!
testCreationFromStringNotNil
	| uuid string |
	string := UUID new asString.
	uuid := UUID fromString: string.
	self should: [uuid size = 16].
	self should: [uuid asString size = 36].

! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'JMM 11/22/2001 17:16'!
testCreationNil
	| uuid |
	uuid := UUID nilUUID.
	self should: [uuid size = 16].
	self should: [uuid isNilUUID].
	self should: [uuid asString size = 36].
	self should: [uuid asArray asSet size = 1].
	self should: [(uuid asArray asSet asArray at: 1) = 0].
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'nice 12/27/2009 04:14'!
testCreationNodeBased
	

	(UUID new asString last: 12) = (UUID new asString last: 12) ifFalse: [^self].
	1000 timesRepeat:
		[ | uuid |
		uuid := UUID new.
		self should: [((uuid at: 7) bitAnd: 16rF0) = 16r10].
		self should: [((uuid at: 9) bitAnd: 16rC0) = 16r80]]
! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'nice 12/27/2009 04:15'!
testDuplicationsKinda
	| check size |

	size := 5000.
	check := Set new: size.
	size timesRepeat: 
		[ | uuid |
		uuid := UUID new.
		self shouldnt: [check includes: uuid].
		check add: uuid].
		! !

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'nice 12/27/2009 04:15'!
testOrder
	
	100 timesRepeat:
		[ | uuid1 uuid2 |
		uuid1 := UUID new.
		uuid2 := UUID new.
		(uuid1 asString last: 12) = (uuid2 asString last: 12) ifTrue:
			[self should: [uuid1 < uuid2].
			self should: [uuid2 > uuid1].
			self shouldnt: [uuid1 = uuid2]]]
! !

!UUIDTest methodsFor: 'as yet unclassified' stamp: 'DSG 5/28/2013 09:37'!
testComparison
	"Test if the comparison operators define a total sort function."

	#(
		#[3 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0] #[2 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
		#[0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 1] #[0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 4]
		#[2 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0] #[3 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
		#[0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 4] #[0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 1]
		#[0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 4] #[0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 4]
	) pairsDo: [ :x :y |
		| a b c d |
		a := UUID newFrom: x.
		b := UUID newFrom: y.
		c := x asString.
		d := y asString.
		"Check if the comparison is lexicographical, just like strings'."
		#(< > <= >= = ~=) do: [ :operation | 
			self assert: (a perform: operation with: b) = (c perform: operation with: d) ].
		"And a few more"
		self
			assert: (a < b) = (a >= b) not;
			assert: (a > b) = (a <= b) not;
			assert: (a = b) = (a ~= b) not;
			assert: (a < b) = (b > a);
			assert: (a > b) = (b < a);
			assert: (a >= b) = (b <= a);
			assert: (a <= b) = (b >= a);
			assert: (a = b) = (b = a);
			assert: (a ~= b) = (b ~= a);
			assert: (a > b) = ((a >= b) & (a ~= b));
			assert: (a < b) = ((a <= b) & (a ~= b));
			assert: (a >= b) = ((a = b) | (a > b));
			assert: (a <= b) = ((a = b) | (a < b));
			assert: (a ~= b) = ((a < b) | (a > b));
			assert: (a <= b) & (b <= a) = (a = b);
			assert: (a >= b) & (b >= a) = (a = b);
			assert: (a <= b) | (b <= a);
			"assert: (a = b) asBit + (a < b) asBit + (b < a) asBit = 1"
			assert: (((a = b) and: (a < b)) not) = (((a = b) xor: (a < b)) xor: (b < a)) ]! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 11/22/2001 13:45'!
generateBytes: aPlaceHolder forVersion: aVersion
	aVersion = 4 ifTrue: [self generateFieldsVersion4]
		ifFalse: [self error: 'Unsupported version'].
	self placeFields: aPlaceHolder.! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 11/22/2001 23:13'!
generateFieldsVersion4

	timeLow := self generateRandomBitsOfLength: 32.
	timeMid := self generateRandomBitsOfLength: 16.
	timeHiAndVersion := 16r4000 bitOr: (self generateRandomBitsOfLength: 12).
	clockSeqHiAndReserved := 16r80 bitOr: (self generateRandomBitsOfLength: 6).
	clockSeqLow := self generateRandomBitsOfLength: 8.
	node := self generateRandomBitsOfLength: 48.
	! !

!UUIDGenerator methodsFor: 'generator' stamp: 'nice 12/26/2009 22:20'!
generateOneOrZero
	^self semaphoreForGenerator
		critical: [| value | 
			value := self randomGenerator next.
			self randomCounter: self randomCounter + 1.
			self randomCounter > 100000
				ifTrue: [self setupRandom].
			value < 0.5
				ifTrue: [0]
				ifFalse: [1]].! !

!UUIDGenerator methodsFor: 'generator' stamp: 'JMM 11/21/2001 15:12'!
generateRandomBitsOfLength: aNumberOfBits
| target |
	target := 0.
	aNumberOfBits isZero ifTrue: [^target].
	target := self generateOneOrZero.
	(aNumberOfBits - 1)  timesRepeat:
		[target := (target bitShift: 1)  bitOr: self generateOneOrZero].
	^target! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 11/21/2001 14:30'!
initialize
	self setupRandom.
	semaphoreForGenerator := Semaphore forMutualExclusion.
	! !

!UUIDGenerator methodsFor: 'random seed' stamp: 'jmv 5/19/2015 23:34'!
makeSeed
	"Try various methods of getting good seeds"
	| seed |
	seed := self makeUnixSeed.
	seed ifNotNil: [^seed].

	"not sure if this is reliably random... commented out for now. -dew"
	"seed := self makeSeedFromSound.
	seed ifNotNil: [^seed]."
	
	"default"
	[seed := (Time millisecondClockValue bitAnd: 16r3FFFFFFF) bitXor: self hash.
	seed := seed bitXor: (Time primLocalSecondsClock bitAnd: 16r3FFFFFFF).
	seed = 0] whileTrue: ["Try again if ever get a seed = 0"].

	^seed
! !

!UUIDGenerator methodsFor: 'random seed' stamp: 'DSG 12/4/2013 20:39'!
makeSeedFromSound
	^[SoundSystem default randomBitsFromSoundInput: 32]
		ifError: [nil].! !

!UUIDGenerator methodsFor: 'random seed' stamp: 'pb 5/25/2016 01:34'!
makeUnixSeed
	
	^[
		'/dev/urandom' asFileEntry readStreamDo: [ :stream |
			stream binary.
			(Integer
				byte1: stream next
				byte2: stream next
				byte3: stream next
				byte4: stream next) ] ] 
		on: Error
		do: [ nil ]! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 11/22/2001 23:12'!
placeFields: aByteArray

	aByteArray at: 1 put: ((timeLow bitShift: -24) bitAnd: 16rFF).
	aByteArray at: 2 put: ((timeLow bitShift: -16) bitAnd: 16rFF).
	aByteArray at: 3 put: ((timeLow bitShift: -8) bitAnd: 16rFF).
	aByteArray at: 4 put: (timeLow bitAnd: 16rFF).
	aByteArray at: 5 put: ((timeMid bitShift: -8) bitAnd: 16rFF).
	aByteArray at: 6 put: (timeMid bitAnd: 16rFF).
	aByteArray at: 7 put: ((timeHiAndVersion bitShift: -8) bitAnd: 16rFF).
	aByteArray at: 8 put: (timeHiAndVersion bitAnd: 16rFF).
	aByteArray at: 9 put: clockSeqHiAndReserved.
	aByteArray at: 10 put: clockSeqLow.
	0 to: 5 do: [:i |
		aByteArray at: 11 + i put: ((node bitShift: (-8*i)) bitAnd: 16rFF)]
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:28'!
randomCounter
	^randomCounter! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:29'!
randomCounter: aNumber
	randomCounter := aNumber
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:27'!
randomGenerator
	^randomGenerator
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:27'!
randomGenerator: aGenerator
	randomGenerator := aGenerator
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:29'!
semaphoreForGenerator
	^semaphoreForGenerator! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 11/21/2001 14:29'!
semaphoreForGenerator: aSema
	semaphoreForGenerator := aSema
! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'CdG 11/19/2002 21:30'!
setupRandom
	randomCounter := 0.
	randomGenerator := Random seed: self makeSeed.! !

!UUIDGenerator class methodsFor: 'instance creation' stamp: 'JMM 11/22/2001 13:41'!
default
	Default ifNil: [self generateDefault].
	^Default! !

!UUIDGenerator class methodsFor: 'instance creation' stamp: 'nk 7/30/2004 21:51'!
generateDefault
	Default := self new! !

!UUIDGenerator class methodsFor: 'class initialization' stamp: 'CdG 11/19/2002 21:06'!
initialize
	Smalltalk addToStartUpList: self after: nil.! !

!UUIDGenerator class methodsFor: 'class initialization' stamp: 'CdG 11/19/2002 21:07'!
startUp
	Default := nil! !
UUIDGenerator initialize!
