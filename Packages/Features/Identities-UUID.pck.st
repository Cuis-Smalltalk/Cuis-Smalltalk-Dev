'From Cuis7.7 [latest update: #7779] on 23 December 2025 at 4:39:03 pm'!
'Description Code was filed out from Squeak 4.5 and ported to Cuis with few changes by David Graham.'!
!provides: 'Identities-UUID' 1 11!
SystemOrganization addCategory: #'Identities-UUID'!


!classDefinition: #UUIDGenerator category: #'Identities-UUID'!
Object subclass: #UUIDGenerator
	instanceVariableNames: 'timeLow timeMid timeHiAndVersion clockSeqHiAndReserved clockSeqLow node randomCounter randomGenerator semaphoreForGenerator'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Identities-UUID'!
!classDefinition: 'UUIDGenerator class' category: #'Identities-UUID'!
UUIDGenerator class
	instanceVariableNames: ''!

!classDefinition: #UUID category: #'Identities-UUID'!
ByteArray variableByteSubclass: #UUID
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Identities-UUID'!
!classDefinition: 'UUID class' category: #'Identities-UUID'!
UUID class
	instanceVariableNames: ''!


!UUIDGenerator commentStamp: '<historical>' prior: 0!
This class generates a pseudo-random UUID
by John M McIntosh johnmci@smalltalkconsulting.com

See http://www.webdav.org/specs/draft-leach-uuids-guids-01.txt!

!UUID commentStamp: '<historical>' prior: 0!
A class to generate UUID
by John M McIntosh johnmci@smalltalkconsulting.com

See http://www.webdav.org/specs/draft-leach-uuids-guids-01.txt

If a plugin does not exist then we generate a UUID version 4 type GUUID!

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 22/Nov/2001 13:45:00'!
generateBytes: aPlaceHolder forVersion: aVersion
	aVersion = 4 ifTrue: [self generateFieldsVersion4]
		ifFalse: [self error: 'Unsupported version'].
	self placeFields: aPlaceHolder.! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 22/Nov/2001 23:13:00'!
generateFieldsVersion4

	timeLow := self generateRandomBitsOfLength: 32.
	timeMid := self generateRandomBitsOfLength: 16.
	timeHiAndVersion := 16r4000 bitOr: (self generateRandomBitsOfLength: 12).
	clockSeqHiAndReserved := 16r80 bitOr: (self generateRandomBitsOfLength: 6).
	clockSeqLow := self generateRandomBitsOfLength: 8.
	node := self generateRandomBitsOfLength: 48.
	! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 21/Nov/2001 14:30:00'!
initialize
	self setupRandom.
	semaphoreForGenerator := Semaphore forMutualExclusion.
	! !

!UUIDGenerator methodsFor: 'instance creation' stamp: 'JMM 22/Nov/2001 23:12:00'!
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

!UUIDGenerator methodsFor: 'instance creation' stamp: 'CdG 19/Nov/2002 21:30:00'!
setupRandom
	randomCounter := 0.
	randomGenerator := Random seed: self makeSeed.! !

!UUIDGenerator methodsFor: 'generator' stamp: 'nice 26/Dec/2009 22:20:00'!
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

!UUIDGenerator methodsFor: 'generator' stamp: 'JMM 21/Nov/2001 15:12:00'!
generateRandomBitsOfLength: aNumberOfBits
| target |
	target := 0.
	aNumberOfBits isZero ifTrue: [^target].
	target := self generateOneOrZero.
	(aNumberOfBits - 1)  timesRepeat:
		[target := (target bitShift: 1)  bitOr: self generateOneOrZero].
	^target! !

!UUIDGenerator methodsFor: 'random seed' stamp: 'jmv 9/Nov/2021 18:16:32'!
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
	seed := seed bitXor: (Time localSecondClock bitAnd: 16r3FFFFFFF).
	seed = 0] whileTrue: ["Try again if ever get a seed = 0"].

	^seed
! !

!UUIDGenerator methodsFor: 'random seed' stamp: 'jmv 16/Nov/2024 08:58:26'!
makeSeedFromSound
	"
	UUIDGenerator new makeSeedFromSound
	Load the Sound package and uncomment code to use.
	"
	"
	^[SoundSystem default randomBitsFromSoundInput: 32]
		ifError: [nil].
	"! !

!UUIDGenerator methodsFor: 'random seed' stamp: 'jmv 16/Nov/2024 08:57:04'!
makeUnixSeed
	"
	UUIDGenerator new makeUnixSeed
	"
	
	^[
		'/dev/urandom' asFileEntry readStreamDo: [ :stream |
			stream binary.
			(stream next bitShift: 24)
			 + (stream next bitShift: 16)
			 + (stream next bitShift: 8)
			 + stream next
			"(Integer
				byte1: stream next
				byte2: stream next
				byte3: stream next
				byte4: stream next)" ] ] 
		on: Error
		do: [ nil ]! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 21/Nov/2001 14:28:00'!
randomCounter
	^randomCounter! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 21/Nov/2001 14:29:00'!
randomCounter: aNumber
	randomCounter := aNumber
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 21/Nov/2001 14:27:00'!
randomGenerator
	^randomGenerator
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 21/Nov/2001 14:27:00'!
randomGenerator: aGenerator
	randomGenerator := aGenerator
! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 21/Nov/2001 14:29:00'!
semaphoreForGenerator
	^semaphoreForGenerator! !

!UUIDGenerator methodsFor: 'accessors and mutators' stamp: 'JMM 21/Nov/2001 14:29:00'!
semaphoreForGenerator: aSema
	semaphoreForGenerator := aSema
! !

!UUIDGenerator class methodsFor: 'class initialization' stamp: 'CdG 19/Nov/2002 21:06:00'!
initialize
	Smalltalk addToStartUpList: self after: nil.! !

!UUIDGenerator class methodsFor: 'class initialization' stamp: 'CdG 19/Nov/2002 21:07:00'!
startUp
	Default := nil! !

!UUIDGenerator class methodsFor: 'instance creation' stamp: 'JMM 22/Nov/2001 13:41:00'!
default
	Default ifNil: [self generateDefault].
	^Default! !

!UUIDGenerator class methodsFor: 'instance creation' stamp: 'nk 30/Jul/2004 21:51:00'!
generateDefault
	Default := self new! !

!UUID methodsFor: 'printing' stamp: 'JMM 9/Oct/2001 14:46:00'!
printOn: aStream
	aStream nextPutAll: 'an UUID('.
	self asString printOn: aStream.
	aStream nextPutAll: ')'! !

!UUID methodsFor: 'printing' stamp: 'ar 8/Feb/2004 12:16:00'!
printString
	^self asString! !

!UUID methodsFor: 'comparing' stamp: 'jmv 17/Jun/2015 18:03:00'!
< aMagnitude 
	"Answer whether the receiver is less than the argument."

	| x y |
	1 to: self size do: [ :i |
		(x := self at: i) = (y := aMagnitude at: i) ifFalse: [ ^x < y ] ].
	^false.! !

!UUID methodsFor: 'comparing' stamp: 'JMM 22/Nov/2001 17:30:00'!
<= aMagnitude 
	"Answer whether the receiver is less than or equal to the argument."

	^(self > aMagnitude) not! !

!UUID methodsFor: 'comparing' stamp: 'JMM 22/Nov/2001 17:30:00'!
> aMagnitude 
	"Answer whether the receiver is greater than the argument."

	^aMagnitude < self! !

!UUID methodsFor: 'comparing' stamp: 'JMM 22/Nov/2001 17:30:00'!
>= aMagnitude 
	"Answer whether the receiver is greater than or equal to the argument."

	^(self < aMagnitude) not! !

!UUID methodsFor: 'converting' stamp: 'jmv 15/Nov/2022 10:40:59'!
asString
	^String streamContents: [ :result |
		1 to: 4 do:[:i| self printHexAt: i to: result].
		result nextPut: $-.
		5 to: 6 do:[:i| self printHexAt: i to: result].
		result nextPut: $-.
		7 to: 8 do:[:i| self printHexAt: i to: result].
		result nextPut: $-.
		9 to: 10 do:[:i| self printHexAt: i to: result].
		result nextPut: $-.
		11 to: 16 do:[:i| self printHexAt: i to: result]].! !

!UUID methodsFor: 'converting' stamp: 'JMM 22/Nov/2001 13:09:00'!
asUUID: aString
	| stream token byte |
	stream := ReadStream on: (aString copyReplaceAll: '-' with: '') asUppercase.
	1 to: stream size/2 do: [:i | 
		token := stream next: 2.
		byte := Integer readFrom: (ReadStream on: token ) base: 16.
		self at: i put: byte].
	^self
! !

!UUID methodsFor: 'converting' stamp: 'ar 8/Feb/2004 12:16:00'!
printHexAt: index to: aStream
	| map v |
	map := '0123456789abcdef'.
	v := self at: index.
	aStream nextPut: (map at: (v bitShift: -4) + 1). 
	aStream nextPut: (map at: (v bitAnd: 15) + 1).
! !

!UUID methodsFor: 'initalize-release' stamp: 'ar 3/Feb/2002 19:42:00'!
initialize
	self primMakeUUID.! !

!UUID methodsFor: 'testing' stamp: 'JMM 9/Oct/2001 14:17:00'!
isNilUUID
	1 to: self size do: [:i | (self at: i) ~= 0 ifTrue: [^false]].
	^true.! !

!UUID methodsFor: 'system primitives' stamp: 'ar 3/Feb/2002 19:42:00'!
primMakeUUID
	<primitive: 'primitiveMakeUUID' module: 'UUIDPlugin'>
	UUIDGenerator default generateBytes: self forVersion: 4.! !

!UUID class methodsFor: 'instance creation' stamp: 'dvf 10/Sep/2004 23:10:00'!
fromString: aString
	| object |
	aString size ~= 36 ifTrue: [Error signal].
	object := self nilUUID. 
	object asUUID: aString.
	^object! !

!UUID class methodsFor: 'instance creation' stamp: 'nk 28/Jun/2004 16:10:00'!
nilUUID
	"Must call basicNew: here because I have a non-trivial initialize method."

	^self basicNew: 16! !

!UUID class methodsFor: 'instance creation' stamp: 'jmv 29/Sep/2019 12:55:03'!
numElements
	^16! !
UUIDGenerator initialize!
