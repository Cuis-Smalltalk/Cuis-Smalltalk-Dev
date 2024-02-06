'From Cuis6.3 [latest update: #6213] on 6 February 2024 at 10:26:48 am'!
'Description '!
!provides: 'Tests-Identities-UUID' 1 1!
!requires: 'Identities-UUID' 1 8 nil!
SystemOrganization addCategory: #'Tests-Identities-UUID'!


!classDefinition: #UUIDPrimitivesTest category: #'Tests-Identities-UUID'!
TestCase subclass: #UUIDPrimitivesTest
	instanceVariableNames: ''
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Tests-Identities-UUID'!
!classDefinition: 'UUIDPrimitivesTest class' category: #'Tests-Identities-UUID'!
UUIDPrimitivesTest class
	instanceVariableNames: ''!

!classDefinition: #UUIDTest category: #'Tests-Identities-UUID'!
TestCase subclass: #UUIDTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Identities-UUID'!
!classDefinition: 'UUIDTest class' category: #'Tests-Identities-UUID'!
UUIDTest class
	instanceVariableNames: ''!


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

!UUIDPrimitivesTest methodsFor: 'tests' stamp: 'jmv 2/6/2024 10:12:47'!
testDuplicationsKinda
	| check size |

	size := 5000.
	check := Set newWithRoomForMoreThan: size.
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
