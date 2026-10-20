'From Cuis7.9 [latest update: #8149] on 29 August 2026 at 6:52:29 pm'!
'Description '!
!provides: 'Tests-JSON' '1.8.0'!
!requires: 'JSON' '1.24.0' '1.*.*'!
SystemOrganization addCategory: #'Tests-JSON'!


!classDefinition: #JsonDummyTestObject category: #'Tests-JSON'!
Object subclass: #JsonDummyTestObject
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-JSON'!
!classDefinition: 'JsonDummyTestObject class' category: #'Tests-JSON'!
JsonDummyTestObject class
	instanceVariableNames: ''!

!classDefinition: #JsonTests category: #'Tests-JSON'!
TestCase subclass: #JsonTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-JSON'!
!classDefinition: 'JsonTests class' category: #'Tests-JSON'!
JsonTests class
	instanceVariableNames: ''!


!JsonTests commentStamp: '<historical>' prior: 0!
I provide a number of test cases for class Json.!

!JsonDummyTestObject methodsFor: 'as yet unclassified' stamp: 'tonyg 29/Nov/2005, 8:56:00 pm (UTC)'!
= other
	^ other class == self class and: [
		a = other a and: [
		b = other b and: [
		c = other c]]]! !

!JsonDummyTestObject methodsFor: 'as yet unclassified' stamp: 'tonyg 30/Nov/2005, 7:42:00 pm (UTC)'!
jsonWriteOn: s
	Json renderInstanceVariables: {#a. #b. #c} of: self on: s
! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'tonyg 29/Nov/2005, 8:48:00 pm (UTC)'!
a
	"Answer the value of a"

	^ a! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'dkb 25/Dec/2009, 5:56:00 am (UTC)'!
a: anObject
	"Set the value of a"

	a := anObject! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'tonyg 29/Nov/2005, 8:48:00 pm (UTC)'!
b
	"Answer the value of b"

	^ b! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'dkb 25/Dec/2009, 5:56:00 am (UTC)'!
b: anObject
	"Set the value of b"

	b := anObject! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'tonyg 29/Nov/2005, 8:48:00 pm (UTC)'!
c
	"Answer the value of c"

	^ c! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'dkb 25/Dec/2009, 5:56:00 am (UTC)'!
c: anObject
	"Set the value of c"

	c := anObject! !

!JsonDummyTestObject class methodsFor: 'as yet unclassified' stamp: 'tonyg 29/Nov/2005, 8:49:00 pm (UTC)'!
constructFromJson: j
	^ self new a: (j at: 'a'); b: (j at: 'b'); c: (j at: 'c'); yourself! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 29/Nov/2005, 9:03:00 pm (UTC)'!
json: aString equals: aValue
	| readValue |
	readValue := self readFrom: aString.
	self assert: readValue = aValue.! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 29/Nov/2005, 9:03:00 pm (UTC)'!
readFrom: aString
	^ (Json newWithConstructors: {JsonDummyTestObject.}) readFrom: aString readStream
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 17/Aug/2005, 7:24:00 pm (UTC)'!
render: anObject equals: aString
	self assert: (Json render: anObject) = aString! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 29/Nov/2005, 8:51:00 pm (UTC)'!
simpleDummyObject
	^ JsonDummyTestObject new a: 1; b: 2; c: 3; yourself! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 17/Aug/2005, 2:51:00 am (UTC)'!
testArray
	self json: '[]' equals: #().
	self json: '[[]]' equals: #(#()).
	self json: '[[], []]' equals: #(#() #()).
	self json: '["hi", "there"]' equals: #('hi' 'there').
	self json: '[["a", "b", null]]' equals: #(('a' 'b' nil)).! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 17/Aug/2005, 2:42:00 am (UTC)'!
testAtomFalse
	self json: 'false' equals: false.
	self json: '  false' equals: false.
	self json: 'false  ' equals: false.
	self json: '  false  ' equals: false.
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 17/Aug/2005, 2:43:00 am (UTC)'!
testAtomNull
	self json: 'null' equals: nil.
	self json: '  null' equals: nil.
	self json: 'null  ' equals: nil.
	self json: '  null  ' equals: nil.
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 17/Aug/2005, 12:10:00 pm (UTC)'!
testAtomNumber
	self json: '1' equals: 1.
	self json: '0123' equals: 123.
	self json: '1.23e2' equals: 123.
	self json: '-1' equals: -1.
	self json: '-0' equals: 0.
	self json: '[-1.2]' equals: #(-1.2).! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'jmv 4/Nov/2016, 1:04:05 pm (UTC)'!
testAtomSpecialFloats
	| read |
	read _ self readFrom: 'NaN'.
	self assert: read isNaN.
	read _ self readFrom: 'Infinity'.
	self assert: read  = Float infinity.
	read _ self readFrom: '-Infinity'.
	self assert: read  = Float negativeInfinity.! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'jmv 9/Jan/2024, 7:28:38 pm (UTC)'!
testAtomString
	self json: '"hi"' equals: 'hi'.
	self json: '"\""' equals: '"'.
	self json: '"\\"' equals: '\'.
	self json: '""' equals: ''.
	self json: '"a\u0004b"' equals: ({$a. Character codePoint: 4. $b} as: String).
	self json: '"a\nb"' equals: ({$a. Character lf. $b} as: String).! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 17/Aug/2005, 2:36:00 am (UTC)'!
testAtomTrue
	self json: 'true' equals: true.
	self json: '  true' equals: true.
	self json: 'true  ' equals: true.
	self json: '  true  ' equals: true.
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 29/Nov/2005, 8:58:00 pm (UTC)'!
testCtor
	self json: '@JsonDummyTestObject {"a": 1, "b": 2, "c": 3}' equals: self simpleDummyObject.
	self json: (Json render: self simpleDummyObject) equals: self simpleDummyObject.! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'jmv 29/Aug/2026, 9:34:55 pm (UTC)'!
testDictionary
	
	self json: '{}' equals: (OrderedDictionary new).
	self json: '{"a": "a"}' equals: (OrderedDictionary new at: 'a' put: 'a'; yourself).
	self json: '{"a": [[]]}' equals: (OrderedDictionary new at: 'a' put: #(#()); yourself).
	self json: '{"a":"b", "b":"a"}' equals: (OrderedDictionary new add: 'a'->'b'; add: 'b'->'a';yourself).
	self json: '{"a" : {"1":2, "3":4},  "b" : 1}' equals: (OrderedDictionary new
		add: 'a'->(OrderedDictionary new add: '1'->2; add: '3'->4; yourself);
		add: 'b'->1; yourself).
	self json: '{"a" : {"1":2},  "b" : 1}' equals: (OrderedDictionary new
		add: 'a'->(OrderedDictionary new add: '1'->2; yourself);
		add: 'b'->1; yourself).
	self json: '{"a" : {},  "b" : 1}' equals: (OrderedDictionary new
		add: 'a'->(OrderedDictionary new);
		add: 'b'->1; yourself).
	
	self json: '{}' equals: (JsonObject new).
	self json: '{"a": "a"}' equals: (JsonObject new at: 'a' put: 'a'; yourself).
	self json: '{"a": [[]]}' equals: (JsonObject new at: 'a' put: #(#()); yourself).
	self json: '{"a":"b", "b":"a"}' equals: (JsonObject new add: 'a'->'b'; add: 'b'->'a';yourself).
	self json: '{"a" : {"1":2, "3":4},  "b" : 1}' equals: (JsonObject new
		at: 'a' put: (JsonObject new at: '1' put: 2; at: '3' put: 4; yourself);
		at: 'b' put: 1; yourself).
	self json: '{"a" : {"1":2},  "b" : 1}' equals: (JsonObject new
		at: 'a' put: (JsonObject new at: '1' put: 2; yourself);
		at: 'b' put: 1; yourself).
	self json: '{"a" : {},  "b" : 1}' equals: (JsonObject new
		at: 'a' put: (JsonObject new);
		at: 'b' put: 1; yourself).! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'cwp 19/Oct/2006, 10:06:00 pm (UTC)'!
testMissingCtor
	self 
		should: [self readFrom: '@Missing[]']
		raise: JsonSyntaxError
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'cwp 19/Oct/2006, 10:06:00 pm (UTC)'!
testMissingCtorNoMap
	self 
		should: [Json new readFrom: '@Missing[]' readStream]
		raise: JsonSyntaxError! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'JEP 23/Dec/2025, 4:52:27 pm (UTC)'!
testMissingValue
	self 
		should: [self readFrom: '[']
		raise: JsonSyntaxError.
		
	self 
		should: [self readFrom: '{"key":']
		raise: JsonSyntaxError
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'jmv 9/Jan/2024, 7:28:49 pm (UTC)'!
testStringWithUnicode
	| unicodeString |
	unicodeString := UnicodeString with: (Character codePoint: 16r263A).
	self json: '"\u263A"' equals: unicodeString.
	self json: '"', unicodeString, '"' equals: unicodeString.
	self render: unicodeString equals: '"', unicodeString, '"'! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'matthias.berth 19/Nov/2009, 1:21:00 pm (UTC)'!
testWriteAssociation
	self render: 'key' -> 'value' equals: '"key": "value"'.
	self render: 'key' -> 2 equals: '"key": 2'.
	"keys should be strings"
	self render: 42 -> 2 equals: '"42": 2'.
	"try to do _something_ for more complex keys"
	self render: #(42 43 44) -> 2 equals:  '"#(42 43 44)": 2'.
	
	
	! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'jmv 9/Jan/2024, 7:28:55 pm (UTC)'!
testWriteString
	self render: '"' equals: '"\""'.
	self render: '\' equals: '"\\"'.
	self render: 'hi' equals: '"hi"'.
	self render: ({$a. Character lf. $b} as: String) equals: '"a\nb"'.
	self render: ({$a. Character codePoint: 4. $b} as: String) equals: '"a\u0004b"'.
	self render: 'agüita' equals: '"agüita"'.
	self render: 'ñandú' equals: '"ñandú"'.! !
