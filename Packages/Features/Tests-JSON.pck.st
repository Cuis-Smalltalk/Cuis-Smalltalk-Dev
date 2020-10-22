'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 9:27:20 pm'!
'Description '!
!provides: 'Tests-JSON' 1 0!
!requires: 'JSON' 1 19 nil!
SystemOrganization addCategory: 'Tests-JSON'!


!classDefinition: #JsonTests category: 'Tests-JSON'!
TestCase subclass: #JsonTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-JSON'!
!classDefinition: 'JsonTests class' category: 'Tests-JSON'!
JsonTests class
	instanceVariableNames: ''!

!classDefinition: #JsonDummyTestObject category: 'Tests-JSON'!
Object subclass: #JsonDummyTestObject
	instanceVariableNames: 'a b c'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-JSON'!
!classDefinition: 'JsonDummyTestObject class' category: 'Tests-JSON'!
JsonDummyTestObject class
	instanceVariableNames: ''!


!JsonTests commentStamp: '<historical>' prior: 0!
I provide a number of test cases for class Json.!

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 11/29/2005 18:03'!
json: aString equals: aValue
	| readValue |
	readValue := self readFrom: aString.
	self assert: readValue = aValue.! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 11/29/2005 18:03'!
readFrom: aString
	^ (Json newWithConstructors: {JsonDummyTestObject.}) readFrom: aString readStream
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 8/17/2005 16:24'!
render: anObject equals: aString
	self assert: (Json render: anObject) = aString! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 11/29/2005 17:51'!
simpleDummyObject
	^ JsonDummyTestObject new a: 1; b: 2; c: 3; yourself! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 8/16/2005 23:51'!
testArray
	self json: '[]' equals: #().
	self json: '[[]]' equals: #(#()).
	self json: '[[], []]' equals: #(#() #()).
	self json: '["hi", "there"]' equals: #('hi' 'there').
	self json: '[["a", "b", null]]' equals: #(('a' 'b' nil)).! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 8/16/2005 23:42'!
testAtomFalse
	self json: 'false' equals: false.
	self json: '  false' equals: false.
	self json: 'false  ' equals: false.
	self json: '  false  ' equals: false.
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 8/16/2005 23:43'!
testAtomNull
	self json: 'null' equals: nil.
	self json: '  null' equals: nil.
	self json: 'null  ' equals: nil.
	self json: '  null  ' equals: nil.
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 8/17/2005 09:10'!
testAtomNumber
	self json: '1' equals: 1.
	self json: '0123' equals: 123.
	self json: '1.23e2' equals: 123.
	self json: '-1' equals: -1.
	self json: '-0' equals: 0.
	self json: '[-1.2]' equals: #(-1.2).! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'jmv 11/4/2016 10:04:05'!
testAtomSpecialFloats
	| read |
	read _ self readFrom: 'NaN'.
	self assert: read isNaN.
	read _ self readFrom: 'Infinity'.
	self assert: read  = Float infinity.
	read _ self readFrom: '-Infinity'.
	self assert: read  = Float negativeInfinity.! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'jmv 9/5/2016 16:53:32'!
testAtomString
	self json: '"hi"' equals: 'hi'.
	self json: '"\""' equals: '"'.
	self json: '"\\"' equals: '\'.
	self json: '""' equals: ''.
	self json: '"a\u0004b"' equals: ({$a. Character numericValue: 4. $b} as: String).
	self json: '"a\nb"' equals: ({$a. Character lf. $b} as: String).! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 8/16/2005 23:36'!
testAtomTrue
	self json: 'true' equals: true.
	self json: '  true' equals: true.
	self json: 'true  ' equals: true.
	self json: '  true  ' equals: true.
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'tonyg 11/29/2005 17:58'!
testCtor
	self json: '@JsonDummyTestObject {"a": 1, "b": 2, "c": 3}' equals: self simpleDummyObject.
	self json: (Json render: self simpleDummyObject) equals: self simpleDummyObject.! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'Igor.Stasenko 12/27/2009 08:19'!
testDictionary
	self json: '{}' equals: (JsonObject new).
	self json: '{"a": "a"}' equals: (JsonObject new at: 'a' put: 'a'; yourself).
	self json: '{"a": [[]]}' equals: (JsonObject new at: 'a' put: #(#()); yourself).
	self json: '{"a":"b", "b":"a"}' equals: (JsonObject new add: 'a'->'b'; add: 'b'->'a';yourself).! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'cwp 10/19/2006 19:06'!
testMissingCtor
	self 
		should: [self readFrom: '@Missing[]']
		raise: JsonSyntaxError
! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'cwp 10/19/2006 19:06'!
testMissingCtorNoMap
	self 
		should: [Json new readFrom: '@Missing[]' readStream]
		raise: JsonSyntaxError! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'jmv 8/28/2013 12:29'!
testStringWithUnicode
	| unicodeString |
	unicodeString _ '(Unicode value: 16r263A)' asString.
	unicodeString _ String streamContents: [ :strm |
		String addUnicodeCodePoint: 16r263A to: strm hex: true ].
	self json: '"\u263A"' equals: unicodeString.
	self json: '"', unicodeString, '"' equals: unicodeString.
	self render: unicodeString equals: '"', unicodeString, '"'! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'matthias.berth 11/19/2009 10:21'!
testWriteAssociation
	self render: 'key' -> 'value' equals: '"key": "value"'.
	self render: 'key' -> 2 equals: '"key": 2'.
	"keys should be strings"
	self render: 42 -> 2 equals: '"42": 2'.
	"try to do _something_ for more complex keys"
	self render: #(42 43 44) -> 2 equals:  '"#(42 43 44)": 2'.
	
	
	! !

!JsonTests methodsFor: 'as yet unclassified' stamp: 'jmv 9/5/2016 16:53:37'!
testWriteString
	self render: '"' equals: '"\""'.
	self render: '\' equals: '"\\"'.
	self render: 'hi' equals: '"hi"'.
	self render: ({$a. Character lf. $b} as: String) equals: '"a\nb"'.
	self render: ({$a. Character numericValue: 4. $b} as: String) equals: '"a\u0004b"'.! !

!JsonDummyTestObject methodsFor: 'as yet unclassified' stamp: 'tonyg 11/29/2005 17:56'!
= other
	^ other class == self class and: [
		a = other a and: [
		b = other b and: [
		c = other c]]]! !

!JsonDummyTestObject methodsFor: 'as yet unclassified' stamp: 'tonyg 11/30/2005 16:42'!
jsonWriteOn: s
	Json renderInstanceVariables: {#a. #b. #c} of: self on: s
! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'tonyg 11/29/2005 17:48'!
a
	"Answer the value of a"

	^ a! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'dkb 12/25/2009 02:56'!
a: anObject
	"Set the value of a"

	a := anObject! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'tonyg 11/29/2005 17:48'!
b
	"Answer the value of b"

	^ b! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'dkb 12/25/2009 02:56'!
b: anObject
	"Set the value of b"

	b := anObject! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'tonyg 11/29/2005 17:48'!
c
	"Answer the value of c"

	^ c! !

!JsonDummyTestObject methodsFor: 'accessing' stamp: 'dkb 12/25/2009 02:56'!
c: anObject
	"Set the value of c"

	c := anObject! !

!JsonDummyTestObject class methodsFor: 'as yet unclassified' stamp: 'tonyg 11/29/2005 17:49'!
constructFromJson: j
	^ self new a: (j at: 'a'); b: (j at: 'b'); c: (j at: 'c'); yourself! !
