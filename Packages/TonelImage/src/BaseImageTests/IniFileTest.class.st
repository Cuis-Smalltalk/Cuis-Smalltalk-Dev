Class {
	#name : 'IniFileTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-Support',
	#package : 'BaseImageTests',
	#tag : 'System-Support'
}

{ #category : 'tests' }
IniFileTest >> testBasic [
	| ini section |
	ini := IniFile new.

	section := ini ensureSectionNamed: 'foo1'.
	self assert: section notNil.
	section at: 'bar' put: 'baz1'.
	self assert: (section at: 'bar') equals: 'baz1'.
	self assert: (ini atSection: 'foo1' entry: 'bar') equals: 'baz1'.

	ini atSection: 'foo2' entry: 'bar' put: 'baz2'.
	self assert: (ini atSection: 'foo1' entry: 'bar') equals: 'baz1'.
	self assert: ((ini sectionNamed: 'foo1') at: 'bar') equals: 'baz1'.
	self assert: (ini atSection: 'foo2' entry: 'bar') equals: 'baz2'.
	self assert: ((ini sectionNamed: 'foo2') at: 'bar') equals: 'baz2'.
]

{ #category : 'tests' }
IniFileTest >> testBasicGlobal [
	| ini |
	ini := IniFile new.

	ini globalEntry: 'bar' put: 'baz1'.
	self assert: (ini globalSection at: 'bar') equals: 'baz1'.
	self assert: (ini atGlobalEntry: 'bar') equals: 'baz1'.

]

{ #category : 'tests' }
IniFileTest >> testMissingEntry [
	| ini section |
	ini := IniFile new.

	section := ini ensureSectionNamed: 'foo1'.
	self assert: section notNil.
	self assert: (section at: 'bar') isNil.
	self assert: (section at: 'bar' orDefault: 'whoknows1?') equals: 'whoknows1?'.
	self assert: (ini atSection: 'foo1' entry: 'bar' orDefault: 'whoknows2?') equals: 'whoknows2?'.
	self assert: (ini atSection: 'foo3' entry: 'bar') isNil.
	self assert: (ini atSection: 'foo3' entry: 'bar' orDefault: 'whoknows3?') equals: 'whoknows3?'.
]

{ #category : 'tests' }
IniFileTest >> testMissingEntryStoringDefault [
	| ini section |
	ini := IniFile new.

	section := ini ensureSectionNamed: 'foo1'.
	self assert: section notNil.
	self assert: (section at: 'bar1' orPutDefault: 'newDefault') equals: 'newDefault'.
	self assert: (ini atSection: 'foo1' entry: 'bar2' orPutDefault: 'newDefault2') equals: 'newDefault2'.
	self assert: (section at: 'bar1' orDefault: 'whoknows?') equals: 'newDefault'.
	self assert: (ini atSection: 'foo1' entry: 'bar2' orDefault: 'whoknows?') equals: 'newDefault2'.
]

{ #category : 'tests' }
IniFileTest >> testMissingGlobalEntry [
	| ini |
	ini := IniFile new.

	self assert: ini globalSection isNil.
	ini ensureGlobalSection.
	self assert: ini globalSection notNil.
	self assert: (ini globalSection at: 'bar') isNil.
	self assert: (ini globalSection at: 'bar' orDefault: 'whoknows1?') equals: 'whoknows1?'.
	self assert: (ini atGlobalEntry: 'bar' orDefault: 'whoknows2?') equals: 'whoknows2?'.
	self assert: (ini atGlobalEntry: 'bar') isNil.
	self assert: (ini atGlobalEntry: 'bar' orDefault: 'whoknows3?') equals: 'whoknows3?'.
]

{ #category : 'tests' }
IniFileTest >> testMissingGlobalEntryStoringDefault1 [
	| ini section |
	ini := IniFile new.

	section := ini ensureGlobalSection.
	self assert: section notNil.
	self assert: (section at: 'bar1' orPutDefault: 'newDefault') equals: 'newDefault'.
	self assert: (ini atSection: 'foo1' entry: 'bar2' orPutDefault: 'newDefault2') equals: 'newDefault2'.
	self assert: (section at: 'bar1' orDefault: 'whoknows?') equals: 'newDefault'.
	self assert: (ini atSection: 'foo1' entry: 'bar2' orDefault: 'whoknows?') equals: 'newDefault2'.
]

{ #category : 'tests' }
IniFileTest >> testMissingGlobalEntryStoringDefault2 [
	| ini |
	ini := IniFile new.

	self assert: (ini atGlobalEntry: 'bar1' orPutDefault: 'newDefault') equals: 'newDefault'.
	self assert: (ini atGlobalEntry: 'bar2' orPutDefault: 'newDefault2') equals: 'newDefault2'.
	self assert: (ini atGlobalEntry: 'bar1' orDefault: 'whoknows?') equals: 'newDefault'.
	self assert: (ini atGlobalEntry: 'bar2' orDefault: 'whoknows?') equals: 'newDefault2'.
]

{ #category : 'tests' }
IniFileTest >> testMissingGlobalSection [
	| ini |
	ini := IniFile new.

	self assert: ini globalSection isNil.
	ini ensureGlobalSection.
	self deny: ini globalSection isNil.
]

{ #category : 'tests' }
IniFileTest >> testMissingSection [
	| ini |
	ini := IniFile new.

	self assert: (ini sectionNamed: 'foo1') isNil.
	ini ensureSectionNamed: 'foo1'.
	self deny: (ini sectionNamed: 'foo1') isNil.
]

{ #category : 'tests' }
IniFileTest >> testReadEmptyValue [
	| content ini |
	content := '[foo]
key1 =      '.

	ini := IniFile readFrom: content readStream.
	self assert: ((ini sectionNamed: 'foo') at: 'key1') isNil
]

{ #category : 'tests' }
IniFileTest >> testReadGlobalAndTwoNamedSections [
	| content ini |
	content := '
key1 = val1global

key2 = val2global

[bar]
;comment

key1 = val1bar

key2 = val2
;key2 = oldvalue

;foo heading comment
[foo]
key1=    val1
key2= val with a space in it

'.

	ini := IniFile readFrom: content readStream.
	self assert: (ini atGlobalEntry: 'key1') = 'val1global'.
	self assert: (ini atGlobalEntry: 'key2') = 'val2global'.
	self assert: (ini atSection: 'foo' entry: 'key1') = 'val1'.
	self assert: (ini atSection: 'foo' entry: 'key2') = 'val with a space in it'.
	self assert: (ini atSection: 'bar' entry: 'key1') = 'val1bar'.
	self assert: (ini atSection: 'bar' entry: 'key2') = 'val2'.

]

{ #category : 'tests' }
IniFileTest >> testReadTwoSections [
	| content ini |
	content := '

[bar]
;comment

key1 = val1bar

key2 = val2
;key2 = oldvalue

;foo heading comment
[foo]
key1=    val1
key2= val with a space in it

'.

	ini := IniFile readFrom: content readStream.
	self assert: ini globalSection isNil.
	self assert: (ini atSection: 'foo' entry: 'key1') = 'val1'.
	self assert: (ini atSection: 'foo' entry: 'key2') = 'val with a space in it'.
	self assert: (ini atSection: 'bar' entry: 'key1') = 'val1bar'.
	self assert: (ini atSection: 'bar' entry: 'key2') = 'val2'.

]

{ #category : 'tests' }
IniFileTest >> testWriteGlobalSection [
	| out written globalSection |
	out := IniFile new.
	out globalEntry: 'key1' put: 'val1'.
	out globalEntry: 'key2' put: 'val2'.
	written := String streamContents: [ :stream | out writeOn: stream ].
	
	globalSection := (IniFile readFrom: written readStream) globalSection.
	self assert: (globalSection at: 'key1') = 'val1'.
	self assert: (globalSection at: 'key2') = 'val2'.
]

{ #category : 'tests' }
IniFileTest >> testWriteMultipleSections [
	| out written in |
	out := IniFile new.
	(out ensureSectionNamed: 'foo') at: 'key1' put: 'val1'.
	(out ensureSectionNamed: 'foo') at: 'key2' put: 'val2'.
	(out ensureSectionNamed: 'bar') at: 'key1' put: 'val1x'.
	(out ensureSectionNamed: 'bar') at: 'key2' put: 'val2y'.
	written := String streamContents: [ :stream | out writeOn: stream ].
	
	in := IniFile readFrom: written readStream.
	self assert: ((in sectionNamed: 'foo') at: 'key1') = 'val1'.
	self assert: ((in sectionNamed: 'foo') at: 'key2') = 'val2'.
	self assert: ((in sectionNamed: 'bar') at: 'key1') = 'val1x'.
	self assert: ((in sectionNamed: 'bar') at: 'key2') = 'val2y'.
]

{ #category : 'tests' }
IniFileTest >> testWriteMultipleSections2 [
	| out written in |
	out := IniFile new.
	out globalEntry: 'key1' put: 'g1'.
	out globalEntry: 'key2' put: 'g2'.
	(out ensureSectionNamed: 'foo') at: 'key1' put: 'val1'.
	(out ensureSectionNamed: 'foo') at: 'key2' put: 'val2'.
	(out ensureSectionNamed: 'bar') at: 'key1' put: 'val1x'.
	(out ensureSectionNamed: 'bar') at: 'key2' put: 'val2y'.
	written := String streamContents: [ :stream | out writeOn: stream ].
	
	in := IniFile readFrom: written readStream.
	self assert: (in atGlobalEntry: 'key1') = 'g1'.
	self assert: (in atGlobalEntry: 'key2') = 'g2'.
	self assert: ((in sectionNamed: 'foo') at: 'key1') = 'val1'.
	self assert: ((in sectionNamed: 'foo') at: 'key2') = 'val2'.
	self assert: ((in sectionNamed: 'bar') at: 'key1') = 'val1x'.
	self assert: ((in sectionNamed: 'bar') at: 'key2') = 'val2y'.
]

{ #category : 'tests' }
IniFileTest >> testWriteNilGlobalValue [
	| ini written expected |
	ini := IniFile new.
	ini globalEntry: 'key1' put: nil.
	written := String streamContents: [ :stream | ini writeOn: stream ].
	expected := 'key1=
'.
	self assert: written equals: expected.
]

{ #category : 'tests' }
IniFileTest >> testWriteNilValue [
	| ini written expected |
	ini := IniFile new.
	ini atSection: 'foo' entry: 'key1' put: nil.
	written := String streamContents: [ :stream | ini writeOn: stream ].
	expected := '[foo]
key1=

'.
	self assert: written equals: expected.
]

{ #category : 'tests' }
IniFileTest >> testWriteSingleSection [
	| out written foo |
	out := IniFile new.
	out atSection: 'foo' entry: 'key1' put: 'val1'.
	out atSection: 'foo' entry: 'key2' put: 'val2'.
	written := String streamContents: [ :stream | out writeOn: stream ].
	
	foo := (IniFile readFrom: written readStream) sectionNamed: 'foo'.
	self assert: (foo at: 'key1') = 'val1'.
	self assert: (foo at: 'key2') = 'val2'.
]
