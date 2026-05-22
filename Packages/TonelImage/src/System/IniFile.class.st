"
An IniFile is a collection of options grouped in IniFileSections meant to be saved as an .ini file.
IniFiles are used for configuration of software.
"
Class {
	#name : 'IniFile',
	#superclass : 'Object',
	#instVars : [
		'globalSection',
		'namedSections'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'instance creation' }
IniFile class >> addSettingFrom: lineString to: section [
	| stream key value |
	stream := lineString readStream.
	key := (stream upTo: $=) withBlanksTrimmed.
	value := (stream upToEnd) withBlanksTrimmed.
	section at: key put: (value isEmpty ifTrue: [nil] ifFalse: [value]).
]

{ #category : 'instance creation' }
IniFile class >> newSectionIn: anIniFile from: aString [
	| stream |
	stream := aString readStream.
	stream upTo: $[.
	^ anIniFile ensureSectionNamed: (stream upTo: $])
]

{ #category : 'instance creation' }
IniFile class >> readFrom: aStream [
	| section line ini |
	section := nil.
	ini := self new.
	
	[aStream atEnd] whileFalse:
		[line := aStream nextLine withBlanksTrimmed.
		line isEmpty ifFalse: [
			line first
				caseOf: {
					[ $[ ] -> [section := self newSectionIn: ini from: line].
					[ $; ] -> ["ignore comment line"].
					}
				otherwise: [
					self addSettingFrom: line to: (section ifNil: [ini ensureGlobalSection]) 					]]].
	
	^ ini
]

{ #category : 'accessing global entries' }
IniFile >> atGlobalEntry: entryName [
	^self atGlobalEntry: entryName orDefault: nil.
]

{ #category : 'accessing global entries' }
IniFile >> atGlobalEntry: entryName orDefault: aBlock [
	"If section or entry is missing, answer the result of evaluating aBlock,
	but don't add the section or entry."

	globalSection isNil ifTrue: [ ^aBlock value ].
	^globalSection at: entryName orDefault: aBlock.
]

{ #category : 'accessing global entries' }
IniFile >> atGlobalEntry: entryName orPutDefault: aBlock [
	"If section or entry is missing, answer the result of evaluating aBlock,
	AND add it as the value of the section/entry."

	^self ensureGlobalSection
		at: entryName orPutDefault: aBlock.

]

{ #category : 'accessing sections and entries' }
IniFile >> atSection: sectionName entry: entryName [
	"Answer the value, or nil if it hasn't been set."

	^self atSection: sectionName entry: entryName orDefault: nil.

]

{ #category : 'accessing sections and entries' }
IniFile >> atSection: sectionName entry: entryName orDefault: aBlock [
	"If section or entry is missing, answer the result of evaluating aBlock,
	but don't add the section or entry."

	^(namedSections at: sectionName ifAbsent: [ ^aBlock value ])
		at: entryName orDefault: aBlock.
]

{ #category : 'accessing sections and entries' }
IniFile >> atSection: sectionName entry: entryName orPutDefault: aBlock [
	"If section or entry is missing, answer the result of evaluating aBlock,
	AND add it as the value of the section/entry."

	^(self ensureSectionNamed: sectionName)
		at: entryName orPutDefault: aBlock.
]

{ #category : 'accessing sections and entries' }
IniFile >> atSection: sectionName entry: entryName put: aValue [
	"Just store the value."

	^(self ensureSectionNamed: sectionName)
		at: entryName put: aValue.
]

{ #category : 'accessing global entries' }
IniFile >> ensureGlobalSection [
	"Make it an empty section if not there."

	^globalSection ifNil: [ globalSection := IniFileSection newGlobal ].

]

{ #category : 'accessing sections and entries' }
IniFile >> ensureSectionNamed: sectionName [
	"Make it an empty section if not there."

	^namedSections at: sectionName ifAbsentPut: [ IniFileSection newNamed: sectionName ].

]

{ #category : 'accessing global entries' }
IniFile >> globalEntry: entryName put: aValue [
	"Just store the value."

	^self ensureGlobalSection
		at: entryName put: aValue.
]

{ #category : 'accessing global entries' }
IniFile >> globalSection [
	"Answer nil if not there."

	^globalSection
]

{ #category : 'initialization' }
IniFile >> initialize [
	globalSection := nil.
	namedSections := OrderedDictionary new.
]

{ #category : 'accessing sections and entries' }
IniFile >> sectionNamed: sectionName [
	"Answer nil if not there."

	^namedSections at: sectionName ifAbsent: nil.

]

{ #category : 'accessing sections and entries' }
IniFile >> sectionNames [
	"Answer the names of the sections. Note these are in order as they appear in the file."

	^namedSections keys

]

{ #category : 'writing' }
IniFile >> writeOn: aStream [
	
	globalSection ifNotNil: [
		globalSection writeOn: aStream.
		((globalSection size > 0) and:[namedSections size > 0])
			ifTrue:[ aStream newLine ]].
	namedSections
		do: [:aSection |
			aSection writeOn: aStream.
			aStream newLine ] separatedBy: [aStream newLine].
]
