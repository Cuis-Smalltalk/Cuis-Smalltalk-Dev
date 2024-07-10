'From Cuis7.1 [latest update: #6512] on 10 July 2024 at 3:25:51 pm'!
!classDefinition: #IniFile category: #'System-Support'!
Object subclass: #IniFile
	instanceVariableNames: 'globalSection namedSections'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!IniFile commentStamp: 'jmv 7/10/2024 09:10:24' prior: 0!
An IniFile is a collection of options grouped in IniFileSections meant to be saved as an .ini file.
IniFiles are used for configuration of software.!

!classDefinition: #IniFileSection category: #'System-Support'!
Object subclass: #IniFileSection
	instanceVariableNames: 'name entries'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!IniFileSection commentStamp: '<historical>' prior: 0!
Part of an IniFile.!


!IniFile methodsFor: 'accessing sections and entries' stamp: 'jmv 7/10/2024 10:20:04'!
atSection: sectionName entry: entryName
	"Answer the value, or nil if it hasn't been set."

	^self atSection: sectionName entry: entryName orDefault: nil.
! !

!IniFile methodsFor: 'accessing sections and entries' stamp: 'jmv 7/10/2024 10:18:59'!
atSection: sectionName entry: entryName orDefault: aBlock
	"If section or entry is missing, answer the result of evaluating aBlock,
	but don't add the section or entry."

	^(namedSections at: sectionName ifAbsent: [ ^aBlock value ])
		at: entryName orDefault: aBlock.! !

!IniFile methodsFor: 'accessing sections and entries' stamp: 'jmv 7/10/2024 10:18:39'!
atSection: sectionName entry: entryName orPutDefault: aBlock
	"If section or entry is missing, answer the result of evaluating aBlock,
	AND add it as the value of the section/entry."

	^(self ensureSectionNamed: sectionName)
		at: entryName orPutDefault: aBlock.! !

!IniFile methodsFor: 'accessing sections and entries' stamp: 'jmv 7/10/2024 10:31:52'!
atSection: sectionName entry: entryName put: aValue
	"Just store the value."

	^(self ensureSectionNamed: sectionName)
		at: entryName put: aValue.! !

!IniFile methodsFor: 'accessing sections and entries' stamp: 'jmv 7/10/2024 10:29:40'!
ensureSectionNamed: sectionName
	"Make it an empty section if not there."

	^namedSections at: sectionName ifAbsentPut: [ IniFileSection newNamed: sectionName ].
! !

!IniFile methodsFor: 'accessing sections and entries' stamp: 'jmv 7/10/2024 10:20:42'!
sectionNamed: sectionName
	"Answer nil if not there."

	^namedSections at: sectionName ifAbsent: nil.
! !

!IniFile methodsFor: 'accessing global entries' stamp: 'jmv 7/10/2024 10:34:29'!
atGlobalEntry: entryName
	^self atGlobalEntry: entryName orDefault: nil.! !

!IniFile methodsFor: 'accessing global entries' stamp: 'jmv 7/10/2024 10:28:22'!
atGlobalEntry: entryName orDefault: aBlock
	"If section or entry is missing, answer the result of evaluating aBlock,
	but don't add the section or entry."

	globalSection isNil ifTrue: [ ^aBlock value ].
	^globalSection at: entryName orDefault: aBlock.! !

!IniFile methodsFor: 'accessing global entries' stamp: 'jmv 7/10/2024 10:30:34'!
atGlobalEntry: entryName orPutDefault: aBlock
	"If section or entry is missing, answer the result of evaluating aBlock,
	AND add it as the value of the section/entry."

	^self ensureGlobalSection
		at: entryName orPutDefault: aBlock.
! !

!IniFile methodsFor: 'accessing global entries' stamp: 'jmv 7/10/2024 10:29:34'!
ensureGlobalSection
	"Make it an empty section if not there."

	^globalSection ifNil: [ globalSection := IniFileSection newGlobal ].
! !

!IniFile methodsFor: 'accessing global entries' stamp: 'jmv 7/10/2024 10:32:24'!
globalEntry: entryName put: aValue
	"Just store the value."

	^self ensureGlobalSection
		at: entryName put: aValue.! !

!IniFile methodsFor: 'accessing global entries' stamp: 'jmv 7/10/2024 10:37:02'!
globalSection
	"Answer nil if not there."

	^globalSection! !

!IniFile methodsFor: 'initialization' stamp: 'jmv 7/10/2024 09:25:44'!
initialize
	globalSection := nil.
	namedSections := OrderedDictionary new.! !

!IniFile methodsFor: 'writing' stamp: 'jmv 7/10/2024 11:23:33'!
writeOn: aStream

	globalSection ifNotNil: [
		globalSection writeOn: aStream.
		aStream newLine ].
	namedSections
		do: [:ea | ea writeOn: aStream]
		separatedBy: [ aStream newLine ].
! !


!IniFile class methodsFor: 'instance creation' stamp: 'jmv 7/10/2024 10:50:33'!
addSettingFrom: lineString to: section
	| stream key value |
	stream := lineString readStream.
	key := (stream upTo: $=) withBlanksTrimmed.
	value := (stream upToEnd) withBlanksTrimmed.
	section at: key put: (value isEmpty ifTrue: [nil] ifFalse: [value]).! !

!IniFile class methodsFor: 'instance creation' stamp: 'jmv 7/10/2024 11:01:46'!
newSectionIn: anIniFile from: aString
	| stream |
	stream := aString readStream.
	stream upTo: $[.
	^ anIniFile ensureSectionNamed: (stream upTo: $])! !

!IniFile class methodsFor: 'instance creation' stamp: 'jmv 7/10/2024 11:06:28'!
readFrom: aStream
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
	
	^ ini! !


!IniFileSection methodsFor: 'accessing' stamp: 'jmv 7/10/2024 10:21:05'!
at: entryName
	"Answer the value, or nil if it hasn't been set."

	^entries at: entryName ifAbsent: nil.
! !

!IniFileSection methodsFor: 'accessing' stamp: 'jmv 7/10/2024 10:21:20'!
at: entryName orDefault: aBlock 
	"If entry is missing, answer the result of evaluating aBlock,
	but don't add the section or entry."

	^entries at: entryName ifAbsent: aBlock.
! !

!IniFileSection methodsFor: 'accessing' stamp: 'jmv 7/10/2024 10:21:48'!
at: entryName orPutDefault: aBlock 
	"If entry is missing, answer the result of evaluating aBlock,
	AND add it as the value of the entry."

	^entries at: entryName ifAbsentPut: aBlock.
! !

!IniFileSection methodsFor: 'accessing' stamp: 'jmv 7/10/2024 10:21:57'!
at: entryName put: aValue
	"Just store the value."

	entries at: entryName put: aValue.
	^aValue.! !

!IniFileSection methodsFor: 'initialization' stamp: 'jmv 7/10/2024 09:17:45'!
initializeGlobal
	"Initialize a Global section. There should be, at most, one of these in an IniFile."

	name := nil.
	entries := OrderedDictionary new.! !

!IniFileSection methodsFor: 'initialization' stamp: 'jmv 7/10/2024 09:17:56'!
initializeNamed: aString
	"Initialize a Global section. There should be, at most, one of these in an IniFile."

	name := aString.
	entries := OrderedDictionary new.! !

!IniFileSection methodsFor: 'writing' stamp: 'jmv 7/10/2024 11:22:29'!
writeOn: aStream
	name ifNotNil: [ 
		aStream
			nextPut: $[; nextPutAll: name; nextPut: $]; newLine ].
	entries associationsDo: [ :assoc |
		aStream nextPutAll: assoc key; nextPut: $=; nextPutAll: (assoc value ifNil: ['']); newLine ].
! !


!IniFileSection class methodsFor: 'instance creation' stamp: 'jmv 7/10/2024 09:18:33'!
new
	^ self shouldNotImplement! !

!IniFileSection class methodsFor: 'instance creation' stamp: 'jmv 7/10/2024 09:18:49'!
newGlobal
	^ self basicNew initializeGlobal! !

!IniFileSection class methodsFor: 'instance creation' stamp: 'jmv 7/10/2024 09:19:00'!
newNamed: aString
	^ self basicNew initializeNamed: aString! !


!TestCase reorganize!
('accessing' errored: methodForTest raisedError resources selector setTestSelector:)
('assertions' assert: assert:changes: assert:changes:by: assert:changes:from:to: assert:description: assert:description:resumable: assert:doesNotChange: assert:equals: assert:includes: comparingStringBetween:and: deny: deny:description: deny:description:resumable: should: should:description: should:notTakeMoreThan: should:raise: should:raise:description: should:raise:withExceptionDo: should:raise:withExceptionDo:description: should:raise:withMessageText: shouldFail: shouldnt: shouldnt:description: shouldnt:raise: shouldnt:raise:description: shouldntFail:)
('debugging' debug debugAsFailure debugAsFailureIfCanNot: debugInstance)
('debugging - private' canNotDebugMethodErrorDescription ifCanNotDebugDo: openDebuggerOnFailingTestMethod signalCanNotDebugMethod)
('dependencies' addDependentToHierachy: removeDependentFromHierachy:)
('failing' defaultFailDescription expectedFailures fail failWith: failureString signalFailure:)
('log' failureLog logFailure:)
('printing' printOn:)
('running' run run:skipExpectedFailures: runCase runCaseAsFailure)
('running - private' incrementAssertionCount performTest)
('setUp/tearDown' initialize setUp setUpResources tearDown tearDownResources)
('testing' is: isLogging isSameAs:)
('tests')
!

