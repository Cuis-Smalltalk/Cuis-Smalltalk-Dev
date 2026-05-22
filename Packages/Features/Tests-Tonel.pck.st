'From Cuis7.7 [latest update: #7777] on 22 May 2026 at 00:00:00 am'!
'Description Tests for Cuis Tonel export support.'!
!provides: 'Tests-Tonel' 1 1!
!requires: 'Tonel' 1 1 nil!
SystemOrganization addCategory: #'Tests-Tonel'!


!classDefinition: #TonelWriterTest category: #'Tests-Tonel'!
TestCase subclass: #TonelWriterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Tonel'!
!classDefinition: 'TonelWriterTest class' category: #'Tests-Tonel'!
TonelWriterTest class
	instanceVariableNames: ''!

!classDefinition: #TonelReaderTest category: #'Tests-Tonel'!
TestCase subclass: #TonelReaderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Tonel'!
!classDefinition: 'TonelReaderTest class' category: #'Tests-Tonel'!
TonelReaderTest class
	instanceVariableNames: ''!

!classDefinition: #TonelImporterTest category: #'Tests-Tonel'!
TestCase subclass: #TonelImporterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Tonel'!
!classDefinition: 'TonelImporterTest class' category: #'Tests-Tonel'!
TonelImporterTest class
	instanceVariableNames: ''!

!classDefinition: #TonelImageExporterTest category: #'Tests-Tonel'!
TestCase subclass: #TonelImageExporterTest
	instanceVariableNames: 'exportRoot'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Tonel'!
!classDefinition: 'TonelImageExporterTest class' category: #'Tests-Tonel'!
TonelImageExporterTest class
	instanceVariableNames: ''!


!TonelWriterTest methodsFor: 'assertions' stamp: 'FJG 5/22/2026 00:00:00'!
assert: aString includes: expectedSubstring
	self
		assert: (aString includesSubString: expectedSubstring)
		description: [ 'Expected to find ', expectedSubstring printString, ' in ', aString printString ]! !

!TonelWriterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testClassDefinitionIncludesReconstructionMetadata
	| source |
	source := TonelWriter sourceForClass: TonelWriterTest.
	self assert: source includes: 'Class {'.
	self assert: source includes: '#name : ''TonelWriterTest'''.
	self assert: source includes: '#superclass : ''TestCase'''.
	self assert: source includes: '#category : ''Tests-Tonel'''.
	self assert: source includes: '#package : ''Tests'''.
	self assert: source includes: '#tag : ''Tonel'''! !

!TonelWriterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testClassDefinitionIncludesVariablesWhenPresent
	| source |
	source := TonelWriter sourceForClass: Editor.
	self assert: source includes: '#instVars : ['.
	self assert: source includes: '''morph'''.
	self assert: source includes: '#classInstVars : ['.
	self assert: source includes: '''keyboardActions'''! !

!TonelWriterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testClassMethodUsesClassSideDeclaration
	| source |
	source := TonelWriter sourceForClass: TonelWriterTest.
	self assert: source includes: '{ #category : ''testing'' }'.
	self assert: source includes: 'TonelWriterTest class >> isAbstract ['! !

!TonelWriterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testCommentIsWrittenBeforeClassDefinition
	| source |
	source := TonelWriter sourceForClass: Object.
	self assert: source includes: '"
Object is the root class'.
	self assert: source includes: '"
Class {'! !

!TonelWriterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testInstanceMethodIncludesCategoryDeclarationAndBody
	| source |
	source := TonelWriter sourceForClass: TonelWriterTest.
	self assert: source includes: '{ #category : ''assertions'' }'.
	self assert: source includes: 'TonelWriterTest >> assert: aString includes: expectedSubstring ['.
	self assert: source includes: 'includesSubString: expectedSubstring'! !

!TonelWriterTest class methodsFor: 'testing' stamp: 'FJG 5/22/2026 00:00:00'!
isAbstract
	^ false! !

!TonelReaderTest methodsFor: 'fixtures' stamp: 'FJG 5/22/2026 00:00:00'!
sampleTonelSource
	^ '"
Imported comment.
"
Class {
	#name : ''TonelRoundTripExample'',
	#superclass : ''Object'',
	#instVars : [
		''value''
	],
	#classInstVars : [
		''classValue''
	],
	#category : ''Tests-Tonel-Imported'',
	#package : ''Tests'',
	#tag : ''Tonel-Imported''
}

{ #category : ''accessing'' }
TonelRoundTripExample >> value [
	^ value
]

{ #category : ''accessing'' }
TonelRoundTripExample >> value: anObject [
	value := anObject
]

{ #category : ''instance creation'' }
TonelRoundTripExample class >> withValue: anObject [
	^ self new value: anObject; yourself
]
'! !

!TonelReaderTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testReadsClassMetadata
	| definition |
	definition := TonelReader readStream: self sampleTonelSource readStream.
	self assert: definition className equals: #TonelRoundTripExample.
	self assert: definition superclassName equals: #Object.
	self assert: definition instanceVariableNames equals: #('value').
	self assert: definition classInstanceVariableNames equals: #('classValue').
	self assert: definition category equals: #'Tests-Tonel-Imported'.
	self assert: definition package equals: #Tests.
	self assert: definition tag equals: #'Tonel-Imported'! !

!TonelReaderTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testReadsMethodDefinitions
	| definition selectors |
	definition := TonelReader readStream: self sampleTonelSource readStream.
	selectors := definition methods collect: [ :method | method selector ].
	self assert: selectors asArray equals: #(value value: withValue:).
	self assert: definition methods first protocol equals: #accessing.
	self assert: definition methods first classSide not.
	self assert: definition methods third classSide! !

!TonelReaderTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testReadsWriterOutput
	| definition |
	definition := TonelReader readStream: (TonelWriter sourceForClass: TonelReaderTest) readStream.
	self assert: definition className equals: #TonelReaderTest.
	self assert: (definition methods anySatisfy: [ :method | method selector = #sampleTonelSource ])! !

!TonelImporterTest methodsFor: 'fixtures' stamp: 'FJG 5/22/2026 00:00:00'!
sampleTonelSource
	^ TonelReaderTest new sampleTonelSource! !

!TonelImporterTest methodsFor: 'running' stamp: 'FJG 5/22/2026 00:00:00'!
tearDown
	(Smalltalk classNamed: #TonelRoundTripExample) ifNotNil: [ :class |
		class removeFromSystemUnlogged ].
	super tearDown! !

!TonelImporterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testImportCreatesClassAndCompilesInstanceMethods
	| class instance |
	TonelImporter importStream: self sampleTonelSource readStream.
	class := Smalltalk classNamed: #TonelRoundTripExample.
	self deny: class isNil.
	self assert: class superclass equals: Object.
	self assert: class category equals: #'Tests-Tonel-Imported'.
	instance := class withValue: 7.
	self assert: instance value equals: 7! !

!TonelImporterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testReimportUpdatesExistingMethod
	| class source instance |
	TonelImporter importStream: self sampleTonelSource readStream.
	source := self sampleTonelSource copyReplaceAll: '^ value' with: '^ value + 1'.
	TonelImporter importStream: source readStream.
	class := Smalltalk classNamed: #TonelRoundTripExample.
	instance := class withValue: 7.
	self assert: instance value equals: 8! !

!TonelImageExporterTest methodsFor: 'assertions' stamp: 'FJG 5/22/2026 00:00:00'!
contentsOf: aFileEntry
	aFileEntry readStreamDo: [ :stream | ^ stream upToEnd ]! !

!TonelImageExporterTest methodsFor: 'running' stamp: 'FJG 5/22/2026 00:00:00'!
setUp
	super setUp.
	exportRoot := ('/private/tmp/cuis-tonel-exporter-test' asDirectoryEntry) assureExistence! !

!TonelImageExporterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testExportClassesWritesGroupedTonelFile
	| file source |
	TonelImageExporter exportClasses: { TonelWriterTest } to: exportRoot.
	file := exportRoot / 'src' / 'Tests' // 'TonelWriterTest.class.st'.
	self assert: file exists.
	source := self contentsOf: file.
	self assert: (source includesSubString: 'Class {').
	self assert: (source includesSubString: 'TonelWriterTest >> assert: aString includes: expectedSubstring [')! !

!TonelImageExporterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testExportWritesTonelProperties
	| file |
	TonelImageExporter exportClasses: { TonelWriterTest } to: exportRoot.
	file := exportRoot / 'src' // '.properties'.
	self assert: file exists.
	self assert: ((self contentsOf: file) includesSubString: '#format : #tonel')! !

!TonelImageExporterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testFileNameForClass
	self assert: (TonelImageExporter fileNameForClass: TonelWriterTest) equals: 'TonelWriterTest.class.st'! !

!TonelImageExporterTest methodsFor: 'tests' stamp: 'FJG 5/22/2026 00:00:00'!
testPackageNameForCategory
	self assert: (TonelImageExporter packageNameForCategory: #'Kernel-Objects') equals: 'Kernel'.
	self assert: (TonelImageExporter packageNameForCategory: #'Tests-Tonel') equals: 'Tests'.
	self assert: (TonelImageExporter packageNameForCategory: #Tonel) equals: 'Tonel'! !
