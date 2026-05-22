"
I provide an inert model of a Class, used by FileContentsBrowser to manipulate filedout code. Instead of a method dictionary or selectors onto CompiledMethods, I have a dictionary ("source") of selectors onto ChangeRecords, which were, in the case of FileContentsBrowser, parsed from a source or change set file.
"
Class {
	#name : 'PseudoClass',
	#superclass : 'Object',
	#instVars : [
		'name',
		'definition',
		'organization',
		'source',
		'metaClass'
	],
	#category : 'Tools-Code File Browser',
	#package : 'Tools',
	#tag : 'Code File Browser'
}

{ #category : 'categories' }
PseudoClass class >> removedCategoryName [
	^ `Text string: '*** removed methods ***'  attribute: TextColor red`
]

{ #category : 'methods' }
PseudoClass >> addMethodChange: aChangeRecord [
	| selector |
	selector := self parserClass selectorFrom: aChangeRecord string.
	self organization classify: selector under: aChangeRecord category.
	self sourceCodeAt: selector put: aChangeRecord
]

{ #category : 'accessing' }
PseudoClass >> allCallsOn [
	^ (self realClass ifNil: [ ^#() ]) allCallsOn
]

{ #category : 'accessing' }
PseudoClass >> allInstVarNames [
	^#()
]

{ #category : 'private' }
PseudoClass >> allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level [
	^ self allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: false
]

{ #category : 'private' }
PseudoClass >> allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: aBoolean [
	^ (self realClass ifNil: [ ^self ])
		allSubclassesWithLevelDo: classAndLevelBlock
		startingLevel: level
		sortByCategory: aBoolean
]

{ #category : 'accessing' }
PseudoClass >> allSuperclasses [
	^ (self realClass ifNil: [ ^#() ]) allSuperclasses
]

{ #category : 'testing method dictionary' }
PseudoClass >> bindingOf: varName [
	self exists ifTrue:[
		(self realClass bindingOf: varName) ifNotNil: [ :binding | ^binding].
	].
	^Smalltalk bindingOf: varName asSymbol
]

{ #category : 'compatibility' }
PseudoClass >> browseClassVarRefs [

]

{ #category : 'testing method dictionary' }
PseudoClass >> canUnderstand: selector  [
	"Answer whether the receiver can respond to the message whose selector 
	is the argument. The selector can be in the method dictionary of the 
	receiver's class or any of its superclasses."

	(self includesSelector: selector) ifTrue: [^true].
	self exists ifTrue: [
		^self realClass canUnderstand: selector ].
	^false
]

{ #category : 'compatibility' }
PseudoClass >> category [
	^nil
]

{ #category : 'compatibility' }
PseudoClass >> chooseInstVarThenDo: aBlock [

]

{ #category : 'class' }
PseudoClass >> classComment: aChangeRecord [
	self organization classComment: aChangeRecord
]

{ #category : 'errors' }
PseudoClass >> classNotDefined [
	^self inform: self name,' is not defined in the system.
You have to define this class first.'.
]

{ #category : 'class' }
PseudoClass >> classPool  [
	self exists ifFalse: [^ nil].
	^ self realClass classPool
]

{ #category : 'class' }
PseudoClass >> comment [

	^self smalltalkComment.
]

{ #category : 'class' }
PseudoClass >> comment: aString [
	self classComment: aString asPlainString
]

{ #category : 'methods' }
PseudoClass >> compiledMethodAt: selector ifAbsent: aBlock [
	^aBlock value
]

{ #category : 'accessing' }
PseudoClass >> compilerClass [
	^ (Smalltalk at: name ifAbsent: [^ Compiler]) compilerClass
]

{ #category : 'private' }
PseudoClass >> confirmRemovalOf: aString [
	^self confirm:'Remove ',aString,' ?'
]

{ #category : 'class' }
PseudoClass >> definition [

	^definition ifNil: [
		Text fromString: 'There is no class definition for ', self name, ' in this file'].
]

{ #category : 'class' }
PseudoClass >> definition: aString [
	definition := aString
]

{ #category : 'private' }
PseudoClass >> evaluate: aString [
	^Compiler evaluate: aString for: nil logged: true
]

{ #category : 'testing' }
PseudoClass >> exists [
	^(Smalltalk at: self name asSymbol ifAbsent:[^false]) isKindOf: Behavior
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileIn [
	"FileIn the receiver"
	self hasDefinition ifTrue:[self fileInDefinition].
	self fileInMethods: self selectors.
	metaClass ifNotNil:[metaClass fileIn].
	self needsInitialize ifTrue:[
		self evaluate: self name,' initialize'.
	].
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileInCategory: aCategory [
	^self fileInMethods: (self organization listAtCategoryNamed: aCategory)
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileInDefinition [
	(self makeSureSuperClassExists: self superclassName) ifFalse:[^self].
	self hasDefinition ifTrue:[
		Transcript newLine; show:'Defining ', self name.
		self evaluate: self definition].
	self exists ifFalse:[^self].
	self hasComment ifTrue:[self realClass classComment: self comment].
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileInDefinitionAndMetaclass [
	self fileInDefinition.
	metaClass ifNotNil: [ metaClass fileInDefinition ]
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileInMethod: selector [
	^self fileInMethods: (Array with: selector)
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileInMethods [
	^self fileInMethods: self selectors
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileInMethods: aCollection [
	"FileIn all methods with selectors taken from aCollection"
	| theClass |
	self exists ifFalse:[^self classNotDefined].
	theClass := self realClass.
	aCollection do:[:sel|
		| cat |
		cat := self organization categoryOfElement: sel.
		cat = self class removedCategoryName ifFalse:[
			theClass 
				compile: (self sourceCodeAt: sel) 
				classified: cat
				withStamp: (self stampAt: sel)
				notifying: nil.
		].
	].
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileOut [

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |		
		self fileOutOn: fileStream.
		self needsInitialize ifTrue: [
			fileStream newLine; nextChunkPut: self name,' initialize' ]].
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileOutCategory: categoryName [

	DirectoryEntry fileOutsDirQuery: (self name,'-',categoryName,'.st') writeStreamDo: [ :fileStream |
		self fileOutMethods: (self organization listAtCategoryNamed: categoryName) on: fileStream ].
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileOutDefinitionOn: aStream [
	self hasDefinition ifFalse:[^self].
	aStream nextChunkPut: self definition; newLine.
	self hasComment ifTrue: [
		aStream newLine.
		self organization commentRemoteStr fileOutOn: aStream]
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileOutMethod: selector [

	DirectoryEntry fileOutsDirQuery: (name,'-', selector asFileName, '.st') writeStreamDo: [ :fileStream |
		self fileOutMethods: (Array with: selector) on: fileStream ].
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileOutMethods: aCollection on: aStream [
	"FileOut all methods with selectors taken from aCollection"
	| categories |
	categories := Dictionary new.
	aCollection do:[:sel|
		| cat |
		cat := self organization categoryOfElement: sel.
		cat = self class removedCategoryName ifFalse:[
			(categories includesKey: cat) 
				ifFalse:[ categories at: cat put: Set new ].
			(categories at: cat) add: sel].
	].
	categories associationsDo:[:assoc|
		assoc value do: [ :sel |
			aStream newLine.
			(self sourceCode at: sel) fileOutOn: aStream.
		].
	].
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileOutMethodsOn: aStream [
	^self fileOutMethods: self selectors on: aStream.
]

{ #category : 'fileIn/fileOut' }
PseudoClass >> fileOutOn: aStream [
	"FileOut the receiver"
	self fileOutDefinitionOn: aStream.
	metaClass ifNotNil:[metaClass fileOutDefinitionOn: aStream].
	self fileOutMethods: self selectors on: aStream.
	metaClass ifNotNil:[metaClass fileOutMethods: metaClass selectors on: aStream].
]

{ #category : 'accessing' }
PseudoClass >> fullName [
	^self name
]

{ #category : 'testing' }
PseudoClass >> hasChanges [

	self sourceCode isEmpty ifFalse:[^true].
	self organization hasNoComment ifFalse:[^true].
	definition ifNotNil: [ self realClass ifNotNil: [ :realClass | realClass definition = definition ifFalse: [ ^true]]].
	metaClass ifNotNil: [ ^metaClass hasChanges].
	^false
]

{ #category : 'testing' }
PseudoClass >> hasComment [
	^self organization commentRemoteStr notNil
]

{ #category : 'testing' }
PseudoClass >> hasDefinition [
	^definition notNil
]

{ #category : 'testing' }
PseudoClass >> hasMetaclass [
	^metaClass notNil
]

{ #category : 'testing method dictionary' }
PseudoClass >> includesSelector: aSymbol [
	^ source keys includes: aSymbol.
]

{ #category : 'accessing' }
PseudoClass >> instVarNames [
	^ #()
]

{ #category : 'testing' }
PseudoClass >> is: aSymbol [
	^ aSymbol == #PseudoClass or: [ super is: aSymbol ]
]

{ #category : 'testing' }
PseudoClass >> isMeta [
	^false
]

{ #category : 'testing' }
PseudoClass >> lastUnderscoreMeansSubscript [
	"Redefine this method on the class side of those classes where you want
	a_1 to be shown as 'a subscript 1'"

	^ false
]

{ #category : 'printing' }
PseudoClass >> literalScannedAs: scannedLiteral notifying: requestor  [
	^ scannedLiteral
]

{ #category : 'private' }
PseudoClass >> makeSureClassExists: aString [
	| theClass |
	theClass := Smalltalk at: (aString asSymbol) ifAbsent: nil.
	theClass ifNotNil:[^true].
	^self confirm: aString,' does not exist in the system. Use nil instead?'.
]

{ #category : 'private' }
PseudoClass >> makeSureSuperClassExists: aString [
	| theClass |
	theClass := Smalltalk at: (aString asSymbol) ifAbsent: nil.
	theClass ifNotNil:[^true].
	^self confirm: 'The super class ',aString,' does not exist in the system. Use nil instead?'.
]

{ #category : 'methods' }
PseudoClass >> methodChange: aChangeRecord [
	aChangeRecord isMetaClassChange ifTrue:[
		^self theMetaClass addMethodChange: aChangeRecord.
	] ifFalse:[
		^self addMethodChange: aChangeRecord.
	].

]

{ #category : 'accessing' }
PseudoClass >> name [
	^name
]

{ #category : 'accessing' }
PseudoClass >> name: anObject [
	name := anObject
]

{ #category : 'testing' }
PseudoClass >> nameExists [
	^Smalltalk includesKey: self name asSymbol
]

{ #category : 'testing' }
PseudoClass >> needsInitialize [
	^self hasMetaclass and:[
		self theMetaClass realClass includesSelector: #initialize]
]

{ #category : 'accessing' }
PseudoClass >> organization [
	organization ifNil: [
		organization := PseudoClassOrganizer empty].

	"Making sure that subject is set correctly. It should not be necessary."
	organization setSubject: self.
	^ organization
]

{ #category : 'private' }
PseudoClass >> parserClass [

	^ Compiler parserClass
]

{ #category : 'printing' }
PseudoClass >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut:$(; print: name; nextPut:$)
]

{ #category : 'accessing' }
PseudoClass >> realClass [
	^Smalltalk at: self name asSymbol ifAbsent: nil
]

{ #category : 'removing' }
PseudoClass >> removeAllUnmodified [
	| stClass |
	self exists ifFalse:[^self].
	self removeUnmodifiedMethods: self selectors.
	stClass := self realClass.
	(self hasDefinition and:[stClass definition = self definition]) ifTrue:[definition := nil].
	(self hasComment and:[stClass smalltalkComment asPlainString = self smalltalkComment asPlainString]) ifTrue: [
		self classComment: nil].
	metaClass ifNotNil: [ metaClass removeAllUnmodified]
]

{ #category : 'categories' }
PseudoClass >> removeCategory: selector [
	(self organization listAtCategoryNamed: selector) do:[:sel|
		self organization removeElement: sel.
		self sourceCode removeKey: sel.
	].
	self organization removeCategory: selector.
]

{ #category : 'methods' }
PseudoClass >> removeMethod: selector [
	self organization removeElement: selector.
	self sourceCode removeKey: selector.

]

{ #category : 'methods' }
PseudoClass >> removeSelector: aSelector [
	| catName |
	catName := self class removedCategoryName asPlainString.
	self organization addCategory: catName before:
		(self organization categories ifNotEmpty: [ :coll | coll first ]).
	self organization classify: aSelector under: catName.
	self sourceCodeAt: aSelector put:'methodWasRemoved' asText.
]

{ #category : 'methods' }
PseudoClass >> removeSelectorIfInBaseSystem: selector [
	self sourceCode removeKey: selector ifAbsent: [^ self].
	self organization removeElement: selector
]

{ #category : 'removing' }
PseudoClass >> removeUnmodifiedMethods: aCollection [
	| stClass |
	self exists ifFalse:[^self].
	stClass := self realClass.
	aCollection do:[:sel|
		(self sourceCodeAt: sel) = (stClass sourceCodeAt: sel ifAbsent:['']) asPlainString ifTrue:[
			self removeMethod: sel.
		].
	].
	self organization removeEmptyCategories.
]

{ #category : 'class' }
PseudoClass >> renameTo: aString [

	self hasDefinition ifTrue:[
		self isMeta ifTrue:[
			self definition: (self definition
				copyReplaceAll: name,' class'
				with: aString, ' class').
		] ifFalse:[
			self definition: (self definition 
					copyReplaceAll:'ubclass: #',name
					with:'ubclass: #', aString)]].
	name := aString.
	metaClass ifNotNil:[metaClass renameTo: aString].
]

{ #category : 'methods' }
PseudoClass >> selectors [

	^ self sourceCode keys
]

{ #category : 'class' }
PseudoClass >> sharedPools  [
	self exists ifFalse: [^ nil].
	^ self realClass sharedPools
]

{ #category : 'class' }
PseudoClass >> smalltalkComment [
	| rStr |
	rStr := self organization commentRemoteStr.
	^rStr
		ifNil: [ self name,' has not been commented in this file']
		ifNotNil: [ rStr string]
]

{ #category : 'methods' }
PseudoClass >> sourceCode [
	^source ifNil:[source := Dictionary new]
]

{ #category : 'methods' }
PseudoClass >> sourceCodeAt: sel [
	^(self sourceCode at: sel) string
]

{ #category : 'methods' }
PseudoClass >> sourceCodeAt: sel put: object [
	self sourceCode at: sel put: object
]

{ #category : 'methods' }
PseudoClass >> sourceCodeTemplate [
	^''
]

{ #category : 'methods' }
PseudoClass >> stampAt: selector [
	| record |
	record := (self sourceCode at: selector).
	^(record is: #Text)
		ifTrue: [ '']
		ifFalse: [ record stamp ]
]

{ #category : 'class' }
PseudoClass >> superclass [
	
	"I do not use superclassName because definition can be nil. This can cause a inconsistency becuase the definition
	could be different to what the image has ... 
	On the other hand, allSuperclasses uses realClass so it is inconsistent already - Hernan"
	
	^self realClass ifNotNil: [ :aClass | aClass superclass ]
]

{ #category : 'accessing' }
PseudoClass >> superclassName [
	"Answer the name of the superclass as declared in the file.
	If no definition in the file, then answer the name of the existing superclass in the system.
	If neither, answer nil."

	self hasDefinition ifTrue: [
		^definition copyUpTo: Character space ].
	self exists ifTrue: [
		^self realClass superclass name ].
	^nil
]

{ #category : 'accessing' }
PseudoClass >> theMetaClass [
	^ metaClass ifNil: [ metaClass := PseudoMetaclass new name: self name ].
]

{ #category : 'accessing' }
PseudoClass >> theNonMetaClass [
	"Sent to a class or metaclass, always return the class"

	^self
]

{ #category : 'accessing' }
PseudoClass >> variablesAndOffsetsDo: aBinaryBlock [
	"NOp"
]

{ #category : 'categories' }
PseudoClass >> whichCategoryIncludesSelector: aSelector  [
	"Answer the category of the argument, aSelector, in the organization of 
	the receiver, or answer nil if the receiver does not inlcude this selector."

	^ self organization categoryOfElement: aSelector
]
