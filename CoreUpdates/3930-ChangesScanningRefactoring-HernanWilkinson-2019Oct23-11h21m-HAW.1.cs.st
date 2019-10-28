'From Cuis 5.0 [latest update: #3926] on 26 October 2019 at 2:37:20 pm'!
!classDefinition: #ClassDeletionChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ClassDeletionChangeRecord
	instanceVariableNames: 'className doItOnlyIfInBaseSystem stamp classDefinition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #MethodDeletionChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDeletionChangeRecord
	instanceVariableNames: 'methodReference doItOnlyIfInBaseSystem sourceCode stamp '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #ClassChangeRecord category: #'Tools-Changes'!
Object subclass: #ClassChangeRecord
	instanceVariableNames: 'inForce changeTypes priorDefinition thisName priorName methodChanges stamp '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #MethodChangeRecord category: #'Tools-Changes'!
Object subclass: #MethodChangeRecord
	instanceVariableNames: 'changeType currentMethod infoFromRemoval stamp '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:25:33'!
itemIsRecognized: item

	^ self knownPreambles anySatisfy: [ :preamble | item includesSubString: preamble ] ! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/23/2019 11:25:05'!
knownPreambles

	^ `{
	'commentStamp:'. 
	'methodsFor:'. 
	'classDefinition:'. 
	'classRemoval:'. 
	'methodRemoval:'. 
	'methodMoveToSomePackage:'. 
	'classMoveToSomePackage:'. 
	'provides'. 
	'requires' }`! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:42:13'!
notSeparatorChar

	| prevChar |
	
	[file atEnd not and: [file peek isSeparator]] whileTrue: [prevChar _ file next].
	
	^prevChar! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/23/2019 11:41:41'!
scanAndIgnore: item in: position

	| record |
	
	record _ ChangeRecord new 
		file: file 
		position: position 
		type: #preamble.
		
	self 
		addItem: record 
		text: ('preamble: ' , item contractTo: 160)
! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:30:10'!
scanClassComment: tokens

	| className stamp record |
	
	className _ tokens first.
	stamp _ tokens third.
	record _ ChangeRecord new 
			file: file 
			position: file position 
			type: #classComment 
			class: className 
			category: nil 
			meta: false 
			stamp: stamp.
			
	self 
		addItem: record
		text: 'class comment for ' , className, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
		
	file nextChunk.
! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:32:17'!
scanClassDefinition: tokens

	| classDefinition isMeta itemPosition className record fullClassName |
	
	itemPosition _ file position.
	fullClassName _ tokens second.
	isMeta _ fullClassName includesSubString: ' class'.
	className _ isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	classDefinition _ file nextChunk.
	
	record _ ChangeRecord new 
		file: file 
		position: itemPosition 
		type: #classDefinition 
		class: className asSymbol 
		category: tokens last 
		meta: isMeta 
		stamp: nil.
					
	self addItem: record text: 'classDefinition: ', classDefinition.! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/25/2019 10:54:48'!
scanClassRemoval: tokens

	| doItOnlyIfInBaseSystem removeType classDefinition className description record stamp |
	
	removeType _ tokens first.
	className _ tokens second.
	doItOnlyIfInBaseSystem _ removeType == #classMoveToSomePackage:.
	classDefinition _ file nextChunk.
	stamp _ tokens last.
	
	record _ ClassDeletionChangeRecord 
			className: className
			definition: classDefinition 
			doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
			stamp: stamp.

	description _ doItOnlyIfInBaseSystem 
			ifTrue: ['clase move to some package: '] 
			ifFalse: ['class removal: '].
			
	self 
		addItem: record
		text: description, className, '; ', stamp ! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:48:24'!
scanDoIt
	
	| itemPosition command |

	itemPosition _ file position.
	command _ file nextChunk.
	
	command notEmpty ifTrue: [
		self
			addItem: (ChangeRecord new file: file position: itemPosition type: #doIt)
			text: 'do it: ' , (command contractTo: 160)]! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/23/2019 11:44:09'!
scanFeatureProvision: tokens

	| feature |
	
	feature _ FeatureChangeRecord new
					type: #provides
					feature: (Feature name: tokens second version: tokens third revision: tokens fourth).
					
	self addItem: feature text: feature string! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:39:44'!
scanFeatureRequirement: tokens
	
	| feature requirement |
	
	requirement _ FeatureRequirement 
		name: tokens second 
		minVersion: tokens third 
		minRevision: tokens fourth 
		maxVersion: (tokens size > 4 ifTrue: [tokens fifth]).
		
	feature _ FeatureChangeRecord new
		type: #requires
		feature: requirement.
					
	self addItem: feature text: feature string.! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:52:16'!
scanMethodDefinition: tokens

	| stamp stampIndex className |
	
	className _ tokens first.
	stamp _ ''.
	stampIndex _ tokens indexOf: #stamp: ifAbsent: nil.
	stampIndex ifNotNil: [stamp _ tokens at: (stampIndex + 1)].
	
	tokens second == #methodsFor: ifTrue: [
		^ self scanCategory: tokens third class: className meta: false stamp: stamp ].

	tokens third == #methodsFor: ifTrue: [
		^ self scanCategory: tokens fourth class: className meta: true stamp: stamp ].
	
	self shouldNotHappen 
! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/23/2019 23:35:04'!
scanMethodRemoval: tokens

	| doItOnlyIfInBaseSystem removeType isMeta sourceCode methodReference className description record selector stamp |

	removeType _ tokens first.
	className _ tokens second.
	doItOnlyIfInBaseSystem _ removeType == #methodMoveToSomePackage:.

	sourceCode _ file nextChunk.
	isMeta _ tokens third == #class.	
	selector _ tokens at: tokens size - 2.
	stamp _ tokens last.

	methodReference _ MethodReference new
		setClassSymbol: className
		classIsMeta: isMeta
		methodSymbol: selector
		stringVersion: className, (isMeta ifTrue: [' class '] ifFalse: [' ']), selector,'; ', stamp.
		
	record _ MethodDeletionChangeRecord 
		methodReference: methodReference
		doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
		source: sourceCode 
		stamp: stamp.
	
	description _ doItOnlyIfInBaseSystem 
		ifTrue: ['method move to some package: '] 
		ifFalse: ['method removal: '].
		
	self
		addItem: record
		text: description, methodReference stringVersion! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:45:05'!
scanSpecificChangeRecordTypeIfNotAtEnd: prevChar

	(prevChar notNil and: [ prevChar isLineSeparator ]) ifTrue: [self scanSpecificChangeRecordType]! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:51:26'!
scanUpTo: stopPosition informing: barBlock
	
	[file position < stopPosition] whileTrue: [ | prevChar |
		barBlock value: file position.
		prevChar _ self notSeparatorChar. 
		"A line starting with $!! means a specific ChangeRecord type"
		(file peekFor: $!!)
			ifTrue: [ self scanSpecificChangeRecordTypeIfNotAtEnd: prevChar]
			ifFalse: [ self scanDoIt ]]! !


!Class methodsFor: 'fileIn/Out' stamp: 'HAW 10/24/2019 09:41:50'!
definitionReplacingCategoryWith: aNewCategory

	| definition categoryDefinitionIndex currentCategoryDefinition definitionWithNewCategory |
	
	definition := self definition.
	"category can be nil, that is why I sent asString to it - Hernan"
	currentCategoryDefinition := 'category: ''', self category asString, ''''.
	categoryDefinitionIndex := definition 
		indexOfSubCollection: currentCategoryDefinition 
		startingAt: 1
		ifAbsent: [ self error: 'Definition of category not found!!' ].
	
	definitionWithNewCategory := definition first: categoryDefinitionIndex - 1.
	definitionWithNewCategory := definitionWithNewCategory, 'category: ''', aNewCategory, ''''.
	
	^definitionWithNewCategory		! !


!MethodReference methodsFor: 'queries' stamp: 'HAW 10/23/2019 20:56:40'!
actualClassIfAbsent: anAbsentBlock 

	^Smalltalk 
		at: classSymbol 
		ifPresent: [ :actualClass |
			classIsMeta
				ifTrue: [ actualClass class ]
				ifFalse: [ actualClass ] ]
		ifAbsent: anAbsentBlock 
! !

!MethodReference methodsFor: 'queries' stamp: 'HAW 10/23/2019 21:40:47'!
sourceCodeIfAbsent: aBlock

	| actualClass |

	actualClass := self actualClassIfAbsent: [ ^aBlock value ].
	^actualClass sourceCodeAt: methodSymbol ifAbsent: aBlock! !


!MethodReference class methodsFor: 'instance creation' stamp: 'HAW 10/23/2019 21:05:22'!
classSymbol: aClassName classIsMeta: isMeta methodSymbol: aSelector 
	
	^self new setClassSymbol: aClassName classIsMeta: isMeta methodSymbol: aSelector stringVersion: ''.! !

!MethodReference class methodsFor: 'error description' stamp: 'HAW 10/23/2019 21:38:36'!
classDoesNotExistErrorMessage

	^'Class does not exist'! !


!ChangeListElement methodsFor: 'perform' stamp: 'HAW 10/25/2019 10:35:21'!
performOn: aCodeFile

	 ^aCodeFile perform: (self changeType copyWith: $:) asSymbol with: self ! !


!ChangeRecord methodsFor: 'printing' stamp: 'HAW 10/26/2019 11:57:37'!
printOn: aStream

	super printOn: aStream.
	aStream 
		nextPutAll: ' - type: ';
		nextPutAll: type ! !


!ClassDeletionChangeRecord methodsFor: 'accessing' stamp: 'HAW 10/24/2019 08:57:18'!
className: aSymbol

	className _ aSymbol! !

!ClassDeletionChangeRecord methodsFor: 'initialization' stamp: 'HAW 10/24/2019 08:55:20'!
initializeClassName: aClassName definition: aClassDefinition doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem stamp: aStamp 
	
	className := aClassName.
	classDefinition := aClassDefinition.
	doItOnlyIfInBaseSystem := aDoItOnlyIfInBaseSystem.
	stamp := aStamp.! !

!ClassDeletionChangeRecord methodsFor: 'services' stamp: 'HAW 10/26/2019 10:38:56'!
command

	^doItOnlyIfInBaseSystem
		ifTrue: [ 'Smalltalk removeClassNamedIfInBaseSystem: #', className ]
		ifFalse: [ 'Smalltalk removeClassNamed: #', className ].
! !


!ClassDeletionChangeRecord class methodsFor: 'instance creation' stamp: 'HAW 10/24/2019 08:55:05'!
className: aClassName definition: aClassDefinition doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem stamp: aStamp 

	^self new 
		initializeClassName: aClassName 
		definition: aClassDefinition 
		doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem 
		stamp: aStamp ! !


!MethodDeletionChangeRecord methodsFor: 'initialization' stamp: 'HAW 10/23/2019 23:35:56'!
initializeMethodReference: aMethodReference doItOnlyIfInBaseSystem: aDoit source: aSourceCode stamp: aStamp  
	
	methodReference := aMethodReference.
	doItOnlyIfInBaseSystem := aDoit.
	sourceCode := aSourceCode.
	stamp := aStamp ! !

!MethodDeletionChangeRecord methodsFor: 'perform' stamp: 'HAW 10/25/2019 10:37:08'!
performOn: aCodeFile

	^aCodeFile removedMethod: self command with: self ! !


!MethodDeletionChangeRecord class methodsFor: 'instance creation' stamp: 'HAW 10/23/2019 23:36:19'!
methodReference: aMethodReference doItOnlyIfInBaseSystem: aDoit source: aSourceCode stamp: aStamp 

	^self new 
		initializeMethodReference: aMethodReference 
		doItOnlyIfInBaseSystem: aDoit 
		source: aSourceCode 
		stamp: aStamp ! !


!ChangeSet methodsFor: 'accessing' stamp: 'HAW 10/26/2019 12:27:22'!
movedClassRecords

	^ changeRecords values select: [ :aChangeRecord | aChangeRecord isClassMoveToOtherPackage ]! !

!ChangeSet methodsFor: 'accessing' stamp: 'HAW 10/25/2019 10:02:15'!
removedClassRecords

	^ changeRecords values select: [ :aChangeRecord | aChangeRecord isClassRemoval]! !

!ChangeSet methodsFor: 'accessing' stamp: 'HAW 10/26/2019 12:27:57'!
sortedClassRecords: classRecords

	^ classRecords sort: [:left :rigth | left thisName < rigth thisName ]! !

!ChangeSet methodsFor: 'accessing' stamp: 'HAW 10/26/2019 12:27:57'!
sortedMovedClassesToOtherPackage

	^ self sortedClassRecords: self movedClassRecords! !

!ChangeSet methodsFor: 'accessing' stamp: 'HAW 10/26/2019 12:27:57'!
sortedRemovedClassRecords

	^ self sortedClassRecords: self removedClassRecords! !

!ChangeSet methodsFor: 'class changes' stamp: 'HAW 10/25/2019 09:51:55'!
noteRemovalOf: class fromCategory: aCategoryName 
	"The class is about to be removed from the system.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class) noteRemoved: class fromCategory: aCategoryName.
	changeRecords removeKey: class class name ifAbsent: nil.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:41:49'!
fileOutClassDefinitionsOf: classList on: stream

	classList do: [ :aClass | self fileOutClassDefinition: aClass on: stream ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:35:49'!
fileOutClassDeletionFrom: aClassChangeRecord doItOnlyIfInBaseSystem: aDoIt on: stream

	| record |
	
	record := ClassDeletionChangeRecord 
		className: aClassChangeRecord thisName 
		definition: aClassChangeRecord priorDefinition 
		doItOnlyIfInBaseSystem: aDoIt 
		stamp: aClassChangeRecord stamp.
		
	record fileOutOn: stream
	
! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:44:32'!
fileOutClassInitializationsOf: classList on: stream

	^ classList do: [ :aClass |
		self fileOutPSFor: aClass on: stream ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/23/2019 23:37:18'!
fileOutMethodRemovalOf: selector for: class movedToOtherPackage: moved on: stream stamp: stamp 

	| methodReference changeRecord |
	
	methodReference := MethodReference class: class selector: selector.
	
	changeRecord := MethodDeletionChangeRecord 
		methodReference: methodReference
		doItOnlyIfInBaseSystem: moved
		source: (methodReference sourceCodeIfAbsent: [ 'Missing' ]) 
		stamp: stamp.
		
	changeRecord fileOutOn: stream ! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/24/2019 08:40:10'!
fileOutMethodRemovalsOf: changeTypes movedToOtherPackage: moved for: class on: stream 
	"Write out removals and initialization for this class."

	| classRecord methodChanges changeType |
	
	classRecord _ changeRecords at: class name ifAbsent: [^ self].
	methodChanges _ classRecord methodChanges.
	
	methodChanges keysAndValuesDo: [:selector :aMethodChange |
		changeType _ aMethodChange changeType.
		(changeTypes includes: changeType)
			ifTrue: [ self 
				fileOutMethodRemovalOf: selector 
				for: class 
				movedToOtherPackage: moved 
				on: stream 
				stamp: aMethodChange stamp ]].		 
! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:41:55'!
fileOutMethodsAdditionsOf: classList on: stream

	classList do: [ :aClass | self fileOutMethodAdditionsFor: aClass on: stream ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:42:26'!
fileOutMethodsChangesOf: stream on: classList

	^ classList do: [ :aClass | self fileOutMethodChangesFor: aClass on: stream ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:36:11'!
fileOutMovedClassRecord: aRemovedClassRecord on: stream

	self fileOutClassDeletionFrom: aRemovedClassRecord doItOnlyIfInBaseSystem: true on: stream
! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:45:35'!
fileOutMovedClassesOn: stream

	^ self sortedMovedClassesToOtherPackage do: [ :aMovedClassRecord |
		self fileOutMovedClassRecord: aMovedClassRecord on: stream ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:43:33'!
fileOutRemovedAndMovedMethodsOf: classList on: stream

	^ classList reverseDo: [ :aClass |
		self fileOutMethodRemovalsFor: aClass on: stream.
		self fileOutMethodMovedToOtherPackagesFor: aClass on: stream ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:35:04'!
fileOutRemovedClassRecord: aRemovedClassRecord on: stream

	self fileOutClassDeletionFrom: aRemovedClassRecord doItOnlyIfInBaseSystem: false on: stream
! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:45:13'!
fileOutRemovedClassesOn: stream

	^ self sortedRemovedClassRecords do: [ :aRemovedClassRecord | 
		self fileOutRemovedClassRecord: aRemovedClassRecord on: stream ]! !


!ClassChangeRecord methodsFor: 'all changes' stamp: 'HAW 10/25/2019 10:10:10'!
noteRemoved: class fromCategory: aCategoryName

	priorDefinition := class definitionReplacingCategoryWith: aCategoryName.
	self noteChangeType: #remove fromClass: class
! !

!ClassChangeRecord methodsFor: 'stamp' stamp: 'HAW 10/25/2019 10:19:05'!
stamp
	
	^stamp! !


!MethodChangeRecord methodsFor: 'stamp' stamp: 'HAW 10/23/2019 23:41:48'!
stamp

	^stamp ! !

!MethodChangeRecord methodsFor: 'perform' stamp: 'HAW 10/25/2019 10:36:24'!
performOn: aCodeFile

	 ^aCodeFile perform: (self changeType copyWith: $:) asSymbol with: self ! !


!CodeFile methodsFor: 'reading' stamp: 'HAW 10/26/2019 12:51:11'!
buildFrom: changes informingTo: barBlock 

	changes withIndexDo: [ :changeRecord :anIndex | 
		barBlock value: anIndex.
		changeRecord performOn: self.
	].
! !


!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:39:00'!
scanCategory: category class: class meta: meta stamp: stamp

	| itemPosition method |

	[
		itemPosition _ file position.
		method _ file nextChunk.
		method notEmpty ] whileTrue: [ "done when double terminators"
			self
				addItem: (ChangeRecord new 
					file: file 
					position: itemPosition 
					type: #method
					class: class 
					category: category 
					meta: meta 
					stamp: stamp)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
					, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 11:50:35'!
scanFile: aFile from: startPosition to: stopPosition

	file _ aFile.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	file position: startPosition.
	
	'Scanning ', aFile localName, '...'
		displayProgressAt: Sensor mousePoint
		from: startPosition to: stopPosition
		during: [ :barBlock | self scanUpTo: stopPosition informing: barBlock ].
		
	self clearSelections! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/23/2019 22:47:43'!
scanSpecificChangeRecordType
	"Scan anything that involves more than one chunk"

	| itemPosition item tokens firstToken secondToken |

	itemPosition _ file position.
	item _ file nextChunk.	
	
	(self itemIsRecognized: item) ifFalse: [
		"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
		^ self scanAndIgnore: item in: itemPosition ].

	tokens _ Scanner new scanTokens: item.
	tokens size >= 2 ifTrue: [
		firstToken _ tokens first.
		secondToken _ tokens second.
		
		firstToken == #classDefinition: 
			ifTrue: [ ^ self scanClassDefinition: tokens ].
		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ]) 
			ifTrue: [ ^ self scanClassRemoval: tokens ].
		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ]) 
			ifTrue: [ ^ self scanMethodRemoval: tokens ].
		(secondToken == #methodsFor: or: [ tokens third == #methodsFor: ]) 
			ifTrue: [ ^ self scanMethodDefinition: tokens ].
		secondToken == #commentStamp: 
			ifTrue: [ ^ self scanClassComment: tokens ].
		firstToken == #provides: 
			ifTrue: [ ^ self scanFeatureProvision: tokens ].
		firstToken == #requires: 
			ifTrue: [ ^ self scanFeatureRequirement: tokens ].
	]! !


!ChangeList class methodsFor: 'public access' stamp: 'HAW 10/24/2019 08:57:14'!
browsePackageContents: aFileEntry
	"Opens a changeList on a fileStream"
	| changeList packageFile |
	aFileEntry readStreamDo: [ :stream |
		changeList _ self new scanFile: stream from: 0 to: stream size.
		stream reset.
		packageFile _ CodePackageFile buildFileStream: stream.
	].
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference stringVersion ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new className: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aFileEntry name! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 10/24/2019 09:45:22'!
classRemoved: aClass fromCategory: aCategoryName

	| classDefinition |
	
	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			"I have to recreate the category because the classs has already been removed form the
			SystemOrganizer - Hernan"
			classDefinition := aClass definitionReplacingCategoryWith: aCategoryName. 
			
			self 
				logChange: classDefinition 
				preamble: 'classRemoval: ', aClass name printString, ' stamp: ', Utilities changeStamp printString ].
			
			! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 10/23/2019 19:09:00'!
methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	aClass acceptsLoggingOfCompilation 
		ifTrue: [ self 
			logChange: aMethod sourceCode 
			preamble: 'methodRemoval: ', aClass name, ' ', aSymbol storeString, ' stamp: ', Utilities changeStamp printString ]. 
! !


!MethodReference methodsFor: 'queries' stamp: 'HAW 10/23/2019 20:58:03'!
actualClass 

	^self actualClassIfAbsent: [ nil ]! !

!MethodReference methodsFor: 'queries' stamp: 'HAW 10/23/2019 21:38:10'!
sourceCode

	^ (self actualClassIfAbsent: [ self error: self class classDoesNotExistErrorMessage ]) 
		sourceCodeAt: methodSymbol! !


!ClassDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'HAW 10/25/2019 10:41:36'!
fileIn

	doItOnlyIfInBaseSystem
		ifTrue: [ Smalltalk removeClassNamedIfInBaseSystem: className]
		ifFalse: [ self changeClass ifNotNil: [ :aClass | aClass removeFromSystem ] ]! !

!ClassDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'HAW 10/26/2019 10:50:42'!
fileOutOn: stream
	"File the receiver out on the given file stream"

	| record |
		
	record := String streamContents: [ :recordStream |
		recordStream
		 	nextPutAll: (doItOnlyIfInBaseSystem ifTrue: [ 'classMoveToSomePackage: #' ] ifFalse: ['classRemoval: #']);
			nextPutAll: className;
			nextPutAll: ' stamp: ';
			print: stamp ].
		
	stream 
		nextPut: $!!;
		nextChunkPut: record;
		newLine;
		nextChunkPut: self command;
		newLine; newLine.
	! !

!ClassDeletionChangeRecord methodsFor: 'accessing' stamp: 'HAW 10/24/2019 08:54:41'!
changeClass
	^Smalltalk at: className ifAbsent: nil! !

!ClassDeletionChangeRecord methodsFor: 'accessing' stamp: 'HAW 10/24/2019 08:54:42'!
changeClassName
	^className! !

!ClassDeletionChangeRecord methodsFor: 'accessing' stamp: 'HAW 10/24/2019 08:56:50'!
string
	
	^classDefinition ifNil: [ '' ]! !


!MethodDeletionChangeRecord methodsFor: 'accessing' stamp: 'HAW 10/23/2019 17:22:05'!
string

	^sourceCode ifNil: [ '' ]! !

!MethodDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'HAW 10/26/2019 10:48:23'!
fileOutOn: stream
	"File the receiver out on the given file stream"

	| record |

	record := String streamContents: [ :recordStream |
		recordStream
		 	nextPutAll: (doItOnlyIfInBaseSystem ifTrue: [ 'methodMoveToSomePackage: ' ] ifFalse: ['methodRemoval: ']);
			nextPutAll: self changeClassName;
			space;
			nextPutAll: self methodSelector storeString;
			nextPutAll: ' stamp: ';
			print: stamp ].
		
	stream 
		nextPut: $!!;
		nextChunkPut: record;
		newLine;
		nextChunkPut: self command;
		newLine
	
! !

!MethodDeletionChangeRecord methodsFor: 'services' stamp: 'HAW 10/26/2019 10:48:08'!
command

	^String streamContents: [ :stream |
		stream
			nextPutAll: self changeClassName;
			space;
			nextPutAll: (doItOnlyIfInBaseSystem ifTrue: ['removeSelectorIfInBaseSystem:'] ifFalse: ['removeSelector:']);
			space;
			nextPutAll: self methodSelector storeString ]! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/23/2019 22:26:35'!
fileOutMethodMovedToOtherPackagesFor: class on: stream 
	"Write out removals and initialization for this class."

	self 
		fileOutMethodRemovalsOf: #(movedToOtherPackage) 
		movedToOtherPackage: true
		for: class 
		on: stream
! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/23/2019 22:26:05'!
fileOutMethodRemovalsFor: class on: stream 
	"Write out removals and initialization for this class."

	self 
		fileOutMethodRemovalsOf: #(remove addedThenRemoved) 
		movedToOtherPackage: false
		for: class 
		on: stream
		! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:46:36'!
fileOutOn: stream 
	"Write out all the changes the receiver knows about"

	| classList |
	
	self isEmpty ifTrue: [ self inform: 'Warning: no changes to file out' ].
	
	classList _ Array streamContents: [ :strm |
		Smalltalk hierarchySorted: self changedClasses do: [ :cls | strm nextPut: cls ]].

	self fileOutClassDefinitionsOf: classList on: stream.
	self fileOutMethodsAdditionsOf: classList on: stream.
	self fileOutMethodsChangesOf: stream on: classList.
	self fileOutRemovedAndMovedMethodsOf: classList on: stream.
	self fileOutClassInitializationsOf: classList on: stream.
	self fileOutRemovedClassesOn: stream.
	self fileOutMovedClassesOn: stream.
! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'HAW 10/24/2019 15:00:59'!
classRemoved: aClass fromCategory: aCategoryName

	self noteRemovalOf: aClass fromCategory: aCategoryName! !


!ClassChangeRecord methodsFor: 'all changes' stamp: 'HAW 10/25/2019 09:53:27'!
noteChangeType: changeSymbol fromClass: class

	stamp _ Utilities changeStamp.
	
	changeSymbol == #movedToOtherPackage ifTrue: [
		^ changeTypes add: changeSymbol].
	"Any other change type meanse we're still here!!"
	changeTypes remove: #movedToOtherPackage ifAbsent: nil.

	(changeSymbol == #new or: [changeSymbol == #add]) ifTrue: [
		changeTypes add: #add.
		changeTypes remove: #change ifAbsent: nil.
		^ self].
	changeSymbol == #change ifTrue: [
		(changeTypes includes: #add) ifTrue: [^ self].
		^ changeTypes add: changeSymbol].
	changeSymbol == #addedThenRemoved ifTrue: [
		^ self].  "An entire class was added but then removed"
	changeSymbol == #comment ifTrue: [
		^ changeTypes add: changeSymbol].
	changeSymbol == #reorganize ifTrue: [
		^ changeTypes add: changeSymbol].
	changeSymbol == #rename ifTrue: [
		^ changeTypes add: changeSymbol].
	(changeSymbol beginsWith: 'oldName: ') ifTrue: [
		"Must only be used when assimilating other changeSets"
		(changeTypes includes: #add) ifTrue: [^ self].
		priorName _ changeSymbol copyFrom: 'oldName: ' size + 1 to: changeSymbol size.
		^ changeTypes add: #rename].
	changeSymbol == #remove ifTrue: [
		(changeTypes includes: #add)
			ifTrue: [changeTypes add: #addedThenRemoved]
			ifFalse: [changeTypes add: #remove].
		^ changeTypes removeAllFoundIn: #(add change comment reorganize)].

	self error: 'Unrecognized changeType'! !

!ClassChangeRecord methodsFor: 'method changes' stamp: 'HAW 10/24/2019 08:49:09'!
findOrMakeMethodChangeAt: selector priorMethod: priorMethod

	^ methodChanges 
		at: selector
		ifAbsentPut: [MethodChangeRecord new priorMethod: priorMethod]! !


!MethodChangeRecord methodsFor: 'change type' stamp: 'HAW 10/25/2019 17:21:14'!
noteChangeType: newChangeType

	stamp _ Utilities changeStamp.
	
	"Change of an added method, is still an add"
	(changeType == #add and: [ newChangeType == #change ])
		ifTrue: [ ^self ].

	"Change of an added method, is still an add"
	(changeType == #addedThenRemoved and: [ newChangeType == #change ])
		ifTrue: [ 
			changeType _ #add.
			^self ].

	changeType _ newChangeType.! !


!CodeFile methodsFor: 'reading' stamp: 'HAW 10/26/2019 12:50:49'!
buildFrom: aStream

	| changes |
	
	changes _ (ChangeList new scanFile: aStream from: 0 to: aStream size) changeList.
	
	('Processing ', self name) 
		displayProgressAt: Sensor mousePoint
		from: 1
		to: changes size
		during: [ :barBlock | self buildFrom: changes informingTo: barBlock ].
! !

!methodRemoval: ChangeSet #classMovesToOtherPackage stamp: 'HAW 10/26/2019 12:27:32'!
ChangeSet removeSelector: #classMovesToOtherPackage!
!methodRemoval: ChangeSet #fileOutMethodRemovalOf:for:movedToOtherPackage:on: stamp: nil!
ChangeSet removeSelector: #fileOutMethodRemovalOf:for:movedToOtherPackage:on:!
!methodRemoval: ChangeSet #classRemoves stamp: 'HAW 10/25/2019 09:59:24'!
ChangeSet removeSelector: #classRemoves!
!methodRemoval: ChangeSet #fileOutRemovedClassRecord: stamp: 'HAW 10/25/2019 10:06:29'!
ChangeSet removeSelector: #fileOutRemovedClassRecord:!
!methodRemoval: ChangeSet #noteRemovalOf: stamp: 'HAW 10/24/2019 15:00:59'!
ChangeSet removeSelector: #noteRemovalOf:!
!methodRemoval: ChangeSet #fileOutInitializationOf:on: stamp: 'HAW 10/26/2019 12:44:32'!
ChangeSet removeSelector: #fileOutInitializationOf:on:!
!methodRemoval: ChangeSet #sorteClassRecords: stamp: 'HAW 10/26/2019 12:27:57'!
ChangeSet removeSelector: #sorteClassRecords:!
!methodRemoval: MethodDeletionChangeRecord class #methodReference:doItOnlyIfInBaseSystem:source: stamp: nil!
MethodDeletionChangeRecord class removeSelector: #methodReference:doItOnlyIfInBaseSystem:source:!
!methodRemoval: MethodDeletionChangeRecord #initializeMethodReference:doItOnlyIfInBaseSystem:source: stamp: nil!
MethodDeletionChangeRecord removeSelector: #initializeMethodReference:doItOnlyIfInBaseSystem:source:!
!methodRemoval: ClassDeletionChangeRecord #clsName: stamp: 'HAW 10/24/2019 08:57:15'!
ClassDeletionChangeRecord removeSelector: #clsName:!
!methodRemoval: MethodReference class #classSymbol:isMeta:selector: stamp: nil!
MethodReference class removeSelector: #classSymbol:isMeta:selector:!
!methodRemoval: MethodReference #sourceCodeIfMissing: stamp: nil!
MethodReference removeSelector: #sourceCodeIfMissing:!

!ChangeList reorganize!
('initialization-release' addItem:text: initialize wantsPrettyDiffOption)
('scanning' itemIsRecognized: knownPreambles notSeparatorChar scanAndIgnore:in: scanCategory:class:meta:stamp: scanClassComment: scanClassDefinition: scanClassRemoval: scanDoIt scanFeatureProvision: scanFeatureRequirement: scanFile:from:to: scanMethodDefinition: scanMethodRemoval: scanSpecificChangeRecordType scanSpecificChangeRecordTypeIfNotAtEnd: scanUpTo:informing:)
('menu actions' compareToCurrentVersion currentVersionsOfSelections deselectAll destroyCurrentCodeOfSelections fileInSelections fileOutCurrentVersionsOfSelections fileOutSelections invertSelections removeDoIts removeEmptyClassComments removeNonSelections removeOlderMethodVersions removeSelections removeUpToDate selectAll selectAllForAbsentClasses selectAllForThisClass selectEquivalentMethods selectMethodsOlderThanCurrent selectNewMethods selectRemovalsOfSent selectSuchThat: selectUnchangedMethods xtraVersion)
('viewing access' acceptedStringOrText annotation contentsDiffedFromCurrent contentsSymbolQuints diffedVersionContents list listIndex listSelectionAt: listSelectionAt:put: selectedClass selectedClassOrMetaClass selectedMessageName toggleListIndex: undiffedContents)
('accessing' changeList contentsSymbol currentChange file showsVersions)
('shout styling' shouldStyle:with:)
('private' clearSelections)
('testing' isNewMethod:)
!


!ChangeList class reorganize!
('public access' browseContents: browseFrom:on:labeled: browsePackageContents: browseRecent:on: browseRecentLog browseRecentLogOn: browseRecentLogOn:startingFrom:)
('fileIn/Out' fileReaderServicesForFile:suffix: serviceContents servicePackageContents serviceRecentChanges)
!


!Class reorganize!
('accessing' classPool classPool: classPoolFrom: name)
('testing' hasMethods isObsolete)
('copying' copy)
('class name' rename: safeRenameTo:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classVarNames ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools removeSharedPool: sharedPools)
('compiling' binding bindingOf: compileAllFrom: localBindingOf: possibleVariablesFor:continuedFrom:)
('subclass creation' immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableDoubleWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('fileIn/Out' definitionReplacingCategoryWith: fileOut fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' addSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('private' setName: spaceUsed)
('initialization' declare: sharing: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('tdd' testCaseClass)
('*LiveTyping' isSubclassesEmpty unloadFromAllRawTypes unloadFromAllRawTypesOf:)
!


!MethodReference class reorganize!
('instance creation' class:selector: classSymbol:classIsMeta:methodSymbol: method:)
('error description' classDoesNotExistErrorMessage)
!


!ChangeListElement reorganize!
('testing' isClassDeletion isDoIt isMethodDeletion)
('accessing' text)
('perform' performOn:)
!


!ChangeRecord reorganize!
('access' category changeClass changeClassName changeType compilerClass dateAndTime fileName fileOutOn: isMetaClassChange methodSelector stamp stamp: string)
('initialization' file:position:type: file:position:type:class:category:meta:stamp: fileIn)
('testing' isDoIt)
('printing' printOn:)
!

!classDefinition: #ClassDeletionChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ClassDeletionChangeRecord
	instanceVariableNames: 'className doItOnlyIfInBaseSystem classDefinition stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ClassDeletionChangeRecord reorganize!
('fileIn/fileOut' fileIn fileOutOn:)
('accessing' changeClass changeClassName changeType className: doItOnlyIfInBaseSystem: isMetaClassChange methodSelector string)
('testing' isClassDeletion)
('initialization' initializeClassName:definition:doItOnlyIfInBaseSystem:stamp:)
('services' command)
!

!classDefinition: #MethodDeletionChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDeletionChangeRecord
	instanceVariableNames: 'methodReference doItOnlyIfInBaseSystem sourceCode stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!MethodDeletionChangeRecord reorganize!
('accessing' changeClass changeClassName changeType doItOnlyIfInBaseSystem: isMetaClassChange methodReference: methodSelector string)
('fileIn/fileOut' fileIn fileOutOn:)
('services' command)
('testing' isMethodDeletion)
('initialization' initializeMethodReference:doItOnlyIfInBaseSystem:source:stamp:)
('perform' performOn:)
!


!MethodDeletionChangeRecord class reorganize!
('instance creation' methodReference:doItOnlyIfInBaseSystem:source:stamp:)
!


!ChangeSet reorganize!
('change logging' noteAddClass: noteChangeClass:from: noteClassMoveToOtherPackage: noteMethodMoveToOtherPackage:forClass: noteNewMethod:forClass:selector:priorMethod: noteRecategorizationOfClass: noteRemoveSelector:class:priorMethod:lastMethodInfo: noteRenameClass:as:)
('accessing' author codePackage codePackage: editPostscript hasPostscript hasUnsavedChanges hasUnsavedChanges: isForBaseSystem isForBaseSystem: methodChanges methodInfoFromRemoval: movedClassRecords name name: printOn: removePostscript removedClassRecords sortedClassRecords: sortedMovedClassesToOtherPackage sortedRemovedClassRecords)
('testing' canHavePreambleAndPostscript isEmpty isMoribund methodsWithoutClassifications okayToRemove)
('method changes' changedMessageList changedMessageListAugmented messageListForChangesWhich:ifNone: methodChangesAtClass: removeSelectorChanges:class: selectorsInClass:)
('class changes' changedClassCategories changedClassNames changedClasses classChangeAt: fatDefForClass: noteClassForgotten: noteClassStructure: noteCommentClass: noteRemovalOf:fromCategory: noteReorganizeClass: trimHistory)
('moving changes' hasPreamble methodsWithAnyInitialsOtherThan: methodsWithInitialsOtherThan: methodsWithoutComments removeClassChanges: removePreamble)
('fileIn/Out' askAddedInstVars: askRemovedInstVars: askRenames:addTo:using: assurePostscriptExists assurePreambleExists checkForSlips fileOut fileOutClassDefinitionsOf:on: fileOutClassDeletionFrom:doItOnlyIfInBaseSystem:on: fileOutClassInitializationsOf:on: fileOutMethodAdditionsFor:on: fileOutMethodChangesFor:on: fileOutMethodMovedToOtherPackagesFor:on: fileOutMethodRemovalOf:for:movedToOtherPackage:on:stamp: fileOutMethodRemovalsFor:on: fileOutMethodRemovalsOf:movedToOtherPackage:for:on: fileOutMethodsAdditionsOf:on: fileOutMethodsChangesOf:on: fileOutMovedClassRecord:on: fileOutMovedClassesOn: fileOutOn: fileOutPSFor:on: fileOutPostscriptOn: fileOutPreambleOn: fileOutRemovedAndMovedMethodsOf:on: fileOutRemovedClassRecord:on: fileOutRemovedClassesOn: objectForDataStream: postscriptString postscriptString: preambleString preambleString: preambleTemplate)
('private' addCoherency: atClass:add: atClass:includes: atSelector:class: changeRecorderFor: fileOutClassDefinition:on: oldNameFor:)
('system change notifications' classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classRenamed:from:to:inCategory: classReorganized: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('initialization' clear initialize wither)
!

!classDefinition: #ClassChangeRecord category: #'Tools-Changes'!
Object subclass: #ClassChangeRecord
	instanceVariableNames: 'inForce changeTypes priorDefinition thisName priorName methodChanges stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #MethodChangeRecord category: #'Tools-Changes'!
Object subclass: #MethodChangeRecord
	instanceVariableNames: 'changeType currentMethod infoFromRemoval stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!MethodChangeRecord reorganize!
('method' currentMethod methodInfoFromRemoval noteMethodInfoFromRemoval: noteNewMethod:)
('printing' printOn:)
('object serialization' storeDataOn:)
('initialization' priorMethod:)
('change type' changeType noteChangeType:)
('stamp' stamp)
('perform' performOn:)
!

