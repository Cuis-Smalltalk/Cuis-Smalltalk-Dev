"
ChangeSets keep track of the changes made to a system, so they can be written on a file as source code (a "fileOut").
There are separate ChangeSets for caturing changes done to the Cuis base system and changes done to Packages. Usually the ChangeSets for Packages are not important, and ignored, because Packages are saved on pck.st files. Change sets for the Cuis base system are the standard way of capturing changes to Cuis, and are used to evolve Cuis itself.

name - a String used to name the changeSet, and thus any associated project or fileOut.

preamble and postscript:  two strings that serve as prefix (useful for documentation) and suffix (useful for doits) to the fileout of the changeSet.

changeRecords -  Dictionary {class name -> a ClassChangeRecord}.
These classChangeRecords (qv) remember all of the system changes.

structures -    Dictionary {#Rectangle -> #(<classVersionInteger> 'origin' 'corner')}.
Of  the names of the instances variables before any changes for all classes in classChanges, and all of their superclasses.  In the same format used in SmartRefStream.  Inst var names are strings.  

superclasses -    Dictionary {#Rectangle -> #Object}.
Of all classes in classChanges, and all of their superclasses.

Structures and superclasses save the instance variable names of this class and all of its superclasses.  Later we can tell how it changed and write a conversion method.  The conversion method is used when old format objects are brought in from the disk from ImageSegment files (.extSeg) or SmartRefStream files (.obj .morph .bo .sp).

NOTE:  It should be fairly simple, by adding a bit more information to the classChangeRecords, to reconstruct the information now stored in 'structures' and 'superclasses'.  This would be a welcome simplification.

"
Class {
	#name : 'ChangeSet',
	#superclass : 'Object',
	#instVars : [
		'name',
		'preamble',
		'postscript',
		'changeRecords',
		'structures',
		'superclasses',
		'hasUnsavedChanges',
		'isForBaseSystem',
		'codePackage'
	],
	#classVars : [
		'AllChangeSets',
		'CurrentBaseCSNumber',
		'Installing'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'system change notifications' }
ChangeSet class >> aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName [

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			aboutToRenameClass: aClass
			from: oldClassName
			to: newClassName
			inCategory: aCategoryName ].
	packageOrNil ifNotNil: [ 
		packageOrNil hasUnsavedChanges: true ]
]

{ #category : 'enumerating' }
ChangeSet class >> allChangeSets [
	"Return the list of all current ChangeSets"

	^ AllChangeSets
]

{ #category : 'enumerating' }
ChangeSet class >> allChangeSetsWithClass: aClass  [
	
	^ AllChangeSets select: [ :aChangeSet | aChangeSet isWithClass: aClass ]
]

{ #category : 'enumerating' }
ChangeSet class >> allChangeSetsWithClass: class selector: selector [

	class ifNil: [^ #()].
	^ AllChangeSets select: [ :cs |
		(cs atSelector: selector class: class) ~~ #none]
]

{ #category : 'change set to use' }
ChangeSet class >> baseSystemNameFor: aChangeSetNumber [

	^String streamContents: [ :strm |
		strm
			nextPutAll: (self baseSystemPrefixFor: aChangeSetNumber);
			nextPutAll: '-CuisCore-';
"			nextPutAll: Utilities authorName asCamelCase;"
			nextPutAll: 'AuthorName';
"			nextPutAll: '-';
			nextPutAll: Utilities authorInitials asCamelCase;"
			nextPutAll: '-' ]
]

{ #category : 'change set to use' }
ChangeSet class >> baseSystemPrefixFor: aChangeSetNumber [

	^String streamContents: [ :strm |
		strm
			nextPutAll: SystemVersion current updatesPrefix;
			nextPutAll: (aChangeSetNumber printString padded: #left to: 4 with: $0) ]
]

{ #category : 'change set to use' }
ChangeSet class >> changeSetForBaseSystem [

	| csName numberToUse |
	self notInstallOrTestRun
		ifTrue: [
			numberToUse := self currentBaseCSNumber.
			AllChangeSets
				detect: [ :any | any name beginsWith: (ChangeSet baseSystemPrefixFor: numberToUse) ]
				ifFound: [ :existing | ^existing ]
				ifNone: [
					csName := (self baseSystemNameFor: numberToUse),
						(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]).
					^ChangeSet existingOrNewChangeSetNamed: csName forBaseSystem: true ]]

		ifFalse: [
			csName := 'Affects-BaseSystem--', Installing.
			^ChangeSet existingOrNewChangeSetNamed: csName forBaseSystem: false
			"Changes are for the base system, but are not user's own changes..." ].
]

{ #category : 'change set to use' }
ChangeSet class >> changeSetForPackage: aCodePackage [
	| csName |
	aCodePackage ifNil: [ ^ self changeSetForBaseSystem ].
	^self notInstallOrTestRun
		ifTrue: [
			ChangeSet existingOrNewChangeSetForPackage: aCodePackage ]
		ifFalse: [
			csName := Installing = ('Install-' , aCodePackage packageName)
				ifTrue: [ Installing ]
				ifFalse: [ 'Affects-' , aCodePackage packageName, '--' , Installing ].
			ChangeSet
				existingOrNewChangeSetNamed: csName
				forBaseSystem: false ]
]

{ #category : 'enumerating' }
ChangeSet class >> changeSetNamed: aName [
	"Return the change set of the given name, or nil if none found."

	^ AllChangeSets
		detect: [:aChangeSet | aChangeSet name = aName]
		ifNone: nil
]

{ #category : 'system change notifications' }
ChangeSet class >> classAdded: aClass inCategory: aCategoryName [

	| packageOrNil |

	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classAdded: aClass inCategory: aCategoryName ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]
]

{ #category : 'system change notifications' }
ChangeSet class >> classCommented: aClass [

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfClass: aClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classCommented: aClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ].
]

{ #category : 'system change notifications' }
ChangeSet class >> classDefinitionChangedFrom: oldClass to: newClass oldClassDefinition: oldClassDefinition [
	"In case the class is moved from one package to another, both change sets should be affected.
	But there's no need to do it here, as #classRecategorized:from:to: is also called."

	| packageOrNil |
	
	newClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfClass: newClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classDefinitionChangedFrom: oldClass to: newClass oldClassDefinition: oldClassDefinition ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]
]

{ #category : 'system change notifications' }
ChangeSet class >> classRecategorized: aClass from: oldCategory to: newCategory [
	"If the class was moved to a dfferent package, out of the base system, record the fact in the change set.
	The actual class redefinition is done at #classDefinitionChangedFrom:to: that is also called (if the class really changed)."

	| oldPackageOrNil newPackageOrNil newChangeSet |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	newPackageOrNil := CodePackage
		packageOfSystemCategory: newCategory
		ifNone: nil.
	newPackageOrNil ifNotNil: [ newPackageOrNil hasUnsavedChanges: true ].
	newChangeSet := ChangeSet changeSetForPackage: newPackageOrNil.
	newChangeSet noteRecategorizationOfClass: aClass.

	oldPackageOrNil := CodePackage
		packageOfSystemCategory: oldCategory
		ifNone: nil.
	oldPackageOrNil
		ifNotNil: [ oldPackageOrNil hasUnsavedChanges: true ]
		ifNil: [
			"If destination is a package, but source isn't, then record the change in the base system changeset"
			newPackageOrNil ifNotNil: [
				self changeSetForBaseSystem noteClassMoveToOtherPackage: aClass ]]
]

{ #category : 'system change notifications' }
ChangeSet class >> classRemoved: aClass fromCategory: aCategoryName [

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classRemoved: aClass fromCategory: aCategoryName ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]
]

{ #category : 'system change notifications' }
ChangeSet class >> classReorganized: aClass [

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfClass: aClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classReorganized: aClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]
]

{ #category : 'enumerating' }
ChangeSet class >> countOfChangeSetsWithClass: aClass andSelector: aSelector [
	"Answer how many change sets record a change for the given class and selector"

	^ (self allChangeSetsWithClass: aClass selector: aSelector) size
]

{ #category : 'change set to use' }
ChangeSet class >> currentBaseCSNumber [
	CurrentBaseCSNumber ifNil: [
		CurrentBaseCSNumber := SystemVersion current highestUpdate + 1 ].
	^CurrentBaseCSNumber
]

{ #category : 'enumerating' }
ChangeSet class >> doesAnyChangeSetHaveClass: aClass andSelector: aSelector [
	"Answer whether any known change set bears a change for the given class and selector"

	^ (self countOfChangeSetsWithClass: aClass andSelector: aSelector) > 0
]

{ #category : 'enumerating' }
ChangeSet class >> existingOrNewChangeSetForPackage: aCodePackage [
	| answer |
	answer := ChangeSet
		existingOrNewChangeSetNamed: 'UnsavedChangesTo-' , aCodePackage packageName
		forBaseSystem: false.
	answer codePackage: aCodePackage.
	^answer
]

{ #category : 'enumerating' }
ChangeSet class >> existingOrNewChangeSetNamed: aName forBaseSystem: aBoolean [

	| newSet |
	^(ChangeSet changeSetNamed: aName)
		ifNotNil: [ :existing |
			existing assert: existing isForBaseSystem = aBoolean ]
		ifNil: [
			newSet := ChangeSet named: aName.
			newSet isForBaseSystem: aBoolean.
			AllChangeSets add: newSet.
			newSet ]
]

{ #category : 'services' }
ChangeSet class >> fileIn: aFileEntry [
	"File in the entire contents of the file specified by the name provided"

	aFileEntry ifNil: [^ Smalltalk beep ].
	aFileEntry readStreamDo: [ :stream |
		stream ifNotNil: [ stream fileIn ]]
]

{ #category : 'file list services' }
ChangeSet class >> fileReaderServicesForFileEntry: filename suffix: suffix [
	"These would better be done by ChangeSorter!"

	(#('cs' 'cs.st') includes: suffix) ifTrue: [
		^{self serviceInstall} ].

	(#('st') includes: suffix) ifTrue: [
		^{self serviceFileIn} ].

	^#()
]

{ #category : 'change set to use' }
ChangeSet class >> incrementCurrentBaseCSNumber [
	CurrentBaseCSNumber := self currentBaseCSNumber +1
]

{ #category : 'class initialization' }
ChangeSet class >> initialize [
	"
	ChangeSet initialize
	"
	AllChangeSets := OrderedCollection new.

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to:oldClassDefinition: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #aboutToRenameClass send: #aboutToRenameClass:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;
		when: #methodAddedInProtocolTimeStamp send: #methodAdded:selector:inProtocol:class:methodTimeStamp:requestor: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self.
]

{ #category : 'services' }
ChangeSet class >> install: aFileEntry [
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	ChangeSet installing: aFileEntry name do: [ self fileIn: aFileEntry ].
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		('Installed ChangeSet: ', aFileEntry name) print]
]

{ #category : 'services' }
ChangeSet class >> installNewUpdates [
 	
	| updatesFileDirectory |
	updatesFileDirectory := DirectoryEntry coreUpdatesDirectory.

	updatesFileDirectory exists ifFalse: [
		^self inform: 'Could not find a CoreUpdates folder\No updates loaded' withNewLines ].
		
	ChangeSet installNewUpdates: updatesFileDirectory.
]

{ #category : 'services' }
ChangeSet class >> installNewUpdates: updatesFileDirectory [

	| warnUser |
	self newUpdates: updatesFileDirectory do: [ :each |
		self install: each ].	
	warnUser := false.
	CodePackage installedPackages keysAndValuesDo: [ :pckName :package |
		package fullFileName ifNotNil: [ :fullFileName | | providedFeatureInFile |
			providedFeatureInFile := CodePackageFile providedFeatureInFile: fullFileName asFullFileEntry.
			(providedFeatureInFile isLaterThan: package provides) ifTrue: [
				warnUser := true.
				package hasUnsavedChanges
					ifTrue: [ ('Package: ', pckName, '. Package has unsaved changes, but there is a newer version of the file.') print ]
					ifFalse: [ ('Package: ', pckName, '. There is a newer version than currently loaded.') print ]]]].
	warnUser
		ifTrue: [
			self inform: Smalltalk versionString, ' - ', Smalltalk lastUpdateString, String newLineString,
'Some package(s) loaded in the image
have updated package file on disk.
Please see Transcript.' ]
		ifFalse: [
			Smalltalk systemInformationString print ].
]

{ #category : 'change set to use' }
ChangeSet class >> installing: aCodePackageOrChangeSetName do: aBlock [

	| currentCS currentCSNumber newHighestUpdate |
	Installing := 'Install-', aCodePackageOrChangeSetName.
	aBlock ensure: [ Installing := nil ].
	Smalltalk forceChangesToDisk.

	SystemVersion current registerUpdate: aCodePackageOrChangeSetName.
	newHighestUpdate :=  SystemVersion current highestUpdate.

	currentCSNumber := self currentBaseCSNumber.
	currentCS := self changeSetForBaseSystem.
	currentCS isEmpty ifTrue: [
		ChangeSet removeChangeSet: currentCS.
		currentCSNumber > newHighestUpdate ifFalse: [
			CurrentBaseCSNumber := newHighestUpdate + 1 ]]
]

{ #category : 'change set to use' }
ChangeSet class >> installingString [
	^Installing 
]

{ #category : 'change set to use' }
ChangeSet class >> isInstalling: aCodePackage [

	^Installing = ('Install-', aCodePackage packageName)
]

{ #category : 'system change notifications' }
ChangeSet class >> methodAdded: aCompiledMethod selector: aSymbol inProtocol: aCategoryName class: aClass methodTimeStamp: aTimeStampString requestor: requestor [

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfMethod: aCompiledMethod methodReference ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodAdded: aCompiledMethod
			selector: aSymbol
			inProtocol: aCategoryName
			class: aClass
			methodTimeStamp: aTimeStampString
			requestor: requestor ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]
]

{ #category : 'system change notifications' }
ChangeSet class >> methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor [

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfMethod: newMethod methodReference ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodChangedFrom: oldMethod
			to: newMethod
			selector: aSymbol
			inClass: aClass
			requestor: requestor ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]
]

{ #category : 'system change notifications' }
ChangeSet class >> methodRemoved: aCompiledMethod selector: aSymbol inProtocol: aCategoryName class: aClass [

	| packageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	packageOrNil := CodePackage packageOfMethodCategory: aCategoryName ofClass: aClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodRemoved: aCompiledMethod
			selector: aSymbol
			inProtocol: aCategoryName
			class: aClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]
]

{ #category : 'instance creation' }
ChangeSet class >> named: aName [

	^ (self basicNew name: aName) initialize
]

{ #category : 'instance creation' }
ChangeSet class >> new [

	self error: 'ChangeSets are created calling #existingOrNewChangeSetNamed:'
]

{ #category : 'services' }
ChangeSet class >> newUpdates: updatesFileDirectory do: aBlock [
	| prefix updates |
	prefix := SystemVersion current updatesPrefix.
	updates := SortedCollection sortBlock: [ :a :b | a name < b name ].
	updatesFileDirectory filesDo: [ :fileEntry |
		(fileEntry name beginsWith: prefix) ifTrue: [
			(fileEntry name integerOrNilAt: prefix size + 1) ifNotNil: [ :updateNumber |
				updateNumber > SystemVersion current highestUpdate ifTrue: [
					updates add: fileEntry ]]]].
	updates do: aBlock.
]

{ #category : 'change set to use' }
ChangeSet class >> notInstallOrTestRun [

	^Installing isNil or: [
		(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name ~= 'TestRunner' ]]
	
	"
	(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name = 'TestRunner' ]	-> Test
	(Installing beginsWith: Install-')															-> Install
	Installing isNil 																			-> Normal
	(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name ~= 'TestRunner' ]	-> Normal
	"
]

{ #category : 'removing' }
ChangeSet class >> removeChangeSet: aChangeSet [
	"Remove the given changeSet. Caller must assure that it's ok to do this."

	"If the change set to be removed was using the current number for base system changes,
	and it is not empty, start using the following number for the upcoming changes."
	((aChangeSet name beginsWith: (ChangeSet baseSystemPrefixFor: ChangeSet currentBaseCSNumber)) and: [
		aChangeSet isEmpty not ])
			ifTrue: [
				ChangeSet incrementCurrentBaseCSNumber ].

	AllChangeSets remove: aChangeSet ifAbsent: nil.
	aChangeSet wither.
]

{ #category : 'change set to use' }
ChangeSet class >> runningTest: aTestName do: aBlock [

	Installing := 'RunningTest-', aTestName.
	aBlock ensure: [ Installing := nil ]
]

{ #category : 'system change notifications' }
ChangeSet class >> selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass [
	"If the method was moved to a dfferent package, affect the package that lost the it. Tell it that it lost the method.
	The actual method redefinition is done at one of the method definition methods, that is also called."

	| newPackageOrNil newChangeSet oldPackageOrNil |
	
	aClass wantsChangeSetLogging ifFalse: [ ^self ].
	
	newPackageOrNil := CodePackage packageOfMethodCategory: newCategory ofClass: aClass ifNone: nil.
	newPackageOrNil ifNotNil: [ newPackageOrNil hasUnsavedChanges: true ].
	newChangeSet := ChangeSet changeSetForPackage: newPackageOrNil.
	newChangeSet selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass.

	oldPackageOrNil := CodePackage packageOfMethodCategory: oldCategory ofClass: aClass ifNone: nil.
	oldPackageOrNil
		ifNotNil: [ oldPackageOrNil hasUnsavedChanges: true ]
		ifNil: [
			"If destination is a package, but source isn't, then record the change in the base system changeset"
			newPackageOrNil ifNotNil: [
				self changeSetForBaseSystem noteMethodMoveToOtherPackage: selector forClass: aClass ]]
]

{ #category : 'file list services' }
ChangeSet class >> serviceFileIn [
	"Answer a service for filing in an entire file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'file in'
		selector: #fileIn:
		description: 'file in the entire contents of the file, which is expected to contain Smalltalk code in fileout ("chunk") format'
		buttonLabel: 'filein'
		icon: #saveIcon)
			sortOrder: 100
]

{ #category : 'file list services' }
ChangeSet class >> serviceInstall [
	"Answer a service for filing in an entire file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'install code'
		selector: #install:
		description: 'install change set'
		buttonLabel: 'install'
		icon: #saveIcon)
			sortOrder: 100
]

{ #category : 'removing' }
ChangeSet class >> zapAllChangeSets [
	"Keep those change sets that are recording the unsaved changes of some package. Also kee the change set currently capturing changes to base system: it should be filed out and destroyed by the user."
	AllChangeSets copy do: [ :changeSet |
		(changeSet isForBaseSystem or: [
			changeSet codePackage notNil and: [changeSet isEmpty not]])
		ifFalse: [
			self removeChangeSet: changeSet ]]
]

{ #category : 'system change notifications' }
ChangeSet >> aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName [

	self noteRenameClass: aClass as: newClassName
]

{ #category : 'private' }
ChangeSet >> addCoherency: className [
	"SqR! 19980923: If I recreate the class then don't remove it"

	(self changeRecorderFor: className)
		checkCoherence.
"
	classRemoves remove: className ifAbsent: [].
	(classChanges includesKey: className) ifTrue:
		[(classChanges at: className) remove: #remove ifAbsent: []]
"
]

{ #category : 'fileIn/Out' }
ChangeSet >> assurePostscriptExists [
	"Make sure there is a TextModel holding the postscript.  "

	"NOTE: FileIn recognizes the postscript by the line with Postscript: on it"
	postscript
		ifNil: [
			self postscriptString: '"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!).
Be sure to put any further comments in double-quotes, like this one."
']
]

{ #category : 'fileIn/Out' }
ChangeSet >> assurePreambleExists [
	"Make sure there is a TextModel holding the preamble; if it's found to have reverted to empty contents, put up the template"

	(preamble == nil or: [preamble actualContents isEmptyOrNil])
		ifTrue: [
			preamble := TextModel withText: self preambleTemplate.	
			self hasUnsavedChanges: true ]
]

{ #category : 'private' }
ChangeSet >> atClass: class add: changeType [

	(self changeRecorderFor: class)
		noteChangeType: changeType fromClass: class
]

{ #category : 'private' }
ChangeSet >> atClass: class includes: changeType [

	^(changeRecords at: class name ifAbsent: [^false])
		includesChangeType: changeType
]

{ #category : 'private' }
ChangeSet >> atSelector: selector class: class [

	^ (changeRecords at: class name ifAbsent: [^ #none])
		atSelector: selector ifAbsent: [^ #none]
]

{ #category : 'accessing' }
ChangeSet >> author [
	| author |
	self assurePreambleExists.
	author := self preambleString lineNumber: 3.
	author := author copyFrom: 8 to: author size. "Strip the 'Author:' prefix. Ugly ugly."	
	^author withBlanksTrimmed.
	
]

{ #category : 'testing' }
ChangeSet >> canHavePreambleAndPostscript [
	"Don't allow preambles and postscripts in changesets for Packages,
	because packages don't support them. Use prerequisites and class initialize methods instead."
	
	^ self isForBaseSystem
]

{ #category : 'private' }
ChangeSet >> changeRecorderFor: classOrClassName [

	| cname |
	cname := classOrClassName isString
		ifTrue: [ classOrClassName ]
		ifFalse: [ classOrClassName name ].

	"Later this will init the changeRecords so according to whether they should be revertable."
	^ changeRecords at: cname
			ifAbsent: [
				^ changeRecords at: cname
					put: (ClassChangeRecord new initFor: cname revertable: false)]
]

{ #category : 'class changes' }
ChangeSet >> changedClassCategories [

	| answer |
	answer := Dictionary new.
	self changedClasses do: [ :cls | 
		(answer at: cls category ifAbsentPut: Set new)
			add: cls ].
	^ answer
]

{ #category : 'class changes' }
ChangeSet >> changedClassNames [
	"Answer a OrderedCollection of the names of changed or edited classes.
	DOES include removed classes.  Sort alphabetically."

	^ changeRecords keysSortedSafely 
]

{ #category : 'class changes' }
ChangeSet >> changedClasses [
	"Answer an OrderedCollection of changed or edited classes.
	Does not include removed classes.  Sort alphabetically by name."

	"Much faster to sort names first, then convert back to classes.  Because metaclasses reconstruct their name at every comparison in the sorted collection.
	8/91 sw chgd to filter out non-existent classes (triggered by problems with class-renames"

	^ self changedClassNames
		collect: [:className | Smalltalk classNamed: className]
		thenSelect: [:aClass | aClass notNil]
]

{ #category : 'method changes' }
ChangeSet >> changedMessageList [
	"Used by a message set browser to access the list view information."

	| messageList |
	messageList := OrderedCollection new.
	changeRecords associationsDo: [ :clAssoc | | classNameInFull classNameInParts |
		classNameInFull := clAssoc key asString.
		classNameInParts := classNameInFull findTokens: ' '.

		(clAssoc value allChangeTypes includes: #comment) ifTrue: [
			messageList add:
				(MethodReference new
					setClassSymbol: classNameInParts first asSymbol
					classIsMeta: false 
					methodSymbol: #Comment 
					stringVersion: classNameInFull, ' Comment')].

		clAssoc value methodChangeTypes associationsDo: [ :mAssoc |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				messageList add:
					(MethodReference new
						setClassSymbol: classNameInParts first asSymbol
						classIsMeta: classNameInParts size > 1 
						methodSymbol: mAssoc key 
						stringVersion: classNameInFull, ' ' , mAssoc key)]]].
	^ messageList sorted
]

{ #category : 'method changes' }
ChangeSet >> changedMessageListAugmented [
	"Even added classes have all messages in changedMessageList."
	^ self changedMessageList asArray
]

{ #category : 'fileIn/Out' }
ChangeSet >> checkForSlips [
	"Return a collection of method refs with possible debugging code in them."
	| slips |
	slips := OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc |  | method |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				method := aClass compiledMethodAt: mAssoc key ifAbsent: nil.
				method ifNotNil: [
					method hasReportableSlip
						ifTrue: [slips add: (MethodReference method: method)]]]]].
	^ slips
]

{ #category : 'system change notifications' }
ChangeSet >> classAdded: aClass inCategory: aCategoryName [

	self noteAddClass: aClass
]

{ #category : 'class changes' }
ChangeSet >> classChangeAt: className [
	"Return what we know about class changes to this class."

	^ (changeRecords at: className ifAbsent: [^ Set new])
		allChangeTypes
]

{ #category : 'system change notifications' }
ChangeSet >> classCommented: aClass [

	self noteCommentClass: aClass
]

{ #category : 'system change notifications' }
ChangeSet >> classDefinitionChangedFrom: oldClass to: newClass oldClassDefinition: oldClassDefinition [

	(newClass hasChangedComparedTo: oldClass) ifTrue: [ 
		self noteChangeClass: newClass from: oldClass oldClassDefinition: oldClassDefinition ]
]

{ #category : 'system change notifications' }
ChangeSet >> classRecategorized: aClass from: oldCategory to: newCategory [

	self noteRecategorizationOfClass: aClass
]

{ #category : 'system change notifications' }
ChangeSet >> classRemoved: aClass fromCategory: aCategoryName [

	self noteRemovalOf: aClass fromCategory: aCategoryName
]

{ #category : 'system change notifications' }
ChangeSet >> classReorganized: aClass [

	self noteReorganizeClass: aClass
]

{ #category : 'initialization' }
ChangeSet >> clear  [
	"Reset the receiver to be empty.  "

	changeRecords := Dictionary new.
	preamble := nil.
	postscript := nil.
	self hasUnsavedChanges: false.
	self isForBaseSystem: true 	"Not a great default, but at least some Boolean"
]

{ #category : 'accessing' }
ChangeSet >> codePackage [
	^codePackage
]

{ #category : 'accessing' }
ChangeSet >> codePackage: aCodePackage [
	codePackage := aCodePackage.
	self isForBaseSystem: false
]

{ #category : 'accessing' }
ChangeSet >> editPostscript [
	"edit the receiver's postscript, in a separate window."

	self assurePostscriptExists.
	postscript openLabel: 'Postscript for ChangeSet named ' , name
]

{ #category : 'class changes' }
ChangeSet >> fatDefForClass: class [

	| newDef oldDef oldStrm newStrm outStrm oldVars newVars addedVars |
	newDef := class definition.
	oldDef := (self changeRecorderFor: class) priorDefinition.
	oldDef ifNil: [^ newDef].
	oldDef = newDef ifTrue: [^ newDef].

	oldStrm := oldDef readStream.
	newStrm := newDef readStream.
	outStrm := String writeStream.

	"Merge inst vars from old and new defs..."
	oldStrm upToAll: 'instanceVariableNames'; upTo: $'.
	outStrm 
		nextPutAll: (newStrm upToAll: 'instanceVariableNames'); 
		nextPutAll: 'instanceVariableNames:'.
	newStrm peek == $: ifTrue: [newStrm next].	"may or may not be there, but already written"
	outStrm
		nextPutAll: (newStrm upTo: $'); nextPut: $'.
	oldVars := (oldStrm upTo: $') findTokens: Character separators.
	newVars := (newStrm upTo: $') findTokens: Character separators.
	addedVars := oldVars asSet addAll: newVars; removeAll: oldVars; asOrderedCollection.
	oldVars , addedVars do: [:var | outStrm nextPutAll: var; space].
	outStrm nextPut: $'.

	class isMeta ifFalse: [
		"Merge class vars from old and new defs..."
		oldStrm upToAll: 'classVariableNames:'; upTo: $'.
		outStrm nextPutAll: (newStrm upToAll: 'classVariableNames:'); nextPutAll: 'classVariableNames:';
			nextPutAll: (newStrm upTo: $'); nextPut: $'.
		oldVars := (oldStrm upTo: $') findTokens: Character separators.
		newVars := (newStrm upTo: $') findTokens: Character separators.
		addedVars := oldVars asSet addAll: newVars; removeAll: oldVars; asOrderedCollection.
		oldVars , addedVars do: [:var | outStrm nextPutAll: var; space].
		outStrm nextPut: $'].

	outStrm nextPutAll: newStrm upToEnd.
	^ outStrm contents

]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOut [
	"File out the receiver, to a file whose name is a function of the  
	change-set name and a unique numeric tag."

	| folder slips nameToUse |
	folder := DirectoryEntry savedChangeSetsDirectory.
	nameToUse := self name.
	nameToUse := nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse := folder nextNameFor: nameToUse coda: '-', Utilities authorInitials extension: 'cs.st'.

	folder // nameToUse writeStreamDo: [ :fileStream |
		fileStream ifNotNil: [
			fileStream writeSystemTimeStamp.
			self fileOutPreambleOn: fileStream.
			self fileOutOn: fileStream.
			self fileOutPostscriptOn: fileStream ]].

	self hasUnsavedChanges: false.
	(Preferences at: #checkForSlips) 		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips notEmpty
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ].
]

{ #category : 'private' }
ChangeSet >> fileOutClassDefinition: class on: stream  [
	"Write out class definition for the given class on the given stream, if the class definition was added or changed."

	(self atClass: class includes: #rename) ifTrue: [
		stream nextChunkPut: 'Smalltalk renameClassNamed: #', (self oldNameFor: class), ' as: #', class name; newLine].

	(self atClass: class includes: #change) ifTrue: [ "fat definition only needed for changes"
		stream
			nextPut: $!; nextChunkPut: class definitionPreambleWithoutStamp; newLine;
			nextChunkPut: (self fatDefForClass: class); newLine.
	] ifFalse: [
		(self atClass: class includes: #add) ifTrue: [ "use current definition for add"
			stream
				nextPut: $!; nextChunkPut: class definitionPreambleWithoutStamp; newLine;
				nextChunkPut: class definition; newLine.
		].
	].

	(self atClass: class includes: #comment) ifTrue: [
		class theNonMetaClass organization putCommentOnFile: stream numbered: 0 moveSource: false forClass: class theNonMetaClass.
		stream newLine].


]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutClassDefinitionsOf: classList on: stream [

	classList do: [ :aClass | self fileOutClassDefinition: aClass on: stream ]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutClassDeletionFrom: aClassChangeRecord doItOnlyIfInBaseSystem: aDoIt on: stream [

	| record |
	
	record := ClassDeletionChangeRecord 
		className: aClassChangeRecord thisName 
		definition: aClassChangeRecord priorDefinition 
		doItOnlyIfInBaseSystem: aDoIt 
		stamp: aClassChangeRecord stamp.
		
	record fileOutOn: stream
	

]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutClassInitializationsOf: classList on: stream [

	classList do: [ :aClass |
		self fileOutPSFor: aClass on: stream ]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutMethodAdditionsFor: class on: stream [
	"Write out all the method changes for this class."

	| changes |
	changes := Set new.
	(self methodChangesAtClass: class name) associationsDo: [ :mAssoc |
		mAssoc value == #add
			ifTrue: [ changes add: mAssoc key ]].
	changes isEmpty ifFalse: [
		class fileOutChangedMessages: changes on: stream.
		stream newLine ]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutMethodChangesFor: class on: stream [
	"Write out all the method changes for this class."

	| changes |
	changes := Set new.
	(self methodChangesAtClass: class name) associationsDo: [ :mAssoc |
		(mAssoc value == #remove
			or: [ mAssoc value == #addedThenRemoved
				or: [ mAssoc value == #add
					or: [ mAssoc value == #movedToOtherPackage ]]])
			ifFalse: [ changes add: mAssoc key ]].
	changes isEmpty ifFalse: [
		class fileOutChangedMessages: changes on: stream.
		stream newLine ]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutMethodMovedToOtherPackagesFor: class on: stream  [
	"Write out removals and initialization for this class."

	self 
		fileOutMethodRemovalsOf: #(movedToOtherPackage) 
		movedToOtherPackage: true
		for: class 
		on: stream

]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutMethodRemovalOf: selector for: class movedToOtherPackage: moved on: stream stamp: stamp  [

	| methodReference changeRecord |
	
	methodReference := MethodReference class: class selector: selector.
	
	changeRecord := MethodDeletionChangeRecord 
		methodReference: methodReference
		doItOnlyIfInBaseSystem: moved
		source: (methodReference sourceCodeIfAbsent: [ 'Missing' ]) 
		stamp: stamp.
		
	changeRecord fileOutOn: stream 
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutMethodRemovalsFor: class on: stream  [
	"Write out removals and initialization for this class."

	self 
		fileOutMethodRemovalsOf: #(remove addedThenRemoved) 
		movedToOtherPackage: false
		for: class 
		on: stream
		
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutMethodRemovalsOf: changeTypes movedToOtherPackage: moved for: class on: stream  [
	"Write out removals and initialization for this class."

	| classRecord methodChanges changeType |
	
	classRecord := changeRecords at: class name ifAbsent: [^ self].
	methodChanges := classRecord methodChanges.
	
	methodChanges keysAndValuesDo: [:selector :aMethodChange |
		changeType := aMethodChange changeType.
		(changeTypes includes: changeType)
			ifTrue: [ self 
				fileOutMethodRemovalOf: selector 
				for: class 
				movedToOtherPackage: moved 
				on: stream 
				stamp: aMethodChange stamp ]].		 

]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutMethodsAdditionsOf: classList on: stream [

	classList do: [ :aClass | self fileOutMethodAdditionsFor: aClass on: stream ]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutMethodsChangesOf: stream on: classList [

	classList do: [ :aClass | self fileOutMethodChangesFor: aClass on: stream ]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutMovedClassRecord: aRemovedClassRecord on: stream [

	self fileOutClassDeletionFrom: aRemovedClassRecord doItOnlyIfInBaseSystem: true on: stream

]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutMovedClassesOn: stream [

	self sortedMovedClassesToOtherPackage do: [ :aMovedClassRecord |
		self fileOutMovedClassRecord: aMovedClassRecord on: stream ]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutOn: stream  [
	"Write out all the changes the receiver knows about"

	| classList |
	
	self isEmpty ifTrue: [ self inform: 'Warning: no changes to file out' ].
	
	classList := Array streamContents: [ :strm |
		Smalltalk hierarchySorted: self changedClasses do: [ :cls | strm nextPut: cls ]].

	self fileOutClassDefinitionsOf: classList on: stream.
	self fileOutMethodsAdditionsOf: classList on: stream.
	self fileOutMethodsChangesOf: stream on: classList.
	self fileOutRemovedAndMovedMethodsOf: classList on: stream.
	self fileOutClassInitializationsOf: classList on: stream.
	self fileOutRemovedClassesOn: stream.
	self fileOutMovedClassesOn: stream.

]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutPSFor: class on: stream  [
	"Write out removals and initialization for this class."

	| dict classRecord currentDef |
	classRecord := changeRecords at: class name ifAbsent: [^ self].
	dict := classRecord methodChangeTypes.
	((dict includesKey:  #initialize) and: [ class isMeta ]) ifTrue: [
		stream nextChunkPut: class soleInstance name, ' initialize'; newLine].
	((classRecord includesChangeType: #change)
			and: [(currentDef := class definition) ~= (self fatDefForClass: class)]) ifTrue: [
		stream
			nextPut: $!;
			nextChunkPut: class definitionPreambleWithoutStamp; newLine;
			nextChunkPut: currentDef; newLine].
	(classRecord includesChangeType: #reorganize) ifTrue: [
		class fileOutOrganizationOn: stream excludingExtensions: true.
		stream newLine]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutPostscriptOn: stream  [
	"If the receiver has a postscript, put it out onto the stream.  "

	| aString |
	aString := self postscriptString.
	(aString notNil and: [ aString size > 0])
		ifTrue: [
			stream nextChunkPut: aString "surroundedBySingleQuotes".
			stream newLine; newLine]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutPreambleOn: stream  [
	"If the receiver has a preamble, put it out onto the stream.  "

	| aString |
	aString := self preambleString.
	(aString notNil and: [aString size > 0])
		ifTrue: [
			stream nextChunkPut: aString "surroundedBySingleQuotes".
			stream newLine; newLine]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutRemovedAndMovedMethodsOf: classList on: stream [

	classList reverseDo: [ :aClass |
		self fileOutMethodRemovalsFor: aClass on: stream.
		self fileOutMethodMovedToOtherPackagesFor: aClass on: stream ]
]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutRemovedClassRecord: aRemovedClassRecord on: stream [

	self fileOutClassDeletionFrom: aRemovedClassRecord doItOnlyIfInBaseSystem: false on: stream

]

{ #category : 'fileIn/Out' }
ChangeSet >> fileOutRemovedClassesOn: stream [

	self sortedRemovedClassRecords do: [ :aRemovedClassRecord | 
		self fileOutRemovedClassRecord: aRemovedClassRecord on: stream ]
]

{ #category : 'accessing' }
ChangeSet >> hasPostscript [
	^ postscript notNil
]

{ #category : 'moving changes' }
ChangeSet >> hasPreamble [
	^ preamble notNil
]

{ #category : 'accessing' }
ChangeSet >> hasUnsavedChanges [

	^hasUnsavedChanges
]

{ #category : 'accessing' }
ChangeSet >> hasUnsavedChanges: aBoolean [

	hasUnsavedChanges := aBoolean.
	self triggerEvent: #dirtyFlagChanged
]

{ #category : 'initialization' }
ChangeSet >> initialize  [
	"Initialize the receiver to be empty."

	name ifNil:
		[^ self error: 'All changeSets must be registered, as in ChangeSorter newChangeSet'].
	self clear.

]

{ #category : 'testing' }
ChangeSet >> isEmpty [
	"Answer whether the receiver contains any elements."
	
	changeRecords ifNil: [^true].
	^ changeRecords isEmpty 
]

{ #category : 'accessing' }
ChangeSet >> isForBaseSystem [

	^isForBaseSystem
]

{ #category : 'accessing' }
ChangeSet >> isForBaseSystem: aBoolean [

	isForBaseSystem := aBoolean
]

{ #category : 'testing' }
ChangeSet >> isMoribund [
	"Answer whether the receiver is obsolete and about to die; part of an effort to get such guys cleared out from the change sorter.  2/7/96 sw"

	^ name == nil 
]

{ #category : 'testing' }
ChangeSet >> isWithClass: aClass  [
	
	^changeRecords includesKey: aClass name
]

{ #category : 'method changes' }
ChangeSet >> messageListForChangesWhich: aBlock ifNone: ifEmptyBlock [

	| answer |

	answer := self changedMessageListAugmented select: [ :each |
		aBlock value: each actualClass value: each methodSymbol
	].
	answer isEmpty ifTrue: [^ifEmptyBlock value].
	^answer

]

{ #category : 'system change notifications' }
ChangeSet >> methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass methodTimeStamp: aTimeStampString requestor: requestor [

	self
		noteNewMethod: aMethod
		forClass: aClass
		selector: aSymbol
		priorMethod: nil
]

{ #category : 'system change notifications' }
ChangeSet >> methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor [

	self
		noteNewMethod: newMethod
		forClass: aClass
		selector: aSymbol
		priorMethod: oldMethod
]

{ #category : 'accessing' }
ChangeSet >> methodChanges [

	| methodChangeDict |
	methodChangeDict := Dictionary new.
	changeRecords associationsDo: [ :assn | | changeTypes |
		changeTypes := assn value methodChangeTypes.
		changeTypes isEmpty ifFalse: [methodChangeDict at: assn key put: changeTypes]].
	^ methodChangeDict
]

{ #category : 'method changes' }
ChangeSet >> methodChangesAtClass: className [
	"Return an old-style dictionary of method change types."

	^(changeRecords at: className ifAbsent: [^ Dictionary new])
		methodChangeTypes
]

{ #category : 'accessing' }
ChangeSet >> methodInfoFromRemoval: classAndSelector [

	^ (self changeRecorderFor: classAndSelector first)
		infoFromRemoval: classAndSelector last
]

{ #category : 'system change notifications' }
ChangeSet >> methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass [

	self
		noteRemoveSelector: aSymbol
		class: aClass
		priorMethod: aMethod
		lastMethodInfo: {aMethod sourcePointer. protocol}
]

{ #category : 'moving changes' }
ChangeSet >> methodsWithAnyInitialsOtherThan: myInits [
	"Return a collection of method refs whose author appears to be different from the given one, even historically"
	| slips |
	slips := Set new.
	self changedClasses do: [:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | method |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				method := aClass compiledMethodAt: mAssoc key ifAbsent: nil.
				method ifNotNil: [
					(aClass changeRecordsAt: mAssoc key) do: [ :chg | | aTimeStamp |
						aTimeStamp := chg stamp.
						(aTimeStamp notNil and: [(aTimeStamp beginsWith: myInits) not])
							ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]]].
	^ slips
]

{ #category : 'moving changes' }
ChangeSet >> methodsWithInitialsOtherThan: myInits [
	"Return a collection of method refs whose author appears to be different from the given one"
	| slips |
	slips := OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | method |
				(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
					method := aClass compiledMethodAt: mAssoc key ifAbsent: nil.
					method ifNotNil: [ | authorAndStamp |
						((authorAndStamp := method authorAndStamp) notNil and: [
							(authorAndStamp beginsWith: myInits) not])
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithInitialsOtherThan: 'sw') name: 'authoring problems'
	"
]

{ #category : 'moving changes' }
ChangeSet >> methodsWithoutComments [
	"Return a collection representing methods in the receiver which have no precode comments"

	| slips |
	slips := OrderedCollection new.
	self changedClasses do:
		[:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: 
				[:mAssoc | (#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse:
					[(aClass selectors includes:  mAssoc key) ifTrue:
						[(aClass firstPrecodeCommentFor: mAssoc key) isEmptyOrNil
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithoutComments) name: 'methods lacking comments'
	"
]

{ #category : 'accessing' }
ChangeSet >> movedClassRecords [

	^ changeRecords values select: [ :aChangeRecord | aChangeRecord isClassMoveToOtherPackage ]
]

{ #category : 'accessing' }
ChangeSet >> name [
	"The name of this changeSet.
	 2/7/96 sw: If name is nil, we've got garbage.  Help to identify."

	^ name
		ifNil: ['<no name -- garbage?>']
		ifNotNil: [name]
]

{ #category : 'accessing' }
ChangeSet >> name: anObject [
	name := anObject
]

{ #category : 'change logging' }
ChangeSet >> noteAddClass: class [
	"Include indication that a new class was created."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #new.
	self atClass: class add: #change.
	self addCoherency: class name.
	self hasUnsavedChanges: true
]

{ #category : 'change logging' }
ChangeSet >> noteChangeClass: class from: oldClass oldClassDefinition: oldClassDefinition [
	"Remember that a class definition has been changed.  Record the original structure, so that a conversion method can be built."

	class wantsChangeSetLogging ifFalse: [^ self].
	class isMeta 
		ifFalse: [self atClass: class add: #change]	"normal"
		ifTrue: [((self classChangeAt: class theNonMetaClass name) includes: #add) 
			ifTrue: [self atClass: class add: #add] 	"When a class is defined, the metaclass
				is not recorded, even though it was added.  A further change is
				really just part of the original add."
			ifFalse: [self atClass: class add: #change]].
	self addCoherency: class name.
	(self changeRecorderFor: class) notePriorClassDefinition: oldClassDefinition.
	self hasUnsavedChanges: true
]

{ #category : 'change logging' }
ChangeSet >> noteClassMoveToOtherPackage: class [
	"The class is about to be moved to some other package, who will hold it.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #movedToOtherPackage.
	changeRecords removeKey: class class name ifAbsent: nil.
	self hasUnsavedChanges: true
]

{ #category : 'class changes' }
ChangeSet >> noteCommentClass: class  [
	"Include indication that a class comment has been changed."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #comment.
	self hasUnsavedChanges: true
]

{ #category : 'change logging' }
ChangeSet >> noteMethodMoveToOtherPackage: selector forClass: class [

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteMethodMoveToOtherPackage: selector.
	self hasUnsavedChanges: true
]

{ #category : 'change logging' }
ChangeSet >> noteNewMethod: newMethod forClass: class selector: selector priorMethod: methodOrNil [

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteNewMethod: newMethod selector: selector priorMethod: methodOrNil.
	self hasUnsavedChanges: true
]

{ #category : 'change logging' }
ChangeSet >> noteRecategorizationOfClass: aClass [
	"Remember that a class definition has been changed.  Record the original structure, so that a conversion method can be built."

	aClass wantsChangeSetLogging ifFalse: [^ self].
	self atClass: aClass add: #change.
	self addCoherency: aClass name.
	self hasUnsavedChanges: true
]

{ #category : 'class changes' }
ChangeSet >> noteRemovalOf: class fromCategory: aCategoryName  [
	"The class is about to be removed from the system.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class) noteRemoved: class fromCategory: aCategoryName.
	changeRecords removeKey: class class name ifAbsent: nil.
	self hasUnsavedChanges: true
]

{ #category : 'change logging' }
ChangeSet >> noteRemoveSelector: selector class: class priorMethod: priorMethod lastMethodInfo: info [
	"Include indication that a method has been forgotten.
	info is a pair of the source code pointer and message category
	for the method that was removed."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteRemoveSelector: selector priorMethod: priorMethod lastMethodInfo: info.
	self hasUnsavedChanges: true
]

{ #category : 'change logging' }
ChangeSet >> noteRenameClass: class as: newName  [
	"Include indication that a class has been renamed."

	| recorder |
	(recorder := self changeRecorderFor: class)
		noteChangeType: #rename;
		noteNewName: newName asSymbol.
		
	"store under new name (metaclass too)"
	changeRecords at: newName put: recorder.
	changeRecords removeKey: class name.

	recorder := changeRecords at: class class name ifAbsent: nil.
	recorder ifNotNil: [
		changeRecords at: (newName, ' class') put: recorder.
		changeRecords removeKey: class class name.
		recorder noteNewName: newName , ' class' ].

	self hasUnsavedChanges: true
]

{ #category : 'class changes' }
ChangeSet >> noteReorganizeClass: class  [
	"Include indication that a class was reorganized."

	self atClass: class add: #reorganize.
	self hasUnsavedChanges: true
]

{ #category : 'fileIn/Out' }
ChangeSet >> objectForDataStream: refStrm [
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	"try to write reference for me"
	^ DiskProxy 
		global: #ChangeSet
		selector: #existingOrNewChangeSetNamed:forBaseSystem:
		args: (Array with: self name with: self isForBaseSystem)
"===
	refStrm replace: self with: nil.
	^ nil
==="

]

{ #category : 'testing' }
ChangeSet >> okayToRemove [
	"Should suggest a fileOut if changeSet is dirty... Implement this!"
	^ "self okayToRemoveInforming: true" true
]

{ #category : 'private' }
ChangeSet >> oldNameFor: class [

	^ (changeRecords at: class name) priorName
]

{ #category : 'fileIn/Out' }
ChangeSet >> postscriptString [
	"Answer the string representing the postscript.  "

	^ postscript
		ifNil: [postscript]
		ifNotNil: [postscript actualContents asPlainString]
]

{ #category : 'fileIn/Out' }
ChangeSet >> postscriptString: aString [

	postscript := TextModel withText: aString.
	self hasUnsavedChanges: true
]

{ #category : 'fileIn/Out' }
ChangeSet >> preambleString [
	"Answer the string representing the preamble"

	^ preamble
		ifNotNil: [ preamble actualContents asPlainString ]
]

{ #category : 'fileIn/Out' }
ChangeSet >> preambleString: aString [
	"Establish aString as the new contents of the preamble.  "

	preamble := TextModel withText: aString.
	self hasUnsavedChanges: true
]

{ #category : 'fileIn/Out' }
ChangeSet >> preambleTemplate [
	"Answer a string that will form the default contents for a change set's preamble.
	Just a first stab at what the content should be."

	^ String streamContents: [:strm |
		strm nextPutAll: '"Change Set:'.  "NOTE: fileIn recognizes preambles by this string."
		strm tab;tab; nextPutAll: self name.
		strm newLine; nextPutAll: 'Date:'; tab; tab; tab; nextPutAll: Date today printString.
		strm newLine; nextPutAll: 'Author:'; tab; tab; tab; nextPutAll: (Preferences at: #defaultAuthorName).
		strm newLine; newLine; nextPutAll: '<your descriptive text goes here>"']
"
ChangeSet changeSetForBaseSystem preambleTemplate
"
]

{ #category : 'accessing' }
ChangeSet >> printOn: aStream [
	"2/7/96 sw: provide the receiver's name in the printout"
	super printOn: aStream.
	aStream nextPutAll: ' named ', self name
]

{ #category : 'moving changes' }
ChangeSet >> removeClassChanges: classOrClassName [
	"Remove all memory of changes associated with this class"
	| cname |
	cname := classOrClassName isString
		ifTrue: [ classOrClassName ]
		ifFalse: [ classOrClassName name ].

	changeRecords removeKey: cname ifAbsent: nil.
]

{ #category : 'accessing' }
ChangeSet >> removePostscript [
	postscript := nil
]

{ #category : 'moving changes' }
ChangeSet >> removePreamble [
	preamble := nil
]

{ #category : 'method changes' }
ChangeSet >> removeSelectorChanges: selector class: class  [
	"Remove all memory of changes associated with the argument, selector, in 
	this class."

	| chgRecord |
	self hasUnsavedChanges: true.	"set the flag in any case"
	(chgRecord := changeRecords at: class name ifAbsent: [^ self])
		removeSelector: selector.
	chgRecord hasNoChanges ifTrue: [changeRecords removeKey: class name]
]

{ #category : 'accessing' }
ChangeSet >> removedClassRecords [

	^ changeRecords values select: [ :aChangeRecord | aChangeRecord isClassRemoval]
]

{ #category : 'system change notifications' }
ChangeSet >> selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass [

	self noteReorganizeClass: aClass
]

{ #category : 'method changes' }
ChangeSet >> selectorsInClass: aClass [
	"Used by a ChangeSorter to access the list methods."

	^ (changeRecords at: aClass ifAbsent: [^#()]) changedSelectors
]

{ #category : 'accessing' }
ChangeSet >> sortedClassRecords: classRecords [

	^ classRecords sort: [:left :rigth | left thisName < rigth thisName ]
]

{ #category : 'accessing' }
ChangeSet >> sortedMovedClassesToOtherPackage [

	^ self sortedClassRecords: self movedClassRecords
]

{ #category : 'accessing' }
ChangeSet >> sortedRemovedClassRecords [

	^ self sortedClassRecords: self removedClassRecords
]

{ #category : 'class changes' }
ChangeSet >> trimHistory  [
	"Drop non-essential history:  methods added and then removed, as well as rename and reorganization of newly-added classes."

	changeRecords do: [:chgRecord | chgRecord trimHistory]
]

{ #category : 'initialization' }
ChangeSet >> wither [
	"The receiver is to be clobbered.  Clear it out.  2/7/96 sw"

	self clear.
	name := nil
]
