'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 26 March 2012 at 6:36:02 pm'!
!classDefinition: #CodePackage category: #'Package Support'!
Object subclass: #CodePackage
	instanceVariableNames: 'packageName methodCategoryPrefix fullFileName hasUnsavedChanges '
	classVariableNames: 'InstalledPackages '
	poolDictionaries: ''
	category: 'Package Support'!

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/26/2012 17:54'!
             fullFileName

	^fullFileName ifNil: 'Never saved yet'! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/26/2012 17:52'!
   fullFileName: aString

	fullFileName _ aString! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/26/2012 18:05'!
          hasUnsavedChanges

	^hasUnsavedChanges! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/26/2012 18:05'!
                  hasUnsavedChanges: aBoolean

	hasUnsavedChanges _ aBoolean! !


!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/26/2012 18:26'!
       classAdded: aClass inCategory: aCategoryName

	| packageOrNil |
	packageOrNil _ CodePackage
		packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classAdded: aClass inCategory: aCategoryName ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/26/2012 18:14'!
               classCommented: aClass

	| packageOrNil |
	packageOrNil _ CodePackage packageOfClass: aClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classCommented: aClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ].! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/26/2012 18:29'!
            classDefinitionChangedFrom: oldClass to: newClass
	"In case the class is moved from one package to another, both change sets should be affected.
	But there's no need to do it here, as #classRecategorized:from:to: is also called."

	| packageOrNil |
	packageOrNil _ CodePackage packageOfClass: newClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classDefinitionChangedFrom: oldClass to: newClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/26/2012 18:27'!
 classRemoved: aClass fromCategory: aCategoryName

	| packageOrNil |
	packageOrNil _ CodePackage
		packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classRemoved: aClass fromCategory: aCategoryName ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/26/2012 18:28'!
       classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	| packageOrNil |
	packageOrNil _ CodePackage
		packageOfSystemCategory: aCategoryName ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			classRenamed: aClass
			from: oldClassName
			to: newClassName
			inCategory: aCategoryName ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/26/2012 18:16'!
                       classReorganized: aClass

	| packageOrNil |
	packageOrNil _ CodePackage packageOfClass: aClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet classReorganized: aClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/26/2012 18:19'!
         methodAdded: aCompiledMethod selector: aSymbol inClass: aClass requestor: requestor

	| packageOrNil |
	packageOrNil _ CodePackage
		packageOfMethod: aCompiledMethod methodReference ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodAdded: aCompiledMethod
			selector: aSymbol
			inClass: aClass
			requestor: requestor ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/26/2012 18:21'!
           methodAdded: aCompiledMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	| packageOrNil |
	packageOrNil _ CodePackage
		packageOfMethod: aCompiledMethod methodReference ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodAdded: aCompiledMethod
			selector: aSymbol
			inProtocol: aCategoryName
			class: aClass
			requestor: requestor ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/26/2012 18:22'!
                        methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	| packageOrNil |
	packageOrNil _ CodePackage
		packageOfMethod: newMethod methodReference ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodChangedFrom: oldMethod
			to: newMethod
			selector: aSymbol
			inClass: aClass
			requestor: requestor ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/26/2012 18:32'!
                  methodRemoved: aCompiledMethod selector: aSymbol inProtocol: aCategoryName class: aClass

	| packageOrNil |
	packageOrNil _ CodePackage
		packageOfMethodCategory: aCategoryName ofClass: aClass ifNone: nil.
	(self changeSetForPackage: packageOrNil) ifNotNil: [ :changeSet |
		changeSet
			methodRemoved: aCompiledMethod
			selector: aSymbol
			inProtocol: aCategoryName
			class: aClass ].
	packageOrNil ifNotNil: [
		packageOrNil hasUnsavedChanges: true ]! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/26/2012 18:12'!
     changeSetForPackage: aCodePackage

	| csName |
	aCodePackage ifNil: [
		^self changeSetForBaseSystem ].
	csName _ installing
		ifNil: [ 'ChangesTo-', aCodePackage name ]
		ifNotNil: [
			installing = aCodePackage packageName
				ifTrue: [ 'Installing-', installing ]
				ifFalse: [ 'Modified-', aCodePackage name, '--Installing-', installing ]].
	^ChangeSorter existingOrNewChangeSetNamed: csName! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 3/26/2012 18:03'!
              save
	| nameToUse |
	fullFileName ifNil: [
		fullFileName _
			(ChangeSet defaultChangeSetDirectory pathName, FileDirectory slash, 
			self packageName, FileDirectory dot, 'pck')
				asFileName ].
	nameToUse _ fullFileName.
"	nameToUse _ Preferences changeSetVersionNumbers
		ifTrue: [
			ChangeSet defaultChangeSetDirectory
				nextNameFor: self packageName coda: '-', Utilities authorInitials
				extension: 'pck' ]
		ifFalse: [ (self packageName , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'pck') asFileName ]."
	Cursor write
		showWhile: [
			| file |
			file _ ChangeSet defaultChangeSetDirectory newFileNamed: nameToUse.
			[
				file timeStamp.
				self writeOnStream: file ]
					ensure: [ file close ]].
	hasUnsavedChanges _ false! !


!CodePackage class methodsFor: 'instance creation' stamp: 'jmv 3/26/2012 18:06'!
             newNamed: aString
	"Answer a new, unregistered instance"

	^ self new
		packageName: aString! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 3/26/2012 18:05'!
                        install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newCodePackage |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Crear, instalar y devolver una instancia de PackageInfo"
	newCodePackage _ CodePackage newNamed: packageName.
	newCodePackage fullFileName: fullName.
	newCodePackage hasUnsavedChanges: false.
	CodePackage register: newCodePackage.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine ].
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared print.

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/26/2012 18:35'!
                     packageDirtyFlags

	^self packages collect: [ :each |
		each hasUnsavedChanges
			ifTrue: [ '     *']
			ifFalse: [ '' ]]! !

!CodePackageList methodsFor: 'accessing' stamp: 'jmv 3/26/2012 18:00'!
                           packageRepositories

	^self packages collect: [ :each | each fullFileName ]! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 3/26/2012 17:59'!
                               buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names repositories  upperRow description summary buttonRow diffsButton saveButton |
	dirtyFlags _ PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names _ PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	repositories _ PluggableListMorph
		model: model 
		listGetter: #packageRepositories
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.1;
		addMorph: names proportionalWidth: 0.3;
		addMorph: repositories proportionalWidth: 0.6.
	description _ TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:.
	summary _ TextModelMorph
		textProvider: model
		textGetter: #summary.
	saveButton _ PluggableButtonMorph model: self action: #save label: 'Save'.
	diffsButton _ PluggableButtonMorph model: self action: #diffs label: 'Diffs with saved'.
	buttonRow _ LayoutMorph newRow.
"	buttonRow
		addMorph: saveButton layoutSpec: (LayoutSpec
			proportionalWidth: 0.3
			proportionalHeight: 0.6);
		addMorph: diffsButton layoutSpec: (LayoutSpec
			proportionalWidth: 0.3
			proportionalHeight: 0.6)."
	buttonRow
		addMorph: (Morph new color: Color transparent) proportionalWidth: 0.1;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: (Morph new color: Color transparent)proportionalWidth: 0.1;
		addMorph: diffsButton proportionalWidth: 0.6;
		addMorph: (Morph new color: Color transparent) proportionalWidth: 0.1.
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: summary proportionalHeight: 0.1;
		addAdjusterAndMorph: description proportionalHeight: 0.3;
		addAdjusterAndMorph: buttonRow proportionalHeight: 0.1.
	self setLabel: 'Installed Packages'! !

!classDefinition: #CodePackage category: #'Package Support'!
Object subclass: #CodePackage
	instanceVariableNames: 'packageName methodCategoryPrefix fullFileName hasUnsavedChanges'
	classVariableNames: 'InstalledPackages'
	poolDictionaries: ''
	category: 'Package Support'!

!CodePackage reorganize!
('comparing' = hash)
('enumerating' actualMethodsDo: allOverriddenMethodsDo: methodsInCategory:ofClass:do: overriddenMethodsDo: overriddenMethodsInClass:do: overrideCategoriesForClass:do: sortedMethods)
('modifying' addCoreMethod: addExtensionMethod: addMethod: baseCategoryOfMethod: externalBehaviors externalTraits removeMethod:)
('listing' allOverriddenMethods allOverridenMethods classes classesAndMetaClasses coreMethods extensionClasses extensionMethods foreignClasses foreignSystemCategories methods overriddenMethods overriddenMethodsInClass: overrideMethods selectors systemCategories)
('testing' category:matches: changeRecordForOverriddenMethod: coreCategoriesForClass: coreMethodsForClass: extensionCategoriesForClass: extensionMethodsForClass: extensionMethodsFromClasses: foreignExtensionCategoriesForClass: foreignExtensionMethodsForClass: includesChangeRecord: includesClass: includesClassNamed: includesMethod:ofClass: includesMethodCategory:ofClass: includesMethodCategory:ofClassNamed: includesMethodReference: includesSystemCategory: isForeignClassExtension: isOverrideCategory: isOverrideMethod: isOverrideOfYourMethod: isYourClassExtension: methodsInCategory:ofClass: outsideClasses overrideCategoriesForClass: referenceForMethod:ofClass:)
('naming' methodCategoryPrefix name packageName packageName: systemCategoryPrefix)
('dependencies' externalCallers externalClasses externalRefsSelect:thenCollect: externalSubclasses externalUsers)
('source code management' linesOfCode)
('printing' asStringOrText printOn:)
('registering' register)
('saving' save writeClassCommentsOn: writeClassDefinitionsOn: writeInitializersOn: writeMethodsOn: writeOnStream: writeSystemCategoriesOn:)
('accessing' fullFileName fullFileName: hasUnsavedChanges hasUnsavedChanges:)
!

!methodRemoval: ChangeSet class #changeSetForClass:!
ChangeSet class removeSelector: #changeSetForClass:!
!methodRemoval: ChangeSet class #changeSetForMethod:!
ChangeSet class removeSelector: #changeSetForMethod:!
!methodRemoval: ChangeSet class #changeSetForMethodCategory:ofClass:!
ChangeSet class removeSelector: #changeSetForMethodCategory:ofClass:!
!methodRemoval: ChangeSet class #changeSetForSystemCategory:!
ChangeSet class removeSelector: #changeSetForSystemCategory:!
