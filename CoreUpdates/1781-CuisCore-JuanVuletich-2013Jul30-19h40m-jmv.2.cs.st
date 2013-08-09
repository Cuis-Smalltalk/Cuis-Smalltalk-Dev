'From Cuis 4.2 of 25 July 2013 [latest update: #1780] on 30 July 2013 at 8:38:01.032813 pm'!

!CodePackage methodsFor: 'naming' stamp: 'jmv 7/30/2013 19:50'!
packageFileName
	^ packageName, FileDirectory dot, 'pck.st'! !

!CodePackage methodsFor: 'printing' stamp: 'jmv 7/30/2013 20:27'!
printNameAndVersionOn: aStream
	aStream
		nextPutAll: packageName;
		nextPut: $ .
	featureSpec ifNotNil: [
		featureSpec provides printVersionRevisionOn: aStream ]! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 7/30/2013 20:30'!
packageNameAndVersion
	^String streamContents: [ :strm |
		self printNameAndVersionOn: strm ]! !


!Feature methodsFor: 'printing' stamp: 'jmv 7/30/2013 20:19'!
printVersionRevisionOn: aStream
	version printOn: aStream.
	aStream nextPut: $..
	revision printOn: aStream! !


!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 7/30/2013 20:14'!
packageFileName
	"Answer a package name based on feature name"

	^ name asString, FileDirectory dot, 'pck.st'! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 7/30/2013 20:30'!
packageNames

	^ packages collect: [ :each | each packageNameAndVersion ]! !

!CodePackageList methodsFor: 'events' stamp: 'jmv 7/30/2013 20:37'!
updateDirtyFlags
	self
		changed: #packageDirtyFlags;
		changed: #packageNames;		"if it was set dirty, revision number changed"
		changed: #packageFullNames	"if it was never saved and now it is, fileName changes"! !


!CodePackage methodsFor: 'naming' stamp: 'jmv 7/30/2013 20:08'!
methodCategoryPrefix
	^ methodCategoryPrefix ifNil: [
		methodCategoryPrefix _ '*', packageName asLowercase ]! !

!CodePackage methodsFor: 'naming' stamp: 'jmv 7/30/2013 20:11'!
systemCategoryPrefix
	^ packageName! !

!CodePackage methodsFor: 'printing' stamp: 'jmv 7/30/2013 20:28'!
printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	self printNameAndVersionOn: aStream.
	aStream nextPut: $)! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 7/30/2013 19:51'!
save
	| file |
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		(FileDirectory on: (FileDirectory dirPathFor: fullFileName)) exists ifFalse: [
			fullFileName _ nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to default directory."
	fullFileName ifNil: [
		fullFileName _
			ChangeSet defaultChangeSetDirectory fullNameFor: self packageFileName].
		
	file _ FileStream forceNewFileNamed: fullFileName.
	[
		file timeStamp.
		self writeOnStream: file ]
			ensure: [ file close ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 7/30/2013 20:35'!
hasUnsavedChanges: aBoolean

	hasUnsavedChanges = aBoolean ifFalse: [
		hasUnsavedChanges _ aBoolean.
		hasUnsavedChanges ifTrue: [
			featureSpec provides newRevision ].
		self triggerEvent: #dirtyFlagChanged ]! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 3/16/2012 11:14'!
packageName
	^ packageName! !


!Feature methodsFor: 'printing' stamp: 'jmv 7/30/2013 20:19'!
printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: name;
		nextPut: $ .
	self printVersionRevisionOn: aStream..
	aStream
		nextPut: $)! !

!Feature methodsFor: 'incrementing' stamp: 'jmv 7/30/2013 20:35'!
newRevision
	"Increment revision number"

	revision _ revision + 1! !

!Feature methodsFor: 'incrementing' stamp: 'jmv 7/30/2013 20:35'!
newVersion
	"Increment major version number."

	version _ version + 1.
	revision _ 0! !


!Feature class methodsFor: 'instance creation' stamp: 'jmv 7/30/2013 20:18'!
name: aSymbol version: integerVersion revision: integerRevision

	| newInst |
	newInst := self new.
	newInst name: aSymbol asSymbol version: integerVersion revision: integerRevision.
	
	^ newInst! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/30/2013 20:13'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName |
	pathName ifNotNil: [
		(FileDirectory default fileExists: pathName) ifTrue: [ ^ true ]].
	packageFileName _ self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :fileDirectory |
		(fileDirectory fileExists: packageFileName) ifTrue: [
			pathName _ fileDirectory fullNameFor: packageFileName.
			^ true ]].
	^ false! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 7/30/2013 19:40'!
install
	"Preconditions have been satisfied.  Install the required package."

	CodePackageFile basicInstallPackageStream: ( FileStream readOnlyFileNamed: pathName).

	"No need to have a platform and machine specific path around anymore. It was just for installation. Clear it."
	pathName _ nil! !

!methodRemoval: FeatureRequirement #packageName!
FeatureRequirement removeSelector: #packageName!
!methodRemoval: CodePackage #asStringOrText!
CodePackage removeSelector: #asStringOrText!
!methodRemoval: CodePackage #name!
CodePackage removeSelector: #name!
!methodRemoval: CodePackage #popoinst!
CodePackage removeSelector: #popoinst!

!CodePackage reorganize!
('comparing' = hash)
('enumerating' actualMethodsDo: allOverriddenMethodsDo: methodsInCategory:ofClass:do: overriddenMethodsDo: overriddenMethodsInClass:do: overrideCategoriesForClass:do:)
('modifying' addCoreMethod: addExtensionMethod: addMethod: baseCategoryOfMethod: removeMethod:)
('listing' allOverriddenMethods classesDo: classesInSafeOrderDo: coreMethods extensionClassNamesIn: extensionClasses extensionMethods foreignClasses foreignSystemCategories methods overriddenMethods overriddenMethodsInClass: overrideMethods selectors sortedExtensionMethodsDo:displayingProgress: systemCategories systemCategoriesWithExtensionMethods)
('testing' category:matches: changeRecordForOverriddenMethod: coreCategoriesForClass: coreMethodsForClass: extensionCategoriesForClass: foreignExtensionCategoriesForClass: foreignExtensionMethodsForClass: hasAnyExtensionCategoriesForClass: includesAnyCode includesChangeRecord: includesClass: includesClassNamed: includesMethod:ofClass: includesMethodCategory:ofClass: includesMethodCategory:ofClassNamed: includesMethodReference: includesSystemCategory: isForeignClassExtension: isOverrideCategory: isOverrideMethod: isOverrideOfYourMethod: isYourClassExtension: overrideCategoriesForClass: referenceForMethod:ofClass:)
('naming' methodCategoryPrefix packageFileName packageName: systemCategoryPrefix)
('dependencies' externalCallers externalClasses externalRefsSelect:thenCollect: externalSubclasses externalUsers)
('source code management' linesOfCode)
('printing' printNameAndVersionOn: printOn:)
('saving' save write:classCommentsOn: write:classDefinitionsOn: write:initializersOn: write:methodsOn: writeFeatureSpecOn: writeMethod:on: writeMethodsOf:on: writeOnStream: writeSystemCategoriesOn:)
('accessing' classNames description description: featureSpec: fullFileName fullFileName: hasUnsavedChanges hasUnsavedChanges: packageName packageNameAndVersion provides sourceSystem sourceSystem:)
!

