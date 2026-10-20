'From Cuis7.9 [latest update: #8271] on 20 October 2026 at 1:07:09 pm'!

!SystemVersion commentStamp: '<historical>' prior: 0!
SystemVersion current.

Instance variables:
	version. A SemanticVersion: major and minor are the Cuis version, patch is the number of the last update installed, regardless of prefix.
	releaseDate. If nil, not a release.
	updatesPrefix. Valid prefixes for update change sets. nil or '' -> No prefix.
	lastUpdateDate. Date of last update installed.

Note: Cuis itself doesn't strictly follow semantic versioning, as our release process is based on Dates and not on Features.!


!CodePackage methodsFor: 'naming' stamp: 'CAP 18/Oct/2026, 12:16:21 am (UTC)'!
packageName: aString
	packageName := aString.
	description := ''.
	featureSpec := FeatureSpec new.
	featureSpec provides: (Feature name: packageName version: SemanticVersion zero).
	hasUnsavedChanges := self includesAnyCode.! !

!CodePackage methodsFor: 'printing' stamp: 'CAP 20/Oct/2026, 2:36:41 pm (UTC)'!
printNameAndVersionOn: aStream
	aStream
		nextPutAll: packageName;
		nextPut: $ .
	featureSpec ifNotNil: [
		featureSpec provides version printOn: aStream ]! !

!CodePackage methodsFor: 'saving' stamp: 'CAP 17/Oct/2026, 9:57:50 pm (UTC)'!
writeFeatureSpecOn: aStream
	| provides |
	provides := featureSpec provides.
	aStream
		nextPut: $!!; 
		nextChunkPut: 'provides: ', provides name asPlainString printString, ' ', provides version printString printString;
		newLine.
	featureSpec requires do: [ :requires |
		aStream
		nextPut: $!!; 
			nextChunkPut: 'requires: ', requires name asPlainString printString, ' ', requires minVersion printString printString, ' ', requires maxVersion printString printString;
			newLine ]! !

!CodePackage methodsFor: 'accessing' stamp: 'CAP 18/Oct/2026, 12:16:21 am (UTC)'!
hasUnsavedChanges: aBoolean

	"Not during package install"
	(ChangeSet isInstalling: self) ifFalse: [
		hasUnsavedChanges = aBoolean ifFalse: [
			hasUnsavedChanges := aBoolean.
			self triggerEvent: #dirtyFlagChanged ]]! !


!CodePackageList methodsFor: 'commands' stamp: 'CAP 18/Oct/2026, 12:22:21 am (UTC)'!
save

	selection ifNotNil: [
		self
			request: 'Version to save as (major.minor.patch):'
			initialAnswer: selection provides version printString
			verifying: [ :aString |
				[ selection provides version: (SemanticVersion fromString: aString). true ]
					on: Error
					do: [ :error | error messageText ] ]
			do: [ :aString | selection save ] ]! !


!Feature methodsFor: 'printing' stamp: 'CAP 20/Oct/2026, 2:36:02 pm (UTC)'!
printDetailsOn: aStream

	aStream
		nextPutAll: name;
		nextPut: $ .
	version printOn: aStream.! !


!CodePackageFile methodsFor: 'initialization' stamp: 'CAP 16/Oct/2026, 5:28:02 pm (UTC)'!
buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	"Just build the CodePackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName := pkName.
	fullName := fullFileName.
	"This will most likely be updated with the actual data from the file, calling #provides:"
	featureSpec provides: (Feature name: pkName version: SemanticVersion zero).
	"Don't register a package!!"
	packageInMemory := CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: false.
	self buildFrom: aFileStream.
	"Compute stuff no longer in package: Should be removed from system."
	classesDefined := Set new.
	classesExtended := Set new.
	classes do: [ :pseudoClass |
		pseudoClass hasDefinition
			ifTrue: [ classesDefined add: pseudoClass name ]
			ifFalse: [ classesExtended add: pseudoClass name ]].
	classesToRemove := packageInMemory classNames difference: classesDefined.
	"Add here:
		- classes in classesToDelete, that #allCallsOn answers selectors that aren't in classesToDelete or methodsToRemove
		- classes with #subclasses that aren't in classesToDelete.
		- classes with existing instances (#instanceCount)? Not really sure... Maybe sole instance referenced from classVar or such...
		- something else I forgot?
	Warning: This search for stuff that can't be removed must be iterated again until it doesn't find any more."
	classesToDeleteButCant := classesToRemove intersection: classesExtended.
	classesToReallyDelete := classesToRemove difference: classesToDeleteButCant.
	"Methods. Could also mean classes that can't be deleted!! (include in the iteration)
	Warn if deleting last implementor of sent messages?"
	methodsToRemove := packageInMemory methods asSet difference: self allMethodReferences.
	methodsToRemove := methodsToRemove reject: [ :methodReference | classesToReallyDelete includes: methodReference classSymbol ].

"
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	('classesToDeleteButCant: ', classesToDeleteButCant printString) print.
	('classesToReallyDelete: ', classesToReallyDelete printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
"! !

!methodRemoval: Feature #newRevision stamp: 'CAP 20/Oct/2026, 4:06:19 pm (UTC)'!
Feature removeSelector: #newRevision!
!methodRemoval: Feature #printVersionRevisionOn: stamp: 'CAP 20/Oct/2026, 4:04:44 pm (UTC)'!
Feature removeSelector: #printVersionRevisionOn:!
