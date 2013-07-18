'From Cuis 4.1 of 12 December 2012 [latest update: #1751] on 14 July 2013 at 6:34:56.55388 pm'!

!Feature class methodsFor: 'instance creation' stamp: 'jmv 7/14/2013 17:40'!
name: aSymbol
	^self name: aSymbol version: nil revision: nil! !

!Feature class methodsFor: 'instance creation' stamp: 'jmv 7/14/2013 17:40'!
name: aSymbol version: integerVersionOrNil
	^self name: aSymbol version: integerVersionOrNil revision: nil! !


!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 7/14/2013 17:25'!
buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	"Just build the CodePackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName _ pkName.
	fullName _ fullFileName.
	"This will most likely be updated with the actual data from the file, calling #provides:"
	featureSpec provides: (Feature name: pkName version: 1 revision: 0).
	"Don't register a package!!"
	packageInMemory _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: false.
	self buildFrom: aFileStream.
	"Compute stuff no longer in package: Should be removed from system."
	classesDefined _ Set new.
	classesExtended _ Set new.
	classes do: [ :pseudoClass |
		pseudoClass hasDefinition
			ifTrue: [ classesDefined add: pseudoClass name ]
			ifFalse: [ classesExtended add: pseudoClass name ]].
	classesToRemove _ packageInMemory classNames difference: classesDefined.
	"Add here:
		- classes in classesToDelete, that #allCallsOn answers selectors that aren't in classesToDelete or methodsToRemove
		- classes with #subclasses that aren't in classesToDelete.
		- classes with existing instances (#instanceCount)? Not really sure... Maybe sole instance referenced from classVar or such...
		- something else I forgot?
	Warning: This search for stuff that can't be removed must be iterated again until it doesn't find any more."
	classesToDeleteButCant _ classesToRemove intersection: classesExtended.
	classesToReallyDelete _ classesToRemove difference: classesToDeleteButCant.
	"Methods. Could also mean classes that can't be deleted!! (include in the iteration)
	Warn if deleting last implementor of sent messages?"
	methodsToRemove _ packageInMemory methods asSet difference: self allMethodReferences.
	methodsToRemove _ methodsToRemove reject: [ :methodReference | classesToReallyDelete includes: methodReference classSymbol ].

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

