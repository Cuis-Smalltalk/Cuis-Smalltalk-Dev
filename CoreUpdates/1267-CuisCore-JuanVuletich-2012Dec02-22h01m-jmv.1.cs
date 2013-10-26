'From Cuis 4.0 of 21 April 2012 [latest update: #1266] on 2 December 2012 at 10:01:51 pm'!

!BorderedMorph methodsFor: 'initialization' stamp: 'jmv 12/2/2012 21:35'!
initialize
	"initialize the state of the receiver"
	super initialize.
	"initialize the receiver state related to border"
	borderColor _ self defaultBorderColor.
	borderWidth _ self defaultBorderWidth! !


!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 12/2/2012 21:37'!
buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	"Just build the PackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName _ pkName.
	fullName _ fullFileName.
	"Don't register a package!!"
	packageInMemory _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: false.
	self buildFrom: aFileStream.
	"Compute stuff no longer in package: Should be removed from system."
	classesDefined _ Set new.
	classesExtended _ Set new.
	self classes do: [ :pseudoClass |
		pseudoClass hasDefinition
			ifTrue: [ classesDefined add: pseudoClass name ]
			ifFalse: [ classesExtended add: pseudoClass name ]].
	classesToRemove _ (packageInMemory classes asSet collect: [ :each | each name ]) difference: classesDefined.
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


!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 12/2/2012 21:34'!
createPackage

	| pkName |
	pkName _ FillInTheBlank request: 'Name for new package?'.
	CodePackage
		named: pkName
		createIfAbsent: true
		registerIfNew: true! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 12/2/2012 21:36'!
browseFullProtocol
	"Create and schedule a new protocol browser on the currently selected class or meta."

	| aPBrowser label |
	model selectedClassOrMetaClass ifNotNil: [ :classOrMetaclass |
		aPBrowser _ ProtocolBrowser new on: classOrMetaclass.
		label _ 'Entire protocol of: ', classOrMetaclass name.
		MessageSetWindow open: aPBrowser label: label ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 12/2/2012 21:36'!
browseProtocol
	"Create and schedule a new protocol browser on the currently selected class or meta."
	| aPBrowser label |
	model selectedClassOrMetaClass ifNotNil: [ :classOrMetaclass |
		aPBrowser _ ProtocolBrowser new onSubProtocolOf: classOrMetaclass.
		label _'Sub-protocol of: ', classOrMetaclass name.
		MessageSetWindow open: aPBrowser label: label ]! !


!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 12/2/2012 22:01'!
arrowOfDirection: aSymbol size: finalSizeInteger
	^CachedForms
		at: { aSymbol . finalSizeInteger }
		ifAbsentPut: [
			Form extent: finalSizeInteger@finalSizeInteger depth: 32.
			"self buildArrowOfDirection: aSymbol size: finalSizeInteger" ]! !


!HaloSpec methodsFor: 'as yet unclassified' stamp: 'jmv 12/2/2012 21:35'!
horizontalPlacement: hp verticalPlacement: vp color: col iconSymbol: is addHandleSelector: sel
	horizontalPlacement _ hp.
	verticalPlacement _ vp.
	color _ col.
	iconSymbol _ is asSymbol.
	addHandleSelector _ sel! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/2/2012 21:37'!
okayToDiscardUnsavedCodeSaving: wouldSave
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCodeSaving: true
	Smalltalk okayToDiscardUnsavedCodeSaving: false
	"

	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSorter allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges ]].
	dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ].

	baseCSdirty & dirtyPackages ifTrue: [
		wouldSave ifTrue: [
			^self confirm: 'There are both unsaved Packages', String newLineString,
				'      (would need to be saved on next run), ', String newLineString,
				'and unsaved Changes to Cuis core', String newLineString,
				'      (they would be lost as a separate ChangeSet).', String newLineString,
				'Continue?' ]
		ifFalse: [
			^self confirm: 'There are both unsaved Packages', String newLineString,
				'and unsaved Changes to Cuis core.', String newLineString,
				'If you continue, they will all be lost.', String newLineString,
				'Continue?' ]].

	baseCSdirty ifTrue: [
		^self confirm: 'Some ChangeSet for Cuis core might have unsaved changes.', String newLineString,
			'If you continue, they would be lost.', String newLineString,
			'Continue?' ].

	dirtyPackages ifTrue: [
		wouldSave ifTrue: [
			^self confirm: 'There are unsaved Packages.', String newLineString,
				'If you continue, they will need to be saved on next run.', String newLineString,
				'Continue?' ]
		ifFalse: [
			^self confirm: 'There are unsaved Packages.', String newLineString,
				'If you continue, they will all be lost.', String newLineString,
				'Continue?' ]].

	^true! !

