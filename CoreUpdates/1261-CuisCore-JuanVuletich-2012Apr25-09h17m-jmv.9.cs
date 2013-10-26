'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 25 April 2012 at 10:29:12 am'!
!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses isForBaseSystem hasUnsavedChanges '
	classVariableNames: 'CurrentBaseCSNumber Installing '
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeSet methodsFor: 'accessing' stamp: 'jmv 4/25/2012 09:46'!
hasUnsavedChanges

	^hasUnsavedChanges! !

!ChangeSet methodsFor: 'accessing' stamp: 'jmv 4/25/2012 09:46'!
hasUnsavedChanges: aBoolean

	hasUnsavedChanges _ aBoolean.
	self triggerEvent: #dirtyFlagChanged! !

!ChangeSet methodsFor: 'accessing' stamp: 'jmv 4/25/2012 10:18'!
isForBaseSystem: aBoolean

	isForBaseSystem _ aBoolean! !


!ChangeSorter class methodsFor: 'enumerating' stamp: 'jmv 4/25/2012 10:24'!
existingOrNewChangeSetNamed: aName forBaseSystem: aBoolean

	| newSet |

	^(self changeSetNamed: aName)
		ifNotNil: [ :existing |
			existing assert: existing isForBaseSystem = aBoolean ]
		ifNil: [
			newSet _ ChangeSet basicNewNamed: aName.
			newSet isForBaseSystem: aBoolean.
			AllChangeSets add: newSet.
			newSet ]! !


!CodePackage class methodsFor: 'packages access' stamp: 'jmv 4/25/2012 09:34'!
named: aString createIfAbsent: neverAnswerNil registerIfNew: doRegisterIfJustCreated
	"Answer the instance with name aString.
	If there's no package named aString, answer nil or a new instance as requested.
	If a new instance is created, register it or not as requested.
	CodePackage named: 'Morphic' createIfAbsent: false registerIfNew: false
	CodePackage named: 'Morphic' createIfAbsent: true registerIfNew: false
	CodePackage named: 'XXXXXTest' createIfAbsent: true registerIfNew: true
	"

	| answer newInstance |
	answer _ InstalledPackages
		at: aString
		ifAbsent: [
			neverAnswerNil ifTrue: [ 
				newInstance _ self new packageName: aString.
				doRegisterIfJustCreated ifTrue: [ self register: newInstance ].
				newInstance ]].
	^ answer! !


!ChangeSet methodsFor: 'initialize-release' stamp: 'jmv 4/25/2012 10:24'!
clear 
	"Reset the receiver to be empty.  "

	changeRecords _ Dictionary new.
	preamble _ nil.
	postscript _ nil.
	self hasUnsavedChanges: false.
	self isForBaseSystem: true 	"Not a great default, but at least some Boolean"! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 4/25/2012 09:50'!
noteAddClass: class
	"Include indication that a new class was created."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #new.
	self atClass: class add: #change.
	self addCoherency: class name.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 4/25/2012 09:52'!
noteChangeClass: class from: oldClass
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
	(self changeRecorderFor: class) notePriorDefinition: oldClass.
	self noteClassStructure: oldClass.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 4/25/2012 09:56'!
noteClassMoveToOtherPackage: class
	"The class is about to be moved to some other package, who will hold it.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #movedToOtherPackage.
	changeRecords removeKey: class class name ifAbsent: nil.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 4/25/2012 09:56'!
noteMethodMoveToOtherPackage: selector forClass: class

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteMethodMoveToOtherPackage: selector.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 4/25/2012 09:54'!
noteNewMethod: newMethod forClass: class selector: selector priorMethod: methodOrNil

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteNewMethod: newMethod selector: selector priorMethod: methodOrNil.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 4/25/2012 10:10'!
noteRemoveSelector: selector class: class priorMethod: priorMethod lastMethodInfo: info
	"Include indication that a method has been forgotten.
	info is a pair of the source code pointer and message category
	for the method that was removed."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteRemoveSelector: selector priorMethod: priorMethod lastMethodInfo: info.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 4/25/2012 09:54'!
noteRenameClass: class as: newName 
	"Include indication that a class has been renamed."

	| recorder |
	(recorder _ self changeRecorderFor: class)
		noteChangeType: #rename;
		noteNewName: newName asSymbol.
		
	"store under new name (metaclass too)"
	changeRecords at: newName put: recorder.
	changeRecords removeKey: class name.
	self noteClassStructure: class.

	recorder _ changeRecords at: class class name ifAbsent: [nil].
	recorder ifNotNil: [
		changeRecords at: (newName, ' class') put: recorder.
		changeRecords removeKey: class class name.
		recorder noteNewName: newName , ' class' ].

	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'accessing' stamp: 'jmv 4/25/2012 10:18'!
isForBaseSystem

	^isForBaseSystem! !

!ChangeSet methodsFor: 'method changes' stamp: 'jmv 4/25/2012 10:07'!
removeSelectorChanges: selector class: class 
	"Remove all memory of changes associated with the argument, selector, in 
	this class."

	| chgRecord |
	(chgRecord _ changeRecords at: class name ifAbsent: [^ self])
		removeSelector: selector.
	chgRecord hasNoChanges ifTrue: [changeRecords removeKey: class name].
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'class changes' stamp: 'jmv 4/25/2012 09:56'!
noteClassForgotten: className
	"Remove from structures if class is not a superclass of some other one we are remembering"

	structures ifNil: [^ self].
	Smalltalk at: className ifPresent: [:cls |
		cls subclasses do: [:sub | (structures includesKey: sub) ifTrue: [
			^ self]]].  "No delete"
	structures removeKey: className ifAbsent: nil.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'class changes' stamp: 'jmv 4/25/2012 09:51'!
noteCommentClass: class 
	"Include indication that a class comment has been changed."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #comment.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'class changes' stamp: 'jmv 4/25/2012 09:52'!
noteRemovalOf: class
	"The class is about to be removed from the system.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteChangeType: #remove fromClass: class.
	changeRecords removeKey: class class name ifAbsent: nil.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'class changes' stamp: 'jmv 4/25/2012 09:54'!
noteReorganizeClass: class 
	"Include indication that a class was reorganized."

	self atClass: class add: #reorganize.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 4/25/2012 10:01'!
assurePreambleExists
	"Make sure there is a TextModel holding the preamble; if it's found to have reverted to empty contents, put up the template"

	(preamble == nil or: [preamble actualContents isEmptyOrNil])
		ifTrue: [
			preamble _ TextModel withText: self preambleTemplate.	
			self hasUnsavedChanges: true ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 4/25/2012 10:02'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse |
	self checkForConversionMethods.
	nameToUse := Preferences changeSetVersionNumbers
				ifTrue: [self defaultChangeSetDirectory nextNameFor: self name coda: '-', Utilities authorInitials extension: 'cs']
				ifFalse: [(self name , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'cs') asFileName].
	Cursor write
		showWhile: [
			| file |
			file := self defaultChangeSetDirectory newFileNamed: nameToUse.
			[
				file timeStamp.
				self fileOutPreambleOn: file.
				self fileOutOn: file.
				self fileOutPostscriptOn: file]
					ensure: [file close]].
	
	self hasUnsavedChanges: false.
	Preferences checkForSlips
		ifFalse: [^ self].
	slips := self checkForSlips.
	(slips size > 0
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [Smalltalk browseMessageList: slips name: 'Possible slips in ' , name]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 4/25/2012 10:25'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	"try to write reference for me"
	^ DiskProxy 
		global: #ChangeSorter
		selector: #existingOrNewChangeSetNamed:forBaseSystem:
		args: (Array with: self name with: self isForBaseSystem)
"===
	refStrm replace: self with: nil.
	^ nil
==="
! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 4/25/2012 09:58'!
postscriptString: aString

	postscript _ TextModel withText: aString.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 4/25/2012 09:58'!
preambleString: aString
	"Establish aString as the new contents of the preamble.  "

	preamble _ TextModel withText: aString.
	self hasUnsavedChanges: true! !

!ChangeSet methodsFor: 'system change notifications' stamp: 'jmv 4/25/2012 09:52'!
classRecategorized: aClass from: oldCategory to: newCategory

	"
	self hasUnsavedChanges: true
	"! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 4/25/2012 09:41'!
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
			file _ ChangeSet defaultChangeSetDirectory forceNewFileNamed: nameToUse.
			[
				file timeStamp.
				self writeOnStream: file ]
					ensure: [ file close ]].
	self hasUnsavedChanges: false.
	ChangeSorter removeChangeSet: (ChangeSet changeSetForPackage: self)! !


!CodePackage class methodsFor: 'packages access' stamp: 'jmv 4/25/2012 09:40'!
register: aCodePackage
	"
	Usually call #named:createIfAbsent:registerIfNew: instead
	CodePackage register: (CodePackage newNamed: 'Tests-Files')
	"
	InstalledPackages at: aCodePackage packageName put: aCodePackage.
	self triggerEvent: #installedPackagesChanged! !


!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 4/25/2012 09:35'!
buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	"Just build the PackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName _pkName.
	fullName _fullFileName.
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


!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 4/25/2012 09:36'!
createPackage

	| pkName |
	pkName_ FillInTheBlank request: 'Name for new package?'.
	CodePackage
		named: pkName
		createIfAbsent: true
		registerIfNew: true! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/25/2012 10:11'!
okayToDiscardUnsavedCodeSaving: wouldSave
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCodeSaving: true
	Smalltalk okayToDiscardUnsavedCodeSaving: false
	"

	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSorter allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any isEmpty hasUnsavedChanges ]].
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

!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses hasUnsavedChanges isForBaseSystem'
	classVariableNames: 'CurrentBaseCSNumber Installing'
	poolDictionaries: ''
	category: 'Tools-Changes'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| name |
ChangeSet allInstancesDo: [ :each |
	each hasUnsavedChanges: each isEmpty not.
	name _ each name.
	each isForBaseSystem: ((name at: 1) isDigit
		and: [ (name at: 1) isDigit
			and: [ (name at: 1) isDigit
				and: [ (name at: 1) isDigit
					and: [ name is: '-CuisCore-' substingAt: 5 ]]]])]!

