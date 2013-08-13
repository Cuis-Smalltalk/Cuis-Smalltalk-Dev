'From Cuis 4.2 of 25 July 2013 [latest update: #1792] on 12 August 2013 at 6:01:58.408152 pm'!

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 8/12/2013 18:01'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse file |
	self checkForConversionMethods.
	nameToUse := Preferences changeSetVersionNumbers
				ifTrue: [self defaultChangeSetDirectory nextNameFor: self name coda: '-', Utilities authorInitials extension: 'cs.st']
				ifFalse: [(self name , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'cs.st') asFileName].
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asCamelCase. 
	file := self defaultChangeSetDirectory newFileNamed: nameToUse.
	[
		file timeStamp.
		self fileOutPreambleOn: file.
		self fileOutOn: file.
		self fileOutPostscriptOn: file]
			ensure: [ file close ].
	
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


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 8/12/2013 17:59'!
baseSystemNameFor: aChangeSetNumber

	^String streamContents: [ :strm |
	strm
		nextPutAll: (aChangeSetNumber asString padded: #left to: 4 with: $0);
		nextPutAll: '-CuisCore-';
"		nextPutAll: Utilities authorName asCamelCase;"
		nextPutAll: 'AuthorName';
"		nextPutAll: '-';
		nextPutAll: Utilities authorInitials asCamelCase;"
		nextPutAll: '-' ]! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 8/12/2013 16:09'!
install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newCodePackage |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	classesToRemove notEmpty ifTrue: [
		('classesToRemove: ', classesToRemove printString) print.
		'=============' print ].
	methodsToRemove notEmpty ifTrue: [
		'methodsToRemove: ' print.
		methodsToRemove do: [ :methodReference | methodReference print ].
		'=============' print ].
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Create, install and answer an instance of CodePackage"
	newCodePackage _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: true.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		newCodePackage
			fullFileName: fullName;
			sourceSystem: sourceSystem;
			description: description;
			featureSpec: featureSpec.
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !

