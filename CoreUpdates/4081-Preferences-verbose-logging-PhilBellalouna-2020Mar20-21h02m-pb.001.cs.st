'From Cuis 5.0 [latest update: #4069] on 20 March 2020 at 9:39:27 pm'!

!Preferences class methodsFor: 'standard queries' stamp: 'pb 3/20/2020 21:02:28'!
transcriptLogVerbose
	^ self
		valueOfFlag: #transcriptLogVerbose
		ifAbsent: [ true ].! !


!String methodsFor: 'displaying' stamp: 'pb 3/20/2020 21:04:16'!
displayProgressAt: aPoint from: minVal to: maxVal during: workBlock
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:barBlock |
		1 to: 100 do: [:x | 
			(Delay forMilliseconds: 10) wait.
			barBlock value: x.]].

['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:barBlock |
	1 to: 100 do: [:x | barBlock value: x.
			(Delay forMilliseconds: 100) wait]].] fork
	
['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:barBlock |
	1 to: 30 do: [:x | barBlock value: x \\ 11.
			(Delay forMilliseconds: 100) wait]]] fork

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:barBlock |
	1 to: 30 do: [:x | barBlock value: nil.
			(Delay forMilliseconds: 200) wait]].

HOW IT WORKS (Try this in any other language :-)
Since your code (the last 2 lines in the above example) is in a block,
this method gets control to display its heading before, and clean up 
the screen after, its execution.
The key, though, is that the block is supplied with an argument,
named 'bar' in the example, which will update the bar image every 
it is sent the message value: x, where x is in the from:to: range.

The use of ProgressInitiationException allows for avoiding actual
progress display, by catching the exception.
"
	| answer duration |
	Preferences transcriptLogVerbose ifTrue: [
		Transcript show: self; show: '...'].
	duration _ [
		answer _ ProgressInitiationException 
			display: self
			at: aPoint 
			from: minVal 
			to: maxVal 
			during: workBlock] durationToRun.
	Preferences transcriptLogVerbose ifTrue: [
		Transcript show: ' done. Took '; show: duration printString; newLine].
	^answer! !


!ChangeSet class methodsFor: 'services' stamp: 'pb 3/20/2020 21:18:53'!
install: aFileEntry
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	ChangeSet installing: aFileEntry name do: [ self fileIn: aFileEntry ].
	Preferences transcriptLogVerbose ifTrue: [
		('Installed ChangeSet: ', aFileEntry name) print]! !


!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'pb 3/20/2020 21:05:57'!
fileIn
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 4 ifTrue: [^nil].
	doitsMark = 2 ifTrue:[self fileInDoits].
	classOrder do:[:cls|
		cls fileInDefinition.
	].
	classes do:[:cls|
		Preferences transcriptLogVerbose ifTrue: [
			Transcript newLine; show:'Filing in ', cls name].
		cls fileInMethods.
		cls hasMetaclass ifTrue:[cls theMetaClass fileInMethods].
	].
	doitsMark = 3 ifTrue: [ self fileInDoits ]! !


!CodePackageFile methodsFor: 'services' stamp: 'pb 3/20/2020 21:29:40'!
install: aFileStream
	"Installs the package. Replace all existing code in the CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass currentCS |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	Preferences transcriptLogVerbose ifTrue: [
		'=============' print.
		classesToRemove notEmpty ifTrue: [
			('classesToRemove: ', classesToRemove printString) print.
			'=============' print ].
		methodsToRemove notEmpty ifTrue: [
			'methodsToRemove: ' print.
			methodsToRemove do: [ :methodReference | methodReference print ].
			'=============' print ]].

	"Create, install and answer a (sub)instance of CodePackage"
	localName _ fullName asFileEntry name.
	ChangeSet installing: packageName do: [
		"This change set will capture a possible class definition for a subclass of CodePackage.
		If it only has that, then remove it after package install.
		One example needing this is 'Color-Extras.pck.st'"
		currentCS _ ChangeSet changeSetForBaseSystem.
		currentCS isEmpty ifFalse: [ currentCS _ nil ].
		pckClass _ CodePackage.
		classes do: [ :ee |
			(ee hasDefinition and: [ee superclassName = 'CodePackage']) ifTrue: [
				ee fileInDefinitionAndMetaclass.
				pckClass _ Smalltalk at: ee name ]].
		newCodePackage _ pckClass
			named: packageName
			createIfAbsent: true
			registerIfNew: true.
		newCodePackage
			fullFileName: fullName;
			sourceSystem: sourceSystem;
			description: description;
			featureSpec: featureSpec.
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		currentCS ifNotNil: [ ChangeSet removeChangeSet: currentCS ]].
	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	DataStream initialize. "Just in case"
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	Preferences transcriptLogVerbose ifTrue: [
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
		Smalltalk cleanOutUndeclared.
		Undeclared notEmpty ifTrue: [
			('Undeclared: ', Undeclared printString) print ]].
	^newCodePackage! !


!TestResult methodsFor: 'logging' stamp: 'pb 3/20/2020 21:36:27'!
reportAboutToRun: aTestCase
	Preferences transcriptLogVerbose ifTrue: [
		Transcript show: 'Will run: '; print: aTestCase; newLine]! !

!TestResult methodsFor: 'logging' stamp: 'pb 3/20/2020 21:35:31'!
reportFailed: aTestCase because: anException
	Preferences transcriptLogVerbose ifTrue: [
		Transcript print: anException; newLine].! !

!TestResult methodsFor: 'logging' stamp: 'pb 3/20/2020 21:35:55'!
reportPassed: aTestCase
	Preferences transcriptLogVerbose ifTrue: [
		Transcript show: 'finished.'; newLine]! !

