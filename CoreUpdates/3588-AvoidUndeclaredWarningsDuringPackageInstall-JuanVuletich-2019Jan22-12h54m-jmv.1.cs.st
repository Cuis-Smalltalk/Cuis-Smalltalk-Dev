'From Cuis 5.0 of 7 November 2016 [latest update: #3587] on 22 January 2019 at 12:56:51 pm'!

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 1/22/2019 12:29:29'!
fileIn
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler."

	self fileInAnnouncing: 'Reading ' , self name.
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 1/22/2019 12:25:39'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	Utilities logsUserChanges: false.
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :barBlock | 
			[ self atEnd ] whileFalse: [
					barBlock value: self position.
					self skipSeparators.
					
					[
						val := (self peekFor: $!!) 
								ifTrue: [
									chunk := self nextChunk.
									"These are the ones that should do nothing, 
									because next line is a doit that does the stuff
									(or because it is handled elsewhere)"
									(chunk beginsWith: 'description:  ')
									| (chunk beginsWith: 'provides: ')
									| (chunk beginsWith: 'requires: ')
									| (chunk beginsWith: 'classDefinition: ')
									| (chunk beginsWith: 'classRemoval: ')
									| (chunk beginsWith: 'methodRemoval: ')
									| (chunk beginsWith: 'classMoveToSomePackage: ')
									| (chunk beginsWith: 'methodMoveToSomePackage: ')
										ifFalse: [(Compiler evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									[ Compiler evaluate: chunk logged: true ]
										on: Error
										do: [ :ex |
											ex print.
											('while evaluating: ', chunk) print.
											ex pass ]
										]] 
							on: InMidstOfFileinNotification, UndeclaredVariableWarning
							do: [ :ex | ex resume: true ] ].
			].
	"Note:  The main purpose of this banner is to flush the changes file."
	Utilities logsUserChanges: true.
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !


!FileStream methodsFor: 'fileIn/Out' stamp: 'jmv 1/22/2019 12:27:38'!
fileIn
	"Guarantee that the receiver is readOnly before fileIn for efficiency and
	to eliminate remote sharing conflicts."

	self readOnly.
	self fileInAnnouncing: 'Loading ', self localName.
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].
! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 1/22/2019 12:56:06'!
install: aFileStream
	"Installs the package. Replace all existing code in the CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass currentCS |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	classesToRemove notEmpty ifTrue: [
		('classesToRemove: ', classesToRemove printString) print.
		'=============' print ].
	methodsToRemove notEmpty ifTrue: [
		'methodsToRemove: ' print.
		methodsToRemove do: [ :methodReference | methodReference print ].
		'=============' print ].

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
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

	^newCodePackage! !

