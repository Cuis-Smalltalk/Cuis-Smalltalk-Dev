'From Cuis 4.1 of 12 December 2012 [latest update: #1751] on 14 July 2013 at 6:36:23.831326 pm'!
!classDefinition: #CodePackage category: #'Package Support'!
Object subclass: #CodePackage
	instanceVariableNames: 'packageName methodCategoryPrefix fullFileName hasUnsavedChanges description sourceSystem featureSpec '
	classVariableNames: 'InstalledPackages '
	poolDictionaries: ''
	category: 'Package Support'!

!CodePackage methodsFor: 'saving' stamp: 'jmv 7/14/2013 17:02'!
writeFeatureSpecOn: aStream
	| provides |
	provides _ featureSpec provides.
	aStream
		nextPut: $!!; 
		nextChunkPut: 'provides: ', provides name asString printString, ' ', provides version printString, ' ', provides revision printString;
		newLine.
	featureSpec requires do: [ :requires |
		aStream
		nextPut: $!!; 
			nextChunkPut: 'provides: ', requires name asString printString, ' ', requires version printString, ' ', requires revision printString;
			newLine ]! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 7/9/2013 18:26'!
featureSpec: aFeatureSpec

	featureSpec _ aFeatureSpec! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 7/9/2013 18:28'!
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
	newCodePackage
		fullFileName: fullName;
		sourceSystem: sourceSystem;
		description: description;
		featureSpec: featureSpec.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
	newCodePackage hasUnsavedChanges: false.
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


!CodePackage methodsFor: 'saving' stamp: 'jmv 7/14/2013 17:03'!
writeOnStream: aStream

	| sortedClasses |
	sortedClasses _ Array streamContents: [ :strm |
		self classesInSafeOrderDo: [ :cls |
			strm nextPut: cls ]].
	aStream
		nextChunkPut: ('Description ', description) printString;
		newLine.
	self writeFeatureSpecOn: aStream.
	self
		write: sortedClasses classDefinitionsOn: aStream;
		write: sortedClasses classCommentsOn: aStream;
		write: sortedClasses methodsOn: aStream;
		write: sortedClasses initializersOn: aStream! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 7/14/2013 17:01'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :bar | 
			[ self atEnd ] whileFalse: [
					bar value: self position.
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
											ex resume: true ]
										]] 
							on: InMidstOfFileinNotification
							do: [ :ex | ex resume: true] ].
			self close ].
	"Note:  The main purpose of this banner is to flush the changes file."
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !

!classDefinition: #CodePackage category: #'Package Support'!
Object subclass: #CodePackage
	instanceVariableNames: 'packageName methodCategoryPrefix fullFileName hasUnsavedChanges description sourceSystem featureSpec'
	classVariableNames: 'InstalledPackages'
	poolDictionaries: ''
	category: 'Package Support'!
