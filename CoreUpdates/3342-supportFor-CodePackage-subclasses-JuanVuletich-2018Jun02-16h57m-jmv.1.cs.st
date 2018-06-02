'From Cuis 5.0 of 7 November 2016 [latest update: #3341] on 2 June 2018 at 5:00:22 pm'!

!CodePackage methodsFor: 'accessing' stamp: 'jmv 6/2/2018 16:58:49'!
codePackageClass
	"Answer the specific CodePackage subclass to use."

	self class == CodePackage ifFalse: [
		^ self class ].
	self classesDo: [ :cls |
		(cls inheritsFrom: CodePackage)
			ifTrue: [ ^ cls ]].
	^ nil! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 6/2/2018 16:59:03'!
writeOnStream: aStream

	| sortedClasses |
	aStream
		nextChunkPut: ('Description ', description) printString;
		newLine.
	self writeFeatureSpecOn: aStream.
	self writeSystemCategoriesOn: aStream.

	self codePackageClass ifNotNil: [ :codePackageClass |
		self
			write: {codePackageClass} classDefinitionsOn: aStream;
			write: {codePackageClass} classCommentsOn: aStream;
			write: {codePackageClass} methodsOn: aStream.
		aStream nextChunkPut: codePackageClass name, ' prePackageInstall'; newLine ].

	sortedClasses _ Array streamContents: [ :strm |
		self classesInSafeOrderDo: [ :cls |
			cls == self class ifFalse: [
				strm nextPut: cls ]]].
	self
		write: sortedClasses classDefinitionsOn: aStream;
		write: sortedClasses classCommentsOn: aStream;
		write: sortedClasses methodsOn: aStream.

	self
		sortedExtensionMethodsDo:  [ :methodReference |
			methodReference isValid ifTrue: [
				self writeMethod: methodReference on: aStream ]]
		displayingProgress: 'Saving extension methods...'.
	self write: sortedClasses initializersOn: aStream.

	self codePackageClass ifNotNil: [ :codePackageClass |
		self write: { codePackageClass } initializersOn: aStream.
		aStream nextChunkPut: codePackageClass name, ' postPackageInstall'; newLine ]! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 6/2/2018 16:59:49'!
install: aFileStream
	"Installs the package. Replace all existing code in the CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass |

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

	localName _ fullName asFileEntry name.
	ChangeSet installing: packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		"Create, install and answer an instance of CodePackage"
		pckClass _ CodePackage.
		classes do: [ :ee | (ee hasDefinition and: [ee superclassName = 'CodePackage']) ifTrue: [ 
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
	].
	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	DataStream initialize. "Just in case"
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

!methodRemoval: CodePackage #writeInitializerExtensionMethods:on:!
CodePackage removeSelector: #writeInitializerExtensionMethods:on:!
