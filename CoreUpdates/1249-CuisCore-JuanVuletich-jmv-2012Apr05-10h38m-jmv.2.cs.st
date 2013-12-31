'From Cuis 4.0 of 3 April 2012 [latest update: #1248] on 5 April 2012 at 10:43:13 am'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 4/5/2012 10:43'!
baseSystemNameFor: aChangeSetNumber

	^String streamContents: [ :strm |
	strm
		nextPutAll: (aChangeSetNumber asString padded: #left to: 4 with: $0);
		nextPutAll: '-CuisCore-';
		nextPutAll: Utilities authorName asCamelCase;
"		nextPutAll: '-';
		nextPutAll: Utilities authorInitials asCamelCase;"
		nextPutAll: '-' ]! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 4/5/2012 10:42'!
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

	"Crear, instalar y devolver una instancia de PackageInfo"
	newCodePackage _ CodePackage newNamed: packageName.
	newCodePackage
		fullFileName: fullName;
		sourceSystem: sourceSystem;
		description: description.
	CodePackage register: newCodePackage.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
	newCodePackage hasUnsavedChanges: false.
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !

