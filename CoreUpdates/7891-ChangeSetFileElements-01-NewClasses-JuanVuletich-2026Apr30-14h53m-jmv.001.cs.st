'From Cuis7.7 [latest update: #7890] on 30 April 2026 at 2:54:58 pm'!
!classDefinition: #ClassCommentChange category: #'Tools-Changes'!
ChangeListElement subclass: #ClassCommentChange
	instanceVariableNames: 'file position class stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ClassCommentChange commentStamp: 'jmv 30/Apr/2026 11:44:29' prior: 0!
A ClassCommentChange represents a change recorded on a file in fileOut format.
It includes a type (more needs to be done here), and additional information
for certain types such as method defs which need class and category.!

!classDefinition: #ClassDefinitionChange category: #'Tools-Changes'!
ChangeListElement subclass: #ClassDefinitionChange
	instanceVariableNames: 'file position class category meta stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ClassDefinitionChange commentStamp: 'jmv 30/Apr/2026 11:44:39' prior: 0!
A ClassDefinitionChange represents a change recorded on a file in fileOut format.
It includes a type (more needs to be done here), and additional information
for certain types such as method defs which need class and category.!

!classDefinition: #ClassDeletionChange category: #'Tools-Changes'!
ChangeListElement subclass: #ClassDeletionChange
	instanceVariableNames: 'className doItOnlyIfInBaseSystem classDefinition stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ClassDeletionChange commentStamp: '<historical>' prior: 0!
Used to represent classes that will be deleted on install.!

!classDefinition: #ClassRenamedChange category: #'Tools-Changes'!
ChangeListElement subclass: #ClassRenamedChange
	instanceVariableNames: 'previousName newName stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #FeatureProvidesRecord category: #'Tools-Changes'!
ChangeListElement subclass: #FeatureProvidesRecord
	instanceVariableNames: 'feature'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #FeatureRequiresRecord category: #'Tools-Changes'!
ChangeListElement subclass: #FeatureRequiresRecord
	instanceVariableNames: 'feature'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #MethodDefinitionChange category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDefinitionChange
	instanceVariableNames: 'file position class category meta stamp prior libraryName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!MethodDefinitionChange commentStamp: 'jmv 30/Apr/2026 11:44:44' prior: 0!
A MethodDefinitionChange represents a change recorded on a file in fileOut format.
It includes a type (more needs to be done here), and additional information
for certain types such as method defs which need class and category.!

!classDefinition: #MethodDeletionChange category: #'Tools-Changes'!
ChangeListElement subclass: #MethodDeletionChange
	instanceVariableNames: 'methodReference doItOnlyIfInBaseSystem sourceCode stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!MethodDeletionChange commentStamp: 'jmv 7/4/2013 22:29' prior: 0!
Used to represent methods that will be deleted on install.!

!classDefinition: #PreambleRecord category: #'Tools-Changes'!
ChangeListElement subclass: #PreambleRecord
	instanceVariableNames: 'file position stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!PreambleRecord commentStamp: 'jmv 30/Apr/2026 11:44:50' prior: 0!
A PreambleRecord represents a change recorded on a file in fileOut format.
It includes a type (more needs to be done here), and additional information
for certain types such as method defs which need class and category.!

!classDefinition: #UnspecificRecord category: #'Tools-Changes'!
ChangeListElement subclass: #UnspecificRecord
	instanceVariableNames: 'file position stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!UnspecificRecord commentStamp: 'jmv 30/Apr/2026 11:44:55' prior: 0!
A UnspecificRecord represents a change recorded on a file in fileOut format.
It includes a type (more needs to be done here), and additional information
for certain types such as method defs which need class and category.!


!ClassCommentChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 12:20:46'!
changeClass
	| methodClassName |
	methodClassName := class substrings
		ifEmpty: [ ^ nil ]
		ifNotEmpty: [ :parts |
			parts first asSymbol ].
	^(Smalltalk hasClassNamed: methodClassName) ifTrue: [
		Smalltalk classNamed: methodClassName ]! !

!ClassCommentChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:40:09'!
changeClassName
	^class! !

!ClassCommentChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:40:09'!
compilerClass
	^ (Smalltalk at: class ifAbsent: [^ Compiler]) compilerClass! !

!ClassCommentChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 14:41:47'!
dateAndTime

	^CompiledMethod authorAndStamp: stamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]! !

!ClassCommentChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:40:09'!
fileName
	^(file ifNotNil: [ file name ]) 
			ifNil: [ '<no file>' ]! !

!ClassCommentChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:13:05'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	aFileStream nextPut: $!!.
	aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.
	aFileStream newLine.
	aFileStream nextChunkPut: self string.
	aFileStream newLine.! !

!ClassCommentChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:40:09'!
stamp
	^ stamp! !

!ClassCommentChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:40:09'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| string mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	string := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ string! !

!ClassCommentChange methodsFor: 'initialization' stamp: 'jmv 30/Apr/2026 12:17:13'!
file: aFile position: aPosition class: aClassName stamp: aStamp

	file := aFile.
	position := aPosition.
	class := aClassName.
	stamp := aStamp.! !

!ClassCommentChange methodsFor: 'initialization' stamp: 'jmv 30/Apr/2026 10:11:24'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."

	(Smalltalk at: class)
		comment: self text
		stamp: stamp.! !

!ClassCommentChange methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:47:28'!
isClassComment
	^true! !

!ClassCommentChange methodsFor: 'perform' stamp: 'jmv 30/Apr/2026 11:01:16'!
performOn: aCodeFile

	 ^aCodeFile classComment: self! !


!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:07:48'!
changeClass
	| methodClassName methodClass |
	methodClassName := class substrings
		ifEmpty: [ ^ nil ]
		ifNotEmpty: [ :parts |
			parts first asSymbol ].
	(Smalltalk includesKey: methodClassName) ifFalse: [ ^ nil ].
	methodClass := Smalltalk at: methodClassName.
	^ meta
		ifTrue: [ methodClass class ]
		ifFalse: [ methodClass ]! !

!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:27'!
changeClassName
	^class! !

!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:27'!
compilerClass
	^ (Smalltalk at: class ifAbsent: [^ Compiler]) compilerClass! !

!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 14:41:53'!
dateAndTime

	^CompiledMethod authorAndStamp: stamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]! !

!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:27'!
fileName
	^(file ifNotNil: [ file name ]) 
			ifNil: [ '<no file>' ]! !

!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 12:22:21'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	aFileStream nextPut: $!!.
	aFileStream nextChunkPut: 
		'classDefinition: ', 
		(self isMetaClassChange ifTrue: [self changeClassName, ' class'] ifFalse: [self changeClassName]) printString,
		' category: ', category printString.
	aFileStream newLine.
	aFileStream nextChunkPut: self string.
	aFileStream newLine.! !

!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:27'!
isMetaClassChange
	^meta! !

!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:27'!
stamp
	^ stamp! !

!ClassDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:27'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| string mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	string := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ string! !

!ClassDefinitionChange methodsFor: 'initialization' stamp: 'jmv 30/Apr/2026 12:11:03'!
file: aFile position: aPosition class: aClassName category: aClassCategory meta: isMeta stamp: aStamp

	file := aFile.
	position := aPosition.
	class := aClassName.
	category := aClassCategory.
	meta := isMeta.
	stamp := aStamp.! !

!ClassDefinitionChange methodsFor: 'initialization' stamp: 'jmv 30/Apr/2026 10:10:29'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."

	Compiler evaluate: self string.! !

!ClassDefinitionChange methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:36:03'!
isClassDefinition
	^true! !

!ClassDefinitionChange methodsFor: 'perform' stamp: 'jmv 30/Apr/2026 11:01:38'!
performOn: aCodeFile

	 ^aCodeFile classDefinition: self! !


!ClassDeletionChange methodsFor: 'fileIn/fileOut' stamp: 'HAW 25/Oct/2019 10:41:36'!
fileIn

	doItOnlyIfInBaseSystem
		ifTrue: [ Smalltalk removeClassNamedIfInBaseSystem: className]
		ifFalse: [ self changeClass ifNotNil: [ :aClass | aClass removeFromSystem ] ]! !

!ClassDeletionChange methodsFor: 'fileIn/fileOut' stamp: 'HAW 26/Oct/2019 10:50:42'!
fileOutOn: stream
	"File the receiver out on the given file stream"

	| record |
		
	record := String streamContents: [ :recordStream |
		recordStream
		 	nextPutAll: (doItOnlyIfInBaseSystem ifTrue: [ 'classMoveToSomePackage: #' ] ifFalse: ['classRemoval: #']);
			nextPutAll: className;
			nextPutAll: ' stamp: ';
			print: stamp ].
		
	stream 
		nextPut: $!!;
		nextChunkPut: record;
		newLine;
		nextChunkPut: self command;
		newLine; newLine.
	! !

!ClassDeletionChange methodsFor: 'accessing' stamp: 'HAW 24/Oct/2019 08:54:41'!
changeClass
	^Smalltalk at: className ifAbsent: nil! !

!ClassDeletionChange methodsFor: 'accessing' stamp: 'HAW 24/Oct/2019 08:54:42'!
changeClassName
	^className! !

!ClassDeletionChange methodsFor: 'accessing' stamp: 'HAW 24/Oct/2019 08:57:18'!
className: aSymbol

	className := aSymbol! !

!ClassDeletionChange methodsFor: 'accessing' stamp: 'jmv 23/Mar/2012 19:37:00'!
doItOnlyIfInBaseSystem: aBoolean
	doItOnlyIfInBaseSystem := aBoolean! !

!ClassDeletionChange methodsFor: 'accessing' stamp: 'HAW 26/Oct/2019 17:53:44'!
stamp

	^stamp! !

!ClassDeletionChange methodsFor: 'accessing' stamp: 'HAW 24/Oct/2019 08:56:50'!
string
	
	^classDefinition ifNil: [ '' ]! !

!ClassDeletionChange methodsFor: 'testing' stamp: 'jmv 7/Dec/2011 23:14:00'!
isClassDeletion
	^true! !

!ClassDeletionChange methodsFor: 'initialization' stamp: 'HAW 24/Oct/2019 08:55:20'!
initializeClassName: aClassName definition: aClassDefinition doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem stamp: aStamp 
	
	className := aClassName.
	classDefinition := aClassDefinition.
	doItOnlyIfInBaseSystem := aDoItOnlyIfInBaseSystem.
	stamp := aStamp.! !

!ClassDeletionChange methodsFor: 'services' stamp: 'HAW 26/Oct/2019 10:38:56'!
command

	^doItOnlyIfInBaseSystem
		ifTrue: [ 'Smalltalk removeClassNamedIfInBaseSystem: #', className ]
		ifFalse: [ 'Smalltalk removeClassNamed: #', className ].
! !

!ClassDeletionChange methodsFor: 'perform' stamp: 'jmv 30/Apr/2026 11:02:06'!
performOn: aCodeFile

	 ^aCodeFile classRemoval: self! !


!ClassDeletionChange class methodsFor: 'instance creation' stamp: 'HAW 24/Oct/2019 08:55:05'!
className: aClassName definition: aClassDefinition doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem stamp: aStamp 

	^self new 
		initializeClassName: aClassName 
		definition: aClassDefinition 
		doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem 
		stamp: aStamp ! !


!ClassRenamedChange methodsFor: 'initialization' stamp: 'MGD 19/Dec/2019 19:19:01'!
initializeFrom: previousClassName to: newClassName stamp: aString 

	previousName := previousClassName.
	newName := newClassName.
	stamp := aString.! !

!ClassRenamedChange methodsFor: 'accessing' stamp: 'MGD 19/Dec/2019 21:14:43'!
changeClass
	^ nil! !

!ClassRenamedChange methodsFor: 'accessing' stamp: 'MGD 19/Dec/2019 19:32:01'!
changeClassName
	^ previousName ! !

!ClassRenamedChange methodsFor: 'accessing' stamp: 'MGD 19/Dec/2019 19:33:26'!
newClassName
	^ newName ! !

!ClassRenamedChange methodsFor: 'accessing' stamp: 'MGD 19/Dec/2019 19:31:06'!
stamp
	^stamp! !

!ClassRenamedChange methodsFor: 'printing' stamp: 'sqr 5/Apr/2023 21:07:18'!
string
	^ 'classRenamed: #', previousName, ' as: #', newName! !

!ClassRenamedChange methodsFor: 'fileIn/fileOut' stamp: 'jmv 8/Apr/2023 11:05:42'!
fileIn

	Smalltalk renameClassNamed: previousName as: newName! !

!ClassRenamedChange methodsFor: 'perform' stamp: 'jmv 30/Apr/2026 11:03:02'!
performOn: aCodeFile

	 ^aCodeFile classRenamed: self! !


!ClassRenamedChange class methodsFor: 'instance creation' stamp: 'MGD 19/Dec/2019 19:17:45'!
from: previousClassName to: newClassName stamp: aString
	^self new initializeFrom: previousClassName to: newClassName stamp: aString ! !


!FeatureProvidesRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:51:36'!
changeClass
	^nil! !

!FeatureProvidesRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:51:36'!
feature
	^feature! !

!FeatureProvidesRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 11:19:08'!
feature: aFeature
	feature := aFeature.! !

!FeatureProvidesRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:30:37'!
string
	^ 'provides: ', feature printString! !

!FeatureProvidesRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 30/Apr/2026 09:51:36'!
fileIn
	
	"It does nothing - Hernan"! !

!FeatureProvidesRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 30/Apr/2026 09:51:36'!
fileOutOn: aFileStream

	"Nothing to file out - Hernan"! !

!FeatureProvidesRecord methodsFor: 'perform' stamp: 'jmv 30/Apr/2026 11:03:30'!
performOn: aCodeFile

	 ^aCodeFile provides: self! !


!FeatureRequiresRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:51:47'!
changeClass
	^nil! !

!FeatureRequiresRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:51:47'!
feature
	^feature! !

!FeatureRequiresRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 11:19:00'!
feature: aFeature
	feature := aFeature.! !

!FeatureRequiresRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:30:52'!
string
	^ 'requires: ', feature printString! !

!FeatureRequiresRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 30/Apr/2026 09:51:47'!
fileIn
	
	"It does nothing - Hernan"! !

!FeatureRequiresRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 30/Apr/2026 09:51:47'!
fileOutOn: aFileStream

	"Nothing to file out - Hernan"! !

!FeatureRequiresRecord methodsFor: 'perform' stamp: 'jmv 30/Apr/2026 11:03:46'!
performOn: aCodeFile

	 ^aCodeFile requires: self! !


!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:08:01'!
changeClass
	| methodClassName methodClass |
	methodClassName := class substrings
		ifEmpty: [ ^ nil ]
		ifNotEmpty: [ :parts |
			parts first asSymbol ].
	(Smalltalk includesKey: methodClassName) ifFalse: [ ^ nil ].
	methodClass := Smalltalk at: methodClassName.
	^ meta
		ifTrue: [ methodClass class ]
		ifFalse: [ methodClass ]! !

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:51'!
changeClassName
	^class! !

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:51'!
compilerClass
	^ (Smalltalk at: class ifAbsent: [^ Compiler]) compilerClass! !

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 14:42:00'!
dateAndTime

	^CompiledMethod authorAndStamp: stamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]! !

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:51'!
fileName
	^(file ifNotNil: [ file name ]) 
			ifNil: [ '<no file>' ]! !

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:14:15'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	self changeClass printCategoryChunk: category on: aFileStream
		withStamp: stamp libraryName: libraryName priorMethod: nil.
	aFileStream newLine.
	aFileStream nextChunkPut: self string.
	aFileStream nextChunkPut: ' '; newLine.
	aFileStream newLine.! !

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:51'!
isMetaClassChange
	^meta! !

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:14:57'!
methodSelector
	^(Smalltalk at: class ifAbsent: [Object]) parserClass new parseSelector: self string! !

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:51'!
stamp
	^ stamp! !

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:39:51'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| string mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	string := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ string! !

!MethodDefinitionChange methodsFor: 'initialization' stamp: 'jmv 30/Apr/2026 11:57:56'!
file: aFile position: aPosition class: aClassName category: aClassCategory meta: isMeta stamp: aStamp prior: aPrior libraryName: aString

	file := aFile.
	position := aPosition.
	class := aClassName.
	category := aClassCategory.
	meta := isMeta.
	stamp := aStamp.
	prior := aPrior.
	libraryName := aString.! !

!MethodDefinitionChange methodsFor: 'initialization' stamp: 'jmv 30/Apr/2026 10:11:15'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."

	self changeClass ifNotNil: [ :methodClass |
		methodClass
			compile: self text
			classified: category
			withStamp: stamp
			libraryName: libraryName
			notifying: nil ].! !

!MethodDefinitionChange methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:40:12'!
isMethod

	^true! !

!MethodDefinitionChange methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:35:13'!
isMethodDefinition

	^true! !

!MethodDefinitionChange methodsFor: 'perform' stamp: 'jmv 30/Apr/2026 11:04:05'!
performOn: aCodeFile

	 ^aCodeFile method: self! !


!MethodDeletionChange methodsFor: 'accessing' stamp: 'jmv 31/Jan/2013 22:59:00'!
changeClass
	^methodReference actualClass! !

!MethodDeletionChange methodsFor: 'accessing' stamp: 'jmv 31/Jan/2013 22:55:00'!
changeClassName
	^methodReference classIsMeta
		ifFalse: [ methodReference classSymbol ]
		ifTrue: [ methodReference classSymbol, ' class' ]! !

!MethodDeletionChange methodsFor: 'accessing' stamp: 'jmv 23/Mar/2012 19:37:00'!
doItOnlyIfInBaseSystem: aBoolean
	doItOnlyIfInBaseSystem := aBoolean! !

!MethodDeletionChange methodsFor: 'accessing' stamp: 'jmv 8/Dec/2011 22:57:00'!
isMetaClassChange
	^methodReference classIsMeta! !

!MethodDeletionChange methodsFor: 'accessing' stamp: 'jmv 7/Dec/2011 17:48:00'!
methodReference: aMethodReference
	methodReference := aMethodReference! !

!MethodDeletionChange methodsFor: 'accessing' stamp: 'jmv 7/Dec/2011 23:09:00'!
methodSelector
	^methodReference selector! !

!MethodDeletionChange methodsFor: 'accessing' stamp: 'HAW 26/Oct/2019 17:53:57'!
stamp

	^stamp! !

!MethodDeletionChange methodsFor: 'accessing' stamp: 'HAW 23/Oct/2019 17:22:05'!
string

	^sourceCode ifNil: [ '' ]! !

!MethodDeletionChange methodsFor: 'fileIn/fileOut' stamp: 'jmv 23/Mar/2012 19:55:00'!
fileIn

	doItOnlyIfInBaseSystem
		ifTrue: [ methodReference actualClass removeSelectorIfInBaseSystem: self methodSelector ]
		ifFalse: [ methodReference actualClass removeSelector: self methodSelector ]! !

!MethodDeletionChange methodsFor: 'fileIn/fileOut' stamp: 'HAW 26/Oct/2019 10:48:23'!
fileOutOn: stream
	"File the receiver out on the given file stream"

	| record |

	record := String streamContents: [ :recordStream |
		recordStream
		 	nextPutAll: (doItOnlyIfInBaseSystem ifTrue: [ 'methodMoveToSomePackage: ' ] ifFalse: ['methodRemoval: ']);
			nextPutAll: self changeClassName;
			space;
			nextPutAll: self methodSelector storeString;
			nextPutAll: ' stamp: ';
			print: stamp ].
		
	stream 
		nextPut: $!!;
		nextChunkPut: record;
		newLine;
		nextChunkPut: self command;
		newLine
	
! !

!MethodDeletionChange methodsFor: 'services' stamp: 'HAW 26/Oct/2019 10:48:08'!
command

	^String streamContents: [ :stream |
		stream
			nextPutAll: self changeClassName;
			space;
			nextPutAll: (doItOnlyIfInBaseSystem ifTrue: ['removeSelectorIfInBaseSystem:'] ifFalse: ['removeSelector:']);
			space;
			nextPutAll: self methodSelector storeString ]! !

!MethodDeletionChange methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:40:06'!
isMethod
	^true! !

!MethodDeletionChange methodsFor: 'testing' stamp: 'jmv 8/Feb/2013 08:10:00'!
isMethodDeletion
	^true! !

!MethodDeletionChange methodsFor: 'initialization' stamp: 'HAW 23/Oct/2019 23:35:56'!
initializeMethodReference: aMethodReference doItOnlyIfInBaseSystem: aDoit source: aSourceCode stamp: aStamp  
	
	methodReference := aMethodReference.
	doItOnlyIfInBaseSystem := aDoit.
	sourceCode := aSourceCode.
	stamp := aStamp ! !

!MethodDeletionChange methodsFor: 'perform' stamp: 'HAW 25/Oct/2019 10:37:08'!
performOn: aCodeFile

	^aCodeFile removedMethod: self command with: self ! !


!MethodDeletionChange class methodsFor: 'instance creation' stamp: 'HAW 23/Oct/2019 23:36:19'!
methodReference: aMethodReference doItOnlyIfInBaseSystem: aDoit source: aSourceCode stamp: aStamp 

	^self new 
		initializeMethodReference: aMethodReference 
		doItOnlyIfInBaseSystem: aDoit 
		source: aSourceCode 
		stamp: aStamp ! !


!PreambleRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:21:54'!
changeClass
	^nil! !

!PreambleRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 14:42:09'!
dateAndTime

	^CompiledMethod authorAndStamp: stamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]! !

!PreambleRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:42:50'!
fileName
	^(file ifNotNil: [ file name ]) 
			ifNil: [ '<no file>' ]! !

!PreambleRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:24:55'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	aFileStream nextPut: $!!.
	aFileStream nextChunkPut: self string.
	aFileStream newLine.! !

!PreambleRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:42:50'!
stamp
	^ stamp! !

!PreambleRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:42:50'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| string mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	string := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ string! !

!PreambleRecord methodsFor: 'initialization' stamp: 'jmv 30/Apr/2026 11:17:48'!
file: aFile position: aPosition

	file := aFile.
	position := aPosition.
"
file closed ifFalse: [
	'' print.
	file print.
	self print.
	thisContext printStack: 10 ]
"! !

!PreambleRecord methodsFor: 'initialization' stamp: 'jmv 30/Apr/2026 10:23:22'!
fileIn
	"File the receiver in.
	Nothing to do for us."
! !

!PreambleRecord methodsFor: 'perform' stamp: 'jmv 30/Apr/2026 11:04:35'!
performOn: aCodeFile

	 ^aCodeFile preamble: self! !


!UnspecificRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:21:56'!
changeClass
	^nil! !

!UnspecificRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 14:42:13'!
dateAndTime

	^CompiledMethod authorAndStamp: stamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]! !

!UnspecificRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:48:19'!
fileName
	^(file ifNotNil: [ file name ]) 
			ifNil: [ '<no file>' ]! !

!UnspecificRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 10:24:33'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	aFileStream nextChunkPut: self string.
	aFileStream newLine.! !

!UnspecificRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:48:19'!
stamp
	^ stamp! !

!UnspecificRecord methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 09:48:19'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| string mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	string := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ string! !

!UnspecificRecord methodsFor: 'initialization' stamp: 'jmv 30/Apr/2026 11:17:54'!
file: aFile position: aPosition

	file := aFile.
	position := aPosition.
"
file closed ifFalse: [
	'' print.
	file print.
	self print.
	thisContext printStack: 10 ]
"! !

!UnspecificRecord methodsFor: 'initialization' stamp: 'jmv 30/Apr/2026 10:23:48'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."

	| s |
	((s := self string) beginsWith: '----') ifFalse: [
		Compiler evaluate: s ].! !

!UnspecificRecord methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:32:45'!
isDoIt

	^true! !

!UnspecificRecord methodsFor: 'perform' stamp: 'jmv 30/Apr/2026 11:04:53'!
performOn: aCodeFile

	 ^aCodeFile doIt: self! !

