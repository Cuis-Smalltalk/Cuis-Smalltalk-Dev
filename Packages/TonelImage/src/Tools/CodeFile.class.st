"
My instances represent a file with Smalltalk code.
"
Class {
	#name : 'CodeFile',
	#superclass : 'Object',
	#instVars : [
		'fullName',
		'sourceSystem',
		'classes',
		'doIts',
		'classOrder'
	],
	#category : 'Tools-Code File Browser',
	#package : 'Tools',
	#tag : 'Code File Browser'
}

{ #category : 'instance creation' }
CodeFile class >> newFromFile: aFileEntry [
	^ aFileEntry readStreamDo: [ :stream |
		self new
			fullName: aFileEntry pathName;
			buildFrom: stream ].
]

{ #category : 'xtras' }
CodeFile >> allMethodReferences [
	"Create an answer a Set with method references for all methods in us"
	| answer className metaClass |
	answer := Set new.
	
	classes do: [ :pseudoClass | 
		className := pseudoClass name.
		pseudoClass selectors do: [ :selector |
			answer add: 
				(MethodReference new
					setClassSymbol: className
					classIsMeta: false
					methodSymbol: selector
					stringVersion: className, ' ' , selector) ].
		pseudoClass hasMetaclass ifTrue: [
			metaClass := pseudoClass theMetaClass.
			metaClass selectors do: [ :selector |
				answer add: 
					(MethodReference new
						setClassSymbol: className
						classIsMeta: true
						methodSymbol: selector
						stringVersion: className, ' class ' , selector) ].
				]].
	^answer
]

{ #category : 'fileIn/fileOut' }
CodeFile >> askForDoits [
	| menu choice choices |
	choices := #('do not process' 'at the beginning' 'at the end' 'cancel').
	menu := SelectionMenu selections: choices.
	choice := nil.
	[choices includes: choice] whileFalse: [
		choice := menu startUpWithCaption: 
'The CodeFile contains unprocessed doIts.
When would like to process those?'].
	^choices indexOf: choice
]

{ #category : 'private' }
CodeFile >> baseLabel [
	^ 'base'
]

{ #category : 'change list' }
CodeFile >> browseFileChanges [

	ChangeList browseContents: (FileEntry withAbsolutePathName: fullName).
]

{ #category : 'reading' }
CodeFile >> buildFrom: aStream [

	| changes |
	changes := (ChangeList new scanFile: aStream from: 0 to: aStream size) changeList.
	'Processing ', self name
		displayProgressAt: Sensor mousePoint
		from: 1
		to: changes size
		during: [ :barBlock | self buildFrom: changes informingTo: barBlock ].
]

{ #category : 'reading' }
CodeFile >> buildFrom: changes informingTo: barBlock  [

	changes withIndexDo: [ :changeRecord :anIndex | 
		barBlock value: anIndex.
		changeRecord performOn: self.
	].

]

{ #category : 'change record types' }
CodeFile >> classComment: chgRec [
	"See senders of #classComment "
	(self getClass: chgRec changeClassName) classComment: chgRec
]

{ #category : 'change record types' }
CodeFile >> classDefinition: chgRec [
	"See senders of #classDefinition "
	| string |
	string := chgRec string.
	^chgRec isMetaClassChange 
		ifTrue: [self metaClassDefinition: string with: chgRec]
		ifFalse: [self classDefinition: string with: chgRec]
]

{ #category : 'private' }
CodeFile >> classDefinition: string with: chgRec [
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	tokens size = 11 ifFalse:[^doIts add: chgRec].
	theClass := self getClass: (tokens at: 3).
	theClass definition: string.
	classOrder add: theClass.
]

{ #category : 'accessing' }
CodeFile >> classDictionary [
	^classes
]

{ #category : 'accessing' }
CodeFile >> classNamed: className [
	^ classes at: className
]

{ #category : 'change record types' }
CodeFile >> classRemoval: aClassDeletionChangeRecord  [
	
	^self classDefinition: aClassDeletionChangeRecord 
]

{ #category : 'accessing' }
CodeFile >> classes [
	^ self classDictionary values
]

{ #category : 'accessing' }
CodeFile >> description [
	^String streamContents:[:s|
		s nextPutAll: 'CodeFile: '.
		s nextPutAll: fullName; newLine; newLine.
		sourceSystem isEmpty ifFalse:[
			s nextPutAll: sourceSystem; newLine; newLine ].
		doIts isEmpty ifFalse:[
			s nextPutAll: 'Unresolvable doIts:'; newLine; newLine.
			doIts do: [ :chgRec |
				s
					nextPut:$!;
					nextPutAll: chgRec string;
					nextPut: $!;
					newLine ]]]
]

{ #category : 'change record types' }
CodeFile >> doIt: chgRec [
	"See senders of #doIt "
	| string |
	string := chgRec string.
	
	"Method classification spec"
	(string beginsWith: '(''') ifTrue: [
		^ doIts add: chgRec ].
		
	"Just for compatibility with Squeak, as Cuis always adds the #classDefinition change type marker in the files."
	('*ubclass:*instanceVariableNames:*classVariableNames:*poolDictionaries:*category:*'
		match: string) ifTrue:[^self classDefinition: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #classDefinition change type marker in the files."
	('* class*instanceVariableNames:*'
		match: string) ifTrue:[^self metaClassDefinition: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #methodRemoval: (or similar) change type marker in the files."
	('* removeSelector: *'
		match: string) ifTrue:[^self removedMethod: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #classComment change type marker in the files."
	('* comment:*'
		match: string) ifTrue:[^self msgClassComment: string with: chgRec].

	"Don't add these to a CodeFile. They will be added on save if needed."
	('* initialize'
		match: string) ifTrue:[^self]. "Initialization is done based on class>>initialize"

	('''From *'
		match: string) ifTrue:[^self possibleSystemSource: chgRec].
	doIts add: chgRec.
]

{ #category : 'fileIn/fileOut' }
CodeFile >> fileIn [
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 4 ifTrue: [^nil].
	doitsMark = 2 ifTrue:[self fileInDoits].
	classOrder do:[:cls|
		cls fileInDefinition.
	].
	classes do:[:cls|
		(Preferences at: #transcriptLogVerbose) ifTrue: [
			Transcript newLine; show:'Filing in ', cls name].
		cls fileInMethods.
		cls hasMetaclass ifTrue:[cls theMetaClass fileInMethods].
	].
	doitsMark = 3 ifTrue: [ self fileInDoits ]
]

{ #category : 'fileIn/fileOut' }
CodeFile >> fileInDoits [
	doIts do:[:chgRec| chgRec fileIn].
]

{ #category : 'fileIn/fileOut' }
CodeFile >> fileOut [
		DirectoryEntry fileOutsDirQuery: 'filename.st' writeStreamDo: [ :fileStream |
			sourceSystem isEmpty
				ifFalse: [ fileStream nextChunkPut: sourceSystem printString; newLine ].
			self fileOutOn: fileStream.
			fileStream newLine; newLine.
			classes do: [ :cls |
				cls needsInitialize
					ifTrue: [ fileStream newLine; nextChunkPut: cls name,' initialize']].
			fileStream newLine ].
]

{ #category : 'fileIn/fileOut' }
CodeFile >> fileOutDoits: aStream [
	doIts do:[:chgRec| chgRec fileOutOn: aStream].
]

{ #category : 'fileIn/fileOut' }
CodeFile >> fileOutOn: aStream [
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 4 ifTrue: [^nil].
	doitsMark = 2 ifTrue:[self fileOutDoits: aStream].
	classOrder do:[:cls|
		cls fileOutDefinitionOn: aStream.
	].
	classes do:[:cls|
		cls fileOutMethodsOn: aStream.
		cls hasMetaclass ifTrue:[cls theMetaClass fileOutMethodsOn: aStream].
	].
	doitsMark = 3 ifTrue:[self fileOutDoits: aStream].
]

{ #category : 'accessing' }
CodeFile >> fullName [
	^fullName
]

{ #category : 'accessing' }
CodeFile >> fullName: aString [
	fullName := aString
]

{ #category : 'private' }
CodeFile >> getClass: className [
	| pseudoClass |
	(classes includesKey: className) ifTrue:[
		^classes at: className.
	].
	pseudoClass := PseudoClass new.
	pseudoClass name: className.
	classes at: className put: pseudoClass.
	^pseudoClass.
]

{ #category : 'initialization' }
CodeFile >> initialize [
	classes := Dictionary new.
	classOrder := OrderedCollection new.
	sourceSystem := ''.
	doIts := OrderedCollection new
]

{ #category : 'testing' }
CodeFile >> isLiveSmalltalkImage [
	^ false
]

{ #category : 'private' }
CodeFile >> metaClassDefinition: string with: chgRec [
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	theClass := self getClass: (tokens at: 1).
	theClass theMetaClass definition: string.
	classOrder add: theClass theMetaClass.
]

{ #category : 'change record types' }
CodeFile >> method: chgRec [
	"See senders of #method "
	(self getClass: chgRec changeClassName) methodChange: chgRec
]

{ #category : 'private' }
CodeFile >> msgClassComment: string with: chgRec [
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	(tokens size = 3 and:[(tokens at: 3) isString ]) ifTrue:[
		theClass := self getClass: tokens first.
		^theClass classComment: tokens last].
	(tokens size = 4 and:[(tokens at: 3) asString = 'class' and:[(tokens at: 4) isString ]]) ifTrue:[
		theClass := self getClass: tokens first.
		theClass theMetaClass classComment: tokens last].

]

{ #category : 'accessing' }
CodeFile >> name [

	^ fullName asFullFileEntry name
]

{ #category : 'accessing' }
CodeFile >> organization [
	^ SystemOrganizer empty.
]

{ #category : 'private' }
CodeFile >> possibleSystemSource: chgRec [
	| tokens |
	sourceSystem isEmpty ifTrue:[
		tokens := Scanner new scanTokens: chgRec string.
		(tokens size = 1 and: [tokens first isString]) ifTrue:[
			sourceSystem := tokens first.
			^self]].
	doIts add: chgRec.
]

{ #category : 'change record types' }
CodeFile >> preamble: chgRec [
	"See senders of #preamble "
	self doIt: chgRec
]

{ #category : 'accessing' }
CodeFile >> removeClass: aPseudoClass [
	classes removeKey: aPseudoClass name.
	classOrder copy do: [ :cls |
		cls name = aPseudoClass name ifTrue: [ classOrder remove: cls]]
]

{ #category : 'accessing' }
CodeFile >> removeDoIts [
	doIts := OrderedCollection new.
]

{ #category : 'private' }
CodeFile >> removedMethod: string with: chgRec [
	| class tokens firstToken secondToken thirdToken |
	tokens := Scanner new scanTokens: string.
	tokens size >= 3 ifTrue: [
		firstToken := tokens at: 1.
		secondToken := tokens at: 2.
		thirdToken := tokens at: 3.
		(tokens size = 3 and: [ secondToken == #removeSelector: or: [ secondToken == #removeSelectorIfInBaseSystem: ]]) ifTrue:[
			class := self getClass: firstToken.
			^class perform: secondToken with: thirdToken.
		].
		(tokens size = 4 and: [ secondToken == #class and: [ thirdToken == #removeSelector: or: [ thirdToken == #removeSelectorIfInBaseSystem: ]]]) ifTrue:[
			class := self getClass: firstToken.
			^class theMetaClass perform: thirdToken with: (tokens at: 4).
		].
	].
	doIts add: chgRec
]

{ #category : 'accessing' }
CodeFile >> renameClass: aPseudoClass to: newName [
	| oldName |
	oldName := aPseudoClass name.
	classes removeKey: oldName.
	classes at: newName put: aPseudoClass.
	aPseudoClass renameTo: newName.
]

{ #category : 'accessing' }
CodeFile >> sourceSystem [
	^sourceSystem
]

{ #category : 'accessing' }
CodeFile >> summary [
	| nClasses newClasses oldClasses |
	nClasses := newClasses := oldClasses := 0.
	classes do:[:cls|
		nClasses := nClasses + 1.
		(Smalltalk includesKey: (cls name asSymbol))
			ifTrue: [ oldClasses := oldClasses + 1]
			ifFalse: [ newClasses := newClasses + 1]].
	^nClasses printString,' classes (', newClasses printString, ' new / ', oldClasses printString, ' modified)'
]
