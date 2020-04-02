'From Cuis 5.0 [latest update: #4102] on 2 April 2020 at 3:45:31 pm'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 4/2/2020 11:38:53'!
getPreamble
	| file preamble |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file _ SourceFiles at: self fileIndex.
	file ifNil: [^ ''].  "sources file not available"
	"file does not exist happens in secure mode"
	[
		file name asFileEntry readStreamDo: [ :stream |
			preamble _ (self getPreambleFrom: stream at: (0 max: self filePosition)) ifNil: [ '' ].
			]
	] on: FileDoesNotExistException do: [ :ex | preamble _ '' ].
	^ preamble! !


!CompiledMethod class methodsFor: 'source code management aux' stamp: 'HAW 10/29/2019 11:06:21'!
classCategoryFrom: tokens

	^self field: #category: from: tokens ifAbsentOrNil: [ Categorizer default ]! !

!CompiledMethod class methodsFor: 'source code management aux' stamp: 'HAW 10/29/2019 10:53:32'!
field: aFieldName from: tokens ifAbsentOrNil: aBlock

	| fieldValue fieldIndex |
	
	fieldIndex _ tokens indexOf: aFieldName ifAbsent: [ ^aBlock value ].
	fieldValue _ tokens at: fieldIndex + 1 ifAbsent: [ ^aBlock value ].
	
	^ fieldValue ifNil: aBlock! !

!CompiledMethod class methodsFor: 'source code management aux' stamp: 'jmv 4/2/2020 12:50:44'!
overridenMethodReferenceFrom: tokens

	^ self field: #overrides: from: tokens ifAbsentOrNil: [ nil ]! !

!CompiledMethod class methodsFor: 'source code management aux' stamp: 'jmv 4/2/2020 12:51:03'!
priorReferenceFrom: tokens

	^ self field: #prior: from: tokens ifAbsentOrNil: [ nil ]! !

!CompiledMethod class methodsFor: 'source code management aux' stamp: 'HAW 10/29/2019 10:54:41'!
stampFrom: tokens

	^self field: #stamp: from: tokens ifAbsentOrNil: [ '' ]	
	
! !


!MessageSet methodsFor: 'message list' stamp: 'jmv 4/2/2020 13:12:43'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class _ selectedMessage actualClass.
	selector _ selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod _ class organization commentRemoteStr.
			^ class comment ].
		selector == #Definition ifTrue: [
			^ class definition ].
		selector == #Hierarchy ifTrue: [
			^ class printHierarchy ]].

	(class notNil and: [ class includesSelector: selector]) ifFalse: [
		currentCompiledMethod _ nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod _ class compiledMethodAt: selector ifAbsent: nil.
	self showingDocumentation ifTrue: [
		^ self commentContents ].

	^self sourceStringPrettifiedAndDiffed! !


!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/29/2019 11:06:27'!
classDefinitionRecordFrom: tokens
	
	| classDefinition isMeta itemPosition className record fullClassName category stamp |
	
	itemPosition _ file position.
	fullClassName _ tokens second.
	isMeta _ fullClassName includesSubString: ' class'.
	className _ isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	category _ CompiledMethod classCategoryFrom: tokens.
	stamp _ CompiledMethod stampFrom: tokens.
	classDefinition _ file nextChunk.
	
	record _ ChangeRecord new 
		file: file 
		position: itemPosition 
		type: #classDefinition 
		class: className asSymbol 
		category: category 
		meta: isMeta 
		stamp: stamp.
		
	record markAsTest: (classDefinition beginsWith: TestCase name asString).
	
	^record
! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 17:58:04'!
scanClassRemoval: tokens

	| doItOnlyIfInBaseSystem removeType classDefinition className description record stamp |
	
	removeType _ tokens first.
	className _ tokens second.
	doItOnlyIfInBaseSystem _ removeType == #classMoveToSomePackage:.
	stamp _ CompiledMethod stampFrom: tokens.
	classDefinition _ file nextChunk.
	
	record _ ClassDeletionChangeRecord 
			className: className
			definition: classDefinition 
			doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
			stamp: stamp.

	description _ doItOnlyIfInBaseSystem 
			ifTrue: ['clase move to some package: '] 
			ifFalse: ['class removal: '].
			
	self 
		addItem: record
		text: description, className, '; ', stamp ! !

!ChangeList methodsFor: 'scanning' stamp: 'MGD 12/19/2019 19:36:26'!
scanClassRenamed: tokens

	| record stamp newName previousName preamble |

	preamble _ tokens first.
	previousName _ tokens second.
	newName _ tokens fourth.
	stamp _ CompiledMethod stampFrom: tokens.
	file nextChunk.
	
	record _ ClassRenamedChangeRecord from: previousName to: newName stamp: stamp.
			
	self 
		addItem: record
		text: preamble, previousName, ' - ', newName, '; ', stamp ! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 4/2/2020 12:51:10'!
scanMethodDefinition: tokens

	| stamp className priorMethod overridenMethod |
	
	className _ tokens first.
	stamp _ CompiledMethod stampFrom: tokens.
	priorMethod _ CompiledMethod priorReferenceFrom: tokens.
	overridenMethod _ CompiledMethod overridenMethodReferenceFrom: tokens.
	
	tokens second == #methodsFor: ifTrue: [
		^ self scanCategory: tokens third class: className meta: false stamp: stamp prior: priorMethod overrides: overridenMethod ].

	tokens third == #methodsFor: ifTrue: [
		^ self scanCategory: tokens fourth class: className meta: true stamp: stamp prior: priorMethod overrides: overridenMethod ].
	
	self error: 'Unsupported method definition' 
! !

!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 23:23:14'!
scanMethodRemoval: tokens

	| doItOnlyIfInBaseSystem removeType isMeta sourceCode methodReference className description record selector stamp |

	removeType _ tokens first.
	className _ tokens second.
	doItOnlyIfInBaseSystem _ removeType == #methodMoveToSomePackage:.

	sourceCode _ file nextChunk.
	isMeta _ tokens third == #class.	
	selector _ isMeta ifTrue: [ tokens fourth ] ifFalse: [ tokens third ].
	stamp _ CompiledMethod stampFrom: tokens.

	methodReference _ MethodReference new
		setClassSymbol: className
		classIsMeta: isMeta
		methodSymbol: selector
		stringVersion: className, (isMeta ifTrue: [' class '] ifFalse: [' ']), selector,'; ', stamp.
		
	record _ MethodDeletionChangeRecord 
		methodReference: methodReference
		doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
		source: sourceCode 
		stamp: stamp.
	
	description _ doItOnlyIfInBaseSystem 
		ifTrue: ['method move to some package: '] 
		ifFalse: ['method removal: '].
		
	self
		addItem: record
		text: description, methodReference stringVersion! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 4/2/2020 12:51:19'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp prevPos prevFileIndex preamble tokens sourceFilesCopy |
	selectorOfMethod _ selector.
	currentCompiledMethod _ method.
	classOfMethod _ meta ifTrue: [class class] ifFalse: [class].
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	self addedChangeRecord ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex _ 0.
	position _ method filePosition.
	sourceFilesCopy _ SourceFiles collect:
		[:x | x ifNotNil: [ x name asFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file _ sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil]
		whileTrue:
		[file position: (0 max: position-150).  "Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevFileIndex _ nil.
		prevPos _ nil.
		stamp _ ''.
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens _ Scanner new scanTokens: preamble.
				stamp _ CompiledMethod stampFrom: tokens.
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorMethodRef |
					prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: priorMethodRef.
					prevPos _ sourceFilesCopy filePositionFromSourcePointer: priorMethodRef ]].
 		self addItem:
				(ChangeRecord new file: file position: position type: #method
						class: class name category: category meta: meta stamp: stamp)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position _ prevPos.
		prevPos notNil ifTrue: [
			file _ sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 4/2/2020 12:55:42'!
scanVersionsOf: class
	"Scan for all past versions of the class comment of the given class"

	| oldCommentRemoteStr sourceFilesCopy position prevPos stamp preamble tokens prevFileIndex |

	classOfMethod _ class.
	oldCommentRemoteStr _ class  organization commentRemoteStr.
	currentCompiledMethod _ oldCommentRemoteStr.
	selectorOfMethod _ #Comment.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	oldCommentRemoteStr ifNil:[^ nil] ifNotNil: [oldCommentRemoteStr sourcePointer].

	sourceFilesCopy _ SourceFiles collect: [ :x | x ifNotNil: [x name asFileEntry readStream]].
	position _ oldCommentRemoteStr position.
	file _ sourceFilesCopy at: oldCommentRemoteStr sourceFileNumber.
	[ position notNil & file notNil]  whileTrue: [
		file position: (0 max: position-150).  " Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		prevPos _ nil.
		stamp _ ''.
		(preamble includesSubString: 'commentStamp:')
			ifTrue: [
				tokens _ Scanner new scanTokens: preamble.
				stamp _ CompiledMethod field: #commentStamp: from: tokens ifAbsentOrNil: [ '' ].
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorRef |
					prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: priorRef.
					prevPos _ sourceFilesCopy filePositionFromSourcePointer: priorRef ]]
			ifFalse: [
				"The stamp get lost, maybe after a condenseChanges"
				stamp _ '<historical>'].
 		self addItem:
				(ChangeRecord new file: file position: position type: #classComment
						class: class name category: nil meta: class isMeta stamp: stamp)
			text: stamp , ' ' , class name , ' class comment'. 
		prevPos = 0 ifTrue: [ prevPos _ nil ].
		position _ prevPos.
		prevPos notNil ifTrue: [ file _ sourceFilesCopy at: prevFileIndex ]].
	sourceFilesCopy do: [ :x | x notNil ifTrue: [ x close ]].
	self clearSelections! !


!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 4/2/2020 12:23:16'!
timeStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available.
	(CompiledMethod compiledMethodAt: #timeStamp) timeStamp
	"
	| preamble tokens |
	preamble _ self getPreamble.
	(preamble includesSubString: 'methodsFor:') ifFalse: [ ^''].
	tokens _ Scanner new scanTokens: preamble.
	^CompiledMethod stampFrom: tokens! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 4/2/2020 12:18:19'!
allMethodsWithString: aString
	"Answer a sorted Collection of all the methods that contain, in a string literal, aString as a substring.  2/1/96 sw.  The search is case-sensitive, and does not dive into complex literals, confining itself to string constants.
	5/2/96 sw: fixed so that duplicate occurrences of aString in the same method don't result in duplicated entries in the browser"
	| aStringSize list |
	aStringSize _ aString size.
	list _ Set new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel |
			(class compiledMethodAt: sel) literalsDo: [ :aLiteral |
				((aLiteral isMemberOf: String) and: [ aLiteral size >= aStringSize ]) ifTrue: [
					(aLiteral includesSubString: aString) ifTrue: [
						list add: 
							(MethodReference new
								setStandardClass: class 
								methodSymbol: sel) ]]]]].
	^ list asArray sort! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 4/2/2020 13:09:20'!
browseVersions
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."
	| selector class |
	selector _ model selectedMessageName.
	(selector isNil or: [ MessageSet isPseudoSelector: selector ]) ifTrue: [
		^ VersionsBrowserWindow
			browseCommentOf: model selectedClass ].
	class _ model selectedClassOrMetaClass.
	^ VersionsBrowserWindow
		browseVersionsOf: (class compiledMethodAt: selector)
		class: model selectedClass
		meta: class isMeta
		category: (class organization categoryOfElement: selector)
		selector: selector! !

!methodRemoval: ChangeList #field:from:ifAbsentOrNil: stamp: 'jmv 4/2/2020 15:43:30'!
ChangeList removeSelector: #field:from:ifAbsentOrNil:!
!methodRemoval: ChangeList #overridenMethodReferenceFrom: stamp: 'jmv 4/2/2020 15:42:42'!
ChangeList removeSelector: #overridenMethodReferenceFrom:!
!methodRemoval: ChangeList #classCategoryFrom: stamp: 'jmv 4/2/2020 15:42:30'!
ChangeList removeSelector: #classCategoryFrom:!
!methodRemoval: ChangeList #priorMethodReferenceFrom: stamp: 'jmv 4/2/2020 15:43:37'!
ChangeList removeSelector: #priorMethodReferenceFrom:!
!methodRemoval: ChangeList #stampFrom: stamp: 'jmv 4/2/2020 15:43:08'!
ChangeList removeSelector: #stampFrom:!
