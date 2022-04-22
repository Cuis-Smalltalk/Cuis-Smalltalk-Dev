'From Cuis 6.0 [latest update: #5109] on 22 April 2022 at 11:59:26 am'!

!String methodsFor: 'converting' stamp: 'jmv 4/21/2022 11:43:33'!
asPlainString
	"Answer the receiver itself.
	Not a Symbol, not a Text."
	^self! !


!Symbol methodsFor: 'converting' stamp: 'jmv 4/21/2022 11:43:54'!
asPlainString
	"Answer a plain String, not a Symbol."
	| newString |
	newString _ String new: self size.
	1 to: self size do: [:index | newString at: index put: (self at: index)].
	^newString! !


!Text methodsFor: 'converting' stamp: 'jmv 4/21/2022 11:44:03'!
asPlainString
	"Answer a plain String, not a Text."
	^string! !


!Object methodsFor: 'accessing' stamp: 'jmv 4/21/2022 12:12:08'!
addInstanceVarNamed: aName withValue: aValue
	"Add an instance variable named aName and give it value aValue"
	self class addInstVarName: aName asPlainString.
	self instVarAt: self class instSize put: aValue! !

!Object methodsFor: 'error handling' stamp: 'jmv 4/21/2022 12:12:44'!
primitiveFailed: selector
	"Announce that a primitive has failed and there is no appropriate 
	Smalltalk code to run."

	self error: selector asPlainString, ' failed'! !

!Object methodsFor: 'system primitives' stamp: 'jmv 4/21/2022 12:12:17'!
className
	"Answer a string characterizing the receiver's class, for use in list views for example"

	^ self class name asPlainString! !

!Object methodsFor: 'system primitives' stamp: 'jmv 4/21/2022 12:12:34'!
instVarNamed: aString
	"Return the value of the instance variable in me with that name.  Slow and unclean, but very useful. "

	^ self instVarAt: (self class allInstVarNames indexOf: aString asPlainString)


! !

!Object methodsFor: 'system primitives' stamp: 'jmv 4/21/2022 12:12:38'!
instVarNamed: aString put: aValue
	"Store into the value of the instance variable in me of that name.  Slow and unclean, but very useful. "

	^ self instVarAt: (self class allInstVarNames indexOf: aString asPlainString) put: aValue
! !


!TextModel methodsFor: 'as yet unclassified' stamp: 'jmv 4/21/2022 12:12:51'!
saveOn: stream as: format
	"Saves the model to the given stream"
	stream binary.
	stream nextPutAll: self actualContents asPlainString.! !


!Workspace methodsFor: 'shout styling' stamp: 'jmv 4/21/2022 11:48:30'!
toggleStyling

	shouldStyle _ self shouldStyle not.
	actualContents _ actualContents asPlainString asText.
	self changed: #actualContents! !


!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 4/21/2022 12:13:05'!
copySelector
	"Copy the selected selector to the clipboard"

	| selector |
	(selector _ self selectedMessageName) ifNotNil: [
		Clipboard storeObject: selector asPlainString]! !


!Browser methodsFor: 'accessing' stamp: 'jmv 4/21/2022 11:55:18'!
contents: input notifying: aRequestor
	"The retrieved information has changed and its source must now be
	 updated. The information can be a variety of things, depending on
	 the list selections (such as templates for class or message definition,
	 methods) or the user menu commands (such as definition, comment).
	Answer the result of updating the source."

	| aString aText theClass |
	aString _ input asPlainString.
	aText _ input asText.
	editSelection == #editSystemCategories ifTrue: [ ^ self changeSystemCategories: aString ].
	self isEditingClass ifTrue: [
		[
			self defineClass: aString notifying: aRequestor 
		] on: RecompilationFailure do: [ :ex |
			self inform: ex messageText.
			^ false].
		^ true].
	editSelection == #editComment
		ifTrue: [
			theClass _ self selectedClass.
			theClass
				ifNil: [
					self inform: 'You must select a class
before giving it a comment.'.
					^ false].
			theClass comment: aText stamp: Utilities changeStamp.
			self changed: #classCommentText.
			^ true].
	editSelection == #editMessageCategories ifTrue: [ ^ self changeMessageCategories: aString ].
	editSelection == #editMessage | (editSelection == #newMessage)
		ifTrue: [
			^ self okayToAccept
				ifFalse:[ false ]
				ifTrue: [
					(self compileMessage: aText notifying: aRequestor)
						ifTrue: [ self triggerEvent: #annotationChanged ];
						yourself ]].
	editSelection == #none
		ifTrue: [
			self inform: 'This text cannot be accepted
in this part of the browser.'.
			^ false].
	self error: 'unacceptable accept'! !

!Browser methodsFor: 'class comment pane' stamp: 'jmv 4/21/2022 11:51:10'!
newClassComment: aText
	"The user has just entered aText.
	It may be all red (a side-effect of replacing the default comment), so remove the color if it is."
	| theClass |
	theClass _ self selectedClassOrMetaClass theNonMetaClass.
	theClass ifNotNil: [
		theClass classComment: aText asPlainString ].
	self changed: #classCommentText.
	^ true! !

!Browser methodsFor: 'class functions' stamp: 'jmv 4/21/2022 12:13:11'!
defineClass: defString notifying: aRequestor  
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aRequestor."
	| oldClass class newClassName defTokens keywdIx |
	oldClass _ self selectedClassOrMetaClass.
	defTokens _ defString findTokens: Character separators.
	keywdIx _ defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx _ defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName _ (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [ oldClass theNonMetaClass name asPlainString ~= newClassName ])
		and: [ Smalltalk includesKey: newClassName asSymbol ]) ifTrue: [
			"Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: (newClassName bold , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?'))
				ifFalse: [ ^ false ]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil: [ oldClass _ oldClass superclass ].
	class _ Compiler
				evaluate: defString
				notifying: aRequestor
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: [
				self changed: #systemCategoryRoots.
				self changed: #classList.
				self changed: #clearUserEdits.
				self setClass: class selector: nil.
				"self clearUserEditFlag; editClass."
				^ true ]
		ifFalse: [ ^ false ]! !


!MessageNames methodsFor: 'search' stamp: 'jmv 4/21/2022 12:13:33'!
searchString: aString
	"Take what the user typed and find all selectors containing it"

	searchString _ aString asPlainString.
	self changed: #relabel.
	selectorList _ nil.
	self changed: #selectorList.
	self changed: #messageList.
	^ true! !


!ProtocolBrowser methodsFor: 'accessing' stamp: 'jmv 4/21/2022 12:13:43'!
labelString
	"Answer the string for the window title"
	
	^ 'Protocol for: ', baseClass name, ' up to: ', (selectedName ifNil: [ ProtoObject name asPlainString ])! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 4/21/2022 12:13:48'!
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
		
	record markAsTest: (classDefinition beginsWith: TestCase name asPlainString).
	
	^record
! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 4/21/2022 12:13:56'!
compareToCurrentVersion
	"If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text"

	| change class s1 s2 differDesc diffWords |
	listIndex = 0
		ifTrue: [^ self].
	change _ changeList at: listIndex.
	((class _ change changeClass) notNil
			and: [class includesSelector: change methodSelector])
		ifTrue: [
			s1 _ (class sourceCodeAt: change methodSelector) asPlainString.
			s2 _ change string.
			s1 = s2
				ifTrue: [^ self inform: 'Exact Match'].
			diffWords _ self shouldDiffWords.
			differDesc _ diffWords
				ifTrue: [ 'Words']
				ifFalse: [ 'Lines'].
			(TextModel
				withText: (
					(DifferenceFinder
						displayPatchFrom: s1 to: s2
						tryWords: diffWords
						prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class]))
							font: Preferences standardCodeFont))
				openLabel: 'Comparison to Current Version: ', differDesc, 
					(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])]
		ifFalse: [self flash]! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 4/21/2022 12:13:58'!
selectUnchangedMethods
	"Selects all method definitions for which there is already a method in the current image, whose source is exactly the same.  9/18/96 sw"

	| change class |
	1 to: changeList size do: [ :i | 
		change _ changeList at: i.
		listSelections at: i put:
			((change changeType == #method and:
				[(class _ change changeClass) notNil]) and:
					[(class includesSelector: change methodSelector) and:
						[change string = (class sourceCodeAt: change methodSelector) asPlainString ]])].
	self changed: #allSelections.
	self changed: #annotation! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 4/21/2022 12:14:04'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	| w |
	Preferences logDebuggerStackToFile ifTrue: [
		Smalltalk
			logError: title
			inContext: context
			to: 'CuisDebug' ].
	w := UISupervisor newProcessIfUI: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	UISupervisor whenUIinSafeState: [
		[ | debugger |
		"In case an error in Morphic code got us here, ensure mouse focus has been released"
		true runningWorld ifNotNil: [ :rWorld |
			rWorld activeHand ifNotNil: [ :aHand |
				aHand releaseMouseFocus ]].
		debugger := self new
			process: process
			context: context.
		debugger interruptedProcessUI: w.
		bool
			ifTrue: [ debugger openFullMorphicLabel: title ]
			ifFalse: [
				PreDebugWindow
					open: debugger
					label: title
					message: nil ]]
			on: UnhandledError
			do: [ :exOuter | | errorDescription |
				errorDescription := 'Error while trying to open Debugger' , String newLineString , 'Orginal error: ' , title asPlainString , '.' , String newLineString , '	Debugger error: ' ,
					([ exOuter description ]
						on: UnhandledError
						do: [ :exInner |
							exInner return: 'a ' , exInner class printString ]) , ':'.
				self primitiveError: errorDescription ]].
	process suspend.! !


!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 4/21/2022 12:14:07'!
pattern: textOrStringOrNil

	textOrStringOrNil
		ifNil: [pattern _ '*']
		ifNotNil: [pattern _ textOrStringOrNil asPlainString].
	self updateFileList.
	^ true
! !

!FileList methodsFor: 'private' stamp: 'jmv 4/21/2022 11:51:15'!
put: aText
	"Private - put the supplied text onto the file"

	| nameUsed type |
	brevityState == #fullFile ifTrue: [
		directory // fileName writeStreamDo: [ :stream |
			nameUsed _ stream name.
			stream nextPutAll: aText asPlainString ].
		fileName = nameUsed
			ifTrue: [ acceptedContentsCache _ aText asPlainString]
			ifFalse: [self updateFileList].		"user chose a different name (instead of overwriting)"
		^ true  "accepted"].

	listIndex = 0 ifTrue: [
		self inform: 'No fileName is selected'.
		^ false  "failed"].
	type _ 'These'.
	brevityState = #briefFile ifTrue: [type _ 'Abbreviated'].
	brevityState = #briefHex ifTrue: [type _ 'Abbreviated'].
	brevityState = #fullHex ifTrue: [type _ 'Hexadecimal'].
	brevityState = #FileList ifTrue: [type _ 'Directory'].
	self inform: ('{1} contents cannot
meaningfully be saved at present.' format:{type}).
	^ false  "failed"
! !


!SimpleServiceEntry methodsFor: 'printing' stamp: 'jmv 4/21/2022 12:14:12'!
printOn: aStream

	aStream nextPutAll: self class name; nextPutAll: ': ('.
	self provider
		ifNotNil: [ aStream nextPutAll: provider printString].
	aStream nextPutAll: ' --- '. 
	self selector
		ifNotNil: [ aStream nextPutAll: selector asPlainString].
	aStream nextPut: $)

! !


!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 4/21/2022 12:15:27'!
firstCommentAt:  selector
	"Answer a string representing the first comment in the method associated with selector.  Return an empty string if the relevant source file is not available, or if the method's source code does not contain a comment.  Not smart enough to bypass quotes in string constants, but does map doubled quote into a single quote."

	| sourceString commentStart  pos nextQuotePos |

	sourceString _ (self sourceCodeAt: selector) asPlainString.
	sourceString size = 0 ifTrue: [^ ''].
	commentStart _ sourceString findString: '"' startingAt: 1.
	commentStart = 0 ifTrue: [^ ''].
	pos _ commentStart + 1.
	[(nextQuotePos _ sourceString findString: '"' startingAt: pos) = (sourceString findString: '""' startingAt: pos)]
		whileTrue:
			[pos _ nextQuotePos + 2].
	
	commentStart = nextQuotePos ifTrue: [^ ''].  "Must have been a quote in string literal"

	^ (sourceString copyFrom: commentStart + 1 to: nextQuotePos - 1) copyReplaceAll: '""' with: '"'


"Behavior firstCommentAt: #firstCommentAt:"! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 4/21/2022 12:15:31'!
standardMethodHeaderFor: aSelector
	| args |
	args _ (1 to: aSelector numArgs)	collect:[:i| 'arg', i printString].
	args size = 0 ifTrue:[^aSelector asPlainString].
	args size = 1 ifTrue:[^aSelector,' arg1'].
	^String streamContents:[:s|
		(aSelector findTokens: ':') with: args do: [ :tok :arg |
			s nextPutAll: tok; nextPutAll:': '; nextPutAll: arg; nextPutAll:' '.
		].
	].
! !


!ClassDescription methodsFor: 'accessing' stamp: 'jmv 4/21/2022 11:51:22'!
comment: aStringOrText
	"Set the receiver's comment to be the argument, aStringOrText."

	self theNonMetaClass classComment: aStringOrText asPlainString.! !

!ClassDescription methodsFor: 'accessing' stamp: 'jmv 4/21/2022 11:51:27'!
comment: aStringOrText stamp: aStamp
	"Set the receiver's comment to be the argument, aStringOrText."

	self theNonMetaClass classComment: aStringOrText asPlainString stamp: aStamp.! !

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 4/21/2022 11:51:05'!
compile: text classified: category withStamp: changeStamp notifying: requestor logSource: logSource
	| methodAndNode selector isExistingMethod |
	methodAndNode _ self basicCompile: text asPlainString notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector _ methodAndNode selector.
	isExistingMethod _ self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	logSource ifTrue: [
		self logMethodSource: text forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: methodAndNode 
		method inProtocol: category notifying: requestor.
	^ methodAndNode selector! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/21/2022 12:15:51'!
fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asPlainString, ' not found'].
	nameBody _ self name , '-' , (selector copyReplaceAll: ':' with: '').
	DirectoryEntry smalltalkImageDirectory // (nameBody asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream moveSource: false toFile: 0 ]! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/21/2022 12:16:07'!
printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.  If moveSource true, then also set the source code pointer of the method."
	| preamble method oldPos newPos sourceFile endPos |
	doPreamble 
		ifTrue: [preamble _ self name , ' methodsFor: ' ,
					(self organization categoryOfElement: selector) asPlainString printString]
		ifFalse: [preamble _ ''].
	method _ self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ outStream].

	((method fileIndex = 0
		or: [(SourceFiles at: method fileIndex) == nil])
		or: [(oldPos _ method filePosition) = 0])
	ifTrue: [
		"The source code is not accessible.  We must decompile..."
		preamble size > 0 ifTrue: [ outStream newLine; nextPut: $!!; nextChunkPut: preamble; newLine].
		outStream nextChunkPut: method decompileString]
	ifFalse: [
		sourceFile _ SourceFiles at: method fileIndex.
		preamble size > 0
			ifTrue:    "Copy the preamble"
				[outStream copyPreamble: preamble from: sourceFile at: oldPos]
			ifFalse:
				[sourceFile position: oldPos].
		"Copy the method chunk"
		fileIndex = 0 ifFalse: [
			outStream padTo: SourceFiles pointerScaleForWriting put: $  ].
		newPos _ outStream position.
		outStream copyMethodChunkFrom: sourceFile.
		moveSource ifTrue: [    "Set the new method source pointer"
			endPos _ outStream position.
			method checkOKToAdd: endPos - newPos at: newPos in: method fileIndex.
			method setSourcePosition: newPos inFile: fileIndex]].
	preamble size > 0 ifTrue: [ outStream nextChunkPut: ' ' ].
	^ outStream newLine! !

!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 4/21/2022 12:15:44'!
classThatDefinesInstanceVariable: instVarName
	(instanceVariables notNil and: [instanceVariables includes: instVarName asPlainString]) ifTrue: [^ self]. 
	^ superclass ifNotNil: [superclass classThatDefinesInstanceVariable: instVarName]! !

!ClassDescription methodsFor: 'private' stamp: 'jmv 4/21/2022 11:51:40'!
logMethodSource: aText forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp notifying: requestor
	| priorMethodOrNil overridenMethodOrNil |
	
	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.
	overridenMethodOrNil := self superclass ifNotNil: [ :aSuperclass | 
		aSuperclass lookupSelector: aCompiledMethodWithNode selector ].
	
	aCompiledMethodWithNode method 
		putSource: aText asPlainString
		fromParseNode: aCompiledMethodWithNode node
		class: self 
		category: category 
		withStamp: changeStamp 
		inFile: 2 
		priorMethod: priorMethodOrNil 
		overridesMethod: overridenMethodOrNil.! !


!Class class methodsFor: 'instance creation' stamp: 'jmv 4/21/2022 12:03:15'!
templateForSubclassOf: priorClassName category: systemCategoryName 
	"Answer an expression that can be edited and evaluated in order to define a new class, given that the class previously looked at was as given"

	^ priorClassName asPlainString, ' subclass: #NameOfSubclass
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''' , systemCategoryName asPlainString , ''''! !


!ClassBuilder class methodsFor: 'cleanup obsolete classes' stamp: 'jmv 4/21/2022 12:16:44'!
cleanupClassHierarchyFor: aClassDescription
	
	| myName mySuperclass |
	mySuperclass _ aClassDescription superclass.
	(self isReallyObsolete: aClassDescription) ifTrue: [
		
		"Remove class >>>from SystemDictionary if it is obsolete"
		myName _ aClassDescription name asPlainString.
		Smalltalk keys do: [:each | 
			(each asPlainString = myName and: [(Smalltalk at: each) == aClassDescription])
				ifTrue: [Smalltalk removeKey: each]].

		"Make class officially obsolete if it is not"
		(aClassDescription name asPlainString beginsWith: 'AnObsolete')
			ifFalse: [aClassDescription obsolete].

		aClassDescription isObsolete 
			ifFalse: [self error: 'Something wrong!!'].

		"Add class to obsoleteSubclasses of its superclass"
		mySuperclass
			ifNil: [self error: 'Obsolete subclasses of nil cannot be stored'].
		(mySuperclass obsoleteSubclasses includes: aClassDescription)
			ifFalse: [mySuperclass addObsoleteSubclass: aClassDescription].
	] ifFalse:[
		"check if superclass has aClassDescription in its obsolete subclasses"
		mySuperclass ifNil:[mySuperclass _ Class]. "nil subclasses"
		mySuperclass removeObsoleteSubclass: aClassDescription.
	].
	"And remove its obsolete subclasses if not actual superclass"
	aClassDescription obsoleteSubclasses do:[:obs|
		obs superclass == aClassDescription ifFalse:[
			aClassDescription removeObsoleteSubclass: obs]].
! !


!ContextPart methodsFor: 'debugger access' stamp: 'jmv 4/21/2022 12:18:44'!
errorReportOn: strm
	"Write a detailed error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report.  Suppress any errors while getting printStrings.  Limit the length."

	| cnt aContext startPos |
 	strm print: Date today; space; print: Time now; newLine.
	strm newLine.
	strm nextPutAll: 'VM: ';
		nextPutAll: Smalltalk platformName;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk vmVersion;
		newLine.
	strm nextPutAll: 'Image: ';
		nextPutAll: Smalltalk version;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString;
		nextPutAll: ']';
		newLine.
	strm newLine.
	
	"Note: The following is an open-coded version of ContextPart>>stackOfSize: since this method may be called during a low space condition and we might run out of space for allocating the full stack."
	cnt _ 0.  startPos _ strm position.
	aContext _ self.
	[aContext notNil and: [(cnt _ cnt + 1) < Debugger defaultDebugStackSize]] whileTrue: [
		aContext printDetails: strm.	"variable values"
		strm newLine.
		aContext _ aContext sender].

	strm newLine; nextPutAll: '--- The full stack ---'; newLine.
	aContext _ self.
	cnt _ 0.
	[aContext == nil] whileFalse: [
		cnt _ cnt + 1.
		cnt = Debugger defaultDebugStackSize ifTrue: [strm nextPutAll: ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'; newLine ].
		strm print: aContext; newLine.  "just class>>selector"	

		strm position > (startPos+40000) ifTrue: [strm nextPutAll: '...etc...'.
			^ self]. 	"exit early"
		cnt > 100 ifTrue: [strm nextPutAll: '-- and more not shown --'.  ^ self].
		aContext _ aContext sender].
! !


!CompiledMethod methodsFor: 'printing' stamp: 'jmv 4/21/2022 14:05:52'!
printPrimitiveOn: aStream
	"Print the primitive on aStream"
	| primIndex primDecl |
	(primIndex := self primitive) = 0 ifTrue:
		[^self].
	primIndex = 120 ifTrue: "External call spec"
		[^aStream print: (self literalAt: 1); newLine].
	aStream nextPutAll: '<primitive: '.
	primIndex = 117
		ifTrue: [
			primDecl := self literalAt: 1.
			(primDecl at: 2) asPlainString printOn: aStream.
			(primDecl at: 1) ifNotNil: [ :moduleName |
				aStream nextPutAll:' module: '.
				moduleName asPlainString printOn: aStream]]
		ifFalse: [
			aStream print: primIndex].
	self primitiveErrorVariableName ifNotNil: [ :primitiveErrorVariableName |
		aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>; newLine! !


!Text methodsFor: 'accessing' stamp: 'jmv 4/21/2022 12:23:33'!
findString: aString startingAt: start 
	"Answer the index of subString within the receiver, starting at position start.
	If the receiver does not contain subString, answer 0."

	^string findString: aString asPlainString startingAt: start! !

!Text methodsFor: 'accessing' stamp: 'jmv 4/21/2022 12:23:37'!
findString: aString startingAt: start caseSensitive: caseSensitive
	"Answer the index of subString within the receiver, starting at index 
	start. If the receiver does not contain subString, answer 0."

	^string findString: aString asPlainString startingAt: start caseSensitive: caseSensitive! !

!Text methodsFor: 'converting' stamp: 'jmv 4/21/2022 11:52:11'!
withCuisLineEndings
	"Answer a copy of myself in which all sequences of <CR><LF> or <CF> have been changed to <LF>"

	| newText wrongLineEnd |
	wrongLineEnd _ String crlfString detect: [ :char | (char = Character newLineCharacter) not ].
	(string includes: wrongLineEnd) ifFalse: [ ^self copy ].
	newText _ self copyReplaceAll: String crlfString with: String newLineString asTokens: false.
	(newText asPlainString includes: wrongLineEnd) ifFalse: [ ^newText ].
	^newText copyReplaceAll: (String with: wrongLineEnd) with: String newLineString asTokens: false.! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 4/21/2022 12:27:00'!
obsoleteClasses   "Smalltalk obsoleteClasses inspect"
	"NOTE:  Also try inspecting comments below"
	| obs c |
	obs _ OrderedCollection new.  Smalltalk garbageCollect.
	Metaclass allInstancesDo:
		[:m | c _ m soleInstance.
		(c notNil and: ['AnOb*' match: c name asPlainString])
			ifTrue: [obs add: c]].
	^ obs asArray

"Likely in a ClassDict or Pool...
(Association allInstances select: [:a | (a value isKindOf: Class) and: ['AnOb*' match: a value name]]) asArray
"
"Obsolete class refs or super pointer in last lit of a method...
| n l found |
Smalltalk browseAllSelect:
	[:m | found _ false.
	1 to: m numLiterals do:
		[:i | (((l _ m literalAt: i) isMemberOf: Association)
				and: [(l value isKindOf: Behavior)
				and: ['AnOb*' match: l value name]])
			ifTrue: [found _ true]].
	found]
"! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 4/21/2022 12:27:24'!
testFormatter2
	"Smalltalk testFormatter2"
	"Reformats the source for every method in the system, and then verifies that the order of source tokens is unchanged.
	The formatting used will be classic monochrome"
	| newCodeString badOnes n oldCodeString oldTokens newTokens |
	badOnes _ OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldCodeString _ (cls sourceCodeAt: selector) asPlainString.
					newCodeString _ cls compilerClass new
						format: oldCodeString
						in: cls
						notifying: nil.
					oldTokens _ oldCodeString findTokens: Character separators.
					newTokens _ newCodeString findTokens: Character separators.
					oldTokens = newTokens ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes
		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 4/21/2022 12:24:39'!
allMethodsSourceStringMatching: aString
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.
	Search the class comments also.
	Argument might include $*, that matches any subsequence.
	For example, try:
		ensure:*[*close*]
	"
	| list classCount adder |
	list _ Set new.
	adder _ [ :mrClass :mrSel |
		list add: (MethodReference class: mrClass selector: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size
		during: [ :barBlock |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				barBlock value: (classCount _ classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						(aString match: (cl sourceCodeAt: sel)) ifTrue: [
							adder
								value: cl
								value: sel ]].

					(aString match: cl organization classComment) ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 4/21/2022 12:24:43'!
allMethodsWithSourceString: aString matchCase: caseSensitive
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"
	| list classCount adder |
	list _ Set new.
	adder _ [ :mrClass :mrSel |
		list add: (MethodReference class: mrClass selector: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size
		during: [ :barBlock |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				barBlock value: (classCount _ classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						((cl sourceCodeAt: sel)
							findString: aString
							startingAt: 1
							caseSensitive: caseSensitive) > 0 ifTrue: [
							adder
								value: cl
								value: sel ]].
					(cl organization classComment
						findString: aString
						startingAt: 1
						caseSensitive: caseSensitive) > 0 ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 4/21/2022 13:55:07'!
allPrimitiveMethodsInCategories: aList
	"Answer an OrderedCollection of all the methods that are implemented by primitives in the given categories.  1/26/96 sw"

	| aColl method | 
	aColl _ OrderedCollection new: 200.
	self allBehaviorsDo: [ :aClass |
		(aList includes: (SystemOrganization categoryOfElement: aClass theNonMetaClass name asPlainString) asPlainString)
			ifTrue: [
				aClass selectorsDo: [ :sel | 
					method _ aClass compiledMethodAt: sel.
					method primitive ~= 0
						ifTrue: [
							aColl addLast: aClass name , ' ' , sel 
								, ' ' , method primitive printString]]]].
	^ aColl

"Smalltalk allPrimitiveMethodsInCategories: #('Collections-Streams' 'Files-Streams' 'Files-Abstract' 'Files-Macintosh')"! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 4/21/2022 12:26:01'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	Preferences autoNumberUserChanges ifTrue: [
		oldUserChanges _ Smalltalk defaultUserChangesName asFileEntry.
		oldUserChanges exists ifTrue: [
			directory _ oldUserChanges parent.
			oldUserChangesName _ directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: oldUserChangesName ]].
	Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine ].
	StartupStamp _ nil! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 4/21/2022 11:52:20'!
logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	self assureStartupStampLogged.
	aString _ aStringOrText asPlainString.
	aString firstNoBlankIndex = 0 ifTrue: [^ self].  "null doits confuse replay"
	
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].
	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			stream newLine; newLine.
			preambleOrNil ifNotNil: [
				stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			stream nextChunkPut: aString ]]! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/21/2022 12:24:47'!
browseClassCommentsWithString: aString
	"Smalltalk browseClassCommentsWithString: 'my instances' "
	"Launch a message list browser on all class comments containing aString as a substring."
	| caseSensitive suffix list |
	suffix _ (caseSensitive _ Sensor shiftPressed)
		ifTrue: [ ' (case-sensitive)' ]
		ifFalse: [ ' (use shift for case-sensitive)' ].
	list _ Set new.
	Smalltalk allClassesDo: [ :class |
		(class organization classComment
			findString: aString
			startingAt: 1
			caseSensitive: caseSensitive) > 0 ifTrue: [
			list add: (MethodReference class: class selector: #Comment) ]].
	^ self
		browseMessageList: list
		name: 'Class comments containing ', aString printString, suffix
		autoSelect: aString.! !


!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 4/21/2022 13:55:16'!
writeClassRename: newName was: oldName
	"Write a method that tells which modern class to map instances to."
	| oldVer sel code |

	oldVer _ self versionSymbol: (structures at: oldName).
	sel _ oldName asPlainString.
	sel at: 1 put: (sel at: 1) asLowercase.
	sel _ sel, oldVer.	"i.e. #rectangleoc4"

	code _ WriteStream on: (String new: 500).
	code nextPutAll: sel; newLine.
	code newLine; tab; nextPutAll: '^ ', newName.	"Return new class"

	self class compile: code contents classified: 'conversion'.

! !


!Exception methodsFor: 'exceptionDescription' stamp: 'jmv 4/21/2022 13:55:24'!
description
	
"Return a textual description of the exception."

	| desc |
	desc := self class name asPlainString.
	^ self messageText
		ifNil: [ desc]
		ifNotNil: [ :mt | desc , ': ' , mt ]! !


!MessageNotUnderstood methodsFor: 'exceptionBuilder' stamp: 'jmv 4/21/2022 13:53:45'!
messageText
	"Return an exception's message text."

	^ messageText
		ifNil: [
			message
				ifNil: [super messageText]
				ifNotNil: [message lookupClass printString , '>>' , message selector asPlainString]]! !


!Compiler methodsFor: 'public access' stamp: 'jmv 4/21/2022 12:09:54'!
evaluateMethod: method to: receiver logged: doLog profiled: doProfile
	
	"See evaluate:in:to:notifying:ifFail:logged:profiled:
	It does the same but without compiling because it recevies the result of the compilation 
	as the parameter method. 
	self should have compile method"

	| value toLog itsSelection itsSelectionString |
	
	"(jmv) Log before evaluating. This way, if the evaluation is halted by whatever reason, it is logged anyway"
	doLog ifTrue: [
		toLog _ ((requestor respondsTo: #selection)  
			and: [ (itsSelection _ requestor selection) notNil
			and: [ (itsSelectionString _ itsSelection asPlainString) isEmptyOrNil not ]])
				ifTrue: [ itsSelectionString ]
				ifFalse: [ sourceStream contents ].
		SystemChangeNotifier uniqueInstance evaluated: toLog context: context ].

	"Evaluate now."
	doProfile
		ifTrue: [
			TimeProfileBrowser onBlock: [
				value _ receiver
					withArgs: (context ifNil: [#()] ifNotNil: [{context}])
					executeMethod: method ]]
		ifFalse: [
			value _ receiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method ].

	^ value! !

!Compiler methodsFor: 'private' stamp: 'jmv 4/21/2022 12:06:13'!
from: textOrStream class: aClass context: aContext notifying: req

	sourceStream _ (textOrStream is: #Stream)
		ifTrue: [ textOrStream ]
		ifFalse: [ ReadStream on: textOrStream asPlainString ].
	class _ aClass.
	context _ aContext.
	requestor _ req! !


!Scanner methodsFor: 'public access' stamp: 'jmv 4/21/2022 14:07:18'!
scanMessageParts: sourceString
	"Return an array of the form (comment keyword comment arg comment keyword comment arg comment) for the message pattern of this method.  Courtesy of Ted Kaehler, June 1999"

	| coll nonKeywords |
	coll := OrderedCollection new.
	self scan: (ReadStream on: sourceString asPlainString).
	nonKeywords := 0.
	[tokenType == #doIt] whileFalse:
		[(currentComment == nil or: [currentComment isEmpty])
			ifTrue: [coll addLast: nil]
			ifFalse: [coll addLast: currentComment removeFirst.
				[currentComment isEmpty] whileFalse:
					[coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
		(token numArgs < 1 or: [token = #| and: [ coll size > 1 ] ])
			ifTrue: [(nonKeywords := nonKeywords + 1) > 1 ifTrue: [^ coll]]
						"done with header"
			ifFalse: [nonKeywords := 0].
		coll addLast: token.
		self scanToken].
	(currentComment == nil or: [currentComment isEmpty])
		ifTrue: [coll addLast: nil]
		ifFalse: [coll addLast: currentComment removeFirst.
			[currentComment isEmpty] whileFalse: [
				coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
	^ coll! !

!Scanner methodsFor: 'public access' stamp: 'jmv 4/21/2022 11:57:10'!
scanTokenPositionsIn: textOrString into: aBlock
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments, in textOrString."

	self initScannerForTokenization.
	source := (ReadStream on: textOrString asPlainString).
	self step.
	self step.
	self scanAllTokenPositionsInto: aBlock

	"| code |
	code := '       #( 1 2 #( 3 4 ))  16r123 123 123.0  ', (Scanner sourceCodeAt: #scanTokenPositionsIn:into:).
	Scanner new scanTokenPositionsIn: code into: [:start :end| Transcript cr; nextPut: $_; nextPutAll: (code copyFrom: start to: end); nextPut: $_; endEntry]"

	"CodeDiffBuilder buildDisplayPatchFrom:  (Scanner sourceCodeAt: #scanTokenPositionsIn:into:) to:  ((Scanner sourceCodeAt: #scanTokenPositionsIn:into:) copyReplaceAll: String crString with: '')"

	"CodeDiffBuilder buildDisplayPatchFrom:  'colorTable ^colorTable ifNil: [colorTable _ ST80ColorTable]' to:'colorTable ^colorTable ifNil: [colorTable _ ST80ColorTable]'"! !

!Scanner methodsFor: 'public access' stamp: 'jmv 4/21/2022 11:57:14'!
scanTokens: textOrString 
	"Answer an Array that has been tokenized as though the input text, 
	textOrString, had appeared between the array delimitors #( and ) in a 
	Smalltalk literal expression."

	self initScannerForTokenization.
	self scan: (ReadStream on: textOrString asPlainString).
	self scanLitVec.
	^token

	"Scanner new scanTokens: 'identifier keyword: 8r31 ''string'' .'"! !

!Scanner methodsFor: 'public access' stamp: 'jmv 4/21/2022 11:57:20'!
typedScanTokens: textOrString 
	"Answer an Array that has been tokenized with literals mapped to literals,
	 special characters mapped to symbols and variable names and keywords
	 to strings. This methiod accepts _ (underscore) as an assignment token
	 irrespective of whether the system prefers := as the assignment token."
	| s |
	self initScannerForTokenization.
	self scan: (ReadStream on: textOrString asPlainString).
	s := WriteStream on: (Array new: 16).
	[tokenType == #doIt] whileFalse:
		[(token == #- 
		  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: 
			[self scanToken.
			 token := token negated].
		s nextPut: token.
		self scanToken].
	^s contents

	"Scanner new typedScanTokens: (Scanner sourceCodeAt: #typedScanTokens:)"! !


!Parser methodsFor: 'private' stamp: 'jmv 4/21/2022 14:07:26'!
initPattern: aString notifying: req return: aBlock
	| result |
	self
		init: (ReadStream on: aString asPlainString)
		notifying: req
		failBlock: [^nil].
	encoder _ self.
	result _ aBlock value: (self pattern: false inContext: nil).
	encoder _ failBlock _ nil.  "break cycles"
	^result! !

!Parser methodsFor: 'scanning' stamp: 'jmv 4/21/2022 11:56:21'!
transformVerticalBarAndUpArrowIntoABinarySelector
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a serie of immediately following vertical bars, up arrows and a binary selector.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0.
	Colon $: can be used as binary, but '::' means Chain."
	| toMakeBinary |
	toMakeBinary _ #(verticalBar upArrow colon).
	"Special case: '::' is not a binary selector but the Chain operator"
	(hereType = #colon and: [tokenType = #colon]) ifTrue: [^ self ].
	(toMakeBinary identityIncludes: hereType) ifFalse: [
		^ self ].
	here := here asPlainString.
	hereType := #binary.
	[(toMakeBinary identityIncludes: tokenType) and: [hereMark + here size = mark]]
		whileTrue: [
			here := here , token asPlainString.
			hereEnd := hereEnd + 1.
			self scanToken].
	(tokenType == #binary and: [hereMark + here size = mark])
		ifTrue: [
			here := here asString , token.
			hereType := #binary.
			hereEnd := hereEnd + token size.
			self scanToken].! !


!MethodNode methodsFor: 'printing' stamp: 'jmv 4/21/2022 14:05:40'!
printPrimitiveOn: aStream
	"Print the primitive on aStream"
	| primDecl |
	primitive = 0 ifTrue:
		[^self].
	primitive = 120 ifTrue: "External call spec"
		[^aStream print: encoder literals first].
	aStream nextPutAll: '<primitive: '.
	primitive = 117
		ifTrue:
			[primDecl := encoder literals at: 1.
			 (primDecl at: 2) asPlainString printOn: aStream.
			 (primDecl at: 1) ifNotNil:
				[:moduleName|
				aStream nextPutAll:' module: '.
				moduleName asPlainString printOn: aStream]]
		ifFalse:
			[aStream print: primitive].
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>.
	((Smalltalk classNamed: #StackInterpreter) ifNil: [Smalltalk classNamed: #Interpreter]) ifNotNil:
		[:interpreterClass|
		 aStream nextPutAll: ' "', ((interpreterClass primitiveTable) at: primitive + 1), '" ']! !


!Editor methodsFor: 'menu messages' stamp: 'jmv 4/21/2022 11:52:46'!
pasteString
	"Paste the text's string from the shared buffer over the current selection and 
	redisplay if necessary.
	Pasting a string means using destination current attributes if appropriate."

	self replaceSelectionWith: self clipboardStringOrText asPlainString! !


!Editor class methodsFor: 'class initialization' stamp: 'jmv 4/21/2022 12:09:30'!
collectCmdShortcutsSpecUsing: anInitializationMessage

	| shortcutsSpec dynamicInitializationMessage |
	
	shortcutsSpec := self perform: anInitializationMessage.

	dynamicInitializationMessage := (self name asPlainString uncapitalized, anInitializationMessage asPlainString capitalized) asSymbol.
	(Smalltalk allClassesImplementing: dynamicInitializationMessage) do: [ :aClass | 
		shortcutsSpec := shortcutsSpec, (aClass soleInstance perform: dynamicInitializationMessage) ].

	^shortcutsSpec

! !


!DirectoryEntry methodsFor: 'actions-path' stamp: 'jmv 4/21/2022 12:07:53'!
/ pathString
	"Answer an instance of DirectoryEntry.
	If you want an instance of FileEntry, please call #//"
	^self concatPathComponentsAsDirectory: pathString asPathComponents! !

!DirectoryEntry methodsFor: 'actions-path' stamp: 'jmv 4/21/2022 12:07:59'!
// pathString
	"Answer an instance of FileEntry.
	If you want an instance of DirectoryEntry, please call #/"
	^self concatPathComponentsAsFile: pathString asPathComponents! !


!RemoteString methodsFor: 'private' stamp: 'jmv 4/21/2022 11:52:54'!
string: aStringOrText onFileNumber: fileNumber toFile: aFileStream
	"Store this as the receiver's text if source files exist."

	| position |
	fileNumber = 0 ifFalse: [
		aFileStream padTo: SourceFiles pointerScaleForWriting put: $  ].
	position _ aFileStream position.
	self fileNumber: fileNumber position: position.
	aFileStream nextChunkPut: aStringOrText asPlainString! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 4/21/2022 13:56:44'!
writeFeatureSpecOn: aStream
	| provides |
	provides _ featureSpec provides.
	aStream
		nextPut: $!!; 
		nextChunkPut: 'provides: ', provides name asPlainString printString, ' ', provides version printString, ' ', provides revision printString;
		newLine.
	featureSpec requires do: [ :requires |
		aStream
		nextPut: $!!; 
			nextChunkPut: 'requires: ', requires name asPlainString printString, ' ', requires minVersion printString, ' ', requires minRevision printString, ' ', requires maxVersion printString;
			newLine ]! !


!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 4/21/2022 13:56:14'!
packageFileName
	"Answer a package name based on feature name"

	^ name asPlainString, '.pck.st'! !

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 4/21/2022 13:56:56'!
addToLoad: toLoad withMyRequirements: requirements

	"Add self to OrderedCollection 'toLoad' before any requirements I have"
	| myRequirements |
	myRequirements := self requirements.
	toLoad do: [ :featureReq | 
		(myRequirements anySatisfy: [ :req | featureReq sameNameAs: req])
		ifTrue: [
			"Transcript show: ('adding ', self name asPlainString, ' before: ', featureReq name asPlainString); newLine."
			toLoad add: self before: featureReq.
			^ toLoad
		]
	].
	"Transcript show: ('adding ', self name asPlainString); newLine."
	toLoad addLast: self.
	^ toLoad
		! !


!Preferences class methodsFor: 'personalization' stamp: 'jmv 4/21/2022 13:57:07'!
compileHardCodedPref: prefName enable: aBoolean
	"Compile a method that returns a simple true or false (depending on the value of aBoolean) when Preferences is sent prefName as a message"

	self class compileSilently: (prefName asPlainString, '
	"compiled programatically -- return hard-coded preference value"
	^ ', aBoolean storeString) classified: 'hard-coded prefs'.
	SmalltalkCompleter addSelector: prefName.
	
"Preferences compileHardCodedPref: #testing enable: false"! !


!Morph methodsFor: 'meta-actions' stamp: 'jmv 4/21/2022 13:57:14'!
addEmbeddingMenuItemsTo: aMenu hand: aHandMorph
	| menu |
	menu _ MenuMorph new defaultTarget: self.
	self potentialEmbeddingTargets reverseDo: [:m | 
		menu 
			add: m class name asPlainString 
			targetHighlight: m 
			action: #addMorphFrontFromWorldPosition: 
			argumentList: {self}].
	aMenu ifNotNil:[
		menu submorphCount > 0 
			ifTrue:[aMenu add:'embed into' subMenu: menu].
	].
	^menu! !


!StringRequestMorph methodsFor: 'accessing' stamp: 'jmv 4/21/2022 11:53:20'!
response: aText
	"Sent when text pane accepts."
	response _ aText.
	validationBlock ifNotNil: [(validationBlock value: aText asPlainString) ifFalse: [self flash. ^ false]].
	[acceptBlock ifNotNil: [acceptBlock value: aText asPlainString]] ensure: [self delete].
	^ true! !

!StringRequestMorph methodsFor: 'initialization' stamp: 'jmv 4/21/2022 11:57:45'!
addTitle: aString
	| titleMorph s pp w |
	titleMorph _ BoxedMorph new noBorder.
	titleMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asPlainString linesDo: [ :line |
		s _ LabelMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 24) @ (pp y).
	self addMorphKeepMorphHeight: titleMorph.
	^titleMorph morphWidth! !


!SystemWindow methodsFor: 'printing' stamp: 'jmv 4/21/2022 14:07:36'!
printOn: aStream 
	aStream nextPutAll: labelString asPlainString! !


!FillInTheBlankMorph methodsFor: 'accessing' stamp: 'jmv 4/21/2022 11:53:24'!
response: aText
	"Sent when text pane accepts."

	response _ aText asPlainString.
	done _ true.
	^ true
! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 4/21/2022 11:57:39'!
addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w newMorph |
	
	newMorph _ BoxedMorph new noBorder.
	newMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asPlainString linesDo: [ :line | | font |
		font _ Preferences standardMenuFont.
		s _ LabelMorph new
			contents: line;
			font: (titleMorph
				ifNil: [ font bold ]
				ifNotNil: [ font italic ])..
		newMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ newMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	newMorph morphExtent: (w + 16) @ (pp y).
	titleMorph
		 ifNil: [
			titleMorph _ newMorph.
			self addMorphFront: titleMorph ]
		ifNotNil: [ self addMorphBack: newMorph ].
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !


!MVCMenuMorph class methodsFor: 'instance creation' stamp: 'jmv 4/21/2022 14:08:24'!
from: aPopupMenu title: titleStringOrNil
	"Answer a MenuMorph constructed from the given PopUpMenu. Used to simulate MVC-style menus in a Morphic-only world."

	| menu items lines selections labelString labelPlainString j emphasis |
	menu _ self new.
	titleStringOrNil ifNotNil: [
		titleStringOrNil isEmpty ifFalse: [menu addTitle: titleStringOrNil]].
	labelString _ aPopupMenu labelString.
	labelPlainString _ labelString asPlainString.
	items _ labelPlainString lines.
	(labelString is: #Text) ifTrue: [
		"Pass along text emphasis if present"
		j _ 1.
		items _ items collect: [ :item |
			j _ labelPlainString findString: item startingAt: j.
			emphasis _ TextEmphasis new emphasisCode: (labelString emphasisAt: j).
			item asText addAttribute: emphasis]].
	lines _ aPopupMenu lineArray.
	lines ifNil: [lines _ #()].
	menu cancelValue: 0.
	menu defaultTarget: menu.
	selections _ (1 to: items size) asArray.
	1 to: items size do: [ :i |
		menu add: (items at: i) target: menu action: #selectMVCItem: argument: (selections at: i) icon: (aPopupMenu iconAt: i).
		(lines includes: i) ifTrue: [menu addLine]].
	^ menu
! !


!HaloSpec methodsFor: 'printing' stamp: 'jmv 4/21/2022 13:54:03'!
printOn: aStream
	"Add a textual printout representing the receiver to a stream"

	super printOn: aStream.
	aStream nextPutAll: ' (', addHandleSelector asPlainString, ' ', iconSymbol asPlainString, ')'! !


!PseudoClass methodsFor: 'class' stamp: 'jmv 4/21/2022 14:08:33'!
comment: aString
	self classComment: aString asPlainString! !

!PseudoClass methodsFor: 'methods' stamp: 'jmv 4/21/2022 13:57:19'!
removeSelector: aSelector
	| catName |
	catName := self class removedCategoryName asPlainString.
	self organization addCategory: catName before: self organization categories first.
	self organization classify: aSelector under: catName.
	self sourceCodeAt: aSelector put:'methodWasRemoved' asText.! !


!TestCase class methodsFor: 'building suites' stamp: 'jmv 4/21/2022 13:57:23'!
buildSuite
	| suite |
	suite _ TestSuite new.
	^ self isAbstract
		ifTrue: [
			suite name: self name asPlainString.
			self allSubclasses
				do: [:each | each isAbstract
						ifFalse: [each addToSuiteFromSelectors: suite]].
			suite]
		ifFalse: [self addToSuiteFromSelectors: suite]! !

!TestCase class methodsFor: 'building suites' stamp: 'jmv 4/21/2022 13:57:33'!
buildSuiteFromMethods: testMethods 
	| suite |
	suite _ TestSuite new
				name: self name asPlainString;
				yourself.
	^self addToSuite: suite fromMethods: testMethods! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 4/21/2022 13:57:39'!
saveTo: aFileName
	"
	| st |
	st := SpaceTally new.
	st spaceTally: (Array with: EllipseMorph with: Point).
	st saveTo: 'spaceTally2'
	"

	DirectoryEntry smalltalkImageDirectory // aFileName forceWriteStreamDo: [ :stream |
		results do: [ :each |
				stream nextPutAll: each analyzedClassName asPlainString; 
						nextPutAll: ' '; nextPutAll: each codeSize printString; 
						nextPutAll: ' '; nextPutAll: each instanceCount printString; 
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; newLine ]]! !


!SpaceTallyItem methodsFor: 'printing' stamp: 'jmv 4/21/2022 13:57:44'!
printOn: aStream

	analyzedClassName
		ifNotNil: [ aStream nextPutAll: analyzedClassName asPlainString]. 
	aStream nextPutAll: ' ('.
	codeSize
		ifNotNil: [ aStream nextPutAll: 'code size: ' ;  nextPutAll: codeSize printString]. 
	instanceCount
		ifNotNil: [ aStream nextPutAll: ' instance count: ' ;  nextPutAll: instanceCount printString]. 
	spaceForInstances
		ifNotNil: [ aStream nextPutAll: ' space for instances: ' ;  nextPutAll: spaceForInstances printString]. 
	aStream nextPut: $).
	! !


!SHTextStylerST80 methodsFor: 'styling' stamp: 'jmv 4/21/2022 11:53:28'!
styleWorkspaceFrom: start to: end allowEmphasis: aBoolean
	"Style a copy of part of the model text. Then apply attributes to model text. Useful for Workspaces.
	Call #formatWorkspace: becore calling us.
	Note: classOrMetaClass is assumed to be nil.
	Note: after last call, do `textModel changed: #shoutStyled`"

	parser
		source: (formattedText asPlainString copyFrom: start to: end).
	parser parse.
	parser ranges ifNotNil: [ :ranges |
		ranges do: [ :range |
			(self attributesFor: range rangeType) ifNotNil: [ :attributes |
				attributes do: [ :each |
					"For Workspaces, avoid attributes that affect text layout: very large contents would be slow."
					(aBoolean or: [each emphasisCode noMask: 3]) ifTrue: [
						formattedText
							addAttribute: each
							from: range start +start-1
							to: range end +start-1 ]]]]].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 4/21/2022 11:44:19'!
parseSetWorkspace: aBoolean
	"Answer a collection of SHRanges by parsing aText.
	When formatting it is not necessary to set the workspace, and this can make the parse take less time, so aBoolean specifies whether the parser should be given the workspace"
	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace:
			(aBoolean ifTrue: [ workspace ]);
		classOrMetaClass: classOrMetaClass;
		source: formattedText asPlainString.
	parser parse.
	^ parser ranges.! !


!RemoveParameter methodsFor: 'initialization' stamp: 'jmv 4/21/2022 13:54:26'!
initializeNamed: aParameterToRemove
	ofKeywordAtIndex: aParameterIndex
	from: anOldSelector 
	creating: aNewSelector 
	implementors: implementorsCollection 
	senders: sendersCollection
	
	super initializeFrom: anOldSelector to: aNewSelector implementors: implementorsCollection senders: sendersCollection.

	parameterToRemove := aParameterToRemove.
	parameterIndex := aParameterIndex.
	senderReplacementString := newSelector isUnary ifTrue: [ newSelector asPlainString ] ifFalse: [ '' ].
	isLastParameter := oldSelector numArgs = parameterIndex 
	! !


!RenameGlobal class methodsFor: 'exceptions' stamp: 'jmv 4/21/2022 13:57:52'!
errorMessageForGlobalNotDefined: anOldName

	^ anOldName asPlainString, ' is not defined as global variable'! !


!SafelyRemoveClass class methodsFor: 'exceptions' stamp: 'jmv 4/21/2022 13:57:58'!
errorMessageForCanNotRemove: aClassToSafelyRemove dueToReferencesToAll: referenced

	^String streamContents: [ :stream |
		stream 
			nextPutAll: aClassToSafelyRemove name asPlainString;
			nextPutAll: ' can not be removed '.
	
		referenced size = 1 
			ifTrue: [ (referenced includes: aClassToSafelyRemove) 
				ifTrue: [ stream nextPutAll: 'because it has references' ]
				ifFalse: [ stream 
					nextPutAll: 'because it subclass, ';
					nextPutAll: referenced anyOne name asPlainString;
					nextPutAll: ', has references' ]]
			ifFalse: [ (referenced includes: aClassToSafelyRemove)
				ifTrue: [ stream 
					nextPutAll: 'due to references to: ';
					nextPutAll: referenced asCommaStringAnd ]
				ifFalse: [ stream
					nextPutAll: 'due to references to its subclasses: ';
					nextPutAll: referenced asCommaStringAnd ]]]! !

!SafelyRemoveClass class methodsFor: 'warnings' stamp: 'jmv 4/21/2022 13:58:02'!
warningMessageFor: aClassToSafelyRemove hasSubclasses: allSubclasses

	^String streamContents: [ :stream |
		stream nextPutAll: aClassToSafelyRemove name asPlainString.
		allSubclasses size = 1
			ifTrue: [ stream nextPutAll: ' has a subclass' ]
			ifFalse: [ stream 
					nextPutAll: ' has ';
					print:  allSubclasses size;
					nextPutAll: ' subclasses' ].
		stream nextPutAll: ' that will be removed'].
		
	! !


!RenameGlobalApplier methodsFor: 'refactoring - parameters request' stamp: 'jmv 4/21/2022 13:58:07'!
askNewName

	newName := self request: 'Enter new name:' initialAnswer: oldName asPlainString.
	newName := newName withBlanksTrimmed asSymbol.
	! !


!SafelyRemoveClassApplier methodsFor: 'refactoring - parameters request' stamp: 'jmv 4/21/2022 14:08:56'!
confirmationMessageText
	
	^'Are you sure you want to remove ', classToRemove name asPlainString, '?'! !

