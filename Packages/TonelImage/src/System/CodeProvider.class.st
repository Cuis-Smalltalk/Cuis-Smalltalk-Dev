"
An ancestor class for all models which can show code.
"
Class {
	#name : 'CodeProvider',
	#superclass : 'TextProvider',
	#instVars : [
		'currentCompiledMethod',
		'contentsSymbol'
	],
	#classVars : [
		'CodeNormalizers'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'services' }
CodeProvider class >> addCodeNormalizer: aBlock [
	"aBlock should have one argument, the code to process,
	and should evaluate to the processed code
	CodeProvider addCodeNormalizer: [ :code :cls | code, 'This is an extra comment1' asSmalltalkComment ].
	CodeProvider addCodeNormalizer: [ :code :cls | cls compilerClass new format: code in: cls notifying: nil. ].
	CodeProvider addCodeNormalizer: [ :code :cls | code, 'This is an extra comment2' asSmalltalkComment ].
	""
	CodeProvider clearCodeNormalizers.
	"
	CodeNormalizers := CodeNormalizers
		ifNil: [ { aBlock } ]
		ifNotNil: [ CodeNormalizers copyWith: aBlock ].
]

{ #category : 'services' }
CodeProvider class >> clearCodeNormalizers [
	"Just forget them."

	CodeNormalizers := nil.

]

{ #category : 'services' }
CodeProvider class >> normalizeCodeForSaving: aTextOrString in: aClassOrMetaclass [
	"Adapt edited code to be suitable for saving to sources or changes files.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' for assignment, '^' for return, and any replacements of
	ASCII by Unicode symbols done by Shout.
	See also SHTextStylerST80 >> #privateConvertAssignmentsOffset:
	See also Clipboard >> #storeObject:
	See also Workspace >> #contentsForFileSaving"

	| answer |
	answer := aTextOrString asPlainString.
	answer := answer copyReplaceAll: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString` with: ':='.
	answer := answer copyReplaceAll: `Character smalltalkLeftArrow asString` with: ':='.
	answer := answer copyReplaceAll: `Character smalltalkUpArrow asString` with: '^'.
	"Apply code any normalization services. For example, try:
	CodeProvider addCodeNormalizer: [ :code :cls | code, ' 						This is an extra comment1' asSmalltalkComment ].
	CodeProvider addCodeNormalizer: [ :code :cls | cls compilerClass new format: code in: cls notifying: nil. ].
	CodeProvider addCodeNormalizer: [ :code :cls | code, ' 						This is an extra comment2' asSmalltalkComment ].
	""
	CodeProvider clearCodeNormalizers.
	"
	CodeNormalizers ifNotNil: [
		CodeNormalizers do: [ :each |
			answer := each value: answer value: aClassOrMetaclass ]].
	^answer.
]

{ #category : 'shout styling' }
CodeProvider >> aboutToStyleWith: anSHTextStyler [
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly."

	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
]

{ #category : 'contents' }
CodeProvider >> acceptedContentsChanged [

	self changed: #acceptedContents.
	self triggerEvent: #decorateButtons.
	self triggerEvent: #annotationChanged
]

{ #category : 'contents' }
CodeProvider >> acceptedStringOrText [
	"Answer the source code or documentation for the selected method"

	self showingByteCodes ifTrue: [
		^ self selectedBytecodes].

	^ self selectedMessage
]

{ #category : 'annotation' }
CodeProvider >> addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream [
	"add an annotation detailing the prior versions count"
	| versionsCount |

	versionsCount := VersionsBrowser versionCountForSelector: aSelector class: aClass.
	aStream nextPutAll: 
				((versionsCount > 1
					ifTrue:
						[versionsCount = 2 ifTrue:
							['1 prior version']
							ifFalse:
								[versionsCount printString, ' prior versions']]
					ifFalse:
						['no prior versions']))
]

{ #category : 'shout styling' }
CodeProvider >> allowsStylingFor: selector isTextDirty: hasUnsavedEdits [
	"Answer false if showing difs, to veto the styling."

	^self showingAnyKindOfDiffs not
]

{ #category : 'annotation' }
CodeProvider >> annotation [
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |

	((aSelector := self selectedMessageName) == nil or: [(aClass := self selectedClassOrMetaClass) == nil])
		ifTrue: [^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass
]

{ #category : 'annotation' }
CodeProvider >> annotationForClassCommentFor: aClass [
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the clas comment of the given class."

	| aStamp nonMeta |
	aStamp :=  (nonMeta := aClass theNonMetaClass) organization commentStamp.
	^ aStamp
		ifNil:
			[nonMeta name, ' has no class comment']
		ifNotNil:
			['class comment for ', nonMeta name,
				(aStamp = '<historical>'
					ifFalse:
						[' - ', aStamp]
					ifTrue:
						[''])]
]

{ #category : 'annotation' }
CodeProvider >> annotationForClassDefinitionFor: aClass [
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Class definition for ';
			nextPutAll: aClass name.
		(Preferences at: #classAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#instanceMethodsCount] -> [
					strm
						print: (aClass theNonMetaClass selectors size);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (aClass theMetaClass selectors size);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (aClass theNonMetaClass linesOfCode);
						nextPutAll: ' total lines of code' ].
				[#messageSendsCount] -> [
					strm
						print: (aClass theNonMetaClass messageSendsCount);
						nextPutAll: ' total message sends' ].
			}]].
]

{ #category : 'annotation' }
CodeProvider >> annotationForMessageCategory: aCategory ofClass: aClass [
	"Provide a line of content for an annotation pane, given that the receiver is pointing at a System Category (i.e. a group of classes)."
	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Message Category: ';
			nextPutAll: aCategory.
		(Preferences at: #messageCategoryAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#messagesCount] -> [
					strm
						print: (aClass messagesCountInCategory: aCategory);
						nextPutAll: ' total messages' ].
				[#messageSendsCount] -> [
					strm
						print: (aClass messageSendsCountInCategory: aCategory);
						nextPutAll: ' total message sends' ].
			}]].
]

{ #category : 'annotation' }
CodeProvider >> annotationForSelector: aSelector ofClass: aClass [
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"

	| method |
	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].

	method := aClass compiledMethodAt: aSelector ifAbsent: nil.
	^ String streamContents: [ :strm |
		(Preferences at: #methodAnnotations)
			do: [ :each |
				each caseOf: {
					[#firstComment] -> [
						strm nextPutAll: (aClass firstCommentAt: aSelector) ].
					[#masterComment] -> [
						strm nextPutAll: ((aClass supermostPrecodeCommentFor: aSelector) ifNil: ['']) ].
					[#documentation] -> [
						strm nextPutAll: ((aClass precodeCommentOrInheritedCommentFor: aSelector) ifNil: ['']) ].
					[#timeStamp] -> [ | stamp |
						stamp := self timeStamp.
						strm nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp'])].
					[#linesOfCode] -> [
						method notNil ifTrue: [
							strm
								print: method linesOfCode;
								nextPutAll: ' lines of code' ]].
					[#messageSendsCount] -> [
						method notNil ifTrue: [
							strm
								print: method messageSendsCount;
								nextPutAll: ' message sends' ]].
					[#messageCategory] -> [
						strm nextPutAll: (( aClass organization categoryOfElement: aSelector) ifNil: ['']) ].
					[#sendersCount] -> [ | sendersCount |
						sendersCount := Smalltalk numberOfSendersOf: aSelector.
						sendersCount := sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
						strm nextPutAll: sendersCount ].
					[#implementorsCount] -> [ | implementorsCount |
						implementorsCount := Smalltalk numberOfImplementorsOf: aSelector.
						implementorsCount := implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
						strm nextPutAll: implementorsCount ].
					[#priorVersionsCount] -> [
						self addPriorVersionsCountForSelector: aSelector ofClass: aClass to: strm].
					[#priorTimeStamp] -> [ | stamp |
						stamp := VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
						strm nextPutAll: 'prior timestamp: '; nextPutAll: (stamp ifNil: ['None']) ].
					[#packages] -> [
						method notNil ifTrue: [
							(CodePackage packageOfMethod: method methodReference ifNone: nil)
								ifNil: [ strm nextPutAll: 'in no package' ]
								ifNotNil: [ :codePackage |
									strm nextPutAll: 'in package '; nextPutAll: codePackage packageName ]]].
					[#library] -> [
						method notNil ifTrue: [
							(method propertyValueAt: #libraryName)
								ifNil: [ strm nextPutAll: 'in no library' ]
								ifNotNil: [ :libraryName |
									strm nextPutAll: 'in library '; nextPutAll: libraryName ]]].
					[#changeSets] -> [ | aList |
						aList := ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allChangeSets] -> [ | aList |
						aList := ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allBaseSystemChangeSets] -> [ | aList |
						aList := (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in base system change set']
									ifFalse: [strm nextPutAll: 'in base system change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no base system change set']].
					
				}]
			separatedBy: [ strm nextPutAll: self annotationSeparator ].
		(method notNil and: [method hasBreakpoint]) ifTrue: [
			strm nextPutAll: self annotationSeparator.
			strm nextPutAll: '[Breakpoint]' ]
	].
]

{ #category : 'annotation' }
CodeProvider >> annotationForSystemCategory: aCategory [
	"Provide a line of content for an annotation pane, given that the receiver is pointing at a System Category (i.e. a group of classes)."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'System Category: ';
			nextPutAll: aCategory.
		(Preferences at: #systemCategoryAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#classCount] -> [
					strm
						print: (SystemOrganization fullListAtCategoryNamed: aCategory) size;
						nextPutAll: ' classes' ].
				[#instanceMethodsCount] -> [
					strm
						print: (SystemOrganization instanceMethodCountOf: aCategory);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (SystemOrganization classMethodCountOf: aCategory);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (SystemOrganization linesOfCodeOf: aCategory);
						nextPutAll: ' total lines of code' ].
				[#messageSendsCount] -> [
					strm
						print: (SystemOrganization messageSendsCountOf: aCategory);
						nextPutAll: ' total message sends' ].
			}]].
]

{ #category : 'annotation' }
CodeProvider >> annotationSeparator [
	"Answer the separator to be used between annotations"

	^ ' ° '
]

{ #category : 'user interface support' }
CodeProvider >> autoCompleterClassFor: textGetter [
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass autoCompleterClass ].
	^SmalltalkCompleter
]

{ #category : 'categories' }
CodeProvider >> categoryFromUserWithPrompt: aPrompt for: aClass [
	"self new categoryFromUserWithPrompt: 'testing' for: SystemDictionary"

	^(MethodCategoriesPrompter 
		staringFrom: aClass 
		rejectingFirst: false) prompt: aPrompt ifNone: nil
]

{ #category : 'categories' }
CodeProvider >> categoryOfCurrentMethod [
	"Answer the category that owns the current method.  If unable to determine a category, answer nil."

	^ self selectedClassOrMetaClass ifNotNil: [ :cls | 
		self selectedMessageName ifNotNil: [ :sel |
			cls whichCategoryIncludesSelector: sel]]
]

{ #category : 'categories' }
CodeProvider >> changeCategory [
	"Present a menu of the categories of messages for the current class, 
	and let the user choose a new category for the current message"

	self selectedClassOrMetaClass ifNotNil: [ :cls |
		self selectedMessageName ifNotNil: [ :sel |
			(self letUserReclassify: sel in: cls) ifTrue: [
				self methodCategoryChanged]]]
]

{ #category : 'contents' }
CodeProvider >> contentsSymbol [
	"Answer a symbol indicating what kind of content should be shown for the method; for normal showing of source code, this symbol is #source.  A nil value in the contentsSymbol slot will be set to #source by this method"

	^ contentsSymbol ifNil: [
		contentsSymbol := (Preferences at: #browseWithPrettyPrint)
					ifTrue:
						[#prettyPrint]
					ifFalse:
						[#source]]
]

{ #category : 'contents' }
CodeProvider >> contentsSymbol: aSymbol [
	"Set the contentsSymbol as indicated.  #source means to show source code, #comment means to show the first comment found in the source code"

	contentsSymbol := aSymbol.
	self acceptedContentsChanged
]

{ #category : 'message list menu' }
CodeProvider >> copySelector [
	"Copy the selected selector to the clipboard"

	| selector |
	(selector := self selectedMessageName) ifNotNil: [
		Clipboard storeObject: selector asPlainString]
]

{ #category : 'accessing' }
CodeProvider >> currentCompiledMethod [
	^currentCompiledMethod
]

{ #category : 'contents' }
CodeProvider >> currentMethodRefactored [

	self acceptedContentsChanged

]

{ #category : 'message list' }
CodeProvider >> decompiledSource [
	"Obtain a source string by decompiling the method's code.
	Also return the string."
	| class selector method answer |
	class := self selectedClassOrMetaClass.
	selector := self selectedMessageName.
	"Was method deleted while in another project?"
	method := class
		compiledMethodAt: selector
		ifAbsent: [ ^ '' ].
	currentCompiledMethod := method.
	"decompile without temp names "
	answer := (class decompilerClass new
		decompile: selector
		in: class
		method: method) decompileString.
	^ answer copy
]

{ #category : 'diffs' }
CodeProvider >> defaultDiffsSymbol [
	"Answer the code symbol to use when generically switching to diffing"

	^ (Preferences at: #diffsWithPrettyPrint)
		ifTrue: [
			#prettyLineDiffs]
		ifFalse: [
			#lineDiffs]
]

{ #category : 'self-updating' }
CodeProvider >> didCodeChangeElsewhere [
	"Determine whether the code for the currently selected method and class has been changed somewhere else."

	| aClass aSelector aCompiledMethod |
	currentCompiledMethod ifNil: [^ false].
	(aClass := self selectedClassOrMetaClass) ifNil: [^ false].
	(aSelector := self selectedMessageName) ifNil: [^ false].

	aSelector == #Comment ifTrue:
		[^ currentCompiledMethod ~~ aClass organization commentRemoteStr].
	^ ((aCompiledMethod := aClass compiledMethodAt: aSelector ifAbsent: [^ false]) ~~ currentCompiledMethod)
		and: [aCompiledMethod last ~= 0 "either not yet installed"
				or: [currentCompiledMethod last = 0 "or these methods don't have source pointers"]]
	
]

{ #category : 'diffs' }
CodeProvider >> diffFromPriorSourceFor: sourceCode  [
	"If there is a prior version of source for the selected method, return a diff, else just return the source code"

	^ self priorSourceOrNil
		ifNil: [ sourceCode ]
		ifNotNil: [ :prior |
			DifferenceFinder
				displayPatchFrom: prior to: sourceCode
				tryWords: self shouldDiffWords
				prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [self selectedClass])]
]

{ #category : 'evaluation' }
CodeProvider >> doItContext [
	"Answer the context in which a text selection can be evaluated."

	^nil
]

{ #category : 'evaluation' }
CodeProvider >> doItReceiver [
	^ nil
]

{ #category : 'user interface support' }
CodeProvider >> editorClassFor: textGetter [
	(#(acceptedContents classCommentText) statePointsTo: textGetter) ifFalse: [
		^super editorClassFor: textGetter ].
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass editorClass ].
	^SmalltalkEditor
]

{ #category : 'message list menu' }
CodeProvider >> fileOutMessage [

	self selectedMessageName ifNotNil: [
		^self selectedClassOrMetaClass fileOutMethod: self selectedMessageName].

	self messageList ifNotNil: [ :theMethods |
		DirectoryEntry fileOutsDirQuery: 'methods.st' writeStreamDo: [ :fileStream |
			fileStream writeSystemTimeStamp.
			theMethods do: [ :methodRef |
				methodRef actualClass
					printMethodChunk: methodRef methodSymbol
					withPreamble: true
					on: fileStream
					moveSource: false
					toFile: 0 ]]].
]

{ #category : 'message list menu' }
CodeProvider >> inspectCompiledMethod [
	"Open an Inspector on the CompiledMethod itself"

	self selectedMessageName ifNotNil: [
		(self selectedClassOrMetaClass compiledMethodAt: self selectedMessageName)
			inspect ]
]

{ #category : 'testing' }
CodeProvider >> is: aSymbol [
	^ aSymbol == #CodeProvider or: [ super is: aSymbol ]
]

{ #category : 'shout styling' }
CodeProvider >> isModeStyleable [
	"determine if Shout can style in the current mode"
	^ self showingSource or: [self showingPrettyPrint or: [self showingDecompile]]
]

{ #category : 'misc' }
CodeProvider >> isThereAnOverride [
	"Answer whether any subclass of my selected class implements my 
	selected selector"
	| aName aClass |
	aName := self selectedMessageName
				ifNil: [^ false].
	aClass := self selectedClassOrMetaClass ifNil: [ ^ false ].
	aClass allSubclassesDo: [ :cls | (cls includesSelector: aName) ifTrue: [ ^true ]].
	^ false
]

{ #category : 'misc' }
CodeProvider >> isThisAnOverride [
	"Answer whether any superclass of my selected class implements my selected selector"
	| aName aClass |
	aName := self selectedMessageName ifNil: [^ false].
	aClass := self selectedClassOrMetaClass ifNil: [^false ].
	aClass allSuperclassesDo: [ :cls | (cls includesSelector: aName) ifTrue: [ ^true ]].
	^ false
]

{ #category : 'categories' }
CodeProvider >> letUserReclassify: anElement in: aClass [
	"Put up a list of categories and solicit one from the user.  
	Answer true if user indeed made a change, else false"
	

	| currentCat newCat |
	currentCat := aClass organization categoryOfElement: anElement.
	newCat := self 
				categoryFromUserWithPrompt: 'choose category (currently "', currentCat, '")' 
				for: aClass.
	(newCat notNil and: [newCat ~= currentCat])
		ifTrue: [
			aClass organization classify: anElement under: newCat suppressIfDefault: false.
			^ true]
		ifFalse: [
			^ false]
]

{ #category : 'message list' }
CodeProvider >> messageList [
	"Answer the list of messages being displayed by the receiver, if any.
	 By default answer nil. Subclasses that present a list of messages override."

	^nil
]

{ #category : 'categories' }
CodeProvider >> methodCategoryChanged [
	self triggerEvent: #annotationChanged
]

{ #category : 'contents' }
CodeProvider >> methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock [

	^[ self selectedClassOrMetaClass methodNodeFor: aSourceCode ] on: Error, UndeclaredVariableReference do: aParsingErrorBlock
	
]

{ #category : 'misc' }
CodeProvider >> okayToAccept [
	"Answer whether it is okay to accept the receiver's input"

	self showingByteCodes ifTrue: [
		self inform: 
'Sorry, you can only submit changes here 
when you are showing source.'.
		^ false].

	self showingAnyKindOfDiffs ifTrue: [
		^ SelectionMenu confirm: 
'Caution!  You are "showing diffs" here, so 
there is a danger that some of the text in the
code pane is contaminated by the "diff" display'
		trueChoice: 'save anyway -- I''ll take my chances' falseChoice: 'um, let me reconsider' icons: #(acceptIcon cancelIcon)
	].

	^ true
]

{ #category : 'misc' }
CodeProvider >> priorSourceOrNil [
	"If the currently-selected method has a previous version, return its source, else return nil"
	| aClass aSelector  changeRecords |
	(aClass := self selectedClassOrMetaClass) ifNil: [^ nil].
	(aSelector := self selectedMessageName) ifNil: [^ nil].
	changeRecords := aClass changeRecordsAt: aSelector.
	(changeRecords == nil or: [changeRecords size <= 1]) ifTrue: [^ nil].
	^ (changeRecords at: 2) string 

]

{ #category : 'misc' }
CodeProvider >> releaseCachedState [
	"Can always be found again.  Don't write on a file."
	currentCompiledMethod := nil.
]

{ #category : 'commands' }
CodeProvider >> removeClass [
	"Remove the selected class from the system, at interactive user request.  Make certain the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| message  className classToRemove result |
	classToRemove := self selectedClassOrMetaClass ifNil: [Smalltalk beep. ^ false].
	classToRemove := classToRemove theNonMetaClass.
	className := classToRemove name.
	classToRemove subclasses notEmpty ifTrue: [
		self inform:
className, ' has subclasses.
Please remove them first.'.
			^ false ].
	message :=
'Are you certain that you
want to REMOVE the class ', className, '
from the system?'.
	(result := self confirm: message)
		ifTrue: [
			classToRemove removeFromSystem.
			self changed: #classList.
			true ].
	^ result.
]

{ #category : 'message list menu' }
CodeProvider >> revertToPreviousVersion [
	"Revert to the previous version of the current method"
	| aClass aSelector  changeRecords |
	aClass := self selectedClassOrMetaClass.
	aClass ifNil: [^ self changed: #flash].
	aSelector := self selectedMessageName.
	changeRecords := aClass changeRecordsAt: aSelector.
	(changeRecords == nil or: [changeRecords size <= 1]) ifTrue: [self changed: #flash.  ^ Smalltalk beep].
	changeRecords second fileIn.
	self acceptedContentsChanged
]

{ #category : 'message list' }
CodeProvider >> selectedBytecodes [
	"Answer text to show in a code pane when in showing-byte-codes mode"

	^ (self selectedClassOrMetaClass compiledMethodAt: self selectedMessageName ifAbsent: [^ '' asText]) symbolic asText
]

{ #category : 'accessing' }
CodeProvider >> selectedClass [
	^ nil
]

{ #category : 'accessing' }
CodeProvider >> selectedClassOrMetaClass [

	^ self selectedClass	"I don't know any better"
]

{ #category : 'message list' }
CodeProvider >> selectedMessage [
	"Answer a copy of the source code for the selected message.  This generic version is probably actually never reached, since every subclass probably reimplements and does not send to super.  In time, ideally, most, or all, reimplementors would vanish and all would defer instead to a universal version right here.  Everything in good time."

	| class selector method |

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	class := self selectedClassOrMetaClass.
	(class isNil or: [(selector := self selectedMessageName) isNil]) ifTrue: [^ ''].
	method := class compiledMethodAt: selector ifAbsent: [^ ''].	"method deleted while in another project"
	currentCompiledMethod := method.

	^ self sourceStringPrettifiedAndDiffed
			copy
]

{ #category : 'categories' }
CodeProvider >> selectedMessageCategoryName [
	"Answer the name of the message category of the message of the currently selected context."

	^ self selectedClass organization categoryOfElement: self selectedMessageName
]

{ #category : 'accessing' }
CodeProvider >> selectedMessageName [

	^ nil
]

{ #category : 'message list' }
CodeProvider >> selector: oldSelector refactoredTo: newSelector [

	"Used by the refactorings when a selector is renamed, a parameter is added, etc. - Hernan"

	self subclassResponsibility 
]

{ #category : 'diffs' }
CodeProvider >> shouldDiffWords [
	"Answer whether the receiver is currently set to use the word based differ"

	^ #(wordDiffs prettyWordDiffs) includes: self contentsSymbol
]

{ #category : 'what to show' }
CodeProvider >> showByteCodes [
	self contentsSymbol: #byteCodes
]

{ #category : 'what to show' }
CodeProvider >> showDecompile [
	self contentsSymbol: #decompile
]

{ #category : 'what to show' }
CodeProvider >> showLineDiffs [
	self contentsSymbol: #lineDiffs
]

{ #category : 'what to show' }
CodeProvider >> showPrettyLineDiffs [
	self contentsSymbol: #prettyLineDiffs
]

{ #category : 'what to show' }
CodeProvider >> showPrettyPrint [
	self contentsSymbol: #prettyPrint
]

{ #category : 'what to show' }
CodeProvider >> showPrettyWordDiffs [
	self contentsSymbol: #prettyWordDiffs
]

{ #category : 'what to show' }
CodeProvider >> showSource [
	self contentsSymbol: #source
]

{ #category : 'what to show' }
CodeProvider >> showWordDiffs [
	self contentsSymbol: #wordDiffs
]

{ #category : 'diffs' }
CodeProvider >> showingAnyKindOfDiffs [
	"Answer whether the receiver is currently set to show any kind of diffs"

	^ #(lineDiffs prettyLineDiffs wordDiffs prettyWordDiffs) includes: self contentsSymbol
]

{ #category : 'diffs' }
CodeProvider >> showingAnyKindOfPrettyDiffs [
	"Answer whether the receiver is currently set to show any kind of pretty diffs"

	^ #(prettyLineDiffs prettyWordDiffs) includes: self contentsSymbol
]

{ #category : 'what to show' }
CodeProvider >> showingByteCodes [
	"Answer whether the receiver is showing bytecodes"

	^ self contentsSymbol == #byteCodes
]

{ #category : 'what to show' }
CodeProvider >> showingByteCodesString [
	"Answer whether the receiver is showing bytecodes"

	^ self showingByteCodes asMenuItemTextPrefix,
		'byteCodes'
]

{ #category : 'what to show' }
CodeProvider >> showingDecompile [
	"Answer whether the receiver should show decompile rather than, say, source code"

	^ self contentsSymbol == #decompile

]

{ #category : 'what to show' }
CodeProvider >> showingDecompileString [
	"Answer a string characerizing whether decompilation is showing"

	^ self showingDecompile asMenuItemTextPrefix,
		'decompile'
]

{ #category : 'diffs' }
CodeProvider >> showingLineDiffs [
	"Answer whether the receiver is showing regular diffs of source code"

	^ self contentsSymbol == #lineDiffs

]

{ #category : 'diffs' }
CodeProvider >> showingLineDiffsString [
	"Answer a string representing whether I'm showing regular diffs"

	^ self showingLineDiffs asMenuItemTextPrefix,
		'lineDiffs'
]

{ #category : 'diffs' }
CodeProvider >> showingPrettyLineDiffs [
	"Answer whether the receiver is showing pretty diffs of source code"

	^ self contentsSymbol == #prettyLineDiffs
]

{ #category : 'diffs' }
CodeProvider >> showingPrettyLineDiffsString [
	"Answer a string representing whether I'm showing pretty diffs"

	^ self showingPrettyLineDiffs asMenuItemTextPrefix,
		'linePrettyDiffs'
]

{ #category : 'what to show' }
CodeProvider >> showingPrettyPrint [
	"Answer whether the receiver is showing pretty-print"

	^ self contentsSymbol == #prettyPrint
]

{ #category : 'what to show' }
CodeProvider >> showingPrettyPrintString [
	"Answer whether the receiver is showing pretty-print"

	^ self showingPrettyPrint asMenuItemTextPrefix,
		'prettyPrint'
]

{ #category : 'diffs' }
CodeProvider >> showingPrettyWordDiffs [
	"Answer whether the receiver is showing pretty diffs of source code"

	^ self contentsSymbol == #prettyWordDiffs

]

{ #category : 'diffs' }
CodeProvider >> showingPrettyWordDiffsString [
	"Answer a string representing whether I'm showing pretty diffs"

	^ self showingPrettyWordDiffs asMenuItemTextPrefix,
		'wordPrettyDiffs'
]

{ #category : 'what to show' }
CodeProvider >> showingSource [
	"Answer whether the receiver is currently showing source code"

	^ self contentsSymbol == #source

]

{ #category : 'what to show' }
CodeProvider >> showingSourceString [
	"Answer a string telling whether the receiver is showing plain source"

	^ self showingSource asMenuItemTextPrefix,
		'source'
]

{ #category : 'diffs' }
CodeProvider >> showingWordDiffs [
	"Answer whether the receiver is showing regular diffs (alternative algorithm) of source code"

	^ self contentsSymbol == #wordDiffs

]

{ #category : 'diffs' }
CodeProvider >> showingWordDiffsString [
	"Answer a string representing whether I'm showing regular diffs"

	^ self showingWordDiffs asMenuItemTextPrefix,
		'wordDiffs'
]

{ #category : 'message list' }
CodeProvider >> sourceStringPrettifiedAndDiffed [
	"Answer a copy of the source code for the selected message, transformed by diffing and pretty-printing exigencies"
	| class selector sourceString |
	class := self selectedClassOrMetaClass.
	selector := self selectedMessageName.
	(class isNil or: [ selector isNil ]) ifTrue: [ ^ 'missing' ].
	sourceString := class
		ultimateSourceCodeAt: selector
		ifAbsent: [ ^ 'error' ].
	(self showingPrettyPrint or: [ self showingAnyKindOfPrettyDiffs ]) ifTrue: [
		sourceString := class compilerClass new
			format: sourceString
			in: class
			notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		sourceString := self diffFromPriorSourceFor: sourceString ].
	^ sourceString
]

{ #category : 'shout styling' }
CodeProvider >> styleByParagraphs [
	"Answer true if each paragraph should be styled independent of the others.
	False, for instance, in all code Browsers.
	True in Workspaces and Inspectors, where the whole contents might not be valid Smalltalk.
	Note that this precludes multi paragraph comments. Multiple comments are required in such cases."

	^false
]

{ #category : 'user interface support' }
CodeProvider >> textStylerClassFor: textGetter [

	(Preferences at: #syntaxHighlightingAsYouType)
		ifFalse: [ ^nil ].
	textGetter = #acceptedContents ifFalse: [
		^super textStylerClassFor: textGetter ].
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass textStylerClass ].
	^SHTextStylerST80
]

{ #category : 'message list menu' }
CodeProvider >> timeStamp [
	"Answer the time stamp for the chosen class and method, if any, else an empty string"

	|  selector  |
	(selector := self selectedMessageName) ifNotNil: [
		^self selectedClassOrMetaClass 
			ifNil: [
				String new]
			ifNotNil: [
				self selectedClassOrMetaClass stampAt: selector]].
	^ String new
]

{ #category : 'diffs' }
CodeProvider >> toggleLineDiffing [
	"Toggle whether regular-diffing should be shown in the code pane"

	self showingLineDiffs
		ifFalse: [self showLineDiffs]
		ifTrue: [self showSource]
]

{ #category : 'diffs' }
CodeProvider >> togglePrettyLineDiffing [
	"Toggle whether pretty-diffing should be shown in the code pane"

	self showingPrettyLineDiffs
		ifFalse: [self showPrettyLineDiffs]
		ifTrue: [self showSource]
]

{ #category : 'diffs' }
CodeProvider >> togglePrettyWordDiffing [
	"Toggle whether pretty-diffing should be shown in the code pane"

	self showingPrettyWordDiffs
		ifFalse: [self showPrettyWordDiffs]
		ifTrue: [self showSource]
]

{ #category : 'diffs' }
CodeProvider >> toggleWordDiffing [
	"Toggle whether regular-diffing should be shown in the code pane"

	self showingWordDiffs
		ifFalse: [self showWordDiffs]
		ifTrue: [self showSource]
]

{ #category : 'message list menu' }
CodeProvider >> unusedMethods [
	| classes unsent messageList cls |

	(cls := self selectedClass) ifNil: [^ nil].
	classes := Array with: cls with: cls class.
	unsent := Set new.
	classes do: [:c | unsent addAll: c selectors].
	unsent := Smalltalk allUnSentMessagesIn: unsent.
	messageList := OrderedCollection new.
	classes do: [:c | (c selectors select: [:s | unsent includes: s]) asArray sort
					do: [:sel | messageList add: 
						(MethodReference class: c selector: sel) ]].
	^messageList
]

{ #category : 'self-updating' }
CodeProvider >> updateIfNeeded [
	self didCodeChangeElsewhere
		ifTrue: [
			self acceptedContentsChanged]
]

{ #category : 'controls' }
CodeProvider >> whatToShowSubmenuSpec [
	" 'What to Show' SubMenu"
	^{
		{3010. 	'-----'}.
		{3020.	 self showingSourceString. 			nil. 	#codeProvider. 	#showSource. 				nil. 'the textual source code as written'}.
		{3030. 	'-----'}.
		{3040.	self showingPrettyPrintString. 			nil. 	#codeProvider. 	#showPrettyPrint. 			nil. 'the method source presented in a standard text format'}.
		{3050. 	'-----'}.
		{3060.	self showingLineDiffsString. 			nil. 	#codeProvider. 	#showLineDiffs. 			nil. 'the textual source lines diffed from its prior version'}.
		{3070.	self showingWordDiffsString. 			nil. 	#codeProvider. 	#showWordDiffs. 			nil. 'the textual source words diffed from its prior version'}.
		{3080.	self showingPrettyLineDiffsString. 		nil. 	#codeProvider. 	#showPrettyLineDiffs. 		nil. 'formatted source lines diffed from formatted prior version'}.
		{3090.	self showingPrettyWordDiffsString. 	nil. 	#codeProvider. 	#showPrettyWordDiffs. 	nil. 'formatted source words diffed from formatted prior version'}.
		{3100. 	'-----'}.
		{3110.	self showingDecompileString. 			nil. 	#codeProvider. 	#showDecompile. 			nil. 'source code decompiled from byteCodes'}.
		{3120.	self showingByteCodesString. 			nil. 	#codeProvider. 	#showByteCodes. 			nil. 'the bytecodes that comprise the compiled method'}.
	}.

]
