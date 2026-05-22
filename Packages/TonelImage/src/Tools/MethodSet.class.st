"
I represent a query path of the retrieval result of making a query about methods in the system. The result is a set of methods, denoted by a message selector and the class in which the method was found. As a TextProvider, the string I represent is the source code of the currently selected method. I am typically viewed in a Message Set Browser consisting of a MessageListView and a BrowserCodeView.

Ivar messageList is a collection of MethodReferences.
Ivar selectedMessage is not as symbol (as in superclasses) but the selected MethodReference.
"
Class {
	#name : 'MethodSet',
	#superclass : 'CodeProvider',
	#instVars : [
		'selectedMessage',
		'messageList',
		'shouldAutoHighlight',
		'autoHighlightString',
		'autoHighlightAllOccurrences'
	],
	#category : 'Tools-Browser',
	#package : 'Tools',
	#tag : 'Browser'
}

{ #category : 'utilities' }
MethodSet class >> isPseudoSelector: aSelector [
	"Answer whether the given selector is a special marker"

	^ #(Comment Definition) statePointsTo: aSelector
]

{ #category : 'instance creation' }
MethodSet class >> messageList: anArray  [
	"Answer an instance of me with message list anArray."

	^self new initializeMessageList: anArray
]

{ #category : 'contents' }
MethodSet >> acceptedStringOrText [
	"Answer the contents of the receiver"
	^selectedMessage
		ifNil: [
			currentCompiledMethod := nil.
			'']
		ifNotNil: [
			self showingByteCodes
				ifTrue: [ self selectedBytecodes ]
				ifFalse: [ self selectedMessage ]]
]

{ #category : 'refactorings' }
MethodSet >> addAsSubclassResponsibilitySelector [

	AddAsSubclassResponsibilityApplier applyFrom: self.
]

{ #category : 'message list' }
MethodSet >> addMethodReference: aMethodReference [
	"Append a aMethodReference to the list.  Select the new item."

	messageList add: aMethodReference.
	self changed: #messageList.
	self messageListIndex: messageList size
]

{ #category : 'message list' }
MethodSet >> addMethodReference: aMethodReferenceToAdd ifIncluded: aBlockClosure [

	(messageList includes: aMethodReferenceToAdd)
		ifTrue: aBlockClosure
		ifFalse: [ self addMethodReference: aMethodReferenceToAdd ]
]

{ #category : 'refactorings' }
MethodSet >> addParameter [

	RefactoringApplier addParameterApplier applyFrom: self
	
]

{ #category : 'shout styling' }
MethodSet >> allowsStylingFor: selector isTextDirty: hasUnsavedEdits [
	"Answer true to allow styling to proceed, or false to veto the styling"

	^self isModeStyleable
]

{ #category : 'annotation' }
MethodSet >> annotation [
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass := self selectedClassOrMetaClass)
		ifNil: [ ^ ''].
	(aSelector := self selectedMessageName)
		ifNil: [ ^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass.
]

{ #category : 'shout styling' }
MethodSet >> autoHighlightAllOccurrences [
	"Should also highlight any and all occurrences of the string?"

	^ autoHighlightAllOccurrences.
]

{ #category : 'shout styling' }
MethodSet >> autoHighlightString [
	"Return the string to be automatically highlighted."

	^ autoHighlightString.
]

{ #category : 'shout styling' }
MethodSet >> autoHighlightString: aString allOccurrences: aBoolean [
	"Set the string to be automatically highlighted."

	shouldAutoHighlight := true.
	autoHighlightString := aString.
	autoHighlightAllOccurrences := aBoolean.
]

{ #category : 'refactorings' }
MethodSet >> changeKeywordOrder [
	
	ChangeKeywordsSelectorOrderApplier applyFrom: self.
]

{ #category : 'accessing' }
MethodSet >> contents: input notifying: aRequestor  [
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| codeString category selector class oldSelector |
	codeString := input string.
	self okayToAccept ifFalse: [^ false].
	selectedMessage ifNil: [^ false].
	class := selectedMessage actualClass.
	oldSelector := selectedMessage methodSymbol.
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue: [
		oldSelector == #Comment ifTrue: [
			class comment: codeString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Compiler
				evaluate: codeString
				notifying: aRequestor
				logged: true.
			self changed: #clearUserEdits.
 			^ false]].
	"Normal method accept"
	category := class organization categoryOfElement: oldSelector.
	selector := class compile: codeString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	shouldAutoHighlight := true.
	self triggerEvent: #annotationChanged.
	^ true
]

{ #category : 'accessing' }
MethodSet >> contentsSelection [
	"Return the interval of text in the code pane to select when I set the pane's contents"

	^nil
]

{ #category : 'run tests' }
MethodSet >> debugMethodTest [
	
	currentCompiledMethod isTestMethod ifTrue: [
		currentCompiledMethod methodClass debugAsFailure: currentCompiledMethod selector ]

]

{ #category : 'message functions' }
MethodSet >> deleteFromMessageList: aMethodReference [
	"Delete the given message from the receiver's message list"

	messageList := messageList copyWithout: aMethodReference
]

{ #category : 'accessing' }
MethodSet >> doItReceiver [
	"This class's classPool has been jimmied to be the classPool of the class 
	being browsed. A doIt in the code pane will let the user see the value of 
	the class variables."

	^ self selectedClass
]

{ #category : 'message functions' }
MethodSet >> fileOutAllMethods [

	DirectoryEntry fileOutsDirQuery: 'methods.st' writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self messageList do: [ :methodRef |
			methodRef actualClass
				printMethodChunk: methodRef methodSymbol
				withPreamble: true
				on: fileStream
				moveSource: false
				toFile: 0 ]].
]

{ #category : 'filtering' }
MethodSet >> filterFrom: aBlock [
	"Filter the receiver's list down to only those items that satisfy aBlock, which takes a class an a selector as its arguments."
	
	self setFilteredList: (messageList select: [ :methodReference |
		aBlock
			value: methodReference actualClass
			value: methodReference methodSymbol ])
]

{ #category : 'filtering' }
MethodSet >> filterToAnyChangeSet [
	"Filter down only to messages present in ANY change set"

	self filterFrom: [ :aClass :aSelector |
		ChangeSet doesAnyChangeSetHaveClass: aClass andSelector: aSelector ]
]

{ #category : 'filtering' }
MethodSet >> filterToCommentedMethods [
	"Filter the receiver's list down to only those items which have comments"

	self filterFrom:
		[:aClass :aSelector |
			(aClass selectors includes: aSelector) and:
						[(aClass firstPrecodeCommentFor: aSelector) isEmptyOrNil not]]
]

{ #category : 'filtering' }
MethodSet >> filterToCurrentAuthor [
	"Filter down only to messages with my initials as most recent author"

	| myInitials aMethod aTimeStamp |
	(myInitials := Utilities authorInitialsPerSe) ifNil: [^ self inform: 'No author initials set in this image'].
	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:			
				[aMethod := aClass compiledMethodAt: aSelector ifAbsent: nil.
				aMethod notNil and:
					[(aTimeStamp := aMethod authorAndStamp) notNil and:
						[aTimeStamp beginsWith: myInitials]]]]
]

{ #category : 'filtering' }
MethodSet >> filterToImplementorsOf [
	"Filter the receiver's list down to only those items with a given selector."

	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector |
						aSelector == aSymbol]]]
]

{ #category : 'filtering' }
MethodSet >> filterToMessagesInChangesFile [
	"Filter down only to messages whose source code risides in the Changes file.  This allows one to ignore long-standing methods that live in the .sources file."

	| cm |
	self filterFrom:
		[:aClass :aSelector |
			aClass notNil and: [aSelector notNil and:
				[(self class isPseudoSelector: aSelector) not and:
					[(cm := aClass compiledMethodAt: aSelector ifAbsent: nil) notNil and:
					[cm fileIndex ~= 1]]]]]
]

{ #category : 'filtering' }
MethodSet >> filterToMessagesInSourcesFile [
	"Filter down only to messages whose source code resides in the .sources file."

	| cm |
	self filterFrom: [:aClass :aSelector |
		(aClass notNil and: [aSelector notNil]) and:
			[(self class isPseudoSelector: aSelector) not and:
				[(cm := aClass compiledMethodAt: aSelector ifAbsent: nil) notNil and:
					[cm fileIndex = 1]]]]
]

{ #category : 'filtering' }
MethodSet >> filterToMessagesWithPriorVersions [
	"Filter down only to messages which have at least one prior version"

	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:
				[(self class isPseudoSelector: aSelector) not and:
					[(VersionsBrowser versionCountForSelector: aSelector class: aClass) > 1]]]
]

{ #category : 'filtering' }
MethodSet >> filterToMessagesWithoutPriorVersions [
	"Filter down only to messages which have no prior version stored"

	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:
				[(self class isPseudoSelector: aSelector) not and:
					[(VersionsBrowser versionCountForSelector: aSelector class: aClass) <= 1]]]
]

{ #category : 'filtering' }
MethodSet >> filterToNotAnyChangeSet [
	"Filter down only to messages present in NO change set"

	self filterFrom: [ :aClass :aSelector |
		(ChangeSet doesAnyChangeSetHaveClass: aClass andSelector: aSelector) not ]
]

{ #category : 'filtering' }
MethodSet >> filterToNotCurrentAuthor [
	"Filter down only to messages not stamped with my initials"

	| myInitials aMethod aTimeStamp |
	(myInitials := Utilities authorInitialsPerSe) ifNil: [^ self inform: 'No author initials set in this image'].
	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:			
				[aMethod := aClass compiledMethodAt: aSelector ifAbsent: nil.
				aMethod notNil and:
					[(aTimeStamp := aMethod authorAndStamp) isNil or:
						[(aTimeStamp beginsWith: myInitials) not]]]]
]

{ #category : 'filtering' }
MethodSet >> filterToNotImplementorsOf [
	"Filter the receiver's list down to only those items whose selector is NOT one solicited from the user."

	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector |
						aSelector ~~ aSymbol]]]
]

{ #category : 'filtering' }
MethodSet >> filterToNotSendersOf [
	"Filter the receiver's list down to only those items which do not send a given selector."
	
	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol |
				self filterFrom:
					[:aClass :aSelector | | aMethod |
						(aMethod := aClass compiledMethodAt: aSelector) isNil or:
							[(aMethod hasLiteralExplicitly: aSymbol) not]]]]
]

{ #category : 'filtering' }
MethodSet >> filterToSendersOf [
	"Filter the receiver's list down to only those items which send a given selector."
	
	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector | | aMethod |
						(aMethod := aClass compiledMethodAt: aSelector) notNil and:
							[aMethod hasLiteralExplicitly: aSymbol]]]]
]

{ #category : 'filtering' }
MethodSet >> filterToUncommentedMethods [
	"Filter the receiver's list down to only those items which lack comments"

	self filterFrom:
		[:aClass :aSelector |
			(aClass selectors includes: aSelector) and:
						[(aClass firstPrecodeCommentFor: aSelector) isEmptyOrNil]]
]

{ #category : 'filtering' }
MethodSet >> filterToUnsentMessages [
	"Filter the receiver's list down to only those items which have no  
	senders"
	self filterFrom: [ :aClass :aSelector |
		(Smalltalk isThereAReferenceTo: aSelector) not ].
]

{ #category : 'initialization' }
MethodSet >> initialize [

	super initialize.
	shouldAutoHighlight := false.
]

{ #category : 'private' }
MethodSet >> initializeMessageList: anArray [

	messageList := anArray.
	messageList isEmpty
		ifTrue: [ selectedMessage := nil ]
		ifFalse: [ self messageListIndex: 1 ]
]

{ #category : 'refactorings' }
MethodSet >> inlineMethod [

	RefactoringApplier inlineMethodApplier applyFrom: self
]

{ #category : 'testing' }
MethodSet >> isEditingMethod [

	^selectedMessage notNil and: [ selectedMessage selector ~~ #Comment ]
]

{ #category : 'testing' }
MethodSet >> isEditingNone [

	^selectedMessage isNil
]

{ #category : 'shout styling' }
MethodSet >> justDidAutoHighlight [
	"Don't do autoHighlight unless accepted contents are updated."
	shouldAutoHighlight := false
]

{ #category : 'accessing' }
MethodSet >> labelString [
	^'Filtered: [',
		messageList size printString,
		']'.
]

{ #category : 'message list' }
MethodSet >> messageList [
	"Answer the current list of messages."

	^messageList
]

{ #category : 'message list' }
MethodSet >> messageListIndex [
	"Answer the index of the selected message selector into the currently 
	selected message category."

	selectedMessage ifNil: [ ^0 ].
	^self messageList indexOf: selectedMessage
]

{ #category : 'message list' }
MethodSet >> messageListIndex: anInteger [

	"Set the index of the selected item to be anInteger."

	| list |
	
	list := self messageList.
	selectedMessage := (list isInBounds: anInteger) ifTrue: [ list at: anInteger ].
	shouldAutoHighlight := true.
	self changed: #messageListIndex.	 "update my selection"
	self changed: #editSelection.
	self acceptedContentsChanged
]

{ #category : 'shout styling' }
MethodSet >> messageSendsRangesOf: aSelector [

	^ selectedMessage 
		ifNil: [ #() ]
		ifNotNil: [selectedMessage messageSendsRangesOf: aSelector ]
]

{ #category : 'class list' }
MethodSet >> metaClassIndicated [
	"Answer the boolean flag that indicates whether
	this is a class method."

	^ self selectedClassOrMetaClass isMeta
]

{ #category : 'message functions' }
MethodSet >> methodCategoryChanged [
	self triggerEvent: #annotationChanged
]

{ #category : 'refactorings' }
MethodSet >> moveToInstanceOrClassMethod [

	MoveToInstanceOrClassMethodApplier applyFrom: self
]

{ #category : 'refactorings' }
MethodSet >> pushDownSelectorToOneSubclass [
	
	PushDownMethodToOneSubclassApplier applyFrom: self.
]

{ #category : 'refactorings' }
MethodSet >> pushDownSelectorToSubclasses [
	
	PushDownMethodToSubclassesApplier applyFrom: self.
]

{ #category : 'refactorings' }
MethodSet >> pushUpSelector [

	PushUpMethodApplier applyFrom: self.
]

{ #category : 'message functions' }
MethodSet >> reformulateList [
	"The receiver's messageList has been changed; rebuild it"

	self messageListIndex: 0.
	self initializeMessageList: messageList.
	self changed: #messageList.
	self changed: #messageListIndex.
	self acceptedContentsChanged
]

{ #category : 'accessing' }
MethodSet >> reformulateListNoting: newSelector [
	"A method has possibly been saved for the receiver with newSelector as its selector.
	If the receiver has a way of reformulating its message list, here is a chance for it to do so"

	^ self reformulateList
]

{ #category : 'message functions' }
MethodSet >> removeMessage [
	"Remove the selected message from the system. 1/15/96 sw"
	| messageName confirmation |
	selectedMessage ifNil: [ ^self ].
	messageName := self selectedMessageName.
	confirmation := Smalltalk confirmRemovalOf: messageName on: self selectedClassOrMetaClass.
	confirmation = 3
		ifTrue: [^ self].
	self selectedClassOrMetaClass removeSelector: messageName.
	self deleteFromMessageList: self selection.
	self reformulateList.
	confirmation = 2
		ifTrue: [Smalltalk browseAllCallsOn: messageName]
]

{ #category : 'message list' }
MethodSet >> removeMessageFromBrowser [
	"Remove the selected message from the browser."

	self removeMessageFromBrowserKeepingLabel.
	self changed: #relabel
]

{ #category : 'message list' }
MethodSet >> removeMessageFromBrowserKeepingLabel [

	selectedMessage ifNil: [ ^nil ].
	self deleteFromMessageList: self selection.
	self reformulateList.

]

{ #category : 'refactorings' }
MethodSet >> removeParameter [

	RefactoringApplier removeParameterApplier applyFrom: self
	
]

{ #category : 'refactorings' }
MethodSet >> renameSelector [

	RefactoringApplier renameSelectorApplier applyFrom: self
	
]

{ #category : 'run tests' }
MethodSet >> runMethodTest [
	
	| suite |
	
	suite := TestSuite forCompiledMethod: currentCompiledMethod.
	(ProgressiveTestRunner for: suite) value
]

{ #category : 'class list' }
MethodSet >> selectedClass  [
	"Return the base class for the current selection.  1/17/96 sw fixed up so that it doesn't fall into a debugger in a msg browser that has no message selected"

	^ self selectedClassOrMetaClass
		ifNotNil: [ :c | c theNonMetaClass]
]

{ #category : 'class list' }
MethodSet >> selectedClassOrMetaClass [
	"Answer the currently selected class (or metaclass)."

	^selectedMessage ifNotNil: [ :methodReference | methodReference actualClass ]
]

{ #category : 'message list' }
MethodSet >> selectedMessage [
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class := selectedMessage actualClass.
	selector := selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod := class organization commentRemoteStr.
			^ class smalltalkComment ].
		selector == #Definition ifTrue: [
			^ class definition ].].

	(class notNil and: [ class includesSelector: selector]) ifFalse: [
		currentCompiledMethod := nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod := class compiledMethodAt: selector ifAbsent: nil.

	^self sourceStringPrettifiedAndDiffed
]

{ #category : 'class list' }
MethodSet >> selectedMessageCategoryName  [
	"Answer the name of the selected message category or nil."
	"Ver si esta implementacion loca se usa. Si no se usa, eliminar todas (intentarlo!)"
	selectedMessage ifNil: [ ^nil ].
	^ self selectedClassOrMetaClass organization categoryOfElement: self selectedMessageName
]

{ #category : 'message list' }
MethodSet >> selectedMessageName [
	"Answer the name of the currently selected message."

	^selectedMessage ifNotNil: [ :methodReference | methodReference methodSymbol ]
]

{ #category : 'system category list' }
MethodSet >> selectedSystemCategory [
	
	"This message is needed when pressing ctrl+y to run all tests in the class category from a window like 	implementors, senders, etc."
	^nil
]

{ #category : 'private' }
MethodSet >> selection [
	"Answer the item in the list that is currently selected, or nil if no selection is present"

	^ selectedMessage
]

{ #category : 'message list' }
MethodSet >> selector: oldSelector refactoredTo: newSelector [

	self selectedMessageName = oldSelector ifTrue: [ self removeMessageFromBrowserKeepingLabel ]
]

{ #category : 'private' }
MethodSet >> setFilteredList: newList [
	"Establish newList as the new list if appropriate, and adjust the window title accordingly; if the new list is of the same size as the old, warn and do nothing"

	newList isEmpty
		ifTrue: [
			^ self inform: 'Nothing would be left in the list if you did that'].
	newList size = messageList size
		ifTrue: [
			^ self inform: 'That leaves the list unchanged'].
	self initializeMessageList: newList.
	self reformulateList.
	self changed: #relabel
]

{ #category : 'shout styling' }
MethodSet >> shouldAutoHighlight [
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^shouldAutoHighlight
]

{ #category : 'message list' }
MethodSet >> sortByClassHierarchy [
	"Sort the message-list by class / selector. List classes in hierarchical order."
	
	| aClass bClass classes classesAndPositions i |

	classes := Set new.
	messageList do: [ :methodReference |
		methodReference actualClass ifNotNil: [ :actualClass | classes add: actualClass ]].
	classesAndPositions := Dictionary new.
	i := 1.
	Smalltalk hierarchySorted: classes do: [ :each |
		classesAndPositions at: each put: i.
		i := i + 1 ].
	
	messageList := messageList sort: [ :a :b |
		(a classSymbol = b classSymbol and: [ a classIsMeta = b classIsMeta ])
			ifTrue: [
				"Same class (or metaclass). Class comment first, then sorted selectors."
				a methodSymbol == #Comment
					ifTrue: [ true ]
					ifFalse: [
						b methodSymbol == #Comment
							ifTrue: [ false ]
							ifFalse: [ a methodSymbol < b methodSymbol ]]]
			ifFalse: [
				"Different class / Metaclass."
				aClass := a actualClass.
				bClass := b actualClass.
				aClass isNil == bClass isNil
					ifTrue: [
						aClass isNil
							ifTrue: [a classSymbol < b classSymbol]
							ifFalse: [
								"Neither class is nil"
								a classIsMeta = b classIsMeta
									ifFalse: [a classIsMeta not] "Instance side first, class side then."
									ifTrue: [
										"Both class side, or both instance side. Sort by hierarchy."
										(classesAndPositions at: aClass) < (classesAndPositions at: bClass)]]]
					ifFalse: [ aClass isNil ]]].
	messageList do: [ :each |
		each indentLevel:
			(each actualClass theNonMetaClass allSuperclasses select: [ :c |
				classesAndPositions includesKey: c ]) size ].
	self changed: #messageList.
]

{ #category : 'message list' }
MethodSet >> sortByClassName [
	"Sort the message-list by class / selector"
	
	messageList := messageList sort: [ :a :b |
		a classSymbol = b classSymbol
			ifTrue: [
				a methodSymbol == #Comment
					ifTrue: [ true ]
					ifFalse: [
						b methodSymbol == #Comment
							ifTrue: [ false ]
							ifFalse: [
								a classIsMeta = b classIsMeta
									ifTrue: [ a methodSymbol < b methodSymbol ]
									ifFalse: [ a classIsMeta not ]	]]]
			ifFalse: [ a classSymbol < b classSymbol ]
		].
	messageList do: [ :each | each removeStringVersionPrefix ].
	self changed: #messageList.
]

{ #category : 'message list' }
MethodSet >> sortByDate [
	"Sort the message-list by date of time-stamp"

	| assocs aCompiledMethod aDate inOrder |
	assocs := messageList collect: [ :aRef |
		aDate := aRef methodSymbol == #Comment
			ifTrue: [
				aRef actualClass organization dateCommentLastSaved start]
			ifFalse: [
				aCompiledMethod := aRef actualClass compiledMethodAt: aRef methodSymbol ifAbsent: nil.
				aCompiledMethod ifNotNil: [aCompiledMethod dateAndTime]].
		aRef -> (aDate ifNil: [(Date fromString: '01/01/1996') start])].  "The dawn of Squeak history"
	inOrder := assocs asArray sort: [ :a :b | a value < b value].

	inOrder do: [ :each | each key prefixStringVersionWith: each value date yyyymmdd ].
	messageList := inOrder collect: [ :assoc | assoc key ].
	self changed: #messageList
]

{ #category : 'message list' }
MethodSet >> sortBySelector [
	"Sort the message-list by selector / class"
	
	messageList := messageList sort: [ :a :b |
		a methodSymbol == #Comment
			ifTrue: [ true ]
			ifFalse: [
				b methodSymbol == #Comment
					ifTrue: [ false ]
					ifFalse: [
						a methodSymbol = b methodSymbol
							ifTrue: [
								a classSymbol = b classSymbol
									ifTrue: [ a classIsMeta not ]
									ifFalse: [ a classSymbol < b classSymbol ]]
							ifFalse: [ a methodSymbol < b methodSymbol ]]]
		].
	messageList do: [ :each | each prefixStringVersionWith: each methodSymbol ].
	self changed: #messageList
]

{ #category : 'message list' }
MethodSet >> sortReverse [
	"Reverse the current sort order"
	messageList := messageList reversed .
	self changed: #messageList
]

{ #category : 'breakpoints' }
MethodSet >> toggleBreakOnEntry [
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self ].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[ selectedMethod uninstall ]
		ifFalse:
			[ (BreakingMethodWrapper on: selectedMethod) install ].
	self
		changed: #messageList;
		changed: #annotation
]
