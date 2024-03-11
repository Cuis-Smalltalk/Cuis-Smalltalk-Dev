'From Cuis6.3 [latest update: #6254] on 8 March 2024 at 2:57:02 pm'!
!classDefinition: #MethodSet category: #'Tools-Browser'!
CodeProvider subclass: #MethodSet
	instanceVariableNames: 'selectedMessage messageList shouldAutoHighlight autoHighlightString autoHighlightAllOccurrences'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!

!MethodSet commentStamp: '<historical>' prior: 0!
I represent a query path of the retrieval result of making a query about methods in the system. The result is a set of methods, denoted by a message selector and the class in which the method was found. As a TextProvider, the string I represent is the source code of the currently selected method. I am typically viewed in a Message Set Browser consisting of a MessageListView and a BrowserCodeView.

Ivar messageList is a collection of MethodReferences.
Ivar selectedMessage is not as symbol (as in superclasses) but the selected MethodReference.!


!MethodSet methodsFor: 'accessing' stamp: 'jmv 3/8/2024 14:50:57'!
contents: input notifying: aRequestor 
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
	^ true! !

!MethodSet methodsFor: 'accessing' stamp: 'jmv 3/8/2024 14:52:03'!
contentsSelection
	"Return the interval of text in the code pane to select when I set the pane's contents"

	^nil! !

!MethodSet methodsFor: 'accessing' stamp: 'jmv 3/8/2024 14:52:09'!
doItReceiver
	"This class's classPool has been jimmied to be the classPool of the class 
	being browsed. A doIt in the code pane will let the user see the value of 
	the class variables."

	^ self selectedClass! !

!MethodSet methodsFor: 'accessing' stamp: 'jmv 3/8/2024 14:50:57'!
labelString
	^'Filtered: [',
		messageList size printString,
		']'.! !

!MethodSet methodsFor: 'accessing' stamp: 'jmv 3/8/2024 14:50:57'!
reformulateListNoting: newSelector
	"A method has possibly been submitted for the receiver with newSelector as its selector; If the receiver has a way of reformulating its message list, here is a chance for it to do so"

	^ self reformulateList! !

!MethodSet methodsFor: 'annotation' stamp: 'jmv 3/8/2024 14:52:24'!
annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass := self selectedClassOrMetaClass)
		ifNil: [ ^ ''].
	(aSelector := self selectedMessageName)
		ifNil: [ ^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass.! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
addMethodReference: aMethodReference
	"Append a aMethodReference to the list.  Select the new item."

	messageList add: aMethodReference.
	self changed: #messageList.
	self messageListIndex: messageList size! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
addMethodReference: aMethodReferenceToAdd ifIncluded: aBlockClosure

	(messageList includes: aMethodReferenceToAdd)
		ifTrue: aBlockClosure
		ifFalse: [ self addMethodReference: aMethodReferenceToAdd ]! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
growable
	"Answer whether the receiver is capable of growing/shrinking dynamically"

	^ true! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
messageList
	"Answer the current list of messages."

	^messageList! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:53:02'!
messageListIndex
	"Answer the index of the selected message selector into the currently 
	selected message category."

	selectedMessage ifNil: [ ^0 ].
	^self messageList indexOf: selectedMessage! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
messageListIndex: anInteger

	"Set the index of the selected item to be anInteger."

	| list |
	
	list := self messageList.
	selectedMessage := (list isInBounds: anInteger) ifTrue: [ list at: anInteger ].
	shouldAutoHighlight := true.
	self changed: #messageListIndex.	 "update my selection"
	self changed: #editSelection.
	self acceptedContentsChanged! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
removeMessageFromBrowser
	"Remove the selected message from the browser."

	self removeMessageFromBrowserKeepingLabel.
	self changed: #relabel! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
removeMessageFromBrowserKeepingLabel

	selectedMessage ifNil: [ ^nil ].
	self deleteFromMessageList: self selection.
	self reformulateList.
! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class := selectedMessage actualClass.
	selector := selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod := class organization commentRemoteStr.
			^ class comment ].
		selector == #Definition ifTrue: [
			^ class definition ].].

	(class notNil and: [ class includesSelector: selector]) ifFalse: [
		currentCompiledMethod := nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod := class compiledMethodAt: selector ifAbsent: nil.
	self showingDocumentation ifTrue: [
		^ self commentContents ].

	^self sourceStringPrettifiedAndDiffed! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
selectedMessageName
	"Answer the name of the currently selected message."

	^selectedMessage ifNotNil: [ :methodReference | methodReference methodSymbol ]! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
sortByClassHierarchy
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
		(a classSymbol = b classSymbol and: [ b classIsMeta = b classIsMeta ])
			ifTrue: [
				a methodSymbol == #Comment
					ifTrue: [ true ]
					ifFalse: [
						b methodSymbol == #Comment
							ifTrue: [ false ]
							ifFalse: [ a methodSymbol < b methodSymbol ]]]
			ifFalse: [
				aClass := a actualClass.
				bClass := b actualClass.
				aClass isNil == bClass isNil
					ifTrue: [
						aClass isNil
							ifTrue: [a classSymbol < b classSymbol]
							ifFalse: [(classesAndPositions at: aClass) < (classesAndPositions at: bClass)]]
					ifFalse: [aClass isNil]]].
	messageList do: [ :each |
		each indentLevel:
			(each actualClass theNonMetaClass allSuperclasses select: [ :c |
				classesAndPositions includesKey: c ]) size ].
	self changed: #messageList.! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
sortByClassName
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
									ifFalse: [ a classIsMeta ]	]]]
			ifFalse: [ a classSymbol < b classSymbol ]
		].
	messageList do: [ :each | each removeStringVersionPrefix ].
	self changed: #messageList! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
sortByDate
	"Sort the message-list by date of time-stamp"

	| assocs aCompiledMethod aDate inOrder |
	assocs := messageList collect: [ :aRef |
		aDate := aRef methodSymbol == #Comment
			ifTrue: [
				aRef actualClass organization dateCommentLastSubmitted]
			ifFalse: [
				aCompiledMethod := aRef actualClass compiledMethodAt: aRef methodSymbol ifAbsent: nil.
				aCompiledMethod ifNotNil: [aCompiledMethod dateMethodLastSubmitted]].
		aRef -> (aDate ifNil: [Date fromString: '01/01/1996'])].  "The dawn of Squeak history"
	inOrder := assocs asArray sort: [ :a :b | a value < b value].

	inOrder do: [ :each | each key prefixStringVersionWith: each value yyyymmdd ].
	messageList := inOrder collect: [ :assoc | assoc key ].
	self changed: #messageList! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
sortBySelector
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
									ifTrue: [ a classIsMeta ]
									ifFalse: [ a classSymbol < b classSymbol ]]
							ifFalse: [ a methodSymbol < b methodSymbol ]]]
		].
	messageList do: [ :each | each prefixStringVersionWith: each methodSymbol ].
	self changed: #messageList! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:50:56'!
sortReverse
	"Reverse the current sort order"
	messageList := messageList reversed .
	self changed: #messageList! !

!MethodSet methodsFor: 'message functions' stamp: 'jmv 3/8/2024 14:50:56'!
deleteFromMessageList: aMethodReference
	"Delete the given message from the receiver's message list"

	messageList := messageList copyWithout: aMethodReference! !

!MethodSet methodsFor: 'message functions' stamp: 'jmv 3/8/2024 14:50:57'!
methodCategoryChanged
	self triggerEvent: #annotationChanged! !

!MethodSet methodsFor: 'message functions' stamp: 'jmv 3/8/2024 14:50:57'!
reformulateList
	"The receiver's messageList has been changed; rebuild it"

	self messageListIndex: 0.
	self initializeMessageList: messageList.
	self changed: #messageList.
	self changed: #messageListIndex.
	self acceptedContentsChanged! !

!MethodSet methodsFor: 'message functions' stamp: 'jmv 3/8/2024 14:50:57'!
removeMessage
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
		ifTrue: [Smalltalk browseAllCallsOn: messageName]! !

!MethodSet methodsFor: 'class list' stamp: 'jmv 3/8/2024 14:50:57'!
metaClassIndicated
	"Answer the boolean flag that indicates whether
	this is a class method."

	^ self selectedClassOrMetaClass isMeta! !

!MethodSet methodsFor: 'class list' stamp: 'jmv 3/8/2024 14:50:57'!
selectedClass 
	"Return the base class for the current selection.  1/17/96 sw fixed up so that it doesn't fall into a debugger in a msg browser that has no message selected"

	^ self selectedClassOrMetaClass
		ifNotNil: [ :c | c theNonMetaClass]! !

!MethodSet methodsFor: 'class list' stamp: 'jmv 3/8/2024 14:50:57'!
selectedClassOrMetaClass
	"Answer the currently selected class (or metaclass)."

	^selectedMessage ifNotNil: [ :methodReference | methodReference actualClass ]! !

!MethodSet methodsFor: 'class list' stamp: 'jmv 3/8/2024 14:50:57'!
selectedMessageCategoryName 
	"Answer the name of the selected message category or nil."
	"Ver si esta implementacion loca se usa. Si no se usa, eliminar todas (intentarlo!!)"
	selectedMessage ifNil: [ ^nil ].
	^ self selectedClassOrMetaClass organization categoryOfElement: self selectedMessageName! !

!MethodSet methodsFor: 'contents' stamp: 'jmv 3/8/2024 14:50:57'!
acceptedStringOrText
	"Answer the contents of the receiver"
	^selectedMessage
		ifNil: [
			currentCompiledMethod := nil.
			'']
		ifNotNil: [
			self showingByteCodes
				ifTrue: [ self selectedBytecodes ]
				ifFalse: [ self selectedMessage ]]! !

!MethodSet methodsFor: 'breakpoints' stamp: 'jmv 3/8/2024 14:52:49'!
toggleBreakOnEntry
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self ].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[ selectedMethod uninstall ]
		ifFalse:
			[ BreakingMethodWrapper on: selectedMethod :: install ].
	self
		changed: #messageList;
		changed: #annotation! !

!MethodSet methodsFor: 'testing' stamp: 'jmv 3/8/2024 14:53:53'!
isEditingMethod

	^selectedMessage notNil! !

!MethodSet methodsFor: 'testing' stamp: 'jmv 3/8/2024 14:53:57'!
isEditingNone

	^selectedMessage isNil! !

!MethodSet methodsFor: 'private' stamp: 'jmv 3/8/2024 14:50:57'!
initializeMessageList: anArray

	messageList := anArray.
	messageList isEmpty
		ifTrue: [ selectedMessage := nil ]
		ifFalse: [ self messageListIndex: 1 ]! !

!MethodSet methodsFor: 'private' stamp: 'jmv 3/8/2024 14:50:57'!
selection
	"Answer the item in the list that is currently selected, or nil if no selection is present"

	^ selectedMessage! !

!MethodSet methodsFor: 'private' stamp: 'jmv 3/8/2024 14:50:57'!
setFilteredList: newList
	"Establish newList as the new list if appropriate, and adjust the window title accordingly; if the new list is of the same size as the old, warn and do nothing"

	newList isEmpty
		ifTrue: [
			^ self inform: 'Nothing would be left in the list if you did that'].
	newList size = messageList size
		ifTrue: [
			^ self inform: 'That leaves the list unchanged'].
	self initializeMessageList: newList.
	self changed: #relabel! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterFrom: aBlock
	"Filter the receiver's list down to only those items that satisfy aBlock, which takes a class an a selector as its arguments."
	
	self setFilteredList: (messageList select: [ :methodReference |
		aBlock
			value: methodReference actualClass
			value: methodReference methodSymbol ])! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToAnyChangeSet
	"Filter down only to messages present in ANY change set"

	self filterFrom: [ :aClass :aSelector |
		ChangeSet doesAnyChangeSetHaveClass: aClass andSelector: aSelector ]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToCommentedMethods
	"Filter the receiver's list down to only those items which have comments"

	self filterFrom:
		[:aClass :aSelector |
			(aClass selectors includes: aSelector) and:
						[(aClass firstPrecodeCommentFor: aSelector) isEmptyOrNil not]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToCurrentAuthor
	"Filter down only to messages with my initials as most recent author"

	| myInitials aMethod aTimeStamp |
	(myInitials := Utilities authorInitialsPerSe) ifNil: [^ self inform: 'No author initials set in this image'].
	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:			
				[aMethod := aClass compiledMethodAt: aSelector ifAbsent: nil.
				aMethod notNil and:
					[(aTimeStamp := aMethod timeStamp) notNil and:
						[aTimeStamp beginsWith: myInitials]]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToImplementorsOf
	"Filter the receiver's list down to only those items with a given selector."

	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector |
						aSelector == aSymbol]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToMessagesInChangesFile
	"Filter down only to messages whose source code risides in the Changes file.  This allows one to ignore long-standing methods that live in the .sources file."

	| cm |
	self filterFrom:
		[:aClass :aSelector |
			aClass notNil and: [aSelector notNil and:
				[(self class isPseudoSelector: aSelector) not and:
					[(cm := aClass compiledMethodAt: aSelector ifAbsent: nil) notNil and:
					[cm fileIndex ~= 1]]]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToMessagesInSourcesFile
	"Filter down only to messages whose source code resides in the .sources file."

	| cm |
	self filterFrom: [:aClass :aSelector |
		(aClass notNil and: [aSelector notNil]) and:
			[(self class isPseudoSelector: aSelector) not and:
				[(cm := aClass compiledMethodAt: aSelector ifAbsent: nil) notNil and:
					[cm fileIndex = 1]]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToMessagesWithPriorVersions
	"Filter down only to messages which have at least one prior version"

	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:
				[(self class isPseudoSelector: aSelector) not and:
					[(VersionsBrowser versionCountForSelector: aSelector class: aClass) > 1]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToMessagesWithoutPriorVersions
	"Filter down only to messages which have no prior version stored"

	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:
				[(self class isPseudoSelector: aSelector) not and:
					[(VersionsBrowser versionCountForSelector: aSelector class: aClass) <= 1]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToNotAnyChangeSet
	"Filter down only to messages present in NO change set"

	self filterFrom: [ :aClass :aSelector |
		(ChangeSet doesAnyChangeSetHaveClass: aClass andSelector: aSelector) not ]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToNotCurrentAuthor
	"Filter down only to messages not stamped with my initials"

	| myInitials aMethod aTimeStamp |
	(myInitials := Utilities authorInitialsPerSe) ifNil: [^ self inform: 'No author initials set in this image'].
	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:			
				[aMethod := aClass compiledMethodAt: aSelector ifAbsent: nil.
				aMethod notNil and:
					[(aTimeStamp := aMethod timeStamp) isNil or:
						[(aTimeStamp beginsWith: myInitials) not]]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToNotImplementorsOf
	"Filter the receiver's list down to only those items whose selector is NOT one solicited from the user."

	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector |
						aSelector ~~ aSymbol]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToNotSendersOf
	"Filter the receiver's list down to only those items which do not send a given selector."
	
	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol |
				self filterFrom:
					[:aClass :aSelector | | aMethod |
						(aMethod := aClass compiledMethodAt: aSelector) isNil or:
							[(aMethod hasLiteralThorough: aSymbol) not]]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToSendersOf
	"Filter the receiver's list down to only those items which send a given selector."
	
	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector | | aMethod |
						(aMethod := aClass compiledMethodAt: aSelector) notNil and:
							[aMethod hasLiteralThorough: aSymbol]]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToUncommentedMethods
	"Filter the receiver's list down to only those items which lack comments"

	self filterFrom:
		[:aClass :aSelector |
			(aClass selectors includes: aSelector) and:
						[(aClass firstPrecodeCommentFor: aSelector) isEmptyOrNil]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 3/8/2024 14:50:57'!
filterToUnsentMessages
	"Filter the receiver's list down to only those items which have no  
	senders"
	self filterFrom: [ :aClass :aSelector |
		(Smalltalk isThereAReferenceTo: aSelector) not ].! !

!MethodSet methodsFor: 'shout styling' stamp: 'jmv 3/8/2024 14:50:57'!
autoHighlightAllOccurrences
	"Should also highlight any and all occurrences of the string?"

	^ autoHighlightAllOccurrences.! !

!MethodSet methodsFor: 'shout styling' stamp: 'jmv 3/8/2024 14:50:57'!
autoHighlightString
	"Return the string to be automatically highlighted."

	^ autoHighlightString.! !

!MethodSet methodsFor: 'shout styling' stamp: 'jmv 3/8/2024 14:50:57'!
autoHighlightString: aString allOccurrences: aBoolean
	"Set the string to be automatically highlighted."

	shouldAutoHighlight := true.
	autoHighlightString := aString.
	autoHighlightAllOccurrences := aBoolean.! !

!MethodSet methodsFor: 'shout styling' stamp: 'jmv 3/8/2024 14:50:57'!
justDidAutoHighlight
	"Don't do autoHighlight unless accepted contents are updated."
	shouldAutoHighlight := false! !

!MethodSet methodsFor: 'shout styling' stamp: 'jmv 3/8/2024 14:50:57'!
messageSendsRangesOf: aSelector

	^ selectedMessage 
		ifNil: [ #() ]
		ifNotNil: [selectedMessage messageSendsRangesOf: aSelector ]! !

!MethodSet methodsFor: 'shout styling' stamp: 'jmv 3/8/2024 14:50:57'!
shouldAutoHighlight
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^shouldAutoHighlight! !

!MethodSet methodsFor: 'shout styling' stamp: 'jmv 3/8/2024 14:50:57'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [ ^false ].
	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
	^true! !

!MethodSet methodsFor: 'initialization' stamp: 'jmv 3/8/2024 14:50:57'!
initialize

	super initialize.
	shouldAutoHighlight := false.! !

!MethodSet methodsFor: 'run tests' stamp: 'jmv 3/8/2024 14:53:24'!
debugMethodTest
	
	currentCompiledMethod isTestMethod ifTrue: [
		currentCompiledMethod methodClass debugAsFailure: currentCompiledMethod selector ifCanNot: [ PopUpMenu inform: TestCase canNotDebugMethodErrorDescription ]]
! !

!MethodSet methodsFor: 'run tests' stamp: 'jmv 3/8/2024 14:53:29'!
runMethodTest
	
	| suite |
	
	suite := TestSuite forCompiledMethod: currentCompiledMethod.
	(ProgressiveTestRunner for: suite) value! !


!MethodSet class methodsFor: 'instance creation' stamp: 'jmv 3/8/2024 14:50:57'!
messageList: anArray 
	"Answer an instance of me with message list anArray."

	^self new initializeMessageList: anArray! !

!MethodSet class methodsFor: 'utilities' stamp: 'jmv 3/8/2024 14:50:57'!
isPseudoSelector: aSelector
	"Answer whether the given selector is a special marker"

	^ #(Comment Definition) statePointsTo: aSelector! !

!MethodSet class methodsFor: 'ui' stamp: 'jmv 3/8/2024 14:50:57'!
open
	^ MessageSetWindow openFullProtocolForClass: Object! !

