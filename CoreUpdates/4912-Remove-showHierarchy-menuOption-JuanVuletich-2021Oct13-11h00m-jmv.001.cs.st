'From Cuis 5.0 [latest update: #4910] on 13 October 2021 at 11:30:24 am'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 10/13/2021 11:28:09'!
annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"

	| stamp sendersCount implementorsCount aCategory separator aString aList aComment stream requestList |
	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].
	stream _ WriteStream on: String new.
	requestList _ self annotationRequests.
	separator _ self annotationSeparator.
	requestList
		do: [:aRequest | 
			aRequest == #firstComment
				ifTrue: [
					aComment _ aClass firstCommentAt: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream position = 0 ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aComment]].
			aRequest == #masterComment
				ifTrue: [
					aComment _ aClass supermostPrecodeCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream position = 0 ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aComment]].
			aRequest == #documentation
				ifTrue: [
					aComment _ aClass precodeCommentOrInheritedCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream isEmpty ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aComment]].
			aRequest == #timeStamp
				ifTrue: [
					stamp _ self timeStamp.
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp'])].
			aRequest == #linesOfCode
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: ((aClass compiledMethodAt: aSelector ifAbsent: nil)
						ifNotNil: [ :cm | cm linesOfCode]) printString, ' lines of code'].
			aRequest == #messageCategory
				ifTrue: [
					aCategory _ aClass organization categoryOfElement: aSelector.
					aCategory
						ifNotNil: ["woud be nil for a method no longer present,  
							e.g. in a recent-submissions browser"
							stream isEmpty ifFalse: [stream nextPutAll: separator].
							stream nextPutAll: aCategory]].
			aRequest == #sendersCount
				ifTrue: [
					sendersCount _ Smalltalk numberOfSendersOf: aSelector.
					sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: sendersCount].
			aRequest == #implementorsCount
				ifTrue: [
					implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
					implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: implementorsCount].
			aRequest == #priorVersionsCount
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					self
						addPriorVersionsCountForSelector: aSelector
						ofClass: aClass
						to: stream].
			aRequest == #priorTimeStamp
				ifTrue: [
					stamp _ VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
					stamp
						ifNotNil: [stream isEmpty ifFalse: [stream nextPutAll: separator].
							stream nextPutAll: 'prior timestamp: ' , stamp]].
			aRequest == #packages
				ifTrue: [
					(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
						stream isEmpty ifFalse: [stream nextPutAll: separator].
						(CodePackage packageOfMethod: cm methodReference ifNone: nil)
							ifNil: [ stream nextPutAll: 'in no package' ]
							ifNotNil: [ :codePackage |
								stream nextPutAll: 'in package '; nextPutAll: codePackage packageName ]]].
			aRequest == #changeSets
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set']
								ifFalse: [stream nextPutAll: 'in change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no change set']].
			aRequest == #allChangeSets
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set']
								ifFalse: [stream nextPutAll: 'in change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no change set']].
			aRequest == #allBaseSystemChangeSets
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
					aList size > 0
						ifTrue: [
							aList size = 1
								ifTrue: [stream nextPutAll: 'only in base system change set']
								ifFalse: [stream nextPutAll: 'in base system change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no base system change set']].
			aRequest == #closuresInfo
				ifTrue: [
					aString _ aClass closuresInfoAt: aSelector.
					aString size > 0
						ifTrue: [stream isEmpty ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aString]].
		].
	^ stream contents! !


!Browser methodsFor: 'accessing' stamp: 'jmv 10/13/2021 11:19:23'!
acceptedStringOrText
	"Depending on the current selection, different information is retrieved.
	Answer a string description of that information. This information is the
	method of the currently selected class and message."

	| comment theClass latestCompiledMethod |
	latestCompiledMethod _ currentCompiledMethod.
	currentCompiledMethod _ nil.

	editSelection == #none ifTrue: [^ ''].
	editSelection == #editSystemCategories 
		ifTrue: [^ systemOrganizer printString].
	self isEditingNewClass 
		ifTrue: [^ (theClass _ self selectedClass)
			ifNil: [
				Class template: selectedSystemCategory]
			ifNotNil: [
				Class templateForSubclassOf: theClass category: selectedSystemCategory]].
	self isEditingExistingClass 
		ifTrue: [^ self classDefinitionText ].
	editSelection == #editComment 
		ifTrue: [
			(theClass _ self selectedClass) ifNil: [^ ''].
			comment _ theClass comment.
			currentCompiledMethod _ theClass organization commentRemoteStr.
			^ comment size = 0
				ifTrue: ['This class has not yet been commented.']
				ifFalse: [comment]].
	editSelection == #editMessageCategories 
		ifTrue: [^ self classOrMetaClassOrganizer printString].
	editSelection == #newMessage
		ifTrue: [
			^ (theClass _ self selectedClassOrMetaClass) 
				ifNil: ['']
				ifNotNil: [theClass sourceCodeTemplate]].
	editSelection == #editMessage
		ifTrue: [
			self showingByteCodes ifTrue: [^ self selectedBytecodes].
			currentCompiledMethod _ latestCompiledMethod.
			^ self selectedMessage].

	self error: 'Browser internal error: unknown edit selection.'! !

!Browser methodsFor: 'accessing' stamp: 'jmv 10/13/2021 11:23:02'!
contents: input notifying: aRequestor
	"The retrieved information has changed and its source must now be
	 updated. The information can be a variety of things, depending on
	 the list selections (such as templates for class or message definition,
	 methods) or the user menu commands (such as definition, comment).
	Answer the result of updating the source."

	| aString aText theClass |
	aString _ input asString.
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

!Browser methodsFor: 'class functions' stamp: 'jmv 10/13/2021 11:23:55'!
explainSpecial: string 
	"Answer a string explaining the code pane selection if it is displaying 
	one of the special edit functions."

	| classes whole lits reply |
	self isEditingClass
		ifTrue: 
			["Selector parts in class definition"
			string last == $: ifFalse: [^nil].
			lits _ Array with:
				#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:.
			(whole _ lits detect: [:each | (each keywords
					detect: [:frag | frag = string] ifNone: nil) notNil]
						ifNone: nil) notNil
				ifTrue: [reply _ '"' , string , ' is one part of the message selector ' , whole , '.']
				ifFalse: [^nil].
			classes _ Smalltalk allClassesImplementing: whole.
			classes _ 'these classes ' , classes printString.
			^reply , '  It is defined in ' , classes , '."
Smalltalk browseAllImplementorsOf: #' , whole].

	editSelection == #editSystemCategories ifTrue: [^nil].
	editSelection == #editMessageCategories ifTrue: [^nil].
	^nil! !

!Browser methodsFor: 'class functions' stamp: 'jmv 10/13/2021 11:25:19'!
plusButtonHit
	"Cycle among definition and comment."

	editSelection == #editComment
		ifTrue: [
			self editSelection: #editClass.
			selectedClassName ifNil: [ ^self ].
			self changed: #editComment.
			self acceptedContentsChanged.
			^ self].
	self editComment.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.! !


!MessageSet methodsFor: 'message list' stamp: 'jmv 10/13/2021 11:19:12'!
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
			^ class definition ].].

	(class notNil and: [ class includesSelector: selector]) ifFalse: [
		currentCompiledMethod _ nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod _ class compiledMethodAt: selector ifAbsent: nil.
	self showingDocumentation ifTrue: [
		^ self commentContents ].

	^self sourceStringPrettifiedAndDiffed! !

!MessageSet methodsFor: 'accessing' stamp: 'jmv 10/13/2021 11:28:31'!
contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| category selector class oldSelector |
	self okayToAccept ifFalse: [^ false].
	selectedMessage ifNil: [^ false].
	class _ selectedMessage actualClass.
	oldSelector _ selectedMessage methodSymbol.
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue:
		[oldSelector == #Comment ifTrue:
			[class comment: aString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Compiler
				evaluate: aString
				notifying: aRequestor
				logged: true.
			self changed: #clearUserEdits.
 			^ false]].
	"Normal method accept"
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	self triggerEvent: #annotationChanged.
	^ true! !


!MessageSet class methodsFor: 'utilities' stamp: 'jmv 10/13/2021 11:29:12'!
isPseudoSelector: aSelector
	"Answer whether the given selector is a special marker"

	^ #(Comment Definition) statePointsTo: aSelector! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 10/13/2021 11:27:31'!
classListMenuOptions

	^ `{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 			-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		30.
				#label 			-> 		'browse protocol (p)'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutClass.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		5.
				#label 			-> 		'show classes hierarchically'.
				#object 			-> 		#model.
				#selector 			-> 		#enableListClassesHierarchically.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		6.
				#label 			-> 		'show classes alphabetically'.
				#object 			-> 		#model.
				#selector 			-> 		#enableListClassesAlphabetically.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'show definition'.
				#object 			-> 		#model.
				#selector 			-> 		#editClass.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		30.
				#label 			-> 		'show comment'.
				#object 			-> 		#model.
				#selector 			-> 		#editComment.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 			-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 			-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'class var refs...'.
				#selector 			-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		20.
				#label 			-> 		'class vars'.
				#selector 			-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		30.
				#label 			-> 		'class refs (N)'.
				#selector 			-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		10.
				#label 			-> 		'rename class... (R)'.
				#object 			-> 		#model.
				#selector 			-> 		#renameClass.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		20.
				#label 			-> 		'copy class...'.
				#object 			-> 		#model.
				#selector 			-> 		#copyClass.
				#icon 			-> 		#copyIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'remove class (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 			-> 		70.
				#itemOrder 			-> 		10.
				#label 			-> 		'run tests (t)'.
				#object 			-> 		#model.
				#selector 			-> 		#runClassTests.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		70.
				#itemOrder 			-> 		20.
				#label 			-> 		'more...'.
				#selector 			-> 		#offerClassListMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	! !

!methodRemoval: PseudoClass #printHierarchy stamp: 'jmv 10/13/2021 11:19:42'!
PseudoClass removeSelector: #printHierarchy!
!methodRemoval: Behavior #printHierarchy stamp: 'jmv 10/13/2021 11:19:42'!
Behavior removeSelector: #printHierarchy!
!methodRemoval: Browser #hierarchy stamp: 'jmv 10/13/2021 11:27:46'!
Browser removeSelector: #hierarchy!
!methodRemoval: CodeProvider #annotationForHierarchyFor: stamp: 'jmv 10/13/2021 11:29:37'!
CodeProvider removeSelector: #annotationForHierarchyFor:!
