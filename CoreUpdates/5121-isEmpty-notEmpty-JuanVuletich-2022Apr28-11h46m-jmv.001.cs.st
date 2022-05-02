'From Cuis 6.0 [latest update: #5116] on 28 April 2022 at 12:03:39 pm'!

!ArrayedCollection methodsFor: 'testing' stamp: 'jmv 4/27/2022 10:28:41'!
isEmpty
	^self size = 0! !


!Interval methodsFor: 'accessing' stamp: 'jmv 4/28/2022 11:20:19'!
isEmpty
	^self size = 0! !


!OrderedCollection methodsFor: 'accessing' stamp: 'jmv 4/28/2022 11:20:47'!
isEmpty
	^self size = 0! !


!RunArray methodsFor: 'accessing' stamp: 'jmv 4/28/2022 11:24:21'!
isEmpty
	^runs size = 0! !


!Text methodsFor: 'testing' stamp: 'jmv 4/28/2022 11:17:59'!
isEmpty
	^self size = 0! !


!Object methodsFor: 'copying' stamp: 'jmv 4/27/2022 12:36:30'!
copyForClipboard
	"The answer is used both for internal (Smalltalk) and external (Host OS) clipboard.
	Some subclasses might need specific behavior."
	^self copy! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 4/28/2022 11:40:04'!
annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"

	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].

	^ String streamContents: [ :strm |
		Preferences methodAnnotations
			do: [ :each |
				each caseOf: {
					[#firstComment] -> [
						strm nextPutAll: (aClass firstCommentAt: aSelector) ].
					[#masterComment] -> [
						strm nextPutAll: ((aClass supermostPrecodeCommentFor: aSelector) ifNil: ['']) ].
					[#documentation] -> [
						strm nextPutAll: ((aClass precodeCommentOrInheritedCommentFor: aSelector) ifNil: ['']) ].
					[#timeStamp] -> [ | stamp |
						stamp _ self timeStamp.
						strm nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp'])].
					[#linesOfCode] -> [
						strm
							print: ((aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm | cm linesOfCode]);
							nextPutAll: ' lines of code' ].
					[#messageCategory] -> [
						strm nextPutAll: (( aClass organization categoryOfElement: aSelector) ifNil: ['']) ].
					[#sendersCount] -> [ | sendersCount |
						sendersCount _ Smalltalk numberOfSendersOf: aSelector.
						sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
						strm nextPutAll: sendersCount ].
					[#implementorsCount] -> [ | implementorsCount |
						implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
						implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
						strm nextPutAll: implementorsCount ].
					[#priorVersionsCount] -> [
						self addPriorVersionsCountForSelector: aSelector ofClass: aClass to: strm].
					[#priorTimeStamp] -> [ | stamp |
						stamp _ VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
						strm nextPutAll: 'prior timestamp: '; nextPutAll: (stamp ifNil: ['None']) ].
					[#packages] -> [
						(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
							(CodePackage packageOfMethod: cm methodReference ifNone: nil)
								ifNil: [ strm nextPutAll: 'in no package' ]
								ifNotNil: [ :codePackage |
									strm nextPutAll: 'in package '; nextPutAll: codePackage packageName ]]].
					[#changeSets] -> [ | aList |
						aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allChangeSets] -> [ | aList |
						aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allBaseSystemChangeSets] -> [ | aList |
						aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in base system change set']
									ifFalse: [strm nextPutAll: 'in base system change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no base system change set']].
					[#closuresInfo] -> [
						strm nextPutAll: (aClass closuresInfoAt: aSelector)].
					
				}]
			separatedBy: [ strm nextPutAll: self annotationSeparator ] ].! !

!CodeProvider methodsFor: 'commands' stamp: 'jmv 4/28/2022 11:40:20'!
removeClass
	"Remove the selected class from the system, at interactive user request.  Make certain the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| message  className classToRemove result |
	classToRemove _ self selectedClassOrMetaClass ifNil: [Smalltalk beep. ^ false].
	classToRemove _ classToRemove theNonMetaClass.
	className _ classToRemove name.
	message _ 'Are you certain that you
want to REMOVE the class ', className, '
from the system?'.
	(result _ self confirm: message)
		ifTrue: [
			classToRemove subclasses notEmpty
				ifTrue: [(self confirm: 'class has subclasses: ' , message)
					ifFalse: [^ false]].
			classToRemove removeFromSystem.
			self changed: #classList.
			true].
	^ result! !


!Browser methodsFor: 'accessing' stamp: 'jmv 4/27/2022 11:17:38'!
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
			^ comment isEmpty
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

!Browser methodsFor: 'class list' stamp: 'jmv 4/27/2022 11:18:00'!
recent
	"Let the user select from a list of recently visited classes.  11/96 stp.
	 12/96 di:  use class name, not classes themselves.
	 : dont fall into debugger in empty case"

	| className class recentList |
	recentList _ self class recentClasses select: [:n | Smalltalk includesKey: n].
	recentList isEmpty ifTrue: [^ Smalltalk beep].
	className _ (SelectionMenu selections: recentList) startUpMenu.
	className ifNil: [^ self].
	class _ Smalltalk at: className.
	self setSelectedSystemCategory: class category.
	self selectClass: class! !

!Browser methodsFor: 'message category functions' stamp: 'jmv 4/27/2022 11:18:05'!
removeMessageCategory
	"If a message category is selected, create a Confirmer so the user can 
	verify that the currently selected message category should be removed
 	from the system. If so, remove it."

	| messageCategoryName |
	selectedMessageCategory ifNil: [^ self].
	messageCategoryName _ self selectedMessageCategoryName.
	(self messageList isEmpty
		or: [self confirm: 'Are you sure you want to
remove this method category 
and all its methods?'])
		ifTrue: [
			self selectedClassOrMetaClass removeCategory: messageCategoryName.
			self messageCategoryListIndex: 0.
			self changed: #classSelectionChanged].
	self changed: #messageCategoryList.
! !

!Browser methodsFor: 'system category functions' stamp: 'jmv 4/28/2022 11:40:27'!
moveAllToOtherSystemCategory
	"If a class category is selected, prompt user for category to move to,
	create a Confirmer so the user can verify that all the classes in current category
 	should be moved to the selected category."
	| newSystemCategory |
	selectedSystemCategory ifNil: [ ^ self ].
	newSystemCategory _ Smalltalk systemCategoryFromUserWithPrompt: 'Move classes to System Category...'.
	(newSystemCategory notNil and: [
		self classList notEmpty and: [ self confirm: 'Are you sure you want to
move classes from ' , selectedSystemCategory , ' 
to ' , newSystemCategory , '?' ]]) ifTrue: [
		"Safer this way (#classList will be a collection of strings with spaces and who knows what in the future.  So let's just get the classes we need directly)"
		(SystemOrganization classesAt: selectedSystemCategory) do: [ :eaClass |
			eaClass category: newSystemCategory ].
		self changed: #systemCategoryRoots ].! !

!Browser methodsFor: 'system category functions' stamp: 'jmv 4/27/2022 11:18:09'!
removeSystemCategory
	"If a class category is selected, create a Confirmer so the user can 
	verify that the currently selected class category and all of its classes
 	should be removed from the system. If so, remove it."

	selectedSystemCategory ifNil: [^ self].
	(self classList isEmpty
		or: [self confirm: 'Are you sure you want to
remove this system category 
and all its classes?'])
		ifTrue: [
			"Remove subcategories:"
			systemOrganizer removeCategoriesMatching: selectedSystemCategory, '-*'.
			"Then remove the top category (it doesn't do anything if it doesn't exist):"
			systemOrganizer removeSystemCategory: selectedSystemCategory.
			self setSelectedSystemCategory: nil.
			self changed: #systemCategoryRoots ]! !


!MessageSet methodsFor: 'private' stamp: 'jmv 4/27/2022 11:18:12'!
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


!MessageNames methodsFor: 'search' stamp: 'jmv 4/28/2022 11:40:38'!
computeSelectorListFromSearchString
	"Compute selector list from search string"
	| sorted |
	sorted _ SortedCollection sortBlock: [ :x :y |
		x asLowercase <= y asLowercase ].
	(searchString findBetweenSubStrs: ';') do: [ :selPat | | raw n m |
		selPat notEmpty ifTrue: [
			(selPat first = $" and: [
				selPat last = $" and: [ selPat size > 2 ]])
				ifTrue: [
					Symbol
						hasInterned:
							(selPat
								copyFrom: 2
								to: selPat size - 1)
						ifTrue: [ :sym |
							sorted add: sym ]]
				ifFalse: [
					raw _ ((n _ selPat occurrencesOf: $*) > 0 | ((m _ selPat occurrencesOf: $#) > 0) and: [ selPat size > (n + m) ])
						ifTrue: [ Symbol selectorsMatching: selPat ]
						ifFalse: [ Symbol selectorsContaining: selPat ].
					sorted addAll: raw ]]].
	selectorList _ sorted asArray  select: [ :sym |
		(Smalltalk isThereAnImplementorOf: sym) or: [ Smalltalk isThereAReferenceTo: sym ]].
	^ selectorList.! !

!MessageNames methodsFor: 'selector list' stamp: 'jmv 4/28/2022 11:40:43'!
messageList
	"Answer the receiver's message list, computing it if necessary. The way 
	to force a recomputation is to set the messageList to nil"
	messageList
		ifNil: [
			messageList _ selectedSelector
				ifNil: [#()]
				ifNotNil: [
					Smalltalk allImplementorsOf: selectedSelector].
			self initializeMessageList: messageList.
			self messageListIndex: (messageList notEmpty
				ifTrue: [1]
				ifFalse: [0])
			].
	^ messageList! !


!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 4/27/2022 11:18:16'!
defaultClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	| answer |
	answer _ selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [
			(package includesSystemCategory: selectedSystemCategory)
				ifTrue: [ systemOrganizer listAtCategoryNamed: selectedSystemCategory ]
				ifFalse: [ 
					package extensionClassNamesIn: (selectedSystemCategory copyFrom: 2 to: selectedSystemCategory size) ]].
	selectedClassName ifNil: [
		answer isEmpty ifFalse: [
			selectedClassName _ answer first.
			self setClassOrganizer.
			self editSelection: #editClass ]].
	^answer! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 4/27/2022 11:18:20'!
messageCategoryList
	"Answer the selected category of messages."

	"Do not include the -- all -- category"
	| answer |
	answer _ self rawMessageCategoryList.
	selectedMessageCategory ifNil: [
		answer isEmpty ifFalse: [
			(package includesSystemCategory: selectedSystemCategory) ifFalse: [
				selectedMessageCategory _ answer first ]]].
	^answer! !

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 4/27/2022 11:18:23'!
messageList
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!!) (??), it is reset to zero."

	| answer |
	answer _ selectedMessageCategory
		ifNil: [ #() ]
		ifNotNil: [
			(self classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory) ifNil: [
				selectedMessageCategory _ nil.
				#() ]].
	selectedMessage ifNil: [
		answer isEmpty ifFalse: [
			(package includesSystemCategory: selectedSystemCategory) ifFalse: [
				selectedMessage _ answer first.
				self editSelection: #editMessage ]]].
	^answer! !


!CodeFileBrowser methodsFor: 'removing' stamp: 'jmv 4/27/2022 11:18:26'!
removeMessageCategory
	"If a message category is selected, create a Confirmer so the user can 
	verify that the currently selected message category should be removed
 	from the system. If so, remove it."

	| messageCategoryName |
	selectedMessageCategory ifNil: [ ^self ].
	messageCategoryName _ self selectedMessageCategoryName.
	(self messageList isEmpty
		or: [self confirm: 'Are you sure you want to
remove this method category 
and all its methods?']) ifFalse: [^ self].
	self selectedClassOrMetaClass removeCategory: messageCategoryName.
	self messageCategoryListIndex: 0.
	self changed: #messageCategoryList.! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 4/28/2022 11:41:06'!
destroyCurrentCodeOfSelections
	"Actually remove from the system any in-memory methods with class and selector identical to items current selected.  This may seem rather arcane but believe me it has its great uses, when trying to split out code.  To use effectively, first file out a change set that you wish to split off.  Then open a ChangeList browser on that fileout.  Now look through the methods, and select any of them which you want to remove completely from the system, then issue this command.  For those methods where you have made changes to pre-existing versions, of course, you won't want to remove them from the system, so use this mechanism with care!!"

	|  aClass aChange aList |
	aList _ OrderedCollection new.
	1 to: changeList size do:
		[:index |
			(listSelections at: index) ifTrue:
				[aChange _ changeList at: index.
				(aChange changeType == #method
					and: [(aClass _ aChange changeClass) notNil
					and: [aClass includesSelector: aChange methodSelector]])
						ifTrue:
							[aList add: {aClass. aChange methodSelector}]]].

	aList notEmpty ifTrue: [
		(self confirm: 'Warning!! This will actually remove ', aList size printString,  ' method(s) from the system!!') ifFalse: [^ self]].
	aList do: [ :aPair |
		Transcript newLine; show: 'Removed: ', aPair first printString, '.', aPair second.
		aPair first removeSelector: aPair second ]! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 4/27/2022 11:18:28'!
removeNonSelections
	"Remove the unselected items from the receiver."

	| newChangeList newList |

	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.

	1 to: changeList size do: [ :i | 
		(listSelections at: i) ifTrue: [
			newChangeList add: (changeList at: i).
			newList add: (list at: i)]].
	newChangeList isEmpty ifTrue: [
		^ self inform: 'That would remove everything.
Why would you want to do that?'].

	newChangeList size < changeList size
		ifTrue: [
			changeList _ newChangeList.
			list _ newList.
			listIndex _ 0.
			self clearSelections ].
	self changed: #list

	! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 4/28/2022 11:40:57'!
browseRecentLogOn: origChangesFileEntry
	"figure out where the last snapshot or quit was, then browse the recent entries."

	| end done block positions prevBlock |
	origChangesFileEntry readStreamDo: [ :changesFile |
		positions _ SortedCollection new.
		end _ changesFile size.
		prevBlock _ end.
		block _ end - 1024 max: 0.
		done _ false.
		[ done or: [ positions notEmpty ]]
			whileFalse: [
				changesFile position: block.
				"ignore first fragment"
				changesFile nextChunk.
				[ changesFile position < prevBlock ]
					whileTrue: [
						| pos chunk |
						pos _ changesFile position.
						chunk _ changesFile nextChunk.
						((Smalltalk isQuitRecord: chunk) or: [ Smalltalk isSnapshotRecord: chunk ])
							ifTrue: [ positions add: pos ]].
				block = 0
					ifTrue: [done _ true]
					ifFalse: [
						prevBlock _ block.
						block _ block - 1024 max: 0]].
	].
	self browseRecentLogOn: origChangesFileEntry startingFrom: (positions isEmpty ifTrue: [0] ifFalse: [positions last])! !


!VersionsBrowser methodsFor: 'misc' stamp: 'jmv 4/28/2022 11:41:10'!
addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream
	"Add an annotation detailing the prior versions count.  Specially handled here for the case of a selector no longer in the system, whose prior versions are seen in a versions browser -- in this case, the inherited version of this method will not work."

	(aClass includesSelector: aSelector) ifTrue:
		[^ super addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream].

	aStream nextPutAll: 
		((changeList notEmpty
			ifTrue:
				[changeList size = 1
					ifTrue:
						['deleted - one prior version']
					ifFalse:
						['deleted - ', changeList size printString, ' prior versions']]
			ifFalse:
				['surprisingly, no prior versions']))! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 4/27/2022 11:18:31'!
rename
	"Store a new name string into the selected ChangeSet.  reject duplicate name; allow user to back out"

	self request: 'New name for this change set' initialAnswer: myChangeSet name do: [:newName|
		(newName = myChangeSet name or: [newName isEmpty])
			ifTrue: [Smalltalk beep]
			ifFalse:
				[(ChangeSet changeSetNamed: newName) notNil
					ifTrue: [self inform: 'Sorry that name is already used']
					ifFalse:
						[myChangeSet name: newName.
						self update.
						self changed: #mainButtonName.
						self changed: #relabel]]]! !

!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 4/28/2022 11:41:16'!
contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."
	| category selector class oldSelector |

	(class _ self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [aString isEmpty]) ifTrue: [ ^ false].
		(aString count: [:char | char == $"]) odd 
			ifTrue: [self inform: 'unmatched double quotes in preamble']
			ifFalse: [(Scanner new scanTokens: aString) notEmpty ifTrue: [
				self inform: 'Part of the preamble is not within double-quotes.
To put a double-quote inside a comment, type two double-quotes in a row.
(Ignore this warning if you are including a doIt in the preamble.)']].
		myChangeSet preambleString: aString.
		self currentSelector: nil.  "forces update with no 'unsubmitted chgs' feedback"
		^ true].
	oldSelector _ self selectedMessageName.
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector ifNil: [^ false].
	(self messageList includes: selector)
		ifTrue: [self currentSelector: selector]
		ifFalse: [self currentSelector: oldSelector].
	self update.
	^ true! !


!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 4/27/2022 11:16:48'!
firstCommentAt:  selector
	"Answer a string representing the first comment in the method associated with selector.  Return an empty string if the relevant source file is not available, or if the method's source code does not contain a comment.  Not smart enough to bypass quotes in string constants, but does map doubled quote into a single quote."

	| sourceString commentStart  pos nextQuotePos |

	sourceString _ (self sourceCodeAt: selector) asPlainString.
	sourceString isEmpty ifTrue: [^ ''].
	commentStart _ sourceString findString: '"' startingAt: 1.
	commentStart = 0 ifTrue: [^ ''].
	pos _ commentStart + 1.
	[(nextQuotePos _ sourceString findString: '"' startingAt: pos) = (sourceString findString: '""' startingAt: pos)]
		whileTrue:
			[pos _ nextQuotePos + 2].
	
	commentStart = nextQuotePos ifTrue: [^ ''].  "Must have been a quote in string literal"

	^ (sourceString copyFrom: commentStart + 1 to: nextQuotePos - 1) copyReplaceAll: '""' with: '"'


"Behavior firstCommentAt: #firstCommentAt:"! !


!ClassDescription methodsFor: 'accessing class hierarchy' stamp: 'jmv 4/28/2022 11:54:23'!
removeUninstantiatedSubclassesSilently
	"Remove the classes of any subclasses that have neither instances nor subclasses.  Answer the number of bytes reclaimed"
	"Player removeUninstantiatedSubclassesSilently"

	| candidatesForRemoval  oldFree |

	oldFree _ Smalltalk garbageCollect.
	candidatesForRemoval _
		self subclasses select: [:c |
			(c instanceCount = 0) and: [c subclasses isEmpty]].
	candidatesForRemoval do: [:c | c removeFromSystem].
	^ Smalltalk garbageCollect - oldFree! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/27/2022 11:16:54'!
classComment: aString stamp: aStamp
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| ptr header oldCommentRemoteStr |
	aString isRemote ifTrue: [
		SystemChangeNotifier uniqueInstance classCommented: self.
		^ self organization classComment: aString stamp: aStamp].

	oldCommentRemoteStr _ self organization commentRemoteStr.
	(aString isEmpty) & (oldCommentRemoteStr == nil) ifTrue: [^ self organization classComment: nil].
		"never had a class comment, no need to write empty string out"

	ptr _ oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].
	SourceFiles ifNotNil: [ | file |
		(file _ SourceFiles at: 2) ifNotNil: [
			file setToEnd; newLine; nextPut: $!!.	"directly"
			header _ String streamContents: [:strm | strm nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
				aStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
			file nextChunkPut: header]].
	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.
	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
				stream newLine; nextPut: $!!.	"directly"
				header _ String streamContents: [:strm | strm nextPutAll: self name;
					nextPutAll: ' commentStamp: '.
					aStamp storeOn: strm.
					strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
				stream nextChunkPut: header.
				stream newLine; nextChunkPut: aString.
			].
	].
	SystemChangeNotifier uniqueInstance classCommented: self! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/28/2022 11:41:33'!
printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod overridesMethod: overridenMethod
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	If it has a prior source link, it MUST have a stamp, even if it is empty."

"The current design is that changeStamps and prior source links are preserved in the changes file.  All fileOuts include changeStamps.  Condensing sources, however, eliminates all stamps (and links, natch)."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		(changeStamp notNil and: [
			changeStamp notEmpty or: [priorMethod notNil]]) ifTrue: [
			strm nextPutAll: ' stamp: '; print: changeStamp].
		priorMethod notNil ifTrue: [
			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer].
		overridenMethod notNil ifTrue: [
			strm nextPutAll: ' overrides: '; print: overridenMethod sourcePointer]
		]).
! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/28/2022 11:41:36'!
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
	preamble notEmpty ifTrue: [ outStream nextChunkPut: ' ' ].
	^ outStream newLine! !

!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 4/28/2022 11:41:28'!
chooseInstVarThenDo: aBlock
	"Put up a menu of all the instance variables in the receiver, and when
the user chooses one, evaluate aBlock with the chosen variable as its
parameter.  If the list is 6 or larger, then offer an alphabetical
formulation as an alternative. triggered by a 'show alphabetically' item
at the top of the list."

	| lines labelStream allVars index count offerAlpha |
	(count _ self allInstVarNames size) = 0 ifTrue: 
		[ ^ self inform: 'There are no\instance variables.' withNewLines ].

	allVars _ OrderedCollection new.
	lines _ OrderedCollection new.
	labelStream _ WriteStream on: (String new: 200).

	(offerAlpha _ count > 5)
		ifTrue: [
			lines add: 1.
			allVars add: 'show alphabetically'.
			labelStream nextPutAll: allVars first; newLine].
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class instVarNames.
		vars do: [ :var |
			labelStream nextPutAll: var; newLine.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream skip: -1 "cut last CR".
	(lines notEmpty and: [ lines last = allVars size ]) ifTrue: [
		lines removeLast ].  "dispense with inelegant line beneath last item"
	index _ (PopUpMenu labels: labelStream contents lines: lines)
startUpWithCaption: 'Instance variables in
', self name.
	index = 0 ifTrue: [^ self].
	(index = 1 and: [offerAlpha]) ifTrue: [
		^ self chooseInstVarAlphabeticallyThenDo: aBlock].
	aBlock value: (allVars at: index)! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 4/28/2022 11:55:06'!
categories
	"Answer an Array of categories (names)."
	categoryArray ifNil: [^ nil].
	(categoryArray size = 1 
		and: [categoryArray first = Default & elementArray isEmpty])
		ifTrue: [^Array with: NullCategory].
	^categoryArray! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 4/28/2022 11:56:23'!
classify: element under: heading suppressIfDefault: aBoolean
	"Store the argument, element, in the category named heading.   If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein"

	| catName catIndex elemIndex realHeading |
	
	realHeading _ (heading = NullCategory or: [heading isNil])
		ifTrue: [Default]
		ifFalse: [heading withBlanksTrimmed ].
	
	"done if already under that category"
	(catName _ self categoryOfElement: element) = realHeading ifTrue: [^ self].  

	catName ifNotNil: [
		"return if non-Default category already assigned in memory"
		(aBoolean and: [realHeading = Default]) ifTrue: [^ self].	 
		"remove if in another category"
		self removeElement: element].

	(categoryArray indexOf: realHeading) = 0 ifTrue: [self addCategory: realHeading].

	catIndex _ categoryArray indexOf: realHeading.
	elemIndex _ catIndex > 1
		ifTrue: [categoryStops at: catIndex - 1]
		ifFalse: [0].
	[(elemIndex _ elemIndex + 1) <= (categoryStops at: catIndex) 
		and: [element >= (elementArray at: elemIndex)]] whileTrue.

	"elemIndex is now the index for inserting the element. Do the insertion before it."
	elementArray _ elementArray copyReplaceFrom: elemIndex to: elemIndex-1 with: (Array with: element).

	"add one to stops for this and later categories"
	catIndex to: categoryArray size do: [ :i |
		categoryStops at: i put: (categoryStops at: i) + 1].

	(self listAtCategoryNamed: Default) isEmpty ifTrue: [self removeCategory: Default]! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 4/28/2022 11:57:18'!
removeCategory: cat 
	"Remove the category named, cat. Create an error notificiation if the 
	category has any elements in it."

	| index lastStop |
	index _ categoryArray indexOf: cat ifAbsent: [^self].
	lastStop _ 
		index = 1
			ifTrue: [0]
			ifFalse: [categoryStops at: index - 1].
	(categoryStops at: index) - lastStop > 0 
		ifTrue: [^self error: 'cannot remove non-empty category'].
	categoryArray _ categoryArray copyReplaceFrom: index to: index with: #().
	categoryStops _ categoryStops copyReplaceFrom: index to: index with: #().
	categoryArray isEmpty
		ifTrue: [
			categoryArray _ Array with: Default.
			categoryStops _ Array with: 0]
! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 4/28/2022 11:57:31'!
removeEmptyCategories
	"Remove empty categories."

	| categoryIndex currentStop keptCategories keptStops |
	keptCategories _ WriteStream on: (Array new: 16).
	keptStops _ WriteStream on: (Array new: 16).
	currentStop _ categoryIndex _ 0.
	[(categoryIndex _ categoryIndex + 1) <= categoryArray size]
		whileTrue: 
			[(categoryStops at: categoryIndex) > currentStop
				ifTrue: 
					[keptCategories nextPut: (categoryArray at: categoryIndex).
					keptStops nextPut: (currentStop _ categoryStops at: categoryIndex)]].
	categoryArray _ keptCategories contents.
	categoryStops _ keptStops contents.
	categoryArray isEmpty
		ifTrue: [
			categoryArray _ Array with: Default.
			categoryStops _ Array with: 0]

	"ClassOrganizer allInstancesDo: [:co | co removeEmptyCategories]."! !

!Categorizer methodsFor: 'testing' stamp: 'jmv 4/28/2022 11:54:54'!
hasAnyCategoriesSuchThat: aBlock
	"Answer an Array of categories (names)."
	categoryArray ifNil: [^ false].
	(categoryArray size = 1 
		and: [categoryArray first = Default & elementArray isEmpty])
		ifTrue: [^false].
	^categoryArray anySatisfy: aBlock! !


!BasicClassOrganizer methodsFor: 'accessing' stamp: 'jmv 4/27/2022 11:17:01'!
classComment: aString 
	"Store the comment, aString, associated with the object that refers to the 
	receiver."

	aString ifNil: [ ^classComment _ nil ].
	
	aString isRemote
		ifTrue: [classComment _ aString]
		ifFalse: [aString isEmpty
			ifTrue: [classComment _ nil]
			ifFalse: [
				self error: 'use aClass classComment:'.
				classComment _ RemoteString newString: aString onFileNumber: 2]]
				"Later add priorSource and date and initials?"! !

!BasicClassOrganizer methodsFor: 'accessing' stamp: 'jmv 4/28/2022 11:51:03'!
classComment: aString  stamp: aStamp
	"Store the comment, aString, associated with the object that refers to the receiver."

	self commentStamp: aStamp.
	aString isRemote
		ifTrue: [classComment _ aString]
		ifFalse: [(aString isNil or: [aString isEmpty])
			ifTrue: [classComment _ nil]
			ifFalse:
				[self error: 'use aClass classComment:'.
				classComment _ RemoteString newString: aString onFileNumber: 2]]
				"Later add priorSource and date and initials?"! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 4/28/2022 11:50:53'!
fileOutCategory: category on: aFileStream initializing: aBool
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first poolSet tempClass classes |
	classes _ (self superclassOrderIn: category).
	poolSet _ Set new.
	classes do: [ :class |
		class sharedPools do: [:eachPool | poolSet add: eachPool]].
	poolSet notEmpty ifTrue: [
		tempClass _ Class new.
		tempClass shouldFileOutPools ifTrue:
			[poolSet _ poolSet select: [:aPool | tempClass shouldFileOutPool: (Smalltalk keyAtIdentityValue: aPool)].
			poolSet do: [:aPool | tempClass fileOutPool: aPool onFileStream: aFileStream]]].
	first _ true.
	classes do: [ :class | 
		first
			ifTrue: [first _ false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false].
	aBool ifTrue: [classes do:[:cls| cls fileOutInitializerOn: aFileStream]].! !


!MessageTally methodsFor: 'collecting leaves' stamp: 'jmv 4/28/2022 11:57:41'!
leavesInto: leafDict fromSender: senderTally

	| rcvrs |
	rcvrs _ self sonsOver: 0.
	rcvrs isEmpty
		ifTrue: [ self into: leafDict fromSender: senderTally ]
		ifFalse: [
			rcvrs do: [ :node |
				node isPrimitives
					ifTrue: [ node leavesInto: leafDict fromSender: senderTally ]
					ifFalse: [ node leavesInto: leafDict fromSender: self ]]]! !

!MessageTally methodsFor: 'comparing' stamp: 'jmv 4/28/2022 11:57:47'!
sonsOver: threshold

	| hereTally sons |
	(receivers isNil or: [ receivers isEmpty ]) ifTrue: [ ^#() ].
	hereTally _ tally.
	sons _ receivers select: [ :son | "subtract subNode tallies for primitive hits here"
		hereTally _ hereTally - son tally.
		son tally > threshold ].
	hereTally > threshold
		ifTrue: [
			| last |
			last _ MessageTally new class: class method: method.
			^sons copyWith: (last primitives: hereTally)].
	^sons! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'jmv 4/28/2022 11:31:06'!
first
	"Answer the first element of the receiver.
	Raise an error if the collection is empty."

	self isEmpty ifTrue: [self errorEmptyCollection].
	^ self at: 1! !


!String methodsFor: 'converting' stamp: 'jmv 4/27/2022 11:17:18'!
asIdentifier: shouldBeCapitalized
	"Return a legal identifier, with first character in upper case if shouldBeCapitalized is true, else lower case.  This will always return a legal identifier, even for an empty string"

	| aString |
	aString _ self select: [ :el | el isValidInIdentifiers ].
	(aString isEmpty or: [aString first isValidStartOfIdentifiers not])
		ifTrue:	 [aString _ 'a', aString].
	^ shouldBeCapitalized ifTrue: [ aString capitalized ] ifFalse: [ aString uncapitalized ]

"
'234Fred987' asIdentifier: false
'235Fred987' asIdentifier: true
'' asIdentifier: true
'()87234' asIdentifier: false
'())z>=PPve889  U >' asIdentifier: false
"! !

!String methodsFor: 'converting' stamp: 'jmv 4/28/2022 11:58:20'!
initialIntegerOrNil
	"Answer the integer represented by the leading digits of the receiver, or nil if the receiver does not begin with a digit"
	| firstNonDigit |
	(self isEmpty or: [self first isDigit not]) ifTrue: [^ nil].
	firstNonDigit _ (self findFirst: [:m | m isDigit not]).
	firstNonDigit = 0 ifTrue: [firstNonDigit _ self size + 1].
	^ (self copyFrom: 1  to: (firstNonDigit - 1)) asNumber
"
'234Whoopie' initialIntegerOrNil
'wimpy' initialIntegerOrNil
'234' initialIntegerOrNil
'2N' initialIntegerOrNil
'2' initialIntegerOrNil
'  89Ten ' initialIntegerOrNil
'78 92' initialIntegerOrNil
"
! !

!String methodsFor: 'system primitives' stamp: 'jmv 4/28/2022 11:58:24'!
findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned.

	The algorithm below is not optimum -- it is intended to be translated to C which will go so fast that it wont matter."
	| index |
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>
	self var: #key declareC: 'unsigned char *key'.
	self var: #body declareC: 'unsigned char *body'.
	self var: #matchTable declareC: 'unsigned char *matchTable'.

	key isEmpty ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index _ 1.
			[(matchTable at: (body at: startIndex+index-1) numericValue + 1)
				= (matchTable at: (key at: index) numericValue + 1)]
				whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index _ index+1]].
	^ 0
"
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 1 matchTable: CaseSensitiveOrder 1
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 2 matchTable: CaseSensitiveOrder 7
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 8 matchTable: CaseSensitiveOrder 0
' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseSensitiveOrder 0
' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseInsensitiveOrder 7
"! !

!String methodsFor: 'system primitives' stamp: 'jmv 4/28/2022 11:58:30'!
numArgs
	"Answer either the number of arguments that the receiver would take if considered a selector. 
	Answer -1 if it couldn't be a selector. 
		Note that currently this will answer -1 for anything begining with an uppercase letter even though 
		the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."
	| firstChar numColons start ix |
	self isEmpty ifTrue: [ ^ -1 ].
	"Binary messages"
	(self allSatisfy: [ :c | c isValidInBinarySelectors ])
		ifTrue: [ ^1 ].
	"Unary and keyword messages"
	firstChar _ self at: 1.
	firstChar isValidStartOfIdentifiers ifTrue: [
		"Fast reject if any chars are non-alphanumeric"
		(self
			findSubstring: '~'
			in: self
			startingAt: 1
			matchTable: Tokenish) > 0 ifTrue: [ ^ -1 ].
		"Fast colon count"
		numColons _ 0.
		start _ 1.
		[
		(ix _ self
			findSubstring: ':'
			in: self
			startingAt: start
			matchTable: CaseSensitiveOrder) > 0 ] whileTrue: [
			numColons _ numColons + 1.
			start _ ix + 1 ].
		^ numColons ].
	^ -1.! !


!Symbol methodsFor: 'accessing' stamp: 'jmv 4/28/2022 11:58:35'!
precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise.  The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	| c |
	self isEmpty ifTrue: [^ 0].
	"Consider selectors starting with an underscore $_ as unary, even if Preferences allowUnderscoreSelectors is not set."
	c _ self first.
	c isValidInBinarySelectors ifTrue: [^ 2].
	self last = $: ifTrue: [^ 3].
	^ 1! !


!RunArray methodsFor: 'copying' stamp: 'jmv 4/28/2022 11:32:23'!
, aRunArray 
	"Answer a new RunArray that is a concatenation of the receiver and
	aRunArray."

	| new newRuns answer |
	(aRunArray isMemberOf: RunArray)
		ifFalse: [
			new _ self copy.
			"attempt to be sociable"
			aRunArray do: [:each | new addLast: each].
			^new].
	self isEmpty ifTrue: [^aRunArray copy].
	aRunArray isEmpty ifTrue: [^self copy].
	(self canJoin: (values at: values size) and: (aRunArray values at: 1))
		ifFalse: [ 
			answer _ RunArray
				runs: runs , aRunArray runs
				values: values , aRunArray values.
			answer canJoinMessage: canJoinMessage.
			^answer ].
	newRuns _ runs
					copyReplaceFrom: runs size
					to: runs size
					with: aRunArray runs.
	newRuns at: runs size put: (runs at: runs size) + (aRunArray runs at: 1).
	answer _ RunArray
		runs: newRuns
		values: 
			(values
				copyReplaceFrom: values size
				to: values size
				with: aRunArray values).
	answer canJoinMessage: canJoinMessage.
	^answer! !


!Text methodsFor: 'accessing' stamp: 'jmv 4/27/2022 11:15:58'!
rangeOf: attribute startingAt: index
	"Answer an interval that gives the range of attribute at index position  index. An empty interval with start value index is returned when the attribute is not present at position index."
	^ self isEmpty
		ifTrue: [ index to: index - 1 ]
		ifFalse: [
			runs
				rangeOf: attribute
				startingAt: index ].! !

!Text methodsFor: 'accessing' stamp: 'jmv 4/27/2022 11:14:34'!
size
	^runs size! !

!Text methodsFor: 'emphasis' stamp: 'jmv 4/27/2022 11:15:29'!
addAttribute: att 
	self isEmpty ifTrue: [ ^self ].
	^ self addAttribute: att from: 1 to: self size! !

!Text methodsFor: 'emphasis' stamp: 'jmv 4/27/2022 10:36:29'!
attributesAt: characterIndex
	"Answer the code for characters in the run beginning at characterIndex."

	self isEmpty
		ifTrue: [^ #()].  "null text tolerates access"
	^runs at: characterIndex! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 4/28/2022 11:34:00'!
removeAllUnSentMessages
	"Smalltalk removeAllUnSentMessages"
	"[Smalltalk unusedClasses do: [:c | (Smalltalk at: c) removeFromSystem]. 
	Smalltalk removeAllUnSentMessages > 0] whileTrue."
	"Remove all implementations of unsent messages."
	| sels n |
	sels _ self allUnSentMessages.
	self presumedSentMessages
		do: [:sel | sels
				remove: sel
				ifAbsent: nil].
	sels isEmpty
		ifTrue: [^ 0].
	n _ 0.
	Smalltalk
		allBehaviorsDo: [:x | n _ n + 1].
	'Removing ' , sels size printString , ' messages . . .'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: n
		during: [:barBlock | 
			n _ 0.
			self
				allBehaviorsDo: [:class | 
					barBlock value: (n _ n + 1).
					sels
						do: [:sel | class removeSelector: sel]]].
	^ sels size! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/28/2022 11:43:21'!
browseClassesWithNamesContaining: aString caseSensitive: caseSensitive 
	"Smalltalk browseClassesWithNamesContaining: 'eMorph' caseSensitive: true "
	"Launch a class-list list browser on all classes whose names containg aString as a substring."

	| suffix aList |
	suffix _ caseSensitive
				ifTrue: [' (case-sensitive)']
				ifFalse: [' (use shift for case-sensitive)'].
	aList _ OrderedCollection new.
	Smalltalk allClassesDo: [ :class |
		(class name includesSubstring: aString caseSensitive: caseSensitive)
			ifTrue: [aList add: class name]].
	aList notEmpty
		ifTrue: [HierarchyBrowserWindow forClassesNamed: aList asSet sorted title: 'Classes whose names contain ' , aString , suffix]! !


!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 4/28/2022 11:59:14'!
writeClassRenameMethod: sel was: oldName fromInstVars: oldList 
	"The class coming is unknown.  Ask the user for the existing class it maps to.  If got one, write a method, and restart the obj fileIn.  If none, write a dummy method and get the user to complete it later.  "

	| tell choice newName answ code oldVer newList newVer instSel |
	self flag: #bobconv.
	tell := 'Reading an instance of ' , oldName 
				, '.
Which modern class should it translate to?'.
	answ := (PopUpMenu 
				labels: 'Let me type the name now
Let me think about it
Let me find a conversion file on the disk') 
					startUpWithCaption: tell.
	answ = 1 
		ifTrue: [
			tell := 'Name of the modern class {1} should translate to:' format: {oldName}.
			choice := self request: tell initialAnswer: ''.	"class name"
			choice isEmpty 
				ifTrue: [answ := 'conversion method needed']
				ifFalse: [
					newName := choice.
					answ := Smalltalk at: newName asSymbol
								ifAbsent: ['conversion method needed'].
					answ class == String 
						ifFalse: [renamed at: oldName asSymbol put: answ name]]].
	answ = 3 | (answ = 0) 
		ifTrue: [
			byteStream close.
			^'conversion method needed'].
	answ = 2 ifTrue: [answ := 'conversion method needed'].
	answ = 'conversion method needed' 
		ifTrue: [
			byteStream close.
			newName := 'PutNewClassHere'].
	answ class == String 
		ifFalse: 
			[oldVer := self versionSymbol: (structures at: oldName).
			newList := (Array with: answ classVersion) , answ allInstVarNames.
			newVer := self versionSymbol: newList.
			instSel := 'convert' , oldVer , ':' , newVer , ':'].
	code := WriteStream on: (String new: 500).
	code
		nextPutAll: sel;
		newLine.
	answ class == String 
		ifFalse: [
			code
				newLine;
				tab;
				nextPutAll: 'reshaped at: #' , oldName , ' put: #' , instSel , '.'.
			code
				newLine;
				tab;
				tab;
				nextPutAll: '"Be sure to define that conversion method in class ' 
							, answ name , '"'].
	code
		newLine;
		tab;
		nextPutAll: '^ ' , newName.	"Return new class"
	self class compile: code contents classified: 'conversion'.
	newName = 'PutNewClassHere' 
		ifTrue: [
			self 
				inform: 'Please complete the following method and 
then read-in the object file again.'.
			Smalltalk browseAllImplementorsOf: sel asSymbol].
	self flag: #violateBasicLayerPrinciples.
	"SmartRefStream should not refer to UI!!!!!!!!!! (sd)"

	"The class version number only needs to change under one specific circumstance.  That is when the first letters of the instance variables have stayed the same, but their meaning has changed.  A conversion method is needed, but this system does not know it.  
	If this is true for class Foo, define classVersion in Foo class.  
	Beware of previous object fileouts already written after the change in meaning, but before bumping the version number.  They have the old (wrong) version number, say 2.  If this is true, your method must be able to test the data and successfully read files that say version 2 but are really 3."
	^answ! !

!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 4/28/2022 12:00:17'!
writeConversionMethod: sel class: newClass was: oldName fromInstVars: oldList to: newList
	"The method convertToCurrentVersion:refStream: was not found in newClass.  Write a default conversion method for the author to modify."

	| code newOthers oldOthers copied |

	code _ WriteStream on: (String new: 500).
	code nextPutAll: 'convertToCurrentVersion: varDict refStream: smartRefStrm'; newLine; tab.
	newOthers _ newList asOrderedCollection "copy".
	oldOthers _ oldList asOrderedCollection "copy".
	copied _ OrderedCollection new.
	newList do: [:instVar |
		(oldList includes: instVar) ifTrue: [
			instVar isInteger ifFalse: [copied add: instVar].
			newOthers remove: instVar.
			oldOthers remove: instVar]].
	code nextPutAll: '"These variables are automatically stored into the new instance '.
	code nextPutAll: copied asArray printString; nextPut: $. .
	code newLine; tab; nextPutAll: 'This method is for additional changes.'; 
		nextPutAll: ' Use statements like (foo _ varDict at: ''foo'')."'; newLine; newLine; tab.
	newOthers isEmpty & oldOthers isEmpty ifTrue: [^ self].
		"Instance variables are the same.  Only the order changed.  No conversion needed."
	newOthers notEmpty ifTrue: [
		code nextPutAll: '"New variables: ', newOthers asArray printString, '  If a non-nil value is needed, please assign it."\' withNewLines].
	oldOthers notEmpty ifTrue: [
		code nextPutAll: '	"These are going away ', oldOthers asArray printString, '.  Possibly store their info in some other variable?"'].

	code newLine; tab.
	code nextPutAll: '^ super convertToCurrentVersion: varDict refStream: smartRefStrm'.
	newClass compile: code contents classified: 'object fileIn'.


	"If you write a conversion method beware that the class may need a version number change.  This only happens when two conversion methods in the same class have the same selector name.  (A) The inst var lists of the new and old versions intials as some older set of new and old inst var lists.  or (B) Twice in a row, the class needs a conversion method, but the inst vars stay the same the whole time.  (For an internal format change.)
	If either is the case, fileouts already written with the old (wrong) version number, say 2.  Your method must be able to read files that say version 2 but are really 3, until you expunge the erroneous version 2 files from the universe."! !

!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 4/28/2022 12:01:29'!
writeConversionMethodIn: newClass fromInstVars: oldList to: newList renamedFrom: oldName
	"The method convertToCurrentVersion:refStream: was not found in newClass.  Write a default conversion method for the author to modify.  If method exists, append new info into the end."

	| code newOthers oldOthers copied newCode |

	newOthers _ newList asOrderedCollection "copy".
	oldOthers _ oldList asOrderedCollection "copy".
	copied _ OrderedCollection new.
	newList do: [:instVar |
		(oldList includes: instVar) ifTrue: [
			instVar isInteger ifFalse: [copied add: instVar].
			newOthers remove: instVar.
			oldOthers remove: instVar]].
	code _ WriteStream on: (String new: 500).
	code newLine; newLine; tab; nextPutAll: '"From ', SystemVersion current version, ' [', Smalltalk lastUpdateString;
			nextPutAll: '] on ', Date today printString, '"'; newLine.
	code tab; nextPutAll: '"These variables are automatically stored into the new instance: '.
	code nextPutAll: copied asArray printString; nextPut: $.; newLine.
	code tab; nextPutAll: 'Test for this particular conversion.'; 
		nextPutAll: '  Get values using expressions like (varDict at: ''foo'')."'; newLine; newLine.
	newOthers isEmpty & oldOthers isEmpty & oldName isNil ifTrue: [^ self].
		"Instance variables are the same.  Only the order changed.  No conversion needed."
	newOthers notEmpty ifTrue: [
		code tab; nextPutAll: '"New variables: ', newOthers asArray printString, 
			'.  If a non-nil value is needed, please assign it."'; newLine].
	oldOthers notEmpty ifTrue: [
		code tab; nextPutAll: '"These are going away ', oldOthers asArray printString, 
			'.  Possibly store their info in some other variable?"'; newLine].
	oldName ifNotNil: [
		code tab; nextPutAll: '"Test for instances of class ', oldName, '.'; newLine.
		code tab; nextPutAll: 'Instance vars with the same name have been moved here."'; newLine.
		].
	code tab; nextPutAll: '"Move your code above the ^ super...  Delete extra comments."'; newLine. 

	(newClass includesSelector: #convertToCurrentVersion:refStream:) 
		ifTrue: ["append to old methods"
			newCode _ (newClass sourceCodeAt: #convertToCurrentVersion:refStream:),
				code contents]
		ifFalse: ["new method"
			newCode _ 'convertToCurrentVersion: varDict refStream: smartRefStrm',
				code contents, 
				'	^ super convertToCurrentVersion: varDict refStream: smartRefStrm'].
	newClass compile: newCode classified: 'object fileIn'.


	"If you write a conversion method beware that the class may need a version number change.  This only happens when two conversion methods in the same class have the same selector name.  (A) The inst var lists of the new and old versions intials as some older set of new and old inst var lists.  or (B) Twice in a row, the class needs a conversion method, but the inst vars stay the same the whole time.  (For an internal format change.)
	If either is the case, fileouts already written with the old (wrong) version number, say 2.  Your method must be able to read files that say version 2 but are really 3, until you expunge the erroneous version 2 files from the universe."! !


!TextEditor methodsFor: 'nonediting/nontyping keys' stamp: 'jmv 4/27/2022 11:16:07'!
setSearchString: aKeyboardEvent
	"Establish the current selection as the current search string."

	| aString |
	self lineSelectAndEmptyCheck: [^ true].
	aString _  self selectedString.
	aString isEmpty
		ifTrue: [ self flash ]
		ifFalse: [ self setSearch: aString ].
	^ true! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 4/28/2022 12:02:31'!
explainAnySel: symbol 
	"Is this any message selector?"

	| list reply |
	list _ Smalltalk allClassesImplementing: symbol.
	list isEmpty ifTrue: [^nil].
	list size < 12
		ifTrue: [reply _ ' is a message selector which is defined in these classes ' , list printString]
		ifFalse: [reply _ ' is a message selector which is defined in many classes'].
	^'#' , symbol , reply , '.\' withNewLines, 'Smalltalk browseAllImplementorsOf: #' , symbol! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 4/28/2022 12:02:24'!
explain
	"Try to shed some light on what kind of entity the current selection is. 
	The selection must be a single token or construct. Insert the answer after 
	the selection. Send private messages whose names begin with 'explain' 
	that return a string if they recognize the selection, else nil."

	| string tiVars cgVars selectors delimitors numbers sorry reply symbol provider |
	sorry _ 'Sorry, I can''t explain that.  Please select a single
token, construct, or special character.'.
	(string _ self selectedString) isEmpty
		ifTrue: [reply _ '']
		ifFalse: [
			string _ string withBlanksTrimmed.
			"Remove space, tab, cr"
			"Temps and Instance vars need only test strings that are all letters"
			(string detect: [:char | char isValidInIdentifiers not]
				ifNone: nil) ifNil: [
					tiVars _ (self explainTemp: string)
						ifNil: [self explainInst: string]].
					
			provider _ self codeProvider.
			(tiVars == nil and: [provider respondsTo: #explainSpecial:])
				ifTrue: [tiVars _ provider explainSpecial: string].
			tiVars _ tiVars
				ifNil: [ '']
				ifNotNil: [ tiVars , '\' withNewLines].
			"Context, Class, Pool, and Global vars, and Selectors need 
			only test symbols"
			(Symbol hasInterned: string ifTrue: [:s | symbol _ s])
				ifTrue: [
					cgVars _ (self explainCtxt: symbol) 
						ifNil: [ (self explainClass: symbol)
							ifNil: [ self explainGlobal: symbol]].
					"See if it is a Selector (sent here or not)"
					selectors _ (self explainMySel: symbol)
						ifNil: [(self explainPartSel: string)
							ifNil: [ self explainAnySel: symbol]]]
				ifFalse: [selectors _ self explainPartSel: string].
			cgVars _ cgVars
				ifNil: [ '']
				ifNotNil: [cgVars , '\' withNewLines].
			selectors _ selectors
				ifNil: [ '']
				ifNotNil: [ selectors , '\' withNewLines].
			delimitors _ string size = 1
				ifTrue: ["single special characters"
					self explainChar: string]
				ifFalse: ["matched delimitors"
					self explainDelimitor: string].
			numbers _ self explainNumber: string.
			numbers ifNil: [numbers _ ''].
			delimitors ifNil: [delimitors _ ''].
			reply _ tiVars , cgVars , selectors , delimitors , numbers].
	reply isEmpty ifTrue: [reply _ sorry].

	morph showBalloon: reply.
	self runningWorld ifNotNil: [ :w | w findATranscript ].
	reply print! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 4/27/2022 11:16:19'!
selectedSymbol
	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"

	| aString |
	self hasSelection ifFalse: [^ nil].
	aString _ self selectedString withoutSeparators.
	aString isEmpty ifTrue: [^ nil].
	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].

	^ nil! !


!AbstractFont methodsFor: 'measuring' stamp: 'jmv 4/28/2022 11:37:16'!
approxWidthOfText: aText
	"Return the width of aText -- quickly, and a little bit dirty.
	Used by lists morphs containing Text objects to get a quick,
	fairly accurate measure of the width of a list item."
	| w |
	(aText isNil or: [ aText isEmpty ]) ifTrue: [ ^ 0 ].
	w _ self
		widthOfString: aText string
		from: 1
		to: aText size.
	"If the text has no emphasis, just return the string size.  If it is empasized, 
	just approximate the width by adding about 20% to the width"
	^ ((aText runLengthFor: 1) = aText size and: [ (aText emphasisAt: 1) = 0 ])
		ifTrue: [ w ]
		ifFalse: [ w * 6 // 5 ]! !


!Morph methodsFor: 'copying' stamp: 'jmv 4/27/2022 12:36:40'!
copyForClipboard
	"The answer is used both for internal (Smalltalk) and external (Host OS) clipboard.
	Some subclasses might need specific behavior."

	self okayToDuplicate ifFalse: [ ^ nil ].
	^self copy! !

!Morph methodsFor: 'structure' stamp: 'jmv 4/28/2022 11:37:24'!
veryLastLeaf
	"Answer the last submorph, recursively, i.e. the very last leaf of the morph tree"

	^submorphs isEmpty
		ifTrue: [ self ]
		ifFalse: [ submorphs last veryLastLeaf ]! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 4/28/2022 11:37:39'!
drawOn: aCanvas

	listItems isEmpty ifTrue: [ ^self ].
 
	self drawHighlightOn: aCanvas.
	self drawSelectionOn: aCanvas.

	(self topVisibleRowForCanvas: aCanvas)
		to: (self bottomVisibleRowForCanvas: aCanvas)
		do: [ :row |
			(owner itemSelectedAmongMultiple:  row) ifTrue: [
				self drawBackgroundForMulti: row on: aCanvas. ].
			self draw: (self item: row) displayStringOrText atRow: row on: aCanvas ]! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 4/28/2022 11:38:45'!
showUnreferencedClassVars
	"Search for all class variables known to the selected class, and put up a 
	list of those that have no references anywhere in the system. The 
	search includes superclasses, so that you don't need to navigate your 
	way to the class that defines each class variable in order to determine 
	whether it is unreferenced"
	| cls aList |
	(cls _ model selectedClass)
		ifNil: [^ self].
	aList _ cls allUnreferencedClassVariables.
	aList isEmpty
		ifTrue: [^ self inform: 'There are no unreferenced
class variables in
' , cls name].
	Transcript newLine; nextPutAll: 'Unreferenced class variable(s) in ', cls name; newLine.
	aList do: [:el | Transcript tab; nextPutAll: el; newLine].
	(SelectionMenu labels: aList selections: aList)
		startUpWithCaption: 'Unreferenced
class variables in 
' , cls name! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 4/28/2022 11:38:41'!
showUnreferencedInstVars
	"Search for all instance variables known to the selected class, and put up a list of those that have no references anywhere in the system.  The search includes superclasses, so that you don't need to navigate your way to the class that defines each inst variable in order to determine whether it is unreferenced"

	| cls aList |
	(cls _ model selectedClassOrMetaClass) ifNil: [^ self].
	aList _ cls allUnreferencedInstanceVariables.
	aList isEmpty ifTrue: [^ self inform: 'There are no unreferenced
instance variables in
', cls name].
	Transcript newLine; nextPutAll: 'Unreferenced instance variable(s) in ', cls name; newLine.
	aList do: [ :el | Transcript tab; nextPutAll: el; newLine ].
	(SelectionMenu labels: aList selections: aList) startUpWithCaption: 'Unreferenced
instance variables in 
', cls name! !

!CodeWindow methodsFor: 'misc' stamp: 'jmv 4/28/2022 11:38:49'!
selectMessageAndEvaluate: aBlock
	"Allow the user to choose one selector, chosen from the currently selected message's selector, as well as those of all messages sent by it, and evaluate aBlock on behalf of chosen selector.  If there is only one possible choice, simply make it; if there are multiple choices, put up a menu, and evaluate aBlock on behalf of the the chosen selector, doing nothing if the user declines to choose any"

	| selector method messages |
	(selector _ model selectedMessageName) ifNil: [^ self].
	method _ (model selectedClassOrMetaClass ifNil: [^ self])
		compiledMethodAt: selector
		ifAbsent: nil.
	(method isNil or: [(messages _ method messages) isEmpty])
		 ifTrue: [^ aBlock value: selector].
	(messages size = 1 and: [messages includes: selector])
		ifTrue:
			[^ aBlock value: selector].  "If only one item, there is no choice"

	Smalltalk
		showMenuOf: messages
		withFirstItem: selector
		ifChosenDo: aBlock! !


!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 4/28/2022 11:38:37'!
browseCurrentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"
	| aList |

	aList _ model currentVersionsOfSelections.

	aList isEmpty ifTrue: [^ self inform: 'no selected methods have in-memory counterparts'].
	MessageSetWindow
		openMessageListUnsorted: aList 
		label: 'Current versions of selected methods in ', model file localName! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 4/28/2022 11:45:06'!
fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse |
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse _ Preferences changeSetVersionNumbers
				ifTrue: [
					DirectoryEntry currentDirectory
						nextNameFor: nameToUse coda: '-', Utilities authorInitials
						extension: 'cs.st' ]
				ifFalse: [ (nameToUse , '.' , Utilities dateTimeSuffix , '.cs.st') asFileName ].
		
	nameToUse asFileEntry writeStreamDo: [ :stream |
		stream timeStamp.
		self fileOutPreambleOn: stream.
		self fileOutOn: stream.
		self fileOutPostscriptOn: stream ].
	
	self hasUnsavedChanges: false.
	Preferences checkForSlips
		ifFalse: [^ self].
	slips _ self checkForSlips.
	(slips notEmpty
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ]! !


!TestResult methodsFor: 'testing' stamp: 'jmv 4/28/2022 11:45:14'!
hasErrors

	^self errors notEmpty
			! !

!TestResult methodsFor: 'testing' stamp: 'jmv 4/28/2022 11:45:19'!
hasFailures

	^self failures notEmpty
			! !

