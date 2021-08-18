'From Cuis 5.0 [latest update: #4767] on 18 August 2021 at 1:45:58 pm'!

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/18/2021 13:45:29'!
browseMessageListUnsorted: methodReferences name: labelString autoSelect: autoSelectString
	"Create and schedule a MessageSet browser on the message list.
	Don't sort entries by default."

	| messageListSize title |

	messageListSize _ methodReferences size.
	messageListSize = 0 ifTrue: [^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title _ messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	MessageSetWindow
		openMessageListUnsorted: methodReferences
		label: title
		autoSelect: autoSelectString! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 8/18/2021 13:35:02'!
methodInheritance
	"Create and schedule a method browser on the inheritance of implementors."

	| list aClassNonMeta isMeta theClassOrMeta aClass sel |
	aClass _ model selectedClassOrMetaClass.
	sel _ model selectedMessageName.
	aClass ifNil: [ ^ self ].
	sel ifNil: [ ^ self ].
	aClassNonMeta _ aClass theNonMetaClass.
	isMeta _ aClassNonMeta ~~ aClass.
	list _ OrderedCollection new.
	aClass allSuperclasses reverseDo: [ :cl |
		(cl includesSelector: sel) ifTrue: [
			list addLast: (MethodReference class: cl selector: sel) ]].
	aClassNonMeta
		allSubclassesWithLevelDo: [ :cl :level |
			theClassOrMeta _ isMeta
				ifTrue: [ cl class ]
				ifFalse: [ cl ].
			(theClassOrMeta includesSelector: sel) ifTrue: [
				list addLast: (MethodReference class: theClassOrMeta selector: sel) ]]
		startingLevel: 0.
	Smalltalk
		browseMessageList: list
		name: 'Inheritance of ' , sel.! !


!CodeFileBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 8/18/2021 13:35:08'!
methodInheritance
	(model selectedClassOrMetaClass isNil or:
		[model selectedClassOrMetaClass hasDefinition])
			ifFalse: [super methodInheritance]! !


!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 8/18/2021 13:44:33'!
openMessageListUnsorted: methodReferences label: labelString autoSelect: autoSelectString
	"Open a system view for a MessageSet on messageList.
	Don't sort entries by default."

	| messageSet |

	messageSet _ MessageSet messageList: methodReferences asArray.
	messageSet autoSelectString: autoSelectString.

	^self open: messageSet label: labelString ! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/18/2021 13:33:06'!
testDecompiler
	"
	Smalltalk testDecompiler
	"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	| methodNode oldMethod newMethod badOnes oldCodeString n |
	badOnes _ OrderedCollection new.
	'Decompiling all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldMethod _ cls compiledMethodAt: selector.
					oldCodeString _ (cls decompilerClass new
						decompile: selector
						in: cls
						method: oldMethod) decompileString.
					methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldCodeString =
						(cls decompilerClass new
							decompile: selector
							in: cls
							method: newMethod) decompileString ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: (MethodReference class: cls selector: selector) ]]]].
	Smalltalk
		browseMessageList: badOnes
		name: 'Decompiler Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/18/2021 13:33:12'!
testFormatter
	"Smalltalk testFormatter"
	"Reformats the source for every method in the system, and then compiles that source and verifies that it generates identical code.
	The formatting used will be classic monochrome."
	| newCodeString methodNode oldMethod newMethod badOnes n |
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
					newCodeString _ cls compilerClass new
						format: (cls sourceCodeAt: selector)
						in: cls
						notifying: nil.
					methodNode _ cls compilerClass new
						compile: newCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldMethod _ cls compiledMethodAt: selector.
					oldMethod = newMethod ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes
		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/18/2021 13:33:19'!
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
					oldCodeString _ (cls sourceCodeAt: selector) asString.
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

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/18/2021 13:45:39'!
browseAllAccessesTo: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods 
	or any methods of a subclass/superclass that refer to the instance variable name."

	"self new browseAllAccessesTo: 'contents' from: Collection."
		
	^ self
		browseMessageListUnsorted: (aClass allAccessesTo: instVarName)
		name: 'Accesses to ' , instVarName 
		autoSelect: instVarName! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/18/2021 13:25:24'!
browseAllCallsOn: aLiteral
	"Create and schedule a message browser on each method that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (self allCallsOn: aLiteral)
				name: 'Users of ' , aLiteral key
				autoSelect: aLiteral key ]
		ifFalse: [
			self
				browseMessageList: (self allCallsOn: aLiteral)
				name: 'Senders of ' , aLiteral
				autoSelect: aLiteral ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/18/2021 13:25:36'!
browseAllCallsOn: aLiteral localTo: aClass
	"Create and schedule a message browser on each method in or below the given class that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	aClass ifNil: [ ^ self inform: 'no selected class' ].
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral)
				name: 'Users of ' , aLiteral key , ' local to ' , aClass name
				autoSelect: aLiteral key ]
		ifFalse: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral)
				name: 'Senders of ' , aLiteral , ' local to ' , aClass name
				autoSelect: aLiteral ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/18/2021 13:25:43'!
browseAllCallsOnClass: aClass
	"Create and schedule a message browser on each method that refers to 
	aClass. For example, Smalltalk browseAllCallsOnClass: Object."
	self
		browseMessageList: aClass allCallsOn
		name: 'Users of class ' , aClass theNonMetaClass name
		autoSelect: aClass theNonMetaClass name.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/18/2021 13:30:38'!
browseAllImplementorsOfList: selectorList title: aTitle
	"Create and schedule a message browser on each method that implements the message whose selector is in the argument selectorList. For example,  Smalltalk browseAllImplementorsOfList: #(at:put: size) title: 'stuff'."

	| flattenedList |
	flattenedList _ Array streamContents: [ :stream |
		selectorList do: [ :sel |
			stream nextPutAll: (self allImplementorsOf: sel)]].
	^ self browseMessageList: flattenedList name: aTitle! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/18/2021 13:31:29'!
browseAllReferencesToLiteral: aLiteral
	"Create and schedule a message browser on each method that references aLiteral. For example, 
	Smalltalk browseAllReferencesToLiteral: 47.
	Smalltalk browseAllReferencesToLiteral: 0@0.
	"
	^ self
		browseMessageList: (self allReferencesToLiteral: aLiteral)
		name: 'References to literal ' , aLiteral asString.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/18/2021 13:26:04'!
browseClassCommentsWithString: aString
	"Smalltalk browseClassCommentsWithString: 'my instances' "
	"Launch a message list browser on all class comments containing aString as a substring."
	| caseSensitive suffix list |
	suffix _ (caseSensitive _ Sensor shiftPressed)
		ifTrue: [ ' (case-sensitive)' ]
		ifFalse: [ ' (use shift for case-sensitive)' ].
	list _ Set new.
	Smalltalk allClassesDo: [ :class |
		(class organization classComment asString
			findString: aString
			startingAt: 1
			caseSensitive: caseSensitive) > 0 ifTrue: [
			list add: (MethodReference class: class selector: #Comment) ]].
	^ self
		browseMessageList: list
		name: 'Class comments containing ', aString printString, suffix
		autoSelect: aString.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/18/2021 13:04:28'!
browseMessageList: methodReferences name: labelString autoSelect: autoSelectString
	"Create and schedule a MessageSet browser on the message list."

	| messageListSize title |

	messageListSize _ methodReferences size.
	messageListSize = 0 ifTrue: [^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title _ messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	MessageSetWindow
		openMessageList: methodReferences
		label: title
		autoSelect: autoSelectString! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 8/18/2021 13:40:57'!
browseViewReferencesFromNonViews
	"
	Smalltalk browseViewReferencesFromNonViews
	"
	| aLiteral aCollection |

	aCollection _ OrderedCollection new.

	"Tweak to look just for pluggables or also for menus (or maybe for all morphs)"
"	PopUpMenu withAllSubclasses , MenuMorph withAllSubclasses , PluggableMorph withAllSubclasses do: [ :view |"
	PluggableMorph withAllSubclassesDo: [ :view |
"	MenuMorph withAllSubclassesDo: [ :view |"
		
		aLiteral _ view name.
		
		"tweak to linclude refs to SysWindow subhierarchy or not"
		(view includesBehavior: SystemWindow) & false ifFalse: [
			Smalltalk allBehaviorsDo: [ :class |
				((class includesBehavior: Morph) or: [ class includesBehavior: Morph class ]) ifFalse: [
					class addMethodsTo: aCollection thatReferenceTo: aLiteral special: false byte: nil ]]]].
	
	Smalltalk
		browseMessageList: aCollection asSet
		name: 'References to Views from non-Views'
		autoSelect: ''.! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 8/18/2021 13:41:52'!
referencesToSelectedLiteral
	"Evaluate the selected text and browse methods that reference the same literal"
	[
		self
			evaluateSelectionAndDo: [ :result |
				Smalltalk
					browseMessageList: (Smalltalk allReferencesToLiteral: result)
					name: 'Users of literal: ' , result asString
					autoSelect: self selection ]
			ifFail: nil
			profiled: false ]
	on: UndeclaredVariableReference , UnknownSelector
	do: [ :ex |
		morph flash ]! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 8/18/2021 13:35:46'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList |

	aList _ #(
	(10	'browse'			browseMethodFull				'view this method in a browser')
	(11	'senders' 			browseSendersOfMessages	'browse senders of...')
	(16	'implementors'		browseMessages				'browse implementors of...')
	(12	'versions'			browseVersions					'browse versions')), 

	(Preferences decorateBrowserButtons
		ifTrue:
			[{#(13	'inheritance'		methodInheritance 	'browse method inheritance
green: sends to super
tan: has override(s)
mauve: both of the above
pink: is an override but doesn''t call super
pinkish tan: has override(s), also is an override but doesn''t call super' )}]
		ifFalse:
			[{#(13	'inheritance'		methodInheritance	'browse method inheritance')}]),

	#(
	(12	'hierarchy'			browseHierarchy				'browse class hierarchy')
	(10	'inst vars'			browseInstVarRefs				'inst var refs...')
	(11	'class vars'			browseClassVarRefs			'class var refs...')
	(10	'show...'				offerWhatToShowMenu		'menu of what to show in lower pane')).

	^ aList! !

!CodeWindow methodsFor: 'accessing' stamp: 'jmv 8/18/2021 13:35:17'!
inheritanceButton
	"If receiver has an Inheritance button, answer it, else answer nil.  morphic only at this point"

	^ self buttonWithSelector: #methodInheritance! !

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 8/18/2021 13:35:25'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel class |
	aChar == $D ifTrue: [^ model toggleDiffing].

	sel _ model selectedMessageName.
	aChar == $m ifTrue: [  "These next two put up a type in if no message selected"
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk].
	aChar == $n ifTrue: [
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk].

	"The following require a class selection"
	(class _ model selectedClassOrMetaClass) ifNil: [^ self ].
	aChar == $b ifTrue: [^ BrowserWindow fullOnClass: class selector: sel].
	aChar == $N ifTrue: [^ self browseClassRefs].
	aChar == $i ifTrue: [^ self methodInheritance].
	aChar == $h ifTrue: [^ self browseHierarchy].
	aChar == $p ifTrue: [^ self browseFullProtocol].

	sel
		ifNotNil: [
			"The following require a method selection"
			aChar == $o ifTrue: [^ model fileOutMessage].
			aChar == $c ifTrue: [^ model copySelector].
			aChar == $v ifTrue: [^ self browseVersions].
			aChar == $C ifTrue: [^ model showHomeCategory].
			aChar == $O ifTrue: [^ self openSingleMessageBrowser].
			aChar == $x ifTrue: [^ model removeMessage].
			aChar == $t ifTrue: [^ model runMethodTest].
			aChar == $r ifTrue: [^ model debugMethodTest]]
		ifNil: [
			aChar == $R ifTrue: [^ model renameClass]]! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 8/18/2021 13:38:26'!
messageListMenuOptions
	
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'what to show...'.
				#selector 			-> 		#offerWhatToShowMenu.
				#icon 			-> 		#preferencesIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 			-> 		#toggleBreakOnEntry.
				#icon 			-> 		#debugIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'browse full (b)'.
				#selector 			-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		30.
				#label 			-> 		'browse method (O)'.
				#selector 			-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		40.
				#label 			-> 		'browse protocol (p)'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			{
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'inspect CompiledMethod'.
				#object 			-> 		#model.
				#selector 			-> 		#inspectCompiledMethod.
				#icon 			-> 		#exploreIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'senders of... (n)'.
				#selector 			-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'implementors of... (m)'.
				#selector 			-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'inheritance (i)'.
				#selector 			-> 		#methodInheritance.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		40.
				#label 			-> 		'versions (v)'.
				#selector 			-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'inst var refs...'.
				#selector 			-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		20.
				#label 			-> 		'inst var defs...'.
				#selector 			-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		30.
				#label 			-> 		'class var refs...'.
				#selector 			-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		40.
				#label 			-> 		'class variables'.
				#selector 			-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		50.
				#label 			-> 		'class refs (N)'.
				#selector 			-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		10.
				#label 			-> 		'remove method (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		20.
				#label 			-> 		'run test (t)'.
				#object 			-> 		#model.
				#selector 			-> 		#runMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'debug test (r)'.
				#object 			-> 		#model.
				#selector 			-> 		#debugMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		70.
				#itemOrder 			-> 		10.
				#label 			-> 		'more...'.
				#selector 			-> 		#openMessageListMenu2.
				#icon 			-> 		#listAddIcon
			} asDictionary.
		}`.
	! !


!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'jmv 8/18/2021 13:38:36'!
messageListMenu
	| aMenu itemColl |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Message List'.
	itemColl _ OrderedCollection new.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl add:
			{#label -> 'fileIn'. #object -> #model. #selector -> #fileInMessage. #icon -> #updateIcon} asDictionary ].
	itemColl addAll:
		{
			{#label -> 'fileOut'. #object -> #model. #selector -> #fileOutMessage. #icon -> #fileOutIcon} asDictionary. 
			nil
		}.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl addAll:
			{
				{#label -> 'senders (n)'. #selector -> #browseSenders. #icon -> #mailForwardIcon} asDictionary. 
				{#label -> 'implementors (m)'. #selector -> #browseImplementors. #icon -> #developmentIcon} asDictionary. 
				{#label -> 'method inheritance (h)'. #selector -> #methodInheritance. #icon -> #goDownIcon} asDictionary. 
				{#label -> 'versions (v)'. #selector -> #browseVersions. #icon -> #clockIcon} asDictionary
			} ].
	itemColl addAll:
		{
			nil. 
			{#label -> 'remove method (x)'. #object -> #model. #selector -> #removeMessage. #icon -> #deleteIcon} asDictionary
		}.
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.! !


!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 8/18/2021 13:03:57'!
openMessageList: methodReferences label: aString
	"Create a standard system view for the message set on the list, anArray.
	The label of the view is aString."

	| messageSet |
	messageSet _ MessageSet messageList: methodReferences asArray.
	messageSet sortByClassHierarchy.
	^self open: messageSet label: aString.! !

!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 8/18/2021 13:45:03'!
openMessageList: methodReferences label: labelString autoSelect: autoSelectString
	"Open a system view for a MessageSet on messageList."

	| messageSet |

	messageSet _ MessageSet messageList: methodReferences asArray.
	messageSet sortByClassHierarchy.
	messageSet autoSelectString: autoSelectString.

	^self open: messageSet label: labelString ! !


!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 8/18/2021 13:39:15'!
messageMenu
	"Fill aMenu with items appropriate for the message list; could be for a single or double changeSorter"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'message list'.
	aMenu addStayUpIcons.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'delete method from changeSet (d)'.
				#object 			-> 		#model.
				#selector 			-> 		#forget.
				#icon 			-> 		#warningIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove method from system (x)'.
				#object 			-> 		#model.
				#selector 			-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse full (b)'.
				#selector 			-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#label 			-> 		'browse method (O)'.
				#selector 			-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#label 			-> 		'browse protocol (p)'.
				#selector 			-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 			-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'senders of... (n)'.
				#selector 			-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#label 			-> 		'implementors of... (m)'.
				#selector 			-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#label 			-> 		'inheritance (i)'.
				#selector 			-> 		#methodInheritance.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#label 			-> 		'versions (v)'.
				#selector 			-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
		}`.
	^ aMenu! !

!methodRemoval: CodeFileBrowserWindow #methodHierarchy stamp: 'jmv 8/18/2021 13:39:23'!
CodeFileBrowserWindow removeSelector: #methodHierarchy!
!methodRemoval: CodeWindow #methodHierarchy stamp: 'jmv 8/18/2021 13:39:34'!
CodeWindow removeSelector: #methodHierarchy!
!methodRemoval: SystemDictionary #browseMessageList:ofSize:name:autoSelect: stamp: 'jmv 8/18/2021 13:03:13'!
SystemDictionary removeSelector: #browseMessageList:ofSize:name:autoSelect:!
