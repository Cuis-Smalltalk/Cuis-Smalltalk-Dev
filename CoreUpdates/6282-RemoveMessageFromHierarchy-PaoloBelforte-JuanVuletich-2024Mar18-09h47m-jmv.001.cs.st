'From Cuis6.3 [latest update: #6281] on 18 March 2024 at 10:08:42 am'!

!Browser methodsFor: 'message functions' stamp: 'jmv 3/18/2024 10:07:52'!
removeMessageInHierarchy
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If so,  
	remove it. If the Preference 'confirmMethodRemoves' is set to false, the 
	confirmer is bypassed."
	
	| messageName confirmation selectedClass |
	selectedMessage ifNil: [ ^self ].
	
	messageName := self selectedMessageName.
	selectedClass := self selectedClassOrMetaClass.
	confirmation := Smalltalk confirmRemovalOf: messageName on: selectedClass andSubclasses: true.
	confirmation = 3
		ifTrue: [^ self].

	selectedClass withAllSubclassesDo: [ :eachClass | eachClass removeSelector: messageName ].
	
	self reformulateList.
	self changed: #messageList.
	self setClassOrganizer.
	confirmation = 2
		ifTrue: [Smalltalk browseAllCallsOn: messageName]! !


!SystemDictionary methodsFor: 'ui' stamp: 'jmv 3/18/2024 10:05:45'!
confirmRemovalOf: aSelector on: aClass andSubclasses: aBoolean
	"Determine if it is okay to remove the given selector. Answer 1 if it  
	should be removed, 2 if it should be removed followed by a senders  
	browse, and 3 if it should not be removed."
	| count aMenu labels answer caption allCalls |
	allCalls := self allCallsOn: aSelector.
	(count := allCalls size) = 0
		ifTrue: [^ 1].
	"no senders -- let the removal happen without warning"
	count = 1
		ifTrue: [(allCalls first actualClass == aClass
					and: [allCalls first methodSymbol == aSelector])
				ifTrue: [^ 1]].
	"only sender is itself"
	labels := aBoolean
		ifTrue: [
'Remove from ', aClass name, ' and subclasses
Remove them, then browse senders
Don''t remove, but show me those senders
Forget it -- do nothing -- sorry I asked']
		ifFalse: [
'Remove it
Remove, then browse senders
Don''t remove, but show me those senders
Forget it -- do nothing -- sorry I asked'].
	aMenu := PopUpMenu labels: labels
		icons: #(acceptIcon acceptIcon cancelIcon cancelIcon).

	caption := 'This message has ' , count printString , ' sender'.
	count > 1
		ifTrue: [caption := caption copyWith: $s].
	answer := aMenu startUpWithCaption: caption.
	answer = 3
		ifTrue: [self
				browseMessageList: allCalls
				name: 'Senders of ' , aSelector
				autoHighlight: aSelector keywords first
				allOccurrences: false ].
	answer = 0
		ifTrue: [answer := 3].
	"If user didn't answer, treat it as cancel"
	^ answer min: 3! !


!SystemDictionary methodsFor: 'ui' stamp: 'jmv 3/18/2024 10:02:47'!
confirmRemovalOf: aSelector on: aClass 
	"Determine if it is okay to remove the given selector. Answer 1 if it  
	should be removed, 2 if it should be removed followed by a senders  
	browse, and 3 if it should not be removed."

	^self confirmRemovalOf: aSelector on: aClass andSubclasses: false! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'PB 3/16/2024 17:18:31'!
messageListMenuOptions

	self flag: #assumesKeyboardShortcuts.
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
			{	#itemGroup  			-> 		20.
				#itemOrder  			-> 		50.
				#label            			-> 		'browse method category (c)'.
				#selector       		-> 			#browseCategoryOfCurrentMethod.
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
				#label 			-> 		'remove method in hierarchy'.
				#object 			-> 		#model.
				#selector 			-> 		#removeMessageInHierarchy.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		30.
				#label 			-> 		'run test (t)'.
				#object 			-> 		#model.
				#selector 			-> 		#runMethodTest.
				#icon 			-> 		#weatherFewCloudsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		60.
				#itemOrder 			-> 		40.
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

