'From Cuis7.1 [latest update: #6421] on 31 May 2024 at 3:23:58 pm'!

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/31/2024 14:28:07'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel class |
	self flag: #definesKeyboardShortcuts.
	aChar = $D ifTrue: [^ model toggleDiffing].

	sel := model selectedMessageName.
	aChar = $m ifTrue: [  "These next two put up a type in if no message selected"
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk].
	aChar = $n ifTrue: [
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk].

	"The following require a class selection"
	(class := model selectedClassOrMetaClass) ifNil: [^ self ].
	aChar = $b ifTrue: [^ BrowserWindow fullOnClass: class selector: sel].
	aChar = $N ifTrue: [^ self browseClassRefs].
	aChar = $h ifTrue: [^ self browseHierarchy].
	aChar = $p ifTrue: [^ self browseFullProtocol].

	sel
		ifNotNil: [
			"The following require a method selection"
			aChar = $i ifTrue: [^ self methodInheritance].
			aChar = $o ifTrue: [^ model fileOutMessage].
			aChar = $c ifTrue: [^ model copySelector].
			aChar = $v ifTrue: [^ self browseVersions].
			aChar = $C ifTrue: [^ model showHomeCategory].
			aChar = $O ifTrue: [^ self openSingleMessageBrowser].
			aChar = $x ifTrue: [^ model removeMessage].
			aChar = $t ifTrue: [^ model runMethodTest].
			aChar = $r ifTrue: [^ model debugMethodTest]]
		ifNil: [
			aChar = $R ifTrue: [^ model renameClass]]! !


!InspectorWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/31/2024 14:15:24'!
menuSpecForDictionary
	"Spec for the menu and shortcut keys for the list of variables in our object.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	^#(
		#(100 		'-----')
		#(110		'inspect key' 					nil 	window 			inspectSelectedKey 							nil 	editFindReplaceIcon)
		#(120		'senders of this key' 					nil 	window 			sendersOfSelectedKey 							nil 	mailForwardIcon)
		#(130		'add key' 					nil 	window 			addEntry 							nil 	listAddIcon)
		#(140		'rename key' 					nil 	window 			renameEntry 							nil 	saveAsIcon)
		#(150		'remove' 					nil 	model 			removeSelection 							nil 	listRemoveIcon)
	)! !

