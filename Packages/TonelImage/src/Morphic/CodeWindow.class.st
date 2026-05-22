"
A hierarchy of morphic views, parallel to the CodeProvider models hierarchy. See category 'GUI building'.
"
Class {
	#name : 'CodeWindow',
	#superclass : 'SystemWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'top window' }
CodeWindow >> activateAndSendTopToBack: aBoolean [
	super activateAndSendTopToBack: aBoolean.
	self decorateButtons
]

{ #category : 'GUI building' }
CodeWindow >> addShiftClickEventHandlerFor: codePane [

	^ codePane scroller setProperty: #mouseButton1Up:localPosition: toValue: [ :event :mousePosition |
		event shiftPressed
			ifTrue: [ self shiftClickPressedOn: codePane at: mousePosition. true ]
			ifFalse: [ false ]]
]

{ #category : 'menu commands' }
CodeWindow >> browseClassRefs [

	model selectedClass ifNotNil: [ :cls | 
		Smalltalk browseAllCallsOnClass: cls theNonMetaClass]
]

{ #category : 'menu commands' }
CodeWindow >> browseClassVarRefs  [

	model selectedClass ifNotNil: [ :cls |
		cls browseClassVarRefs ]
]

{ #category : 'menu commands' }
CodeWindow >> browseFullProtocol [
	"Create and schedule a new protocol browser on the currently selected class or meta."

	| aPBrowser label |
	model selectedClassOrMetaClass ifNotNil: [ :classOrMetaclass |
		aPBrowser := ProtocolBrowser new on: classOrMetaclass.
		label := 'Entire protocol of: ', classOrMetaclass name.
		ProtocolBrowserWindow open: aPBrowser label: label ]
]

{ #category : 'menu commands' }
CodeWindow >> browseHierarchy [
	"Create and schedule a new hierarchy browser
	on the currently selected class or meta."

	| answer |
	answer := nil.
	model selectedClassOrMetaClass ifNotNil: [ :myClass |
		answer := Smalltalk browseHierarchy: myClass
			selector: model selectedMessageName ].
	^answer
]

{ #category : 'menu commands' }
CodeWindow >> browseHierarchyAndReplaceMe [
	"Create and schedule a new hierarchy browser on the currently selected class or meta.
	Resize and position it over me and dismiss me."

	self browseHierarchy ifNotNil: [ :newBrowserWindow |
		newBrowserWindow
			positionInWorld: self positionInWorld;
			extentInWorld: self extentInWorld.
		self dismissMorph ].
]

{ #category : 'menu commands' }
CodeWindow >> browseImplementors [
	"Create and schedule a message set browser on all implementors of the currently selected message selector. Do nothing if no message is selected."

	model selectedMessageName ifNotNil: [ :messageName |
		Smalltalk browseAllImplementorsOf: messageName]
]

{ #category : 'menu commands' }
CodeWindow >> browseImplementorsOfMessages [
	"Present a menu of all messages sent by the currently selected message. 
	Open a message set browser of all implementors of the message chosen."

	self getSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk
]

{ #category : 'menu commands' }
CodeWindow >> browseInstVarRefs [

	model selectedClassOrMetaClass ifNotNil: [ :cls |
		Smalltalk browseInstVarRefs: cls ]
]

{ #category : 'menu commands' }
CodeWindow >> browseLocalImplementorsOfMessages [
	"Present a menu of all messages sent by the currently selected message. 
	Open a message set browser of all implementors of the message chosen in, above, or below
	the selected class.
	Do nothing if no message is chosen."
	self getSelectorAndSendQuery: #browseAllImplementorsOf:localTo:
		to: Smalltalk
		with: { model selectedClass }
]

{ #category : 'menu commands' }
CodeWindow >> browseLocalSenders [
	"Present a menu of the currently selected message, as well as all
	messages sent by it.  Open a message set browser of all implementors
	of the message chosen in or below the selected class"

	self sendQuery: #browseAllCallsOn:localTo:
		to: Smalltalk
		with: { model selectedClass } 
]

{ #category : 'menu commands' }
CodeWindow >> browseLocalSendersOfMessages [
	"Present a menu of the currently selected message, as well as all
	messages sent by it.  Open a message set browser of all implementors
	of the message chosen in or below the selected class"

	self getSelectorAndSendQuery: #browseAllCallsOn:localTo:
		to: Smalltalk
		with: { model selectedClass }
]

{ #category : 'menu commands' }
CodeWindow >> browseMethodFull [
	"Create and schedule a full Browser and then select
	the current class and message."

	| answer |
	answer := nil.
	model selectedClassOrMetaClass ifNotNil: [ :myClass |
		answer := Smalltalk browseFull: myClass
			selector: model selectedMessageName ].
	^answer
]

{ #category : 'menu commands' }
CodeWindow >> browseMethodFullAndReplaceMe [
	"Create and schedule a full Browser and then select the current class and message.
	Resize and position it over me and dismiss me."

	self browseMethodFull ifNotNil: [ :newBrowserWindow |
		newBrowserWindow
			positionInWorld: self positionInWorld;
			extentInWorld: self extentInWorld.
		self dismissMorph ].
]

{ #category : 'menu commands' }
CodeWindow >> browseSenders [
	"Browse the senders of the selected method."
	
	model selectedMessageName ifNotNil: [ :selector |
		Smalltalk browseAllCallsOn: selector ]
]

{ #category : 'menu commands' }
CodeWindow >> browseSendersOfMessages [
	"Present a menu of the currently selected message, as well as all messages sent by it.  Open a message set browser of all senders of the selector chosen."

	self getSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk
]

{ #category : 'menu commands' }
CodeWindow >> browseSubImplementors [
	"Browse the senders of the selected method."
	
	model selectedMessageName ifNotNil: [ :selector |
		Smalltalk browseAllImplementorsOf: selector subTo: model selectedClass  ]
]

{ #category : 'menu commands' }
CodeWindow >> browseSuperImplementors [
	"Browse the senders of the selected method."
	
	model selectedMessageName ifNotNil: [ :selector |
		Smalltalk browseAllImplementorsOf: selector superTo: model selectedClass  ]
]

{ #category : 'menu commands' }
CodeWindow >> browseUnusedMethods [
	| list |
	list := model unusedMethods ifNil: [ ^self ].
	Smalltalk browseMessageList: list name: 'Unsent Methods in ', model selectedClass name
]

{ #category : 'menu commands' }
CodeWindow >> browseVersions [
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."
	| method class |
	model selectedMessageName isNil ifTrue: [
		^ VersionsBrowserWindow
			browseCommentOf: model selectedClass ].
	method := model currentCompiledMethod.
	"because maybe the method has been removed --- unhelpful"
	method isNil ifTrue: [^self].
	class := method methodClass.
	^ VersionsBrowserWindow 
		browseVersionsOf: method
		class: class theNonMetaClass 
		meta: class isMeta
		category: (class organization categoryOfElement: method selector)
		selector: method selector
]

{ #category : 'GUI building' }
CodeWindow >> buildLowerPanes [
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons := LinearLayoutMorph newColumn.
	(Preferences at: #optionalButtons) ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	(Preferences at: #showAnnotations) ifFalse: [		^codeAndButtons ].
	codeButtonsAndAnnotations := LinearLayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations
]

{ #category : 'GUI building' }
CodeWindow >> buildMorphicAnnotationsPane [

	| aTextMorph |
	aTextMorph := (TextModelMorph
		textProvider: model
		textGetter: #annotation) emptyTextDisplayMessage: 'Class or method annotation'.
	aTextMorph disableEditing.
	model when: #annotationChanged send: #refetch to: aTextMorph model.
	model when: #decorateButtons send: #decorateButtons to: self.
	aTextMorph
		askBeforeDiscardingEdits: false;
		hideScrollBarsIndefinitely.
	^aTextMorph
]

{ #category : 'GUI building' }
CodeWindow >> buildMorphicCodePane [
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
	
	| codePane |
	
	codePane := self createCodePaneMorph.	
	(Preferences at: #shiftClickShowsImplementors)
		ifTrue: [ self addShiftClickEventHandlerFor: codePane ].
		
	^codePane
]

{ #category : 'accessing' }
CodeWindow >> buttonWithSelector: aSelector  [
	"If receiver has a control button with the given action selector answer it, else answer nil.  morphic only at this point"

	^self 
		findDeepSubmorphThat: [ :m |
			(m is: #PluggableButtonMorph) and: [ m actionSelector == aSelector ]]
		ifAbsent: [ ^nil ]
]

{ #category : 'updating' }
CodeWindow >> classAdded: addedClass [

	"Keep default behavior. Subclasses like BrowserWindow, redefine it - Hernan"
	self updateListsAndCode 
]

{ #category : 'updating' }
CodeWindow >> classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName [
	"Do nothing here. Subclasses should implement if necessary"
]

{ #category : 'GUI building' }
CodeWindow >> createCodePaneMorph [

	^ (TextModelMorph
		textProvider: model
		textGetter: #acceptedContents
		textSetter: #contents:notifying:
		selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Smalltalk code (nothing selected?)'
]

{ #category : 'updating' }
CodeWindow >> decorateButtons [
	"Change screen feedback for any buttons in the UI of the receiver that may wish it.  Initially, it is only the Inheritance button that is decorated, but one can imagine others."

	self decorateForInheritance
]

{ #category : 'updating' }
CodeWindow >> decorateForInheritance [
	"Check to see if the currently-viewed method has a super send or an override, and if so, change screen feedback, unless the #decorateBrowserButtons says not to."

	| cm aColor aButton flags buttonColor |
	(aButton := self inheritanceButton) ifNil: [^ self].
	buttonColor := self buttonColor.

	(Preferences at: #decorateBrowserButtons)
		ifFalse: [ ^aButton color: buttonColor ].
	cm := model currentCompiledMethod.
	(cm is: #CompiledMethod)
		ifFalse: [ ^aButton color: buttonColor ].

	flags := 0.
	model isThisAnOverride ifTrue: [ flags := flags bitOr: 4 ].
	cm sendsToSuper ifTrue: [ flags := flags bitOr: 2 ].
	model isThereAnOverride ifTrue: [ flags := flags bitOr: 1 ].
	aColor := {

		"This is NOTan override. There is no super implementation."
		buttonColor.							"no sends to super. there is not override in any subclass"
		`Color tan`.							"no sends to super. there is an override in some subclass"
		`Color red`.							"sends to super. there is no override in any subclass. Error: no super to call (or calls super with a different message)"
		`Color red`.							"sends to super. there is  an override in some subclass. Error: no super to call (or calls super with a different message)"

		"This is an override. There is some super implementation"
		`Color red muchLighter`.			"doesn't have sub; has super but doesn't call it"
		`Color r: 0.94 g: 0.823 b: 0.673`.		"has sub; has super but doesn't call it"
		`Color green muchLighter`.			"doesn't have sub; has super and callsl it"
		`Color blue muchLighter`.			"has sub; has super and callsl it"

	} at: flags + 1.
	Theme current useUniformColors
		ifTrue: [
			aButton color: (self buttonColor mixed: 0.8 with: aColor) ]
		ifFalse: [
			aButton color: aColor ]
]

{ #category : 'GUI building' }
CodeWindow >> defaultAnnotationPaneHeight [
	"Answer the receiver's preferred default height for new annotation panes."

	^ FontFamily defaultLineSpacing * 2 + 8
]

{ #category : 'menu commands' }
CodeWindow >> findMethodInChangeSets [
	"Find and open a changeSet containing the current method."

	| aName |
	(aName := model selectedMessageName) ifNotNil: [
		ChangeSorterWindow
			browseChangeSetsWithClass: model selectedClassOrMetaClass
			selector: aName ]
]

{ #category : 'misc' }
CodeWindow >> getSelectorAndSendQuery: querySelector to: queryPerformer [
	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained as its argument.  If no message is currently selected, then obtain a method name from a user type-in"

	self getSelectorAndSendQuery: querySelector to: queryPerformer with: { }
]

{ #category : 'misc' }
CodeWindow >> getSelectorAndSendQuery: querySelector to: queryPerformer with: queryArgs [
	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained and queryArgs as its arguments.  If no message is currently selected, then obtain a method name from a user type-in"

	| strm array |
	strm := WriteStream on: (array := Array new: queryArgs size + 1).
	strm nextPut: nil.
	strm nextPutAll: queryArgs.

	model selectedMessageName ifNil: [ | selector |
		selector := self request: 'Type selector:' initialAnswer: 'flag:'.
		^ selector isEmpty ifFalse: [
			(Symbol hasInterned: selector
				ifTrue: [ :aSymbol |
					array at: 1 put: aSymbol.
					queryPerformer perform: querySelector withArguments: array])
				ifFalse: [ self inform: 'no such selector']
		]
	].

	self selectMessageAndEvaluate: [:selector |
		array at: 1 put: selector.
		queryPerformer perform: querySelector withArguments: array
	]
]

{ #category : 'accessing' }
CodeWindow >> inheritanceButton [
	"If receiver has an Inheritance button, answer it, else answer nil.  morphic only at this point"

	^ self buttonWithSelector: #methodInheritance
]

{ #category : 'menu commands' }
CodeWindow >> inspectInstances [
	"Inspect all instances of the selected class."

	model selectedClassOrMetaClass ifNotNil: [ :c |
		c theNonMetaClass inspectAllInstances]
]

{ #category : 'menu commands' }
CodeWindow >> inspectSubInstances [
	"Inspect all instances of the selected class and all its subclasses"

	model selectedClassOrMetaClass ifNotNil: [ :c |
		c theNonMetaClass inspectSubInstances]
]

{ #category : 'GUI building' }
CodeWindow >> lineDiffButton [
	"Return a checkbox that lets the user decide whether regular diffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingLineDiffs
		action: #toggleLineDiffing)
			label: 'lineDiffs';
			setBalloonText: self lineDiffButtonHelp
]

{ #category : 'GUI building' }
CodeWindow >> lineDiffButtonHelp [
	^'Show code differences between the file-based method and the in-memory version, line by line.'
]

{ #category : 'menu commands' }
CodeWindow >> makeSampleInstance [
	| aClass nonMetaClass anInstance |
	(aClass := model selectedClassOrMetaClass) ifNil: [^ self].
	nonMetaClass := aClass theNonMetaClass.
	anInstance := nonMetaClass initializedInstance.
	(anInstance isNil and: [nonMetaClass ~~ UndefinedObject]) ifTrue: 
		[^ self inform: 'Sorry, cannot make an instance of ', nonMetaClass name].
	(anInstance is: #Morph)
		ifTrue: [ anInstance openInHand ]
		ifFalse: [ anInstance inspectWithLabel: 'An instance of ', nonMetaClass name ]
]

{ #category : 'menu commands' }
CodeWindow >> methodInheritance [
	"Create and schedule a method browser on the inheritance of implementors."

	| list aClassNonMeta isMeta aClass sel |
	aClass := model selectedClassOrMetaClass.
	sel := model selectedMessageName.
	aClass ifNil: [ ^ nil ].
	sel ifNil: [ ^ nil ].
	aClassNonMeta := aClass theNonMetaClass.
	isMeta := aClassNonMeta ~~ aClass.
	list := OrderedCollection new.
	aClass allSuperclasses reverseDo: [ :cl |
		(cl includesSelector: sel) ifTrue: [
			list addLast: (MethodReference class: cl selector: sel) ]].
	aClassNonMeta
		allSubclassesWithLevelDo: [ :cl :level |
			| theClassOrMeta |
			theClassOrMeta := isMeta
				ifTrue: [ cl class ]
				ifFalse: [ cl ].
			(theClassOrMeta includesSelector: sel) ifTrue: [
				list addLast: (MethodReference class: theClassOrMeta selector: sel) ]]
		startingLevel: 0.
	^ Smalltalk
		browseMessageList: list
		name: 'Implementors of ' , sel, ' in ', aClass name, ' and super / sub classes'.
]

{ #category : 'menu commands' }
CodeWindow >> methodInheritanceAndReplaceMe [
	"Create and schedule a method browser on the inheritance of implementors.
	Resize and position it over me and dismiss me."

	self browseHierarchy ifNotNil: [ :newBrowserWindow |
		newBrowserWindow
			positionInWorld: self positionInWorld;
			extentInWorld: self extentInWorld.
		self dismissMorph ].
]

{ #category : 'initialization' }
CodeWindow >> model: anObject [
	"Set my model and make me me a dependent of the given object.
	Note: The implementors of the sent messages must check if they are still 'alive'. The usual
	way is asking `owner ifNotNil: [ do stuff ]`.
	The reason is that the receiver (i.e. the CodeWindow) may be already closed, and
	without a model, although not yet garbage collected, when the message gets dispatched"

	super model: anObject.

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded: to: self;
		when: #classCommented send: #updateListsAndCode to: self;
		when: #classDefinitionChanged send: #updateListsAndCode to: self;
		when: #classRecategorized send: #updateListsAndCode to: self;
		when: #classRemoved send: #updateListsAndCode to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #updateListsAndCode to: self;
		when: #methodAddedInProtocolTimeStamp send: #updateListsAndCode to: self;
		when: #methodChanged send: #updateListsAndCode to: self;
		when: #methodRemoved send: #updateListsAndCode to: self;
		when: #selectorRecategorized send: #updateListsAndCode to: self.
]

{ #category : 'menu commands' }
CodeWindow >> openSingleMessageBrowser [
	| msgName mr |
	"Create and schedule a message list browser populated only by the currently selected message"

	(msgName := model selectedMessageName) ifNil: [^ self].

	mr := MethodReference
		class: model selectedClassOrMetaClass
		selector: msgName.

	Smalltalk
		browseMessageList: (Array with: mr)
		name: mr stringVersion
]

{ #category : 'GUI building' }
CodeWindow >> optionalButtonRow [
	"Answer a row of control buttons"

	| row buttons widths buttonColor |
	buttons := OrderedCollection new.
	widths := OrderedCollection new.
	buttonColor := self buttonColor.
	self optionalButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button := PluggableButtonMorph
			model: self
			action: tuple third.
		button color: buttonColor.
		button label: tuple second.
		tuple size > 3 ifTrue: [ button setBalloonText: tuple fourth ].
		tuple size > 4 ifTrue: [ button secondaryActionSelector: tuple fifth ].
		buttons add: button ].
	row := LinearLayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row
		addMorphs: buttons
		widthProportionalTo: widths.
	^ row.
]

{ #category : 'GUI building' }
CodeWindow >> optionalButtonTuples [
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList textConstructor |
	textConstructor := [ :string :backgroundColor | 
		string asText addAttribute: (TextBackgroundColor color: backgroundColor) ].

	aList := #(
		(10	'browse'				browseMethodFull								'view this method in a browser'	browseMethodFullAndReplaceMe)
		(11	'senders' 			browseSendersOfMessages					'browse senders of...'				browseSenders)
		(16	'implementors'		browseImplementorsOfMessages				'browse implementors of...' 		browseImplementors)
		(12	'versions'			browseVersions								'browse versions')), 

		((Preferences at: #decorateBrowserButtons)
			ifTrue: [
				{{13	. 'inheritance'.	 #methodInheritance. 	'Browse Method Inheritance

', (textConstructor value: 'green' value: `Color green muchLighter`),': sends to super
', (textConstructor value: 'tan' value: `Color tan`), ': has override(s)
', (textConstructor value: 'mauve' value: `Color blue muchLighter`), ': both of the above
', (textConstructor value: 'pink' value: `Color red muchLighter`), ': is an override but doesn''t call super
', (textConstructor value: 'pinkish tan' value: `Color r: 0.94 g: 0.823 b: 0.673`), ': has override(s), also is an override but doesn''t call super
'. #methodInheritanceAndReplaceMe }}]
			ifFalse: [
				{#(13	'inheritance'	methodInheritance	 'browse method inheritance'. #methodInheritanceAndReplaceMe)}]),

		#(
		(12	'hierarchy'			browseHierarchy								'browse class hierarchy'			browseHierarchyAndReplaceMe)
		(10	'inst vars'			browseInstVarRefs								'instance variable references...')
		(11	'class vars'			browseClassVarRefs							'class variable references...')).

	^ aList
]

{ #category : 'GUI building' }
CodeWindow >> prettyLineDiffButton [
	"Return a checkbox that lets the user decide whether prettyDiffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingPrettyLineDiffs
		action: #togglePrettyLineDiffing)
			label: 'linePrettyDiffs';
			setBalloonText: self prettyLineDiffButtonHelp
]

{ #category : 'GUI building' }
CodeWindow >> prettyLineDiffButtonHelp [
	^'Show pretty-printed code differences between the file-based method and the in-memory version, line by line.'
]

{ #category : 'GUI building' }
CodeWindow >> prettyWordDiffButton [
	"Return a checkbox that lets the user decide whether prettyDiffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingPrettyWordDiffs
		action: #togglePrettyWordDiffing)
			label: 'wordPrettyDiffs';
			setBalloonText: self prettyWordDiffButtonHelp
]

{ #category : 'GUI building' }
CodeWindow >> prettyWordDiffButtonHelp [
	^'Show pretty-printed code differences between the file-based method and the in-memory version, word by word.'
]

{ #category : 'misc' }
CodeWindow >> selectMessageAndEvaluate: aBlock [
	"Allow the user to choose one selector, chosen from the currently selected message's selector, as well as those of all messages sent by it, and evaluate aBlock on behalf of chosen selector.  If there is only one possible choice, simply make it; if there are multiple choices, put up a menu, and evaluate aBlock on behalf of the the chosen selector, doing nothing if the user declines to choose any"

	| selector method messages |
	(selector := model selectedMessageName) ifNil: [^ self].
	method := (model selectedClassOrMetaClass ifNil: [^ aBlock value: selector])
		compiledMethodAt: selector
		ifAbsent: nil.
	(method isNil or: [(messages := method messages) isEmpty])
		 ifTrue: [^ aBlock value: selector].
	(messages size = 1 and: [messages includes: selector])
		ifTrue:
			[^ aBlock value: selector].  "If only one item, there is no choice"

	Smalltalk
		showMenuOf: messages
		withFirstItem: selector
		ifChosenDo: aBlock
]

{ #category : 'misc' }
CodeWindow >> sendQuery: querySelector to: queryPerformer [
	"Apply a query to the primary selector associated with the current context.  If no such selection exists, obtain one by user type-in. Then send querySelector to queryPerformer with the selector as its argument."

	model selectedMessageName ifNotNil: [:aSymbol| ^ queryPerformer perform: querySelector with: aSymbol].
	self request: 'Type selector:' initialAnswer: 'flag:' verifying: [:aString| aString notEmpty] do: [:aString|
		(Symbol hasInterned: aString ifTrue: [ :aSymbol | queryPerformer perform: querySelector with: aSymbol])
			ifFalse: [self inform: 'no such selector']]
]

{ #category : 'misc' }
CodeWindow >> sendQuery: querySelector to: queryPerformer with: queryArgs [
	"Apply a query to the primary selector associated with the current context.  If no such selection exists, obtain one by user type-in. Then send querySelector to queryPerformer with the selector as its argument."
	|argsBlock|
	argsBlock := [:aSymbol | |strm array|
		strm := WriteStream on: (array := Array new: queryArgs size + 1).
		strm nextPut: aSymbol.
		strm nextPutAll: queryArgs.
		array].

	model selectedMessageName ifNotNil: [:aSymbol| ^ queryPerformer perform: querySelector withArguments: (argsBlock value: aSymbol)].
	self request: 'Type selector:' initialAnswer: 'flag:' verifying: [:aString| aString notEmpty] do: [:aString|
		(Symbol hasInterned: aString ifTrue: [ :aSymbol | queryPerformer perform: querySelector withArguments: (argsBlock value: aSymbol)])
			ifFalse: [self inform: 'no such selector']]
]

{ #category : 'menu commands' }
CodeWindow >> shiftClickPressedOn: aCodePane at: aMousePosition [
	
	aCodePane editor implementorsOfNodeUnder: aMousePosition 
]

{ #category : 'menu commands' }
CodeWindow >> showUnreferencedClassVars [
	"Search for all class variables known to the selected class, and put up a 
	list of those that have no references anywhere in the system. The 
	search includes superclasses, so that you don't need to navigate your 
	way to the class that defines each class variable in order to determine 
	whether it is unreferenced"
	| cls aList |
	(cls := model selectedClass)
		ifNil: [^ self].
	aList := cls allUnreferencedClassVariables.
	aList isEmpty
		ifTrue: [^ self inform: 'There are no unreferenced
class variables in
' , cls name].
	Transcript newLine; nextPutAll: 'Unreferenced class variable(s) in ', cls name; newLine.
	aList do: [:el | Transcript tab; nextPutAll: el; newLine].
	(SelectionMenu labels: aList selections: aList)
		startUpWithCaption: 'Unreferenced
class variables in 
' , cls name
]

{ #category : 'menu commands' }
CodeWindow >> showUnreferencedInstVars [
	"Search for all instance variables known to the selected class, and put up a list of those that have no references anywhere in the system.  The search includes superclasses, so that you don't need to navigate your way to the class that defines each inst variable in order to determine whether it is unreferenced"

	| cls aList |
	(cls := model selectedClassOrMetaClass) ifNil: [^ self].
	aList := cls allUnreferencedInstanceVariables.
	aList isEmpty ifTrue: [^ self inform: 'There are no unreferenced
instance variables in
', cls name].
	Transcript newLine; nextPutAll: 'Unreferenced instance variable(s) in ', cls name; newLine.
	aList do: [ :el | Transcript tab; nextPutAll: el; newLine ].
	(SelectionMenu labels: aList selections: aList) startUpWithCaption: 'Unreferenced
instance variables in 
', cls name
]

{ #category : 'updating' }
CodeWindow >> updateListsAndCode [
	"All code windows receive this message on any code change in the system.
	Process it only once, for the benefit of installing large packages!"

	owner ifNotNil: [
		(self hasProperty: #updateListsAndCode) ifFalse: [
			self setProperty: #updateListsAndCode toValue: true.
			self whenUIinSafeState: [
				self removeProperty: #updateListsAndCode.
				owner ifNotNil: [ self updateListsAndCodeNow ]]]].
]

{ #category : 'updating' }
CodeWindow >> updateListsAndCodeNow [
	"All code windows receive this message on any code change in the system"
	self canDiscardEdits ifTrue: [
		self allMorphsDo: [ :m | (m is: #PluggableListMorph) ifTrue: [ m verifyContents ]].
		model updateIfNeeded ]
]

{ #category : 'misc' }
CodeWindow >> useSelector: incomingSelector orGetSelectorAndSendQuery: querySelector to: queryPerformer [
	"If incomingSelector is not nil, use it, else obtain a selector from user type-in.   Using the determined selector, send the query to the performer provided."

	incomingSelector ifNotNil: [^ queryPerformer perform: querySelector with: incomingSelector].
	self request: 'Type selector:' initialAnswer: 'flag:' verifying: [:aString| aString notEmpty] do: [:aString|
		(Symbol hasInterned: aString ifTrue: [:aSymbol| queryPerformer perform: querySelector with: aSymbol])
			ifFalse: [self inform: 'no such selector']]
]

{ #category : 'GUI building' }
CodeWindow >> wordDiffButton [
	"Return a checkbox that lets the user decide whether regular diffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingWordDiffs
		action: #toggleWordDiffing)
			label: 'wordDiffs';
			setBalloonText: self wordDiffButtonHelp
]

{ #category : 'GUI building' }
CodeWindow >> wordDiffButtonHelp [
	^'Show code differences between the file-based method and the in-memory version, word by word.'
]
