'From Haver 5.0 [latest update: #4990] on 30 November 2021 at 12:25:20 pm'!

!CodeWindow methodsFor: 'menu commands' stamp: 'KLG 11/30/2021 12:08:42'!
browseSendersOfMethod
	"Browse the senders of the selected method."
	
	model selectedMessageName ifNotNil: [ :selector |
		Smalltalk browseAllCallsOn: selector ]! !


!CodeWindow methodsFor: 'GUI building' stamp: 'KLG 11/30/2021 11:51:01'!
optionalButtonRow
	"Answer a row of control buttons"

	| row buttons widths buttonColor |
	buttons _ OrderedCollection new.
	widths _ OrderedCollection new.
	buttonColor _ self buttonColor.
	self optionalButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button _ PluggableButtonMorph
			model: self
			stateGetter: nil
			action: tuple third.
		button color: buttonColor.
		button label: tuple second asString.
		tuple size > 3 ifTrue: [ button setBalloonText: tuple fourth ].
		tuple size > 4 ifTrue: [ button secondaryActionSelector: tuple fifth ].
		buttons add: button ].
	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row
		addMorphs: buttons
		widthProportionalTo: widths.
	^ row.! !

!CodeWindow methodsFor: 'GUI building' stamp: 'KLG 11/30/2021 12:16:33'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList textConstructor |
	textConstructor _ [ :string :backgroundColor | 
		string asText addAttribute: (TextBackgroundColor color: backgroundColor) ].

	aList _ #(
	(10	'browse'			browseMethodFull				'view this method in a browser')
	(11	'senders' 			browseSendersOfMessages	'browse senders of...' browseSendersOfMethod)
	(16	'implementors'		browseMessages				'browse implementors of...' browseImplementors)
	(12	'versions'			browseVersions					'browse versions')), 

	(Preferences decorateBrowserButtons
		ifTrue:
			[{{13	. 'inheritance'.	 #methodInheritance. 	'browse method inheritance

', (textConstructor value:'green' value: `Color green muchLighter`),': sends to super
', (textConstructor value: 'tan' value: `Color tan`), ': has override(s)
', (textConstructor value: 'mauve' value: `Color blue muchLighter`), ': both of the above
', (textConstructor value: 'pink' value: `Color red muchLighter`), ': is an override but doesn''t call super
', (textConstructor value: 'pinkish tan' value: `Color r: 0.94 g: 0.823 b: 0.673`), ': has override(s), also is an override but doesn''t call super'}}]
		ifFalse:
			[{#(13	'inheritance'		methodInheritance	'browse method inheritance')}]),

	#(
	(12	'hierarchy'			browseHierarchy				'browse class hierarchy')
	(10	'inst vars'			browseInstVarRefs				'inst var refs...')
	(11	'class vars'			browseClassVarRefs			'class var refs...')
	(10	'show...'				offerWhatToShowMenu		'menu of what to show in lower pane')).

	^ aList! !


!CodeWindow reorganize!
('GUI building' buildLowerPanes buildMorphicAnnotationsPane buildMorphicCodePane defaultAnnotationPaneHeight lineDiffButton lineDiffButtonHelp optionalButtonRow optionalButtonTuples prettyLineDiffButton prettyLineDiffButtonHelp prettyWordDiffButton prettyWordDiffButtonHelp wordDiffButton wordDiffButtonHelp)
('accessing' buttonWithSelector: inheritanceButton)
('updating' classAdded: classRenamed:from:to:inCategory: decorateButtons decorateForInheritance updateListsAndCode updateListsAndCodeNow)
('top window' activateAndSendTopToBack:)
('menu commands' browseAllMessages browseClassRefs browseClassVarRefs browseClassVariables browseFullProtocol browseHierarchy browseImplementors browseInstVarDefs browseInstVarRefs browseLocalImplementors browseLocalSendersOfMessages browseMessages browseMethodFull browseProtocol browseSenders browseSendersOfMessages browseSendersOfMethod browseUnusedMethods browseVersions findMethodInChangeSets inspectInstances inspectSubInstances makeSampleInstance methodInheritance openSingleMessageBrowser showUnreferencedClassVars showUnreferencedInstVars)
('menu building' addContentsTogglesTo: offerClassListMenu offerClassListMenu2 offerWhatToShowMenu openMessageListMenu openMessageListMenu2)
('misc' getSelectorAndSendQuery:to: getSelectorAndSendQuery:to:with: selectMessageAndEvaluate: sendQuery:to: useSelector:orGetSelectorAndSendQuery:to:)
('keyboard shortcuts' classListKey:from: messageCatListKey:from: messageListKey:from: systemCatListKey:from:)
('initialization' model:)
('notification actions' initializeNotificationActions registerNotificationActions registerNotificationActionsIfModelNotNil removeNotificationActions)
!

