'From Cuis7.1 [latest update: #6386] on 20 May 2024 at 12:22:20 pm'!

!ProtocolBrowserWindow class methodsFor: 'as yet unclassified' stamp: 'jmv 5/20/2024 12:19:34'!
openFullProtocolForClass: aClass
	"Create and schedule a browser for the entire protocol of the class."
	"
	ProtocolBrowserWindow openFullProtocolForClass: Number.
	"

	| aPBrowser label |

	aPBrowser := ProtocolBrowser new on: aClass.
	label := 'Entire protocol of: ', aClass name.

	^self open: aPBrowser label: label! !


!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/20/2024 12:13:05'!
messageListMenu2
	"Fill aMenu with the items appropriate when the shift key is held down"

	self flag: #assumesKeyboardShortcuts.
	^DynamicMenuBuilder 
		buildTitled: 'Message List' 
		targeting: self 
		collectingMenuOptionsWith: #messageListMenu2Options 
		changingThemWith: [ :options |
			self addExtraMenu2ItemsTo: options.
			model canShowMultipleMessageCategories ifTrue: [	
				options add: `{ 
					#itemGroup 			-> 		40.
					#itemOrder 			-> 		45.		
					#label 			-> 		'show category (C)'.
					#object 			-> 		#model.
					#selector 			-> 		#showHomeCategory.
					#icon 			-> 		#packageIcon
				} asDictionary` ]].
! !


!BrowserWindow class methodsFor: 'browser menues' stamp: 'jmv 5/20/2024 12:12:25'!
messageListMenu2Options
	
	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 			-> 		#toggleDiffing.
				#icon 			-> 		#switchIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'implementors of sent messages'.
				#selector 			-> 		#browseAllMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'local senders of...'.
				#selector 			-> 		#browseLocalSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'local implementors of...'.
				#selector 			-> 		#browseLocalImplementors.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'sample instance'.
				#selector 			-> 		#makeSampleInstance.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'inspect instances'.
				#selector 			-> 		#inspectInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'inspect subinstances'.
				#selector 			-> 		#inspectSubInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		40.
				#label 			-> 		'change category...'.
				#object 			-> 		#model.
				#selector 			-> 		#changeCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		50.
				#label 			-> 		'change sets with this method'.
				#selector 			-> 		#findMethodInChangeSets.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		60.
				#label 			-> 		'revert to previous version'.
				#object 			-> 		#model.
				#selector 			-> 		#revertToPreviousVersion.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'more...'.
				#selector 			-> 		#openMessageListMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.			
		}`! !


!MethodSetWindow methodsFor: 'menu building' stamp: 'jmv 5/20/2024 12:12:46'!
messageListMenu2
	"Fill aMenu with the items appropriate when the shift key is held down"

	self flag: #assumesKeyboardShortcuts.
	^DynamicMenuBuilder 
		buildTitled: 'Message Set' 
		targeting: self 
		collectingMenuOptionsWith: #messageSetMenu2Options 
		changingThemWith: [ :options |
			self addExtraMenu2ItemsTo: options.
			model canShowMultipleMessageCategories ifTrue: [	
				options add: `{ 
					#itemGroup 			-> 		40.
					#itemOrder 			-> 		45.		
					#label 			-> 		'show category (C)'.
					#object 			-> 		#model.
					#selector 			-> 		#showHomeCategory.
					#icon 			-> 		#packageIcon
				} asDictionary` ]].

	! !


!MethodSetWindow class methodsFor: 'browser menues' stamp: 'jmv 5/20/2024 12:12:34'!
messageSetMenu2Options
	
	self flag: #assumesKeyboardShortcuts.
	^`{
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 			-> 		#toggleDiffing.
				#icon 			-> 		#switchIcon
			} asDictionary.
			{
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'implementors of sent messages'.
				#selector 			-> 		#browseAllMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'local senders of...'.
				#selector 			-> 		#browseLocalSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'local implementors of...'.
				#selector 			-> 		#browseLocalImplementors.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'sample instance'.
				#selector 			-> 		#makeSampleInstance.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'inspect instances'.
				#selector 			-> 		#inspectInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		30.
				#label 			-> 		'inspect subinstances'.
				#selector 			-> 		#inspectSubInstances.
				#icon 			-> 		#inspectIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		40.
				#label 			-> 		'change category...'.
				#object 			-> 		#model.
				#selector 			-> 		#changeCategory.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		50.
				#label 			-> 		'change sets with this method'.
				#selector 			-> 		#findMethodInChangeSets.
				#icon 			-> 		#changesIcon
			} asDictionary.
			{
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		60.
				#label 			-> 		'revert to previous version'.
				#object 			-> 		#model.
				#selector 			-> 		#revertToPreviousVersion.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#itemGroup 			-> 		50.
				#itemOrder 			-> 		10.
				#label 			-> 		'more...'.
				#selector 			-> 		#openMessageListMenu.
				#icon 			-> 		#listAddIcon
			} asDictionary.			
		}`! !

!methodRemoval: MethodSetWindow class #openSubProtocolForClass: stamp: 'jmv 5/20/2024 12:17:52'!
MethodSetWindow class removeSelector: #openSubProtocolForClass:!
!methodRemoval: MethodSetWindow class #openFullProtocolForClass: stamp: 'jmv 5/20/2024 12:20:23'!
MethodSetWindow class removeSelector: #openFullProtocolForClass:!
!methodRemoval: CodeWindow #browseProtocol stamp: 'jmv 5/20/2024 12:17:52'!
CodeWindow removeSelector: #browseProtocol!
!methodRemoval: ProtocolBrowser #onSubProtocolOf: stamp: 'jmv 5/20/2024 12:17:52'!
ProtocolBrowser removeSelector: #onSubProtocolOf:!
!methodRemoval: MethodSet class #open stamp: 'jmv 5/20/2024 12:20:09'!
MethodSet class removeSelector: #open!

!MethodSet class reorganize!
('instance creation' messageList:)
('utilities' isPseudoSelector:)
!


!ProtocolBrowserWindow class reorganize!
('opening')
('as yet unclassified' openFullProtocolForClass:)
!

