'From Cuis 5.0 [latest update: #3956] on 12 November 2019 at 11:29:19 am'!
!classDefinition: #PushDownMethod category: #'Tools-Refactoring'!
Refactoring subclass: #PushDownMethod
	instanceVariableNames: 'method'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!BrowserWindow methodsFor: 'refactorings' stamp: 'HAW 11/12/2019 11:25:59'!
pushDownSelector

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(PushDownMethod for: model selectedClassOrMetaClass>>selectedSelector) apply.
		model reformulateList.
		model changed: #messageList.
		model setClassOrganizer ].! !


!PushDownMethod methodsFor: 'initialization' stamp: 'HAW 11/12/2019 11:26:37'!
initializeFor: aMethodToPushDown

	method := aMethodToPushDown ! !

!PushDownMethod methodsFor: 'applying' stamp: 'HAW 11/12/2019 11:27:19'!
apply

	| methodCategory |

	methodCategory := method methodClass organization categoryOfElement: method selector.
	method methodClass subclassesDo: [:subClass |
		subClass
			compile: method sourceCode
			classified: methodCategory.
	].

	method methodClass removeSelector: method selector.
 ! !


!PushDownMethod class methodsFor: 'instance creation' stamp: 'HAW 11/12/2019 11:26:28'!
for: aMethodToPushDown

	^self new initializeFor: aMethodToPushDown ! !


!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 11/12/2019 11:28:03'!
messsageRefactoringMenuOptions

	^ `{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename... (R)'.
				#selector 		-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		15.
				#label 			-> 		'change keyword order...'.
				#selector 		-> 		#changeKeywordOrder.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'add parameter...'.
				#selector 		-> 		#addParameter.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove parameter...'.
				#selector 		-> 		#removeParameter.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		40.
				#label 			-> 		'push up'.
				#selector 		-> 		#pushUpSelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		50.
				#label 			-> 		'push down'.
				#selector 		-> 		#pushDownSelector.
				#icon 			-> 		#goBottomIcon
			} asDictionary.
	}`.

	! !

