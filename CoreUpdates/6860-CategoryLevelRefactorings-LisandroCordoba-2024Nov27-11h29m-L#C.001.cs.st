'From Cuis7.1 [latest update: #6859] on 27 November 2024 at 11:30:36 am'!

!Browser methodsFor: 'refactorings - category' stamp: 'L C 11/19/2024 18:14:33'!
addCategoryAsSubclassResponsibilitySelectors


	| methodsOfCategory |
			
	methodsOfCategory := self messageList.
	
	methodsOfCategory do: [ :selectedSelector | selectedSelector ifNotNil: 
			[(AddAsSubclassResponsibilityApplier on: self for: self selectedClassOrMetaClass>>selectedSelector) value ]
		].! !

!Browser methodsFor: 'refactorings - category' stamp: 'L C 11/19/2024 18:24:23'!
pushDownCategorySelectorsToOneSubclass
	
	self messageList do: [ :selectedSelector | selectedSelector ifNotNil: 
		[(PushDownMethodToOneSubclassApplier on: self for: self selectedClassOrMetaClass>>selectedSelector) value ]
	].
	
	self removeMessageCategory.! !

!Browser methodsFor: 'refactorings - category' stamp: 'L C 11/19/2024 18:24:03'!
pushDownCategorySelectorsToSubclasses
	
		self messageList do: [ :selectedSelector | selectedSelector ifNotNil: 
			[(PushDownMethodToSubclassesApplier on: self for: self selectedClassOrMetaClass>>selectedSelector) value] 
		].
	
		self removeMessageCategory.
! !

!Browser methodsFor: 'refactorings - category' stamp: 'L C 11/19/2024 18:14:54'!
pushUpCategorySelectors

	| methodsOfCategory |

	methodsOfCategory := self messageList.
	
	methodsOfCategory do: [:selectedSelector |  (PushUpMethodApplier on: self for: self selectedClassOrMetaClass>>selectedSelector) value ].
	
	self removeMessageCategory 
! !


!BrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'L C 11/27/2024 11:23:31'!
messageCatListMenuSpec
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	^#(
		#(0 		'Message Categories' 			false)
		#(5		'Browse Package (p)' 				$p 	window 	browsePackage 						packageIcon)
		#(6		'-----')
		#(10	'File Out (o)' 						$o 	model 		fileOutMessageCategories 				fileOutIcon)
		#(20 	'-----')
		#(30	'Reorganize (r)' 					$r 	model 		editMessageCategories 				sendReceiveIcon)
		#(40	'Alphabetize (a)' 					$a 	model 		alphabetizeMessageCategories 		fontXGenericIcon)
		#(50	'Remove empty Categories (e)' 	$e 	model 		removeEmptyCategories 				listRemoveIcon)
		#(60	'Categorize all Uncategorized (c)' 	$c 	model 		categorizeAllUncategorizedMethods 	packageIcon)
		#(70	'New Category... (n)' 				$n 	model 		addCategory 							newIcon)
		#(80	'New extension Category... (N)' 	$N 	model 		addExtensionCategory 					listAddIcon)
		#(90 	'-----')
		#(100	'Refactorings' 					nil 	nil 			1000 									listAddIcon)
		#(110	'Rename... (R)' 					$R 	model 		renameCategory 						saveAsIcon)
		#(120	'Remove (x)' 					$x 	model 		removeMessageCategory 				deleteIcon)
		#(130 	'-----')
		#(140	'Run Tests (t)' 					$t 	model 		runMessageCategoryTests 			weatherFewCloudsIcon)

		" 'Refactorings' SubMenu"
		#(1010	'Push Up' 									nil 	model 	pushUpCategorySelectors 							goTopIcon)
		#(1020	'Add in Superclass as subclassResponsibility' 	nil	model 	addCategoryAsSubclassResponsibilitySelectors 	goTopIcon)
		#(1030	'Push down to subclasses' 					nil 	model 	pushDownCategorySelectorsToSubclasses 		goBottomIcon)
		#(1040	'Push down to one subclass' 					nil 	model 	pushDownCategorySelectorsToOneSubclass 		goBottomIcon)
	)! !

