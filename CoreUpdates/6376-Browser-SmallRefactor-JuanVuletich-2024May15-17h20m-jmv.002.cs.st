'From Cuis7.1 [latest update: #6367] on 15 May 2024 at 5:41:01 pm'!

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/15/2024 15:26:15'!
openMessageListMenu
	"Offer the unshifted selector-list menu"

	^ self messageListMenu popUpInWorld! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/15/2024 15:26:42'!
openMessageListMenu2
	"Offer the additional selector-list menu"

	^ self messageListMenu2 popUpInWorld! !

!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/15/2024 15:33:24'!
classListKey: aChar from: view 
	"Respond to a Command key.  I am a model with a list of classes and a 
	code pane, and I also have a listView that has a list of methods.  The 
	view knows how to get the list and selection."

	self flag: #definesKeyboardShortcuts.
	aChar = $r ifTrue: [^ model recent].
	aChar = $h ifTrue: [^ self browseHierarchy].
	aChar = $x ifTrue: [^ model removeClass].
	aChar = $t ifTrue: [^ model runClassTests ].
	
	^ self messageListKey: aChar from: view! !

!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/15/2024 15:31:14'!
messageCatListKey: aChar from: view

	self flag: #definesKeyboardShortcuts.
	aChar = $o ifTrue: [ ^ model fileOutMessageCategories ].
	aChar = $t ifTrue: [ ^ model runMessageCategoryTests ].
	aChar = $x ifTrue: [ ^ model removeMessageCategory ].
	aChar = $R ifTrue: [ ^ model renameCategory ].
	aChar = $n ifTrue: [ ^ model addCategory ].
	aChar = $N ifTrue: [ ^ model addExtensionCategory ].
	aChar = $e ifTrue: [ ^ model removeEmptyCategories ].
	aChar = $c ifTrue: [ ^ model categorizeAllUncategorizedMethods ].
	aChar = $a ifTrue: [ ^ model alphabetizeMessageCategories ].
	aChar = $r ifTrue: [ ^ model editMessageCategories ]! !


!MethodSetWindow methodsFor: 'menu building' stamp: 'jmv 5/15/2024 15:26:16'!
openMessageListMenu
	"Offer the unshifted selector-list menu"

	^ self messageListMenu popUpInWorld! !

!MethodSetWindow methodsFor: 'menu building' stamp: 'jmv 5/15/2024 15:26:41'!
openMessageListMenu2
	"Offer the additional selector-list menu"

	^ self messageListMenu2 popUpInWorld! !


!BrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 5/15/2024 15:30:12'!
systemCatListKey: aChar from: view

	self flag: #definesKeyboardShortcuts.
	aChar = $r ifTrue: [^ model recent ].
	aChar = $f ifTrue: [ ^ self findClass ].
	aChar = $x ifTrue: [ ^ model removeSystemCategory ].
	aChar = $t ifTrue: [ ^ model runSystemCategoryTests ].
	aChar = $a ifTrue: [ ^ model addSystemCategory ].
	aChar = $A ifTrue: [ ^ model alphabetizeSystemCategories ].
	aChar = $b ifTrue: [ ^ self openSystemCategoryBrowser ].
	aChar = $B ifTrue: [ ^ self browseAllClasses ].
	aChar = $o ifTrue: [ ^ model fileOutSystemCategory ].
	aChar = $u ifTrue: [ ^ model updateSystemCategories ].
	aChar = $R ifTrue: [ ^ model renameSystemCategory ].
	
	^ self classListKey: aChar from: view.! !

