'From Cuis 5.0 [latest update: #4695] on 26 July 2021 at 10:42:52 am'!

!MessageSet methodsFor: 'message list' stamp: 'jmv 7/26/2021 10:39:19'!
sortByClass
	"Sort the message-list by class / selector"
	
	messageList _ messageList sort: [ :a :b |
		a classSymbol = b classSymbol
			ifTrue: [
				a methodSymbol == #Comment
					ifTrue: [ true ]
					ifFalse: [
						b methodSymbol == #Comment
							ifTrue: [ false ]
							ifFalse: [
								a classIsMeta = b classIsMeta
									ifTrue: [ a methodSymbol < b methodSymbol ]
									ifFalse: [ a classIsMeta ]	]]]
			ifFalse: [ a classSymbol < b classSymbol ]
		].
	messageList do: [ :each | each removeStringVersionPrefix ].
	self changed: #messageList! !

!MessageSet methodsFor: 'message list' stamp: 'jmv 7/26/2021 10:38:30'!
sortBySelector
	"Sort the message-list by selector / class"
	
	messageList _ messageList sort: [ :a :b |
		a methodSymbol == #Comment
			ifTrue: [ true ]
			ifFalse: [
				b methodSymbol == #Comment
					ifTrue: [ false ]
					ifFalse: [
						a methodSymbol = b methodSymbol
							ifTrue: [
								a classSymbol = b classSymbol
									ifTrue: [ a classIsMeta ]
									ifFalse: [ a classSymbol < b classSymbol ]]
							ifFalse: [ a methodSymbol < b methodSymbol ]]]
		].
	messageList do: [ :each | each prefixStringVersionWith: each methodSymbol ].
	self changed: #messageList! !


!MethodReference methodsFor: 'setting' stamp: 'jmv 7/26/2021 10:36:08'!
removeStringVersionPrefix

	| i prefixCoda |
	prefixCoda _ '] - '.
	i _ stringVersion findString: prefixCoda.
	i = 0 ifFalse: [
		stringVersion _ stringVersion copyFrom: i + prefixCoda size to: stringVersion size ].! !


!MessageSet methodsFor: 'message list' stamp: 'jmv 7/26/2021 10:41:18'!
sortByDate
	"Sort the message-list by date of time-stamp"

	| assocs aCompiledMethod aDate inOrder |
	assocs _ messageList collect: [ :aRef |
		aDate _ aRef methodSymbol == #Comment
			ifTrue: [
				aRef actualClass organization dateCommentLastSubmitted]
			ifFalse: [
				aCompiledMethod _ aRef actualClass compiledMethodAt: aRef methodSymbol ifAbsent: nil.
				aCompiledMethod ifNotNil: [aCompiledMethod dateMethodLastSubmitted]].
		aRef -> (aDate ifNil: [Date fromString: '01/01/1996'])].  "The dawn of Squeak history"
	inOrder _ assocs asArray sort: [ :a :b | a value < b value].

	inOrder do: [ :each | each key prefixStringVersionWith: each value yyyymmdd ].
	messageList _ inOrder collect: [ :assoc | assoc key ].
	self changed: #messageList! !


!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 7/26/2021 10:33:47'!
addReferencesOf: anInstVarName at: anInstVarIndex to: references

	| reference |
	
	self methodsDo: [ :aMethod |
		(aMethod accessorDescriptionOf: anInstVarName at: anInstVarIndex) ifNotEmpty: [ :description |
			reference := MethodReference method: aMethod.
			reference prefixStringVersionWith: description.
			references add: reference ]].
	! !


!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/26/2021 09:52:12'!
messageListMenu2
	"Fill aMenu with the items appropriate when the shift key is held down"

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


!MessageSetWindow methodsFor: 'menu building' stamp: 'jmv 7/26/2021 09:58:13'!
addExtraMenu2ItemsTo: options
	"The shifted selector-list menu is being built.  Add items specific to MessageSet"

	model growable ifTrue: [
		options add: `{
			#itemGroup 			-> 		40.
			#itemOrder 			-> 		31.		
			#label 			-> 		'remove from this browser'.
			#object 			-> 		#model.
			#selector 			-> 		#removeMessageFromBrowser.
			#icon 			-> 		#listRemoveIcon 
		} asDictionary`.
		options add:`{
			#itemGroup 			-> 		40.
			#itemOrder 			-> 		32.		
			#label 			-> 		'filter message list...'.
			#selector 			-> 		#filterMessageList.
			#icon 			-> 		#findIcon
		} asDictionary`	].
	
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		33.		
			#label 			-> 		'sort by class'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortByClass.
			#icon 			-> 		#dateIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		34.		
			#label 			-> 		'sort by selector'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortBySelector.
			#icon 			-> 		#dateIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		35.		
			#label 			-> 		'sort by date'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortByDate.
			#icon 			-> 		#dateIcon
		} asDictionary`.

! !


!MethodReference methodsFor: 'setting' stamp: 'jmv 7/26/2021 10:36:43'!
prefixStringVersionWith: aString

	self removeStringVersionPrefix.
	stringVersion _ '[', aString, '] - ', stringVersion.! !

!MethodReference methodsFor: 'comparisons' stamp: 'jmv 7/26/2021 10:26:17'!
<= anotherMethodReference
	"By default, sort by class"

	^self classSymbol = anotherMethodReference classSymbol
		ifTrue: [
			self methodSymbol == #Comment
				ifTrue: [ true ]
				ifFalse: [
					anotherMethodReference methodSymbol == #Comment
						ifTrue: [ false ]
						ifFalse: [
							self classIsMeta = anotherMethodReference classIsMeta
								ifTrue: [ self methodSymbol < anotherMethodReference methodSymbol ]
								ifFalse: [ self classIsMeta ]	]]]
		ifFalse: [ self classSymbol < anotherMethodReference classSymbol ]! !

