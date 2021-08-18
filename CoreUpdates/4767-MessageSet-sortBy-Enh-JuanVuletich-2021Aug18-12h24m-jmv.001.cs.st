'From Cuis 5.0 [latest update: #4766] on 18 August 2021 at 12:29:42 pm'!

!MessageSet methodsFor: 'message list' stamp: 'jmv 8/18/2021 10:00:10'!
sortByClassHierarchy
	"Sort the message-list by class / selector. List classes in hierarchical order."
	
	| aClass bClass classes classesAndPositions i |

	classes _ Set new.
	messageList do: [ :methodReference |
		methodReference actualClass ifNotNil: [ :actualClass | classes add: actualClass ]].
	classesAndPositions _ Dictionary new.
	i _ 1.
	Smalltalk hierarchySorted: classes do: [ :each |
		classesAndPositions at: each put: i.
		i _ i + 1 ].
	
	messageList _ messageList sort: [ :a :b |
		(a classSymbol = b classSymbol and: [ b classIsMeta = b classIsMeta ])
			ifTrue: [
				a methodSymbol == #Comment
					ifTrue: [ true ]
					ifFalse: [
						b methodSymbol == #Comment
							ifTrue: [ false ]
							ifFalse: [ a methodSymbol < b methodSymbol ]]]
			ifFalse: [
				aClass _ a actualClass.
				bClass _ b actualClass.
				aClass isNil == bClass isNil
					ifTrue: [
						aClass isNil
							ifTrue: [a classSymbol < b classSymbol]
							ifFalse: [(classesAndPositions at: aClass) < (classesAndPositions at: bClass)]]
					ifFalse: [aClass isNil]]].
	messageList do: [ :each | each removeStringVersionPrefix ].
	self changed: #messageList.! !

!MessageSet methodsFor: 'message list' stamp: 'jmv 8/18/2021 10:00:04'!
sortByClassName
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


!MessageSetWindow methodsFor: 'menu building' stamp: 'jmv 8/18/2021 10:01:05'!
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
			#label 			-> 		'sort by class hierarchy'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortByClassHierarchy.
			#icon 			-> 		#classIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		34.		
			#label 			-> 		'sort by class name'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortByClassName.
			#icon 			-> 		#classIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		35.		
			#label 			-> 		'sort by selector'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortBySelector.
			#icon 			-> 		#scriptIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		36.		
			#label 			-> 		'sort by date'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortByDate.
			#icon 			-> 		#dateIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		37.		
			#label 			-> 		'reverse sort'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortReverse.
			#icon 			-> 		#redoIcon
		} asDictionary`.

! !

!methodRemoval: MessageSet #sortByClass stamp: 'jmv 8/18/2021 12:24:38'!
MessageSet removeSelector: #sortByClass!
