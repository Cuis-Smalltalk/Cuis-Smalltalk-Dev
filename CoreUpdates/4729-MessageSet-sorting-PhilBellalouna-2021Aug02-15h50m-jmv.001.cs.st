'From Cuis 5.0 [latest update: #4728] on 2 August 2021 at 3:53:59 pm'!

!MessageSet methodsFor: 'message list' stamp: 'pb 8/1/2021 18:28:53'!
sortReverse
	"Reverse the current sort order"
	messageList _ messageList reversed .
	self changed: #messageList! !


!MessageSetWindow methodsFor: 'menu building' stamp: 'jmv 8/2/2021 15:52:15'!
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
			#icon 			-> 		#classIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		34.		
			#label 			-> 		'sort by selector'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortBySelector.
			#icon 			-> 		#scriptIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		35.		
			#label 			-> 		'sort by date'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortByDate.
			#icon 			-> 		#dateIcon
		} asDictionary`.
	options add: `{
			#itemGroup 			-> 		41.
			#itemOrder 			-> 		36.		
			#label 			-> 		'reverse sort'.
			#object 			-> 		#model.	
			#selector 			-> 		#sortReverse.
			#icon 			-> 		#redoIcon
		} asDictionary`.

! !

