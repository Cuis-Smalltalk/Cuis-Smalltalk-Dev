'From Cuis 5.0 [latest update: #4034] on 2 February 2020 at 12:45:03 pm'!
"Change Set:		4035-CuisCore-AuthorName-2020Feb02-12h37m
Date:			2 February 2020
Author:			Nahuel Garbezza

Add an option to inspect keys on dictionaty inspector"!


!InspectorWindow methodsFor: 'menu commands' stamp: 'RNG 2/2/2020 12:44:24'!
inspectSelectedKey

	^ model selectedKey inspect! !


!InspectorWindow methodsFor: 'menu building' stamp: 'RNG 2/2/2020 12:41:50'!
menuOptionsForDictionary

	^ `{
		nil.
		{
			#label 			-> 		'inspect key'.
			#selector 		-> 		#inspectSelectedKey.
			#icon 			-> 		#findIcon
		} asDictionary.
		{
			#label 			-> 		'senders of this key'.
			#selector 		-> 		#sendersOfSelectedKey.
			#icon 			-> 		#mailForwardIcon
		} asDictionary.
		{
			#label 			-> 		'add key'.
			#selector 		-> 		#addEntry.
			#icon 			-> 		#listAddIcon
		} asDictionary.
		{
			#label 			-> 		'rename key'.
			#selector 		-> 		#renameEntry.
			#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
			#label 			-> 		'remove'.
			#object 			-> 		#model.
			#selector 		-> 		#removeSelection.
			#icon 			-> 		#listRemoveIcon
		} asDictionary.
	}`! !

