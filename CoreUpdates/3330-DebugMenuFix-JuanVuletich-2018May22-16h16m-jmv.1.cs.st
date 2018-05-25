'From Cuis 5.0 of 7 November 2016 [latest update: #3329] on 22 May 2018 at 4:38:46 pm'!

!DebuggerWindow methodsFor: 'menu building' stamp: 'jmv 5/22/2018 16:16:36'!
contextStackMenu
	"Set up the menu appropriately for the context-stack-list, either shifted or unshifted as per the parameter provided"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fullStack (f)'.
				#selector 		-> 		#fullStack
			} asDictionary.
			{
				#label 			-> 		'restart (r)'.
				#selector 		-> 		#restart
			} asDictionary.
			{
				#label 			-> 		'proceed (p)'.
				#selector 		-> 		#proceed
			} asDictionary.
			{
				#label 			-> 		'step (t)'.
				#selector 		-> 		#doStep
			} asDictionary.
			{
				#label 			-> 		'step through (T)'.
				#selector 		-> 		#stepIntoBlock
			} asDictionary.
			{
				#label 			-> 		'send (e)'.
				#selector 		-> 		#send
			} asDictionary.
			{
				#label 			-> 		'where (w)'.
				#selector 		-> 		#where
			} asDictionary.
			{
				#label 			-> 		'peel to first like this'.
				#selector 		-> 		#peelToFirst
			} asDictionary.
			nil.
			{
				#label 			-> 		'return entered value'.
				#selector 		-> 		#returnValue
			} asDictionary.
			{
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleBreakOnEntry
			} asDictionary.
		}`.
	^aMenu! !

