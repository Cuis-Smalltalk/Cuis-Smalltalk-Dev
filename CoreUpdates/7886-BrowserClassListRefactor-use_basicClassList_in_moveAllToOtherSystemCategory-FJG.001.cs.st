'From Cuis7.7 [latest update: #7850] on 5 April 2026 at 1:43:46 pm'!

!Browser methodsFor: 'system category functions' stamp: 'FJG 5/Apr/2026 13:42:30'!
moveAllToOtherSystemCategory
	"If a class category is selected, prompt user for category to move to,
	create a Confirmer so the user can verify that all the classes in current category
 	should be moved to the selected category."
	| newSystemCategory |
	selectedSystemCategory ifNil: [ ^ self ].
	newSystemCategory := Smalltalk systemCategoryFromUserWithPrompt: 'Move classes to System Category...'.
	(newSystemCategory notNil and: [
		self basicClassList notEmpty and: [ self confirm: 'Are you sure you want to
move classes from ' , selectedSystemCategory , ' 
to ' , newSystemCategory , '?' ]]) ifTrue: [
		self basicClassList do: [ :eaClass |
			eaClass category: newSystemCategory ].
		self changed: #systemCategoryRoots ].! !

