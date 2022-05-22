'From Cuis 6.0 [latest update: #5171] on 21 May 2022 at 11:59:01 pm'!

!Class class methodsFor: 'instance creation' stamp: 'HAW 5/21/2022 23:57:46'!
template: aSystemCategoryName 
	"Answer an expression that can be edited and evaluated in order to define a new class."

	^ self templateForSubclassOf: Object category: aSystemCategoryName ! !

