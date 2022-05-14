'From Cuis 6.0 [latest update: #5163] on 14 May 2022 at 12:37:05 pm'!

!Class class methodsFor: 'instance creation' stamp: 'jmv 5/14/2022 12:36:35'!
templateForSubclassOf: priorClass category: systemCategoryName 
	"Answer an expression that can be edited and evaluated in order to define a new class, given that the class previously looked at was as given"

	^ priorClass name asPlainString, ' subclass: #NameOfSubclass
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''' , systemCategoryName asPlainString , ''''! !

