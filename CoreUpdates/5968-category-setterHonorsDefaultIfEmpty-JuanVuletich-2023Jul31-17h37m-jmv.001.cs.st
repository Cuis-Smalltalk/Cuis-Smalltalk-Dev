'From Cuis 6.0 [latest update: #5967] on 31 July 2023 at 5:40:12 pm'!

!ClassDescription methodsFor: 'organization' stamp: 'jmv 7/31/2023 17:39:29'!
category: requested 
	"Categorize the receiver under the system category, cat, removing it from 
	any previous categorization."

	| oldCategory newCategory |
	requested isString ifFalse: [^self errorCategoryName].
	oldCategory := self category.
	newCategory := (requested isNil or: [ requested isEmpty ])
		ifFalse: [requested withBlanksTrimmed]
		ifTrue: [SystemOrganization class default].
	SystemOrganization classify: self name under: newCategory.
	SystemChangeNotifier uniqueInstance classRecategorized: self from: oldCategory to: newCategory.! !

