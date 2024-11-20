'From Cuis7.1 [latest update: #6827] on 20 November 2024 at 3:51:33 pm'!

!SystemOrganizer methodsFor: 'categories' stamp: 'jmv 11/20/2024 15:50:54'!
fullListAtCategoryNamed: aCategoryNameOrPrefix
	"Same as >>listAtCategoryNamed: but include subcategories if aCategoryName is a top category."
	| answer |
	answer := self listAtCategoryNamed: aCategoryNameOrPrefix.
	"If it's a top category, include the classes of all its subcategories:"
	(aCategoryNameOrPrefix includes: $-) ifFalse:
		[| prefix |
		prefix := aCategoryNameOrPrefix, '-'.
		self categories do: [:each|
			(each beginsWith: prefix) ifTrue: [
				answer := answer, (self listAtCategoryNamed: each)]]].
	^ answer! !


!CodePackage methodsFor: 'listing' stamp: 'jmv 11/20/2024 15:51:04'!
extensionClassNamesIn: aCategoryNameOrPrefix

	^ (SystemOrganization fullListAtCategoryNamed: aCategoryNameOrPrefix) select: [ :className | | cls |
		cls := Smalltalk at: className.
		(self hasAnyExtensionCategoriesForClass: cls) or: [
			self hasAnyExtensionCategoriesForClass: cls theMetaClass ]]! !

