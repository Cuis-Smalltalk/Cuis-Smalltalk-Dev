'From Cuis 6.0 [latest update: #5053] on 31 January 2022 at 1:40:48 pm'!

!Browser methodsFor: 'class list' stamp: 'len 1/28/2022 16:21:42'!
defaultClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	| answer |
	selectedSystemCategory ifNil: [^ #()].
	answer _ systemOrganizer listAtCategoryNamed: selectedSystemCategory.
	"If it's a top category, include the classes of all its subcategories:"
	(selectedSystemCategory includes: $-) ifFalse:
		[| prefix |
		prefix _ selectedSystemCategory, '-'.
		systemOrganizer categories do: [:each| (each beginsWith: prefix) ifTrue: [answer _ answer, (systemOrganizer listAtCategoryNamed: each)]]].
	^ answer! !

