'From Cuis 6.0 [latest update: #5119] on 2 May 2022 at 11:17:37 am'!

!Browser methodsFor: 'system category list' stamp: 'len 5/1/2022 14:21:49'!
systemCategoryRoots
	"Answer the roots of the forest of system categories."
	| categories subcategories roots name |
	categories _ self systemCategoryList.
	subcategories _ Dictionary new.
	categories do: [:each|
		name _ (each findTokens: '-') first.
		(subcategories at: name ifAbsentPut: [OrderedCollection new]) add: each].
	roots _ OrderedCollection new.
	categories do: [:each|
		name _ (each findTokens: '-') first.
		(roots includes: name) ifFalse:
			[(subcategories at: name) size > 1 ifTrue: [roots add: name] ifFalse: [roots add: (subcategories at: name) first]]].
	^ roots collect: [:each| SystemCategoryWrapper with: each name: each model: self]! !

