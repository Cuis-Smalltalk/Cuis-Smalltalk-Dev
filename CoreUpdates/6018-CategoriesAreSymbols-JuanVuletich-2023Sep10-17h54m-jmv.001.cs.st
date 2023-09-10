'From Cuis 6.0 [latest update: #6017] on 10 September 2023 at 6:25:57 pm'!

!Categorizer methodsFor: 'accessing' stamp: 'jmv 9/10/2023 18:16:49'!
addCategory: catString before: nextCategory
	"Add a new category named heading.
	If default category exists and is empty, remove it.
	If nextCategory is nil, then add the new one at the end,
	otherwise, insert it before nextCategory."
	
	| index newCategory |
	
	newCategory := catString withBlanksTrimmed asSymbol.
	
	"heading already exists, so done"
	(categoryArray indexOf: newCategory) > 0 ifTrue: [^self].	
	
	index := categoryArray indexOf: nextCategory ifAbsent: [categoryArray size + 1].
	categoryArray := categoryArray
		copyReplaceFrom: index
		to: index-1
		with: (Array with: newCategory).
	categoryStops := categoryStops
		copyReplaceFrom: index
		to: index-1
		with: (Array with: (index = 1
				ifTrue: [0]
				ifFalse: [categoryStops at: index-1])).

	"remove empty default category"
	(newCategory ~= Default and: [(self listAtCategoryNamed: Default) isEmpty])
		ifTrue: [self removeCategory: Default]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Categorizer allSubInstancesDo: [ :each | | cats |
	cats := each categories.
	cats withIndexDo: [ :cat : i | cat isSymbol ifFalse: [ cats at: i put: cat asSymbol ]]].

ClassOrganizer allInstances select: [ :a | a categories anySatisfy: [ :cat | cat isSymbol not ]]:: size!

