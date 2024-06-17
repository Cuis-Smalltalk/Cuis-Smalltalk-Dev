'From Cuis7.1 [latest update: #6472] on 17 June 2024 at 12:05:26 pm'!

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'eem 6/17/2024 12:03:48'!
allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: sortByCategoryBoolean
	"Walk the tree of subclasses, giving the class and its level.
	 Sort nicely: major order is alphabetical by category name, and within a category alphabetical by class name.
	 Earlier versions used the order of the category within the system organization, but that gives different orders
	 when programmers reorder the system categories to suit their wishes. Since this order decides the order of
	 package file-outs, ignoring the speicifc system category order and using its alphabetical order is important
	 for stable package file-out. eem; '24/6/17"

	classAndLevelBlock value: self value: level.
	self == Class ifTrue:  [^ self].  "Don't visit all the metaclasses"
	"Visit subclasses in alphabetical order, either by category and name, or just by name"
	self subclasses
		sort: [ :a :b |
			sortByCategoryBoolean
				ifTrue: [ a category < b category or: [ a category = b category and: [ a name <= b name ]]]
				ifFalse: [ a name <= b name ]
			];
		do: [ :subclass |
			subclass
				allSubclassesWithLevelDo: classAndLevelBlock
				startingLevel: level + 1
				sortByCategory: sortByCategoryBoolean ]! !

