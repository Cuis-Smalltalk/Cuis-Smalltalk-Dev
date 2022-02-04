'From Cuis 6.0 [latest update: #5059] on 4 February 2022 at 5:00:17 pm'!

!SystemOrganizer methodsFor: 'categories' stamp: 'len 2/4/2022 05:52:38'!
allClassesAt: aCategoryName
	"Same as >>classesAt: but if aCategoryName is a top category it includes all classes in subcategories."
	^ (self fullListAtCategoryNamed: aCategoryName) collect: [:aClassName | Smalltalk classNamed: aClassName]! !

!SystemOrganizer methodsFor: 'categories' stamp: 'len 2/4/2022 04:47:04'!
classesAt: aCategoryName
	^ (self listAtCategoryNamed: aCategoryName) collect: [:aClassName | Smalltalk classNamed: aClassName]! !

!SystemOrganizer methodsFor: 'categories' stamp: 'len 2/4/2022 05:44:37'!
fullListAtCategoryNamed: aCategoryName
	"Same as >>listAtCategoryNamed: but include subcategories if aCategoryName is a top category."
	| answer |
	answer _ self listAtCategoryNamed: aCategoryName.
	"If it's a top category, include the classes of all its subcategories:"
	(aCategoryName includes: $-) ifFalse:
		[| prefix |
		prefix _ aCategoryName, '-'.
		self categories do: [:each| (each beginsWith: prefix) ifTrue: [answer _ answer, (self listAtCategoryNamed: each)]]].
	^ answer! !

!SystemOrganizer methodsFor: 'categories' stamp: 'HAW 2/4/2022 16:54:25'!
testCaseClassesAt: aCategoryName

	^(self classesAt: aCategoryName) select: [ :aClass | aClass is: #TestCaseClass ]! !


!Browser methodsFor: 'class list' stamp: 'len 2/4/2022 05:45:07'!
defaultClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	selectedSystemCategory ifNil: [^ #()].
	^ systemOrganizer fullListAtCategoryNamed: selectedSystemCategory! !


!SystemOrganizer methodsFor: 'source code management' stamp: 'HAW 10/13/2021 09:31:11'!
classMethodCountOf: category

	^  (self classesAt: category) sum: [ :cl | cl class selectors size ] ifEmpty: 0.! !

!SystemOrganizer methodsFor: 'source code management' stamp: 'HAW 10/13/2021 09:31:23'!
instanceMethodCountOf: category

	^ (self classesAt: category) sum: [ :cl | cl selectors size ] ifEmpty: 0.! !

!SystemOrganizer methodsFor: 'source code management' stamp: 'HAW 10/13/2021 09:31:30'!
linesOfCodeOf: category
"
SystemOrganization linesOfCodeOf: #'System-Files'
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."

	^ (self classesAt: category) sum: [ :cl | cl linesOfCode ] ifEmpty: 0.! !


!TestSuite class methodsFor: 'instance creation' stamp: 'HAW 2/4/2022 16:59:36'!
forSystemCategoryNamed: aCategoryName  using: aSystemOrganizer

	| testCaseClasses |

	testCaseClasses _ aSystemOrganizer testCaseClassesAt: aCategoryName.

	^ testCaseClasses isEmpty 
		ifTrue: [ self forClasses: (aSystemOrganizer allClassesAt: aCategoryName) named: aCategoryName, ' infered tests' ]
		ifFalse: [ self forTestCaseClasses: testCaseClasses named: aCategoryName, ' tests' ]
		
! !

!methodRemoval: Categorizer #testCaseClassesAt: stamp: 'HAW 2/4/2022 16:58:58'!
Categorizer removeSelector: #testCaseClassesAt:!
!methodRemoval: Categorizer #classesAt: stamp: 'HAW 2/4/2022 16:53:37'!
Categorizer removeSelector: #classesAt:!
