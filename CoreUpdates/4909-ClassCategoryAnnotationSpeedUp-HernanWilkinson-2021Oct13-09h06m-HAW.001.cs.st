'From Cuis 5.0 [latest update: #4908] on 13 October 2021 at 9:32:22 am'!

!Categorizer methodsFor: 'tdd' stamp: 'HAW 10/13/2021 09:29:54'!
classesAt: aCategoryName

	^(self listAtCategoryNamed: aCategoryName) 
		collect: [:aClassName | Smalltalk classNamed: aClassName ]
		thenSelect:  [ :aClass | aClass notNil ]
		! !


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

