'From Cuis 6.0 [latest update: #5611] on 3 January 2023 at 2:50:29 pm'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 1/3/2023 14:48:07'!
annotationForSystemCategory: aCategory
	"Provide a line of content for an annotation pane, given that the receiver is pointing at a System Category (i.e. a group of classes)."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'System Category: ';
			nextPutAll: aCategory.
		(Preferences at: #systemCategoryAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#classCount] -> [
					strm
						print: (SystemOrganization fullListAtCategoryNamed: aCategory) size;
						nextPutAll: ' classes' ].
				[#instanceMethodsCount] -> [
					strm
						print: (SystemOrganization instanceMethodCountOf: aCategory);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (SystemOrganization classMethodCountOf: aCategory);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (SystemOrganization linesOfCodeOf: aCategory);
						nextPutAll: ' total lines of code' ]
			}]].! !


!SystemOrganizer methodsFor: 'categories' stamp: 'jmv 1/2/2023 16:45:56'!
testCaseClassesAt: aCategoryName

	^(self allClassesAt: aCategoryName) select: [ :aClass | aClass is: #TestCaseClass ]! !

!SystemOrganizer methodsFor: 'source code management' stamp: 'jmv 1/2/2023 16:43:14'!
classMethodCountOf: category

	^  (self allClassesAt: category) sum: [ :cl | cl class selectors size ] ifEmpty: 0.! !

!SystemOrganizer methodsFor: 'source code management' stamp: 'jmv 1/2/2023 16:42:56'!
instanceMethodCountOf: category

	^ (self allClassesAt: category) sum: [ :cl | cl selectors size ] ifEmpty: 0.! !

!SystemOrganizer methodsFor: 'source code management' stamp: 'jmv 1/3/2023 14:46:31'!
linesOfCodeOf: category
"
SystemOrganization linesOfCodeOf: #'System-Files'
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."
	^ (self allClassesAt: category) sum: [ :cl | cl linesOfCode ] ifEmpty: 0.! !

