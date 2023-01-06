'From Cuis 6.0 [latest update: #5621] on 6 January 2023 at 9:41:44 am'!

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 1/6/2023 09:40:49'!
browseMethodsWithString: aString matchCase: caseSensitive
	"Launch a browser on all methods that contain string literals with aString as a substring. Make the search case-sensitive or insensitive as dictated by the caseSensitive boolean parameter"

	self
		browseAllSelect: [ :method |
			method  hasLiteralSuchThat: [ :lit |
				lit isString and:
				[lit includesSubstring: aString caseSensitive: caseSensitive]]]
		name:  'Methods with string ', aString printString, (caseSensitive ifTrue: [' (case-sensitive)'] ifFalse: [' (case-insensitive)'])
		autoSelect: aString.! !

