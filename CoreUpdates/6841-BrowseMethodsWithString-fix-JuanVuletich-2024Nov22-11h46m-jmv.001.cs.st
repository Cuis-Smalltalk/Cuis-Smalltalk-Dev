'From Cuis7.1 [latest update: #6840] on 22 November 2024 at 11:46:34 am'!

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 11/22/2024 11:28:49'!
browseMethodsWithString: aString matchCase: caseSensitive
	"Launch a browser on all methods that contain string literals with aString as a substring. Make the search case-sensitive or insensitive as dictated by the caseSensitive boolean parameter"

	self
		browseAllSelect: [ :method |
			method  hasLiteralSuchThat: [ :lit |
				lit isString & lit isSymbol not and:
				[lit includesSubstring: aString caseSensitive: caseSensitive]]]
		name:  'Methods with string ', aString printString, (caseSensitive ifTrue: [' (case-sensitive)'] ifFalse: [' (case-insensitive)'])
		autoHighlight: aString.! !

