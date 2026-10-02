'From Cuis7.9 [latest update: #8258] on 2 October 2026 at 10:11:01 am'!

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 1/Oct/2026, 5:51:22 pm (UTC)'!
compileProperties: aString for: method
	"Keep any Pragmas, but use only newly specified Properties"

	(Compiler evaluate: aString) ifNotNil: [ :props | 
		props isArray ifTrue: [
			method methodProperties: props.
			"Save to Changes and User Changes files."
			method
				putSource: method sourceCode
				class: method methodClass
				category: (method methodClass organization categoryOfElement: method selector)
				withStamp: method authorAndStampAsSaved
				inFile: 2
				priorMethod: method.
			SystemChangeNotifier uniqueInstance methodChanged: method.
			^ true ]].
	^false! !

