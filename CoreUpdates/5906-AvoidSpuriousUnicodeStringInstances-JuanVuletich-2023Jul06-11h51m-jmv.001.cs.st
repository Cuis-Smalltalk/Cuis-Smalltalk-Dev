'From Cuis 6.0 [latest update: #5898] on 6 July 2023 at 11:53:14 am'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 7/6/2023 11:52:22'!
annotationForClassDefinitionFor: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Class definition for ';
			nextPutAll: aClass name.
		(Preferences at: #classAnnotations) do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#instanceMethodsCount] -> [
					strm
						print: (aClass theNonMetaClass selectors size);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (aClass theMetaClass selectors size);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (aClass theNonMetaClass linesOfCode);
						nextPutAll: ' total lines of code' ].
				[#messageSendsCount] -> [
					strm
						print: (aClass theNonMetaClass messageSendsCount);
						nextPutAll: ' total message sends' ].
			}]].! !


!UnicodeSymbol class methodsFor: 'instance creation' stamp: 'jmv 7/6/2023 11:52:07'!
readFrom: strm
	"
	UnicodeSymbol readFrom: '#abc' readStream
	"

	strm peek = $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"! !


!SystemDictionary methodsFor: 'startup' stamp: 'jmv 7/6/2023 11:52:28'!
lastCommandLineArguments
	"To make all command line arguments after end of options '--' available to scripts applications"

	^startUpArguments copyAfter: '--'.! !

