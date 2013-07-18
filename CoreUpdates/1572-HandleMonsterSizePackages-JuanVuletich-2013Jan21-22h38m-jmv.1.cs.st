'From Cuis 4.1 of 12 December 2012 [latest update: #1571] on 21 January 2013 at 10:41:49 pm'!

!ClassDescription methodsFor: 'private' stamp: 'jmv 1/21/2013 22:36'!
linesOfCode

"
Object linesOfCode 
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."

	| lines |
	lines _ 0.
	self selectorsDo: [ :sel |
		lines _ lines + (self compiledMethodAt: sel) linesOfCode ].
	^self isMeta
		ifTrue: [ lines]
		ifFalse: [ lines + self class linesOfCode]
"
(SystemOrganization categories select: [:c | 'Fabrik*' match: c]) detectSum:
		[:c | (SystemOrganization superclassOrder: c) detectSum: [:cl | cl linesOfCode]] 24878
"! !


!CodePackage methodsFor: 'source code management' stamp: 'jmv 1/21/2013 22:36'!
linesOfCode
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."
	^self methods inject: 0 into: [ :sum :each |
		sum + each compiledMethod linesOfCode]! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 1/21/2013 22:41'!
summary

	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem.
		strm nextPutAll: ' -- Number of system categories '.
		selection systemCategories size printOn: strm.
		strm nextPutAll: '.'.
		strm nextPutAll: ' -- Number of classes: '.
		selection classes size printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethods size printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		selection methods size printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		selection linesOfCode printOn: strm.
		strm nextPutAll: '.' ]! !


!CodeProvider methodsFor: 'message list' stamp: 'jmv 1/21/2013 16:25'!
sourceStringPrettifiedAndDiffed
	"Answer a copy of the source code for the selected message, transformed by diffing and pretty-printing exigencies"
	| class selector sourceString |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	(class isNil or: [ selector isNil ]) ifTrue: [ ^ 'missing' ].
	sourceString _ class
		ultimateSourceCodeAt: selector
		ifAbsent: [ ^ 'error' ].
	"Can be expensive for methods of ridiculous size"
	sourceString size < 2000 ifTrue: [
		self
			validateMessageSource: sourceString
			forSelector: selector
			inClass: class ].
	(self showingPrettyPrint or: [ self showingAnyKindOfPrettyDiffs ]) ifTrue: [
		sourceString _ class compilerClass new
			format: sourceString
			in: class
			notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		sourceString _ self diffFromPriorSourceFor: sourceString ].
	^ sourceString! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 1/21/2013 22:36'!
linesOfCode
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."
	| lines |
	lines _ 0.
	self getSource lineIndicesDo: [ :start :endWithoutDelimiters :end |
		endWithoutDelimiters - start > 0 ifTrue: [
			lines _ lines+1 ]].
	^lines! !


!Text methodsFor: 'emphasis' stamp: 'jmv 1/21/2013 16:31'!
makeSelectorBoldIn: aClass
	"For formatting Smalltalk source code, set the emphasis of that portion of 
	the receiver's string that parses as a message selector to be bold."
	| parser s |
	string size = 0 ifTrue: [^self].
	"Don't waste time on methods of absurd size"
	s _ string size < 2000
		ifTrue: [ string ]
		ifFalse: [ string readStream crLfNextLine ].

	(parser _ aClass parserClass new) parseSelector: s.
	self makeBoldFrom: 1 to: (parser endOfLastToken min: s size)! !

