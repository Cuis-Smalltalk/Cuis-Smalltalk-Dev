'From Cuis 6.0 [latest update: #6003] on 9 September 2023 at 4:56:54 pm'!

!TextModel methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:41:04'!
basicActualContents: aTextOrString
	"Do not throw events."
	| prevContents |
	prevContents := actualContents.
	actualContents := aTextOrString asText xasUnicodeStringOrText.
	actualContents = prevContents ifFalse: [	"Compares only characters, not attributes"
		undoRedoCommands resetToStart.
		lastEditTimeStamp := nil ]! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 9/9/2023 16:33:12'!
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
						print: (aClass theNonMetaClass xmessageSendsCount);
						nextPutAll: ' total message sends' ].
			}]].! !

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 9/9/2023 16:35:20'!
annotationForSelector: aSelector ofClass: aClass
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"

	| method |
	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].

	method := aClass compiledMethodAt: aSelector ifAbsent: nil.
	^ String streamContents: [ :strm |
		(Preferences at: #methodAnnotations)
			do: [ :each |
				each caseOf: {
					[#firstComment] -> [
						strm nextPutAll: (aClass firstCommentAt: aSelector) ].
					[#masterComment] -> [
						strm nextPutAll: ((aClass supermostPrecodeCommentFor: aSelector) ifNil: ['']) ].
					[#documentation] -> [
						strm nextPutAll: ((aClass precodeCommentOrInheritedCommentFor: aSelector) ifNil: ['']) ].
					[#timeStamp] -> [ | stamp |
						stamp := self timeStamp.
						strm nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp'])].
					[#linesOfCode] -> [
						method notNil ifTrue: [
							strm
								print: method linesOfCode;
								nextPutAll: ' lines of code' ]].
					[#messageSendsCount] -> [
						method notNil ifTrue: [
							strm
								print: method xmessageSendsCount;
								nextPutAll: ' message sends' ]].
					[#messageCategory] -> [
						strm nextPutAll: (( aClass organization categoryOfElement: aSelector) ifNil: ['']) ].
					[#sendersCount] -> [ | sendersCount |
						sendersCount := Smalltalk numberOfSendersOf: aSelector.
						sendersCount := sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
						strm nextPutAll: sendersCount ].
					[#implementorsCount] -> [ | implementorsCount |
						implementorsCount := Smalltalk numberOfImplementorsOf: aSelector.
						implementorsCount := implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
						strm nextPutAll: implementorsCount ].
					[#priorVersionsCount] -> [
						self addPriorVersionsCountForSelector: aSelector ofClass: aClass to: strm].
					[#priorTimeStamp] -> [ | stamp |
						stamp := VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
						strm nextPutAll: 'prior timestamp: '; nextPutAll: (stamp ifNil: ['None']) ].
					[#packages] -> [
						method notNil ifTrue: [
							(CodePackage packageOfMethod: method methodReference ifNone: nil)
								ifNil: [ strm nextPutAll: 'in no package' ]
								ifNotNil: [ :codePackage |
									strm nextPutAll: 'in package '; nextPutAll: codePackage packageName ]]].
					[#changeSets] -> [ | aList |
						aList := ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allChangeSets] -> [ | aList |
						aList := ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allBaseSystemChangeSets] -> [ | aList |
						aList := (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in base system change set']
									ifFalse: [strm nextPutAll: 'in base system change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no base system change set']].
					[#closuresInfo] -> [
						strm nextPutAll: (aClass closuresInfoAt: aSelector)].
					
				}]
			separatedBy: [ strm nextPutAll: self annotationSeparator ].
		(method notNil and: [method hasBreakpoint]) ifTrue: [
			strm nextPutAll: self annotationSeparator.
			strm nextPutAll: '[Breakpoint]' ]
	].! !

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 9/9/2023 16:30:54'!
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
						nextPutAll: ' total lines of code' ].
				[#messageSendsCount] -> [
					strm
						print: (SystemOrganization xmessageSendsCountOf: aCategory);
						nextPutAll: ' total message sends' ].
			}]].! !


!Browser methodsFor: 'annotation' stamp: 'jmv 9/9/2023 16:32:22'!
annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass := self selectedClassOrMetaClass)
		ifNil: [
			self selectedSystemCategory ifNotNil: [ :sysCat |
				^self annotationForSystemCategory: sysCat ].
			^ ''].
	self editSelection == #editComment
		ifTrue: [^ self annotationForSelector: #Comment ofClass: aClass].
	self isEditingExistingClass 
		ifTrue: [^ self annotationForSelector: #Definition ofClass: aClass].
	(aSelector := self selectedMessageName)
		ifNil: [
			self selectedMessageCategoryName ifNotNil: [ :messageCategory |
				^self xannotationForMessageCategory: messageCategory ofClass: aClass ].
			^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass.! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:35:39'!
summary

	| count |
	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		| methods  linesOfCode |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem.
		strm nextPutAll: ' -- Number of system categories '.
		selection systemCategories size printOn: strm.
		strm nextPutAll: '.'.
		strm nextPutAll: ' -- Number of classes: '.
		count := 0.
		selection classesDo: [ :cls | count := count + 1 ].
		count printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethods size printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		methods := selection methods size.
		methods printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		linesOfCode := selection linesOfCode.
		linesOfCode printOn: strm.
		strm nextPutAll: ' ('.
		linesOfCode / (methods asFloat max: 1.0) printOn: strm fractionDigits: 2.
		strm nextPutAll: ' per method)'.
		strm nextPutAll: '. Total message sends: '.
		linesOfCode := selection xmessageSendsCount.
		linesOfCode printOn: strm ]! !


!Character methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:25'!
isValidInBinarySelectors
	"Can be part of a binary selector?
		$< isValidInBinarySelectors
		$| isValidInBinarySelectors
		$^ isValidInBinarySelectors
		$: isValidInBinarySelectors
	"
	self xisNullCharacter ifTrue: [ ^false ].
	^#(verticalBar upArrow xColon xBinary xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!Character methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:29'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	self xisNullCharacter ifTrue: [ ^false ].
	^#(xLetter xDigit xUnderscore xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!Character methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:33'!
isValidStartOfIdentifiers
	"Can I be the first character of an identifier? (unary or keyword selector, or variable name)"

	self xisNullCharacter ifTrue: [ ^false ].
	^#(xLetter xUnderscore) statePointsTo: (Scanner typeTableAt: self).! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:36'!
isValidInBinarySelectors
	"
	$< isValidInBinarySelectors
	$| isValidInBinarySelectors
	$^ isValidInBinarySelectors
	$: isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r22C7) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r042F) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r0431) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r20D7) isValidInBinarySelectors
	"
	self xisNullCharacter ifTrue: [ ^false ].
	^#(verticalBar upArrow xColon xBinary xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:40'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	self xisNullCharacter ifTrue: [ ^false ].
	^#(xLetter xDigit xUnderscore xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:43'!
isValidStartOfIdentifiers
	"Can I be the first character of an identifier? (unary or keyword selector, or variable name)"

	self xisNullCharacter ifTrue: [ ^false ].
	^#(xLetter xUnderscore) statePointsTo: (Scanner typeTableAt: self).! !


!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 9/9/2023 16:46:30'!
isCombiningStarter: numericCodePoint
	^ (self xcombiningClass: numericCodePoint) = 0! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 9/9/2023 16:31:07'!
findFirst: aBlock
	"Return the index of my first element for which aBlock evaluates as true."

	^self xfindFirst: aBlock startingAt: 1! !


!String methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:45:02'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(subString xisUnicodeString and: [subString isAscii]) ifTrue: [
		^ String findString: subString bytes in: self startingAt: start ].
	"Do conversions if needed"
	subString isByteString ifFalse: [
		^ self xasUnicodeString findString: subString startingAt: start ].
	"Quick primitive if both String"
	^ String findString: subString in: self startingAt: start! !

!String methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:45:05'!
findStringCaseInsensitive: subString startingAt: start
	"Answer the index in this String at which the substring key first occurs, at or beyond start.
	If no match is found, zero will be returned."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(subString xisUnicodeString and: [ subString isAscii ]) ifTrue: [
		^String findStringIgnoreCase: subString bytes in: self startingAt: start ].
	"Do conversions if needed"
	subString isByteString ifFalse: [
		^self xasUnicodeString findStringCaseInsensitive: subString startingAt: start ].
	"Quick primitive if both String"
	^String findStringIgnoreCase: subString in: self startingAt: start! !

!String methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:44:50'!
caseInsensitiveLessOrEqual: aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case insensitive."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(aString xisUnicodeString and: [ aString isAscii ]) ifTrue: [
		^(String compareIgnoreCase: self with: aString bytes) <= 2 ].
	"Do conversions if needed"
	aString isByteString ifFalse: [
		^self xasUnicodeString caseInsensitiveLessOrEqual: aString ].
	"Quick primitive if both String"
	^ (String compareIgnoreCase: self with: aString) <= 2! !

!String methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:45:12'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize subStringBytes |
	index < 1 ifTrue: [ ^false ].
	sequenceSize := subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].

	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self at: index) = (subString at: 1) ifFalse: [ ^false ].
	sequenceSize = 1 ifTrue: [ ^true ].
	(self at: index+sequenceSize-1) = (subString at: sequenceSize) ifFalse: [ ^false ].
	sequenceSize = 2 ifTrue: [ ^true ].
	(subString isAscii not and: [self isAscii]) ifTrue: [ ^false ].

	"Try to find arguments for the primitive in #findString:in:startingAt: cheaply"
	subStringBytes := nil.
	subString isByteString ifTrue: [
		subStringBytes := subString ].
	(subString xisUnicodeString and: [ subString isAscii ]) ifTrue: [
		"Can use #bytes, because it has just ASCII bytes"
		subStringBytes := subString bytes ].
	subStringBytes ifNotNil: [
		^(String findString: subStringBytes in: self startingAt: index) = index ].

	"Do it in Smalltalk. Most likely cheaper than converting argument to String"
	^super is: subString substringAt: index! !

!String methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:45:17'!
sameAs: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(aString xisUnicodeString and: [aString isAscii]) ifTrue: [
		^ (String compareIgnoreCase: self with: aString bytes) = 2 ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString sameAs: self ].

	^ (String compareIgnoreCase: self with: aString) = 2! !

!String methodsFor: 'copying' stamp: 'jmv 9/9/2023 16:44:57'!
copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| rep |
	rep := replacementCollection string.
	rep xisUnicodeString ifTrue: [
		^self xasUnicodeString copyReplaceFrom: start to: stop with: rep  ].
	^super copyReplaceFrom: start to: stop with: rep.! !

!String methodsFor: 'copying' stamp: 'jmv 9/9/2023 16:42:22'!
copyWith: newElement
	newElement isUnicodeCodePoint ifTrue: [
		^self xasUnicodeString copyWith: newElement ].
	^super copyWith: newElement! !


!String class methodsFor: 'instance creation' stamp: 'jmv 9/9/2023 16:50:10'!
fromUtf8Bytes: aByteArray
	"Convert the given bytes from UTF-8 to the internal encoding: ISO Latin 9 (ISO 8859-15).
	Discard any code points that can not be represented."

	"Fast lane."
	(String isAscii: aByteArray) ifTrue: [
		^aByteArray asString ].

	"Need to do UTF-8 decoding."
	^self xfromUtf8Bytes: aByteArray trimLastNull: false! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 9/9/2023 16:45:21'!
intern: aStringOrSymbol
	"If argument is String or ASCII-only UnicodeString, create a Symbol.
	If argument is UnicodeString, and it includes non-ASCII characters, create a UnicodeSymbol.
	This is done for the benefit of (for instance) the SqueakSSL VM plugin,
		that fails if primitive names or module are not instance of Symbol."

	| preferByteStringForAscii |
	^ (self lookup: aStringOrSymbol) ifNil: [
		NewSymbols add:
			(aStringOrSymbol isSymbol
				ifTrue: [ aStringOrSymbol ]
				ifFalse: [
					preferByteStringForAscii := aStringOrSymbol.
					(aStringOrSymbol xisUnicodeString and: [ aStringOrSymbol isAscii ])
						ifTrue: [preferByteStringForAscii := aStringOrSymbol asByteString].
					preferByteStringForAscii class symbolClassToUse basicFromString: preferByteStringForAscii ]) ].! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:45:59'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	| arg startByteIndex byteIndex |
	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		subString isByteString ifTrue: [
			^String findString: subString in: bytes startingAt: start ].
		subString xisUnicodeString ifTrue: [
			^subString isAscii
				ifFalse: [ 0 ] "Won't be found, incudes non ASCII stuff, but we're ASCII"
				ifTrue: [ String findString: subString bytes in: bytes startingAt: start ]]].

	arg := subString asUtf8BytesOrByteString.
	"Do UTF-8 encoding as needed."
	startByteIndex := self byteIndexAt: start.
	byteIndex := String findString: arg in: bytes startingAt: startByteIndex.
	^self codePointIndexAt: byteIndex! !

!UnicodeString methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:46:03'!
findStringCaseInsensitive: subString startingAt: start
	"Answer the index in this String at which the substring key first occurs, at or beyond start.
	If no match is found, zero will be returned."

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		subString isByteString ifTrue: [
			^String findStringIgnoreCase: subString in: bytes startingAt: start ].
		subString xisUnicodeString ifTrue: [
			^subString isAscii
				ifFalse: [ 0 ] "Won't be found, incudes non ASCII stuff, but we're ASCII"
				ifTrue: [ String findStringIgnoreCase: subString bytes in: bytes startingAt: start ]]].

"caseSensitive = false is very expensive. Do better."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase findString: subString asLowercase startingAt: start! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:45:26'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) = 1].
		(aString xisUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 1]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) = 1! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:45:30'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) <= 2].
		(aString xisUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) <= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) <= 2! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:45:38'!
= aString
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, UnicodeString, UnicodeSymbol."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self hash = aString hash ifFalse: [
		^false ].

	(self xisInCanonicalCompositionForm and: [ aString xisInCanonicalCompositionForm ]) ifFalse: [
		^self xcanonicalComposition = aString xcanonicalComposition ].

	"Now we can assume we are both in NFC"
	self size = aString size ifFalse: [
		^false ].

	(self isAscii and: [	aString isByteString ]) ifTrue: [	
		"Fast lane."
		^String is: bytes equalTo: aString ].	

	aString xisUnicodeString ifFalse: [
		^ self beginsWith: aString ].

	self isAscii = aString isAscii ifFalse: [
		^false ].

	^String is: bytes equalTo: aString bytes.! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:45:45'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) = 3].
		(aString xisUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 3]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) = 3! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:45:51'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) >= 2].
		(aString xisUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) >= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) >= 2! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:45:55'!
caseInsensitiveLessOrEqual: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			^(String compareIgnoreCase: bytes with: aString) <= 2 ].
		(aString xisUnicodeString and: [ aString isAscii ]) ifTrue: [
			^(String compareIgnoreCase: bytes with: aString bytes) <= 2 ]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase <= aString asLowercase! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:43:31'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, UnicodeString, UnicodeSymbol and Text
	if they hold same Characters."

	"Lazy initialization."
	hash isNil ifTrue: [
		self xfindCanonicalCompositionAndHash ].

	"If 'hash' is not a number, it actually contains out #canonicalComposition. Ask for its hash then."
	hash isNumber ifFalse: [
		^hash hash ].

	"If 'hash' is a number it means we are in canonicalComposition form (NFC), and 'hash' is an good hash value."
	^hash! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:46:07'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize subStringBytes byteIndex subStringIsAscii |
	index < 1 ifTrue: [ ^false ].
	sequenceSize := subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].

	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self at: index) = (subString at: 1) ifFalse: [ ^false ].
	sequenceSize = 1 ifTrue: [ ^true ].
	(self at: index+sequenceSize-1) = (subString at: sequenceSize) ifFalse: [ ^false ].
	sequenceSize = 2 ifTrue: [ ^true ].
	subStringIsAscii := subString isAscii.
	(self isAscii and: [subStringIsAscii not]) ifTrue: [ ^false ].

	"Try to find arguments for the primitive in #findString:in:startingAt: cheaply"
	subStringBytes := nil.
	subString xisUnicodeString ifTrue: [
		"Simple. Take the bytes"
		subStringBytes := subString bytes ].
	(subString isByteString and: [ subStringIsAscii ]) ifTrue: [
		"Can use instance of String, because it has just ASCII bytes"
		subStringBytes := subString ].
	subStringBytes ifNotNil: [
		byteIndex := self byteIndexAt: index.
		^(String findString: subStringBytes in: bytes startingAt: byteIndex) = byteIndex ].

	"Do it in Smalltalk. Most likely cheaper than converting argument to UnicodeString"
	^super is: subString substringAt: index! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 9/9/2023 16:46:11'!
sameAs: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	self size = aString size ifFalse: [
		^false ].

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			^(String compareIgnoreCase: bytes with: aString) = 2 ].
		aString xisUnicodeString ifTrue: [
			^ aString isAscii
				ifFalse: [ false ] "One is ASCII, other isn't."
				ifTrue: [
					"Both are ASCII"
					(String compareIgnoreCase: bytes with: aString bytes) = 2 ]]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase = aString asLowercase! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 9/9/2023 16:39:58'!
asCodePoints
	"Answer a WordArray."

	^UnicodeString xcodePointsFromUtf8: bytes! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:44:06'!
asNFC
	"Convenience"
	^self xcanonicalComposition! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 9/9/2023 16:43:48'!
asNFD
	"Convenience"
	^self xcanonicalDecomposition! !


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 9/9/2023 16:40:07'!
fromCodePoints: aWordArray

	^self fromUtf8Bytes: (UnicodeString xutf8FromCodePoints: aWordArray) codePointCount: aWordArray size.! !


!UnicodeSymbol class methodsFor: 'private' stamp: 'jmv 9/9/2023 16:44:11'!
basicFromString: aString
	"Private use. Answers an instance that is not yet interned.
	Can break the Symbol invariant: forAllSymbols, (s1 = s2) => (s1 == s2)
	Please don't call lightly."

	| canonicalComposition utf8Bytes |
	canonicalComposition := aString xcanonicalComposition.
	utf8Bytes := canonicalComposition asUtf8Bytes.
	^self basicNew privateBytes: utf8Bytes codePointCount: canonicalComposition size.! !


!SystemDictionary methodsFor: 'startup' stamp: 'jmv 9/9/2023 16:32:09'!
processCommandLineArguments
	"
	Smalltalk processCommandLineArguments

	Note: By default, any exceptions thrown during the evaluation of arguments are simply ignored.
	To ease debugging, use the -e argument: Do Not Ignore Exceptions. 
	For example:
		squeak Cuis.image -e -s myScript.st
	If ``myScript.st`` contains an error, a Debugger will pop up.
	
	See comment at
		processCommandLineOption:optionArgument:raiseExceptions:
	See displayCommandLineUsageOn:
	"
	| optionString optionCharacter optionArgument optionIndex nextOptionIndex optionLastParameterIndex raiseExceptions dashFinder |

	"Any command line argument that starts with $- is an Option.
	NonOption arguments are parameters for the preceding Option."
	raiseExceptions := false.
	startUpScriptArguments := #().
	"Empty Strings are valid command line arguments, generated by two double quotes. Handle them."
	dashFinder := [ :any | any notEmpty and: [ any first = $- ] ].
	optionIndex := startUpArguments xfindFirst: dashFinder startingAt: 1.
	[ optionIndex > 0 ] whileTrue: [
		"Find parameters for this option"
		nextOptionIndex := startUpArguments xfindFirst: dashFinder startingAt: optionIndex+1.
		optionLastParameterIndex := nextOptionIndex = 0
			ifTrue: [startUpArguments size]
			ifFalse: [nextOptionIndex-1].
		optionString := startUpArguments at: optionIndex.
		optionString size > 1 ifTrue: [
			optionCharacter := optionString second.
			optionCharacter = $- "Including    --    as an option means 'ignore everything after here'"
				ifTrue: [^self].
			optionCharacter = $e
				ifTrue: [ raiseExceptions := true ]
				ifFalse: [
					"Handle both    -rRequiredFeature2    and    -r RequiredFeature2"
					optionString size > 2
						ifTrue: [
							optionArgument := optionString copyFrom: 3 to: optionString size.
							startUpScriptArguments := startUpArguments copyFrom: optionIndex+1 to: optionLastParameterIndex ]
						ifFalse: [
							optionArgument := optionLastParameterIndex > optionIndex ifTrue: [startUpArguments at: optionIndex+1].
							startUpScriptArguments := startUpArguments copyFrom: optionIndex+2 to: optionLastParameterIndex ].
					self xprocessCommandLineOption: optionCharacter optionArgument: optionArgument raiseExceptions: raiseExceptions.
					startUpScriptArguments := #() ]
			].
		optionIndex := nextOptionIndex ].! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 9/9/2023 16:31:36'!
startUpScriptAllArguments
	"To make command line arguments to startup scripts available to them.
	Also include #lastCommandLineArguments"

	^ self startUpScriptArguments, self xlastCommandLineArguments! !


!UniFileStream methodsFor: 'open/close' stamp: 'jmv 9/9/2023 16:41:13'!
open: fileName forWrite: writeMode
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	"Changed to do a GC and retry before failing ar 3/21/98 17:25"
	fileID := UniFileStream retryWithGC: [ self primOpen: fileName asUtf8Bytes writable: writeMode ]
					until: [ :id | id notNil ]
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name := fileName.
	"jmv: Register after setting name. Name is assumed to be defined for registered objects."
	self register.
	rwmode := writeMode.
	buffer1 := ByteArray new: 1.
	self xuseUnicodeString.
	self enableReadBuffering.! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 9/9/2023 16:47:11'!
typeTableAt: aCharacterOrCodePoint
	^self class xtypeTable: typeTable at: aCharacterOrCodePoint! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:52'!
isLiteralSymbol: aSymbol 
	"Test whether a symbol can be stored as # followed by its characters.  
	Symbols created internally with asSymbol may not have this property, 
	e.g. '3' asSymbol."
	| i char type |
	i := aSymbol size.
	i = 0 ifTrue: [^ false].
	char := aSymbol at: 1.
	"TypeTable should have been origined at 0 rather than 1 ..."
	char xisNullCharacter ifTrue: [^ false].
	type := self typeTableAt: char.
	type == #xBinary ifTrue:
		[^i <= 2
		  and: [i = 1 or: [(self typeTableAt: (aSymbol at: 2)) == #xBinary]]].
	(type == #xColon or: [type == #verticalBar]) ifTrue:
		[^i = 1].
	(type == #xLetter or: [type == #xUnderscore]) ifTrue: [
		[i > 1] whileTrue: [
			char := aSymbol at: i.
			char xisNullCharacter ifTrue: [^ false].
			type := self typeTableAt: char.
			((type == #xLetter or: [type == #xUnderscore])or: [type == #xDigit or: [type == #xColon]]) ifFalse: [
				^false].
			i := i - 1].
		^true].
	^false! !

!Scanner class methodsFor: 'testing' stamp: 'jmv 9/9/2023 16:47:07'!
typeTableAt: aCharacterOrCodePoint
	TypeTable ifNil: [
		self initTypeTable ].
	^self xtypeTable: TypeTable at: aCharacterOrCodePoint! !


!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 9/9/2023 16:48:38'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| character i replacement |
	"Convert strings like '\+161 ' (š) to unicode"
	aKeyboardEvent isSpace ifTrue: [ | string |
		string := self privateCurrentString.
		self pointIndex - 1 to: (self pointIndex - 7 max: 1) by: -1 do: [ :index |
		string at: index :: = $\ ifTrue: [ | key |
			key := string copyFrom: index + 1 to: self pointIndex - 1.
			key size > 2 and: [ key at: 1 :: = $+ ] ::
				ifTrue: [ | stream number value |
					stream := ReadStream on: key asUppercase from: 2 to: key size.
					[ number := Integer readFrom: stream  base: 16 ] on: Error do: [ ^ false ].
					value := UnicodeCodePoint codePoint: number.
					self
						selectFrom: index to: self pointIndex - 1;
						replaceSelectionWith: (UnicodeString with: value).
						^ false ] ] ] ].
	character := aKeyboardEvent keyCharacter.
	"Linux VMs don't automatically replace combining marks + space with ASCII,
	as Windows and Mac VMs do. It seems it is an app responsability to do that in Linux."
	character xisDiacriticalMark ifTrue: [
		i := self pointIndex-1.
		(i > 0 and: [ (self privateCurrentString at: i) = $  ]) ifTrue: [
			replacement := character caseOf: {
				[ `UnicodeCodePoint codePoint: 16r300` ] -> [ '`' ].
				[ `UnicodeCodePoint codePoint: 16r301` ] -> [ '''' ].
				[ `UnicodeCodePoint codePoint: 16r302` ] -> [ '^' ].
				[ `UnicodeCodePoint codePoint: 16r303` ] -> [ '~' ].
				[ `UnicodeCodePoint codePoint: 16r308` ] -> [ '"' ].
			} otherwise: [].
			replacement notNil ifTrue: [
				self selectFrom: i to: i; replaceSelectionWith: replacement.
				^ false ]
			]
		].
	self addString: (UnicodeString with: character).
	^false! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:49:49'!
userHasEdited

	morph hasUnacceptedEdits: model xisDirty! !


!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 9/9/2023 16:46:56'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| string key |
	aKeyboardEvent keyCharacter isLetter ifTrue: [^ super normalCharacter: aKeyboardEvent].
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	string := self privateCurrentString.
	"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
	self pointIndex - 1 to: (self pointIndex - 20 max: 1) by: -1 do: [:i|
		(string at: i) = $\ ifTrue: 
			[key := string copyFrom: i + 1 to: self pointIndex - 1.
			UnicodeCodePoint xnamedCharactersMap at: key ifPresent: [:value| self selectFrom: i to: self pointIndex - 1; replaceSelectionWith: (UnicodeString with: value)].
			^ super normalCharacter: aKeyboardEvent]].
	^ super normalCharacter: aKeyboardEvent! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 9/9/2023 16:50:02'!
retrieveObject
	"Answer whatever was last stored in the clipboard"
	| stringOrNil |

	"If the OS clipboard has the id for our contents, or the same characters, then answer the richer Smalltalk object.
	Note: if the (extended) clipboard contains a serialized object, it shouldn't contain an id, so
	it is deserialized even if ivar contents contains the object. This is done to guarantee consistency with pasting
	from another Cuis image."
	stringOrNil := self xretrieveIdOrStringFromOS.
	(stringOrNil = (self idFor: contents) or: [ stringOrNil = contents or: [(contents is: #Text) and: [stringOrNil = contents string]]])
		ifTrue: [
			"We copy the object, because the result of each paste operation could be modified independently of the others afterwards
			(and the same clipboard contents might be pasted many times)"
			^contents copyForClipboard ].

	"If we have the ExtendedClipboardInterface, try to get an RTF or Form"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface retrieveObject ifNotNil: [ :object | ^object ]].

	"Otherwise answer the string brought by clipboard primitives,
	but if they are not present or fail, use the internal clipboard."
	^stringOrNil ifNil: [ contents copyForClipboard ]! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 9/9/2023 16:42:36'!
drawString: s at: pt font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	displayEngine colorMap: nil.
	^s xasUnicodeString displayOnBitBltCanvasEngine: displayEngine
		from: 1
		to: s size
		at: pt roundedHAFZ
		font: font
		color: aColor! !


!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 9/9/2023 16:38:08'!
basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont
	"Answer position to place next glyph"

	| fdestX glyph maxHeight byteIndex n ascent lineGap deltaXForBaseGlyph deltaXForDiacriticalMark |
	deltaXForBaseGlyph := 0.
	ascent := aTrueTypeFont ascent.		  "expensive"
	lineGap := aTrueTypeFont lineGap."expensive"
	destY := aPoint y.
	fdestX := aPoint x.
	maxHeight := 0.
	byteIndex := byteStartIndex.
	[  byteIndex <= byteStopIndex ] whileTrue: [
		n := Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
			into: [ :byte1 :byte2 :byte3 :byte4 |
				glyph := aTrueTypeFont glyphAtUtf8Byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4 ] 
			into: nil.
		"Center diacriticals marks."
		glyph advanceWidth = 0.0
			ifFalse: [
				deltaXForBaseGlyph := glyph xdeltaXToCenter.
				deltaXForDiacriticalMark := 0.0 ]
			ifTrue: [
				deltaXForDiacriticalMark := glyph xdeltaXToCenter.
				deltaXForDiacriticalMark = 0 ifFalse: [ "Only for those who specify a delta to center."
					deltaXForDiacriticalMark := deltaXForDiacriticalMark - deltaXForBaseGlyph ].
				 ].
		destX := (fdestX + glyph leftOffset + deltaXForDiacriticalMark) roundedHAFZ.
		destY := aPoint y + ascent + lineGap-1 - glyph topOffset.
		"Please keep consistent with #displayOnVectorEngine:from:to:at:font:color:"
		aTrueTypeFont isSuperscript
			ifTrue: [destY := destY - (ascent*0.55)]
			ifFalse: [
				aTrueTypeFont isSubscript
					ifTrue: [destY := destY + (ascent*0.3)]].
		destY := destY roundedHAFZ.
		sourceForm := glyph form.
		height := sourceForm height.
		width := sourceForm width.
		self copyBits.
		fdestX := fdestX + "width + aTrueTypeFont baseKern"glyph advanceWidth.
		maxHeight := maxHeight max: height.
		byteIndex := byteIndex + n
	].
	^ fdestX ceiling@(destY+maxHeight)! !


!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 9/9/2023 16:42:40'!
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition string |
	line := textLine.
	rightMargin := line rightMargin.
	lastIndex := line first.
	tabCount := 0.
	self setFont.
	self setStopConditions.
	characterIndex := index.  "nil means scanning for point"
	characterPoint := aPoint.
	(characterPoint isNil or: [ characterPoint y > line bottom ])
		ifTrue: [ characterPoint := line bottomRight ].
	(text isEmpty or: [( characterPoint y < line top or: [ characterPoint x < line left ])
				or: [ characterIndex notNil and: [ characterIndex < line first ]]])
		ifTrue:	[^ CharacterBlock
					stringIndex: line first
					text: text
					topLeft: line leftMargin@line top
					extent: 0 @ line lineHeight
					textLine: line].
	 destX := leftMargin := line leftMarginForAlignment: alignment.
	destY := line top.
	runLength := text runLengthFor: line first.
	lineStop := characterIndex	"scanning for index"
		ifNil: [ line last ].			"scanning for point"
	runStopIndex := lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth := 0.
	spaceCount := 0.
	string := text string xasUnicodeString.

	self placeEmbeddedObject.
	[
		stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: characterPoint x
			stopConditions: stopConditions.
		"see setStopConditions for stopping conditions for character block operations."
		lastCharacterWidth := specialWidth ifNil: [ font widthOf: (text at: lastIndex) ].
		(self perform: stopCondition) ifTrue: [
			^characterIndex
				ifNil: [	"Result for characterBlockAtPoint: "
					CharacterBlock
						stringIndex: lastIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]
				ifNotNil: [	"Result for characterBlockForIndex: "
					CharacterBlock
						stringIndex: characterIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]]
		] repeat! !


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 9/9/2023 16:42:44'!
composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph string |
	
	lastIndex := startIndex.	"scanning sets last index"
	tabCount := 0.
	destY := lineRectangle top.
	lineHeight := baseline := 0.  "Will be increased by setFont"
	self setFont.
	self setStopConditions.

	"Set up margins"
	leftMargin := lineRectangle left.
	rightMargin := lineRectangle right.
	xtraSpaceBefore := 0.
	spaceAfterParagraph := 0.
	paragraphStyle ifNotNil: [
		leftSide ifTrue: [
			leftMargin := leftMargin +
				((firstLine and: [ paragraphStyle isListStyle not ])
					ifTrue: [ paragraphStyle firstIndent ]
					ifFalse: [ paragraphStyle restIndent ])].
		rightSide ifTrue: [
			rightMargin := rightMargin - paragraphStyle rightIndent].
		firstLine ifTrue: [ xtraSpaceBefore := paragraphStyle spaceBefore ].
		spaceAfterParagraph := paragraphStyle spaceAfter ].
	destX := spaceX := leftMargin.

	runLength := text runLengthFor: startIndex.
	runStopIndex := lastIndex + runLength - 1.
	line := (TextLine start: lastIndex stop: 0 internalSpaces: 0 paddingWidth: 0)
				rectangle: lineRectangle.
	line isFirstLine: firstLine.
	spaceCount := 0.
	lastLineBreakingSpace := 0.
	leftMargin := destX.
	line leftMargin: leftMargin.
	string := text string xasUnicodeString.

	self placeEmbeddedObject.
	[
			stopCondition := self scanCharactersFrom: lastIndex to: runStopIndex
				in: string rightX: rightMargin stopConditions: stopConditions.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]
	] repeat! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/9/2023 16:42:49'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos x1 |

	topLeft := textTopLeft.
	line := textLine.
	lineY := line top + textTopLeft y.
	rightMargin := line rightMargin + textTopLeft x.
	lastIndex := line first.
	tabCount := 0.
	leftInRun <= 0 ifTrue: [
		self setFont.
		self setStopConditions ].
	leftMargin := (line leftMarginForAlignment: alignment) + textTopLeft x.
	destX := leftMargin.
	destY := lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				foregroundColor := defaultColor.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment := ps alignment.
				paragraphStyle := ps.
				spaceWidth := font widthOf: Character space.
				self setStopConditions.
				text ifNotNil: [ destY := lineY + line baseline - font ascent ]]].
		self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.

	leftInRun <= 0
		ifTrue: [nowLeftInRun := text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun := leftInRun].
	runStopIndex := lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount := 0.
	string := text string xasUnicodeString.

	self placeEmbeddedObject.
	[
		startIndex := lastIndex.
		lastPos := destX@destY.
		stopCondition := self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions.
		backgroundColor ifNotNil: [
			x1 := destX.
			((Preferences at: #backgroundColorFillsAllBackground) and: [startIndex > line last]) ifTrue: [
				x1 := rightMargin ].
			canvas
				fillRectangle: (lastPos corner: x1 @ (line bottom + textTopLeft y))
				color: backgroundColor.
			((Preferences at: #backgroundColorFillsAllBackground) and: [stopCondition = #tab]) ifTrue: [
				canvas
					fillRectangle: (destX @ lastPos y corner: self tabDestX @ (line bottom + textTopLeft y))
					color: backgroundColor ]].
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat! !


!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'jmv 9/9/2023 16:48:50'!
drawOn: aCanvas

	self xdrawTreeOn: aCanvas p1: 275@500 p2: 375@500 depth: 0! !

!Sample10PythagorasTree methodsFor: 'as yet unclassified' stamp: 'jmv 9/9/2023 16:49:13'!
initialize
	super initialize.
	
	depthLimit := 9.
	angle := 0.5.
	self xwithMultipleColors.! !


!AbstractVectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 9/9/2023 16:42:53'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c _ VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	^ aString xasUnicodeString displayOnVectorEngine: engine from: firstIndex to: lastIndex
		at: aPoint font: aTrueTypeFont color: aColor.! !


!VectorCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2023 16:42:57'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c _ VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	| p1 answer |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	currentTransformation isPureTranslation ifTrue: [
		(Preferences at: #cacheTrueTypeGlyphs) ifTrue: [
			p1 := currentTransformation transform: aPoint roundedHAFZ.
			p1 := p1 + (0@(0 - (aTrueTypeFont ascent + aTrueTypeFont lineGap-1))).
			p1 := p1 roundedHAFZ.
			answer := aString xasUnicodeString displayOnBitBltCanvasEngine: auxBitBltEngine
				from: firstIndex to: lastIndex at: p1 font: aTrueTypeFont color: aColor.
			answer ifNotNil: [
				engine updateMorphBoundsLeft: p1 x top: p1 y
					right: answer x bottom: answer y ].
			^answer ]].

	^super drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 9/9/2023 16:38:51'!
into: aMorph
	super into: aMorph.
	attachedCanvas ifNotNil: [
		attachedCanvas
			xcurrentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]! !

!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 9/9/2023 16:38:54'!
outOfMorph
	super outOfMorph.
	attachedCanvas ifNotNil: [
		attachedCanvas
			xcurrentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 9/9/2023 16:38:47'!
initializeWithTranslation: aPoint

	super initializeWithTranslation: aPoint.
	engine geometryTransformation: currentTransformation.

	boundsFinderCanvas := BoundsFinderCanvas new initializeWithTranslation: aPoint.
	"So bounds found are exactly the same as our engine would find."
	boundsFinderCanvas engine antiAliasingWidth: engine antiAliasingWidth.
	boundsFinderCanvas engine subPixelDelta: engine subPixelDelta.
	attachedCanvas ifNotNil: [
		attachedCanvas
			xcurrentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 9/9/2023 16:38:58'!
resetCanvas
	"To be called in case of possible inconsistency due to an exception during drawing.
	See #displayWorldSafely"

	super resetCanvas.
	boundsFinderCanvas resetCanvas.
	engine clearArrays.
	attachedCanvas ifNotNil: [
		attachedCanvas
			xcurrentMorph: currentMorph drawingMorphStack: drawingMorphStack
			currentTransformation: currentTransformation transformations: transformations
			cti: cti ]! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/9/2023 16:43:00'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 := currentTransformation transform: aPoint roundedHAFZ.
	p1 := p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 := p1 roundedHAFZ.
	engine colorMap: nil.
	^aString xasUnicodeString displayOnBitBltCanvasEngine: engine
		from: firstIndex
		to: lastIndex
		at: p1
		font: font
		color: aColor! !


!HybridCanvas methodsFor: 'private' stamp: 'jmv 9/9/2023 16:37:33'!
setForm: aForm subPixelAntiAliasing: aBooleanOrNil
	"nil means use default kind of anti aliasing"

	form := aForm.
	engine := BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	vectorCanvas := VectorCanvas onForm: form.
	vectorCanvas xattachedCanvas: self.
	morphIDsForm := Form extent: form extent depth: 32 bits: (vectorCanvas engine instVarNamed: 'morphIds').
	drawingOnMorphIDs := false.
	self newClipRect: nil.! !


!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 9/9/2023 16:43:05'!
wordsDisplayPatchFrom: srcStringOrText to: dstStringOrText

	| finder answer src1 dst1 changedCount dstString srcString |
	srcString := srcStringOrText asPlainString.
	dstString := dstStringOrText asPlainString.
	srcString class == dstString class ifFalse: [
		^self wordsDisplayPatchFrom: srcString xasUnicodeString to: dstString xasUnicodeString ].

	finder := self base: srcString case: dstString.
	finder compareLines; compute.
	answer := srcString class new asText.
	src1 := srcString class writeStream.
	dst1 := srcString class writeStream.
	changedCount := 0.
	finder differences sort first do: [:item :condition |
		condition caseOf: {
			[ #unchanged ] -> [
				changedCount > 0 ifTrue: [
					"If the sequence of changed lines is large, comparing words gets too slow and less useful"
					changedCount > 30 ifTrue: [
						^nil ].
					"Compare the just ended sequence of changed lines"
					finder base: src1 contents case: dst1 contents.
					finder compareWords; compute: true.
					finder differences ifNil: [ ^nil ].
					answer := answer append:  finder differences anyOne asText.
					src1 resetToStart.
					dst1 resetToStart.
					changedCount := 0.
				].
				"This line hasn't changed. Just add it to the result in plain text."
				answer append: item ].
			[ #removed ] -> [
				"A removed line belongs in the source"
				src1 nextPutAll: item.
				changedCount := changedCount + 1 ].
			[ #inserted ] -> [
				"An added line belongs in the destination"
				dst1 nextPutAll: item.
				changedCount := changedCount + 1  ].
			}.
		].
	"If the sequence of changed lines is large, comparing words gets too slow and less useful"
	changedCount > 30 ifTrue: [
		^nil ].
	finder base: src1 contents case: dst1 contents.
	finder compareWords; compute: true.
	finder differences ifNil: [ ^nil ].
	answer := answer append: finder differences anyOne asText.

	^answer! !


!TTContourConstruction methodsFor: 'primitive format' stamp: 'jmv 9/9/2023 16:49:25'!
addContourDataTo: stream tx: aGeometryTransformationOrNil
	| bezierCount firstPoint cp ep prevEndPoint cpr epr |
	self isEmpty ifTrue: [
		^self ].

	"This bogus iteration could perhaps be avoided at the cost of extra memory."
	bezierCount := 0.
	self
		xstartPointDo: [ :firstBezierStart | ]
		quadraticBeziersDo: [ :controlPoint :endPoint | bezierCount := bezierCount + 1 ].
	stream nextPut: bezierCount.

	self
		xstartPointDo: [ :firstBezierStart |
			firstPoint := firstBezierStart.
			aGeometryTransformationOrNil notNil ifTrue: [
				firstPoint := aGeometryTransformationOrNil transform: firstPoint ].
			stream nextPut: firstPoint x; nextPut: firstPoint y.
			prevEndPoint := firstPoint ]
		quadraticBeziersDo: [ :controlPoint :endPoint |
			cp := controlPoint.
			ep := endPoint.
			aGeometryTransformationOrNil notNil ifTrue: [
				cp := aGeometryTransformationOrNil transform: cp.
				ep := aGeometryTransformationOrNil transform: ep ].
			cpr := cp - prevEndPoint.
			epr := ep - prevEndPoint.
			"Make control point distict from start and end points for the benefit of VectorEnginePlugin"
			(cpr = `0.0@0.0` or: [ cpr = epr ]) ifTrue: [
				cpr := epr * 0.5 ].
			stream nextPut: epr x; nextPut: epr y.
			stream nextPut: cpr x; nextPut: cpr y.
			prevEndPoint := ep ].! !


!TTFontDescription methodsFor: 'conversion' stamp: 'jmv 9/9/2023 16:37:52'!
formGlyphForUtf8Byte1: aByte1 byte2: aByte2OrNil byte3: aByte3OrNil byte4: aByte4OrNil pixelSize: pixelSize
	"Needed only for BitBltCanvasEngine"
	| byte1 byte2OrNil byte3OrNil byte4OrNil utf8ByteArray toPixelsScale formWidth formHeight leftPixelOffset topPixelOffset xtraSpaceForAntiAliasingAndRounding horizontalStrokesAtPixelCenter form canvas advanceWidth boundsBottom boundsLeft boundsRight boundsTop i deltaToXCenter |

	byte1 := aByte1.
	byte2OrNil := aByte2OrNil.
	byte3OrNil := aByte3OrNil.
	byte4OrNil := aByte4OrNil.
	i := contourDataIndexesByUtf8 at: byte1 + 1.
	utf8ByteArray := ByteArray with: byte1.
	byte2OrNil notNil ifTrue: [
		i := contourDataIndexesByUtf8 at: i negated + byte2OrNil + 1.
		utf8ByteArray := ByteArray with: byte1 with: byte2OrNil.
		byte3OrNil notNil ifTrue: [
			i := contourDataIndexesByUtf8 at: i negated + byte3OrNil + 1.
			utf8ByteArray := ByteArray with: byte1 with: byte2OrNil with: byte3OrNil.
			byte4OrNil notNil ifTrue: [
				i := contourDataIndexesByUtf8 at: i negated + byte4OrNil + 1.
				utf8ByteArray := ByteArray with: byte1 with: byte2OrNil with: byte3OrNil with: byte4OrNil ]]].

	"If glyph not found, take first glyph 'theUnknownGlyph'"
	i = 0 ifTrue: [ i := 1 ].

	advanceWidth := contourDataForUtf8 at: i. i := i + 1.
	boundsLeft := contourDataForUtf8 at: i. i := i + 1.
	boundsRight := contourDataForUtf8 at: i. i := i + 1.
	boundsTop := contourDataForUtf8 at: i. i := i + 1.
	boundsBottom := contourDataForUtf8 at: i. i := i + 1.

	horizontalStrokesAtPixelCenter := 0.5.
	xtraSpaceForAntiAliasingAndRounding := 3.
	toPixelsScale := 0.0 + pixelSize / self letterMTopSideBearing.

	"If positive, pixels we save at the left. if negative, xtra pix needed"
	leftPixelOffset := (boundsLeft * toPixelsScale - xtraSpaceForAntiAliasingAndRounding) floor.
	formWidth := (boundsRight - boundsLeft) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).

	"Usually positive. Needed pixels above the baseline"
	topPixelOffset := (boundsBottom * toPixelsScale + xtraSpaceForAntiAliasingAndRounding) ceiling.
	formHeight := (boundsBottom - boundsTop) * toPixelsScale + (xtraSpaceForAntiAliasingAndRounding*2).
	
	form := Form extent: formWidth@formHeight depth: 32.
	form fillColor: Color white.
	canvas := VectorCanvas onFormWithSubPixelAntiAliasing: form.
	canvas 
		engine
			displayUtf8: utf8ByteArray
			fromByte: 1
			toByte: utf8ByteArray size
			at: leftPixelOffset negated@(topPixelOffset - horizontalStrokesAtPixelCenter)
			trueTypeFont: self
			color: Color black
			scale: 1.0 * pixelSize
			preventMirroring: true.
	deltaToXCenter := (advanceWidth - (boundsRight+boundsLeft * 0.5)) * toPixelsScale.
	"DoubleWidth code points that combine above previous and following characters.
	Codepoints between 16r035C and 16r0363.
	Example: o͢o
	Don't center them"
	(byte1 = 205 and: [ byte2OrNil between: 156 and: 163 ]) ifTrue: [
		deltaToXCenter := 0 ].
	
	^(FormGlyph
		form: form
		leftOffset: leftPixelOffset
		topOffset: topPixelOffset
		advanceWidth: (advanceWidth * toPixelsScale))
			xdeltaXToCenter: deltaToXCenter! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 9/9/2023 16:50:26'!
mapAndStoreGlyphsUsing: characterMap
	"
	https://developer.apple.com/fonts/TrueType-Reference-Manual/RM07/appendixB.html
	"
	| glyphsByUTF8 unknownGlyph nullGlyph spaceGlyph mapSize preferredCodePoint arrowGlyphIndexes leftArrowGlyph upArrowGlyph rightArrowGlyph downArrowGlyph carriageReturnGlyph leftArrowGlyphCodePoint upArrowGlyphCodePoint rightArrowGlyphCodePoint downArrowGlyphCodePoint carriageReturnGlyphCodePoint |
	unknownGlyph := glyphs first.
	nullGlyph := glyphs second.
	mapSize := characterMap size.
	glyphsByUTF8 := Array new: 256.
	leftArrowGlyphCodePoint := self class smalltalkAssignmentArrowGlyphCodePoint.
	upArrowGlyphCodePoint := self class smalltalkReturnArrowGlyphCodePoint.
	rightArrowGlyphCodePoint := 8594.
	downArrowGlyphCodePoint := 8595.
	carriageReturnGlyphCodePoint := 8617.
	
	"Store Unicode arrow glyphs for future use if appropriate,"
	(FallbackArrowGlyphs isNil or: [fontDescription name = 'DejaVuSans']) ifTrue: [
		mapSize > 8617 ifTrue: [								"Usually true. Just in case."
			arrowGlyphIndexes := {leftArrowGlyphCodePoint. upArrowGlyphCodePoint. rightArrowGlyphCodePoint. downArrowGlyphCodePoint. carriageReturnGlyphCodePoint}
					collect: [ :codePoint | (characterMap at: codePoint+1)+1 ].
				(arrowGlyphIndexes allSatisfy: [ :i | i > 1 ]) ifTrue: [
					FallbackArrowGlyphs := arrowGlyphIndexes collect: [ :i | glyphs at: i ]]]].
	"Take arrow glyphs if present, or use fallbacks if not."
	leftArrowGlyph := upArrowGlyph := rightArrowGlyph := downArrowGlyph := carriageReturnGlyph := unknownGlyph.
	mapSize > downArrowGlyphCodePoint ifTrue: [
		leftArrowGlyph := glyphs at: (characterMap at: leftArrowGlyphCodePoint+1)+1.
		upArrowGlyph := glyphs at: (characterMap at: upArrowGlyphCodePoint+1)+1.
		rightArrowGlyph := glyphs at: (characterMap at: rightArrowGlyphCodePoint+1)+1.
		downArrowGlyph := glyphs at: (characterMap at: downArrowGlyphCodePoint+1)+1 ].
	leftArrowGlyph == unknownGlyph ifTrue: [ leftArrowGlyph := FallbackArrowGlyphs at: 1 ].
	upArrowGlyph == unknownGlyph ifTrue: [ upArrowGlyph := FallbackArrowGlyphs at: 2 ].
	rightArrowGlyph == unknownGlyph ifTrue: [ rightArrowGlyph := FallbackArrowGlyphs at: 3 ].
	downArrowGlyph == unknownGlyph ifTrue: [ downArrowGlyph := FallbackArrowGlyphs at: 4 ].
	mapSize > carriageReturnGlyphCodePoint ifTrue: [
		carriageReturnGlyph := glyphs at: (characterMap at: carriageReturnGlyphCodePoint+1)+1 ].
	carriageReturnGlyph == unknownGlyph ifTrue: [ carriageReturnGlyph := FallbackArrowGlyphs at: 5 ].

	0 to: mapSize - 1 do: [ :codePoint | | g array prevByte|

		preferredCodePoint := codePoint.
		(codePoint = $- codePoint and: [mapSize > 16r2212]) ifTrue: [ preferredCodePoint := 16r2212 ]. "WIDE MINUS"
		(codePoint = $* codePoint and: [mapSize > 16r2217]) ifTrue: [ preferredCodePoint := 16r2217 ]. "CENTERED ASTERISK"
		g := glyphs at: (characterMap at: preferredCodePoint+1)+1 ifAbsent: [ unknownGlyph ].
		g == unknownGlyph ifTrue: [
			g := glyphs at: (characterMap at: codePoint+1)+1 ifAbsent: [ unknownGlyph ]].

		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ].
					].
				prevByte := byte.
				]
			withUtf8BytesOfUnicodeCodePoint: codePoint.
			array at: prevByte+1 put: g.
		codePoint = 32 ifTrue: [
			spaceGlyph := g ].

		].

	"Synthetic Unicode zero width space glyph. Add others as needed."
	{ UnicodeCodePoint zeroWidthSpace } do: [ :codePoint | | g array prevByte|
		g := TTGlyph new.					"Suitable as zero width space"
		array := glyphsByUTF8.
		prevByte := nil.
		Character
			evaluate: [ :byte |
				prevByte notNil ifTrue: [
					array := (array at: prevByte+1) ifNil: [ array at: prevByte+1 put: (Array new: 256) ]].
				prevByte := byte ]
			withUtf8BytesOfUnicodeCodePoint: codePoint codePoint.
			array at: prevByte+1 put: g ].

	Character separators do: [ :s |
		glyphsByUTF8 at: s codePoint +1 put: spaceGlyph ].
	1 to: 4 do: [ :i |			glyphsByUTF8 at: 28+i put: i ].
	glyphsByUTF8 at: 13 +1 put: carriageReturnGlyph.
	fontDescription
		xglyphsByUtf8Bytes: glyphsByUTF8
		unknownGlyph: unknownGlyph scaleArrowGlyphs: {leftArrowGlyph.​ upArrowGlyph.​ rightArrowGlyph.​ downArrowGlyph. carriageReturnGlyph}! !


!TTGlyph methodsFor: 'drawing' stamp: 'jmv 9/9/2023 16:49:39'!
drawGlyphOn: aVectorCanvas geometryTransformation: txOrNil
	| initialPathAndMove |
	contours do: [ :contour |
		contour isEmpty ifFalse: [
			aVectorCanvas moveTo: `0.0@0.0`.
			initialPathAndMove := contour xinitialMoveToAndPathCommands: txOrNil.
			initialPathAndMove first runOn: aVectorCanvas .
			initialPathAndMove second runOn: aVectorCanvas ]].! !

