'From Cuis 5.0 [latest update: #4120] on 25 April 2020 at 11:03:53 am'!

!False methodsFor: 'controlling' stamp: 'LC 4/25/2020 10:34:34'!
orNot: alternativeBlock

	^alternativeBlock value not! !


!True methodsFor: 'controlling' stamp: 'LC 4/25/2020 10:34:49'!
orNot: alternativeBlock 
	"Nonevaluating disjunction -- answer true since the receiver is true."

	^self! !


!String methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:35:17'!
article
	| article first letter second |
	self isEmpty ifTrue: [^self].
	article := self first isVowel ifTrue: ['an'] ifFalse: ['a'].
	first := self first asLowercase.
	letter := self size = 1.
	second := letter ifFalse: [self second asLowercase].
	(first = $f and: [letter orNot: ['aeiloru' includes: second]])
		ifTrue: [^'an'].
	first = $u ifTrue: [
		(letter or: ['ck' includes: second]) ifTrue: [^'a'].
		second = $n
			ifTrue: [(self size = 2 or: [self third isVowel]) ifTrue: [^'a']]].
	(first = $e and: [second = $u]) ifTrue: [^'a'].
	^article! !

!String methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:29:48'!
asPlural
	| k trimmed plural n |
	k := self findFirst: [:ch | ch isSeparator not].
	k > 1
		ifTrue: [^(self copyFrom: 1 to: k - 1) , (self allButFirst: k - 1) asPlural].
	trimmed := self withBlanksTrimmed.
	trimmed isEmpty ifTrue: [^''].
	plural := trimmed asLowercase lowercasePlural.
	n := trimmed size min: plural size.
	1 to: n do: [:i |
		(trimmed at: i) isUppercase
			ifTrue: [plural at: i put: (plural at: i) asUppercase]].
	^plural! !

!String methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:25:30'!
exceptionalPlural
	| singular plural index |
	singular := #(
		'addendum' 'aircraft' 'alga' 'alumnus' 'amoeba' 'antenna' 'appendix'
		'bacterium' 'barracks'
		'cactus' 'child' 'criterion' 'curriculum'
		'datum' 'deer' 'dwarf'
		'echo' 'ephemeris' 'embargo'
		'fish' 'focus' 'foot' 'forum' 'fungus'
		'gallows' 'genus' 'goose'
		'hero'
		'index' 'is'
		'larva' 'louse'
		'matrix' 'means' 'memorandum' 'mithos' 'money' 'mouse'
		'nucleus'
		'offspring' 'ox'
		'person' 'phenomenon' 'potato' 'proof'
		'roof'
		'series' 'sheep' 'species' 'spoof' 'stimulus' 'syllabus'
		'tomato' 'tooth' 'torpedo' 'trilby'
		'vertebra' 'veto'
		'was').
	plural := #(
		'addenda' 'aircraft' 'algae' 'alumni' 'amoebae' 'antennae' 'appendices'
		'bacteria' 'barracks'
		'cacti' 'children' 'criteria' 'curricula'
		'data' 'deer' 'dwarfs'
		'echoes' 'ephemerides' 'embargoes'
		'fish' 'foci' 'feet' 'fora' 'fungi'
		'gallows' 'genera' 'geese'
		'heroes'
		'indices'
		'are'
		'larvae' 'lice'
		'matrices' 'means' 'memoranda' 'mythoi' 'moneys' 'mice'
		'nuclei'
		'offspring' 'oxen'
		'people' 'phenomena' 'potatoes' 'proofs'
		'roofs'
		'series' 'sheep' 'species' 'spoofs' 'stimuli' 'syllabi'
		'tomatoes' 'teeth' 'torpedoes' 'trilbys'
		'vertebrae' 'vetoes'
		'were').
	index := singular indexOf: self.
	^index > 0 ifTrue: [plural at: index]! !

!String methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:25:45'!
isUninflictedNoun
	| nouns |
	nouns := #(
		'bison' 'bream' 'breeches' 'britches'
		'carp' 'chassis' 'clippers' 'cod' 'contretemps' 'corps'
		'debris' 'diabetes' 'djinn'
		'eland' 'elk'
		'flounder'
		'gallows' 'graffiti'
		'headquarters' 'herpes' 'high-jinks' 'homework'
		'innings'
		'jackanapes'
		'mackerel' 'measles' 'mews' 'mumps'
		'news'
		'pincers' 'pliers' 'proceedings'
		'rabies'
		'salmon' 'scissors' 'sea-bass' 'series' 'shears' 'species' 'swine'
		'trout' 'tuna'
		'whiting' 'wildebeest').
	^nouns includes: self! !

!String methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:24:38'!
lowercasePlural
	| last |
	self exceptionalPlural ifNotNil: [:pl | ^pl].
	self isUninflictedNoun ifTrue: [^self].
	last := self last.
	last = $y ifTrue: [
		#('ay' 'ey' 'oy' 'uy') do: [:t |
			(self endsWith: t) ifTrue: [^self , 's'].
			^self allButLast , 'ies']].
	#('zz' 'ch' 'sh') do: [:t | (self endsWith: t) ifTrue: [^self , 'es']].
	last = $s ifTrue: [
		self = 'its' ifTrue: [^'their'].
		#('bs' 'cs' 'ds' 'ks' 'ls' 'ms' 'rs' 'ts' 'ws')
			do: [:t | (self endsWith: t) ifTrue: [^self]].
		#('sis' 'xis')
			do: [:t | (self endsWith: t) ifTrue: [^(self allButLast: 2) , 'es']]].
	last = $z ifTrue: [^self , 'zes'].
	(last = $x or: [last = $s]) ifTrue: [^self , 'es'].
	(self endsWith: 'man') ifTrue: [^(self allButLast: 2) , 'en'].
	last = $f ifTrue: [^self allButLast , 'ves'].
	(self endsWith: 'fe') ifTrue: [^(self allButLast: 2) , 'ves'].
	^self , 's'! !

!String methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:26:19'!
pluralize: aBoolean
	^aBoolean ifTrue: [self asPlural] ifFalse: [self]! !

!String methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:45:49'!
withArticle
	^self article , ' ' , self! !


!Semaphore methodsFor: 'printing' stamp: 'LC 4/25/2020 10:32:35'!
printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: ' with ';
		nextPutAll: excessSignals asString;
		space;
		nextPutAll: ('signal' pluralize: excessSignals ~= 1)! !


!Object methodsFor: 'message handling' stamp: 'LC 4/25/2020 11:00:15'!
argumentName
	| name |
	name _ self argumentNameSufix.
	^name article, name! !

!Object methodsFor: 'printing' stamp: 'LC 4/25/2020 11:01:36'!
printOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	aStream
		nextPutAll: self class name withArticle! !

!Object methodsFor: 'printing' stamp: 'LC 4/25/2020 11:02:06'!
printWithClosureAnalysisOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	aStream
		nextPutAll: self class name withArticle! !


!ContextPart methodsFor: 'debugger access' stamp: 'LC 4/25/2020 11:02:34'!
print: anObject on: aStream
	"Safely print anObject in the face of direct ProtoObject subclasses."
	| objClass |
	objClass _ self objectClass: anObject.
	(objClass canUnderstand: #printOn:) ifTrue: [
		^anObject printOn: aStream].
	aStream nextPutAll: objClass name withArticle! !

!methodRemoval: String #aOrAnPrefix stamp: 'LC 4/25/2020 10:58:06'!
String removeSelector: #aOrAnPrefix!
!methodRemoval: String #prefixedWithAOrAn stamp: 'LC 4/25/2020 11:00:29'!
String removeSelector: #prefixedWithAOrAn!
!methodRemoval: String #trackwithArticle stamp: 'LC 4/25/2020 10:45:52'!
String removeSelector: #trackwithArticle!

!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: lineCount lineIndicesDo: lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string)
('comparing' < <= = > >= alike: beginsWith: caseInsensitiveLessOrEqual: caseSensitiveLessOrEqual: commonPartWith:startAt:stopAt:applying: commonPrefixWith: compare: compare:caseSensitive: crc16 endsWith: hash is:substringAt: match: sameAs: startingAt:match:startingAt:)
('copying' , append: appendToString: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: afterBlanksEndsWith: asCamelCase asDate asFileName asHex asIdentifier: asInteger asLowercase asMonth asNumber asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUppercase asUtf8 asUtf8: asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findSelector firstNoBlankIndex initialIntegerOrNil keywords lastNoBlankIndex prefixAndSuffix: romanNumber squeezedTo: substrings surroundedBySingleQuotes translateFrom:to:table: translateToLowercase translateToUppercase translateWith: truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withLineEndings: withNewLines withoutEnclosing: withoutLeadingBlanks withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('enumerating' collect:)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('printing' isLiteral print printOn: storeOn:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom: replaceFrom:to:with:startingAt:)
('system primitives' compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' hasContentsInExplorer is: isAlphaNumeric isRemote isString)
('paragraph support' encompassLine: encompassParagraph: endOfParagraphBefore: indentationIfBlank:)
('arithmetic' * + - / // \\)
('user interface' edit editLabel:)
('formatting' format:)
('uCompletion' separateKeywords)
('fileman-converting' asAbsolutePathName asDirectoryEntry asDriveName asFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-actions' fileContents fileContents:)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('text conversion emphasis' bold italic pointSize: struck sub super under)
('text conversion attributes' centered justified leftFlush rightFlush)
('text conversion color' black blue cyan gray green magenta red yellow)
('*vectorGraphics' displayOnBitBltCanvasEngine:from:to:at:trueTypeFont:color:)
('grammar' article asPlural exceptionalPlural isUninflictedNoun lowercasePlural pluralize: withArticle)
!


!Semaphore reorganize!
('communication' signal wait waitTimeoutMSecs: waitTimeoutSeconds:)
('mutual exclusion' critical: critical:ifCurtailed: critical:ifError: critical:ifLocked:)
('comparing' = hash)
('testing' isSignaled)
('initialization' initSignals terminateProcess)
('private' species)
('printing' printOn:)
!

