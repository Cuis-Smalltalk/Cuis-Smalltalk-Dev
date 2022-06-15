'From Cuis 6.0 [latest update: #5280] on 13 June 2022 at 3:34:32 pm'!

!CharacterSequence methodsFor: 'grammar' stamp: 'LC 7/5/2020 16:47:58'!
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
		(letter or: ['cks' includes: second]) ifTrue: [^'a'].
		second = $n
			ifTrue: [(self size = 2 or: [self third isVowel]) ifTrue: [^'a']]].
	(first = $e and: [second = $u]) ifTrue: [^'a'].
	^article! !

!CharacterSequence methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:29:48'!
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

!CharacterSequence methodsFor: 'grammar' stamp: 'LC 6/19/2020 12:52:49'!
exceptionalPlural
	| singular plural index |
	singular := #(
		'addendum' 'aircraft' 'alga' 'alumna' 'alumnus' 'amoeba' 'antenna' 'appendix' 'automaton'
		'bacillus' 'bacterium' 'barracks'
		'cactus' 'calculus' 'child' 'cicatrix' 'colossus' 'corpus' 'corrigendum' 'criterion' 'curriculum'
		'datum' 'deer' 'desideratum' 'dwarf'
		'echo' 'embargo' 'ephemeris' 'erratum' 'extremum'
		'fish' 'focus' 'foot' 'forum' 'fungus'
		'gallows' 'genus' 'goose'
		'hero'
		'index' 'infimum' 'is'
		'lacuna' 'larva' 'louse'
		'matrix' 'maximum' 'means' 'memorandum' 'minimum' 'mythos' 'money' 'mouse'
		'nucleus'
		'offspring' 'optimum' 'opus' 'ox'
		'person' 'phenomenon' 'phylum' 'potato' 'proof'
		'quantum'
		'roof'
		'series' 'sheep' 'species' 'spoof' 'stimulus' 'stratum' 'syllabus'
		'tomato' 'tooth' 'torpedo' 'trilby'
		'vertebra' 'vertex' 'veto'
		'was').
	plural := #(
		'addenda' 'aircraft' 'algae' 'alumnae' 'alumni' 'amoebae' 'antennae' 'appendices' 'automata'
		'bacilli' 'bacteria' 'barracks'
		'cacti' 'calculi' 'children' 'cicatrices' 'colossi' 'corpora' 'corrigenda' 'criteria' 'curricula'
		'data' 'deer' 'desiderata' 'dwarfs'
		'echoes' 'embargoes' 'ephemerides' 'errata' 'extrema'
		'fish' 'foci' 'feet' 'fora' 'fungi'
		'gallows' 'genera' 'geese'
		'heroes'
		'indices' 'infima' 'are'
		'lacunae' 'larvae' 'lice'
		'matrices' 'maxima' 'means' 'memoranda' 'minima' 'mythoi' 'moneys' 'mice'
		'nuclei'
		'offspring' 'optima' 'opera' 'oxen'
		'people' 'phenomena' 'phyla' 'potatoes' 'proofs'
		'quanta'
		'roofs'
		'series' 'sheep' 'species' 'spoofs' 'stimuli' 'strata' 'syllabi'
		'tomatoes' 'teeth' 'torpedoes' 'trilbys'
		'vertebrae' 'vertices' 'vetoes'
		'were').
	index := singular indexOf: self.
	^index > 0 ifTrue: [plural at: index]! !

!CharacterSequence methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:25:45'!
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

!CharacterSequence methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:24:38'!
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

!CharacterSequence methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:26:19'!
pluralize: aBoolean
	^aBoolean ifTrue: [self asPlural] ifFalse: [self]! !

!CharacterSequence methodsFor: 'grammar' stamp: 'LC 4/25/2020 10:45:49'!
withArticle
	^self article , ' ' , self! !

!methodRemoval: String #withArticle stamp: 'jmv 6/13/2022 15:33:52'!
String removeSelector: #withArticle!
!methodRemoval: String #asPlural stamp: 'jmv 6/13/2022 15:33:40'!
String removeSelector: #asPlural!
!methodRemoval: String #pluralize: stamp: 'jmv 6/13/2022 15:33:50'!
String removeSelector: #pluralize:!
!methodRemoval: String #lowercasePlural stamp: 'jmv 6/13/2022 15:33:47'!
String removeSelector: #lowercasePlural!
!methodRemoval: String #article stamp: 'jmv 6/13/2022 15:33:36'!
String removeSelector: #article!
!methodRemoval: String #exceptionalPlural stamp: 'jmv 6/13/2022 15:33:42'!
String removeSelector: #exceptionalPlural!
!methodRemoval: String #isUninflictedNoun stamp: 'jmv 6/13/2022 15:33:45'!
String removeSelector: #isUninflictedNoun!

!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findString:in:startingAt: findString:startingAt: findString:startingAt:caseSensitive: findStringIgnoreCase:in:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: size string:lineIndicesDo:)
('comparing' < <= = > >= caseInsensitiveLessOrEqual: compare: compare:caseSensitive: crc16 hash is:equalTo: is:substringAt: sameAs:)
('copying' , appendToString:)
('converting' asByteString asFileName asHex asLowercase asUppercase asUtf32Words asUtf32Words: asUtf8Bytes asUtf8Bytes: asUtf8BytesOrByteString base64Decoded capitalized fixLineEndings string:withLineEndings: substrings substrings: surroundedBySingleQuotes thatCanHoldCodePoints translateFrom:to:table: translateToLowercase translateToUppercase translateWith: uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('private' replaceFrom:to:with:startingAt:)
('system primitives' compare:with: compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' canHoldCodePoints hasContentsInExplorer is: isAscii isByteString isEmpty isRemote)
('paragraph support' encompassLine: encompassParagraph:)
('fileman-converting' asAbsolutePathName asDirectoryEntry)
('fileman-actions' fileContents fileContents:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
!

