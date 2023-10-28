'From Cuis 6.0 [latest update: #6058] on 27 October 2023 at 10:42:50 pm'!
'Description '!
!provides: 'Gettext' 1 28!
!requires: 'System-Locales' 1 4 nil!
SystemOrganization addCategory: #Gettext!


!classDefinition: #GetTextExporter category: #Gettext!
Object subclass: #GetTextExporter
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Gettext'!
!classDefinition: 'GetTextExporter class' category: #Gettext!
GetTextExporter class
	instanceVariableNames: ''!

!classDefinition: #MOFile category: #Gettext!
Object subclass: #MOFile
	instanceVariableNames: 'localeID isLittleEndian magic revision nStrings originalTableOffset translatedTableOffset hashTableSize hashTableOffset hashTable originalStrings translatedStrings translations'
	classVariableNames: 'Cr Lf'
	poolDictionaries: ''
	category: 'Gettext'!
!classDefinition: 'MOFile class' category: #Gettext!
MOFile class
	instanceVariableNames: ''!

!classDefinition: #NaturalLanguageTranslator category: #Gettext!
Object subclass: #NaturalLanguageTranslator
	instanceVariableNames: 'id'
	classVariableNames: 'Translators'
	poolDictionaries: ''
	category: 'Gettext'!
!classDefinition: 'NaturalLanguageTranslator class' category: #Gettext!
NaturalLanguageTranslator class
	instanceVariableNames: ''!

!classDefinition: #GetTextTranslator category: #Gettext!
NaturalLanguageTranslator subclass: #GetTextTranslator
	instanceVariableNames: 'moFiles'
	classVariableNames: 'LocaleDirsForDomain SystemDefaultLocaleDirs UserDefaultLocaleDirs'
	poolDictionaries: ''
	category: 'Gettext'!
!classDefinition: 'GetTextTranslator class' category: #Gettext!
GetTextTranslator class
	instanceVariableNames: ''!

!classDefinition: #TextDomainInfo category: #Gettext!
Object subclass: #TextDomainInfo
	instanceVariableNames: 'categoryPrefixes categories'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Gettext'!
!classDefinition: 'TextDomainInfo class' category: #Gettext!
TextDomainInfo class
	instanceVariableNames: ''!

!classDefinition: #TextDomainManager category: #Gettext!
Object subclass: #TextDomainManager
	instanceVariableNames: ''
	classVariableNames: 'ClassCategories Classes DefaultDomain DomainInfos LoneClasses'
	poolDictionaries: ''
	category: 'Gettext'!
!classDefinition: 'TextDomainManager class' category: #Gettext!
TextDomainManager class
	instanceVariableNames: ''!

!classDefinition: #TranslatedReceiverFinder category: #Gettext!
Object subclass: #TranslatedReceiverFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Gettext'!
!classDefinition: 'TranslatedReceiverFinder class' category: #Gettext!
TranslatedReceiverFinder class
	instanceVariableNames: ''!


!GetTextExporter commentStamp: '' prior: 0!
Export translations to gettext format divided into categories.

"Export gettext template files"
GetTextExporter new exportTemplate.

"Export translation files for current locale"
GetTextExporter new exportTranslator: (InternalTranslator newLocaleID: LocaleID current).

"Export all gettext template and po files."
GetTextExporter exportAll.

"To register a class category as a new domain"
TextDomainManager registerClassCategory: 'Morphic-Books' domain: 'Book'.
"Remove a class category"
TextDomainManager unregisterClassCategory: 'Morphic-Books'.!

!MOFile commentStamp: '' prior: 0!
Wrapper for MO file of gettext.
Known limitation:  
	currently don't support prural form.
	translation strings have to be encoded in utf-8.

Implementation notes:
	Testing on XO showed emulation of hash search without plugin + on demand loading is slow.
	The test also showed conversion of utf8 string to Squeak's String is really slow (especially for non-latin language).
	so in this version, all of original/translated strings are loaded on initiaization,
	but "translated strings" is left as ByteString on loading time, to reduce loading time.
	After that the translated string is converted on demand. 
!

!NaturalLanguageTranslator commentStamp: '<historical>' prior: 0!
abstract class of natural language translator.
Class side manages and holds loaded instances of concrete classes.

To refresh translators:
NaturalLanguageTranslator privateStartUp!

!GetTextTranslator commentStamp: '' prior: 0!
emulation of gettext runtime
Known limitation:  
     currently don't support prural form.
!

!TextDomainInfo commentStamp: '' prior: 0!
I hold criteria for deciding wheter a systemCategory belongs to domain. 
- categoryPrefixes is collection of prefix of class category.
- categories is for specifying fine grained criterion.
!

!TextDomainManager commentStamp: '<historical>' prior: 0!
I manages mapping from class category to textdomain (.mo files).

Usage:
	TextDomainManager registerCategoryPrefix: 'DrGeo' domain: 'drgeo'.
	TextDomainManager unregisterDomain: 'DrGeo'.

	TextDomainManager registerClass: #TerseGuideHelp domain: 'guides'.

The domain name map to a .mo file in {image|vm}Path/locale/{lang}/LC_MESSAGES/domain.mo
For example locale/fr/LC_MESSAGES/drgeo.mo
{lang} is the host language locale resolved by the System-Locales package.

Class variables:
 ClassCategories				IdentityDictionary -- classCategory -> domainName 
 Classes				IdentityDictionary -- class name (a Symbol) -> domainName   (a cache only!!)
 DefaultDomain				String -- the default domain name
 DomainInfos				Dictionary -- domainName -> a TextDomainInfo
 LoneClasses				IdentityDictionary -- class name (a Symbol) -> domainName.  For classes whose entire category are not all in the same domain (BookMorph and QuickGuideMorph)

!

!TranslatedReceiverFinder commentStamp: '' prior: 0!
I am a utility class of Gettext. Most uses are internal to Gettext.

You can use me to browse all methods sending translation messages to non-string receivers:

	TranslatedReceiverFinder browseNonLiteralReceivers!

!GetTextExporter methodsFor: 'exporting'!
appendTranslations: domains 

	self 
		appendTranslations: domains 
		for: self class defaultSelectors ! !

!GetTextExporter methodsFor: 'exporting'!
appendTranslations: domains for: someSelectors
	"Append translations in the given domains.
	Will be done by searching for strings receiving the given selectors"

	someSelectors do:[ :selector |
		self appendStringReceivers: selector into: domains]
! !

!GetTextExporter methodsFor: 'exporting' stamp: 'hlsf 3/5/2022 14:32:37'!
dirNameDomain: domain
"Answer a file name for the domain."
	^ 'po' asDirectoryEntry / domain // (domain , '.pot')
! !

!GetTextExporter methodsFor: 'exporting' stamp: 'drgeo 9/10/2022 22:24:17'!
exportFor: someSelectors
"Export translation files. the file extention is 'pot'"
	| domains |
	domains _ Dictionary new.
	self appendTranslations: domains for: someSelectors.
	domains 	keysAndValuesDo: [:domainName :value | 	
		self export: value 	domain: domainName]! !

!GetTextExporter methodsFor: 'exporting' stamp: 'hlsf 3/5/2022 14:48:19'!
exportTemplate
	"GetTextExporter new exportTemplate"
	self exportFor: self class defaultSelectors ! !

!GetTextExporter methodsFor: 'file out' stamp: 'hlsf 3/5/2022 14:41:32'!
exportBody: literals
	"Export a gettext file body. literals is a dictionary of keyword ->
	#(MethodReference...) in the textDomain."
	"Build {sortKey. comment. msgid } to optimize sorting (getting category is
	too slow).
	If there are two or more methods for a mgsid, only first method
	(alphabetical) is used for sorting."
	| sorted msgid sortedMethods category sortKey comment triplets commentUnderLined |
	triplets _ literals associations collect: [:assoc | 
		msgid _ assoc key.
		sortedMethods _ assoc value asArray sort.
		category _ sortedMethods first actualClass category asString.
		sortKey _ category , ',' , sortedMethods first printString , ',' , msgid.
		comment _ (sortedMethods
			collect: [:each | each actualClass asString , '>>' , each selector asString])
			inject: category
			into: [:result :methodName | result , ',' , methodName].
		"Replace white spaces to _ because gettext tool might replace a space to 
		a new line some times, and it makes 		difficult to take a diff."
		commentUnderLined _ comment copyReplaceAll: ' ' with: '_'.
		Array 	with: sortKey with: commentUnderLined with: msgid].
	
	"Sort and output the words"
	sorted _ triplets			sort: [:a :b | a first <= b first].
	sorted 	do: [:triplet | 
		comment _ triplet second.
		msgid _ triplet third.
		self exportRecordHeader: comment.
		self exportPhrase: msgid]! !

!GetTextExporter methodsFor: 'accessing'!
stream
	^ stream! !

!GetTextExporter methodsFor: 'accessing'!
stream: aStream
	stream _ aStream! !

!GetTextExporter methodsFor: 'private' stamp: 'hlsf 3/5/2022 12:18:51'!
appendStringReceivers: aSymbol into: domains
	| literals references domainName methodReference keywords found |
	
	found _ TranslatedReceiverFinder new stringReceiversOf: aSymbol.
	found do: [ :assoc |
		methodReference _ assoc key.
		keywords _ assoc value.
		domainName _ self getTextDomainForClassCategory:			methodReference methodClass category.
		literals _ domains at: domainName ifAbsentPut: [Dictionary new].
		keywords do: [ :literal |
			references _ literals at: literal ifAbsentPut: [OrderedCollection new].
			references add: methodReference.
		].
	]. 
! !

!GetTextExporter methodsFor: 'private' stamp: 'hlsf 3/5/2022 12:42:28'!
createHeaders
	| headers |
	headers _ OrderedCollection new.
	headers add: 'Project-Id-Version' -> 'Cuis'.
	headers add: 'POT-Creation-Date' -> self currentDateAndTime.
	headers add: 'PO-Revision-Date' -> self currentDateAndTime.
	headers add: 'Last-Translator' -> ''.
	headers add: 'Language-Team' -> ''.
	headers add: 'MIME-Version' -> '1.0'.
	headers add: 'Content-Type' -> ('text/plain; charset=ascii').
	headers add: 'Content-Transfer-Encoding' -> '8bit'.
	headers add: 'X-Cuis-SystemVersion' -> (SystemVersion current asString).
	^ headers! !

!GetTextExporter methodsFor: 'private'!
currentDateAndTime
	^ String
		streamContents: [:aStream | 
			aStream nextPutAll: Date today yyyymmdd;
				space.
			Time now
				print24: true
				showSeconds: false
				on: aStream.
			aStream nextPutAll: '-0000']! !

!GetTextExporter methodsFor: 'private' stamp: 'hlsf 3/5/2022 14:43:11'!
export: literals domain: domainName 
"Export a gettext file in a category. literals is a dictionary of keyword -> #(MethodReference...) in the textDomain."
	| fileEntry |	
	fileEntry _ self dirNameDomain: domainName.
	fileEntry ensureParent.
	[stream _ fileEntry forceWriteStream.
	self exportHeader: domainName.
	self exportBody: literals] 		
		ensure: 	[stream close]! !

!GetTextExporter methodsFor: 'private' stamp: 'hlsf 3/5/2022 12:48:57'!
exportHeader: domainName
	| headers |
	self exportTag: 'msgid' msg: ''.
	self exportTag: 'msgstr' msg: ''.
	headers _ self createHeaders.
	headers add: 'X-Cuis-Domain' -> domainName.
	headers do: [:each | self exportHeaderLineKey: each key value: each value].
	stream lf! !

!GetTextExporter methodsFor: 'private' stamp: 'hlsf 3/5/2022 12:48:29'!
exportHeaderLineKey: keyString value: valueString 
	stream nextPut: $";
		 nextPutAll: keyString;
		 nextPut: $:;
		 space;
		 nextPutAll: valueString;
		 nextPutAll: '\n';
		 nextPut: $";
		 lf.! !

!GetTextExporter methodsFor: 'private' stamp: 'hlsf 3/5/2022 14:38:35'!
exportPhrase: phraseString
	phraseString ifEmpty: [^ self].
	self exportTag: 'msgid' msg: phraseString.
	self exportTag: 'msgstr' msg: ''.
	stream lf
! !

!GetTextExporter methodsFor: 'private' stamp: 'hlsf 3/5/2022 12:56:23'!
exportRecordHeader: context
	stream 
		nextPutAll: '#: ';
		nextPutAll: context;
		lf.! !

!GetTextExporter methodsFor: 'private' stamp: 'drgeo 9/10/2022 22:32:39'!
exportTag: tag msg: aString 
	| pos end line |
	(aString indexOf: Character lf)	 = 0
		ifTrue: [self exportTag: tag singleLine: aString]
		ifFalse: [self exportTag: tag singleLine: ''.
			pos _ 1.
			end _ 0.
			[end < aString size]
				whileTrue: [end _ aString indexOf: Character lf startingAt: pos.
					end = 0
						ifTrue: [end _ aString size].
					line _ aString copyFrom: pos to: end.
					stream nextPut: $";
						
						nextPutAll: (self formatString: line);
						 nextPut: $";
						 lf.
					pos _ end + 1]]! !

!GetTextExporter methodsFor: 'private' stamp: 'hlsf 3/5/2022 12:54:57'!
exportTag: tag singleLine: aString 
	stream nextPutAll: tag.
	stream space.
	stream nextPut: $".
	stream
		nextPutAll: (self formatString: aString).
	stream nextPut: $".
	stream lf! !

!GetTextExporter methodsFor: 'private' stamp: 'drgeo 9/10/2022 22:37:22'!
formatReplacements
	| replacements |
	replacements _ OrderedCollection new.
	replacements add: '\' -> '\\'.
	replacements add: String lfString -> '\n'.
	replacements add: String tab -> '\t'.
	replacements add: '"' -> '\"'.
	^ replacements! !

!GetTextExporter methodsFor: 'private'!
formatString: aString 
	| result |
	result _ aString.
	self formatReplacements
		do: [:each | result _ result copyReplaceAll: each key with: each value].
	^ result! !

!GetTextExporter methodsFor: 'private'!
getTextDomainForClassCategory: aClassCategory
	^TextDomainManager domainForClassCategory:  aClassCategory
		! !

!GetTextExporter class methodsFor: 'utilities'!
exportTemplate
	"GetTextExporter exportTemplate"
	self new exportTemplate! !

!GetTextExporter class methodsFor: 'utilities' stamp: 'hlsf 3/5/2022 14:48:45'!
exportTemplateFor: someSelectors
	"Writes files to be used as the translation template.
	It will include strings that are receivers of any of someSelectors"
	
	self new exportFor: someSelectors ! !

!GetTextExporter class methodsFor: 'utilities'!
exportTemplateIncluding: aSelector
	"Writes files to be used as the translation template.
	It will include strings that are receivers of aSelector"
	
	self exportTemplateFor: self defaultSelectors, {aSelector} ! !

!GetTextExporter class methodsFor: 'utilities'!
keys
	| categories |
	categories _ Dictionary new.
	GetTextExporter new appendTranslations: categories.
	^ categories values
		inject: Set new
		into: [:set :next | set addAll: next keys;
				 yourself]! !

!GetTextExporter class methodsFor: 'accessing'!
defaultSelectors
	"Answers the selectors that are sent to strings that should be translated"
	
	^ #(
		#translated 
		#translatedNoop
		)! !

!MOFile methodsFor: 'experimental'!
hashPjw: aString
	"This is the hash function used by the (unused) hashTable. Kept in case someone wants to try and make it work"
	"So called `hashpjw' function by P.J. Weinberger
   	[see Aho/Sethi/Ullman, COMPILERS: Principles, Techniques and Tools,
   	1986, 1987 Bell Telephone Laboratories, Inc.] "
	| stringSize hash g |
	stringSize _ aString size.
	hash _ 0.
	1 to: stringSize do: [:pos |
		hash _ hash bitShift: 4.
		hash _ hash + ((aString at: pos) asInteger).
		g _ hash bitAnd: 16rF0000000.
		g = 0 ifFalse: [
			hash _ hash  bitXor: (g bitShift: -24).
			hash _ hash bitXor: g.
		]
	].
	^hash.
! !

!MOFile methodsFor: 'public' stamp: 'hlsf 3/9/2022 18:28:42'!
load: aFileEntry localeID: id
"all of strings are loaded. 
translation strings are converted to internal string format on load time.
original-string/index pairs are registerd to Dictionary on load time."
	localeID _ id.
	aFileEntry readStreamDo: [:strm | |originalTable translatedTable|
		self loadHeader: strm.
		originalTable _ self loadStringPointers: strm offset: originalTableOffset.					
		translatedTable _ self loadStringPointers: strm offset: translatedTableOffset.
		originalStrings _ self loadStrings: strm pointers: originalTable.
		translatedStrings _ self loadStrings: strm pointers: translatedTable.
		translations _ Dictionary new: nStrings.  
		1 to: nStrings do: [:index | | key |
			key _ originalStrings at: index.
			translations at: key put: index].
		originalStrings _ nil] ! !

!MOFile methodsFor: 'public'!
searchFor: aString
	| index |
	index _ translations at: aString ifAbsent: [^nil].
	^translatedStrings at: index.
	
! !

!MOFile methodsFor: 'public'!
translationFor: aString 
	aString size = 0 ifTrue: [^ '']. "Gettext header"
	^ (self searchFor: aString) ifNil: [aString]
! !

!MOFile methodsFor: 'private' stamp: 'hlsf 5/11/2023 11:26:33'!
loadHeader: strm
	strm binary.
	magic :=  strm nextUint32BigEndian: true .
	magic = 16rDE120495 
		ifTrue: [isLittleEndian := true]
		ifFalse: [
			magic = 16r950412DE 
				ifTrue: [isLittleEndian := false]
				ifFalse: [ self error: 'invalid MO'] 		].
	revision := strm nextUint32BigEndian: isLittleEndian not.
	nStrings := strm nextUint32BigEndian: isLittleEndian not.
	originalTableOffset := strm nextUint32BigEndian: isLittleEndian not.
	translatedTableOffset := strm nextUint32BigEndian: isLittleEndian not.
	hashTableSize := strm nextUint32BigEndian: isLittleEndian not.
	hashTableOffset := strm nextUint32BigEndian: isLittleEndian not! !

!MOFile methodsFor: 'private' stamp: 'hlsf 5/11/2023 11:26:16'!
loadStringPointers: strm offset: tableOffset
"returns tupple {arrayOfOffsetToString  arrayOfLengthOfString}"
	| offsetTable lenTable len offset tupple |
	offsetTable := IntegerArray new: nStrings.
	lenTable := IntegerArray new: nStrings.
	strm binary.
	strm position: tableOffset.
	1 to: nStrings do: [:index |
		len := strm nextUint32BigEndian: isLittleEndian not.
		offset := strm nextUint32BigEndian: isLittleEndian not.
		offsetTable at: index put: offset.
		lenTable at: index put: len ].
	tupple := Array new: 2.
	tupple at: 1 put: offsetTable.
	tupple at: 2 put:  lenTable.
	^tupple! !

!MOFile methodsFor: 'private' stamp: 'hlsf 10/27/2023 22:35:57'!
loadStrings: strm pointers: table
"We convert string encoding at load time.
Currently, we do not take care to set the leadingChar for a languageEnvironment"
	| offsetTable lenTable strings |
	strm useCharacters.
	offsetTable :=  table first.
	lenTable := table second.
	strings := Array new: nStrings.
	1 to: nStrings do: [:index | |  rawStr start byteLength endPos|
		start := offsetTable at: index.
		byteLength := lenTable at: index.
		endPos := start + byteLength.
		rawStr :=  (String new: byteLength) writeStream.
		strm position:  start.
		[strm position < endPos] whileTrue: [rawStr nextPut: strm next].
		strings at: index put: (UnicodeString fromUtf8Bytes: rawStr contents asByteArray	)].
	^strings.! !

!MOFile class methodsFor: 'class initialization'!
initialize
	Cr _ Character cr.
	Lf _ Character lf.
! !

!MOFile class methodsFor: 'instance creation'!
fileName: path localeID: id
	^self new 
			load:path localeID: id! !

!NaturalLanguageTranslator methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: self class name; nextPut: $(; print: self localeID; nextPut: $)! !

!NaturalLanguageTranslator methodsFor: 'translation'!
translate: aString inDomain: aDomainName
	^ aString! !

!NaturalLanguageTranslator methodsFor: 'accessing'!
domainRegistered: aDomainName
	"notify that new TextDomain is registered.  Concrete subclass can responds to this event if needed"! !

!NaturalLanguageTranslator methodsFor: 'accessing'!
domainUnregistered: aDomainName
	"notify that new TextDomain is unregistered.  Concrete subclass can responds to this event if needed"! !

!NaturalLanguageTranslator methodsFor: 'accessing'!
localeID
	^id! !

!NaturalLanguageTranslator methodsFor: 'accessing'!
localeID: anID
	id _ anID! !

!NaturalLanguageTranslator methodsFor: 'language switching'!
setCurrent
	"notify locale of the translator become current"
! !

!NaturalLanguageTranslator class methodsFor: 'class initialization' stamp: 'hlsf 8/16/2022 11:09:35'!
initialize
	Smalltalk addToStartUpList: NaturalLanguageTranslator
	
	! !

!NaturalLanguageTranslator class methodsFor: 'class initialization'!
privateStartUp
	self reset.
	GetTextTranslator reset.
	self localeChanged.! !

!NaturalLanguageTranslator class methodsFor: 'class initialization'!
startUp: resuming 
	resuming
		ifFalse: [^ self].
	self privateStartUp.! !

!NaturalLanguageTranslator class methodsFor: 'actions'!
reset
	"Flush the translator instances"
	
	Translators _ nil! !

!NaturalLanguageTranslator class methodsFor: 'accessing'!
availableForLocaleID: localeID 
	"Answer available locale ID.
	If translator is not found for correct locale ID, then isoLanguage is
	attempted for the key."
	^ self translators
		at: localeID
		ifAbsent: [localeID hasParent
				ifTrue: [self translators
						at: localeID parent
						ifAbsent: [self default]]
				ifFalse: [self default]] ! !

!NaturalLanguageTranslator class methodsFor: 'accessing'!
availableLanguageLocaleIDs
	"Return the locale ids for the currently available languages.  
	Meaning those which either internally or externally have  
	translations available."
	"NaturalLanguageTranslator availableLanguageLocaleIDs"
	^ self translators values collect:[:each | each localeID]! !

!NaturalLanguageTranslator class methodsFor: 'accessing'!
current
	^ self availableForLocaleID: LocaleID current! !

!NaturalLanguageTranslator class methodsFor: 'accessing'!
default
	"Answer translator for backstop"
	"self default translate: 'test'"
	^ self new
		localeID: (LocaleID isoLanguage: 'en')! !

!NaturalLanguageTranslator class methodsFor: 'accessing'!
domainRegistered: aDomainName
	"notify that new TextDomain is registered"
	self translators do: [:each | each domainRegistered: aDomainName]! !

!NaturalLanguageTranslator class methodsFor: 'accessing'!
domainUnregistered: aDomainName
	"notify that new TextDomain is unregistered"
	self translators do: [:each | each domainUnregistered: aDomainName]! !

!NaturalLanguageTranslator class methodsFor: 'accessing'!
localeChanged
	"notify some project starts to use this locale.
	 this facility may use the event to load translation data dynamically" 
	self current setCurrent
! !

!NaturalLanguageTranslator class methodsFor: 'accessing'!
removeLocaleID: localeID 
	"self removeLocaleID: (LocaleID isoString: 'ja-kids')"
	^ self translators
		removeKey: localeID
		ifAbsent: []! !

!NaturalLanguageTranslator class methodsFor: 'accessing'!
translators
	^ Translators ifNil: [Translators _ Dictionary new]	! !

!NaturalLanguageTranslator class methodsFor: 'translation'!
translate: aString
	^ self translate: aString toLocale: LocaleID current! !

!NaturalLanguageTranslator class methodsFor: 'translation' stamp: 'hlsf 3/9/2022 21:42:29'!
translate: aString toLocale: localeID
	| here domain |
	here _ thisContext sender sender sender methodClass.
	domain _ TextDomainManager domainForClass: here.
	^ self translate: aString toLocale: localeID inDomain: domain! !

!NaturalLanguageTranslator class methodsFor: 'translation'!
translate: aString toLocale: localeID inDomain: aDomainName
	^ (self availableForLocaleID: localeID)
		translate: aString inDomain: aDomainName! !

!GetTextTranslator methodsFor: 'accessing'!
domainRegistered: aDomainName
	"only current translator actually load the MO, to minimize loading time.
	 other translator will load anyway when it goes current"
	(self class current == self) 
		ifTrue: [self moFileForDomain: aDomainName].
	! !

!GetTextTranslator methodsFor: 'accessing'!
domainUnregistered: aDomainName
	moFiles removeKey: aDomainName ifAbsent: [^self]
	! !

!GetTextTranslator methodsFor: 'accessing' stamp: 'hlsf 10/27/2023 22:22:34'!
loadMOFiles
	TextDomainManager allKnownDomains 
		do: [:domainName | 
			self moFileForDomain: domainName
		].! !

!GetTextTranslator methodsFor: 'accessing'!
refresh
	"Purge the cached translations (and load on demand)"
	
	moFiles _ Dictionary new
! !

!GetTextTranslator methodsFor: 'language switching'!
setCurrent
	"ensure actual contents of MOs is loaded on switching language"
	self loadMOFiles! !

!GetTextTranslator methodsFor: 'private' stamp: 'hlsf 3/9/2022 15:58:41'!
loadMOFileForDomain: aDomainName
	| moName |
	moName _ self class 
		findMOForLocaleID: self localeID
		domain: aDomainName.
	moName notNil
		 ifTrue: [^MOFile new :: 
			load: moName	localeID: self localeID]
		ifFalse: [^nil]
! !

!GetTextTranslator methodsFor: 'private' stamp: 'hlsf 3/9/2022 15:59:04'!
moFileForDomain: domainName
"Returns the cached MOFile for the given domainName (or nil).
If none is cached, it will try to load one (every time) and cache it on demand."
	| moFile |
	^moFiles 
		at: domainName 
		ifAbsent: [
			moFile _ self loadMOFileForDomain: domainName.
			moFile notNil ifTrue:[				moFiles at: domainName put: moFile].
			moFile]! !

!GetTextTranslator methodsFor: 'translation'!
translate: aString inDomain: aDomainName
	| mo |
	mo _ self moFileForDomain: aDomainName.
	^mo isNil 
		ifTrue: [aString] 
		ifFalse: [mo translationFor: aString]
! !

!GetTextTranslator methodsFor: 'initialize-release'!
initialize
	moFiles _ Dictionary new.! !

!GetTextTranslator class methodsFor: 'class initialization'!
initialize
	SystemDefaultLocaleDirs _ OrderedCollection new.
	UserDefaultLocaleDirs _ OrderedCollection new.
	LocaleDirsForDomain _ Dictionary new.! !

!GetTextTranslator class methodsFor: 'translation data layout'!
addSystemDefaultLocaleDir: dir
	"new dir will be put as first"
 	self systemDefaultLocaleDirs addFirst: dir! !

!GetTextTranslator class methodsFor: 'translation data layout'!
defaultLocaleDirs
	| dirs |
	dirs _ OrderedCollection new.
	UserDefaultLocaleDirs ifNotNil: [dirs addAll: UserDefaultLocaleDirs].
	dirs addAll: self systemDefaultLocaleDirs.
	^dirs
! !

!GetTextTranslator class methodsFor: 'translation data layout'!
localeDirForDomain: aDomainName
	"returns registered localeDirectory for the textdomain. returns nil if not registered"
	^LocaleDirsForDomain at: aDomainName ifAbsent: [nil]! !

!GetTextTranslator class methodsFor: 'translation data layout'!
localeDirsForDomain: aDomainName
	"returns collection of locale directories for text domain.  
	This includes user defined one for the domain, user defaults and system defaults" 
	| dirs dir |
	dirs _ OrderedCollection new.
	dir _ self localeDirForDomain: aDomainName.
	dir ifNotNil: [dirs add: dir].
	dirs addAll:  self defaultLocaleDirs. 
	
	^dirs! !

!GetTextTranslator class methodsFor: 'translation data layout' stamp: 'hlsf 3/9/2022 16:54:45'!
setLocaleDir: path forDoamin: aDomainName
	self localeDirsForDomain
		at: aDomainName
		put: path.! !

!GetTextTranslator class methodsFor: 'translation data layout' stamp: 'hlsf 3/9/2022 15:47:55'!
setupLocaleDirs
	| dirs |
	SystemDefaultLocaleDirs _ nil.
	dirs _ self systemDefaultLocaleDirs.
	dirs add:  DirectoryEntry smalltalkImageDirectory / 'locale'.
	dirs add:  DirectoryEntry vmDirectory / 'locale'.
	^dirs! !

!GetTextTranslator class methodsFor: 'translation data layout'!
systemDefaultLocaleDirs
	^SystemDefaultLocaleDirs ifNil: [SystemDefaultLocaleDirs _ OrderedCollection new]
! !

!GetTextTranslator class methodsFor: 'translation data layout'!
userDefaultLocaleDirs
	^UserDefaultLocaleDirs ifNil: [UserDefaultLocaleDirs _ OrderedCollection new]
! !

!GetTextTranslator class methodsFor: 'private' stamp: 'hlsf 3/9/2022 17:48:52'!
availableLanguageLocaleIDs
"GetTextTranslator availableLanguageLocaleIDs"
	| ids dirs localeDirForLang directoryNames |
	ids _ Set new.
	dirs _ Set new.
	dirs addAll: LocaleDirsForDomain values.
	dirs addAll: self defaultLocaleDirs.
	dirs do: [:dir |
		| localesDir |
		localesDir _ dir assureExistence. 
		directoryNames _ [localesDir directoryNames] on: FileDoesNotExistException do: [:e | #()].
		directoryNames			do: [:langDirName | | localeID  |
			localeID _ LocaleID posixName: langDirName.
			localeDirForLang _ (localesDir / localeID posixName / 'LC_MESSAGES') assureExistence.
			localeDirForLang ifNotNil: [
				(localeDirForLang fileMatching: '*.mo') ifNotNil: [ids add: localeID]
			]
		].
	].
	^ids! !

!GetTextTranslator class methodsFor: 'private' stamp: 'hlsf 3/9/2022 17:48:45'!
createAvailableTranslators
"Load new instances of translators corresponding to the currently available translation files"
	self setupLocaleDirs.
	self availableLanguageLocaleIDs do: [ :localeID |
		NaturalLanguageTranslator translators 
			at: localeID 
			put: (self newForLocaleID: localeID).
	]! !

!GetTextTranslator class methodsFor: 'private' stamp: 'hlsf 3/9/2022 15:57:31'!
findMOForLocaleID: id domain: aDomainName
	| moFile |
	(self localeDirsForDomain: aDomainName) do: [:each |
		moFile _ each / id posixName / 'LC_MESSAGES' // (aDomainName, '.mo').
		[moFile exists ifTrue: [^moFile] ] 
			on: FileDoesNotExistException 
			do: [:e | ^nil]].
	^nil! !

!GetTextTranslator class methodsFor: 'private'!
localeDirsForDomain
	^LocaleDirsForDomain ifNil: [LocaleDirsForDomain _ Dictionary new]! !

!GetTextTranslator class methodsFor: 'private'!
privateStartUp

	self createAvailableTranslators! !

!GetTextTranslator class methodsFor: 'instance creation'!
newForLocaleID: id
	^self new localeID: id! !

!GetTextTranslator class methodsFor: 'actions'!
reset
	"Flush remembered stuff.
	Load new translators (based on the files currently found)"
	
	super reset.
	
	self createAvailableTranslators 
	 
	! !

!TextDomainInfo methodsFor: 'initialize-release'!
initialize
	categoryPrefixes _ Set new.
	categories _ IdentitySet new.
! !

!TextDomainInfo methodsFor: 'private'!
category: categoryName matches: prefix
	^ categoryName notNil and: [categoryName = prefix or: [categoryName beginsWith: prefix, '-']]! !

!TextDomainInfo methodsFor: 'accessing'!
categories
	^categories! !

!TextDomainInfo methodsFor: 'accessing'!
categoryPrefixes
	^categoryPrefixes! !

!TextDomainInfo methodsFor: 'accessing'!
includesCategory: categorySymbol
	(categories includes: categorySymbol) ifTrue: [^true].
	categoryPrefixes do: [:each |
		(self category: categorySymbol matches: each) ifTrue: [^true]
	].

	^false.! !

!TextDomainInfo methodsFor: 'accessing'!
matchedSystemCategories
	^SystemOrganization categories 
		select: [:cat | self includesCategory: cat]! !

!TextDomainManager class methodsFor: 'accessing'!
allKnownDomains
	| domains |
	domains _ Set new.
	domains addAll: ClassCategories values.
	domains add: self defaultDomain.
	^domains
! !

!TextDomainManager class methodsFor: 'accessing'!
defaultDomain
	^DefaultDomain! !

!TextDomainManager class methodsFor: 'accessing'!
defaultDomain: aDomainName
	DefaultDomain _ aDomainName! !

!TextDomainManager class methodsFor: 'accessing'!
domainForClass: aClass
	^Classes at: aClass theNonMetaClass name ifAbsent: [self defaultDomain]! !

!TextDomainManager class methodsFor: 'accessing'!
domainForClassCategory: aCategorySymbol
	^ClassCategories at: aCategorySymbol ifAbsent: [self defaultDomain]! !

!TextDomainManager class methodsFor: 'accessing'!
registerCategoryPrefix: aString domain: aDomainName
	| domInfo |
	domInfo _ self domainInfoFor: aDomainName.
	domInfo categoryPrefixes add: aString.
	self refresh.! !

!TextDomainManager class methodsFor: 'accessing'!
registerClass: className domain: aDomainName
	LoneClasses at: className put: aDomainName.
	self refresh.	"moves it to Classes"
! !

!TextDomainManager class methodsFor: 'accessing'!
registerClassCategory: categorySymbol domain: aDomainName
	| domInfo |
	domInfo _ self domainInfoFor: aDomainName.
	domInfo categories add: categorySymbol.
	self refresh.
! !

!TextDomainManager class methodsFor: 'accessing'!
registerDomain: domainName
	| domInfo |
	domInfo _ TextDomainInfo new.
	DomainInfos at: domainName put: domInfo.
	NaturalLanguageTranslator domainRegistered: domainName.
	^domInfo! !

!TextDomainManager class methodsFor: 'accessing'!
unregisterDomain: domainName
	DomainInfos removeKey: domainName.
	self refresh.
	NaturalLanguageTranslator domainUnregistered: domainName.
! !

!TextDomainManager class methodsFor: 'private'!
domainInfoFor: domainName
	^DomainInfos at: domainName ifAbsentPut: [ self registerDomain: domainName]! !

!TextDomainManager class methodsFor: 'private'!
domainInfos
	^DomainInfos! !

!TextDomainManager class methodsFor: 'private'!
refresh
	ClassCategories _ IdentityDictionary new.
	Classes _ IdentityDictionary new.
	DomainInfos keysAndValuesDo: [:domainName :domainInfo |
		domainInfo matchedSystemCategories do: [:cat |
			ClassCategories at: cat ifPresent: [self error: 'category ', (cat asString) , '  belongs to multiple domains'].
			ClassCategories at: cat put: domainName.
			(SystemOrganization listAtCategoryNamed: cat ) do: [ :cls |
				Classes at: cls put: domainName.
			]
		]
	].
	Classes addAll: LoneClasses.! !

!TextDomainManager class methodsFor: 'class initialization' stamp: 'hlsf 3/5/2022 12:17:54'!
initialize
	"	TextDomainManager initialize	"

	ClassCategories _ IdentityDictionary new.
	Classes _ IdentityDictionary new.
	LoneClasses _ IdentityDictionary new.
	DomainInfos _ Dictionary new.
	self defaultDomain: 'cuis'.! !

!TranslatedReceiverFinder methodsFor: 'private'!
arraySearch: aSymbol fromArray: anArray addTo: aCollection 
	"Find literals ahead of aSymbol from arrays in the method."
	"BUG: it can handle just one occurrence"
	| index |
	(index _ anArray identityIndexOf: aSymbol) > 1
		ifTrue: [aCollection add: (anArray at: index - 1) asString].
	(anArray
		select: [:each | each isMemberOf: Array])
		do: [:each | self
				arraySearch: aSymbol
				fromArray: each
				addTo: aCollection].
	^ aCollection! !

!TranslatedReceiverFinder methodsFor: 'private'!
arraySearch: aSymbol messageNode: aParseNode addTo: aCollection 
	"Find literals ahead of aSymbol from arrays in the method."
	
	aParseNode nodesDo: [:node |
		node isLiteral ifTrue: [
			(node literalValue isMemberOf: Array) ifTrue: [
				self 
					arraySearch: aSymbol
					fromArray: node literalValue
					addTo: aCollection]]].		
	^ aCollection! !

!TranslatedReceiverFinder methodsFor: 'private' stamp: 'hlsf 3/5/2022 11:42:28'!
search: aSymbol messageNode: aParseNode addTo: aCollection 
	aParseNode nodesDo: [:node | 
		node isMessage ifTrue: [
			node selectorSymbol = aSymbol ifTrue: [
				aCollection add: node]]].
	^ aCollection! !

!TranslatedReceiverFinder methodsFor: 'accessing' stamp: 'hlsf 3/5/2022 11:54:04'!
findWordsWith: aSymbol in: aMethodReference 
	"Find words for translation with the symbol in a method. See
	LanguageEditorTest >>testFindTranslatedWords"
	"| message | 
	message _ MethodReference new setStandardClass: Morph class
	methodSymbol: #supplementaryPartsDescriptions.
	self new findWordsWIth: #translatedNoop in: message"
	| messages keywords methodNode |

	methodNode _ aMethodReference methodNode.
	"Find from string literal"
	messages _ Set new.
	self
		search: aSymbol
		messageNode: methodNode
		addTo: messages.
	keywords _ OrderedCollection new.
	messages
		select: [:aMessageNode | aMessageNode receiver isLiteralNode]
		thenDo: [:aMessageNode | keywords add: aMessageNode receiver literalValue			].
	"Find from array literal"
	self
		arraySearch: aSymbol
		messageNode: methodNode
		addTo: keywords.
	^ keywords! !

!TranslatedReceiverFinder methodsFor: 'accessing' stamp: 'hlsf 3/5/2022 11:10:34'!
nonLiteralReceiversOf: aSymbol
	"self new nonLiteralReceiversOf: aSymbol"
	| receivers |
	"Answer method references of non literal senders of #translated"
	^ (Smalltalk allCallsOn: aSymbol)
		select: [:message | 
			receivers _ OrderedCollection new.
			self search: aSymbol messageNode: message methodNode addTo: receivers.
			receivers
				anySatisfy: [:each | (each receiver isLiteralNode) not]]! !

!TranslatedReceiverFinder methodsFor: 'accessing' stamp: 'hlsf 3/5/2022 11:47:45'!
stringReceiversOf: aSymbol
	"Find string receivers for a symbol.
	Answer a collection of aMethodReference -> {keyword. keyword...}"
	"self new stringReceiversOf: #translated"
	| keywords methodReferences |
	methodReferences _ Smalltalk allCallsOn: aSymbol.
	^ methodReferences inject: OrderedCollection new into: [:list :next |
		keywords _ self findWordsWith: aSymbol in: next.
		keywords
			ifNotEmpty: [list add: next -> keywords].
		list]
! !

!TranslatedReceiverFinder methodsFor: 'actions'!
searchBlockNode: aBlockNode addTo: aCollection

	aBlockNode statements do: [:e |
		(e isMemberOf: MessageNode) ifTrue: [self searchMessageNode: e addTo: aCollection].
		(e isMemberOf: ReturnNode) ifTrue: [self searchReturnNode: e addTo: aCollection].
	].
! !

!TranslatedReceiverFinder methodsFor: 'actions'!
searchMessageNode: aMessageNode addTo: aCollection

	((aMessageNode receiver isMemberOf: LiteralNode) and: [(aMessageNode selector isMemberOf: SelectorNode) and: [aMessageNode selector key = #translated]]) ifTrue: [
		aCollection add: aMessageNode receiver key.
	].

	(aMessageNode receiver isMemberOf: BlockNode) ifTrue: [self searchBlockNode: aMessageNode receiver addTo: aCollection].
	(aMessageNode receiver isMemberOf: MessageNode) ifTrue: [self searchMessageNode: aMessageNode receiver addTo: aCollection].
	(aMessageNode receiver isMemberOf: ReturnNode) ifTrue: [self searchReturnNode: aMessageNode receiver addTo: aCollection].

	aMessageNode arguments do: [:a |
		(a isMemberOf: BlockNode) ifTrue: [self searchBlockNode: a addTo: aCollection].
		(a isMemberOf: MessageNode) ifTrue: [self searchMessageNode: a addTo: aCollection].
		(a isMemberOf: ReturnNode) ifTrue: [self searchReturnNode: a addTo: aCollection].
	].
! !

!TranslatedReceiverFinder methodsFor: 'actions'!
searchMethodNode: aMethodNode addTo: aCollection

	(aMethodNode block isMemberOf: BlockNode) ifTrue: [self searchBlockNode: aMethodNode block addTo: aCollection].
	(aMethodNode block isMemberOf: MessageNode) ifTrue: [self searchMessageNode: aMethodNode block addTo: aCollection].
	(aMethodNode block isMemberOf: ReturnNode) ifTrue: [self searchReturnNode: aMethodNode block addTo: aCollection].
! !

!TranslatedReceiverFinder methodsFor: 'actions'!
searchReturnNode: aReturnNode addTo: aCollection

	(aReturnNode expr isMemberOf: BlockNode) ifTrue: [self searchBlockNode: aReturnNode expr addTo: aCollection].
	(aReturnNode expr isMemberOf: MessageNode) ifTrue: [self searchMessageNode: aReturnNode expr addTo: aCollection].
! !

!TranslatedReceiverFinder methodsFor: 'actions' stamp: 'hlsf 3/5/2022 11:07:48'!
senders
	| m o |
	m _ Smalltalk allCallsOn: #translated.
	m _ m
		collect: [ :e | 
			e classIsMeta
				ifTrue: [ (Smalltalk globals at: e classSymbol) class decompile: e methodSymbol ]
				ifFalse: [ (Smalltalk globals at: e classSymbol) decompile: e methodSymbol ] ].
	o _ OrderedCollection new.
	m do: [ :e | self searchMethodNode: e addTo: o ].
	^ o sort! !

!CharacterSequence methodsFor: '*Gettext' stamp: 'hlsf 10/31/2022 21:54:01'!
translated
"answer the receiver translated to the default language"
	^ NaturalLanguageTranslator translate: self
! !

!CharacterSequence methodsFor: '*Gettext' stamp: 'hlsf 10/31/2022 21:54:07'!
translatedTo: localeID 
	"answer the receiver translated to the given locale id"
	^ NaturalLanguageTranslator translate: self toLocale: localeID! !
MOFile initialize!
NaturalLanguageTranslator initialize!
GetTextTranslator initialize!
TextDomainManager initialize!
