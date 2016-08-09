'From Cuis 4.2 of 25 July 2013 [latest update: #2859] on 9 August 2016 at 10:22:34 am'!

!Collection methodsFor: 'comparing' stamp: 'jmv 8/9/2016 09:35:07'!
= other
	"Default implementation. Usually redefined in subclasses."

	^self == other! !


!ByteArray methodsFor: 'comparing' stamp: 'jmv 8/9/2016 09:56:13'!
= another
	"Should do something with BitBlt... Same for Bitmap, and any other Words or Byes object!!
	OTOH, String uses our #hashBytes:startingWith: . We could use their equality..."
	self == another ifTrue: [ ^ true ].
	^super = another! !


!FloatArray methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:20:02'!
primitiveEqual: aFloatArray 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	aFloatArray class == self class ifFalse: [^ false].
	length _ self size.
	length = aFloatArray size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (aFloatArray at: i) ifFalse: [^ false]].
	^ true! !


!Behavior methodsFor: 'accessing class hierarchy' stamp: 'jmv 8/9/2016 10:19:29'!
subclasses
	"slow implementation since Behavior does not keep trace of subclasses"
	
	^ self class allInstances  select: [:each | each superclass == self ]! !


!LookupKey methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:01:30'!
= aLookupKey

	self == aLookupKey ifTrue: [ ^ true ].
	self species == aLookupKey species
		ifFalse: [ ^false ].

	^ key = aLookupKey key! !


!Association methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:01:44'!
= anAssociation

	self == anAssociation ifTrue: [ ^ true ].
	^ super = anAssociation and: [value = anAssociation value]! !


!WeakKeyAssociation methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:01:54'!
= aLookupKey
	self == aLookupKey ifTrue: [ ^ true ].

	self species == aLookupKey species
		ifFalse: [ ^ false ].

	^self key = aLookupKey key! !


!MessageTally methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:02:02'!
= aMessageTally

	self == aMessageTally ifTrue: [ ^ true ].
	self species == aMessageTally species ifFalse: [^ false].
	^ aMessageTally method == method! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 8/9/2016 10:20:07'!
printOn: aStream

	| myTally aSelector aClass |
	myTally := tally.
	receivers
		ifNotNil: [receivers do: [:r | myTally := myTally - r tally]].
	aStream
		print: myTally;
		space.
	receivers
		ifNil: [
			aStream
				nextPutAll: 'primitives';
				newLine]
		ifNotNil: [
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass == class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: aSelector;
				newLine ]! !


!Complex methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:14:47'!
= anObject
	self == anObject ifTrue: [ ^ true ].
	anObject isNumber ifFalse: [^false].
	^anObject isComplex
		ifTrue: [(real = anObject real) & (imaginary = anObject imaginary)]
		ifFalse: [anObject adaptToComplex: self andSend: #=]! !


!Timespan methodsFor: 'ansi protocol' stamp: 'jmv 8/9/2016 10:20:50'!
= comparand
 	self == comparand ifTrue: [ ^ true ].
	^ self class == comparand class 
		and: [ self start = comparand start ]
		and: [ self duration = comparand duration ]
.! !


!Schedule methodsFor: 'squeak protocol' stamp: 'jmv 8/9/2016 10:20:31'!
includes: aDateAndTime

	| dt |
	self assert: aDateAndTime class == DateAndTime.	"Or else understand and fix"
	dt _ aDateAndTime.
	self scheduleDo: [ :e | e = dt ifTrue: [^true] ].
	^ false.
! !


!Pragma class methodsFor: 'finding' stamp: 'jmv 8/9/2016 10:20:20'!
allNamed: aSymbol from: aSubClass to: aSuperClass
	"Answer a collection of all pragmas found in methods of all classes between aSubClass and aSuperClass (inclusive) whose keyword is aSymbol."
	
	^ Array streamContents: [ :stream |
		aSubClass withAllSuperclassesDo: [ :class |
			self withPragmasIn: class do:  [ :pragma |
				pragma keyword = aSymbol
					ifTrue: [ stream nextPut: pragma ] ].
			aSuperClass == class
				ifTrue: [ ^ stream contents ] ] ].! !


!CharacterSet methodsFor: 'comparison' stamp: 'jmv 8/9/2016 09:35:34'!
= anObject
	self == anObject ifTrue: [ ^ true ].
	^self species == anObject species and: [
		self byteArrayMap = anObject byteArrayMap ]! !


!FloatArray methodsFor: 'comparing' stamp: 'jmv 8/9/2016 09:36:37'!
= another 
	self == another ifTrue: [ ^ true ].
	^self primitiveEqual: another! !


!RunArray methodsFor: 'accessing' stamp: 'jmv 8/9/2016 09:54:15'!
= otherArray 
	self == otherArray ifTrue: [ ^ true ].

	"Test if all my elements are equal to those of otherArray"
	(otherArray isMemberOf: RunArray) ifFalse: [^ self hasEqualElements: otherArray].

	"Faster test between two RunArrays"
 	^ (runs hasEqualElements: otherArray runs)
		and: [values hasEqualElements: otherArray values]! !


!Text methodsFor: 'comparing' stamp: 'jmv 8/9/2016 09:57:55'!
= other
	"Am I equal to the other Text or String?  
	***** Warning ***** Two Texts are considered equal if they have the same characters in them.  They might have completely different emphasis, fonts, sizes, text actions, or embedded morphs.  If you need to find out if one is a true copy of the other, you must do (text1 = text2 and: [text1 runs = text2 runs])."

	self == other ifTrue: [ ^ true ].
	(other is: #Text) ifTrue: [ "This is designed to run fast even for megabytes"
		^ string == other string or: [string = other string]].
	other isString ifTrue: [^ string == other or: [string = other]].
	^ false! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/9/2016 10:20:35'!
removeTextCode    "Smalltalk removeTextCode"
	"Scan all methods for source code that is Text (i.e. with attributes)
	Replace it with simpleStrings"
	 | oldCodeString n newCodeString oldStamp oldCategory m classComment c o stamp |
	'Scanning sources for Text.
This will take a few moments...'
		displayProgressAt: Sensor mousePoint
		from: 0 
		to: CompiledMethod instanceCount
		during: [ :bar | 
			n _ 0.
			m _ 0.
			c _ 0.
			Smalltalk allBehaviorsDo: [ :cls | 
				cls selectors do: [ :selector  | 
					(n _ n+1) \\ 100 = 0 ifTrue: [bar value: n].
					oldCodeString _ cls sourceCodeAt: selector.
					oldCodeString class == String ifFalse: [
						newCodeString _ oldCodeString asString.
						oldStamp _ (cls compiledMethodAt: selector) timeStamp.
						oldCategory _ cls whichCategoryIncludesSelector: selector.
						cls compile: newCodeString classified: oldCategory withStamp: oldStamp notifying: nil.
						m _ m + 1]].
				cls isMeta ifFalse: [
					o _ cls organization.
					classComment _ o classComment.
					stamp _ o commentStamp.
					classComment class == String ifFalse: [
						classComment hasAnyAttribute ifTrue: [
							self halt "review" ].
						cls classComment: classComment asString stamp: stamp.
						c _ c + 1 ]].
			].
		].
		Transcript newLine; show: m printString , ' text methods converted to strings.'.
		Transcript newLine; show: c printString , ' text class comments converted to strings.'.! !


!ContentPack methodsFor: 'importing' stamp: 'jmv 8/9/2016 10:19:41'!
decodeContentFrom: associationList

	| associations | 
	
	associations _ associationList collect: [ :assoc |
		(assoc at: 2) class == Array
			ifTrue: [ (assoc at: 1) -> (self decodeContentFrom: (assoc at: 2)) ]
			ifFalse: [ 
				(assoc at: 1) -> (Compiler 
					evaluate: (self 
						perform: 
							('object' , (assoc at: 2) 
								asString) asSymbol) base64Decoded)
			]
	].

	^ associations as: Dictionary! !


!ContentPack class methodsFor: 'code pack' stamp: 'jmv 8/9/2016 10:19:46'!
encodeContentFrom: aDictionary

	| encodedTree |
	
	encodedTree _ Dictionary new.
	
	aDictionary associations
		do: [ :assoc |
			assoc value class == Dictionary
				ifTrue: [ 
					encodedTree 
						at: assoc key 
						put: (self encodeContentFrom: assoc value) 
				]
				ifFalse: [ 
					encodedTree 
						at: assoc key
						put: count.
					count _ count + 1.
					payload add: assoc value storeString base64Encoded
				]
		].

	^ encodedTree associations collect: [ :assoc | 
		{ assoc key . assoc value }
	]! !


!Array2D methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:11:56'!
= another
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^false ].
	width = another width ifFalse: [ ^false ].
	height = another height ifFalse: [ ^false ].
	^elements = another elements! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 8/9/2016 10:20:27'!
printOn: textStream linesOn: linesStream talliesOn: talliesStreams tabs: tabsAndTreeLines total: total totalTime: totalTime

	| aSelector aClass percentage line |
	line _ String streamContents: [ :lineStream |
		tabsAndTreeLines do: [ :tabOrLineChar | lineStream nextPutAll: tabOrLineChar ].
		percentage _ tally asFloat / total * 100.0.
		percentage printOn: lineStream fractionDigits: 2.
		lineStream
			nextPutAll: '% (';
			nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
			nextPutAll: ' ms) '.
		aSelector _ class selectorAtMethod: method setClass: [ :c | aClass _ c].
			blockNesting > 0 ifTrue: [
				lineStream
					next: blockNesting put: $[;
					next: blockNesting put: $];
					space ].
		lineStream
			nextPutAll: class name;
			nextPutAll: (aClass == class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
			nextPutAll: aSelector.
		wasInPrimitive ifTrue: [ lineStream nextPutAll: '   -- primitive (real sender possibly omitted)' ].
		].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStreams nextPut: self! !


!TrieNode methodsFor: 'removing' stamp: 'jmv 8/9/2016 10:21:05'!
removeKey: aString ifAbsent: aBlock characterIndex: i

	| child answer |
	child _ self childFor: aString characterIndex: i orAdd: false.
	child ifNil: [ ^aBlock value ].
	answer _ child removeKey: aString ifAbsent: aBlock characterIndex: i + segmentSize.
	child isEmpty ifTrue: [
		self removeChildFor: aString characterIndex: i.
		"If child is not needed anymore, compress the path"	
		children class == TrieNode ifTrue: [
			someKey _ children getSomeKey.
			segmentSize _ segmentSize + children getSegmentSize.
			childrenFirstChars _ children getChildrenFirstChars.
			children _ children getChildren ]].
	^answer! !


!TextDoIt methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:04:07'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other evalString = evalString]! !


!TextURL methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:04:13'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other url = url]! !


!TextAlignment methodsFor: 'as yet unclassified' stamp: 'jmv 8/9/2016 10:04:23'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other alignment = alignment]! !


!TextColor methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:04:35'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other color = color]! !


!TextEmphasis methodsFor: 'as yet unclassified' stamp: 'jmv 8/9/2016 10:04:42'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other emphasisCode = emphasisCode]! !


!TextFontFamilyAndSize methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:04:48'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [ other pointSize = pointSize
			and: [ other familyName = familyName ]]! !


!TextAnchor methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:04:31'!
= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other anchoredFormOrMorph = anchoredFormOrMorph]! !


!FileSystemEntry methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:19:58'!
= aFileEntry
	| isCaseSensitive myDrive otherDrive theirs mine |

	self == aFileEntry ifTrue: [ ^ true ].
	self class == aFileEntry class ifFalse: [ ^false ].

	isCaseSensitive _ self fileAccessor isCaseSensitive.

	"Check for drive nil or same."
	myDrive _ self drive.
	otherDrive _ aFileEntry drive.
	isCaseSensitive
		ifTrue: [ self drive = aFileEntry drive ifFalse: [ ^false ]]
		ifFalse: [
			myDrive isNil = otherDrive isNil ifFalse: [ ^false ].		"only one of them is nil"
			myDrive ifNotNil: [											"none is nil"
				(myDrive sameAs: otherDrive) ifFalse: [ ^false ]]].

	"Check for all path components same."
	mine _ self pathComponents.
	theirs _ aFileEntry pathComponents.

	isCaseSensitive ifTrue: [
		^mine = theirs ].

	mine size = theirs size ifFalse: [
		^false ].

	mine with: theirs do: [ :m :t |
		(m sameAs: t) ifFalse: [ ^false ]].

	^ true! !


!CodePackage methodsFor: 'comparing' stamp: 'jmv 8/9/2016 09:28:46'!
= other

	self == other ifTrue: [ ^ true ].
	^ self species == other species and: [ packageName = other packageName ]! !


!FeatureRequirement methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:19:52'!
= another
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^false ].
	^name = another name and: [
		minVersion = another minVersion and: [
		minRevision = another minRevision and: [
		maxVersion = another maxVersion ]]]! !


!Form methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:00:45'!
= other 
	"Compare for equal contents. Expensive!!
	See comment at #hash"

	self == other ifTrue: [ ^ true ].

	^ (other class == self class) 
		and: [ other width = width
			and: [ other height = height
				and: [ other depth = self depth
					and: [ other offset = self offset
						and: [ other bits = bits ]]]]]! !


!AffineTransformation methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:19:19'!
= anAffineTransformation 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	self == anAffineTransformation ifTrue: [ ^ true ].
	self class == anAffineTransformation class ifFalse: [^ false].
	length := self size.
	length = anAffineTransformation size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (anAffineTransformation at: i) ifFalse: [^ false]].
	^ true! !


!Point methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:03:11'!
= aPoint

	self == aPoint ifTrue: [ ^ true ].
	self species == aPoint species ifFalse: [ ^ false ].

	^ x = aPoint x and: [ y = aPoint y ]! !


!Rectangle methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:03:38'!
= aRectangle 

	self == aRectangle ifTrue: [ ^ true ].

	"Answer true if the receiver's species, origin and corner match aRectangle's."
	self species == aRectangle species
		ifFalse: [ ^false ].

	^ origin = aRectangle origin and: [ corner = aRectangle corner ]! !


!CharacterBlock methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:03:49'!
= aCharacterBlock

	self == aCharacterBlock ifTrue: [ ^ true ].

	self species == aCharacterBlock species
		ifFalse: [ ^false ].
		
	^ stringIndex = aCharacterBlock stringIndex! !


!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'jmv 8/9/2016 10:20:16'!
genericMenu
	"Borrow a menu from my inspector"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: '*nothing selected*'
				target: self
				selector: #yourself]
		ifNotNil: [
			aMenu addList: #(
				('inspect (i)'							inspectSelection)
				('explore (I)'						exploreSelection)
				('basic inspect'						inspectBasic)
				('references finder'					openReferencesFinder)
				('weight explorer'					openWeightExplorer)
				-
				('browse full (b)'					browseMethodFull)
				('browse hierarchy (h)'				browseHierarchy)
				('browse protocol (p)'				browseFullProtocol)).
			aMenu addLine;
				add: 'monitor changes'
				target: self
				selector: #monitor:
				argument: model getCurrentSelection.
			model class == ReferencesExplorer ifTrue: [
				aMenu addLine;
					add: 'rescan'
					target: self
					selector: #rescan ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: self
				selector: #stopMonitoring ].
	^ aMenu! !


!KeyboardEvent methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:18:52'!
= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^false ].

	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	keyValue = aMorphicEvent keyValue ifFalse: [ ^ false ].
	^ true! !


!MouseEvent methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:19:02'!
= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^ false ].

	type = aMorphicEvent eventType ifFalse: [ ^ false ].
	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	^ true! !


!MouseMoveEvent methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:20:11'!
= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^ false ].

	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	^ true! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 8/9/2016 10:21:01'!
openMenu
	"Build the open window menu for the world."
	| menu items groups firstGroup itemsSorted itemsBase |
	menu _ self menu: 'Open...'.
	itemsBase _ (Smalltalk allClassesImplementing: #worldMenuForOpenGroup)
		collect: [ :item |
			item class == Metaclass ifTrue: [
				item soleInstance
					worldMenuForOpenGroup ] ]
		thenSelect: [ :item |
			item notNil ].
	items _ OrderedCollection new.
	"A single class may add more than one item to a menu"
	itemsBase do: [ :item |
		item class == Dictionary
			ifTrue: [ items add: item ]
			ifFalse: [ items addAll: item ]].
	groups _ (items collect: [ :item |
		item at: #itemGroup ]) asSet asSortedCollection.
	itemsSorted _ OrderedCollection new.
	firstGroup _ true.
	groups do: [ :group |
		firstGroup
			ifTrue: [ firstGroup _ false ]
			ifFalse: [ itemsSorted add: nil ].
		((items select: [ :item |
			(item at: #itemGroup) = group ]) sort: [ :item1 :item2 |
			(item1 at: #itemOrder) < (item2 at: #itemOrder) ]) do: [ :item |
			itemsSorted add: item ]].
	self
		fillIn: menu
		fromDictionaries: itemsSorted.
	^ menu.! !


!TextLine methodsFor: 'comparing' stamp: 'jmv 8/9/2016 10:04:58'!
= line
 	self == line ifTrue: [ ^ true ].

	self species == line species
		ifFalse: [ ^ false ].

	^((firstIndex = line first and: [ lastIndex = line last ])
		and: [ internalSpaces = line internalSpaces ])
		and: [ paddingWidth = line paddingWidth ]! !


!MethodReference methodsFor: 'comparisons' stamp: 'jmv 8/9/2016 10:02:49'!
= anotherMethodReference 
	"Answer whether the receiver and the argument represent the 
	same object."
	self == anotherMethodReference ifTrue: [ ^ true ].
	^ self species == anotherMethodReference species
		and: [self classSymbol == anotherMethodReference classSymbol]
		and: [self classIsMeta == anotherMethodReference classIsMeta]
		and: [self methodSymbol == anotherMethodReference methodSymbol]! !

