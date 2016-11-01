'From Cuis 4.2 of 25 July 2013 [latest update: #2960] on 1 November 2016 at 9:59:13 am'!

!SystemDictionary commentStamp: '<historical>' prior: 0!
I represent a special dictionary that supports protocol for asking questions about the structure of the system. Other than class names, I contain (print this)...

	Smalltalk globals!


!Pragma methodsFor: 'testing' stamp: 'jmv 9/27/2016 21:16:24'!
is: aSymbol
	^ aSymbol == #Pragma or: [ super is: aSymbol ]! !


!PseudoClass methodsFor: 'testing' stamp: 'jmv 9/27/2016 20:05:37'!
is: aSymbol
	^ aSymbol == #PseudoClass or: [ super is: aSymbol ]! !


!CodeFileBrowser methodsFor: 'other' stamp: 'jmv 9/27/2016 20:05:48'!
didCodeChangeElsewhere
	"Determine whether the code for the currently selected method and class has been changed somewhere else."

	| aClass |
	(aClass _ self selectedClassOrMetaClass) ifNil: [^ false].

	(aClass is: #PseudoClass) ifTrue: [^ false]. "class not installed"
	^super didCodeChangeElsewhere! !


!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 9/27/2016 21:40:43'!
objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	self hasSubject ifTrue: [
		(self subject class isMeta) ifTrue: [
			dp _ DiskProxy global: self subject name selector: #organization args: #().
			refStrm replace: self with: dp.
			^ dp]].
	^ self	"in desparation"
! !


!AdditionalMethodState methodsFor: 'testing' stamp: 'jmv 9/27/2016 21:16:32'!
hasAtLeastTheSamePropertiesAs: aMethodProperties
	"Answer if the recever has at least the same properties as the argument.
	 N.B. The receiver may have additional properties and still answer true."
	aMethodProperties keysAndValuesDo:
		[:k :v|
		(v is: #Pragma)
			"ifTrue: [Pragmas have already been checked]"
			ifFalse: [
				(self includes: k->v) ifFalse: [^false]]].
	^true! !


!Message methodsFor: 'stub creation' stamp: 'jmv 9/27/2016 21:14:46'!
createStubMethod
	| argNames aOrAn argName arg argClassName |
	argNames _ Set new.
	^ String streamContents: [ :s |
		self selector keywords withIndexDo: [ :key :i |
			s nextPutAll: key.
			((key last = $:) or: [self selector isInfix]) ifTrue: [
				arg _ self arguments at: i.
				argClassName _ (arg class isMeta) ifTrue: ['Class'] ifFalse: [arg class name].
				aOrAn _ argClassName first isVowel ifTrue: ['an'] ifFalse: ['a'].
				argName _ aOrAn, argClassName.
				[argNames includes: argName] whileTrue: [argName _ argName, i asString].
				argNames add: argName.
				s nextPutAll: ' '; nextPutAll: argName; space
			].
		].
		s newLine; tab.
		s nextPutAll: 'self shouldBeImplemented'
	]! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 9/27/2016 21:36:09'!
fillCaches
	"Fill cachedClassNames and cachedNonClassNames. Return an array with the calculated values."

	| classNames nonClassNames |
	classNames _ IdentitySet new: self size.
	nonClassNames _ IdentitySet new.
	self keysAndValuesDo: [ :key :value |
		value isInMemory ifTrue: [
			"The key == value name test below addresses two separate issues:
				1) Obsolete classes, where key = #Foo and value name = 'AnObsoleteFoo'
				2) Aliases, i.e., Smalltalk at: #OtherName put: aClass"
			(value class isMeta and: [ key == value name ])
				ifTrue: [ classNames add: key ]
				ifFalse: [ nonClassNames add: key ] ] ].
	classNames _ classNames asArray sort.
	nonClassNames _ nonClassNames asArray sort.
	cachedClassNames _ classNames.
	cachedNonClassNames _ nonClassNames.
	^{ classNames. nonClassNames }! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 9/27/2016 21:29:08'!
hasClassNamed: aString
	"Answer whether there is a class of the given name, but don't intern aString if it's not alrady interned.  4/29/96 sw
	Smalltalk hasClassNamed: 'Morph'
	Smalltalk hasClassNamed: 'Display'
	Smalltalk hasClassNamed: 'xMorph'
	"

	Symbol hasInterned: aString ifTrue: [ :aSymbol |
		self at: aSymbol ifPresent: [ :global | ^global class isMeta ]].
	^ false! !

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 9/27/2016 21:33:56'!
globals
	"
	Smalltalk  globals
	"
	
	| globals |
	globals _ IdentityDictionary new.
	self associationsDo: [ :assoc |
		assoc value class isMeta not ifTrue: [ globals add: assoc ]].
	^globals! !


!ContentPack methodsFor: 'converting' stamp: 'jmv 9/27/2016 21:19:58'!
asContentPack: aDictionary

	"Recursively make ContentPacks of all objects which include behavior from Dictionary."

	| associations |

	associations _ aDictionary keys collect: [ :key | 
		((aDictionary at: key) is: #Dictionary)
			ifTrue: [ key -> (self asContentPack: (aDictionary at: key)) ]
			ifFalse: [ key -> (aDictionary at: key) ]	
	].

	^ associations as: ContentPack! !

!ContentPack methodsFor: 'converting' stamp: 'jmv 9/27/2016 21:21:19'!
asDictionary

	"Recursively make Dictionaries of all objects which include behavior from ContentPack."

	| associations |

	associations _ self keys collect: [ :key | 
		((self at: key) is: #ContentPack)
			ifTrue: [ key -> (self at: key) asDictionary ]
			ifFalse: [ key -> (self at: key) ]	
	].

	^ associations as: Dictionary! !

!ContentPack methodsFor: 'converting' stamp: 'jmv 9/27/2016 21:21:33'!
fixFormKeysIn: aContentPack

	"Need to add file extension to the keys for the Forms; 
	it made sense to strip them out before I realized that 
	ContentPack could be used for other media types, 
	which suddenly meant that file type was no longer implicit.."

| associations |

	self flag: #todo. "Remove this in a later change set. This is only needed once to get the icons working right again."


	associations _ aContentPack keys collect: [ :key |
		((aContentPack at: key) is: #Dictionary)
			ifTrue: [ key -> (self fixFormKeysIn: (aContentPack at: key)) ] "Recurse."
			ifFalse: [ (key , '.png') -> (aContentPack at: key) ] "Fix leaf."
	].

	^ associations as: ContentPack
			! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 9/27/2016 21:41:18'!
classFromPattern: pattern withCaption: aCaption
	"If there is a class whose name exactly given by pattern, return it.
	If there is only one class in the system whose name matches pattern, return it.
	Otherwise, put up a menu offering the names of all classes that match pattern, and return the class chosen, else nil if nothing chosen.
	This method ignores tab, space, & newline characters in the pattern"

	| toMatch potentialClassNames classNames exactMatch index |
	(toMatch _  pattern copyWithoutAll:
			{Character space.  Character newLineCharacter.  Character tab})
		isEmpty ifTrue: [^ nil].
	Symbol hasInterned: toMatch ifTrue:
		[:patternSymbol | Smalltalk at: patternSymbol ifPresent:
			[:maybeClass | (maybeClass class isMeta) ifTrue: [^ maybeClass]]].

	toMatch _ (toMatch copyWithout: $.) asLowercase.
	potentialClassNames _ Smalltalk classNames asOrderedCollection.
	classNames _ pattern last = $. 
		ifTrue: [potentialClassNames select:
					[:nm |  nm asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: 
					[:n | n includesSubstring: toMatch caseSensitive: false]].
	classNames isEmpty ifTrue: [^ nil].
	exactMatch _ classNames detect: [:each | each asLowercase = toMatch] ifNone: nil.

	index _ classNames size = 1
		ifTrue:	[1]
		ifFalse:	[exactMatch
			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpWithCaption: aCaption]
			ifNotNil: [classNames addFirst: exactMatch.
				(PopUpMenu labelArray: classNames lines: #(1)) startUpWithCaption: aCaption]].
	index = 0 ifTrue: [^ nil].
	^ Smalltalk at: (classNames at: index) asSymbol

"
	Utilities classFromPattern: 'CharRecog' withCaption: 'xx'
	Utilities classFromPattern: 'rRecog' withCaption: 'xx'
	Utilities classFromPattern: 'znak' withCaption: 'xx'
	Utilities classFromPattern: 'orph' withCaption: 'xx'
"
! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 9/27/2016 21:18:22'!
newMorph
	"The user requested 'new morph' from the world menu.  Put up a menu that allows many ways of obtaining new morphs.  If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used."
	| menu subMenu catDict shortCat class |
	menu _ self menu: 'Add a new morph'.
	menu

		add: 'From Clipboard'
		target: myHand
		action: #pasteMorph;
		
		add: 'From Alphabetical List'
		subMenu: self alphabeticalMorphMenu.
	menu addLine.
		
	"Add 'Classic' New Morph menu"
	catDict _ Dictionary new.
	SystemOrganization categories do: [ :cat |
		((cat beginsWith: 'Morphic-') and: [ (#('Morphic-Menus' 'Morphic-Support' ) includes: cat) not ]) ifTrue: [
			shortCat _ (cat
				copyFrom: 'Morphic-' size + 1
				to: cat size).
			(SystemOrganization listAtCategoryNamed: cat) do: [ :cName |
				class _ Smalltalk at: cName.
				((class is: #Morph) and: [ class includeInNewMorphMenu ]) ifTrue: [
					(catDict includesKey: shortCat)
						ifTrue: [ (catDict at: shortCat) addLast: class ]
						ifFalse: [
							catDict
								at: shortCat
								put: (OrderedCollection with: class) ]]]]].
	catDict keys sort do: [ :categ |
		subMenu _ MenuMorph new.
		((catDict at: categ) asArray sort: [ :c1 :c2 |
			c1 name < c2 name ]) do: [ :cl |
			subMenu
				add: cl name
				target: self
				selector: #newMorphOfClass:event:
				argument: cl ].
		menu
			add: categ
			subMenu: subMenu ].

	self doPopUp: menu.! !

!methodRemoval: Morph #findA:!
Morph removeSelector: #findA:!

!PseudoClass reorganize!
('class' classComment: classPool comment comment: definition definition: metaClass renameTo: sharedPools)
('accessing' allCallsOn allInstVarNames allSuperclasses compilerClass fullName instVarNames name name: organization realClass theNonMetaClass)
('removing' removeAllUnmodified removeUnmodifiedMethods:)
('private' allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: confirmRemovalOf: evaluate: makeSureClassExists: makeSureSuperClassExists: parserClass)
('testing' exists hasChanges hasComment hasDefinition hasMetaclass is: isMeta lastUnderscoreMeansSubscript nameExists needsInitialize)
('fileIn/fileOut' fileIn fileInCategory: fileInDefinition fileInMethod: fileInMethods fileInMethods: fileOut fileOutCategory: fileOutDefinitionOn: fileOutMethod: fileOutMethods:on: fileOutMethodsOn: fileOutOn:)
('errors' classNotDefined)
('categories' removeCategory: removedCategoryName whichCategoryIncludesSelector:)
('methods' addMethodChange: closuresInfoAt: compiledMethodAt:ifAbsent: methodChange: removeMethod: removeSelector: removeSelectorIfInBaseSystem: selectors sourceCode sourceCodeAt: sourceCodeAt:put: sourceCodeTemplate stampAt:)
('printing' literalScannedAs:notifying: printOn:)
('testing method dictionary' bindingOf: includesSelector:)
('compatibility' browseClassVarRefs category chooseInstVarThenDo:)
!

