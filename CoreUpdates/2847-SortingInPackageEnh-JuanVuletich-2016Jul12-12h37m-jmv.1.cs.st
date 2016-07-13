'From Cuis 4.2 of 25 July 2013 [latest update: #2846] on 12 July 2016 at 12:59:30 pm'!

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'jmv 7/12/2016 12:45:16'!
allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: aBoolean
	"Walk the tree of subclasses, giving the class and its level.
	Sort nicely."

	| aCatIndex bCatIndex |
	classAndLevelBlock value: self value: level.
	self == Class ifTrue:  [^ self].  "Don't visit all the metaclasses"
	"Visit subclasses in alphabetical order"
	self subclasses
		sort: [ :a :b |
			aBoolean
				ifTrue: [
					aCatIndex _ SystemOrganization numberOfCategoryOfElement: a name.
					bCatIndex _ SystemOrganization numberOfCategoryOfElement: b name.
					aCatIndex < bCatIndex
						or: [ aCatIndex = bCatIndex and: [ a name <= b name ]]]
				ifFalse: [ a name <= b name ]
			];
		do: [ :subclass |
			subclass
				allSubclassesWithLevelDo: classAndLevelBlock
				startingLevel: level + 1
				sortByCategory: aBoolean ]! !


!PseudoClass methodsFor: 'private' stamp: 'jmv 7/12/2016 12:38:15'!
allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: aBoolean
	^ (self realClass ifNil: [ ^self ])
		allSubclassesWithLevelDo: classAndLevelBlock
		startingLevel: level
		sortByCategory: aBoolean! !


!HierarchyBrowser methodsFor: 'initialization' stamp: 'sw 5/8/2000 01:02'!
initHierarchyForClass: aClassOrMetaClass
	| tab stab index nonMetaClass |
	centralClass _ aClassOrMetaClass.
	nonMetaClass _ aClassOrMetaClass theNonMetaClass.
	self systemOrganizer: SystemOrganization.
	metaClassIndicated _ aClassOrMetaClass isMeta.
	classList _ OrderedCollection new.
	tab _ ''.
	nonMetaClass allSuperclasses reverseDo: 
		[:aClass | 
		classList add: tab , aClass name.
		tab _ tab , '  '].
	index _ classList size + 1.
	nonMetaClass allSubclassesWithLevelDo:
		[:aClass :level |
		stab _ ''.  1 to: level do: [:i | stab _ stab , '  '].
		classList add: tab , stab , aClass name]
	 	startingLevel: 0.
	self classListIndex: index! !


!Behavior methodsFor: 'accessing class hierarchy' stamp: 'jmv 7/12/2016 12:48:26'!
allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level 
	"Walk the tree of subclasses, giving the class and its level.
	Sort nicely."

	^ self allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: false! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 7/12/2016 12:51:07'!
hierachySortedAllClassesDo: aBlock
	"Evaluate the argument, aBlock, for each class in the system.
	Smalltalk hierachySortedAllClassesDo: [ :cls | cls print ]
	"

	| s |
	s _ self allClasses asSet.
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			(s includes: cls) ifTrue: [
				aBlock value: cls ]]
		startingLevel: 1
		sortByCategory: true! !

!SystemDictionary methodsFor: 'query' stamp: 'jmv 7/12/2016 12:56:58'!
hierarchySorted: classes do: aBlock
	"Sort by hierarchy. Among siblings, sort first by class category and (in same category) by name."
	| s |
	s _ classes asSet.
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			(s includes: cls) ifTrue: [
				aBlock value: cls ].
			(s includes: cls class) ifTrue: [
				aBlock value: cls class ].
			]
		startingLevel: 1
		sortByCategory: true! !


!CodePackage methodsFor: 'enumerating' stamp: 'jmv 7/12/2016 12:49:06'!
allOverriddenMethodsDo: aBlock
	"Evaluates aBlock with all the overridden methods in the system"
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			self overriddenMethodsInClass: cls do: aBlock.
			self overriddenMethodsInClass: cls class do: aBlock ]
		startingLevel: 1
		sortByCategory: true! !

!CodePackage methodsFor: 'dependencies' stamp: 'jmv 7/12/2016 12:49:34'!
externalClasses
	"Answer Classes and Metaclasses not defined in self."
	| myClasses |
	myClasses _ Set new.
	self classesDo: [ :cls | myClasses add: cls ].
	^ Array streamContents: [ :s |
		ProtoObject
			allSubclassesWithLevelDo: [ :cls :l |
				(myClasses includes: cls) ifFalse: [
					s nextPut: cls; nextPut: cls class ]]
			startingLevel: 1
			sortByCategory: true ]! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 7/12/2016 12:53:52'!
hierarchyOfImplementorsOf: aSelector forClass: aClass
	"Answer a list of classes in the hierarchy both above and below the given class which implement the given selector."

	^ (self hierarchyOfClassesSurrounding: aClass) select:
		[:cl | cl includesSelector: aSelector]

"Utilities hierarchyOfImplementorsOf: #contents forClass: Morph"! !


!PseudoClass methodsFor: 'private' stamp: 'jmv 7/12/2016 12:48:18'!
allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level
	^ self allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: false! !

