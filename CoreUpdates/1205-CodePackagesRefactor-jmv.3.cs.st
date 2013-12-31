'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 16 March 2012 at 12:29:02 pm'!
!classDefinition: #CodePackage category: #'Package Support'!
Object subclass: #CodePackage
	instanceVariableNames: 'packageName methodCategoryPrefix'
	classVariableNames: 'InstalledPackages'
	poolDictionaries: ''
	category: 'Package Support'!

!CodePackage commentStamp: '<historical>' prior: 0!
                            A CodePackage is a package that is currently loaded in the system. If saved (.pck), then it is stored in a file that can be dealt with as an instance of PackageFile. As the code is already in the system, all we need to know is the packageName. Implementation is originally based on PackageInfo, but has diverged.

CodePackage instances are usually created when installing CodePackageFiles. These instances track the code for that package, that we'll need to save if we don't want to lose changes. These instances are held in the InstalledPackages class variable.

We can also create 'transient' instances with whatever name (and classes and extension methods) we chose, like
	(CodePackage named: 'Collections') inspect; save
This won't mean the system is actually partitioned in such way.

(CodePackage named: 'TestPackage') inspect; save!

!classDefinition: #CodePackageFile category: #'Package Support'!
CodeFile subclass: #CodePackageFile
	instanceVariableNames: 'packageName classesToRemove methodsToRemove'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!CodePackageFile commentStamp: '<historical>' prior: 0!
       A CodePackageFile represents a file with code for a package, regardless of whether it is installed (as a CodePackage) or not. It supports Cuis' .pck as well as .mcz. If formats diverge in the future, maybe a separate class for ,mcz compatibility is in order.

Note: .mcz compatibility is only for browsing .mcz files and installing code (selectively or as a whole). Cuis doesn't support writing .mcz files.!


!ChangeList methodsFor: 'scanning' stamp: 'jmv 3/16/2012 12:04'!
     scanFile: aFile from: startPosition to: stopPosition

	file _ aFile.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	file position: startPosition.
	'Scanning ', aFile localName, '...'
		displayProgressAt: Sensor mousePoint
		from: startPosition to: stopPosition
		during: [:bar |
			[file position < stopPosition] whileTrue: [ | prevChar |
				bar value: file position.
				[file atEnd not and: [file peek isSeparator]]
					whileTrue: [prevChar _ file next].
				(file peekFor: $!!)
					ifTrue: [
						(prevChar notNil and: [ prevChar isLineSeparator ])
							ifTrue: [self scanCategory]]
					ifFalse: [
						| itemPosition item |
						itemPosition _ file position.
						item _ file nextChunk.
						file skipStyleChunk.
						item size > 0 ifTrue: [
							self addItem: (ChangeRecord new file: file position: itemPosition type: #doIt)
								text: 'do it: ' , (item contractTo: 160)]]]].
	self clearSelections! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 3/16/2012 10:55'!
                 browsePackageContents: aStream
	"Opens a changeList on a fileStream"
	| changeList fullName pkName packageFile |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	aStream readOnly.
	Cursor read showWhile: [
		changeList _ self new scanFile: aStream from: 0 to: aStream size.
		aStream reset.
		packageFile _ CodePackageFile
			buildFileStream: aStream
			packageName: pkName
			fullName: fullName.
		aStream close ].
	"Add deletions of methods and classes that are in the PackageInfo (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference asStringOrText ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aStream localName! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 3/16/2012 10:51'!
                         browseMCZContents: aStream
	"Browse the selected file."
	| unzipped changeList fullName packageFile pkName |
	
	"For Monticello packages we do as we do with our own .pck files: Instead of just browsing
	contents, also include what is no longer part of the package (and should therefore be removed on install)
	See #browsePackageContents:
	However, this was never tested to run!!"
	self flag: #jmvVer.

	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	unzipped _ aStream asUnZippedStream: 'snapshot/source.st'.
	unzipped ascii.
	changeList _ Cursor read showWhile: [
		self new scanFile: unzipped from: 0 to: unzipped size.
		aStream reset.
		packageFile _ CodePackageFile
			buildFileStream: aStream
			packageName: pkName
			fullName: fullName ].
	"Add deletions of methods and classes that are in the PackageInfo (i.e., active in the system)
	but are no longer in the PackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference asStringOrText ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aStream name! !


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 3/16/2012 10:52'!
            installMonticelloPackageStream: aStream
	
	| stream fullName pkName |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	stream _ aStream asUnZippedStream: 'snapshot/source.st'.
	stream ascii.
	Cursor wait showWhile: [
		CodePackageFile
			installFileStream: stream
			packageName: pkName
			fullName: fullName ]! !

!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 3/16/2012 10:52'!
                    installPackageStream: aStream

	| fullName pkName |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	Cursor wait showWhile: [
		CodePackageFile
			installFileStream: aStream
			packageName: pkName
			fullName: fullName ]! !


!CodePackage methodsFor: 'comparing' stamp: 'jmv 3/16/2012 10:48'!
                            = other
	^ other species = self species and: [other packageName = self packageName]! !

!CodePackage methodsFor: 'comparing' stamp: 'jmv 3/16/2012 10:48'!
     hash
	^ packageName hash! !

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 3/16/2012 10:48'!
                              actualMethodsDo: aBlock
	"Evaluate aBlock with the actual method objects in this package."
	| enum |
	self extensionMethods do:
		[:mr|
		aBlock value: mr compiledMethod].
	enum := [:behavior|
			behavior organization categories do:
				[:cat|
				(self isForeignClassExtension: cat) ifFalse:
					[(behavior organization listAtCategoryNamed: cat) do:
						[:s|
						aBlock value: (behavior compiledMethodAt: s)]]]].
	self classes do:
		[:c| enum value: c; value: c theMetaClass]
		! !

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 3/16/2012 10:48'!
                  allOverriddenMethodsDo: aBlock
	"Evaluates aBlock with all the overridden methods in the system"
	^ ProtoObject withAllSubclassesDo: [:class | 
		self overriddenMethodsInClass: class do: aBlock]
! !

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 3/16/2012 10:48'!
                   methodsInCategory: aString ofClass: aClass do: aBlock
	((aClass organization listAtCategoryNamed: aString) ifNil: [^self])
		do: [:sel | aBlock value: (self referenceForMethod: sel ofClass: aClass)]! !

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 3/16/2012 10:48'!
                overriddenMethodsDo: aBlock
	"Enumerates the methods the receiver contains which have been overridden by other packages"
	^ self allOverriddenMethodsDo: [:ea |
		(self isOverrideOfYourMethod: ea)
			ifTrue: [aBlock value: ea]]! !

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 3/16/2012 10:48'!
                    overriddenMethodsInClass: aClass do: aBlock
	"Evaluates aBlock with the overridden methods in aClass"
	^ self overrideCategoriesForClass: aClass do: [:cat |
		self methodsInCategory: cat ofClass: aClass do: aBlock]! !

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 3/16/2012 10:48'!
overrideCategoriesForClass: aClass do: aBlock
	"Evaluates aBlock with all the *foo-override categories in aClass"
	^ aClass organization categories do: [:cat |
		(self isOverrideCategory: cat) ifTrue: [aBlock value: cat]]! !

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 3/16/2012 10:48'!
                         sortedMethods
	^ self methods copy sort: [ :a :b |
		a methodSymbol < b methodSymbol or: [
			a methodSymbol = b methodSymbol and: [ a classSymbol <= b classSymbol ]]]! !

!CodePackage methodsFor: 'modifying' stamp: 'jmv 3/16/2012 10:48'!
                 addCoreMethod: aMethodReference
	| category |
	category := self baseCategoryOfMethod: aMethodReference.
	aMethodReference actualClass organization
		classify: aMethodReference methodSymbol
		under: category
		suppressIfDefault: false! !

!CodePackage methodsFor: 'modifying' stamp: 'jmv 3/16/2012 10:48'!
               addExtensionMethod: aMethodReference
	| category |
	category := self baseCategoryOfMethod: aMethodReference.
	aMethodReference actualClass organization
		classify: aMethodReference methodSymbol
		under: self methodCategoryPrefix, '-', category! !

!CodePackage methodsFor: 'modifying' stamp: 'jmv 3/16/2012 10:48'!
     addMethod: aMethodReference
	(self includesClass: aMethodReference class)
		ifTrue: [self addCoreMethod: aMethodReference]
		ifFalse: [self addExtensionMethod: aMethodReference]! !

!CodePackage methodsFor: 'modifying' stamp: 'jmv 3/16/2012 10:48'!
       baseCategoryOfMethod: aMethodReference
	| oldCat oldPrefix tokens | 
	oldCat := aMethodReference category.
	({ 'as yet unclassified'. 'all' } includes: oldCat) ifTrue: [ oldCat := '' ].
	tokens := oldCat findTokens: '*-' keep: '*'.

	"Strip off any old prefixes"
	((tokens at: 1 ifAbsent: [ '' ]) = '*') ifTrue: [
		[ ((tokens at: 1 ifAbsent: [ '' ]) = '*') ]
			whileTrue: [ tokens removeFirst ].
		oldPrefix := tokens removeFirst asLowercase.
		[ (tokens at: 1 ifAbsent: [ '' ]) asLowercase = oldPrefix ]
			whileTrue: [ tokens removeFirst ].
	].

	tokens isEmpty ifTrue: [^ 'as yet unclassified'].
	^ String streamContents:
		[ :s |
		tokens
			do: [ :tok | s nextPutAll: tok ]
			separatedBy: [ s nextPut: $- ]]! !

!CodePackage methodsFor: 'modifying' stamp: 'jmv 3/16/2012 10:48'!
             externalBehaviors
	^self externalClasses , self externalTraits! !

!CodePackage methodsFor: 'modifying' stamp: 'jmv 3/16/2012 10:48'!
                          externalTraits
	"No traits in Cuis"
	^ #()! !

!CodePackage methodsFor: 'modifying' stamp: 'jmv 3/16/2012 10:48'!
              removeMethod: aMethodReference! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
                            allOverriddenMethods
	"search classes and meta classes"
	^ Array streamContents: [:stream |
		self allOverriddenMethodsDo: [:each | stream nextPut: each]]
! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
                               allOverridenMethods
	^ ProtoObject withAllSubclasses gather:
		[:class |
		(self overriddenMethodsInClass: class)]
! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
       classes
	^(self systemCategories gather:
		[:cat |
		(SystemOrganization listAtCategoryNamed: cat)
			collect: [:className | Smalltalk at: className]])
				sorted: [:a :b | a className <= b className]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
                  classesAndMetaClasses
	"Return a Set with all classes and metaclasses belonging to this package"

	| baseClasses result |
	baseClasses := self classes.
	result := (Set new: baseClasses size * 2) 
		addAll: baseClasses;
		yourself.
	baseClasses do: [ :c | 
		result add: c theMetaClass].
	^result
! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
                  coreMethods
	^ self classesAndMetaClasses gather: [:class | self coreMethodsForClass: class]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
                              extensionClasses
	^ self externalBehaviors reject: [:classOrTrait | (self extensionCategoriesForClass: classOrTrait) isEmpty]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
                             extensionMethods
	^ self externalBehaviors gather: [:classOrTrait | self extensionMethodsForClass: classOrTrait]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
          foreignClasses
	| s |
	s := IdentitySet new.
	self foreignSystemCategories
		do: [:c | (SystemOrganization listAtCategoryNamed: c)
				do: [:cl | 
					| cls | 
					cls := Smalltalk at: cl. 
					s add: cls;
					  add: cls class]].
	^ s! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
            foreignSystemCategories
	^ SystemOrganization categories
		reject: [:cat | self includesSystemCategory: cat] ! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
             methods
	^ (self extensionMethods, self coreMethods) select: [:method |
		method isValid ]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
overriddenMethods
	^ Array streamContents: [:stream |
		self overriddenMethodsDo: [:each | stream nextPut: each]]
! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
        overriddenMethodsInClass: aClass
	^Array streamContents: [:stream |
		self overriddenMethodsInClass: aClass
			do: [:each | stream nextPut: each]]
! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
       overrideMethods
	^ self extensionMethods select: [:ea | self isOverrideMethod: ea]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
        selectors
	^ self methods collect: [:ea | ea methodSymbol]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 3/16/2012 10:48'!
systemCategories
	^ SystemOrganization categories select: [:cat | self includesSystemCategory: cat]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                       category: categoryName matches: prefix
	| prefixSize catSize |
	categoryName ifNil: [ ^false ].
	catSize := categoryName size.
	prefixSize := prefix size.
	catSize < prefixSize ifTrue: [ ^false ].
	(categoryName findString: prefix startingAt: 1 caseSensitive: false) = 1
		ifFalse: [ ^false ].
	^(categoryName at: prefix size + 1 ifAbsent: [ ^true ]) = $-! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                       changeRecordForOverriddenMethod: aMethodReference
	| sourceFilesCopy method position |
	method := aMethodReference actualClass compiledMethodAt: aMethodReference methodSymbol.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x isNil ifTrue: [ nil ]
				ifFalse: [x readOnlyCopy]].
	[ | file prevPos prevFileIndex chunk stamp methodCategory tokens |
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil]
		whileTrue:
		[file position: (0 max: position-150).  "Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [chunk := file nextChunk].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevPos := nil.
		stamp := ''.
		(chunk findString: 'methodsFor:' startingAt: 1) > 0
			ifTrue: [tokens := Scanner new scanTokens: chunk]
			ifFalse: [tokens := #()  "ie cant be back ref"].
		((tokens size between: 7 and: 8)
			and: [(tokens at: tokens size-5) = #methodsFor:])
			ifTrue:
				[(tokens at: tokens size-3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp := tokens at: tokens size-2.
						prevPos := tokens last.
						prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: prevPos.
						prevPos := sourceFilesCopy filePositionFromSourcePointer: prevPos]
				ifFalse: ["Old format gives no stamp; prior pointer in two parts"
						prevPos := tokens at: tokens size-2.
						prevFileIndex := tokens last].
				(prevPos = 0 or: [prevFileIndex = 0]) ifTrue: [prevPos := nil]].
		((tokens size between: 5 and: 6)
			and: [(tokens at: tokens size-3) = #methodsFor:])
			ifTrue:
				[(tokens at: tokens size-1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp := tokens at: tokens size]].
		methodCategory := (tokens after: #methodsFor:) ifNil: ['as yet unclassifed'].
		(self includesMethodCategory: methodCategory ofClass: aMethodReference actualClass) ifTrue:
			[methodCategory = (Smalltalk at: #Categorizer ifAbsent: [Smalltalk at: #ClassOrganizer]) default ifTrue: [methodCategory := methodCategory, ' '].
			^ ChangeRecord new file: file position: position type: #method
						class: aMethodReference classSymbol category: methodCategory meta: aMethodReference classIsMeta stamp: stamp].
		position := prevPos.
		prevPos notNil ifTrue:
			[file := sourceFilesCopy at: prevFileIndex]].
		^ nil]
			ensure: [sourceFilesCopy do: [:x | x notNil ifTrue: [x close]]]
	! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
         coreCategoriesForClass: aClass
	^ aClass organization categories select: [:cat | (self isForeignClassExtension: cat) not]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
 coreMethodsForClass: aClass
	^ (aClass selectors difference:
		((self foreignExtensionMethodsForClass: aClass) collect: [:r | r methodSymbol]))
			asArray collect: [:sel | self referenceForMethod: sel ofClass: aClass]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
 extensionCategoriesForClass: aClass
	^ aClass organization categories select: [:cat | self isYourClassExtension: cat]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
     extensionMethodsForClass: aClass
	^ (self extensionCategoriesForClass: aClass)
		gather: [:cat | self methodsInCategory: cat ofClass: aClass ]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
            extensionMethodsFromClasses: classes
	^classes
		gather: [:class | self extensionMethodsForClass: class]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                  foreignExtensionCategoriesForClass: aClass
	^ aClass organization categories select: [:cat | self isForeignClassExtension: cat]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                           foreignExtensionMethodsForClass: aClass
	^ (self foreignExtensionCategoriesForClass: aClass)
		gather: [:cat | (aClass organization listAtCategoryNamed: cat)
						  collect: [:sel | self referenceForMethod: sel ofClass: aClass]]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                     includesChangeRecord: aChangeRecord
	^ aChangeRecord methodClass notNil and:
		[self
			includesMethodCategory: aChangeRecord category
			ofClass: aChangeRecord methodClass]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
             includesClass: aClass
	^ self includesSystemCategory: aClass category! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                     includesClassNamed: aClassName
	^ self includesSystemCategory: ((SystemOrganization categoryOfElement: aClassName) ifNil: [^false])! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                       includesMethod: aSymbol ofClass: aClass
	aClass ifNil: [^ false].
	^ self
		includesMethodCategory: ((aClass organization categoryOfElement: aSymbol)
										ifNil: [' '])
		ofClass: aClass! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                           includesMethodCategory: categoryName ofClass: aClass
	^ (self isYourClassExtension: categoryName)
		or: [(self includesClass: aClass)
				and: [(self isForeignClassExtension: categoryName) not]]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                        includesMethodCategory: categoryName ofClassNamed: aClass
	^ (self isYourClassExtension: categoryName)
		or: [(self includesClassNamed: aClass)
				and: [(self isForeignClassExtension: categoryName) not]]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
              includesMethodReference: aMethodRef
	^ self includesMethod: aMethodRef methodSymbol ofClass: aMethodRef actualClass! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
       includesSystemCategory: categoryName
	^ self category: categoryName matches: self systemCategoryPrefix! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                    isForeignClassExtension: categoryName
	^ categoryName first = $* and: [(self isYourClassExtension: categoryName) not]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
     isOverrideCategory: aString
	^ aString endsWith: '-override'! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                              isOverrideMethod: aMethodReference
	^ self isOverrideCategory: aMethodReference category! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
  isOverrideOfYourMethod: aMethodReference
	"Answers true if the argument overrides a method in this package"
	^ (self isYourClassExtension: aMethodReference category) not and:
		[(self changeRecordForOverriddenMethod: aMethodReference) notNil]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
        isYourClassExtension: categoryName
	^ categoryName notNil and: [self category: categoryName matches: self methodCategoryPrefix]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                           methodsInCategory: aString ofClass: aClass 
	^Array streamContents: [:stream |
		self methodsInCategory: aString ofClass: aClass 
			do: [:each | stream nextPut: each]]
! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                 outsideClasses
	^ProtoObject withAllSubclasses asSet difference: self classesAndMetaClasses! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
                               overrideCategoriesForClass: aClass
	^Array streamContents: [:stream |
		self overrideCategoriesForClass: aClass
			do: [:each | stream nextPut: each]]
! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 3/16/2012 10:48'!
   referenceForMethod: aSymbol ofClass: aClass
	^ MethodReference new setStandardClass: aClass methodSymbol: aSymbol! !

!CodePackage methodsFor: 'naming' stamp: 'jmv 3/16/2012 10:48'!
          methodCategoryPrefix
	^ methodCategoryPrefix ifNil: [methodCategoryPrefix := '*', self packageName asLowercase]! !

!CodePackage methodsFor: 'naming' stamp: 'jmv 3/16/2012 10:48'!
            name

^ self packageName! !

!CodePackage methodsFor: 'naming' stamp: 'jmv 3/16/2012 11:14'!
   packageName
	^ packageName! !

!CodePackage methodsFor: 'naming' stamp: 'jmv 3/16/2012 10:48'!
 packageName: aString
	packageName := aString! !

!CodePackage methodsFor: 'naming' stamp: 'jmv 3/16/2012 10:48'!
               systemCategoryPrefix
	^ self packageName! !

!CodePackage methodsFor: 'dependencies' stamp: 'jmv 3/16/2012 10:48'!
             externalCallers
	^ self 
		externalRefsSelect: [:literal | literal isKindOf: Symbol] 
		thenCollect: [:l | l].! !

!CodePackage methodsFor: 'dependencies' stamp: 'jmv 3/16/2012 10:48'!
       externalClasses
	| myClasses |
	myClasses := self classesAndMetaClasses.
	^ Array streamContents:
		[:s |
		ProtoObject withAllSubclassesDo:
			[:class |
			(myClasses includes: class) ifFalse: [s nextPut: class]]]! !

!CodePackage methodsFor: 'dependencies' stamp: 'jmv 3/16/2012 10:48'!
                               externalRefsSelect: selBlock thenCollect: colBlock
	| pkgMethods dependents extMethods otherClasses otherMethods classNames |

	classNames := self classes collect: [:c | c name].
	extMethods := self extensionMethods collect: [:mr | mr methodSymbol].
	otherClasses := self externalClasses difference: self externalSubclasses.
	otherMethods :=  otherClasses gather: [:c | c selectors].
	pkgMethods := self methods asSet collect: [:mr | mr methodSymbol].
	pkgMethods removeAllFoundIn: otherMethods.

	dependents := Set new.
	otherClasses do: [:c |
		c selectorsAndMethodsDo:
			[:sel :compiled |
			| refs |
			(extMethods includes: sel) ifFalse: 
				[refs := compiled literals select: selBlock thenCollect: colBlock.
				refs do: [:ea |
					((classNames includes: ea) or: [pkgMethods includes: ea])
							ifTrue: [dependents add: (self referenceForMethod: sel ofClass: c) -> ea]]]]].
	^ dependents! !

!CodePackage methodsFor: 'dependencies' stamp: 'jmv 3/16/2012 10:48'!
                   externalSubclasses
	| pkgClasses subClasses |
	pkgClasses := self classes.
	subClasses := Set new.
	pkgClasses do: [:c | subClasses addAll: (c allSubclasses)].
	^ subClasses difference: pkgClasses
! !

!CodePackage methodsFor: 'dependencies' stamp: 'jmv 3/16/2012 10:48'!
                externalUsers
	^ self 
		externalRefsSelect: [:literal | literal isVariableBinding] 
		thenCollect: [:l | l key]! !

!CodePackage methodsFor: 'source code management' stamp: 'jmv 3/16/2012 10:48'!
                           linesOfCode
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."
	^self methods inject: 0 into: [:sum :each | sum + each compiledMethod linesOfCode]! !

!CodePackage methodsFor: 'printing' stamp: 'jmv 3/16/2012 10:48'!
printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self packageName;
		nextPut: $)! !

!CodePackage methodsFor: 'registering' stamp: 'jmv 3/16/2012 11:51'!
       register
	self class register: self! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/16/2012 11:14'!
                        save
	| nameToUse |
	nameToUse _ Preferences changeSetVersionNumbers
		ifTrue: [
			ChangeSet defaultChangeSetDirectory
				nextNameFor: self packageName coda: '-', Utilities authorInitials
				extension: 'pck' ]
		ifFalse: [ (self packageName , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'pck') asFileName ].
	Cursor write
		showWhile: [
			| file |
			file _ ChangeSet defaultChangeSetDirectory newFileNamed: nameToUse.
			[
				file timeStamp.
				self writeOnStream: file ]
					ensure: [ file close ]]! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/16/2012 10:48'!
         writeClassCommentsOn: aStream

	self classes do: [ :class |
		class organization classComment isEmpty ifFalse: [
			class organization
				putCommentOnFile: aStream
				numbered: 0
				moveSource: false
				forClass: class ]]! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/16/2012 10:48'!
                          writeClassDefinitionsOn: aStream

	(ChangeSet superclassOrder: self classes)
		do: [ :class |
			aStream
				nextPut: $!!; nextChunkPut: class definitionPreamble; newLine;
				nextChunkPut: class definition; newLine;

				nextPut: $!!; nextChunkPut: class class definitionPreamble; newLine;
				nextChunkPut: class class definition; newLine;

				newLine ]
		displayingProgress: 'Saving class definitions...'.! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/16/2012 10:48'!
  writeInitializersOn: aStream

	self classes do: [ :class |
		(class class includesSelector: #initialize) ifTrue: [	
			aStream nextChunkPut: class name, ' initialize'; newLine ]]! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/16/2012 10:48'!
         writeMethodsOn: aStream
	self sortedMethods
		do: [ :methodReference |
			methodReference actualClass
				printMethodChunk: methodReference methodSymbol
				withPreamble: true
				on: aStream
				moveSource: false
				toFile: 0 ]
		displayingProgress: 'Saving methods...'.! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/16/2012 10:48'!
           writeOnStream: aStream
	
"	self writeSystemCategoriesOn: aStream."
	self
		writeClassDefinitionsOn: aStream;
		writeClassCommentsOn: aStream;
		writeMethodsOn: aStream;
		writeInitializersOn: aStream! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/16/2012 10:48'!
                    writeSystemCategoriesOn: aStream

	self systemCategories do: [ :categoryName |
		aStream
			nextChunkPut: 'SystemOrganization addCategory: ', categoryName printString;
			newLine ].
	aStream newLine; newLine! !


!CodePackage class methodsFor: 'packages access' stamp: 'jmv 3/16/2012 11:48'!
                            named: aString
	"Answer the instance with name aString.
	Answer only registered (i.e. installed) packages, or nil.
	If a new, unregistered is desired, use #newNamed:"

	^InstalledPackages at: aString ifAbsent: nil! !

!CodePackage class methodsFor: 'packages access' stamp: 'jmv 3/16/2012 11:51'!
                       register: aCodePackage
	
	InstalledPackages at: aCodePackage packageName put: aCodePackage.
	self changed: #packages! !

!CodePackage class methodsFor: 'instance creation' stamp: 'jmv 3/16/2012 11:48'!
                      newNamed: aString
	"Answer a new, unregistered instance"

	^ self new packageName: aString! !

!CodePackage class methodsFor: 'class initialization' stamp: 'jmv 3/16/2012 11:45'!
             initialize
	"
	CodePackage initialize
	"
	InstalledPackages _ Dictionary new! !

!CodePackage class methodsFor: 'searching' stamp: 'jmv 3/16/2012 11:54'!
      packageOfClass: aClass ifNone: errorBlock

	^ InstalledPackages
		detect: [:ea | ea includesClass: aClass]
		ifNone: errorBlock! !

!CodePackage class methodsFor: 'searching' stamp: 'jmv 3/16/2012 11:54'!
                   packageOfMethod: aMethodReference ifNone: errorBlock

	^ InstalledPackages
		detect: [:ea | ea includesMethodReference: aMethodReference]
		ifNone: errorBlock! !

!CodePackage class methodsFor: 'searching' stamp: 'jmv 3/16/2012 11:54'!
                    packageOfMethodCategory: categoryName ofClass: aClass ifNone: errorBlock

	^ InstalledPackages
		detect: [:ea | ea includesMethodCategory: categoryName ofClassNamed: aClass]
		ifNone: errorBlock! !

!CodePackage class methodsFor: 'searching' stamp: 'jmv 3/16/2012 11:55'!
                packageOfSystemCategory: aSystemCategory ifNone: errorBlock

	^ InstalledPackages
		detect: [:ea | ea includesSystemCategory: aSystemCategory]
		ifNone: errorBlock! !


!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 3/16/2012 12:21'!
               buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	"Just build the PackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName _pkName.
	fullName _fullFileName.
	"Don't register a package!!"
	packageInMemory _ (CodePackage named: packageName)
		ifNil: [CodePackage newNamed: packageName].
	self fileInFrom: aFileStream.
	"Compute stuff no longer in package: Should be removed from system."
	classesDefined _ Set new.
	classesExtended _ Set new.
	self classes do: [ :pseudoClass |
		pseudoClass hasDefinition
			ifTrue: [ classesDefined add: pseudoClass name ]
			ifFalse: [ classesExtended add: pseudoClass name ]].
	classesToRemove _ (packageInMemory classes asSet collect: [ :each | each name ]) difference: classesDefined.
	"Add here:
		- classes in classesToDelete, that #allCallsOn answers selectors that aren't in classesToDelete or methodsToRemove
		- classes with #subclasses that aren't in classesToDelete.
		- classes with existing instances (#instanceCount)? Not really sure... Maybe sole instance referenced from classVar or such...
		- something else I forgot?
	Warning: This search for stuff that can't be removed must be iterated again until it doesn't find any more."
	classesToDeleteButCant _ classesToRemove intersection: classesExtended.
	classesToReallyDelete _ classesToRemove difference: classesToDeleteButCant.
	"Methods. Could also mean classes that can't be deleted!! (include in the iteration)
	Warn if deleting last implementor of sent messages?"
	methodsToRemove _ packageInMemory methods asSet difference: self allMethodReferences.
	methodsToRemove _ methodsToRemove reject: [ :methodReference | classesToReallyDelete includes: methodReference classSymbol ].

"
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	('classesToDeleteButCant: ', classesToDeleteButCant printString) print.
	('classesToReallyDelete: ', classesToReallyDelete printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
"! !

!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 3/16/2012 10:51'!
               installFileStream: aFileStream packageName: pkName fullName: fullFileName

	| oldChangeSet |
	self buildFileStream: aFileStream packageName: pkName fullName: fullFileName.

	oldChangeSet _ ChangeSet current.
	[
		aFileStream reset.
		self install: aFileStream.
	] ensure: [
		ChangeSet newChanges: oldChangeSet ]! !

!CodePackageFile methodsFor: 'services' stamp: 'jmv 3/16/2012 12:22'!
                             install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newChangeSet installed |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	newChangeSet _ ChangeSorter basicNewChangeSetLike: 'install package ', localName.
	newChangeSet ifNotNil: [
		ChangeSet newChanges: newChangeSet.
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine].
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared print.

	"Crear, instalar y devolver una instancia de PackageInfo. Descartar la instancia de PackageFile"
	installed _ CodePackage newNamed: packageName.
	CodePackage register: installed.
	^installed! !

!CodePackageFile methodsFor: 'accessing' stamp: 'jmv 3/16/2012 10:51'!
                            classesToRemove
	^classesToRemove! !

!CodePackageFile methodsFor: 'accessing' stamp: 'jmv 3/16/2012 10:51'!
                   methodsToRemove
	^methodsToRemove! !


!CodePackageFile class methodsFor: 'instance creation' stamp: 'jmv 3/16/2012 10:51'!
    buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	| instance |
	instance _ self new.
	[ instance buildFileStream: aFileStream packageName: pkName fullName: fullFileName ]
		ensure: [ aFileStream close ].
	^instance! !

!CodePackageFile class methodsFor: 'instance creation' stamp: 'jmv 3/16/2012 10:51'!
                         installFileStream: aFileStream packageName: pkName fullName: fullFileName
	| instance |
	instance _ self new.
	[ instance installFileStream: aFileStream packageName: pkName fullName: fullFileName ]
		ensure: [ aFileStream close ].
	^instance! !

!CodePackageFile class methodsFor: 'services' stamp: 'jmv 3/16/2012 10:51'!
                              packageNameFrom: fullName
	| localName |
	localName _ FileDirectory localNameFor: fullName.
	^(localName prefixAndSuffix: $-)
		ifNotNil: [ :ary | ary first ]
		ifNil: [ localName sansPeriodSuffix ].! !


!String methodsFor: 'paragraph support' stamp: 'jmv 3/16/2012 12:12'!
             indentationIfBlank: aBlock
	"Answer the number of leading tabs in the receiver.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value."
	| reader leadingTabs lastSeparator tab ch |
	tab _ Character tab.
	reader _ ReadStream on: self.
	leadingTabs _ 0.
	[ reader atEnd not and: [ (ch _ reader next) == tab ]] whileTrue: [
		leadingTabs _ leadingTabs + 1 ].
	lastSeparator _ leadingTabs + 1.
	[ reader atEnd not and: [
		ch isSeparator and: [ ch isLineSeparator not ]]] whileTrue: [
			lastSeparator _ lastSeparator + 1.
			ch _ reader next ].
	lastSeparator = self size | (ch notNil and: [ch isLineSeparator]) ifTrue: [
		^ aBlock value: leadingTabs ].
	^ leadingTabs! !

CodePackage initialize!
!classRemoval: #ChangeSetPackageExporter!
Smalltalk removeClassNamed: #ChangeSetPackageExporter!
!classRemoval: #PackageExporter!
Smalltalk removeClassNamed: #PackageExporter!
!classRemoval: #PackageFile!
Smalltalk removeClassNamed: #PackageFile!
!classRemoval: #PackageInfo!
Smalltalk removeClassNamed: #PackageInfo!
!classRemoval: #PackageList!
Smalltalk removeClassNamed: #PackageList!
!classRemoval: #PackageOrganizer!
Smalltalk removeClassNamed: #PackageOrganizer!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
CodePackage initialize!

