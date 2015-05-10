'From Cuis 4.2 of 25 July 2013 [latest update: #2289] on 1 May 2015 at 10:44:37.246699 pm'!

!CodePackage class methodsFor: 'cached state access' stamp: 'jmv 5/1/2015 22:44'!
releaseClassCachedState

	InstalledPackages isEmpty ifTrue: [
		InstalledPackages _ nil ]! !


!CodePackage class methodsFor: 'packages access' stamp: 'jmv 5/1/2015 22:41'!
deregister: aCodePackage
	"
	CodePackage register: (CodePackage newNamed: 'Tests-Files')
	CodePackage deregister: (CodePackage newNamed: 'Tests-Files')
	"
	self installedPackages removeKey: aCodePackage packageName.
	self triggerEvent: #installedPackagesChanged! !

!CodePackage class methodsFor: 'packages access' stamp: 'jmv 5/1/2015 22:41'!
installedPackages

	InstalledPackages ifNil: [
		InstalledPackages _ Dictionary new ].
	^InstalledPackages! !

!CodePackage class methodsFor: 'packages access' stamp: 'jmv 5/1/2015 22:38'!
named: aString createIfAbsent: neverAnswerNil registerIfNew: doRegisterIfJustCreated
	"Answer the instance with name aString.
	If there's no package named aString, answer nil or a new instance as requested.
	If a new instance is created, register it or not as requested.
	CodePackage named: 'Morphic' createIfAbsent: false registerIfNew: false
	CodePackage named: 'Morphic' createIfAbsent: true registerIfNew: false
	CodePackage named: 'XXXXXTest' createIfAbsent: true registerIfNew: true
	"

	| answer newInstance |
	answer _ self installedPackages
		at: aString
		ifAbsent: [
			neverAnswerNil ifTrue: [ 
				newInstance _ self new packageName: aString.
				doRegisterIfJustCreated ifTrue: [ self register: newInstance ].
				newInstance ]].
	^ answer! !

!CodePackage class methodsFor: 'packages access' stamp: 'jmv 5/1/2015 22:40'!
register: aCodePackage
	"
	Usually call #named:createIfAbsent:registerIfNew: instead
	CodePackage register: (CodePackage newNamed: 'Tests-Files')
	"
	| any base |
	self installedPackages at: aCodePackage packageName put: aCodePackage.

	"Anything that now belongs in this package, was moved out of the base system!!"
	base _ ChangeSet changeSetForBaseSystem.
	any _ false.
	aCodePackage classesDo: [ :cls |
		any _ true.
		base noteClassMoveToOtherPackage: cls ].
	aCodePackage extensionMethods do: [ :methodReference |
		methodReference isValid ifTrue: [
			any _ true.
			base
				noteMethodMoveToOtherPackage: methodReference selector
				forClass: methodReference actualClass ]].
	any ifTrue: [
		aCodePackage hasUnsavedChanges: true ].

	self triggerEvent: #installedPackagesChanged! !

!CodePackage class methodsFor: 'searching' stamp: 'jmv 5/1/2015 22:40'!
packageOfClass: aClass ifNone: errorBlock

	InstalledPackages ifNil: [ ^errorBlock value ].
	^ InstalledPackages
		detect: [:ea | ea includesClass: aClass]
		ifNone: errorBlock! !

!CodePackage class methodsFor: 'searching' stamp: 'jmv 5/1/2015 22:40'!
packageOfMethod: aMethodReference ifNone: errorBlock

	InstalledPackages ifNil: [ ^errorBlock value ].
	^ InstalledPackages
		detect: [:ea | ea includesMethodReference: aMethodReference]
		ifNone: errorBlock! !

!CodePackage class methodsFor: 'searching' stamp: 'jmv 5/1/2015 22:40'!
packageOfMethodCategory: categoryName ofClass: aClass ifNone: errorBlock

	InstalledPackages ifNil: [ ^errorBlock value ].
	^ InstalledPackages
		detect: [:ea | ea includesMethodCategory: categoryName ofClass: aClass]
		ifNone: errorBlock! !

!CodePackage class methodsFor: 'searching' stamp: 'jmv 5/1/2015 22:40'!
packageOfSystemCategory: aSystemCategory ifNone: errorBlock

	InstalledPackages ifNil: [ ^errorBlock value ].
	^ InstalledPackages
		detect: [:ea | ea includesSystemCategory: aSystemCategory]
		ifNone: errorBlock! !

!CodePackage class methodsFor: 'startup' stamp: 'jmv 5/1/2015 22:40'!
clearPaths
	InstalledPackages ifNotNil: [
		InstalledPackages do: [ :a | a fullFileName: nil ]]! !

!methodRemoval: CodePackage class #initialize!
CodePackage class removeSelector: #initialize!
CodePackage initialize!

!CodePackage class reorganize!
('packages access' deregister: installedPackages named:createIfAbsent:registerIfNew: register:)
('searching' packageOfClass:ifNone: packageOfMethod:ifNone: packageOfMethodCategory:ofClass:ifNone: packageOfSystemCategory:ifNone:)
('startup' clearPaths)
('cached state access' releaseClassCachedState)
!

