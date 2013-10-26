'From Cuis 4.0 of 3 April 2012 [latest update: #1254] on 10 April 2012 at 1:00:12 pm'!

!ChangeSet class methodsFor: 'fileIn/Out' stamp: 'jmv 4/10/2012 12:59'!
doWeFileOut: aClass given: notListedYet cache: cache

	| aClassAllSuperclasses aClassSoleInstanceAllSuperclasses |
	aClassAllSuperclasses _ cache
		at: aClass
		ifAbsent: [ cache at: aClass put: aClass allSuperclasses asArray ].

	"Some superclass still not listed?"
	(notListedYet includesAnyOf: aClassAllSuperclasses) ifTrue: [ ^ false ].

	"Not yet if a needed subclass of SharedPool must go first"
	(notListedYet includesAnyOf: aClass sharedPools) ifTrue: [ ^ false ].

	"All superclasses already listed. aClass can go now."
	aClass isMeta ifFalse: [ ^ true ].

	"Only for metaclasses"

	"First include theNonMetaclass"
	(notListedYet includes: aClass soleInstance) ifTrue: [ ^ false ].

	aClassSoleInstanceAllSuperclasses _ cache
		at: aClass soleInstance
		ifAbsent: [ cache at: aClass soleInstance put: aClass soleInstance allSuperclasses asArray ].

	"Some superclass still not listed?"
	(notListedYet includesAnyOf: aClassSoleInstanceAllSuperclasses) ifTrue: [ ^ false ].

	"Ok then"
	^ true! !

!ChangeSet class methodsFor: 'fileIn/Out' stamp: 'jmv 4/10/2012 12:32'!
superclassOrder: classes
	"Arrange the classes in the collection, classes, in superclass order so the 
	classes can be properly filed in. Do it in sets instead of ordered collections.
	SqR 4/12/2000 22:04"

	| all list notListedYet cache |
	list _ classes copy.
	"list is indexable"
	notListedYet _ list asSet.
	cache _ Dictionary new.
	all _ OrderedCollection new: list size.
	list size timesRepeat: [ | aClassIndex aClass |
		aClassIndex _ list findFirst: [ :one |
			one notNil and: [
				self doWeFileOut: one given: notListedYet cache: cache ]].
		aClass _ list at: aClassIndex.
		all addLast: aClass.
		notListedYet remove: aClass.
		list at: aClassIndex put: nil ].
	^ all! !

