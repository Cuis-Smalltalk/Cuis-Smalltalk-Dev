'From Cuis 5.0 of 7 November 2016 [latest update: #3647] on 7 March 2019 at 5:29:58 pm'!

!InsertSuperclass methodsFor: 'applying - private' stamp: 'HAW 3/7/2019 17:28:52'!
changeSuperclassTo: newSuperclass
	
	newSuperclass subclass: classToRefactor name
		instanceVariableNames: classToRefactor instanceVariablesString 
		classVariableNames: classToRefactor classVariablesString 
		poolDictionaries: classToRefactor sharedPoolsString
		category: classToRefactor category.! !


!FileList methodsFor: 'volume list and pattern' stamp: 'HAW 3/7/2019 17:27:14'!
doesPattern: aPattern allow: entry 

	^(aPattern = '*' or: [ aPattern match: entry name ]) and: [
		"Hide Mac resurce forks and folder service stores"
		(entry name = '.DS_Store') not and: [
		('._*' match: entry name) not ]]! !


!InsertSuperclass methodsFor: 'applying' stamp: 'HAW 3/7/2019 17:29:03'!
apply

	| newSuperclass |
	
	newSuperclass := self createSuperclass.
	self changeSuperclassTo: newSuperclass.

	^newSuperclass ! !

!methodRemoval: InsertSuperclass #changeSuperclassOf:to:!
InsertSuperclass removeSelector: #changeSuperclassOf:to:!
