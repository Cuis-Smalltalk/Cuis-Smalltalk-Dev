'From Cuis 4.1 of 12 December 2012 [latest update: #1743] on 6 July 2013 at 5:58:16.781303 pm'!

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 7/6/2013 17:51'!
changeSetDirtyFlags

	^ (ChangeSet allChangeSets collect: [ :each |
		((each isForBaseSystem or: [ each codePackage notNil ]) and: [ each hasUnsavedChanges ])
			ifTrue: [ '     --->']
			ifFalse: [ '       -' ]]) reversed! !


!ChangeSet methodsFor: 'accessing' stamp: 'jmv 7/6/2013 17:53'!
codePackage: aCodePackage
	codePackage _ aCodePackage.
	self isForBaseSystem: false! !

