'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 29 March 2012 at 9:22:47 am'!
!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses '
	classVariableNames: 'LastUsedNumber Installing '
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: 'ChangeSet class' category: #'Tools-Changes'!
ChangeSet class
	instanceVariableNames: 'installing '!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/29/2012 09:14'!
         baseSystemNameFor: aChangeSetNumber

	^String streamContents: [ :strm |
	strm
		nextPutAll: (aChangeSetNumber asString padded: #left to: 4 with: $0);
		nextPutAll: '-CuisCore-';
		nextPutAll: Utilities authorName asCamelCase;
		nextPutAll: '-';
		nextPutAll: Utilities authorInitials asCamelCase;
		nextPutAll: '-' ]! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/29/2012 09:13'!
                lastUsedNumber
	"
	ChangeSet lastUsedNumber
	"
	LastUsedNumber ifNil: [
		LastUsedNumber _ SystemVersion current highestUpdate ].
	^LastUsedNumber! !


!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 3/29/2012 09:14'!
                               filenamishPrintOn: aStream
	"
	String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]
	"
	
	| year month day monthName |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ].
	monthName _ Month nameOfMonth: month.
	aStream
		nextPutAll: (year abs asString padded: #left to: 4 with: $0);
		nextPutAll: (monthName copyFrom: 1 to: 3);
		nextPutAll: (day asString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (self hour asString padded: #left to: 2 with: $0);
		nextPut: $h;
		nextPutAll: (self minute asString padded: #left to: 2 with: $0);
		nextPut: $m! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/29/2012 09:22'!
                               changeSetForBaseSystem

	| csName |
	csName _ Installing
		ifNil: [ 'ChangesTo-BaseSystem' ]
		ifNotNil: [ 'Modified-BaseSystem--Installing-', Installing ].
	^ChangeSorter existingOrNewChangeSetNamed: csName! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/29/2012 09:13'!
                             changeSetForPackage: aCodePackage

	| csName |
	aCodePackage ifNil: [
		^self changeSetForBaseSystem ].
	csName _ Installing
		ifNil: [ 'UnsavedChangesTo-', aCodePackage name ]
		ifNotNil: [
			Installing = aCodePackage packageName
				ifTrue: [ 'Install-', Installing ]
				ifFalse: [ 'Affects-', aCodePackage name, '--Install-', Installing ]].
	^ChangeSorter existingOrNewChangeSetNamed: csName! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/29/2012 09:13'!
installing: aCodePackageName do: aBlock

	Installing _ aCodePackageName.
	aBlock ensure: [ Installing _ nil ]! !

!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses'
	classVariableNames: 'Installing LastUsedNumber'
	poolDictionaries: ''
	category: 'Tools-Changes'!
