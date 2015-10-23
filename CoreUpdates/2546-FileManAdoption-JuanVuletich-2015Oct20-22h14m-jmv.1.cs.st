'From Cuis 4.2 of 25 July 2013 [latest update: #2545] on 20 October 2015 at 10:17:48.719289 pm'!

!FileList methodsFor: 'initialization' stamp: 'jmv 10/20/2015 22:17'!
initialDirectoryList

	| dirList |
	dirList _ FmDirectoryEntry roots collect: [ :each |
		FileDirectoryWrapper with: each name: (each name ifNil: ['/']) model: self].
	dirList isEmpty ifTrue: [
		dirList _ Array with: (FileDirectoryWrapper 
			with: directory
			name: directory localName 
			model: self)].
	^dirList! !


!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 10/19/2015 17:08'!
name
	^name ifNil: [ drive ]! !


!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 10/20/2015 22:16'!
asString
	| result |
	result := itemName.
	"
	('_*_' match: result)
		ifTrue: [ result := (result copyFrom: 2 to: result size - 1) ].
	"
	^ result ! !

