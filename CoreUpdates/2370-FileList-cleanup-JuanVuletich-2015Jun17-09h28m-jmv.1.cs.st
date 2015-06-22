'From Cuis 4.2 of 25 July 2013 [latest update: #2369] on 17 June 2015 at 9:29:30.384365 am'!

!FileList methodsFor: 'initialization' stamp: 'jmv 6/16/2015 14:57'!
initialDirectoryList

	| dirList |
	dirList _ (FileDirectory on: '') directoryNames collect: [ :each |
		FileDirectoryWrapper with: (FileDirectory on: each) name: each model: self].
	dirList isEmpty ifTrue: [
		dirList _ Array with: (FileDirectoryWrapper 
			with: directory
			name: directory localName 
			model: self)].
	^dirList! !

!FileList methodsFor: 'private' stamp: 'jmv 6/16/2015 14:55'!
fullName
	"Answer the full name for the currently selected file; answer nil if no file is selected."

	^ fileName ifNotNil: [directory fullNameFor: fileName]! !

