'From Cuis 4.2 of 25 July 2013 [latest update: #2547] on 21 October 2015 at 1:18:14.292908 pm'!

!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 10/21/2015 09:32'!
contents

	^((model directoryNamesFor: item) asArray sort: [ :a :b | a caseInsensitiveLessOrEqual: b]) collect: [ :n | 
		FileDirectoryWrapper with: (item directoryNamed: n) name: n model: self ]! !

!methodRemoval: FmFileIOAccessor #currentDriveName!
FmFileIOAccessor removeSelector: #currentDriveName!
