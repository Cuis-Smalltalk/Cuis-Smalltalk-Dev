'From Cuis 4.2 of 25 July 2013 [latest update: #2605] on 1 December 2015 at 9:17:10.921078 am'!


!FileDirectoryWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 12/1/2015 09:16'!
contents

	^(item directoryNames asArray sort: [ :a :b | a caseInsensitiveLessOrEqual: b]) collect: [ :n | 
		FileDirectoryWrapper with: item / n name: n model: model ]! !

!methodRemoval: FileDirectoryWrapper #directoryNamesFor:!
FileDirectoryWrapper removeSelector: #directoryNamesFor:!
!methodRemoval: FileList #directoryNamesFor:!
FileList removeSelector: #directoryNamesFor:!
