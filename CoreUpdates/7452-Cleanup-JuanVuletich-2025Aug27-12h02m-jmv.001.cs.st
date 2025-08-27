'From Cuis7.5 [latest update: #7450] on 27 August 2025 at 12:04:24 pm'!

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:44:32'!
fileOutClassInitializationsOf: classList on: stream

	classList do: [ :aClass |
		self fileOutPSFor: aClass on: stream ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:42:26'!
fileOutMethodsChangesOf: stream on: classList

	classList do: [ :aClass | self fileOutMethodChangesFor: aClass on: stream ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:45:35'!
fileOutMovedClassesOn: stream

	self sortedMovedClassesToOtherPackage do: [ :aMovedClassRecord |
		self fileOutMovedClassRecord: aMovedClassRecord on: stream ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:43:33'!
fileOutRemovedAndMovedMethodsOf: classList on: stream

	classList reverseDo: [ :aClass |
		self fileOutMethodRemovalsFor: aClass on: stream.
		self fileOutMethodMovedToOtherPackagesFor: aClass on: stream ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 12:45:13'!
fileOutRemovedClassesOn: stream

	self sortedRemovedClassRecords do: [ :aRemovedClassRecord | 
		self fileOutRemovedClassRecord: aRemovedClassRecord on: stream ]! !

