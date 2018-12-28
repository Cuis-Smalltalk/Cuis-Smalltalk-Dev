'From Cuis 5.0 of 7 November 2016 [latest update: #3544] on 27 December 2018 at 10:21:22 am'!

!DropFilesAction methodsFor: 'evaluating - private' stamp: 'HAW 12/27/2018 10:21:17'!
fileNamedDropped: aFileName 

	(self canBeInstalled: aFileName) ifTrue: [ ^self ifUserWantsInstall: aFileName ].
	(self canBeFiledIn: aFileName) ifTrue: [ ^self ifUserWantsFileIn: aFileName ].
			
	self inform: 'Dropped file ', aFileName, ' not supported'! !

