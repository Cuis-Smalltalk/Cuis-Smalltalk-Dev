'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 23 April 2012 at 11:21:55 pm'!

!CodePackage methodsFor: 'saving' stamp: 'JMG 4/23/2012 23:21'!
save
	| nameToUse |
	fullFileName ifNil: [
		fullFileName _
			ChangeSet defaultChangeSetDirectory fullNameFor: (self packageName, FileDirectory dot, 'pck')].
	nameToUse _ fullFileName.
"	nameToUse _ Preferences changeSetVersionNumbers
		ifTrue: [
			ChangeSet defaultChangeSetDirectory
				nextNameFor: self packageName coda: '-', Utilities authorInitials
				extension: 'pck' ]
		ifFalse: [ (self packageName , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'pck') asFileName ]."
	Cursor write
		showWhile: [
			| file |
			file _ ChangeSet defaultChangeSetDirectory forceNewFileNamed: nameToUse.
			[
				file timeStamp.
				self writeOnStream: file ]
					ensure: [ file close ]].
	self hasUnsavedChanges: false.
	ChangeSorter removeChangeSet: (ChangeSet changeSetForPackage: self)! !

