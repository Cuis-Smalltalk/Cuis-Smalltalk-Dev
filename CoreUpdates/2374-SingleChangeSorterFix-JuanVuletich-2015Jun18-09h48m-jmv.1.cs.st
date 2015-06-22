'From Cuis 4.2 of 25 July 2013 [latest update: #2373] on 18 June 2015 at 9:48:27.933473 am'!

!SingleSetChangeSorter methodsFor: 'accessing' stamp: 'jmv 6/18/2015 09:48'!
changeSetDirtyFlags

	^myChangeSet hasUnsavedChanges
			ifTrue: [ #('     --->')]
			ifFalse: [ #('       -') ]! !

