'From Cuis 4.2 of 25 July 2013 [latest update: #2713] on 2 April 2016 at 10:41:30.767652 pm'!

!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 4/2/2016 22:40'!
getListSize
	"return the current number of items in the displayed list"
	^list ifNotNil: [ list size ] ifNil: [ 0 ]! !

