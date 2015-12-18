'From Cuis 4.2 of 25 July 2013 [latest update: #2636] on 14 December 2015 at 11:28:44.596492 pm'!

!Behavior class methodsFor: 'class initialization' stamp: 'jmv 12/14/2015 23:28'!
releaseClassCachedState
	ObsoleteSubclasses ifNotNil: [
		ObsoleteSubclasses ifEmpty: [
			ObsoleteSubclasses _ nil ]]! !

