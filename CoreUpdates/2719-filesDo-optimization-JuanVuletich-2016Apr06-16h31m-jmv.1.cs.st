'From Cuis 4.2 of 25 July 2013 [latest update: #2717] on 7 April 2016 at 12:35:37.456358 pm'!

!FmDirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 4/7/2016 11:43'!
filesDo: aBlock
	self children do: [ :each |
		each isFile ifTrue: [
			aBlock value: each ]]! !

