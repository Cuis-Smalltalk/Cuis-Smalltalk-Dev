'From Cuis6.3 [latest update: #6334] on 26 April 2024 at 5:13:58 pm'!

!Text methodsFor: 'accessing' stamp: 'jmv 4/26/2024 17:12:38'!
linesDo: aBlock
	"execute aBlock with each line in this Text.  The line endings are not included in what is passed to aBlock"
	
	string lineIndicesDo: [ :start :endWithoutDelimiters :end |
		aBlock value: (self copyFrom: start  to: endWithoutDelimiters) ]! !

