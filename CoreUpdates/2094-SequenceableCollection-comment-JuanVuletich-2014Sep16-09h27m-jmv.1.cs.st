'From Cuis 4.2 of 25 July 2013 [latest update: #2093] on 16 September 2014 at 9:32:24.369531 am'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 9/16/2014 09:32'!
do: aBlock 
	"Refer to the comment in Collection >> #do:
	Note: Subclasses need to redefine either #do: or #size (or both!!).
	"
	1 to: self size do: [ :index |
		aBlock value: (self at: index) ]! !

