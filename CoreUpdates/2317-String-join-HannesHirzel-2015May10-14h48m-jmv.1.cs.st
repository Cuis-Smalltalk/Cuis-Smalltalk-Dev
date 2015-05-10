'From Cuis 4.2 of 25 July 2013 [latest update: #2315] on 10 May 2015 at 3:11:24.24828 pm'!

!String methodsFor: 'copying' stamp: 'hjh 5/9/2015 16:32'!
join: aCollection 
	"'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "

	| w |		
	w := WriteStream on: String new.
	aCollection do: [ :elem | w nextPutAll: elem asString] separatedBy: [w nextPutAll: self].
	^String fromString: w contents! !

