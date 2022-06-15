'From Cuis 6.0 [latest update: #5259] on 10 June 2022 at 9:24:11 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 5/28/2015 10:03'!
adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a number, convert me to a number."

	self shouldNotImplement.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 5/28/2015 10:03'!
adaptToPoint: rcvr andSend: selector
	"If I am involved in arithmetic with a point, convert me to a number."

	self shouldNotImplement.! !

!CharacterSequence methodsFor: 'arithmetic' stamp: 'jmv 9/7/2011 07:30'!
* arg

	self shouldNotImplement.! !

!CharacterSequence methodsFor: 'arithmetic' stamp: 'jmv 9/7/2011 07:29'!
+ arg

	self shouldNotImplement.! !

!CharacterSequence methodsFor: 'arithmetic' stamp: 'jmv 9/7/2011 07:30'!
- arg

	self shouldNotImplement.! !

!CharacterSequence methodsFor: 'arithmetic' stamp: 'pb 5/25/2016 01:13'!
/ arg
	"If working with file paths, just use $/
	Or better yet, use DirectoryEntry protocol"

	self shouldNotImplement! !

!CharacterSequence methodsFor: 'arithmetic' stamp: 'jmv 9/7/2011 07:30'!
// arg

	self shouldNotImplement.! !

!CharacterSequence methodsFor: 'arithmetic' stamp: 'jmv 9/7/2011 07:30'!
\\ arg

	self shouldNotImplement.! !

!methodRemoval: String #+ stamp: 'jmv 6/10/2022 21:22:13'!
String removeSelector: #+!
!methodRemoval: String #\\ stamp: 'jmv 6/10/2022 21:22:51'!
String removeSelector: #\\!
!methodRemoval: String #* stamp: 'jmv 6/10/2022 21:21:58'!
String removeSelector: #*!
!methodRemoval: String #- stamp: 'jmv 6/10/2022 21:22:26'!
String removeSelector: #-!
!methodRemoval: String #adaptToPoint:andSend: stamp: 'jmv 6/10/2022 21:21:47'!
String removeSelector: #adaptToPoint:andSend:!
!methodRemoval: String #// stamp: 'jmv 6/10/2022 21:22:41'!
String removeSelector: #//!
!methodRemoval: String #/ stamp: 'jmv 6/10/2022 21:22:33'!
String removeSelector: #/!
!methodRemoval: String #adaptToNumber:andSend: stamp: 'jmv 6/10/2022 21:21:36'!
String removeSelector: #adaptToNumber:andSend:!
