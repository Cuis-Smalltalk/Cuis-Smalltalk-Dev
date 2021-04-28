'From Cuis 5.0 [latest update: #4567] on 13 April 2021 at 2:15:10 pm'!

!BlockClosure methodsFor: 'private' stamp: 'jmv 4/13/2021 13:50:00'!
valueEnsured
	"Protect against process termination.
	Suggested by Esteban Maringolo at Martin McClure's 'Threads, Critical Sections, and Termination' (Smalltalks 2019 conference)"
	[] ensure: self! !

