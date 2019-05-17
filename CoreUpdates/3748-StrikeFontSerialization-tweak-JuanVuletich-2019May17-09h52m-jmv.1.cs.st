'From Cuis 5.0 of 7 November 2016 [latest update: #3747] on 17 May 2019 at 9:53:10 am'!

!StrikeFont methodsFor: 'objects from disk' stamp: 'jmv 5/17/2019 09:52:58'!
objectForDataStream: refStrm

	"I am about to be written on an object file.  Write a textual reference instead.
	Warning: This saves a lot of space, but might fail if using other fonts than those in AvailableFonts"

	^ DiskProxy
		global: #FontFamily
		selector: #familyName:aroundPointSize:
		args: (Array with: self familyName with: self pointSize)! !

