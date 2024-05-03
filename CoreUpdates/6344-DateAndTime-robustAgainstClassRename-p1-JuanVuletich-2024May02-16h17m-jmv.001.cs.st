'From Cuis6.3 [latest update: #6345] on 2 May 2024 at 4:17:37 pm'!

!DateAndTime methodsFor: 'testing' stamp: 'jmv 5/2/2024 16:17:25'!
is: aSymbol
	"Allow #dateAndTime as argument (see below *)
	Also keep #DateAndTime as valid, as there may be code using it.
	* Using #DateAndTime gives problems in the unlikely case of renaming
	the DateAndTime class to something else. The reason is that this very
	method is heavily used during the rename, to timestamp updated methods.
	If the base image uses a different Symbol, like #dateAndTime, ir works OK.
	"
	^ aSymbol == #DateAndTime or: [
		aSymbol == #dateAndTime or: [
			super is: aSymbol ]].! !

