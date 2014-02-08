'From Cuis 4.2 of 25 July 2013 [latest update: #1940] on 3 February 2014 at 10:12:30.160125 pm'!

!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 2/3/2014 22:10'!
serviceInstall
	"Answer a service for filing in an entire file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'install code'
		selector: #install:
		description: 'install change set'
		buttonLabel: 'install'! !

