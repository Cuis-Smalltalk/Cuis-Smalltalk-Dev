'From Cuis 4.2 of 25 July 2013 [latest update: #2953] on 21 October 2016 at 12:21:24 pm'!

!TheWorldMenu methodsFor: 'construction' stamp: 'KenD 10/18/2016 18:19:08'!
helpMenu
	"Build the help menu for the world."
	| menu |
	menu _ self menu: 'Help...'.
	self
		fillIn: menu
		from: {
			{'About this System...'. {Smalltalk. #aboutThisSystem}. 'current version information.'}.
			{'Terse Guide to Cuis'. {self. #openTerseGuide}. 'explore Cuis Smalltalk'}.
			{'Class Comment Browser'. {self. #openCommentGuide}. 'search & explore Cuis Class Comments'}.
			{'Code management in Cuis'. {Utilities. #openCodeManagementInCuis}. 'Features are kept in Packages.'}.
			{'Using GitHub to host Cuis packages'. {Utilities. #openCuisAndGitHub}. 'GitHub usage pattern.'}.
			nil.
			{'Editor keyboard shortcuts'. {SmalltalkEditor. #openHelp}. 'summary of keyboard shortcuts in editors for Smalltalk code.'}
		}.
	self
		fillIn: menu
		from: {
			{'Useful Expressions'. {Utilities. #openUsefulExpressions}. 'a window full of useful expressions.'}.
			nil.
			{'VM Statistics'. {self. #vmStatistics}. 'obtain some intriguing data about the vm.'}.
			{'Space Left'. {self. #garbageCollect}. 'perform a full garbage-collection and report how many bytes of space remain in the image.'}
		}.
	^ menu! !

